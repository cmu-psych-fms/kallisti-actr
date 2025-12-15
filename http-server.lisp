 ;;;; Copyright 2025 Carnegie Mellon University

;;; TODO update this comment
;;; A simple HTTP server that reads JSON data from a POST request, runs a Lisp function
;;; on a Lisp representation of the JSON, converts the return value from that function
;;; back to JSON and returns it. If there is difficulty assembling the POST data into
;;; a UTF-8 string or parsing that string as JSON it returns HTTP status 400, and if it
;;; subsequently encounters and error it returns HTTP status 500.

#|
For local testing and debugging in a SLIME listener
(progn
  (swank:set-default-directory "/Users/dfm/work/kallisti/kallisti-actr")
  (load "act-up-v1_3_3")
  (load "IBL")
  (load "http-server")
  (swank:set-package "KALLISTI")
  (funcall (find-symbol "START-SERVER" (find-package "KALLISTI")) :debug nil))
|#

#-(and bordeaux-threads hunchentoot)
(ql:quickload '(:cl-interpol :alexandria :iterate
                :bordeaux-threads :hunchentoot :babel
                :com.inuoe.jzon :cl-change-case :uiop :vom))

(defpackage :kallisti
  (:nicknames :kal)
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:ht :hunchentoot) (:b babel) (:jzon :com.inuoe.jzon) (:v :vom))
  (:import-from :cl-user #:run-ibl #:init-model #:run #:hide
                         #:state #:opponent #:action #:reaction #:status #:delta)
  (:export #:start-server #:stop-server #:run-standalone))

(in-package :kallisti)

(interpol:enable-interpol-syntax :modify-*readtable* t)

(defparameter *port* 9899)
(defparameter *debug* nil)
(defparameter *access-log* "kallisti-actr-access.log")

(defun alistify-jzon (jzon)
  (etypecase jzon
    ((or symbol string number) jzon)
    (vector (map 'vector #'alistify-jzon jzon))
    (hash-table (iter (for (k . v) :in (hash-table-alist jzon))
                      (collect (cons k (alistify-jzon v)))))))

(defun result-to-jzon (expr id-map)
  (iter (for (move prob) :in expr)
        (collect (alist-hash-table `(("action_id" . ,(cdr (assoc move id-map)))
                                     ("probability" . ,(float prob)))
                                   :test 'equal)
          :into actions)
        (finally (return (alist-hash-table `(("actions" . , actions)) :test 'equal)))))

(defun cassoc (item alist)
  (cdr (assoc item alist :test #'equalp)))

(define-constant +action-name-map+
    '(("Move" . run) ("Delayed Move" . hide) ("Escorted Move" . ignore))
  :test 'equalp)

(defparameter *action-map* (make-hash-table :test 'equal))
(defparameter *action-id-map* (make-hash-table :test 'equal))

(defun extract-from-jag (json)
  (iter (for a :in-vector (cassoc "actions" json))
        (for id := (cassoc "id" a))
        (for n := (cassoc "name" a))
        (setf (gethash n *action-map*) id)
        (setf (gethash id *action-id-map*) n)
        (collect (cassoc n +action-name-map+))))

(defparameter *state-history-lock* (bt:make-lock "STATE-LOCK"))
(defparameter *state-history* nil)

(defun clear-state-history ()
  (bt:with-lock-held (*state-history-lock*)
    (setf *state-history* nil)))

(defun add-state-history (value)
  (bt:with-lock-held (*state-history-lock*)
    (push value *state-history*)))

(defun get-state-history ()
  (bt:with-lock-held (*state-history-lock*)
    *state-history*))

(defun populate-state-history ()
  ;; TODO just a debugging/testing hack to get something in there until the real stuff is available
  (clear-state-history)
  (add-state-history '(state 5 opponent no delta 3)))

(defun error-response (code msg &optional (request-id 'null))
  (setf (ht:return-code*) code)
  (setf (ht:header-out "Content-Type") "application/json")
  (jzon:stringify (alist-hash-table `(("error" . ,(alist-hash-table `(("code" . ,code) ;
                                                                      ("message" . ,msg))
                                                                    :test 'equal))
                                      ("request_id" . ,request-id)))))

(defmacro define-json-handler (fname (jvar &optional (code 200) (content-type "application/json")) &body body)
  (let ((name (string-downcase fname)))
    `(ht:define-easy-handler (,fname :uri ,#?"/${name}") ()
       (%json-handler ,name ,code ,content-type (lambda (,jvar) ,@body)))))

(defun %json-handler (name code content-type thunk)
  (let (json result)
    (handler-case
        (progn
          (setf json (ht:raw-post-data))
          (v:debug1 "received raw ~A request ~S" name json)
          (unless (stringp json)
            (setf json (b:octets-to-string json :encoding :utf-8)))
          (v:debug "received ~A request ~S" name json)
          (setf json (alistify-jzon (jzon:parse json))))
      (error (e)
        (v:error "Error reading or parsing ~A message: ~A" name e)
        (setf result
          (error-response 400 #?"Error reading or parsing message: ${e}"))))
    (handler-case
        (progn
          (setf (ht:return-code*) code)
          (when content-type
            (setf (ht:header-out "Content-Type") content-type))
          (setf result (funcall thunk json)))
      (error (e)
        (v:error "Error processing ~A message: ~A" name e)
        (setf result
          (error-response 400 #?"Error processing message: ${e}"))))
    result))

(define-json-handler decision (json)
  (let* ((actions (extract-from-jag json))
         ;; this will get more complicated when we have real state data available
         (state-history (first (get-state-history)))
         (state (getf state-history 'state))
         (opponent (getf state-history 'opponent))
         (delta (getf state-history 'delta))
         (result (run-ibl `((state ,state) (opponent ,opponent))
                          `((action ,actions))
                          `((reaction) (status))
                          `((delta ,delta)))))
    (iter (for (m p) :in result)
          (for n := (car (rassoc m +action-name-map+ :test #'equalp)))
          (for id := (gethash n *action-map*))
          (collect (alist-hash-table `(("action_id" . ,id) ("probability" . ,p))) :into probs)
          (:_ probs)
          (finally (return (jzon:stringify (coerce probs 'vector)))))))

(define-json-handler jag-status (json 204 nil)
  ;; TODO Is still used for anything? If so it will have to be updated.
  (v:info "jag-status called: ~S" json)
  nil)

(define-json-handler state-update (json 204 nil)
  ;; TODO just a stub for now
  (v:info "state-update called: ~S" json)
  nil)

(defvar *server* nil)

(defun stop-server (&optional (soft t))
  (let ((result *server*))
    (cond (*server*
           (v:info "Stopping ~A" *server*)
           (ht:stop *server* :soft soft)
           (v:info "~A stopped" *server*)
           (setf *server* nil))
          (t (v:warn "No server was running")))
    result))

(defun enable-debug (&optional (debug t))
  (cond ((null debug) (setf *debug* nil))
        ((not (realp debug)) (setf *debug* t))
        (t (setf debug (clamp (round debug) 0 4))
           (setf *debug* (if (zerop debug) t debug))))
  (v:config :kal (cond ((null *debug*) :info)
                       ((integerp *debug*) (make-keyword #?"DEBUG${*debug*}"))
                       (t :debug))))

(defun init ()
  (init-model)
  (clear-state-history)
  ;; TODO remove the populate thing when we know what the real state updates will look like
  (populate-state-history))

(defun start-server (&key (port *port*) debug)
  (enable-debug debug)
  (setf *port* port)
  (when *server*
    (v:warn "Server ~S already running, restarting it" *server*)
    (stop-server))
  (init) ;; long term will probably be called in somewhere else, but we don't know where yet
  (setf *server* (ht:start (make-instance 'ht:easy-acceptor
                                          :access-log-destination *access-log*
                                          :port port)))
  (v:info "Started ~A" *server*)
  *server*)

(defun run-standalone ()
  (handler-case (progn
                  (start-server)
                  (sleep 1e10))
    (error (e)
      (vom:crit "top level error ~A" e)
      #+SBCL (sb-debug:print-backtrace)
      (uiop:quit 1))
    #+SBCL
    (sb-sys:interactive-interrupt ()
      (stop-server)
      (vom:info "Quitting")
      (uiop:quit 0))))
