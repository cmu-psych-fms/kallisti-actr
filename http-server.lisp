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
  (swank:set-default-directory "/Users/dfm/work/.../bha-actr")
  (load "act-up-v1_3_3")
  (load "IBL")
  (load "http-server")
  (swank:set-package "BHA")
  (funcall (find-symbol "START-SERVER" (find-package "BHA")) :debug nil))
|#

#-(and bordeaux-threads hunchentoot)
(ql:quickload '(:cl-interpol :alexandria :iterate
                :bordeaux-threads :hunchentoot :babel
                :com.inuoe.jzon :cl-change-case :uiop :vom))

(defpackage :bha
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:ht :hunchentoot) (:b babel) (:jzon :com.inuoe.jzon) (:v :vom))
  (:import-from :cl-user #:run-ibl #:learn-ibl #:init-model
                         #:move #:delayed-move #:escorted-move #:yes #:no
                         #:state #:foe #:friend #:delta #:action #:status)
  (:export #:start-server #:stop-server #:run-standalone))

(in-package :bha)

(interpol:enable-interpol-syntax :modify-*readtable* t)

(defparameter *port* 9899)
(defparameter *debug* nil)
(defparameter *access-log* "bha-actr-access.log")

(defun alistify-jzon (jzon)
  (etypecase jzon
    ((or symbol string number) jzon)
    (vector (map 'vector #'alistify-jzon jzon))
    (hash-table (iter (for (k . v) :in (hash-table-alist jzon))
                      (collect (cons k (alistify-jzon v)))))))

(defun cassoc (item alist)
  (cdr (assoc item alist :test #'equalp)))

(defun sequence-id-value (seq id &optional default)
  (if-let ((obj (find-if (lambda (o) (equalp (cassoc "name" o) id)) seq)))
    (or (cassoc "value" obj) default)
    default))

(define-constant +initial-world-state+ '(:state 100 :foe nil :friend t :delta 0) :test #'equal)

(defparameter *world-state* +initial-world-state+)
(defparameter *world-state-lock* (bt:make-lock "WORLD-STATE-LOCK"))

(defmacro with-world-state (() &body body)
  `(%with-world-state (lambda () ,@body)))

(defun %with-world-state (thunk)
  (bt:with-lock-held (*world-state-lock*)
    (funcall thunk)))

(defun init-world-state ()
  (with-world-state ()
    (setf *world-state* +initial-world-state+)))

(defun wsget (key)
  (with-world-state ()
    (getf *world-state* key)))

(defun update-world-state (&rest args &key state foe friend delta)
  (declare (ignore state foe friend delta))
  (with-world-state ()
    (iter (for (k v) :on args :by #'cddr)
          (setf (getf *world-state* k) v))))

(define-constant +action-name-map+
    '(("Move" . move) ("Delayed Move" . delayed-move) ("Escorted Move" . escorted-move))
  :test 'equalp)

(defparameter *action-map* (make-hash-table :test 'equal))
(defparameter *action-id-map* (make-hash-table :test 'equal))

(defun extract-jag-request-data (json)
  ;; Currently returns just a list action names, having added them and their IDs
  ;; to the relevant maps, above.
  (iter (for a :in-vector (cassoc "actions" json))
        (for id := (cassoc "id" a))
        (for n := (cassoc "name" a))
        (setf (gethash n *action-map*) id)
        (setf (gethash id *action-id-map*) n)
        (collect (cassoc n +action-name-map+))))

(defun extract-jag-status-data (json)
  (let ((a (aref (cassoc "actions" json) 0)))
    (values (sequence-id-value (cassoc "outputs" a) "success")
            (gethash (cassoc "id" a) *action-id-map*))))

(defun extract-commander-data (json)
  (iter (for (key id) :on '(:state "supply_level" :foe "red_at_objective"
                            :friend "friend_ship_available" :delta "supply_change")
             :by #'cddr)
        (nconcing (list key (sequence-id-value json id (getf +initial-world-state+ key))))))

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
  (let* ((actions (extract-jag-request-data json))
         (result (run-ibl `((state ,(wsget :state)) (foe ,(wsget :foe)))
                          `((action ,actions))
                          `((status))
                          `((delta ,(wsget :delta))))))
    (iter (for (m p) :in result)
          (for n := (car (rassoc m +action-name-map+ :test #'equalp)))
          (for id := (gethash n *action-map*))
          (collect (alist-hash-table `(("action_id" . ,id) ("probability" . ,p))) :into probs)
          (finally (return (jzon:stringify (coerce probs 'vector)))))))

(define-json-handler jag-status (json 204 nil)
  (multiple-value-bind (success action) (extract-jag-status-data json)
    (learn-ibl `((state ,(wsget :state)) (foe ,(wsget :foe)))
               `((action (,action)))
               `((status))
               `((delta ,(wsget :delta)))))
  nil)

(define-json-handler state-update (json 204 nil)
  (apply #'update-world-state (extract-commander-data json))
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
  (v:config :bha (cond ((null *debug*) :info)
                       ((integerp *debug*) (make-keyword #?"DEBUG${*debug*}"))
                       (t :debug))))

(defun init ()
  (init-model)
  (init-world-state)
  (clrhash *action-map*)
  (clrhash *action-id-map*))

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
