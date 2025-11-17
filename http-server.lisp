 ;;;; Copyright 2025 Carnegie Mellon University

;;; TODO update this comment
;;; A simple HTTP server that reads JSON data from a POST request, runs a Lisp function
;;; on a Lisp representation of the JSON, converts the return value from that function
;;; back to JSON and returns it. If there is difficulty assembling the POST data into
;;; a UTF-8 string or parsing that string as JSON it returns HTTP status 400, and if it
;;; subsequently encounters and error it returns HTTP status 500.

#|
For local testing and debugging in a SLIME listener:
(progn
  (swank:set-default-directory "/Users/dfm/work/kallisti/kallisti-actr")
  (load "act-up-v1_3_3")
  (load "centipede v1_0")
  (load "http-server")
  (swank:set-package "KALLISTI")
  (funcall (find-symbol "START-SERVER" (find-package "KALLISTI")) :debug nil))
|#

#-(and bordeaux-threads hunchentoot)
(ql:quickload '(:cl-interpol :alexandria :iterate :hunchentoot :babel
                :com.inuoe.jzon :cl-change-case :uiop :vom))

(defpackage :kallisti
  (:nicknames :kal)
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:ht :hunchentoot) (:b babel) (:jzon :com.inuoe.jzon) (:v :vom))
  (:import-from :cl-user
                #:take #:make-node #:play-centipede #:learn-centipede
                #:node-actions #:node-utilities #:a #:b)
  (:export #:start-server #:stop-server #:run-standalone))

(in-package :kallisti)

(interpol:enable-interpol-syntax :modify-*readtable* t)

(defparameter *port* 9899)
(defparameter *debug* nil)
(defparameter *access-log* "kallisti-actr-access.log")

#-NODEBUG
(defun readable-jzon (jzon &optional json-format-p)
  ;; Debugging tool: first value is the result of walking over the results of jzon:parse
  ;; replacing hash tables with plists; the second is a string of pretty printed JSON.
  (labels ((%readable-jzon (jz)
             (etypecase jz
               ((or symbol string number) jz)
               (vector (map 'vector #'readable-jzon jz))
               (hash-table (iter (for (k v) :in-hashtable jz)
                                 (collect (list k (readable-jzon v))))))))
    (if json-format-p
        (jzon:stringify jzon :pretty t)
        (%readable-jzon jzon) )))

(defun find-obj-hash-table (vector name)
  (find-if (lambda (x) (equal (gethash "name" x) name)) vector))

(defun jzon-to-node (jzon)
  (iter (with actions := (gethash "actions" jzon))
        (with first := (aref actions 0))
        (with first-inputs := (gethash "inputs" first))
        (with turn := (gethash "value" (find-obj-hash-table first-inputs "turn")))
        (with pot :=  (gethash "value" (find-obj-hash-table first-inputs "pot")))
        (with player := (intern (string-upcase (aref (gethash "actors" first) 0)) :cl-user))
        (for a :in-vector actions)
        (for name := (intern (string-upcase (gethash "name" a)) :cl-user))
        (collect name :into names)
        (collect (cons name (gethash "id" a)) :into ids)
        (for ev := (gethash "expected_value" a))
        (setf ev (and (hash-table-p ev) (gethash "payoff" ev)))
        (collect (and ev `((,player ,ev)
                           (,(if (eq player 'a) 'b 'a) ,(- pot ev))))
          :into utilities)
        (finally (return (values (make-node :player player
                                            :state turn
                                            :actions names
                                            :utilities utilities)
                                 ids)))))

(defun result-to-jzon (expr id-map)
  (iter (for (move prob) :in expr)
        (collect (alist-hash-table `(("action_id" . ,(cdr (assoc move id-map)))
                                     ("probability" . ,(float prob)))
                                   :test 'equal)
          :into actions)
        (finally (return (alist-hash-table `(("actions" . , actions)) :test 'equal)))))

(defun game-over (jzon)
  (when-let* ((a (aref (gethash "actions" jzon) 0))
              (outputs (and (equal (gethash "status" a) "completed")
                            (gethash "outputs" a)))
              (game-over (find-obj-hash-table outputs "game_over"))
              (game-over-p (gethash "value" game-over))
              (payoff (find-obj-hash-table outputs "payoff"))
              (payoff-value (gethash "value" payoff)))
    payoff-value))

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
          (setf json (jzon:parse json)))
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
  (multiple-value-bind (node id-map) (jzon-to-node json)
    (v:debug1 "Calling model on ~S" node)
    (v:debug1 "ID map is ~S" id-map)
    (let ((result (play-centipede node)))
      (v:debug1 "model returned ~S" result)
      (setf result (jzon:stringify (result-to-jzon result id-map)))
      (v:debug "returning ~S" result)
      result)))

(define-json-handler jag-status (json 204 nil)
  (when-let* ((payoff (game-over json))
              (node (jzon-to-node json)))
    (setf (node-actions node) nil)
    (setf (node-utilities node) (list payoff))
    (learn-centipede node))
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

(defun start-server (&key (port *port*) debug)
  (enable-debug debug)
  (setf *port* port)
  (when *server*
    (v:warn "Server ~S already running, restarting it" *server*)
    (stop-server))
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
