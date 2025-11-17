#|

ACT-UP 1.3.2
(c) Christian Lebiere, CMU

API:

- learn (<name> | (<content>))
   learn memory specified ny name or content
   if already exists, reinforces/rehearses it
   <name> is any symbol identifier
   <content> is a set of <attribute> <value> pairs
   <attribute> is any symbol identifier
   <value> is any value including numbers, strings, and symbol identifiers

- retrieve (<pattern>)
  retrieves most active pattern from memory
  <pattern> to be matched to (partial) <content>

- blend (<pattern> <output>)
  outputs blended value from memory chunks
  computed as weighted average of numberical values
  <pattern> reflects conditions on retrieval
  <output> is output slot to be blended over

- blend-vote (<pattern> <output>)
  outputs blended value from memory chunks
  computed as weighted sum over discrete chunk values
  <pattern> reflects conditions on retrieval
  <output> is output slot to be blended over

- blend-vote (<pattern> <output>)
  outputs blended value from memory chunks
  computed as weighted sum over discrete chunk values
  <pattern> reflects conditions on retrieval
  <output> is output slot to be blended over

- blend-general (<pattern> <output>)
  outputs blended value from memory chunks
  computed as minimizing sum of squared (dis)similarities over chunk values
  <pattern> reflects conditions on retrieval
  <output> is output slot to be blended over

- attribute (<memory> <attribute>)
  returns the value of <attribute> in <memory>

- actr-time ({<number>})
  increments time by <value> if <number> is included
  returns the value of time if <number> is not included

- similarity (<namei> <namej> {<number>})
  defines similarity between <namei> and <namej> if <number> is supplied
  returns similarity between <namei> and <namej> if <number> is not supplied

- parameter (<name> {<number>})
  sets parameter <name> to <number> if <number> included
  returns value of parameter <name> if number is not included
  acceptable values for parameter <name> include :bll (*decay*), :mp (*mismatch*), :ans (*noise*), :rt (*threshold*)

|#

;;; architectural parameters

(format t "Loading ACT-UP 1.3.3")

(defvar *time* 0.0)

(defun get-time ()
  *time*)

(defun inc-time (inc)
  (incf *time* inc))

(defun actr-time (&optional (inc nil))
  (if inc (inc-time inc) (get-time)))

(defparameter *decay* 0.5)

(defparameter *optimized-learning* t)

(defparameter *mismatch* 1.0)

(defparameter *max-sim* 0.0)

(defparameter *max-dif* -1.0)

(defparameter *noise* 0.25)

(defparameter *threshold* -10.0)

(defparameter *temperature* 1.0)

(defparameter *latency-factor* 0.25)

(defparameter *similarity-hook-function* nil)

(defparameter *similarity-derivative-function* nil)

(defparameter *verbose* nil)

(defun parameter (name &optional (value :lookup))
  (if (eq value :lookup)
      (case name
        (:bll *decay*)
        (:ol *optimized-learning*)
        (:mp *mismatch*)
        (:ms *max-sim*)
        (:md *max-dif*)
        (:ans *noise*)
        (:rt *threshold*)
        (:tmp *temperature*)
        (:lf *latency-factor*)
        (:v *verbose*)
        (t (error "Unknown parameter ~S" name)))
      (case name
        (:bll (setf *decay* value))
        (:ol (setf *optimized-learning* value))
        (:mp (setf *mismatch* value))
        (:ms (setf *max-sim* value))
        (:md (setf *max-dif* value))
        (:ans (setf *noise* value))
        (:rt (setf *threshold* value))
        (:tmp (setf *temperature* value))
        (:lf (setf *latency-factor* value))
        (:v (setf *verbose* value))
        (t (error "Unknown parameter ~S" name)))
      ))

;;; memory structure
;;; name is separate but type (optional) is part of content with corresponding attribute names
(defstruct chunk
  (name nil)
  (content nil)
  (creation nil)
  (references nil))

(defparameter *memory* nil)

(defun init-memory ()
  (setf *time* 0.0)
  (setf *memory* (make-hash-table :test #'eql)))

(defun create-chunk (description &optional (memory *memory*) (trace *verbose*))
  (let ((name (if (symbolp description) description (gentemp "MEMORY")))
        (content (if (symbolp description) nil description)))
    (when trace (format t "Creating Chunk ~A Content ~S.~%" name content))
    (setf (gethash name memory) (make-chunk :name name :content content :creation (get-time)
                                            :references (if *optimized-learning* 1 (list (get-time)))))))

(defun dump-memory (&optional (memory *memory*))
  (let ((contents nil))
    (maphash #'(lambda (name chunk) (push (list name chunk) contents)) memory)
    contents))

(defun attribute (chunk attribute)
  (when chunk (second (assoc attribute (chunk-content chunk)))))

;;; similarities

(defstruct similarity
  (from nil)
  (to nil)
  (value nil))

(defparameter *similarities* nil)

(defun init-similarities ()
  (setf *similarities* (make-hash-table :test #'equal)))

(defun similarity-name (from to)
  (format nil "SIM-~A-~A" from to))

(defun create-similarity (from to value &optional (similarities *similarities*))
  (let ((name (similarity-name from to)))
    (setf (gethash name similarities) (make-similarity :from from :to to :value value))))

(defun similarity (from to &optional (value nil) (similarities *similarities*))
  (if value (create-similarity from to value similarities)
      (or
       (when *similarity-hook-function*                 ; if defined, call it
         (funcall *similarity-hook-function* from to))  ; and if it returns non-nil use that
       (when (eq from to) *max-sim*) ;;; FIX: add identity case
       (let ((similarity (gethash (similarity-name from to) similarities)))
         (if similarity (similarity-value similarity) *max-dif*)))))

;;; implement ACT-R approximation equation and exact one
(defun add-reference (chunk)
  (if *optimized-learning*
      (incf (chunk-references chunk))
      (push (get-time) (chunk-references chunk))))

(defun noise (s)
  "Approximates a sample from a normal distribution with mean zero and
   the given s-value (/ (sqrt (* 3.0 variance)) 3.1416)."
  ;; Need to test bound because of short-float lack of precision
  (let ((p (max 0.0001 (min (random 1.0) 0.9999))))
    (* s (log (/ (- 1.0 p) p)))))

(defun square (x)
  (* x x))

;;; just base-level activation for now; added noise now
(defun activation (chunk &optional (trace *verbose*))
  "Computes activation according to power law of practice and recency (unless *decay* is nil)."
  (let ((activation 0.0)
        (time (get-time)))
    (when *decay*
      (if *optimized-learning*
          (setf activation (/ (* (chunk-references chunk) (expt (- time (chunk-creation chunk)) (- *decay*)))
                              (- 1.0 *decay*)))
          (dolist (reference (chunk-references chunk))
            (incf activation (expt (- time reference) (- *decay*)))))
      (setf activation (log activation)))
    (when *noise* (incf activation (noise *noise*)))
    (when trace (format t "Calculating Chunk ~A Activation ~6,3F.~%" (chunk-name chunk) activation))
    activation))

(defun retrieval-latency (activation &optional (trace *verbose*))
  "Computes the standard retrieval latency function from chunk activation."
  (let ((latency (* *latency-factor* (exp (- activation)))))
    (when trace (format t "Latency of chunk activation ~6,3F is ~6,3F~%" activation latency))
    latency))

(defun partial-match (conditions &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns memory best matching specified conditions."
  (let ((best-chunk nil)
        (best-activation *threshold*)
        (best-latency 0.0))
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name content))
                       (when (>= activation best-activation)
                         (setf best-activation activation) ;;; bug fix reported by Pete Pirolli (3/10/22)
                         (setf best-chunk chunk))))))
             memory)
    (setf best-latency (retrieval-latency best-activation))
;    (actr-time best-latency) ;;; do not increment time to keep it backward compatible and allow different event management schemes
    (values best-chunk best-activation best-latency)))

;;; Simplified version of blending, adapted from partial match function
(defun blend (conditions outcome &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns blend best matching specified conditions as weighted average of slot outcome."
  ;;; partial match the conditions against each memory and blend the outcome slot (assuming numerical)
  ;;; weighted-sum is the sum over each chunk i of e^(Ai/t) where Ai is activation of chunk i
  ;;; weight-average is the sum over each chunk i of e^(Ai/t)*Oi where Oi is content of outcome slot of chunk i
  ;;; blended value of slot outcome is weighted-average/weighted-sum
  (let ((weighted-average 0.0)
        (weighted-sum 0.0))
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name activation)) ;;; FIX: second argument
                       (let ((weight (exp (/ activation *temperature*))))
                         (incf weighted-average (* weight (second (assoc outcome content))))
                         (incf weighted-sum weight))))))
             memory)
    (/ weighted-average weighted-sum)))

#|
;;; Symbolic version of blending, voting for various discrete symbolic options
(defun blend-vote (conditions outcome &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns blend best matching specified conditions as weighted average of slot outcome."
  ;;; partial match the conditions against each memory and blend the outcome slot (assuming numerical)
  ;;; weighted-sum is the sum over each chunk i of e^(Ai/t) where Ai is activation of chunk i
  ;;; weight-average is the sum over each chunk i of e^(Ai/t)*Oi where Oi is content of outcome slot of chunk i
  ;;; blended value of slot outcome is weighted-average/weighted-sum
  (let ((weighted-list nil)
        (weighted-sum 0.0))
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name activation)) ;;; FIX: second argument
                       (let* ((weight (exp (/ activation *temperature*)))
                              (value (second (assoc outcome content)))
                              (current (assoc value weighted-list)))
                         (if current (incf (second current) weight)
                             (push (list value weight) weighted-list))
;;;                         (incf weighted-average (* weight (second (assoc outcome content))))
                         (incf weighted-sum weight))))))
             memory)
    (setf weighted-list (sort weighted-list #'> :key #'second))
    (values (first (first weighted-list)) (/ (second (first weighted-list)) weighted-sum) weighted-list weighted-sum)
;;;    (/ weighted-average weighted-sum)
    ))
|#

;;; Symbolic version of blending, voting for various discrete symbolic options
;;; RETURNS PROBABILITIES FOR ALL CHUNKS
(defun blend-vote (conditions outcome &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns blend best matching specified conditions as weighted average of slot outcome."
  ;;; partial match the conditions against each memory and blend the outcome slot (assuming numerical)
  ;;; weighted-sum is the sum over each chunk i of e^(Ai/t) where Ai is activation of chunk i
  ;;; weight-average is the sum over each chunk i of e^(Ai/t)*Oi where Oi is content of outcome slot of chunk i
  ;;; blended value of slot outcome is weighted-average/weighted-sum
  (let ((weighted-list nil)
        (chunk-list nil)
        (weighted-sum 0.0))
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name activation)) ;;; FIX: second argument
                       (let* ((weight (exp (/ activation *temperature*)))
                              (value (second (assoc outcome content)))
                              (current (assoc value weighted-list)))
                         (push (list content weight) chunk-list)
                         (if current (incf (second current) weight)
                             (push (list value weight) weighted-list))
;;;                         (incf weighted-average (* weight (second (assoc outcome content))))
                         (incf weighted-sum weight))))))
             memory)
    (setf weighted-list (sort weighted-list #'> :key #'second))
    (values (first (first weighted-list)) (/ (second (first weighted-list)) weighted-sum) weighted-list weighted-sum chunk-list)
;;;    (/ weighted-average weighted-sum)
    ))

;;; Fully general version of blending, minimizing error for all possible values (appearing in chunks)
(defun blend-general (conditions outcome &optional (memory *memory*) (similarities *similarities*) (trace *verbose*))
  "Returns blend best matching specified conditions as minimizing weighted average of slot outcome (dis)similarities."
  ;;; partial match the conditions against each memory and blend the outcome slot (using similarities)
  ;;; weighted-sum is the sum over each chunk i of e^(Ai/t) where Ai is activation of chunk i
  ;;; weighted-list is the list of values V with the sum of e^(Ai/t)*Sim(Vi,V)^2 where Vi is content of outcome slot of chunk i
  ;;; blended value of slot outcome is the one with smallest error in weighted-list
  (let ((weighted-list nil)
        (weighted-sum 0.0))
    ;;; Collect all possible output values
    (maphash #'(lambda (name chunk)
                 (let ((slot-value (assoc outcome (chunk-content chunk))))
                   (when slot-value
                     (when trace (format t "Collecting Chunk ~A Outcome ~S Value ~S.~%" name outcome (second slot-value)))
                     (unless (assoc (second slot-value) weighted-list :test #'equal) ;;; unless already existing
                       (push (list (second slot-value) 0.0) weighted-list))))) ;;; initialize that slot value error
             memory)
    (maphash #'(lambda (name chunk)
                 (let ((slot-match nil)
                       (content (chunk-content chunk))
                       (pairs nil))
                   (dolist (condition conditions (setf slot-match t))
                     (let ((slot-value (assoc (first condition) content)))
                       (if slot-value (push (list (second condition) (second slot-value)) pairs)
                           (return))))
                   (when slot-match
                     (let ((activation (activation chunk)))
                       (dolist (pair pairs)
                         (incf activation (* *mismatch* (similarity (first pair) (second pair) nil similarities))))
                       (when trace (format t "Calculating Chunk ~A Match Score ~6,3F.~%" name activation))
                       (let* ((weight (exp (/ activation *temperature*)))
                              (value (second (assoc outcome content))))
;;;                              (current (assoc value weighted-list)))
                         (dolist (current weighted-list) ;;; iterate over all possible values
;;;                         (if current                  ;;; all values have been initialized
                           (incf (second current) (* weight (square (similarity value (first current))))) ;;; add weighted square similarity error
;;;                             (push (list value weight) weighted-list))
                           (incf weighted-sum weight)))))))
             memory)
    (setf weighted-list (sort weighted-list #'< :key #'second)) ;;; sort by decreasing error
    (values (first (first weighted-list)) (/ (second (first weighted-list)) weighted-sum) weighted-list weighted-sum)
    ))

(defun feature-salience (output feature-values weighted-list &key (weighted-sum 1) (similarity-derivative *similarity-derivative-function*) (output-mapping nil) (trace t))
  "Returns salience of output to features given weighted list and optional weighted sum from blend functions.
  Also takes optional similarity derivative function and function mapping chunk outputs to values."
  (let ((feature-saliences nil))
    (dolist (feature-value feature-values)
      (let ((salience 0.0)
            (sum 0.0))
        ;;; compute the inner sum
        (dolist (chunk-activation weighted-list)
          (incf sum (* (/ (second chunk-activation) weighted-sum)
                       (funcall similarity-derivative (second feature-value) (second (assoc (first feature-value) (first chunk-activation)))))))
        (when trace (format t "Sum for feature ~D is ~6,3F~%" (first feature-value) sum))
        ;;; compute the outer sum
        (dolist (chunk-activation weighted-list)
          (incf salience (* (/ (second chunk-activation) weighted-sum)
                            (- (funcall similarity-derivative (second feature-value) (second (assoc (first feature-value) (first chunk-activation)))) sum)
                            (funcall output-mapping (second (assoc output (first chunk-activation))))))
          (when trace (format t "Salience is ~6,3F~%" salience)))
        (push (list (first feature-value) (* (/ *mismatch* *temperature*) salience)) feature-saliences)))
    (reverse feature-saliences)))

(defun exact-match (conditions &optional (memory *memory*) (trace *verbose*))
  "Returns most active memory exactly matching specified conditions."
  (let ((best-chunk nil)
        (best-activation *threshold*)
        (best-latency 0.0))
    (maphash #'(lambda (name chunk) (when (subsetp conditions (chunk-content chunk) :test #'equal)
                                      (let ((activation (activation chunk)))
                                        (when trace (format t "Chunk ~A exactly matches to Conditions ~S with activation ~6,3F.~%"
                                                            name conditions activation))
                                        (when (>= activation best-activation)
                                          (setf best-chunk chunk)
                                          (setf best-activation activation))))) memory)
    (setf best-latency (retrieval-latency best-activation))
;    (actr-time best-latency) ;;; do not increment time to keep it backward compatible and allow different event management schemes
    (values best-chunk best-activation best-latency)))

(defun perfect-match (conditions &optional (memory *memory*) (trace *verbose*))
  "Returns chunk perfectly matching specified conditions."
  (let ((best-chunk nil)
        (conds (length conditions)))
    (maphash #'(lambda (name chunk) (when (and (subsetp conditions (chunk-content chunk) :test #'equal)
                                               (= conds (length (chunk-content chunk))))
                                      (when trace (format t "Chunk ~A pefectly matches to Conditions ~S.~%" name conditions))
                                      (setf best-chunk chunk))) memory)
    best-chunk))

(defun retrieve (conditions &key (mode 'exact) (memory *memory*) (similarities *similarities*) (trace *verbose*))
  (case mode
    (exact (exact-match conditions memory trace))
    (t (partial-match conditions memory similarities trace))))

(defun learn (description &key (memory *memory*))
  "Learns or rehearses chnk with description consisting of name or content (attribute value pairs)."
  (let ((chunk (if (symbolp description) (gethash description memory) (perfect-match description memory))))
    (if chunk
        (add-reference chunk)
        (setf chunk (create-chunk description memory)))
    chunk))
