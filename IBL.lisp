#|

General IBL model

Context: optional request for information to fill slots similar to Martin categorization and NASA HEM
Actions: iterate over actions (decision making) or generate actions from goal (planning)
Outcomes: predict expected outcomes from context and actions (optional)
Utilities: generate expected utilities from context, actions and expected outcomes

Decision: select action based on utilities or generate probability distribution from utilities
Learning: receive actual outcomes and utilities (or compute them) and learn association with context and actions

v1.1
- Add traces to run-ibl, learn-ibl, and softmax as key argument
- add draw function for probabilistic outcomes
- Update learn-history to reflect slot changes and more realistic semantics and increase sample to 50

|#

(format t "Loading IBL 1.1.1")

;;; from centipede
(defun softmax (options utilities &optional (trace t))
  "Calibrated softmax of list of options with list of utilities."
  (let* ((max (apply #'max utilities))
         ;;; remove log of utilities and max since they can be negative
         (scales (mapcar #'(lambda (utility) (exp (/ (- utility max) *temperature*))) utilities))
         (sum (reduce #'+ scales))
         (probabilities (sort (mapcar #'list options (mapcar #'(lambda (scale) (/ scale sum)) scales)) #'> :key #'second)))
    (when trace (format trace "Mapping utilities ~S of options ~S to probabiliites ~S~%" utilities options probabilities))
    probabilities))

;;; make a decision and return probability distribution
(defun run-ibl (context actions outcomes utilities &key (trace t))
  "Context is list of slots and values.
  Actions is list of slots and possible values.
  Options and utilities are lists of slots."
  ;;; if necessary, could add requesting context values if missing 
  ;;; iterate over actions
  (let ((action-utilities nil)
        ;;; assuming a single action for now
        (action-options (first actions)))
    (dolist (option (second action-options))
      (when trace (format trace "Choosing action ~S in context ~S~%" option context))
      (let ((instance (append context (list (list (first action-options) option)))))
        ;;; iterate predicting outcomes (could omit for model-free version) assuming discrete
        (dolist (outcome outcomes)
          (let ((predicted-outcome (blend-vote instance (first outcome)))) ;;; use blend-vote or blend-general?
            (when trace (format trace "Predicting outcome ~S as ~S~%" (first outcome) predicted-outcome))
            (setf instance (append instance (list (list (first outcome) predicted-outcome))))))
        ;;; predict and store utility for current action - assume current utility for now
        (let* ((utility (first utilities))
               (predicted-utility (blend instance (first utility)))) ;;; assume utility is numerical
          (when trace (format trace "Predicting utility ~S as ~6,3F~%" (first utility) predicted-utility))
          (push predicted-utility action-utilities))))
    (softmax (second action-options) (reverse action-utilities))))

;;; learn an instance
(defun learn-ibl (context actions outcomes utilities &key (inc 1.0) (trace t))
  "All arguments are lists of slots and values."
  (let ((chunk (funcall #'append context actions outcomes utilities)))
    (learn chunk)
    (actr-time inc)
    (when trace (format trace "Learned chunk ~S and incrementing time to ~6,3F~%" chunk (actr-time)))))

;;; randomly select
(defun pick (choices)
  "randomly select among choices."
  (nth (random (length choices)) choices))

;;; pick among two choices according to some probability
(defun draw (alternatives &optional (p 0.5))
  "Return either alternative depending on probability."
  (if (< (random 1.0) p) (first alternatives) (second alternatives)))

;;; seed the knowledge base with a coverage of chunk values
;;; (run-ibl '((state 50) (foe yes) (friend no)) '((action (move delayed-move escorted-move))) '((status)) '((delta)))
(defun learn-history (&optional (n 50) (trace t))
  "Learn n chunks."
  (dotimes (i n)
    (let* ((context nil)
           (actions nil)
           (outcomes nil)
           (utilities nil)
           (state (* 10 (1+ (random 10)))) ;;; random from 10 to 100 by increments of 10
           (foe (pick '(yes no))) ;;; opponent renamed to foe - yes/no value
           (friend (pick '(yes no))) ;;; added friend - yes/no value
           (action (pick '(move delayed-move escorted-move))) ;;; ;;; randomly pick among move, delayed-move, escorted-move
           (status (draw '(success failure)  ;;; more complex decision rule for success or failure
                            (case action     ;;; depends on action chosen
                              (move (if (eq foe 'yes) 0.2 0.8)) ;;; move 20% success if foe, 80% otherwise
                              (delayed-move (if (> state 30) 0.8 0.2)) ;;; delay 80% success if state > 30, 20% otherwise
                              (escorted-move (if (eq friend 'yes) 0.8 0.2)) ;;; escort 80% success if friend, 20% otherwise
                              (t 0.5)))) ;;; if none of these who knows?
           (delta (- (if (eq status 'success) 50 0) ;; if success then add 50 else 0 then subtract cost of delay or escort
                     (case action
                       (move 10)
                       (delayed-move 30)
                       (escorted-move 20)
                       (t 0)))))
      (push (list 'state state) context) 
      (push (list 'foe foe) context)
      (push (list 'friend friend) context) 
      (push (list 'action action) actions) 
      ;;;      (push (list 'reaction (pick '(nothing attack))) outcomes)  ;;; removed reaction
      (push (list 'status status) outcomes)
      (push (list 'delta delta) utilities)
      (learn-ibl (reverse context) (reverse actions) (reverse outcomes) (reverse utilities) :trace trace))))

;;; initialize model
(defun log-similarity (a b)
  "Log ratio. From 0 to minus infinity so not limited to fixed interval."
  (log (/ (min a b) (max a b))))

(defun linear-similarity (pa pb &optional (scale 1.0))
  "Linear differnece in [0,1] interval."
  (/ (- (min pa pb) (max pa pb)) scale))

(defun number-similarity (x y)
  "Assumes payoffs are integers and probabilities are real values."
  (when (and (numberp x) (numberp y))
    (if (or (integerp x) (integerp y))
        (linear-similarity x y 100.0)
        (linear-similarity x y 1.0))))

(defun init-model (&key (n 50) (parameters nil) (trace t))
  (init-memory)
  (init-similarities)
  (setf *similarity-hook-function* 'number-similarity)
  (dolist (parameter parameters)
    (parameter (first parameter) (second parameter)))
  (learn-history n trace))

#|
? Loading IBL 1.1
(init-model)
Learned chunk ((STATE 50) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to  1.000
Learned chunk ((STATE 40) (FOE NO) (FRIEND NO) (ACTION ESCORTED-MOVE) (STATUS FAILURE) (DELTA -20)) and incrementing time to  2.000
Learned chunk ((STATE 70) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to  3.000
Learned chunk ((STATE 30) (FOE YES) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to  4.000
Learned chunk ((STATE 60) (FOE YES) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to  5.000
Learned chunk ((STATE 20) (FOE YES) (FRIEND NO) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to  6.000
Learned chunk ((STATE 100) (FOE YES) (FRIEND YES) (ACTION DELAYED-MOVE) (STATUS SUCCESS) (DELTA 20)) and incrementing time to  7.000
Learned chunk ((STATE 60) (FOE NO) (FRIEND NO) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to  8.000
Learned chunk ((STATE 30) (FOE YES) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to  9.000
Learned chunk ((STATE 90) (FOE YES) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 10.000
Learned chunk ((STATE 20) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 11.000
Learned chunk ((STATE 20) (FOE YES) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 12.000
Learned chunk ((STATE 40) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS FAILURE) (DELTA -20)) and incrementing time to 13.000
Learned chunk ((STATE 40) (FOE YES) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 14.000
Learned chunk ((STATE 20) (FOE NO) (FRIEND NO) (ACTION DELAYED-MOVE) (STATUS FAILURE) (DELTA -30)) and incrementing time to 15.000
Learned chunk ((STATE 90) (FOE NO) (FRIEND YES) (ACTION DELAYED-MOVE) (STATUS SUCCESS) (DELTA 20)) and incrementing time to 16.000
Learned chunk ((STATE 60) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 17.000
Learned chunk ((STATE 90) (FOE YES) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 18.000
Learned chunk ((STATE 20) (FOE NO) (FRIEND NO) (ACTION DELAYED-MOVE) (STATUS SUCCESS) (DELTA 20)) and incrementing time to 19.000
Learned chunk ((STATE 60) (FOE NO) (FRIEND NO) (ACTION ESCORTED-MOVE) (STATUS FAILURE) (DELTA -20)) and incrementing time to 20.000
Learned chunk ((STATE 40) (FOE NO) (FRIEND NO) (ACTION ESCORTED-MOVE) (STATUS FAILURE) (DELTA -20)) and incrementing time to 21.000
Learned chunk ((STATE 90) (FOE YES) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 22.000
Learned chunk ((STATE 70) (FOE NO) (FRIEND NO) (ACTION DELAYED-MOVE) (STATUS SUCCESS) (DELTA 20)) and incrementing time to 23.000
Learned chunk ((STATE 100) (FOE NO) (FRIEND NO) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to 24.000
Learned chunk ((STATE 10) (FOE YES) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 25.000
Learned chunk ((STATE 70) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 26.000
Learned chunk ((STATE 20) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to 27.000
Learned chunk ((STATE 100) (FOE YES) (FRIEND YES) (ACTION DELAYED-MOVE) (STATUS SUCCESS) (DELTA 20)) and incrementing time to 28.000
Learned chunk ((STATE 90) (FOE NO) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 29.000
Learned chunk ((STATE 20) (FOE NO) (FRIEND NO) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to 30.000
Learned chunk ((STATE 20) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 31.000
Learned chunk ((STATE 20) (FOE YES) (FRIEND NO) (ACTION ESCORTED-MOVE) (STATUS FAILURE) (DELTA -20)) and incrementing time to 32.000
Learned chunk ((STATE 60) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 33.000
Learned chunk ((STATE 10) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 34.000
Learned chunk ((STATE 70) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to 35.000
Learned chunk ((STATE 10) (FOE YES) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 36.000
Learned chunk ((STATE 20) (FOE NO) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 37.000
Learned chunk ((STATE 70) (FOE YES) (FRIEND NO) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 38.000
Learned chunk ((STATE 20) (FOE NO) (FRIEND YES) (ACTION DELAYED-MOVE) (STATUS SUCCESS) (DELTA 20)) and incrementing time to 39.000
Learned chunk ((STATE 50) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 40.000
Learned chunk ((STATE 50) (FOE NO) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 41.000
Learned chunk ((STATE 80) (FOE YES) (FRIEND NO) (ACTION DELAYED-MOVE) (STATUS FAILURE) (DELTA -30)) and incrementing time to 42.000
Learned chunk ((STATE 20) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 43.000
Learned chunk ((STATE 50) (FOE YES) (FRIEND NO) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 44.000
Learned chunk ((STATE 80) (FOE NO) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS FAILURE) (DELTA -20)) and incrementing time to 45.000
Learned chunk ((STATE 60) (FOE NO) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS SUCCESS) (DELTA 30)) and incrementing time to 46.000
Learned chunk ((STATE 40) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to 47.000
Learned chunk ((STATE 40) (FOE YES) (FRIEND YES) (ACTION MOVE) (STATUS FAILURE) (DELTA -10)) and incrementing time to 48.000
Learned chunk ((STATE 100) (FOE NO) (FRIEND YES) (ACTION MOVE) (STATUS SUCCESS) (DELTA 40)) and incrementing time to 49.000
Learned chunk ((STATE 60) (FOE YES) (FRIEND YES) (ACTION ESCORTED-MOVE) (STATUS FAILURE) (DELTA -20)) and incrementing time to 50.000
NIL
?  (run-ibl '((state 50) (foe yes) (friend no)) '((action (move delayed-move escorted-move))) '((status)) '((delta)))
Choosing action MOVE in context ((STATE 50) (FOE YES) (FRIEND NO))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -7.918
Choosing action DELAYED-MOVE in context ((STATE 50) (FOE YES) (FRIEND NO))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -9.906
Choosing action ESCORTED-MOVE in context ((STATE 50) (FOE YES) (FRIEND NO))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -6.032
Mapping utilities (-7.9184866 -9.90605 -6.0322685) of options (MOVE DELAYED-MOVE ESCORTED-MOVE) to probabiliites ((ESCORTED-MOVE 0.8529339) (MOVE 0.1293425) (DELAYED-MOVE 0.017723667))
((ESCORTED-MOVE 0.8529339) (MOVE 0.1293425) (DELAYED-MOVE 0.017723667))
?  (run-ibl '((state 50) (foe yes) (friend yes)) '((action (move delayed-move escorted-move))) '((status)) '((delta)))
Choosing action MOVE in context ((STATE 50) (FOE YES) (FRIEND YES))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -2.940
Choosing action DELAYED-MOVE in context ((STATE 50) (FOE YES) (FRIEND YES))
Predicting outcome STATUS as SUCCESS
Predicting utility DELTA as 16.605
Choosing action ESCORTED-MOVE in context ((STATE 50) (FOE YES) (FRIEND YES))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -7.488
Mapping utilities (-2.939706 16.604633 -7.487789) of options (MOVE DELAYED-MOVE ESCORTED-MOVE) to probabiliites ((DELAYED-MOVE 1.0) (MOVE 3.25088E-9) (ESCORTED-MOVE 3.4418652E-11))
((DELAYED-MOVE 1.0) (MOVE 3.25088E-9) (ESCORTED-MOVE 3.4418652E-11))
?  (run-ibl '((state 50) (foe yes) (friend yes)) '((action (move delayed-move escorted-move))) '((status)) '((delta)))
Choosing action MOVE in context ((STATE 50) (FOE YES) (FRIEND YES))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -4.121
Choosing action DELAYED-MOVE in context ((STATE 50) (FOE YES) (FRIEND YES))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -2.390
Choosing action ESCORTED-MOVE in context ((STATE 50) (FOE YES) (FRIEND YES))
Predicting outcome STATUS as SUCCESS
Predicting utility DELTA as 17.607
Mapping utilities (-4.1211233 -2.3895886 17.606556) of options (MOVE DELAYED-MOVE ESCORTED-MOVE) to probabiliites ((ESCORTED-MOVE 1.0) (DELAYED-MOVE 2.0691142E-9) (MOVE 3.6625905E-10))
((ESCORTED-MOVE 1.0) (DELAYED-MOVE 2.0691142E-9) (MOVE 3.6625905E-10))
?  (run-ibl '((state 50) (foe no) (friend no)) '((action (move delayed-move escorted-move))) '((status)) '((delta)))
Choosing action MOVE in context ((STATE 50) (FOE NO) (FRIEND NO))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -0.071
Choosing action DELAYED-MOVE in context ((STATE 50) (FOE NO) (FRIEND NO))
Predicting outcome STATUS as SUCCESS
Predicting utility DELTA as 15.317
Choosing action ESCORTED-MOVE in context ((STATE 50) (FOE NO) (FRIEND NO))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -3.607
Mapping utilities (-0.07083961 15.316518 -3.6070669) of options (MOVE DELAYED-MOVE ESCORTED-MOVE) to probabiliites ((DELAYED-MOVE 0.99999976) (MOVE 2.076612E-7) (ESCORTED-MOVE 6.047722E-9))
((DELAYED-MOVE 0.99999976) (MOVE 2.076612E-7) (ESCORTED-MOVE 6.047722E-9))
?  (run-ibl '((state 50) (foe no) (friend no)) '((action (move delayed-move escorted-move))) '((status)) '((delta)))
Choosing action MOVE in context ((STATE 50) (FOE NO) (FRIEND NO))
Predicting outcome STATUS as SUCCESS
Predicting utility DELTA as 20.086
Choosing action DELAYED-MOVE in context ((STATE 50) (FOE NO) (FRIEND NO))
Predicting outcome STATUS as SUCCESS
Predicting utility DELTA as 16.283
Choosing action ESCORTED-MOVE in context ((STATE 50) (FOE NO) (FRIEND NO))
Predicting outcome STATUS as FAILURE
Predicting utility DELTA as -5.488
Mapping utilities (20.086407 16.282543 -5.4875154) of options (MOVE DELAYED-MOVE ESCORTED-MOVE) to probabiliites ((MOVE 0.97820127) (DELAYED-MOVE 0.021798734) (ESCORTED-MOVE 7.652712E-12))
((MOVE 0.97820127) (DELAYED-MOVE 0.021798734) (ESCORTED-MOVE 7.652712E-12))
|#

