#|

General IBL model

Context: optional request for information to fill slots similar to Martin categorization and NASA HEM
Actions: iterate over actions (decision making) or generate actions from goal (planning)
Outcomes: predict expected outcomes from context and actions (optional)
Utilities: generate expected utilities from context, actions and expected outcomes

Decision: select action based on utilities or generate probability distribution from utilities
Learning: receive actual outcomes and utilities (or compute them) and learn association with context and actions

|#

(format t "Loading IBL 1.0")

;;; from centipede
(defun softmax (options utilities)
  "Calibrated softmax of list of options with list of utilities."
  (let* ((max (apply #'max utilities))
         ;;; remove log of utilities and max since they can be negative
         (scales (mapcar #'(lambda (utility) (exp (/ (- utility max) *temperature*))) utilities))
         (sum (reduce #'+ scales)))
    (sort (mapcar #'list options (mapcar #'(lambda (scale) (/ scale sum)) scales)) #'> :key #'second)))

;;; make a decision and return probability distribution
(defun run-ibl (context actions outcomes utilities)
  "Context is list of slots and values.
  Actions is list of slots and possible values.
  Options and utilities are lists of slots."
  ;;; if necessary, could add requesting context values if missing 
  ;;; iterate over actions
  (let ((action-utilities nil)
        ;;; assuming a single action for now
        (action-options (first actions)))
    (dolist (option (second action-options))
      (let ((instance (append context (list (list (first action-options) option)))))
        ;;; iterate predicting outcomes (could omit for model-free version) assuming discrete
        (dolist (outcome outcomes)
          (let ((predicted-outcome (blend-vote instance (first outcome)))) ;;; use blend-vote or blend-general?
            (setf instance (append instance (list (list (first outcome) predicted-outcome))))))
        ;;; predict and store utility for current action - assume current utility for now
        (let* ((utility (first utilities))
               (predicted-utility (blend instance (first utility)))) ;;; assume utility is numerical
          (push predicted-utility action-utilities))))
    (softmax (second action-options) (reverse action-utilities))))

;;; learn an instance
(defun learn-ibl (context actions outcomes utilities &optional (inc 1.0))
  "All arguments are lists of slots and values."
  (learn (funcall #'append context actions outcomes utilities))
  (actr-time inc))

;;; randomly select
(defun pick (choices)
  "randomly select among choices."
  (nth (random (length choices)) choices))

;;; seed the knowledge base with random chunk values
;;; (run-ibl '((state 5) (opponent yes)) '((action (run hide ignore))) '((reaction) (status)) '((delta)))
(defun learn-history (&optional (n 10))
  "Learn n chunks."
  (dotimes (i n)
    (let ((context nil)
          (actions nil)
          (outcomes nil)
          (utilities nil))
      (push (list 'state (1+ (random 10))) context)
      (push (list 'opponent (pick '(yes no))) context)
      (push (list 'action (pick '(run hide ignore))) actions)
      (push (list 'reaction (pick '(nothing attack))) outcomes)
      (let ((status (pick '(success failure))))
        (push (list 'status status) outcomes)
        (push (list 'delta (if (eq status 'success) (1+ (random 5)) (- (1+ (random 5))))) utilities)
        (learn-ibl (reverse context) (reverse actions) (reverse outcomes) (reverse utilities))))))

;;; initialize model
(defun log-similarity (a b)
  "Log ratio. From 0 to minus infinity so not limited to fixed interval."
  (log (/ (min a b) (max a b))))

(defun linear-similarity (pa pb &optional (scale 10))
  "Linear differnece in [0,1] interval."
  (/ (- (min pa pb) (max pa pb)) scale))

(defun number-similarity (x y)
  "Assumes payoffs are integers and probabilities are real values."
  (when (and (numberp x) (numberp y))
    (if (or (integerp x) (integerp y))
        (linear-similarity x y 10)
        (linear-similarity x y 1.0))))

(defun init-model (&optional (n 10) (parameters nil))
  (init-memory)
  (init-similarities)
  (setf *similarity-hook-function* 'number-similarity)
  (dolist (parameter parameters)
    (parameter (first parameter) (second parameter)))
  (learn-history n))

#|
? (init-model)
0> Calling (LEARN-IBL ((STATE 8) (OPPONENT YES)) ((ACTION IGNORE)) ((REACTION NOTHING) (STATUS FAILURE)) ((DELTA -3))) 
<0 LEARN-IBL returned 1.0
0> Calling (LEARN-IBL ((STATE 1) (OPPONENT YES)) ((ACTION HIDE)) ((REACTION ATTACK) (STATUS FAILURE)) ((DELTA -2))) 
<0 LEARN-IBL returned 2.0
0> Calling (LEARN-IBL ((STATE 5) (OPPONENT NO)) ((ACTION IGNORE)) ((REACTION ATTACK) (STATUS FAILURE)) ((DELTA -1)))
<0 LEARN-IBL returned 3.0
0> Calling (LEARN-IBL ((STATE 7) (OPPONENT YES)) ((ACTION HIDE)) ((REACTION ATTACK) (STATUS SUCCESS)) ((DELTA 2))) 
<0 LEARN-IBL returned 4.0
0> Calling (LEARN-IBL ((STATE 6) (OPPONENT YES)) ((ACTION RUN)) ((REACTION NOTHING) (STATUS SUCCESS)) ((DELTA 2))) 
<0 LEARN-IBL returned 5.0
0> Calling (LEARN-IBL ((STATE 9) (OPPONENT YES)) ((ACTION HIDE)) ((REACTION NOTHING) (STATUS SUCCESS)) ((DELTA 4))) 
<0 LEARN-IBL returned 6.0
0> Calling (LEARN-IBL ((STATE 7) (OPPONENT YES)) ((ACTION HIDE)) ((REACTION ATTACK) (STATUS SUCCESS)) ((DELTA 3))) 
<0 LEARN-IBL returned 7.0
0> Calling (LEARN-IBL ((STATE 3) (OPPONENT YES)) ((ACTION RUN)) ((REACTION NOTHING) (STATUS FAILURE)) ((DELTA -5))) 
<0 LEARN-IBL returned 8.0
0> Calling (LEARN-IBL ((STATE 6) (OPPONENT NO)) ((ACTION RUN)) ((REACTION ATTACK) (STATUS FAILURE)) ((DELTA -1))) 
<0 LEARN-IBL returned 9.0
0> Calling (LEARN-IBL ((STATE 2) (OPPONENT NO)) ((ACTION HIDE)) ((REACTION ATTACK) (STATUS SUCCESS)) ((DELTA 2))) 
<0 LEARN-IBL returned 10.0
NIL
? (run-ibl '((state 5) (opponent yes)) '((action (run hide ignore))) '((reaction) (status)) '((delta)))
0> Calling (SOFTMAX (RUN HIDE IGNORE) (-1.665389 1.8204083 -1.1111838)) 
<0 SOFTMAX returned ((HIDE 0.92255914) (IGNORE 0.049183562) (RUN 0.028257363))
((HIDE 0.92255914) (IGNORE 0.049183562) (RUN 0.028257363))
|#

