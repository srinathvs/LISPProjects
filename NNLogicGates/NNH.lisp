#|  Neural Network program in ABCL, Logic Gates Version D.

Copyright 2020-04-12. Dr. Clark Elliott, DePaul University. All rights reserved.

Refer to Jose Luis Bermudez, Cognitive Science, Chapter 8 for background.

Assignment:

1. Become comfortable running this program and training neurons for NOT, AND, OR

2. Create the train4nor and train4nand functions.

NOR is the same as (NOT (OR A B)) -- write out the truth table first.
NAND is the same as (NOT (AND A B)) -- write out the truth table first.

Use the LISP resources I provide to look up these functions:

COND
IF
WHEN
DOTIMES

We use LET to create temporary variables we'll use in the function being DEFUNed. After the function
has run, the variables go away.

|#

(defun help ()
  (format t "(new-rand) to get random values for your neuron~%")
  (format t "(notgate n) (gate n n) where n is 0/1 to see current logic.~%")
  (format t "(train4NOT) (train4AND) (train4OR) are the training functions.~%")
  (format t "(help) to replay this list.~%")
  )
(help)

#| THREE GLOBAL VALUES W1, W2, THRESHOLD, representing a neuron.

Ordinarily, we would put these in a closure variable, or name them *w1* etc., but for this basic
assignment, let's keep the typing simple.

CDE: For generating unique neurons, use defstruct with accessors next time?

|#

(setf threshold 2) (setf w1 0) (setf w2 0) ; These define the neuron input and output
(setf epsilon 0.25) ; Use as a the learning constant, but can experiment with this value.

(defun new-rand () ; Create a new neuron with random values on the "synapses"
  (setf threshold (random 3))
  (setf w1 (random 3))
  (setf w2 (random 3))
  (format t "T: ~s, W1: ~s W2: ~s~%" threshold w1 w2))

(defun notgate (val)
  (let ((summ 0))
    (setf summ (* val w1))
    (if (>= summ threshold) 1 0)))

(defun train4NOT ()
  (dotimes (junk 20) ; We don't actually need the loop variable value, hence junk.
    (train-notgate 0 1)
    (train-notgate 1 0)
    (format t "T: ~s, W1: ~s~%" threshold w1)))

(defun train-notgate (v1 target) ; For this input value, what is the target?
  (let ((delta 0)(actual 0)) ; make some local variables.
    (setf actual (notgate v1))
    (setf delta (- target actual))
    (cond
      ((equal delta 0) t) ; output is correct, (CDE: setf assoc of input val?)
      (t
       (setf threshold (+ threshold (* -1 epsilon delta)))
       (setf w1 (+ w1 (* epsilon delta v1)))))))

(defun gate (v1 v2)
  (let ((i1 (* v1 w1)) (i2 (* v2 w2)) (summ 0))
    (setf summ (+ i1 i2))
    (if (>= summ threshold) 1 0)))

;;; TRAIN-GATE: For a particular input configuration (v1, v2) what output do we want?

(defun train-gate (v1 v2 target) ; For these input values, what is the target?
  (let ((delta 0)(actual 0))
    (setf actual (gate v1 v2)) ;;; return actual current output of function
    (setf delta (- target actual))
    (cond
      ((equal delta 0) t) ; output is correct, so no change to threshold or weights
      (t ; Otherwise, adjust threshold and input weights:
       (setf threshold (+ threshold (* -1 epsilon delta)))
       (setf w1 (+ w1 (* epsilon delta v1)))
       (setf w2 (+ w2 (* epsilon delta v2)))))))

(defun train4and ()
  (let ((rval nil)) ; Set the default return value to nil. Change to T if we complete the training.
    (dotimes (junk 30) ; Train the neuron 30 times (or until we are done)
      (let ((old-threshold threshold)(old-W1 w1)(old-w2 w2)) ; Save copy of old values
        (train-gate 0 0 0) ; Train the gate for each output we want for a set of input values.
        (train-gate 0 1 0)
        (train-gate 1 0 0)
        (train-gate 1 1 1)
        (format t "T: ~s, ~cW1: ~s ~cW2: ~s~%" threshold #\tab w1 #\tab w2)
	;; When there is no difference between the current values and the old values, we are done:
        (when (and (equal old-threshold threshold) (equal old-w1 w1) (equal old-w2 w2)) ; Training complete
          (setf rval t) ; True that training is complete
          (return))))   ; Return from dotimes
    rval)) ; Return T if training is complete, or nil because it is not.

(defun train4or ()
  (let ((rval nil))
    (dotimes (junk 30)
      (let ((old-threshold threshold)(old-W1 w1)(old-w2 w2))
        (train-gate 0 0 0)
        (train-gate 0 1 1)
        (train-gate 1 0 1)
        (train-gate 1 1 1)
        (format t "T: ~s, ~cW1: ~s ~cW2: ~s~%" threshold #\tab w1 #\tab w2)
        (when (and (equal old-threshold threshold) (equal old-w1 w1) (equal old-w2 w2)) ; Training complete
          (setf rval t)
          (return))))
    rval))

;;;--------------------------------- YOUR CODE GOES HERE ---------------------------------------------

(defun train4nor ()
  ;; Your NOR code goes here
  )

(defun train4nand ()
  ;; Your NAND code goes here
  )


(new-rand) ; make a new random-valued neuron
