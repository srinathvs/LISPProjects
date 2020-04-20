#|

Copyright 2020 Clark Elliott, all rights reserved.

Simple perceptron single neuron face recognition neural network program.

Common LISP. Tested in ABCL on 2020-04-05.

See Jose Luis Bermudez Chapter 8 for exposition of the perceptron convergence rule.

We use a 207 weighted-input perceptron (neuron) to read the "pixels" of a data file representing
different faces. (The pixels are "." and "X" characters.)

Random weights are assigned to the inputs, and a random weight is assigned to the neuron's treshold.

The perceptron is trained to positively recognize exactly one of the faces, but can be trained on
any of them.

We use (equal x y) throughout to simplify for pedagogical reasons.

See sample data files, and example output below.


ASSIGNMENT:

Create the data files, if you have not already copied them (see below).

Search on PQR for missing parts of the LISP code. Replace PQR with the correct code.

Add extensive COMMENTS to the code demonstrating you know how it works.

Get the code fully running for five faces: four from the existing data files, and a fifth from
a face data file of your own making.

Run your code. Put the output into a faces.log file.

Complete the checklist.

Turn in your checklist, your COMMENTED LISP code and your output faces.log file, zipped together,
to D2L.

[Note to CDE: Do I want to provide a compiled version that runs?]

|#


;;; We see the face as a two dimensional array. The perceptron neuron sees it as one-dimensional.

(setf neuron-w (make-array 207)) ; 207 input weights W for neuron for 9 x 23 data array.
(setf smile   (make-array '(9 23)))
(setf neutral (make-array '(9 23)))
(setf frown   (make-array '(9 23)))
(setf mixed   (make-array '(9 23)))
(setf speechless (make-array '(9 23)))

(setf smile1 (make-array '(9 23))) #|:initial-contents '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0)
                                                    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))|#;;Tried to directly make a vector for parsing

(setf faces (list smile neutral frown mixed speechless))
(setf *epsilon* 0.25) ; Can try different learning rates 0 < *epsilon* < 1.

(defun initialize-faces ()
  (create-face-datasets)
  (new-neuron))

(defun uu () (initialize-faces)) ; Easy typing for debugging!
(defun ll () (load "Face.lisp")) ; Easy typing! Type (ll) to reload the file.


(defun create-face-datasets ();;Inputes 0s and 1s into the corresponding face-lists based on their
  (collect-data "smile.data" smile)
  (collect-data "neutral.data" neutral)
  (collect-data "frown.data" frown)
  (collect-data "mixed.data" mixed)
  (collect-data "speechless.data" speechless))

#| PQR. Substitute the right code for the three PQRs in this function: |#
(defun train4smile ()
  (dotimes (n 30 n) ; Set a limit of 30 in case something goes wrong.
    (when
	(and ; After the latest adjustment to the percetron, are ALL the results correct?
	 (train-face smile   1)
	 (train-face neutral 0)
	 (train-face frown   0) ; <- hint
	 (train-face mixed   0)
   (train-face speechless 0))
      (return (+ n 1))))) ; How many times did we have to adjust the perceptron?

#| PQR: Complete these three additional functions: |#

(defun train4neutral ()
  (dotimes (n 30 n) ; Set a limit of 30 in case something goes wrong.
    (when
	(and ; After the latest adjustment to the percetron, are ALL the results correct?
	 (train-face smile   0)
	 (train-face neutral 1)
	 (train-face frown   0)
	 (train-face mixed   0)
   (train-face speechless 0))
      (return (+ n 1))))) ; How many times did we have to adjust the perceptron?

(defun train4frown ()
  (dotimes (n 30 n) ; Set a limit of 30 in case something goes wrong.
    (when
	(and ; After the latest adjustment to the percetron, are ALL the results correct?
	 (train-face smile   0)
	 (train-face neutral 0)
	 (train-face frown   1)
	 (train-face mixed   0)
   (train-face speechless 0))
      (return (+ n 1))))) ; How many times did we have to adjust the perceptron?

(defun train4mixed ()
  (dotimes (n 30 n) ; Set a limit of 30 in case something goes wrong.
    (when
	(and ; After the latest adjustment to the percetron, are ALL the results correct?
	 (train-face smile   0)
	 (train-face neutral 0)
	 (train-face frown   0)
	 (train-face mixed   1)
   (train-face speechless 0))
      (return (+ n 1))))) ; How many times did we have to adjust the perceptron?


#| PQR: Write your own FACE training function, and create the data file: |#

(defun train4MYFACE ()
  (dotimes (n 30 n) ; Set a limit of 30 in case something goes wrong.
    (when
 (and ; After the latest adjustment to the percetron, are ALL the results correct?
  (train-face smile   0)
  (train-face neutral 0)
  (train-face frown   0)
  (train-face mixed   0)
  (train-face speechless 1))
      (return (+ n 1)))));;Returns iteration#


;;;Checking if my program works on on.data type elements. Could not bypass the Vector format error.
  (defun train4MYFACE2 ()
    (dotimes (n 30 n) ; Set a limit of 30 in case something goes wrong.
      (when
   (and ; After the latest adjustment to the percetron, are ALL the results correct?
    (train-face smile1 1))
        (return (+ n 1)))));;Returns iteration#


(defun pass-all-tests (face)
  (let ((match-count 0))
    (mapcar (lambda (f) (when (matchp f) (incf match-count))) faces) ; map over all the faces
    (and (matchp face) ; Matches the right face...
	 (equal match-count 1)))) ;; ...and only matches one face.

(defun matchp (face) (if (equal (recon face) 1) t nil)) ; Match prediate = t or nil

;;Collect data fails with errors.
  (defun collect-data (filename data-array) ; Put the face data into a 9 x 23 array of ones and zeros
    (let ((l nil) (position 0) (file-char nil))
      (with-open-file (inf filename)
        (dotimes (x 9);;Repeating for number of rows
  	(setf l (read-line inf));;Setting up the data file line by line into l
  	(format t "~a~%" l)
  	(dotimes (y 23)
  	  (setf file-char (subseq l y (+ y 1)));;Setting filechar to be the character between y and y+1(Moving one character at a time) in l
  	  (setf (aref data-array position) (if (equal file-char "X")  1 0));;Setting Value in array to 1 if X is found, else 0
  	  (setf position (1+ position)))))
     (coerce data-array 'vector);;Trying to set the array to vector type to resolve my error.
      t))

(defun new-neuron () ; Input weights and threshold
  (dotimes (n 207) (setf (aref neuron-w n) (random 3))) ;;Setting up randomized values in neuron-w 2D array for training
  (setf thresholdN (random 50)));;Setting up of threshold

;;; (defun rr () (recon smile)) ; Save typing during debug

(defun recon (face-list)
  (let ((summ 0))
    (dotimes (k 207);repeating 207 times (till face-list array is emptied)
      (setf summ (+ summ (* (aref face-list k) (aref neuron-w k)))));;Calculating sum from element of the face-list and the neuron weights at k
    (if (>= summ thresholdN) 1 0)));;setting up thresholds for detection

;;; (defun tr () (train-face neutral 1)) ; For debugging.

(defun train-face (face-list target) ; Target is 0 or 1
  (let ((epsilon *epsilon*) (delta 0) (actual 0)) ; Try different learning rates?
    (setf actual (recon face-list)) ;;; return actual current output of function
    (setf delta (- target actual));;Observed-Expected
    (cond
      ((equal delta 0) t) ; Output is correct, so no change to threshold or weights, return T
      (t
       (setf thresholdN (+ thresholdN (* -1 epsilon delta)));;Reducing threshold if delta is high.
       (dotimes (n 207 nil) ; After making changes, return nil so we check again.
	 (setf (aref neuron-w n)
	       (+ (aref neuron-w n)
		  (* (aref face-list n) epsilon delta))))))));;Setting up values in the neuron-weights based on learning rates until the system plateaus

;;; Post instructions on the console:
(format t "srinath's face recognition program.~%") ; PQR Change to your name.
(format t "To run: (initialize-faces) (train4smile) (pass-all-tests smile)~%")
(format t "Ditto for: smile, neutral, frown, mixed, speechless~%")

#|

The data files:

smile.dat:
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . X X . . X X . . .
. . . . . . . . . . . .
. . . . . X X . . . . .
. . . . . . . . . . . .
. . X . . . . . . X . .
. . . X . . . . X . . .
. . . . X X X X . . . .

neutral.dat:
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . X X . . X X . . .
. . . . . . . . . . . .
. . . . . X X . . . . .
. . . . . . . . . . . .
. . X X X X X X X X . .
. . . . . . . . . . . .
. . . . . . . . . . . .

frown.data:
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . X X . . X X . . .
. . . . . . . . . . . .
. . . . . X X . . . . .
. . . . . . . . . . . .
. . . . X X X X . . . .
. . . X . . . . X . . .
. . X . . . . . . X . .

mixed.data:
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . X X . . X X . . .
. . . . . . . . . . . .
. . . . . X X . . . . .
. . . . . . . . X X . .
. . . . X X X X . . . .
. . X X . . . . . . . .
. . . . . . . . . . . .
|#
#|

Sample output (2020-04-05):

CL-USER(31): (load "NNFacesD")
To run: (initialize-faces) (train4smile) (pass-all-tests smile)
Ditto for: smile, neutral, frown, mixed
T
CL-USER(32): (initialize-faces)

[...cut...]

CL-USER(33): (pass-all-tests smile)
NIL
CL-USER(34): thresholdN
21
CL-USER(35): neuron-w
#(0 2 0 0 2 2 0 1 0 1 0 2 0 1 2 1 2 2 1 2 0 0 1 1 2 1 1 1 1 2 0 1 2 2 2 2 0 1 0 2 2 0 2 0 0 2 2 2 0 2 0 1 2 1 0 1 2 1 1 0 0 0 2 0 1 1 0 2 1 0 0 2 2 2 2 1 2 1 2 2 0 0 0 0 2 1 1 0 1 1 0 1 1 1 2 1 0 1 1 0 2 1 1 1 0 0 0 0 0 1 0 0 2 0 1 2 1 1 1 2 1 1 2 0 0 0 0 1 0 2 0 0 2 0 2 2 0 2 1 1 0 2 2 1 0 0 0 0 0 2 0 2 1 0 2 0 0 2 2 0 0 0 0 0 0 0 2 2 2 0 2 2 0 1 1 0 0 0 1 2 0 1 0 1 1 0 2 2 2 2 2 2 2 2 0 2 1 0 1 1 1 2 1 0 0 2 2)
CL-USER(36): (train4smile)
4
CL-USER(37): (pass-all-tests smile)
T
CL-USER(38): thresholdN
20.25
CL-USER(39): neuron-w
#(0.0 2.0 0.0 0.0 2.0 2.0 0.0 1.0 0.0 1.0 0.0 2.0 0.0 1.0 2.0 1.0 2.0 2.0 1.0 2.0 0.0 0.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0 2.0 0.0 1.0 2.0 2.0 2.0 2.0 0.0 1.0 0.0 2.0 2.0 0.0 2.0 0.0 0.0 2.0 2.0 2.0 0.0 2.0 0.0 1.0 2.75 1.0 0.75 1.0 2.0 1.0 1.0 0.0 0.75 0.0 2.75 0.0 1.0 1.0 0.0 2.0 1.0 0.0 0.0 2.0 2.0 2.0 2.0 1.0 2.0 1.0 2.0 2.0 0.0 0.0 0.0 0.0 2.0 1.0 1.0 0.0 1.0 1.0 0.0 1.0 1.0 1.0 2.0 1.0 0.0 1.0 1.0 0.0 2.0 1.0 1.75 1.0 0.75 0.0 0.0 0.0 0.0 1.0 0.0 0.0 2.0 0.0 1.0 2.0 1.0 1.0 1.0 2.0 1.0 1.0 2.0 0.0 0.0 0.0 0.0 1.0 0.0 2.0 0.0 0.0 2.0 0.0 2.0 2.0 0.0 2.0 1.0 1.0 0.0 2.0 2.75 1.0 0.0 0.0 0.0 0.0 0.0 2.0 0.0 2.0 1.0 0.0 2.0 0.0 0.75 2.0 2.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 2.0 2.75 2.0 0.0 2.0 2.0 0.0 1.0 1.0 0.0 0.0 0.75 1.0 2.0 0.0 1.0 0.0 1.0 1.0 0.0 2.0 2.0 2.0 2.0 2.0 2.0 2.75 2.0 0.75 2.0 1.75 0.0 1.75 1.0 1.0 2.0 1.0 0.0 0.0 2.0 2.0)
CL-USER(40): (train4neutral)
7
CL-USER(41): (pass-all-tests neutral)
T
CL-USER(42): thresholdN
20.25
CL-USER(43): (train4frown)
5
CL-USER(44): (pass-all-tests frown)
T
CL-USER(45): (setf *epsilon* 0.1)
0.1
CL-USER(46): (train4mixed)
23
CL-USER(47): thresholdN
20.25
CL-USER(48): (pass-all-tests mixed)
T
CL-USER(49):
|#
