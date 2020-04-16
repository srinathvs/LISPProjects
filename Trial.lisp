(write-line (+ 2 3))

(format t "~%To run the factioral program: (factorial 5)~%")

(defun factorial (x)
  (if (eql x 0)
    1
    (* x (factorial (- x 1)))))
