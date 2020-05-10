#|

The Towers Of Hanoi for Common LISP

For fun, look here for the brilliant crazy nerd-person (my hero)  writing Towers of Hanoi:

http://www.kernelthread.com/projects/hanoi/

His projects inlcude Towers of Hanoi for car-audio MP3-player and an x86 bootable Hanoi operating system!

|#

#| Assignment:

Algorithm:

  To move the tower from A to C
  Move a 1-smaller tower from A to B
  Move the bottom disk to C
  Move the 1-smaller tower on top of it.

All of the LISP code to solve the Towers Of Hanoi problem for N disks is given, but with three
missing pieces of information.  Replace the missing information represented by ?x, ?y, ?z, ?p,
?q and ?r with the arguments from, to and spare. That is, for example, you might replace the
string "?x" with the string "from", and so on.

Hint: Start developing by testing (Hanoi 1) and (Hanoi 2).

|#


(defun hanoi-1(n from to spare)
  (cond
    ((> n 0) ;; Recursion base case is when your job is to move a tower of zero disks.
     (hanoi-1 (- n 1) from spare to)  ;; Otherwise move a 1-disk-smaller tower where you are NOT going.
     (format t "Move disk from ~s ==> ~s~&" from to) ;; Move the bottom disk where you ARE going.
     (hanoi-1 (- n 1) spare to from) ;; Move the smaller tower back on top of it.
     )
    )
  )


(defun hanoi (n)
  (format t "~%Moving ~s disk~p from tower A to tower C:~%" n n) ; ~p forms plural s
  (hanoi-1 n 'A 'C 'B)
  )


#|
Your output from the two functions above should like the following:

CL-USER(66): (hanoi 1)

Moving 1 disks from tower A to tower C:
Move disk from A ==> C
NIL

CL-USER(67): (hanoi 2)

Moving 2 disks from tower A to tower C:
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
NIL

CL-USER(68): (hanoi 3)

Moving 3 disks from tower A to tower C:
Move disk from A ==> C
Move disk from A ==> B
Move disk from C ==> B
Move disk from A ==> C
Move disk from B ==> A
Move disk from B ==> C
Move disk from A ==> C
NIL

CL-USER(69): (hanoi 4)

Moving 4 disks from tower A to tower C:
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
Move disk from A ==> B
Move disk from C ==> A
Move disk from C ==> B
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
Move disk from B ==> A
Move disk from C ==> A
Move disk from B ==> C
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
NIL
CL-USER(70):

|#
