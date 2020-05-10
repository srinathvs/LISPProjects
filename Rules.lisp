#| Rules.lisp for simple rule-based system toy with limping-along unification pattern matcher.

2020-04-25:

Clark Elliott Copyright 2020 all rights reserved.

Note: I wrote this on a Saturday for fun, and to use with class. In fact it is working well
enough to demonstrate how rule-based systems work, but the nested unification and backwards
fail for multiply-bound variables is not fully working. That is, it will give up on matching sometimes before it
should.

As long as you keep the connectives AND, OR, NOT with variables in them straightforward (that
is, stick with top-level iteration (AND) / recursion (And (OR..))) and put your
predicates (is-user ?u) first on the LHS you'll be OK.

Also, I'm not sure the priorities of rules are being fully applied after the initial sort.

No RETE or other efficiencies included: intended as a transparent pedagogical device.


A very nice feature is that we can run any LISP functions (such as Animal) on the RHS.
We could load in all of the different toy programs we have and run them.

In fact, though this is just a toy, it is capable of very sophisticated processing. It has two
additional features that make it quite powerful: (1) It is trivial to call native LISP functions from
the RHS of rules, and (2) we can dynamically define rules WHILE the system is running.

Because there is no RETE algorithm employed, with any complexity it will run slowly, but we can greatly
speed it up even so by compiling the program if we care to.


-----------------------------------------------------------------------------

LHS Restrictions:

Cannot use connectives with variables (not (something ?x)) until AFTER ?x is bound/declared: (is-male ?x)

So, put all the predicates at the top of the LHS.

Note to CDE: A possible solution is to simply extract all of the variables, and add them as declarations at the
top of the list. This would be relatively efficient because we will use subst-vars on the later occurrence
anyway. But: I'm trying to keep this simple enough to be mostly readable by a novice.

Or, of course, work out the full iteration/recursion for complete fail-back retries on all variables.

I think a further restriction is that you can't reliably have two connectives in a connective with
nested variables. The problem has to do with recursive failure at two different points in an AND list.`

-----------------------------------------------------------------------------
INSTRUCTIONS:

In LISP:

1. Load this file
2. Load your rule set  (typically in a .lisp file also)

Example:

LISP> (load "Rules")   <-- loading Rules.lisp, this program
LISP> (load "Romance") <-- loading Romance.lisp, the set of rules within a domain

(run-all) to run
(run-once) to run one rule

> Future choice: perhaps we would like to ALWAYS fire the most specific rule
first, or highest priority first, then go back to the beginning of the
rules each time. For the moment, the rules are sorted by priority.

For now we just go all the way through all the rules.

To extend:

When calling LISP functions from the right-hand side I should also just pass the LHS bindings as well.
Trivial to do this with a key value.

Might like to add arbitrary lisp-function predicates on LHS that can accept variables as input:
(f-SomeFun ?x ?y). Returns t or nil.

Can use LISP functions to manipulate working memory directly. This way, for example, it is
easy to set a limit to how many times a rule can fire, or get work done, or communicate with the web.

Probably can also execute commands to add and remove rules along the way. This could be useful and
an efficiency as well. Kind of nice to have a self-configuring system.

|#

#| Rules each have a name, a left hand, and a right hand.
LHS is a list of tokens.
RHS is a list of actions, each of which is a list
|#

;;; Assuming the name of the file is r5.lisp:
(defun ll () (load "r5") 'r5) ; Easy to load r5.lisp during development. Just type: (ll)

;;; Be careful! The macro character reader does NOT work in debug mode. Only at the top level.
;;; You might not know why the system is forming no matches...

(set-macro-character #\? ; Part of the matcher. Change ?x from input stream to (*var* x).
  #'(lambda(stream char)         
      (list '*var* (read stream t nil t))))

(setf *wm* '(empty)) ; Our working memory
(setf *rules* nil)   ; Our collection of rules
(setf *debug* nil)
(setf *show-fires* nil)
;; (setf *show-fires* t) ; Change to "t" if you want to see the rules firing.

(defun debug-on () (setf *debug* t))
(defun debug-off () (setf *debug* nil))

(defun reset ()
  (setf *wm* '(empty start))
  (setf *rules* nil))

(defun add-rule (name priority lh rh) 
  (setf *rules* (cons (list name priority lh rh) *rules*))
  (format t "Rule added: ~s ~s~%" name priority))

;;; Sort the rules by priority. Higher number means you get to go first.
(defun sort-rules ()
  (setq *rules* 
	(sort *rules*
	      #'(lambda (x y) (> (second x) (second y))))))
  
(defmacro defrule (name priority lh arrow rh) 
  `(add-rule  ',name ',priority ',lh ',rh))

(defun run-once () (mapcar #'process-rule *rules*))

(defun run-all ()
  (sort-rules) ; can move into initialization...
  (let ((count 0))
    (loop
       (incf count)
       (when (> count 10)
	 (format t "Forced break at 10!~%~%")
	 (return))
       (progn
	 (setf *rule-fired* nil)
	 (mapcar #'process-rule *rules*)
	 (when (not *rule-fired*) (return))))))

(defun name-of (rule) (first rule)) 
(defun priority-of (rule) second rule) ; A number.
(defun lhs-of (rule) (third rule)) 
(defun rhs-of (rule) (fourth rule))

(defun process-rule (rule)
  (let ((temp) (bb nil))
    (when *debug* (format t "Rule: ~s " (name-of rule)))

    (setf temp (multiple-value-list ; Match the rule's LHS against WM, start with bindings = nil.q
		(match-wm (cons 'AND (lhs-of rule)) nil))) 

    (when (first temp) ; LHS matches, possibly with bindings
      (setf *rule-fired* t) ; Continue iteration of system because a rule fired.
      (when (or *show-fires* *debug*) (format t "~%~a fired!~%" (name-of rule)))
      (mapcar  #'(lambda (action-item) ; Execute all the actions on the RHS
		   (execute-action action-item (second temp)))
	       (rhs-of rule)))
    (when *debug* (format t "~%"))))

#| MATCH-WM The main iterating recursive matching algorithm, with unification, for the LHS of rules |#
(defun match-wm (lhs-item bb) 
  (let ((all-ok t) (or-test nil) (temp nil) (ground-pattern nil))
    (cond
      ((atom lhs-item)
       (setf all-ok (member lhs-item *wm*))) ; Constant must be in WM
      ((equalp (first lhs-item) 'NOT)
       ;;(format t "This is NOT: ~s, ~s~%" lhs-item bb)
       (setf temp (multiple-value-list
		   (match-wm (second lhs-item) bb)))
       (setf all-ok (not (first temp))) ; opposite of success
       (setf bb (second temp))) ; always update the bindings

      ((equalp (first lhs-item) 'OR)
       (when *debug* (format t "This is OR: ~s~%" lhs-item))
       (setf all-ok nil) ; just need one to be true...
       
       (block or-block
	 (mapcar #'(lambda (or-item)
		     (setf temp (multiple-value-list
				 (match-wm (subst-vars or-item bb) bb))) ;look for ANY match with our bindings.
		     (when (first temp)
		       (setf bb (second temp))  ; Update the bindings on first success only
		       (setf all-ok t)
		       (return-from or-block))) ; ...so don't need to check further.
		 (rest lhs-item))))

      ((equalp (first lhs-item) 'AND)
       (when *debug* (format t "This is AND: ~s~%" lhs-item))
       ;;(format t "This is AND: ~s b: ~s~%" lhs-item bb)

       (block and-block
	 (when (null (second lhs-item))
	   (setf all-ok t)
	   (return-from and-block))
	 (setf all-ok nil) ; If head and tail succeed then change to all-ok true.
	 (let ((head (second lhs-item)) (tail (cddr lhs-item)) (head-success nil) (tail-success nil)
	       (bb2 nil))
	   (mapcar #'(lambda (wm-item)
		       ;; (format t "AND head: ~s, tail: ~s, wm-item: ~s ~%~%" head tail wm-item)
		       (cond
			 ((wm-pattern-p head)
			  (setf head-success
				(multiple-value-list
				 (match-with-bindings head (subst-vars wm-item bb) bb))))
			 (t
			  (setf head-success
				(multiple-value-list
				 (match-wm head bb))))
			 ;;(format t "head-success: ~a~%" head-success)
			 )
		       (when (first head-success)
			 (setf bb2 (second head-success))
			 (setf tail-success
			       (multiple-value-list
				(match-wm (cons 'and (subst-vars tail bb2)) bb2)))
			 (when (first tail-success)
			   (setf bb (second tail-success))
			   (setf all-ok t)
			   (return-from and-block))))
		   *wm*))))
      (t
       
       (block pat ; Is a working memory pattern token.
	 (let ((ground-item nil))
	   (mapcar #'(lambda (wm-item)
		       (setf ground-item (subst-vars lhs-item bb)) ; might still have vars...
		       (setf temp (multiple-value-list
				   (match-with-bindings ground-item wm-item bb)))
		       ;;(format t "~%Temp: ~a, bind: ~a~%" (first temp) (second temp))
		       (when (first temp)
			 (setf all-ok t)
			 (setf bb (second temp))
			 (return-from pat))) ; We got a match with WM.
		   *wm*))
	 (setf all-ok nil)))) ; Nothing in WM matched.
    (when *debug* (format t "Bindings: ~a~%" bb))
    ;; (format t "lhs-item: ~s, All-ok: ~s, bb: ~s~%~%" lhs-item all-ok bb)
    (values all-ok bb)))

(defun wm-pattern-p (p)
  (cond
    ((atom p) t) ; Constants are wm patterns.
    (t
     (let ((x (first p)))
       (cond
	 ((or  (eql x 'AND) (eql x 'OR) (eql x 'NOT)) nil) ; Connectives are not a wm patterns
	 (t t)))))) ; A list that is not a connective is a wm pattern

(defun add-wm (thing) 
  (when *debug* (format t "~S ==> *WM* ~%" thing))
  (setf *wm* (append (list thing) *wm*))) ; Would build rete here.

(defun remove-wm (thing) 
  (when *debug* (format t "~S <== *WM* ~%" thing))
  (setf *wm* (remove thing *wm* :count 1 :test #'equal)))

#| 2020-04-25: cde: This seems to be working. Might want to rename the actions because print and remove
are LISP functions. |# 
(defun execute-action (action-raw bb)
  (let ((action (subst-vars action-raw bb))) ; Substitute from the LHS bindings.
    (when *debug* (print action))
    (cond
      ((equalp (first action) 'add) 
       (mapcar #'add-wm (rest action)))
      ((equalp (first action) 'remove) 
       (mapcar #'remove-wm (rest action)))
      ((equalp (first action) 'print) 
       (format t "~%~a~%" (first (rest action))) nil)
      ((equalp (first action) 'ask)
       (format t "~%~s~%" (second action)) ; Ask the question, then perform all the actions for Y or N.
       (cond 
	 ((y-or-n-p)
	  (mapcar #'(lambda(act)
		      (execute-action act bb))
		  (yes-branch action)))
	 (t
	  (mapcar #'(lambda(act)
		      (execute-action act bb))
		  (no-branch action)))))
      (t
       (when *debug* (format t "~%Action: ~s~%" action))
       (eval action))) ; Allows for the arbitrary execution of lisp code :-)
    ))

(defun yes-branch (action) (third action))
(defun no-branch (action) (fourth action))

(defun xx ()
  (setf *wm* '(start empty))
  (evaluate-actions '((print 6))))

(format t "Can use (debug-on), (debug-off), (reset), and *wm*. (run-once) will step through cycles.~%")
(format t "Load your rule set, then AT THE TOP LEVEL issue (run-all)~%")

#| Match.lisp

Clark Elliott 2019-04-23

Quick throw-away matching code for CSC594 Topics in AI course.

Examples:

CL-USER(13): (match '(a) '(?X))
T
(((*VAR* X) A))

CL-USER(14): (match '(a ?X) '(?x a))
T
(((*VAR* X) A))

CL-USER(15): 
(match '(a ?x z) '(?x a ?y))
T
(((*VAR* Y) Z) ((*VAR* X) A))

CL-USER(16): 

(match '(?p (b ?z) ?z (b 4)) '(?x ?y 4 ?x))

(match '(B 3 (?y ?B)) '(?X 3 (?y ?X)))

(match '(B 3 (?y ?B)) '(?X 3 (y ?X)))

(match '(B 3 (Y B)) '(?X 3 (?x ?x)))

(match '(y 3 (?y x)) '(?X 3 (?y ?X)))

Some of these functions may not be necessary. This is throw-away utility code.

|#

#|

VARIABLE READ MACRO   BE CAREFUL!!

Translates ?x into (*var* x) for easy of writing statements.

Note: Moved up to the top of this file.

(set-macro-character #\? ; Part of the matcher. Change ?x from input stream to (*var* x).
  #'(lambda(stream char)         
      (list '*var* (read stream t nil t))))

This read macro is not in effect when in debug mode. This means that interactively loaded functions will
not be correctly interpreted regarding the variables when in a sub-error-level.

|#

#|
THE MATCHER FUNCTIONS:

Return T or nil. When T then T/bindings are returned as multiple values, else just nil (with
the second value defaulting to nil on multiple-value-list or -bind).

We return multiple values from the matcher. The first value is the success of the match (t or
nil), the second value is the bindings (which can also be nil, if only constants were used in the
match).

The point is: if we just return nil, we don't know whether the match succeeded with no
bindings, or the match failed. The Multiple values solves this problem.

|#

(defun match (pat1 pat2)
  (match-with-bindings pat1 pat2 nil))

(defun match-with-bindings (pat1 pat2 bb)
  (cond
    ((pattern-var-p pat1)
     (variable-match pat1 pat2 bb))
    ((pattern-var-p pat2)
     (variable-match pat2 pat1 bb))
    ((atom pat1) ; No variables, so constant must match a constant
     (when (eq pat1 pat2) (values t bb)))
    ((atom pat2) nil) ; first is not atom, so fail
    ;;((atom pat2) (values nil bb) ; I don't want to screw with this, but prefer to send back values
    (t
     (multiple-value-bind
	   (flag carbindings)
	 (match-with-bindings (car pat1) (car pat2) bb) ; Succeed so capture bindings
       (and flag
	    (match-with-bindings (cdr pat1) (cdr pat2) carbindings))))))
     
(defun variable-match (pattern-var item bb)
  (if (equal pattern-var item) (values t bb)      ; var/var
      (let ((var-binding (get-binding pattern-var bb)))
	(cond
	  ((is-binding pattern-var bb)
	   (match-with-bindings var-binding item bb))
	  ((not (contained-in pattern-var item bb))    ;; occurs check
	   (values t (add-binding pattern-var item bb)))))))
	 
(defun contained-in (pattern-var item bb)
  (cond
    ((atom item) nil)
    ((pattern-var-p item)
     (or (equal pattern-var item)
	 (contained-in pattern-var (get-binding item bb) bb)))
    (t
     (or (contained-in pattern-var (car item) bb)
	 (contained-in pattern-var (cdr item) bb)))))

;;; Example: CL-USER(38): (subst-vars '(a (*var* b)) '(((*var* b) 44)))
;;; (A 44)

(defun subst-vars (item bindings)
  (cond ((atom item) item)
	((pattern-var-p item)
	 (let ((binding (get-binding item bindings)))
	   ;(format t "bb / binding is>>: ~s ~S~%" bind binding)
	   (if (is-binding item bindings)
	       (subst-vars binding bindings)
	       item)))
	(t (cons (subst-vars (car item) bindings)
		 (subst-vars (cdr item) bindings)))))

(defun add-binding (pattern-var item bindings)
  (cons (list pattern-var item) bindings))

(defun add-whole-binding (bind-cell bindings)
  (cons bind-cell bindings))


;;; GET-BIINDING
;;;     Avoiding a problem here. If variable is bound to nil, then we don't know
;;; if there is a binding or not. The above code has been altered to
;;; allow retrieval of "nil" from the variable bindings. IS-BINDING
;;; is used to discriminate between un-bound vabiables, and those
;;; bound to nil. 

(defun get-binding (pattern-var bindings)
  (cadr (assoc pattern-var bindings :test #'equal)))

;;; So...

(defun is-binding (pattern-var bindings)
  (assoc pattern-var bindings :test #'equal))

(defun pattern-var-p (item)      ; test is good enough unless foolish.
  (cond
    ((and
     (listp item)
     (equal (car item) '*var*)))))

;;; End of unification matcher code.
