#|

2020-04-25: Clark Elliott

Romance.lisp

This is a Saturday-afternoon programmed toy rules system with some silly advice about romance.

For the real thing, install CLIPS and build a real system. In that case we would also want a
socket interface into it if we can find or build one, which gives us network communication with other
system and with users over the web. This then becomes an advice itelligent agent, assuming it maintains state
with its users.

Here we are playing with the toy system as an illustration of capturing declarative rules in a domain.


Idea:

I borrowed a few simple ideas from the shared Human Romantic Love start on a content theory we created, and
place these common-sense ideas into a few rules. No claims of importance or social correctness!

What sort of interaction are two people likely to have?

What differences will they have?

What compatibilities?

What bad things can happen in love?

If we wanted to, we could extend this to a dating / advice application where through socket interface
users enter their preferences.

I have added paraphrased one single story from the shared document that is indexed by a key token. We could
collect hundreds (along with many indexes each) and deliver them at appropriate junctures in the interaction.

Note: If you extend this, keep in mind that inside quotes, lisp is forgiving, but inside of lists
then commas, quotes, periods and question markes have programmatic value.

|#

(setf *wm* '(empty start))
(setf *rules* nil)
(defun r () (load "romance")) ; Easy to load this file romance.lisp: (r)

(defrule like-some-advice 3
  (start)
  -->
  (
   (remove start)
   (ask "Do you want some romance advice?"
	((print "Giving advice now...")
	 (ask "You can build a profile manually. But would you like me to build it for you (recommended)?"
	      ((build-user))
	      ((add gather-data)))
	 (add load-data))
	((print "O.K. well, next time then!")))))

;;; Not using any of this at the moment:
(defrule GatherData 20
  (gather-data
   )
  -->
  ((remove gather-data)
   (add ask-stories-happy)
   (add ask-stories-sad)
   (add ask-gender)
   (add self-description)))

;;; Or this:
(defrule TypesOfStories 20
  (gather-datax
   ask-stories-happy)
  -->
  ((remove ask-stories-happy)
   (ask "Do you like happy romance stories?"
	((print "OK adding some happy stories...") (add stories-happy))
	((print "OK. No happy romance stories then.")))))

;;; Or this:
(defrule TypesOfStories 20
  (gather-datax
   ask-stories-sad
   )
  -->
  ((remove ask-stories-sad)
   (ask "Do you like happy romance stories?"
	((print "OK adding some happy stories...") (add stories-sad))
	((print "OK. No happy romance stories then.")))))

(defrule self-description 20
  (
   self-description
   )
  -->
  ((remove self-description)
   (gather-description)))


(defrule KillGatherData 5 ; low priority, goes last
  (
   gather-data
   )
  -->
  (
   (remove gather-data)))

(defrule StopLoadingdata 5 ; lowest priority.
  (load-data)
  -->
  ((print "Done loading data...")
   (remove load-data)))


(defrule LoadTheData 10 ; low priority
  (load-data
   )
  -->
  ((print "Loading data...")
   (add check-not-compatible)
   (add create-other-user)))

(defrule CreateOtherUser 50
  (
   create-other-user)
  -->
  ((add (other-user leslie) (leslie pref young) (leslie pref smart)) ; Can add here
   (build-leslie) ; Or can call a lisp funcion to do it.
   (remove create-other-user) ; just run once.
   ))


;;; Sometimes people share similarities
(defrule SimilarToYou-B 55 ; From the B cycle, fire this one first.
  (
   (user ?u)
   (other-user ?o)
   (?u prefers ?x)
   (?o prefers ?x)
   (not (pref-check ?x))
   )
  -->
  (
   (add (pref-check ?x)) ; Don't repeat this preference
   (add (similar ?u ?o)) ; Are similar to one another
   (add (similar ?o ?u)) 
   (print (?u and ?o share the preference... ?x))
   )
  )

;;; Sometimes people are dissimilar in some ways
(defrule DisSimilarToYou-B 50 ; From the B cycle, fire this one second
  ((user ?u)
   (other-user ?o)
   (?u prefers ?x)
   (not (?o prefers ?x))
   (not (pref-dis-check ?x)))
  -->
  ((add (pref-dis-check ?x)) ; Don't repeat this preference
   (add (dissimilar ?u ?o)) ; Are similar to one another
   (add (dissimilar ?o ?u))
   (add (disagreement ?u ?o ?x))
   (print (?u has the preference ?x that ?o does not))))

;;; If one or the other wants a long-term relationship and their opposite wants a short-term relationship
;;; then they are not compatible.
(defrule NotCompatible-C1 45 ; From the C cylce, fire this one first.
  (check-not-compatible
   (other-user ?o)
   (user ?u)
   (or
    (and
     (?o prefers short-term-relationship)
     (?u prefers long-term-relationship))
    (and
     (?o prefers long-term-relationship)
     (?u prefers short-term-relationship))))
  -->
  ((print "When one prefers long-term and the other prefers short-term you aren't compatible")
   ))

;;; If one wants a long-term relationship and the other wants a short-term relationship
;;; the one who wants a long-term relationship will tend to get hurt
(defrule NotCompatible-C 40 ; From the C cylce, fire this one second.
  (check-not-compatible
   (other-user ?o)
   (user ?u)
   (?o prefers long-term-relationship)
   (?u prefers short-term-relationship))
  -->
  ((remove check-not-compatible)
   (add (hurt ?u ?o))
   (add (disagreement ?u ?o short-or-long-term))
   (print (Because ?o wants long term but ?u wants short term the relationship will probably hurt ?o))
   ))

;;; People who get hurt may get angry
(defrule HurtGetAngry-D 35
  ((hurt ?x ?y)
   )
  -->
  ((remove (hurt ?x ?y))
   (add disagreement ?x ?y hurt)
   (add (angry-at ?y ?x))
   (print (Because ?y is hurt... ?y might get angry at ?x))))

;;; It is not a good idea to get into a romance with someone who will be angry at you
;;; Plus it is not nice to make them unhappy.
(defrule LoveAdvice-D 33
  ((user ?u)
   (angry-at ?o ?u))
  -->
  ((print (Hey ?u))
   (ask "I have some advice for you. Would you like to hear it?"
       ((print (?u you should not form a relationship with ?o))
	(print (Why... because if you do ?o might get angry at you))
	(print (And angry people are unpleasant to be around. Plus why make ?o unhappy...)))
       ((print "OK. Sorry. It was good advice.")
	(add (no-accept-advice ?u about ?o))))
   (add (disagreement ?o ?u angry-about-short-term))
   (add tell-story)
   (add form-agreement)
   (remove (angry-at ?o ?u))))

;;; People who don't accept advice about romance have trouble working out problems.
(defrule NoAdvice-D 31
  ((user ?u)
   (no-accept-advice ?u about ?o))
  -->
  ((remove (no-accept-advice ?u about ?o))
   (print (My advice to ?o is to avoid a relationship with ?u because...))
   (print (...People like ?u who do not accept advice about romance cannot deal with problems and never change.))))

(defrule FormAgreement-E1 12
  (form-agreement
   (user ?u)
   (other-user ?o)
   (not (no-accept-advice ?u about ?o))
   (or
    (disagreement ?u ?o ?z)
    (disagreement ?o ?u ?z)))
  -->
  ((remove form-agreement)
   (add form-agreement2)
   (print "Of course even though you are not compatible. If the two of you still wanted a relationship...")
   (print "Then consider...")))

(defrule FormAgreement-E2 10
  (form-agreement2
   (disagreement ?u ?o ?z))
  -->
  ((print (?u and ?o should try to reach some agreement about their difference ?z))
   (remove (disagreement ?u ?o ?z))))

(defrule TellStory-F2 2 ; Very low priority. Should come last.
  (tell-story)
  -->
  ((remove tell-story)
   (tell-the-story)))



;;; Our LISP functions that get called on the RHS of rules:
;;; Note that we can also build rules dynamically while the system is running.

(defun gather-description ()
  (let ((user nil)(prefs nil)(self nil)
	(prefx '(beauty young healthy refined smart creative kind generous thoughtful nice short tall strong))
	(aversx '(bad-health old short tall smart uneducated mean coarse thrifty no-humor)))
    
    (format t "First, what is your name?~%")
    (setf user (read))
    (format t "Thanks ~s, now we'll get some information from you.~%~%" user)
    (format t "Enter tokens from each of the following lists that apply to your tastes. Then terminate~%")
    (format t "your data entry with a space then the slash character: / ~%~%")
    (format t "Preferences: ~s~%" prefx)
    (setf prefs (read-delimited-list #\/))
    (format t "Aversions: ~s~%" aversx)
    (setf avers (read-delimited-list #\/))
    (format t "Which of the above tokens apply to you?~%")
    (setf quals (read-delimited-list #\/))

    (mapcar
     #'(lambda (pref)
	 (setf temp (list user 'prefers pref))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     prefs)

    (mapcar
     #'(lambda (aver)
	 (setf temp (list user 'has-aversion aver))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     avers)

    (mapcar
     #'(lambda (qual)
	 (setf temp (list user 'has-quality qual))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     quals)
    *wm*
    (add-wm (list 'user user))
    )
  )

(defun build-leslie ()
  (let (
	(temp nil)
	(user 'leslie)
	(prefs '(creative kind nice short long-term-relationship))
	(avers '(bad-health old tall uneducated no-humor))
	(quals '(tall smart mean thoughtful generous)))
    (add-wm '(other-user leslie))
    (mapcar
     #'(lambda (pref)
	 (setf temp (list user 'prefers pref))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     prefs)

    (mapcar
     #'(lambda (aver)
	 (setf temp (list user 'has-aversion aver))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     avers)

    (mapcar
     #'(lambda (qual)
	 (setf temp (list user 'has-quality qual))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     quals)
    'leslie-built
    )
  )

(defun build-user ()
  (let (
	(temp nil)
	(user nil)
	(prefs '(creative kind nice tall short-term-relationship))
	(avers '(bad-health short uneducated no-humor))
	(quals '(tall smart nice generous)))
    (format t "OK. I'll build your profile. What is your first name?~%")
    (setf user (read))
    (add-wm (list 'user user))
    (mapcar
     #'(lambda (pref)
	 (setf temp (list user 'prefers pref))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     prefs)

    (mapcar
     #'(lambda (aver)
	 (setf temp (list user 'has-aversion aver))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     avers)

    (mapcar
     #'(lambda (qual)
	 (setf temp (list user 'has-quality qual))
	 (when (not (member temp *wm* :test #'equal))
	   (add-wm temp)))
     quals)
    'leslie-built
    )
  )

(setf stories
      '((short-or-long-term

       "Jill and Addison were in a relationship. Over time it became clear that Jill loved Addison, but
Addison did not love Jill back. This became a problem. There was resentment on both sides. It
turns out that Jill was always looking for a long-term relationship, but Addison was only
looking for a short-term relationship. They never discussed it and this is why there were
failed expectations on both sides. It would have been better if they had understood their
differences early on and formed some agreement about them.")
))

(defun tell-the-story ()
  (format t "~%Here is a relevant story:~%~%~a~%" (cadr (assoc 'short-or-long-term stories))))
	  
