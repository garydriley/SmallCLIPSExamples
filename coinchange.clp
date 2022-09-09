;;; CLIPS Website
;;; clipsrules.net

;;; CLIPS on SourceForge
;;; sourceforge.net/projects/clipsrules/

;;; Adventures in Rule-Based Programming: A CLIPS Tutorial
;;; clipsrules.net/airbp

;;; Sample Run 
;;;
;;; CLIPS> (load "coinchange.clp")
;;; :%%%*************
;;; TRUE
;;; CLIPS> (reset)
;;; CLIPS> (run)
;;; Denominations: 1 3 4
;;; Amount of change: 6
;;; Fewest coins: 3 3
;;; CLIPS> (reset)
;;; CLIPS> (run)
;;; Denominations: 1 5 10 25
;;; Amount of change: 123
;;; Fewest coins: 25 25 25 25 10 10 1 1 1
;;; CLIPS> (reset)
;;; CLIPS> (run)
;;; Denominations: 3 4
;;; Amount of change: 5
;;; Exact change can't be given.
;;; CLIPS> 

;;;
;;; Deftemplates and Defglobals
;;;

(defglobal ?*high* = 10)

(deftemplate denominations
   (multislot values))
   
(deftemplate change
   (slot value))
   
(deftemplate state
   (slot amount)
   (multislot branches)
   (multislot coins)
   (slot coin-count (default 0)))

;;;
;;; Get Denominations
;;;

(defrule get-denominations
   (not (denominations))
   =>
   (print "Denominations: ")
   (assert (denominations (values (explode$ (readline))))))

(defrule bad-denomination
   (declare (salience ?*high*))
   ?d <- (denominations)
   (or (denominations (values))
       (denominations (values $? ?v $? ?v $?))
       (denominations (values $? ?v&:(or (not (integerp ?v)) (<= ?v 0)) $?)))
   =>
   (println "There must be at least one denomination, denominations must")
   (println "be positives integers, and they cannot be repeated.")
   (retract ?d))
   
(defrule sort-denominations
   ?d <- (denominations (values $?values))
   =>
   (modify ?d (values (sort < ?values))))

;;;
;;; Get Change
;;;
  
(defrule get-change
   (not (change))
   =>
   (print "Amount of change: ")
   (assert (change (value (read)))))
 
(defrule bad-change
   (declare (salience ?*high*))
   ?c <- (change (value ?v&:(or (not (integerp ?v)) (<= ?v 0))))
   =>
   (println "Change must be a positive integer.")
   (retract ?c))

;;;   
;;; Search Rules
;;;

(defrule start
   (denominations (values $?denominations))
   (change (value ?value))
   =>
   (assert (state (amount ?value) (branches ?denominations))))
   
(defrule branch-less-than
   ?s <- (state (amount ?amount&~0)
                (branches ?v&:(< ?v ?amount) $?rest)
                (coins $?coins)
                (coin-count ?count))
   =>
   (modify ?s (branches ?rest))
   (assert (state (amount (- ?amount ?v))
                  (branches ?v ?rest)
                  (coins ?coins ?v)
                  (coin-count (+ ?count 1)))))

(defrule branch-equal
   ?s <- (state (amount ?amount&~0)
                (branches ?amount $?rest)
                (coins $?coins)
                (coin-count ?count))
   =>
   (modify ?s (branches))
   (assert (state (amount 0)
                  (coins ?coins ?amount)
                  (coin-count (+ ?count 1)))))
            
(defrule no-branch-greater-than
   ?s <- (state (amount ?amount&~0)
                (branches ?v&:(> ?v ?amount) $?rest))
   =>
   (modify ?s (branches ?rest)))

;;;   
;;; Prune Search
;;;

(defrule prune-branch
   (declare (salience ?*high*))
   ?s <- (state (amount ~0)
                (branches))
   =>
   (retract ?s))

(defrule better-branch 
   (declare (salience ?*high*))
   (state (amount 0)
          (coin-count ?count1))
   ?s <- (state (amount ?amount2) 
                (coin-count ?count2))  
   (test (or (> ?count2 ?count1)
             (and (> ?amount2 0)
                  (= ?count2 ?count1))))
   =>
   (retract ?s))

;;;   
;;; Print Results
;;;

(defrule best-solution
   (state (amount 0) (coins $?coins))
   (not (state (branches ? $?)))
   =>
   (println "Fewest coins: " (implode$ ?coins)))

(defrule no-solution
   (not (state (amount 0)))
   (not (state (branches ? $?)))
   =>
   (println "Exact change can't be given."))
