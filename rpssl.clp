;;; CLIPS Website
;;; clipsrules.net

;;; CLIPS on SourceForge
;;; sourceforge.net/projects/clipsrules/

;;; Adventures in Rule-Based Programming: A CLIPS Tutorial
;;; clipsrules.net/airbp

;;; Sample Run 
;;;
;;; CLIPS> (load "rpssl.clp")
;;; %%%$*******
;;; TRUE
;;; CLIPS> (reset)
;;; CLIPS> (run)
;;; Rock Paper Scissors Spock Lizard
;;; Choose or quit: rock
;;; Rock crushes lizard!
;;; Choose or quit: paper
;;; Lizard eats paper!
;;; Choose or quit: scissors
;;; Rock crushes scissors!
;;; Choose or quit: spock
;;; Spock vaporizes rock!
;;; Choose or quit: lizard
;;; Lizard poisons Spock!
;;; Choose or quit: quit
;;; Games played: 5.
;;; I won 2. You won 3.
;;; CLIPS> 

(deftemplate wins
   (slot player (allowed-values me you ties))
   (slot count (default 0)))
   
(deftemplate result
   (slot item1)
   (slot verb)
   (slot item2))
   
(deftemplate choice
   (slot player (allowed-values me you))
   (slot value))
   
(deffacts initial
   (choices rock paper scissors spock lizard)
   (result (item1 Scissors) (verb cuts) (item2 paper))
   (result (item1 Paper) (verb covers) (item2 rock))
   (result (item1 Rock) (verb crushes) (item2 lizard))
   (result (item1 Lizard) (verb poisons) (item2 Spock))
   (result (item1 Spock) (verb smashes) (item2 scissors))
   (result (item1 Scissors) (verb decapitates) (item2 lizard))
   (result (item1 Lizard) (verb eats) (item2 paper))
   (result (item1 Paper) (verb disproves) (item2 Spock))
   (result (item1 Spock) (verb vaporizes) (item2 rock))
   (result (item1 Rock) (verb crushes) (item2 scissors))
   (wins (player me))
   (wins (player you))
   (wins (player ties)))

(defrule start
   (declare (salience 10))
   =>
   (println "Rock Paper Scissors Spock Lizard"))
   
(defrule your-choice
   (not (choice (player you)))
   =>
   (print "Choose or quit: ")
   (assert (choice (player you) 
                   (value (lowcase (read))))))

(defrule choice-bad
   ?c <- (choice (value ?choice&~quit))
   (not (choices $? ?choice $?))
   =>
   (retract ?c))
   
(defrule my-choice
   (not (choice (player me)))
   (choices $?choices)
   =>
   (bind ?random (+ 1 (mod (random) (length$ ?choices))))]
   (bind ?choice (nth$ ?random ?choices))
   (assert (choice (player me) (value ?choice))))
   
(defrule battle-winner
   ?c1 <- (choice (player ?p1) (value ?v1))
   ?c2 <- (choice (player ?p2) (value ?v2))
   (result (item1 ?i1) (verb ?verb) (item2 ?i2))
   (test (and (eq (lowcase ?i1) ?v1)
              (eq (lowcase ?i2) ?v2)))
   ?w <- (wins (player ?p1) (count ?count))
   =>
   (retract ?c1 ?c2)
   (modify ?w (count (+ 1 ?count)))
   (println ?i1 " " ?verb " " ?i2 "!"))

(defrule battle-tie
   ?c1 <- (choice (player ?p1) (value ?v))
   ?c2 <- (choice (player ?p2&~?p1) (value ?v))
   ?w <- (wins (player ties) (count ?count))
   =>
   (retract ?c1 ?c2)
   (modify ?w (count (+ 1 ?count)))
   (println "Tie!"))

(defrule quit
   (choice (value quit))
   (wins (player me) (count ?m))
   (wins (player you) (count ?y))
   (wins (player ties) (count ?t))
   =>
   (println "Games played: " (+ ?m ?y ?t) ".")
   (println "I won " ?m ". You won " ?y "."))
