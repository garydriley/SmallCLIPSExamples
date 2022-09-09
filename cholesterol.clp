;;; CLIPS Website
;;; clipsrules.net

;;; CLIPS on SourceForge
;;; sourceforge.net/projects/clipsrules/

;;; Adventures in Rule-Based Programming: A CLIPS Tutorial
;;; clipsrules.net/airbp

;;; Sample Run 
;;;
;;; CLIPS> (load cholesterol.clp)
;;; %*********
;;; TRUE
;;; CLIPS> (reset)
;;; CLIPS> (run)
;;; What is your name? Fred
;;; Fred, what is your gender? male
;;; Fred, what is your total cholesterol? 180
;;; Fred, what is your HDL? 50
;;; Fred, you have a moderate risk of heart disease.
;;; CLIPS> 

(deftemplate attribute
   (slot name)
   (slot value))

(defrule get-name
   =>
   (printout t "What is your name? ")
   (assert (attribute (name name) (value (read)))))
   
(defrule get-gender
   (attribute (name name) (value ?name))
   (not (attribute (name gender)))
   =>
   (printout t ?name ", what is your gender? ")
   (assert (attribute (name gender) (value (read)))))
   
(defrule get-total-cholestorol
   (attribute (name name) (value ?name))
   (not (attribute (name total-cholesterol)))
   =>
   (printout t ?name ", what is your total cholesterol? ")
   (assert (attribute (name total-cholesterol) (value (read)))))
   
(defrule get-HDL
   (attribute (name name) (value ?name))
   (not (attribute (name HDL)))
   =>
   (printout t ?name ", what is your HDL? ")
   (assert (attribute (name HDL) (value (read)))))
      
(defrule bad-answer
   (declare (salience 10))
   (or ?a <- (attribute (name gender) (value ~male&~female))
       ?a <- (attribute (name total-cholesterol | HDL)
                        (value ?value&:(or (not (numberp ?value))
                                           (<= ?value 0)))))
   =>
   (retract ?a))

(defrule compute-ratio
   (attribute (name total-cholesterol) (value ?total))
   (attribute (name HDL) (value ?hdl))
   =>
   (assert (attribute (name ratio) (value (/ ?total ?hdl)))))
   
(defrule low-ratio
   (attribute (name name) (value ?name))
   (or (and (attribute (name gender) (value male))
            (attribute (name ratio) (value ?ratio&:(< ?ratio 3.5))))
       (and (attribute (name gender) (value female))
            (attribute (name ratio) (value ?ratio&:(< ?ratio 3.0)))))
   =>
   (printout t ?name ", you have a low risk of heart disease." crlf))

(defrule moderate-ratio
   (attribute (name name) (value ?name))
   (or (and (attribute (name gender) (value male))
            (attribute (name ratio) (value ?ratio&:(>= ?ratio 3.5)&:(<= ?ratio 5.0))))
       (and (attribute (name gender) (value female))
            (attribute (name ratio) (value ?ratio&:(>= ?ratio 3.0)&:(<= ?ratio 4.4)))))
   =>
   (printout t ?name ", you have a moderate risk of heart disease." crlf))
   
(defrule high-ratio
   (attribute (name name) (value ?name))
   (or (and (attribute (name gender) (value male))
            (attribute (name ratio) (value ?ratio&:(> ?ratio 5.0))))
       (and (attribute (name gender) (value female))
            (attribute (name ratio) (value ?ratio&:(> ?ratio 4.4)))))
   =>
   (printout t ?name ", you have a high risk of heart disease." crlf))