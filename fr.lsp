

(setq 
      COCAINE-DEALER-ENG-PATS '(
         (TEACH (SL-NAME AGENT) (PHRASE TEACHES) (SL-NAME OBJECT) (PHRASE AT)  
                (SL-NAME LOC) (PHRASE TO) (SL-NAME RECIP))
         (HUMAN (D-TREE DTR-1))
         (CHEM (PHRASE CHEMISTRY))
         (HS (SL-NAME REF) (PHRASE HIGH SCHOOL))
         (STATE (D-TREE DTR-2))
         (INFORM (SL-NAME AGENT) (PHRASE TOLD) (SL-NAME RECIP) (PHRASE THAT) 
                 (SL-NAME OBJECT))
         (CANCER (SL-NAME TYPE) (PHRASE CANCER))
         (INGEST (D-TREE DTR-3))
         (NOSE (PHRASE NOSE OF) (SL-NAME PARTOF))
         (DEF (PHRASE THE))
      )
      
      EX21-A-CON '(
            TEACH AGENT (GEORGE)
                   OBJECT (CHEM)
                   LOC (HS REF (DEF))
                   RECIP (STUDENTS)
      )

      Q-CON* '(QUESTION)
)


; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; FUNCTION: FR-TO-ENG
; PURPOSE:  Translates a conceptual Q&A system answer into its English, human-
;           readable sentence equivalent.
; INPUT:    E-PATS: English patterns that take frame predicates and provide
;                   templates for how to translate the given frame
;           C-ANS: A conceptual frame answer to a question posed to our Q&A
;                  system. Assume this has been derived by a hypothetical
;                  inference system not shown.
;           Q-CON: A conceptual frame question created from a user-posed query.
; OUTPUT:   English sentence translation of C-ANS
(defun MATCHING-PATTERN (main pats)
    (loop for x in pats do
        (if (equal main (car x))
            (return (rest x))
        )
    )
)

(defun LIST-CONTAINS-A-LIST (lst)
    (loop for x in lst do
        (if (listp x)
            (return T))))

(defun SL-NAME-HANDLER (c-ans word e-pats)
    (let ((truthValue nil) (ref nil))
        (loop for x in c-ans do
            (if (and (equal truthValue T) (LIST-CONTAINS-A-List x))
                (return (FR-TO-ENG e-pats x nil))
            )
            (if (equal truthValue T)
                (return x)
            )
            (if (and (not (listp x)) (equal x word))
                (setq truthValue T)
                (setq ref x)
            )
        )
    )
)

(defun D-TREE-HANDLER (dtr)
	(EVAL-D-TREE dtr)
)

(defun PHRASE-HANDLER (word)
    (cdr word)
)

(defun FR-TO-ENG (e-pats c-ans q-con)
    (let ((returnFrame nil) (entireFrame nil) (main (car c-ans)))
    	(setq entireFrame (MATCHING-PATTERN main e-pats))
    	(loop for x in entireFrame do
    		(print x)
    	
    		(cond
    			(
    				(equal (car x) 'SL-NAME)
    				(setq returnFrame (append returnFrame (SL-NAME-HANDLER (rest c-ans) (second x) e-pats)))
    			)
    			(
    				(equal (car x) 'PHRASE)
    				(setq returnFrame (append returnFrame (PHRASE-HANDLER x)))
    			)
    			(
    				(eqaul (car x) 'D-TREE)
    				(setq returnFrame (append returnFrame (D-TREE-HANDLER (second x))))
    			)
    			
    			
    		)
    	)
     
     
     	(return-from FR-TO-ENG returnFrame)
    )
)


(FR-TO-ENG COCAINE-DEALER-ENG-PATS EX21-A-CON Q-CON*)



