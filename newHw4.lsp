; CS 161 Winter 2016: HW4 Solution

; [!] Include OUR HW3 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-3-solution using *our* path, which will likely
;     differ from yours (Note: HW3 also includes HW2, which includes HW1)
(load "hw-3-solution.lsp")





(defun TRANSLATE (list gPattern c-ans)
	(setq *CUR-FILLER c-ans)
	(let ((slot-type (car list)) )
		(cond
		;there are a total of three conditions
			((equal slot-type 'SL-NAME )
			;return the slot name
				(FR-TO-ENG-HELPER gPattern (GET-SF (nth 1 list) c-ans) )
			)
			((equal slot-type 'PHRASE )
			;return phrase
				(cdr list)
			)
			
			;return the Decision tree 
			((equal slot-type 'D-TREE )
				(let ((result (EVAL-D-TREE (eval (second list))) ) )
					(if (not (null result))
						
						(TRANSLATE2 result gPattern c-ans)
						
						nil
					)
				)
			)
			
			
		)
		
	)
)


(defun TRANSLATE2 (pattern e-pats c-ans)
	;given a list, which is a certain pattern, translate all of them to a list of English words
	(if (null pattern)
		(return-from TRANSLATE2 nil)
	)
	
	
	(let* ((target (car pattern)) (result (TRANSLATE target e-pats c-ans) ) )
		(if (or (null result) 
		
				(and 
					(= (length result) 1) 
					(null (car result)) 
				) 
			)
		
		
			(TRANSLATE2 (cdr pattern) e-pats c-ans)
			
			;recursively call the translate function
			
			(append 
					result (TRANSLATE2 (cdr pattern) e-pats c-ans )
			
			)
			
			
		)
	)

	
)



(defun MATCHING-PATTERN (gPattern word)

	;return null if it doesn't match
	;returns the pattern given the word
	
	
	(if (null gPattern)
	
		(return-from MATCHING-PATTERN nil)
		
	)
	
	(if (null word)
	
		(return-from MATCHING-PATTERN nil)
		
	)
	(let* ( (pattern (car gPattern)) )
	
		(if (not (equal (car pattern) word))
		
			(MATCHING-PATTERN (cdr gPattern) word)
			(cdr pattern)
			
		)

	)
)


(defun FR-TO-ENG (e-pats c-ans q-con)

    (setq *CUR-C-ANS C-ANS)
    
    (setq *CUR-Q-CON q-con)
    
    (setq *CUR-FILLER C-ANS)
    
    ;we use the helper to call the translate functions 
    (FR-TO-ENG-HELPER e-pats c-ans)
    
)



(defun FR-TO-ENG-HELPER (e-pats c-ans)
	(let* ( (predicate (car c-ans) ) 
			(pattern (MATCHING-PATTERN e-pats predicate) )  
			)
		;if the pattern is null, we just return the first of c-ans
		
		(if (null pattern)
		
			(list predicate)
			
			(TRANSLATE2 pattern e-pats c-ans)
			
		)
	)
)



;=============================================================================
; TEST 1

(setq 
      COCAINE-DEALER-ENG-PATS '(
         (TEACH (SL-NAME AGENT) (PHRASE TEACHES) (SL-NAME OBJECT) (PHRASE AT)  
                (SL-NAME LOC) (PHRASE TO) (SL-NAME RECIP))
         (HUMAN (D-TREE DTR-1))
         (CHEM (PHRASE CHEMISTRY))
         (HS (SL-NAME REF) (PHRASE HIGH SCHOOL))
         (PHYSICIAN (D-TREE DTR-4) (PHRASE IS) (SL-NAME OBJECT)
					(SL-NAME ROLE))
		
         (STATE (D-TREE DTR-2))
         (INFORM (SL-NAME AGENT) (PHRASE TOLD) (SL-NAME RECIP) (PHRASE THAT) 
                 (SL-NAME OBJECT))
         (CANCER (SL-NAME TYPE) (PHRASE CANCER))
         (INGEST (D-TREE DTR-3))
         (NOSE (PHRASE NOSE OF) (SL-NAME PARTOF))
         (DEF (PHRASE THE))
    		(ATTENDED (D-TREE DTR-4) (PHRASE WAS) (SL-NAME AGENT)(SL-NAME OBJECT))
    	(COMEDIAN (D-TREE DTR-4) (PHRASE MAKES ME LAUGH) (D-TREE DTR-4) (D-TREE DTR-4))
  		  (EXPLANATION (PHRASE IT WAS) (D-TREE DTR-5))
      )
      
      EX21-A-CON '(
        	TEACH AGENT (GEORGE)
				   OBJECT (CHEM)
				   LOC (HS REF (DEF)) 
				   RECIP (STUDENTS)
	  )

	  Q-CON* '(QUESTION)
	  
	  
	  
	  Q-CON1  '(ACT AGENT (HUMAN F-NAME (GEORGE)
GENDER (MALE)) LOC (HS REF (DEF))
CLASS (V WHAT?))
	  
	  
	  C-ANS-1 '(TEACH AGENT (HUMAN F-NAME (GEORGE)
GENDER (MALE)) RECIP (HUMAN TYPE (STUDENTS))
OBJECT (CHEM)
LOC (HS REF (DEF)) SITU (S1))

	Q-CON2 '(CAUSE ANTE (V WHY?)
CONSEQ (APPEAR OBJECT (LESIONS LOC (NOSE PARTOF
(HUMAN F-NAME (RICK)
GENDER (MALE))))))

		C-ANS-2 '(INGEST AGENT (HUMAN F-NAME (RICK)
GENDER (MALE)) OBJECT (COCAINE)
TO (NOSE PARTOF (HUMAN F-NAME (RICK) GENDER (MALE)))
SITU (S4))


	ENG-2 '(BECAUSE RICK PUT COCAINE INTO NOSE OF RICK)
	
	
	Q-CON3 '(ENABLES ANTE (V HOW?)
					CONSEQ (KNOWS AGENT (HUMAN F-NAME (GEORGE) 
												GENDER (MALE))
								OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
															 GENDER (MALE))
													OBJECT (DISEASED))))


	C-ANS-3 '(INFORM AGENT (HUMAN ROLE (ONCOLOGIST)
									REF (DEF))
					RECIP (HUMAN F-NAME (GEORGE)
								GENDER (MALE))
					OBJECT (STATE AGENT (HUMAN F-NAME (GEORGE)
											GENDER (MALE)) 
											OBJECT (CANCER TYPE (TERMINAL)))
				SITU (S3)
				)



	ENG-3 '(THE ONCOLOGIST TOLD GEORGE THAT HE HAS TERMINAL CANCER)



	
	  
)
;(print 
;(FR-TO-ENG COCAINE-DEALER-ENG-PATS EX21-A-CON Q-CON*)
;)
; SHOULD RETURN:
; (GEORGE TEACHES CHEMISTRY AT THE HIGH SCHOOL TO STUDENTS)

; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------


; FUNCTION: EVAL-D-TREE
; PURPOSE:  Takes the given decision tree, and uses the frames in the global
;           variables to return a replacement pattern, if any.
; INPUT:    D-TREE: a decision tree (NOT a symbol name for a decision tree,
;                   but the tree itself)
; OUTPUT:   Replacement pattern if a decision tree path is satisfied, else NIL.


(defun EVAL-D-TREE (d-tree)
    (if (null d-tree)
      (return-from EVAL-D-TREE nil)
    )
    (let ( (cur-path (car d-tree))  )
    
      (let ( (cur-eval (EVAL-FIRST (cdr cur-path) )) )
      
      
        (if (not (null cur-eval))
        
        
         cur-eval
         
          (EVAL-D-TREE (cdr d-tree) )
          
  
        )

      )
    )
)


(defun EVAL-FIRST (path)


  (if (null path)
    (return-from EVAL2 nil)
  )
  
  
  (if (= (length path) 1)
    (return-from EVAL-FIRST (eval (car path)))
  )
  
  
  (if (not (null (eval (car path) ) ))
    
    (EVAL-FIRST (cdr path))
    
    nil
    
  )



)




(setq
      *CUR-FILLER '(HUMAN F-NAME (GEORGE)
						  L-NAME (WHITE))
      
      DTR-1 '(
        (AND (GET-SF 'ROLE *CUR-FILLER)
             '((SL-NAME REF) (SL-NAME ROLE)))
        (AND (GET-SF 'TYPE *CUR-FILLER)
             '((SL-NAME TYPE)))
        (AND (GET-SF 'F-NAME *CUR-FILLER) 
             '((SL-NAME F-NAME) (SL-NAME L-NAME)))
        (AND (GET-SF 'GENDER *CUR-FILLER)
             '((SL-NAME GENDER)))
      )
      
      
      
      
      DTR-2 '(
				(AND (SETQ AGNT (GET-NESTED-SF '(AGENT F-NAME) *CUR-FILLER))
				(SETQ GENDER (GET-NESTED-SF '(AGENT GENDER) *CUR-FILLER)) (EQUAL AGNT (GET-NESTED-SF '(RECIP F-NAME) *CUR-C-ANS)) (EQUAL GENDER '(MALE))
				'((PHRASE HE HAS) (SL-NAME OBJECT)))
				(AND '((PHRASE SHE HAS) (SL-NAME OBJECT))) )
	
	
	DTR-3 '(    
			(AND (EQUAL (first *CUR-Q-CON) 'CAUSE)
						(IS-VARIABLE (GET-SF 'ANTE *CUR-Q-CON))
						(EQUAL (first (GET-SF 'TO *CUR-C-ANS)) 'NOSE)
						'((PHRASE BECAUSE) (SL-NAME AGENT) (PHRASE PUT)
						(SL-NAME OBJECT) (PHRASE INTO) (SL-NAME TO)))
					)
					
	C-ANS-4 '(EXPLANATION AGENT (HUMAN F-NAME (GEORGE)
                                   GENDER (MALE))
                      TIME (MIDNIGHT)
                      DATE (JULY-4-2015)
                      LOC (HS REF (RIDGEMONT)))

	DTR-4 '(
		(AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(MALE))
			 '((PHRASE HE)))
		(AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(FEMALE))
			 '((PHRASE SHE)))
		(AND '((PHRASE IT)))
	)

	DTR-5 '(
		(AND (EQUAL (GET-SF 'QUERY *CUR-Q-CON) '(WHO))
			 '((SL-NAME AGENT)))
		(AND (EQUAL (GET-SF 'QUERY *CUR-Q-CON) '(WHEN))
			 '((PHRASE AT) (SL-NAME TIME) (PHRASE ON) (SL-NAME DATE)))
		(AND (EQUAL (GET-SF 'QUERY *CUR-Q-CON) '(WHERE))
			 '((PHRASE AT) (SL-NAME LOC)))
		'(DUNNO)
	)
					
)


;=============================================================================
; TEST 3
;=============================================================================
(defun test3 ()

	(setq DTR-4 '(
	(AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(MALE)) '((PHRASE HE)))
	(AND (EQUAL (GET-SF 'GENDER *CUR-FILLER) '(FEMALE)) '((PHRASE SHE)))
	  (AND '((PHRASE IT)))
	)
	)
	
	(print "test 3.0.1: ")
	(print
		(EVAL-D-TREE DTR-1)
	
	)
	
	
	(setq *CUR-FILLER '(TEACH AGENT (HUMAN F-NAME (GEORGE)
					GENDER (MALE)) OBJECT (CHEM))

			*CUR-C-ANS  '(PTRANS AGENT (DRUGEE)
					RECIP (HUMAN F-NAME (GEORGE)) OBJECT (MONEY))

	)

	
	(print "test 3.0.2: ")
	(print 
	
		(EVAL-D-TREE DTR-2)
	
	)
	
	
	(print "test 3.1: ")

	(print
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-1 Q-CON1)
	)

	(print "test 3.2: ")

	(print 
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-2 Q-CON2)
	)


	(print "test 3.3: ")

	(print 
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-3 Q-CON3)
	)


	(print "test 3.4: ")

	(setq *CUR-FILLER '(HUMAN GENDER (MALE)))


	(print 
		(EVAL-D-TREE DTR-4)
	)
	
	(print "test 3.5: ")
	
	
	(print 
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS '(PHYSICIAN F-NAME (DOC)
														GENDER (MALE)
														ROLE (ONCOLOGIST)
													OBJECT (HUMAN F-NAME (GEORGE)))
       								'(Q))
		
	)
	
	
	(print "test 3.6: ")
	
	
	(print 
	
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS
						'(ATTENDED AGENT (HUMAN F-NAME (GEORGE))
								OBJECT (HS))
						'(Q)
		)	
	
	
	)
	
	
	(print "test 3.7: ")
	
	(print
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS
           '(ATTENDED AGENT (COMEDIAN GENDER (MALE)))
           '(Q))
		
	)
	;should print (IT HE MAKES ME LAUGH HE HE)
	
	(print "test 3.8: ")
	
	(print 
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-4 '(FOLLOW-UP QUERY (WHO)))
	
	)
	
	;should print (IT WAS GEORGE)
	
	(print "test 3.9: ")
	
	(print
		(FR-TO-ENG COCAINE-DEALER-ENG-PATS C-ANS-4 '(FOLLOW-UP QUERY (WHEN)))
		
	)
	
	;should print (IT WAS AT MIDNIGHT ON JULY-4-2015)
	

)

(test3)