; CS 161 Winter 2016: HW4 Solution

; [!] Include OUR HW3 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-3-solution using *our* path, which will likely
;     differ from yours (Note: HW3 also includes HW2, which includes HW1)
;(load "hw-3-solution")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

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

(defun FR-TO-ENG (e-pats c-ans q-con)

    (setq *CUR-C-ANS C-ANS)
    
    (setq *CUR-Q-CON q-con)
    
    (setq *CUR-FILLER C-ANS)
    
    ;we use the helper to call the translate functions 
    (FR-TO-ENG-HELPER e-pats c-ans)
    
)



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



