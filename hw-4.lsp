; CS 161 Winter 2016: HW4 Solution

; [!] Include OUR HW3 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-3-solution using *our* path, which will likely
;     differ from yours (Note: HW3 also includes HW2, which includes HW1)
;(load "hw-1-solution.lsp")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; TODO



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



; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------


; FUNCTION: EVAL-D-TREE
; PURPOSE:  Takes the given decision tree, and uses the frames in the global
;           variables to return a replacement pattern, if any.
; INPUT:    D-TREE: a decision tree (NOT a symbol name for a decision tree,
;                   but the tree itself)
; OUTPUT:   Replacement pattern if a decision tree path is satisfied, else NIL.
(defun EVAL-AND (condition)
  (if (null condition)
    (return-from EVAL-AND nil)
  )
  (if (equal (length condition) 1)
    (return-from EVAL-AND (eval (first condition)))
  )
  (if (null (eval (first condition) ) )
    nil
    (EVAL-AND (rest condition))
  )

)

(defun EVAL-D-TREE (d-tree)
    (if (null d-tree)
      (return-from EVAL-D-TREE nil)
    )
    (let ( (CUR-CONDI (first d-tree))  )
      (let ( (eval-cur (EVAL-AND (rest CUR-CONDI) )) )
        (if (null eval-cur)
          (EVAL-D-TREE (rest d-tree) )
          eval-cur
        )

      )
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
)



