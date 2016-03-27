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




