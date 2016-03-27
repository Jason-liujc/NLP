(defun GET-PATTERN (e-pats matching-word)
	;returns the pattern for the given predicate matching-word in e-pats
	;returns nil if no match
	(if (null e-pats)
		(return-from GET-PATTERN nil)
	)
	(if (null matching-word)
		(return-from GET-PATTERN nil)
	)
	(let* ( (cur-pat (first e-pats)) )
		(if (equal (first cur-pat) matching-word)
			(rest cur-pat)
			(GET-PATTERN (rest e-pats) matching-word)
		)

	)
)

(defun TRANSLATE-SINGLE (tuple e-pats c-ans)
	(setq *CUR-FILLER c-ans)
	(let ((slot-type (first tuple)) )
		(cond
			((equal slot-type 'SL-NAME )
				(FR-TO-ENG-HELPER e-pats (GET-SF (second tuple) c-ans) )
			)
			((equal slot-type 'PHRASE )
				(rest tuple)
			)
			((equal slot-type 'D-TREE )
				(let ((eval-result (EVAL-D-TREE (eval (second tuple))) ) )
					(if (null eval-result)
						nil
						(TRANSLATE-LISTS eval-result e-pats c-ans)
					)
				)
			)
		)
	)
)

(defun TRANSLATE-LISTS (to-translate e-pats c-ans)
	;given a list, which is a certain pattern, translate all of them to a list of English words
	(if (null to-translate)
		(return-from TRANSLATE-LISTS nil)
	)
	(let* ((cur-tar (first to-translate)) (single-result (TRANSLATE-SINGLE cur-tar e-pats c-ans) ) )
		(if (or (null single-result) (and (equal (length single-result) 1) (null (first single-result)) ) )
			;if the translation of current tuple in the pattern returns nil
			(TRANSLATE-LISTS (rest to-translate) e-pats c-ans)
			;recursively translate the whole pattern
			(append single-result (TRANSLATE-LISTS (rest to-translate) e-pats c-ans ))
		)
	)

	
)

(defun FR-TO-ENG-HELPER (e-pats c-ans)
	(let* ( (cur-pre (first c-ans) ) (matching (GET-PATTERN e-pats cur-pre) )  )
		(if (null matching)
			(list cur-pre)
			(TRANSLATE-LISTS matching e-pats c-ans)
		)
	)
)

(defun FR-TO-ENG (e-pats c-ans q-con)
    (setq *CUR-Q-CON q-con)
    (setq *CUR-C-ANS C-ANS)
    (setq *CUR-FILLER C-ANS)
    (FR-TO-ENG-HELPER e-pats c-ans)
)




