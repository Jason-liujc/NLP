
; FUNCTION: PARSE-SENT
; PURPOSE:  Performs a conceptual anaylsis of the input SENT using the known
;           phrases and interpretations within the global *LM.
; INPUT:    sent: list of English words comprising a sentence
; OUTPUT:   frame consisting of: (EXPAND (MAIN-ACT))
(defun PARSE-SENT (sent)
    (let ((x (LOOKUP-PHRASE sent *LM)))
    ; x is now the associated phrase-frame-demon triplet
	    (let ((y (NEWATM 'con)))

	    	(setq y (NEWGAPS x))
	    	(append *WM y)

	    	(GEN-DEMS X Y)

	    	(DEM-EXEC)

	    	(PARSE-SENT (rest x))
	    )
    )
    
    
    
    
    
)

; -----------------------------------------------------------------------------

