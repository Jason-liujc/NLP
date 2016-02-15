
; -----------------------------------------------------------------------------
; Workhorse Functions
; -----------------------------------------------------------------------------

; FUNCTION: DEM-EXEC
; PURPOSE:  Repeatedly calls each active demon instantiation within the global
;           *DM until all active demons return nil. Whenever a demon
;           returns something non-nil, we remove it from the global *DM
;           and then call each remaining demon again
; INPUT:    N/A
; OUTPUT:   Status of *DM after executing all active demons (a list of all
;           remaining, active demon instantiations)
(defun DEM-EXEC ()
   
   (loop for x in *DM do 
		(let ((result (eval x)) )
			(if (not (null result))
	   			(setq *DM (remove x *DM))
	   		)
		)   		
   		
   	)
   
)

; -----------------------------------------------------------------------------
(setq r '(1 2 3 2 5 2))
(defun yo ()
	
		(loop for x in r do 
   			(if (equal x 2)
   				(setq r (remove x r))
   				nil
   			)
   		)
   	
	

)
(yo)
(print r)
