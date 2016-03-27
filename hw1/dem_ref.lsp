(load "find_con.lsp")
; -----------------------------------------------------------------------------


; FUNCTION: AMEND-SF-EXEC
; PURPOSE:  (Workhorse Helper for AMEND-SF)
;           Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist) [Same inputs / outputs]
(defun AMEND-SF-EXEC (slot filler frame)
    (cond
        ; Base case: single predicate, so add the slot
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ; Base case: If first slot is target, replace the filler, keep rest slots
        ((equal slot (front-slot frame))
         (cons (first frame) (append (append (list slot) (list filler) (nthcdr 3 frame))))
        )
        ; Recursive case: First slot not target, so pop and recurse
        (t (append (AMEND-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

; FUNCTION: AMEND-SF
; PURPOSE:  Returns a copy of the input concept with the given slot-filler
;           pair added. If the slot already exists in the frame, its filler
;           will be replaced by the given input filler
; INPUTS:   slot: an atom representing the slot name
;           filler: a filler to place in the corresponding slot
;           frame: the frame being operated on
; OUTPUT:   Frame with added / replaced slot-filler pair
(defun AMEND-SF (slot filler frame)
    ; Check to see if we've been given a bound gap (just in case, you know?)
    (if (atom frame) 
        (if (boundp frame) (AMEND-SF-EXEC slot filler (eval frame)) frame)
        (AMEND-SF-EXEC slot filler frame)
    )
)


; -----------------------------------------------------------------------------


; FUNCTION: DEM-REF
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given mycon. 
;           Returns the found CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-REF (mycon myslot dir class)
    (let* ((l (FIND-CON mycon dir class)  ))
    
    	(if (NULL l) nil)
    	
    	(set l (AMEND-SF myslot mycon l)  )
    	
    	l
    
    
    
	)
)




; -----------------------------------------------------------------------------



(setq *WM '(CON1 CON2))
(setq CON1 '(TEACH AGENT AGENT1
RECIP RECIP1
OBJECT OBJECT1))

(setq CON2 '(INSTITUTION TYPE (HIGHSCHOOL)))

(print 
	(DEM-REF 'CON2 'LOC 'BEF 'ACT)
	)
	
(print con1)