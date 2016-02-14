(load "find_con.lsp")

; -----------------------------------------------------------------------------
; Main Functions
; -----------------------------------------------------------------------------

; FUNCTION: GET-SF
; PURPOSE:  Returns the filler of the given slot-name
; INPUTS:   slot: an atom designating the slot name
;           frame: a frame
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
(defun GET-SF (slot frame)
    (cond
        ; Base case: predicate with no slots (or empty frame)
        ((<= (length frame) 1) nil)
        ; If first slot matches, return its filler.
        ((equal slot (front-slot frame)) (front-filler frame))
        ; Else, first slot does not match, so test the rest of the slots
        (t (GET-SF slot (pop-slot frame)))
    )
)


; FUNCTION: DEM-SRCH
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then sets the top-level gap in the
;           myslot slot in mycon to the found CONatom. Returns the found
;           CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name of the gap in myslot to bind when found
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-SRCH (mycon myslot dir class)
    (let* (( l (FIND-CON mycon dir class) ) (b (GET-SF myslot  (eval mycon) ))  )
    	;(print (eval l))
    	;(print b)
    	(if (NULL l) nil)
    	
    	(set b l)

    )
    
    
)



;Algorithm
; if dir==before
;	find the instance before mycon in VM
; else if dir==after
;	find the instance after mycon in VM
;TA notes: use find-con and binding. 



(setq *WM '(CON0 CON1))
(setq CON0 '(HUMAN F-NAME (GEORGE)
GENDER (MALE)) )


(setq CON1 '(TEACH AGENT AGENT1 RECIP RECIP1
OBJECT OBJECT1)

)

(print 
	(DEM-SRCH 'CON1 'AGENT 'BEF 'HUMAN)

)

