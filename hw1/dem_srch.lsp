; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

; FUNCTION: front-slot
; PURPOSE:  Return the name of the first SLOT in FRAME
; INPUT:    frame
; OUTPUT:   atom: name of first SLOT
(defun front-slot (frame)
    (second frame)
)

; FUNCTION: front-filler
; PURPOSE:  Return the FILLER of the first SLOT in FRAME
; INPUT:    frame
; OUTPUT:   frame/gap: filler of first SLOT in FRAME
(defun front-filler (frame)
    (third frame)
)

; FUNCTION: pop-slot
; PURPOSE:  Return a copy of FRAME with its first slot-filler removed
; INPUT:    frame
; OUTPUT:   frame (with first slot-filler removed)
(defun pop-slot (frame)
    (cons (first frame) (nthcdr 3 frame))
)

; FUNCTION: f-pred
; PURPOSE:  retrieves the front-predicate of a frame, or the symbol name if
;           it's a gap
; INPUTS:   frame: a gap or a frame
; OUTPUT:   the predicate if it's a frame, or the symbol name if it's a gap
(defun f-pred (frame)
    (cond
        ((listp frame) (first frame))
        (t frame)
    )
)

; FUNCTION: f-length
; PURPOSE:  Safely checks the length of the input if it's a frame vs gap
; INPUTS:   frame: a gap or a frame
; OUTPUT:   length of frame if it's a frame; 1 if it's a gap
(defun f-length (frame)
    (cond
        ((listp frame) (length frame))
        (t 1)
    )
)

; FUNCTION: rm-slot
; PURPOSE:  Return a copy of FRAME, but with a single slot-filler removed
; INPUT:    frame: frame to remove slot from
;           slot: slot name to be removed
; OUTPUT:   frame
(defun rm-slot (slot frame)
    (cond
        ; Base case: no slots left, so we're done
        ((<= (length frame) 1) frame)
        ; Base case: front slot matches, so just pop it
        ((equal (front-slot frame) slot) (pop-slot frame))
        ; Recursive case: front slot doesn't match, so keep looking
        (t (append (rm-slot slot (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)


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

; FUNCTION: EXPAND-SLOTS
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching EXPAND
;           on each filler.
; INPUTS:   sf: list of (slot filler) pairs
; OUTPUT:   List of slot-filler pairs with any gaps replaced by their values
;           (follows any number of successive references)
(defun EXPAND-SLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch EXPAND on our first filler
        (t (append (append (list (first sf))               ; rebuild our first slot-filler pair
                           (list (EXPAND (second sf))))    ; dispatch EXPAND on the filler
                           (EXPAND-SLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)

; FUNCTION: EXPAND
; PURPOSE:  Returns the frame represented by the given atom (if it is bound)
;           with all gaps replaced by their values
; INPUTS:   atom: a symbol (possibly) representing a frame
; OUTPUT:   That frame with all bound gaps replaced by their values
(defun EXPAND (atom)
    (cond
        ; Base case: safety for getting nil input - just return nil
        ((null atom) nil)
        ; Base case: we got a non-NIL atom, so evaluate it if bound
        ((atom atom) (if (boundp atom) (EXPAND (eval atom)) atom))
        ; Base case: empty, or single pred frame
        ((<= (length atom) 1) atom)
        ; Main case: dispatch EXPAND-SLOTS on our slot-filler list
        (t (cons (first atom) (EXPAND-SLOTS (rest atom))))
    )
)


; FUNCTION: IS-SUBCLASS
; PURPOSE:  Queries the global *TX for whether or not the given entity
;           ISA member of the given class
; INPUT:    entity, class: items in a class hierarchy
; OUTPUT:   boolean indicating hierarchical relationship



(defun IS-SUBCLASS (entity class)
  
  (SUBCLASS-HELPER *TX entity class )
)

;helper:
(defun SUBCLASS-HELPER(TX entity class)
  
  
  ;if TX is nil then return nil
  (if (equal TX nil) nil)

  (if (equal entity class) (return-from SUBCLASS-HELPER t))

  (loop for x in TX do 
    (
  
      if ( equal ( nth 1 x ) entity)  
        (if (equal  (nth 2 x)  class) 
          (return T)
          
          (if (SUBCLASS-HELPER (remove x TX) (nth 2 x) class ) (return T))

        
        )
      
        
    )
    
    
    
  )
  
)









; FUNCTION: FIND-CON
; PURPOSE:  Returns the CONatom found by searching the *WM for a CONatom with a
;           pred of the given class starting at mycon in direction dir within
;           the global *WM
; INPUT:    mycon: where to start searching in *WM
;           dir: either 'BEF or 'AFT indicating search direction
;           class: predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if not found


(defun LIST-AFT (mycon  WM)
  
 ;(print WM)
  
  (if (equal mycon (car WM)) 
      (cdr  WM)
      (LIST-AFT mycon (cdr WM ))
  )
  
)


(defun LIST-BEF (mycon  WM)
  (LIST-AFT mycon (reverse WM) )

)

(defun LIST-DIR (mycon dir)
  ;if the direction is wrong 
  (if (and (not (equal dir 'AFT)) (not (equal dir 'BEF)) )
    nil
  )

  ;if the direction is after
  (if (equal dir 'AFT)
      
     (LIST-AFT mycon *WM) 
      (LIST-BEF mycon *WM)
   
  )

  
)

(defun FIND-CON (mycon dir class)

    
  
    (loop for x in (LIST-DIR mycon dir) do
      
       (if (IS-SUBCLASS (car (eval x )) class) 
        
        (return x)
      )
  
    
    
    nil
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
    (let ((find_result (FIND-CON mycon dir class)) )
      (if (null find_result)
        nil
        (let ((temp_slot (GET-SF myslot (eval mycon)) ))
         (set temp_slot find_result)

        )
      )
    )
)



;Algorithm
; if dir==before
;	find the instance before mycon in VM
; else if dir==after
;	find the instance after mycon in VM
;TA notes: use find-con and binding. 

(setq *WM '(con0 con1))
(setq con0 
  '(HUMAN F-NAME (GEORGE)
    GENDER (MALE)
    )

)
(setq CON1 '(TEACH AGENT AGENT1
            RECIP RECIP1
            OBJECT OBJECT1)

)
(setq *TX '(
      (MEMB HUMAN ANIMATE)
      (MEMB HOME LOC)
      (MEMB THEATER LOC)
      (MEMB FIDO CANINE)
      (MEMB CANINE ANIMATE)
      (MEMB INGEST PHYS-ACT)
      (MEMB COMMUN MENTAL-ACT)
      (MEMB TEACH MENTAL-ACT)
      (MEMB PHYS-ACT ACT)
      (MEMB THINK ACT)
      (MEMB MENTAL-ACT ACT)
      (MEMB INSTITUTION SOCIAL-ENT)
      (MEMB KNOWLEDGE ABSTRACT)
      (MEMB SOCIAL-ENT CONCEPT)
    )
)
(defun DEM-EXEC ()
   
   (loop for x in *DM do 
    (let ((result (eval x)) )
      (if (not (null result))
          (setq *DM (remove x *DM))
        )
    )       
      
    )
   
)
(load "dem_ref.lsp")
(setq *DM 
   '(
    (DEM-SRCH CON1 AGENT BEF HUMAN)
    (DEM-SRCH CON2 OBJECT AFT ACT)
    (DEM-REF CON2 LOC BEF ACT)
  )
)
;(print (DEM-SRCH 'CON1 'AGENT 'BEF 'HUMAN) )
;(print (EXPAND 'CON1))
;(print temp_slot)
(dem-exec)
(print *DM)