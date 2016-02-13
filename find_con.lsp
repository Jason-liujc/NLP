


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



;;Algorithm:
;Take in mycon, and use a loop that once mycon is found in *WM, enter helper function
;helperFunction: LOCATE-IN-WM-AFT (class, rest *WM)
;returns correct con if class matches something in *WM
;returns nil if not found
;helperFunction: LOCATE-IN-WM-BEF (mycon, class)
;returns true if finds class before locating mycon
;return nil otherwise
;returns helperFunction depending on dir


;TA notes: use is-subclass, we can reverse the direction to simplify it!

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
; Test Helper functions
; -----------------------------------------------------------------------------

; Testing function used to wipe our globals clean
; between tests
(defun EX-CLEAR-GLOBALS ()
    ; Clear out all of our example bindings
    (every #'makunbound *WM)
  
    (setq *LM NIL)
    (setq *WM NIL)
    (setq *DM NIL)
    (setq *TX NIL)
    
    ; Good to go for a new set of tests!
)

; Testing function used to set up globals with their spec-values
(defun SETUP-GLOBALS ()
    (setq *WM NIL)
    (setq *DM NIL)
    (setq *LM '(
      ((HIGH SCHOOL) (INSTITUTION TYPE (HIGHSCHOOL)) ((DEM-REF LOC BEF ACT)))
      ((IS) (BEING AGENT AGENT
                   OBJECT OBJECT)
            ((DEM-SRCH AGENT BEF OBJECT)
             (DEM-SRCH OBJECT AFT OBJECT)))
      ((A) NIL ((DEM-AMEND REF (INDEF) AFT CONCEPT)))
      ((AT) (LOC TYPE TYPE) ((DEM-SRCH TYPE AFT INSTITUTION)))
      ((CHEMISTRY) (KNOWLEDGE TYPE (CHEM)) NIL)
      ((STUDENTS) (HUMAN TYPE (STUDENTS)) NIL)
      ((TEACHES) (TEACH AGENT AGENT
                        RECIP RECIP
                        OBJECT OBJECT)
                 ((DEM-SRCH AGENT BEF HUMAN) 
                  (DEM-SRCH RECIP AFT HUMAN)
                  (DEM-SRCH OBJECT AFT ABSTRACT)))
      ((GEORGE) (HUMAN F-NAME (GEORGE)
                       GENDER (MALE)) NIL)
      ((DRUG) (SUBSTANCE TYPE (DRUG)
                         NAME NM1) NIL)
      ((DRUG DEALER) (HUMAN OCCUPATION (DEALER)
                            F-NAME FN1) NIL)
      ((DRUG DEALER LAB) (LOC TYPE (LABORATORY)
                              CONTAINS (DRUGS)
                              CONOTATION (ILLICIT)) NIL)
      ((DEALS) (ACT AGENT AGENT
                    RECIP RECIP
                    OBJECT DG)
               ((DEM-SRCH AGENT BEF HUMAN) 
                (DEM-SRCH RECIP AFT HUMAN)
                (DEM-SRCH OBJECT AFT DRUG)))
      ((COCAINE) (DRUG NAME (COCAINE)
                    TYPE (STIMULANT)) nil)
    ))
    (setq *TX '(
      (MEMB HUMAN ANIMATE)
      (MEMB ANIMATE OBJECT)
      (MEMB HOME LOC)
      (MEMB THEATER LOC)
      (MEMB FIDO CANINE)
      (MEMB CANINE ANIMATE)
      (MEMB INGEST PHYS-ACT)
      (MEMB COMMUN MENTAL-ACT)
      (MEMB TEACH MENTAL-ACT)
      (MEMB PHYS-ACT ACT)
      (MEMB THINK ACT)
      (MEMB BEING ACT)
      (MEMB MENTAL-ACT ACT)
      (MEMB INSTITUTION SOCIAL-ENT)
      (MEMB KNOWLEDGE ABSTRACT)
      (MEMB SOCIAL-ENT CONCEPT)
      (MEMB COCAINE DRUG)
      (MEMB WEED DRUG)
      (MEMB MJ WEED)
      (MEMB WEED MJ)
      (MEMB DRUG PHYS-OBJ)
    ))
)

(defun SETUP-FOR-FINDCON ()
	(SETUP-GLOBALS)
	(setq *WM '(CON0 CON1 CON2 CON3))
	(setq CON0 '(HUMAN))
	(setq CON1 '(INGEST AGENT AG01 OBJECT OBJ01))
	(setq CON2 '(SIZE VAL (>NORM)))
	(setq CON3 '(FOOD))
	



)

(SETUP-FOR-FINDCON)
;
;( print 
;
;(FIND-CON 'CON1 'AFT 'FOOD)
;
;)
;( print 
;
;(FIND-CON 'CON1 'BEF 'FOOD)
;
;)
;( print 
;
;(FIND-CON 'CON1 'BEF 'ANIMATE)
;
;)
