


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

;(SETUP-FOR-FINDCON)
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
