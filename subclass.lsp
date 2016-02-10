; ;;;;;;NOT FINISHED YET, ALMOST



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


	(loop for x in TX do 
	
		(
	
			if ( equal ( nth 1 x ) entity)  
				(if (equal  (nth 2 x)  class) 
					(return T)
					
					(SUBCLASS-HELPER (remove x TX) (nth 2 x) class )
				

				
				
				)
			
			
				
		)
	)
)










;; Algorithm
;;Recursively, check if entity is in *TX, if entity matches, see if the next word 
;;(new-entity) matches with class (if so, return true) (if you reach the end of *TX,
;;return nil), else IS-SUBCLASS (new-entity, class). 
;; TA notes: check if there's a circular condition





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
(SETUP-GLOBALS)
;(cdr *TX)
(print 
	(IS-SUBCLASS 'TEACH 'ACT)

)




