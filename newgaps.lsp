;;Jason 

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

; -----------------------------------------------------------------------------


; FUNCTION: EXPAND-SLOTS
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching EXPAND
;           on each filler.
; INPUTS:   sf: list of (slot filler) pairs
; OUTPUT:   List of slot-filler pairs with any gaps replaced by their values
;           (follows any number of successive references)
(defun ATM-EXPAND (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch EXPAND on our first filler
        (t (append (append (list (first sf))               ; rebuild our first slot-filler pair
                           (list (EXPAND (second sf))))    ; dispatch EXPAND on the filler
                           (ATM-EXPAND (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)




; FUNCTION: NEWATM
; PURPOSE:  Produces a fresh, unused, unbound, unique symbol with the given
;           symName prefix and (ostensibly) random numerical suffix
; INPUT:    symName: symbol prefix for the new symbol
; OUTPUT:   new unbound symbol with symName prefix and numeric suffix
(defun NEWATM (symName)
    ; Generate the new symbol using gensym
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
    )
)



; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names


(defun NEWGAPS (frame)
    ;;base case, just when it's null
    ((null frame) null)
    
    
    
    
)

;Algorithm:
;loop and find all the gaps, for each gap, call NEWATM gap, set the return value to NIL, 
;replace original gap name, and continue looping through the frame



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
