;;; CS 161 Winter 2016: Example Test Framework

; [!] Load your HW2 solution here!
(load "hw-2-solution.lsp")

; [!] IMPORTANT ===============================================================
; It is assumed that, by this point, you have included either the HW1 solution,
; or your own HW2 solution that includes our HW1. This is important for the
; following tests because they will employ the EQUAL-SF function, and some
; other helpers that are ommitted in these sample tests
; =============================================================================

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

; Test function; makes sure result is equal to expected, and if not, print expected value
(defun test-case (actual expected case-name)
    (cond
        ((equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function; makes sure actual is frame-equivalent to the expected
(defun test-case-fr (actual expected case-name)
    (cond
        ((EQUAL-SF actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function, with equality condition a set-comparison between two lists with
; each matching element being compared using equivalence (#'equal)
(defun test-case-set (actual expected case-name)
    (cond
        ((and (subsetp actual expected :test #'equal) (subsetp expected actual :test #'equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Get "boolean value," i.e. NIL vs NON-NIL
; Best effort to test for NEWGAPS symbols that may have different orderings of
; names
(defun b-val (v)
    (if (null v) nil 'NON-NIL)
)

; Test function; make sure result boolean equal (non-nil = non-nil; nil = nil)
(defun test-case-bool (actual expected case-name)
    (cond
        ((equal (b-val actual) (b-val expected)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Helper function; lex-equal determines if two lexical entries of format:
; (phrase frame demons)
; are equivalent between actual and expected
(defun lex-equal (actual expected)
    (and (equal (first actual) (first expected))
         (EQUAL-SF (second actual) (second expected))
         (and (subsetp (third expected) (third actual) :test #'equal) (subsetp (third actual) (third expected) :test #'equal)))
)

; Test function; see if the phrase-frame-demon triplets are the same
(defun test-case-lex-entry (actual expected case-name)
    (cond
        ((lex-equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function; see if the lexical entries of the format:
; ((phrase frame demons)*)
; are equivalent between actual and expected
(defun test-case-lex-mem (actual expected case-name)
    (cond
        ((and (subsetp expected actual :test #'lex-equal) (subsetp actual expected :test #'lex-equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function; see if the ((phrase frame demon) (rest-sent)) pair for LOOKUP-PHRASE
; are equivalent between actual and expected
(defun test-case-lookup-phrase (actual expected case-name)
    (cond
        ((and (lex-equal (first actual) (first expected)) (equal (second actual) (second expected))) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)


; -----------------------------------------------------------------------------
; Test Frames, Variables, & Globals
; -----------------------------------------------------------------------------

; Global definitions
(setq *LM NIL)
(setq *WM NIL)
(setq *DM NIL)
(setq *TX NIL)
(EX-CLEAR-GLOBALS)

; -----------------------------------------------------------------------------
; Unit Test Flags
; -----------------------------------------------------------------------------

; Unit test activations: only run the unit tests marked with t
(setq
    ADD-TO-LM-tests t
    LOOKUP-PHRASE-tests t
    NEWGAPS-tests t
    IS-SUBCLASS-tests t
    FIND-CON-tests t
    MAIN-ACT-tests t
    GEN-DEMS-tests t
    DEM-SRCH-tests t
    DEM-AMEND-tests t
    DEM-REF-tests t
    DEM-EXEC-tests t
    PARSE-SENT-tests t
)


(SETUP-GLOBALS)
(PARSE-SENT '(GEORGE TEACHES STUDENTS))

(print (EXPAND (MAIN-ACT)))
(print *DM)

