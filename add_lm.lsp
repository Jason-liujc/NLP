(defun EX-CLEAR-GLOBALS ()
    (every #'makunbound *WM)
    (setq *LM NIL)
)

(setq ADD-TO-LM-tests t)

(defun SETUP-GLOBALS ()
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
)







; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

(defun CHECK-DUPLICATE (phrase mem)
  ;(subsetp phrase mem)
  ; OR
  (loop for v in mem 
    do (if (member phrase v) 
          return T)
  )
  return F
)

(defun REMOVE-FRAME (phrase mem)
  (loop for v in mem
    do (if (equal phrase (car v))
    ; remove v
    )
  )
)


; FUNCTION: ADD-TO-LM
; PURPOSE:  Adds the given (phrase frame demon) triplet to the global LEX-MEM,
;           making sure to not add duplicate phrases
; INPUT:    phrase: a list of English words
;           frame: a frame associated with that phrase
;           demons: a list of 0 or more demon instantiations
; OUTPUT:   phrase-frame-demon triplet constructed

(defun ADD-TO-LM (phrase frame demons)
  (if (CHECK-DUPLICATE phrase *LM)
    (REMOVE-FRAME frame *LM)
  )

  (append (append frame demons) *LM)

  (return (append phrase frame demons))
)


; ALGORITHM
; Helper Function: CHECK-DUPLICATE (phrase mem)
;     loops through mem
;         returns t if phrase is in mem
;     returns f

; Helper Function: REMOVE-FRAME (phrase mem)
;     loops through mem
;         finds instance of phrase and removes the frame

; ADD-TO-LM (phrase, concept, demons)
;	If CHECK-DUPLICATE returned true, use REMOVE-FRAME 
;	Append phrase concept demon together into a list
;	Append this list to the front of *LM







; -----------------------------------------------------------------------------
; Test Helper functions
; -----------------------------------------------------------------------------
(defun test-case-lex-entry (actual expected case-name)
    (cond
        ((lex-equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

(defun test-case-lex-mem (actual expected case-name)
    (cond
        ((and (subsetp expected actual :test #'lex-equal) (subsetp actual expected :test #'lex-equal)) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test cases: (ADD-TO-LM)
(cond (ADD-TO-LM-tests
(format t "Testing ADD-TO-LM...~%")
    
    (test-case-lex-entry (ADD-TO-LM '(GEORGE)
                              '(HUMAN F-NAME (GEORGE)
                                      GENDER (MALE))
                              '((D1 A1) (D2 A21 A22)))
                         '((GEORGE) (HUMAN F-NAME (GEORGE)
                                           GENDER (MALE)) ((D1 A1) (D2 A21 A22))) "ADD-TO-LM_ex_1")
    (test-case-lex-mem *LM
                       '(
                          ((GEORGE) (HUMAN F-NAME (GEORGE)
                                           GENDER (MALE)) ((D1 A1) (D2 A21 A22)))
                        ) "ADD-TO-LM_ex_2")
    
(format t "===========================~%")
))
