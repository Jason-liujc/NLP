;;; CS 161 Winter 2016: Example Test Framework

; [!] Load your HW4 solution here!
(load "hw-4-solution.lsp")

; [!] IMPORTANT ===============================================================
; It is assumed that, by this point, you have included either the HW3 solution,
; or your own HW3 solution that includes our HW1, 3. This is important for the
; following tests because they will employ the EQUAL-SF function, and some
; other helpers that are ommitted in these sample tests
; =============================================================================

; -----------------------------------------------------------------------------
; Test Helper functions
; -----------------------------------------------------------------------------

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


; -----------------------------------------------------------------------------
; Test Frames, Variables, & Globals
; -----------------------------------------------------------------------------

(setq *CUR-Q-CON nil
      *CUR-C-ANS nil
      *CUR-FILLER nil
      
      COCAINE-DEALER-ENG-PATS '(
         (TEACH (SL-NAME AGENT) (PHRASE TEACHES) (SL-NAME OBJECT) (PHRASE AT)  
                (SL-NAME LOC) (PHRASE TO) (SL-NAME RECIP))
         (HUMAN (D-TREE DTR-1))
         (CHEM (PHRASE CHEMISTRY))
         (HS (SL-NAME REF) (PHRASE HIGH SCHOOL))
         (STATE (D-TREE DTR-2))
         (INFORM (SL-NAME AGENT) (PHRASE TOLD) (SL-NAME RECIP) (PHRASE THAT) 
                 (SL-NAME OBJECT))
         (CANCER (SL-NAME TYPE) (PHRASE CANCER))
         (INGEST (D-TREE DTR-3))
         (NOSE (PHRASE NOSE OF) (SL-NAME PARTOF))
         (DEF (PHRASE THE))
      )
      
      DTR-1 '(
        (AND (GET-SF 'ROLE *CUR-FILLER)
             '((SL-NAME REF) (SL-NAME ROLE)))
        (AND (GET-SF 'TYPE *CUR-FILLER)
             '((SL-NAME TYPE)))
        (AND (GET-SF 'F-NAME *CUR-FILLER) 
             '((SL-NAME F-NAME) (SL-NAME L-NAME)))
        (AND (GET-SF 'GENDER *CUR-FILLER)
             '((SL-NAME GENDER)))
      )
      
      DTR-2 '(
        (AND (SETQ AGNT (GET-NESTED-SF '(AGENT F-NAME) *CUR-FILLER))
             (SETQ GENDER (GET-NESTED-SF '(AGENT GENDER) *CUR-FILLER))
             (EQUAL AGNT (GET-NESTED-SF '(RECIP F-NAME) *CUR-C-ANS))
             (EQUAL GENDER '(MALE))
             '((PHRASE HE HAS) (SL-NAME OBJECT)))
        (AND '((PHRASE SHE HAS) (SL-NAME OBJECT)))
      )
      
      DTR-3 '(
        (AND (EQUAL (first *CUR-Q-CON) 'CAUSE)
             (IS-VARIABLE (GET-SF 'ANTE *CUR-Q-CON)) 
             (EQUAL (first (GET-SF 'TO *CUR-C-ANS)) 'NOSE)
             '((PHRASE BECAUSE) (SL-NAME AGENT) (PHRASE PUT) 
               (SL-NAME OBJECT) (PHRASE INTO) (SL-NAME TO)))
      )
)


; -----------------------------------------------------------------------------
; Unit Test Flags
; -----------------------------------------------------------------------------

; Unit test activations: only run the unit tests marked with t
(setq
    FR-TO-ENG-tests t
    EVAL-D-TREE-tests t
)


; -----------------------------------------------------------------------------
; Unit Tests
; -----------------------------------------------------------------------------

; Test cases: (FR-TO-ENG)
(cond (FR-TO-ENG-tests
(format t "Testing FR-TO-ENG...~%")
    
(format t "===========================~%")
))

; -----------------------------------------------------------------------------


; Test cases: (EVAL-D-TREE)
(cond (EVAL-D-TREE-tests
(format t "Testing EVAL-D-TREE...~%")
    
(format t "===========================~%")
))

; -----------------------------------------------------------------------------

