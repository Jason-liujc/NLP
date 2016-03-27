; CS 161 Winter 2016: HW3

; [!] Include OUR HW2 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-2-solution using *our* path, which will likely
;     differ from yours (Note: HW2 also includes HW1)
;(load "hw-2-solution")
; CS 161 Winter 2016: HW2 Solution

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
;(load "../hw1/hw-1-solution")
; CS 161 Winter 2016: HW1
; Solution


; -----------------------------------------------------------------------------
; Helper Functions
; -----------------------------------------------------------------------------

(defun REMOVE-FRAME (fr li)
    ;remove the frame
    (if (null li)
        (return-from REMOVE-FRAME li)
    )
    (if (null fr)
        nil
        (let ((cur (first li)) )
            (if (EQUAL-SF fr cur)
                (cdr li)
                (cons cur (REMOVE-FRAME fr (cdr li)))
            )
        )
    )
)



(defun checkVAR (variable)
;check if it is a variable
    (cond
    
        ((null variable) 
            nil
        )
        
        ( (and (listp variable) (equal 2 (length variable)) )
            (if (and (equal (nth 0 variable) 'V) (atom (nth 1 variable)) )
            
                t
                
                nil
            )
        )
        (t nil)
    )
)

;check if it's a list
(defun checkList (variable)

    (cond
        ((null variable)
            nil
        )
        ((listp variable)
            (loop for v in variable
                do
                    (if (not (listp v))
                       
                        (return-from checkList nil)    
                    )
            )
            t
        )
        (t
            nil
        )
    ) 
)

;check if the variable is a frame
(defun checkFrame (variable)
    (if (and (not (checkList variable )) (listp variable) )
        t
        nil
    )
)

(defun EXIST-IN-BD (variable bound)
;check if vari already exists in bd, if so return the value it is bound to
    (loop for e in bound
        do
            (if (listp e)
                (if (equal variable (car e))
                    (return-from EXIST-IN-BD (nth 1 e))
                )
            )   
    )
    nil
)

; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

; FUNCTION: UNIFY-FR
; PURPOSE:  Unifies the given variables, frames, or lists of frames by the
;           criteria listed in the spec
; INPUT:    LFR1: a variable, frame, or list of frames
;           LFR2: a variable, frame, or list of frames
;           BDS: [Optional; default: '(T)] A binding list being built during
;                execution
; OUTPUT:   Binding list


(defun unifyVar (variable x bound)

;some base cases 
    (if (equal variable x)
        (return-from unifyVar bound) 

    )
    (if (and (not (checkVAR x) ) (not (checkVAR variable)) )
                (return-from unifyVar nil)
    )
    (if (and (not (checkVAR (first variable))) (checkVAR x))
        (if (not (and (checkVAR x) (checkVAR variable) )) 
            (return-from unifyVar (unifyVar x variable bound))
        )
    )
    

    
    (let ((var-1 (EXIST-IN-BD variable bound)) (var-2 (EXIST-IN-BD x bound)) )
        (cond
            ((not (null var-1))
                (unifyVar var-1 x bound)
            )
            ((not (null var-2))
                (unifyVar variable var-2 bound)
            )
            (t
                (append bound (list (list variable x)))  
            )
        )
    )
)

(defun UNIFY-FRAMES (f1 f2 bound complete)
;unify two frames 
    (if complete
        (if (equal (car f1) (car f2))
            (UNIFY-FRAMES (cdr f1) (cdr f2) bound nil)
            nil
        )
        (if (< (length f1) 2)
            bound
            (let ((tail (member (car f1) f2))  )
                (if (null tail)
                    nil
                    (let ((result (UNIFY-FR (nth 1 f1) (nth 1 tail) bound) )  )
                        (if (null result)
                            nil
                            (let ((recurse-result (UNIFY-FRAMES (nthcdr 2 f1) f2 result nil)) )
                                recurse-result
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun UNIFY-FR-LIST (l1 l2 bound)

	;checking some base cases 
    (if (null l1)
        (return-from UNIFY-FR-LIST bound)
    )
    (if (or (null l2) (> (length l1) (length l2)))
        (return-from UNIFY-FR-LIST nil)
    )
        (let ((f1 (car l1)) )
            (loop for f2 in l2
                do
                    (let ((new-bound (UNIFY-FR f1 f2 bound)) )
                        
                        (if (not (null new-bound)) 
                            (let ((recurse-result (UNIFY-FR (cdr l1) (REMOVE-FRAME f2 l2) new-bound)) )
                                ;check if this binding is consistent with the biinding of the rest of frames
                                (cond
                                    ((equal (length l1) 1)
                                        (return-from UNIFY-FR-LIST new-bound)
                                    )
                                    (t
                                        (if (not (null recurse-result))
                                        
                                            (let ((u (union new-bound recurse-result)) )
                                                (return-from UNIFY-FR-LIST u)
                                            )   
                                        )
                                    )
                                )
                            )
                        )
                    )
            )
        )
)


(defun UNIFY-FR (lfr1 lfr2 &optional (bds '(T)))
;the main function
    (if (null bds)
        (return-from UNIFY-FR nil)
    )
    (cond
        ((equal lfr1 lfr2)
            bds
        )

        ((checkVAR lfr1)
            (if (checkList lfr2)
                nil
                (unifyVar lfr1 lfr2 bds) 
            )
        )
        ((checkVAR lfr2)
            (if (checkList lfr1)
                nil
                (UNIFY-VAR lfr1 lfr2 bds) 
            )
        )
        ((and (checkFrame lfr1) (checkFrame lfr2))
            (UNIFY-FRAMES lfr1 lfr2 bds t)
        )
        ((and (checkList lfr1) (checkList lfr2))
            (UNIFY-FR-LIST lfr1 lfr2 bds)
        )
        (t
            nil
        )
    )
)



; -----------------------------------------------------------------------------


; FUNCTION: SUBST-FR
; PURPOSE:  Substitutes the bindings in the given BDS into the corresponding
;           variables in the given frame
; INPUT:    FRM: a frame with variables
;           BDS: a binding list
; OUTPUT:   FRM with replacements made
(defun SUBST-HELPER (frame bds rFrame)

;checking a bunch of conditions here, to recursively get every element/word of the frame 
 (let ((first-e  (car frame)) )
    (cond
        ( (null frame) 
            rFrame
        )

        ( (and (atom first-e) (listp first-e) )
            (setq rFrame (append rFrame (list first-e)))
            (SUBST-HELPER (rest frame) bds rFrame)
        )
        ( (and (listp first-e) (equal (length first-e) 1))
            (setq rFrame (append rFrame (list first-e)))
            (SUBST-HELPER (rest frame) bds rFrame)
        )
        ( (atom first-e)
            (setq rFrame (append rFrame ( list first-e )))
            (SUBST-HELPER (rest frame) bds rFrame)
        )


        ( (and (listp first-e) (equal (length first-e) 2))
            (setq rFrame (append rFrame (list(EXIST-IN-BD first-e bds))))
            (SUBST-HELPER (rest frame) bds rFrame)
        )

        ( (and (listp first-e) (equal (length first-e) 2) (equal (EXIST-IN-BD first-e bds) nil))

            (setq rFrame (append rFrame (list first-e)))
            (SUBST-HELPER (rest frame) bds rFrame)
        )


        ( (and (listp first-e) (> (length first-e) 2))
             (setq rFrame (append rFrame (list (SUBST-HELPER first-e bds nil))))
             (SUBST-HELPER (rest frame) bds rFrame)
        )

        (t 
            (SUBST-HELPER (rest frame) bds rFrame)
        )
    )

 )
    
)


(defun SUBST-FR (frm bds)
    (if (null bds) 
             frm
            (let ((rFrame '()) )
                (loop for x in frm do
                    (if (and (listp x) (equal (length x) 2))
                            (loop for y in (cdr bds) do
                                (if (equal (car y) x)
                                        (setq rFrame (append rFrame (cdr y)))     
                                )
                            )
                        (if (listp x)
                            (setq rFrame (append rFrame (list (SUBST-HELPER x (cdr bds) nil))))
                            (setq rFrame (append rFrame (list x)))
                        )
                    )
                )
            rFrame
            )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: MP-INFER
; PURPOSE:  Attempts to unify the given rule's premises on the given facts,
;           and if successful, returns the conclusion of the rule with SUBST-FR
;           called on it using the successful binding list
; INPUT:    RULE: an if-then rule
;           O-FRAMES: a list of facts / concepts
; OUTPUT:   conclusion if successfully unified; nil otherwise
(defun MP-INFER (rule o-frames)
                    
   (let* ((prem (cdr (car rule))) (conc  (first (cdr (first (cdr  rule)) ))) ) 

            (if (or (null prem ) (null conc))
               (return-from MP-INFER nil)
            )
            (let* ((bding  (UNIFY-FR  prem o-frames  )))

                (SUBST-FR conc bding)        
            )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: FRW-CHAIN
; PURPOSE:  Performs simplified forward chaining given the list of rules and
;           facts, returning any new conclusions that are derived
; INPUT:    RULES: a list of if-then rules
;           EPMEM: a list of facts storing an episodic memory
;           NEW-EPMEM: [Optional: default: nil] the list of newly discovered
;                      facts grown through code execution and returned at the
;                      end
; OUTPUT:   NEW-EPMEM

(defun checkFF (frame list-x)
;we use equal-sf to check if two frames are equal
    (if (null list-x) 
        (return-from checkFF nil)
    ) 
    (if (EQUAL-SF frame (car list-x))
        (return-from checkFF t)
        (checkFF frame (cdr list-x))
    )
)

(defun FRW-CHAIN-HELPER (rules epmem newC &optional (new-epmem nil) )
;we use this helper function because we want a newC to store the thing
    (loop for a-rule in rules do 
        (setq new-epmem (MP-INFER a-rule epmem) )  
        (if (not (null (checkFF new-epmem epmem)) )  
            (setq new-epmem nil)
        )
       (setq epmem (cons new-epmem epmem))
        (if (not ( null new-epmem ))
            (if (not (null newC)) 
                (setq newC (append newC (list new-epmem)))
                (setq newC (list new-epmem))
            )
        )
      
    )
    (if (null new-epmem) 
        (return-from FRW-CHAIN-HELPER newC)
        (FRW-CHAIN-HELPER rules epmem newC )
    )
)

(defun FRW-CHAIN (rules epmem &optional (new-epmem nil))    
;we default newC to nil
    (FRW-CHAIN-HELPER rules epmem nil)
)


; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; Problem 4 Rule Definitions
; -----------------------------------------------------------------------------

(setq RULE-1 '((PREMISES
                    (TEACH AGENT(V x)
                        RECIP(HUMAN TYPE (STUDENTS))
                        OBJECT(CHEM)
                        LOC(HIGHSCHOOL)
                        SITU(V z))
               )
               (CONCLU
                    (STATE AGENT (V x)
                        TYPE(EMOTIONAL)
                        VALUE(HAPPY)
                        STIU(V z))
               ))
      
      ; RULE-2 was done for you! Yay!
      RULE-2 '((PREMISES
                    (INFORM AGENT (V x)
                        RECIP (V y)
                        OBJECT (V z)
                        SITU (V sa))
                    (AFTER ANTE (V sa)
                        CONSEQ (V sb))
               )
               (CONCLU
                    (KNOWS AGENT (V y)
                        OBJECT (V z)
                        SITU (V sb))
               ))
      
      RULE-3 '((PREMISES
                    (KNOWS 
                    AGENT (V x)
                    OBJECT( STATE AGENT (V x)
                            OBJECT (CANCER TYPE (TERMINAL)))
                    SITU (V sa)
                 )   
               )
               (CONCLU
                 (
             STATE TYPE (EMOTIONAL)
                    AGENT(V x)
                    VALUE (SAD)
                    SITU (V sa)
                )
           ))
      
      RULE-4 '((PREMISES
                    (MARRIED AGENT (V x)
                        OBJECT (V y)
                        SITU (V sa))
                    (STATE TYPE (PHYSICAL)
                        AGENT (V y)
                        VALUE (PREGNANT)
                        SITU (V sb))
               )
               (CONCLU
                    (SEX-ACT AGENT (V x)
                        OBJECT (V y)
                        SITU(V sa))
               ))
      
      RULE-5 '((PREMISES
                (TEACH AGENT (V x)
                    RECIP(HUMAN TYPE (STUDENTS))
                    OBJECT(CHEM)
                    LOC(HIGHSCHOOL)
                    SITU(V sa))
                (STATE TYPE (EMOTIONAL)
                    AGENT(V x)
                    VALUE(SAD)
                    SITU(V sb))
                (AFTER ANTE (V sa)
                    CONSEQ(V sb))
               )
               (CONCLU
                    (MAKES AGENT (V x)
                        OBJECT(COCAINE)
                        SITU(V sb))
               ))
      
      RULE-6 '((PREMISES
                (INGEST AGENT (V x)
                    OBJECT (COCAINE)
                    SITU(V sa))
                (STATE AGENT (V x)
                    OBJECT(LESIONS AREA (NOSE))
                    SITU (V sb)
                )
                (AFTER ANTE (V sa)
                    CONSEQ(V sb))
               )
               (CONCLU
                (CAUSE ANTE (INGEST AGENT (V x)
                    OBJECT (COCAINE)
                    SITU (V sa))
                   CONSEQ (STATE AGENT (V x)
                        OBJECT (LESIONS AREA (NOSE))
                        SITU (V sb)
                        )
                )
               )
               )
      
      RULE-7 '((PREMISES
                (MAKES AGENT (V x)
                    OBJECT (COCAINE)
                    SITU (V sa))
                (INGEST AGENT (V y)
                    OBJECT(COCAINE)
                    SITU(V sb))
                (AFTER ANTE (V sa)
                    CONSEQ(V sb))
               )
               (CONCLU
                (ACQUIRED AGENT (V y)
                    OBJECT (COCAINE)
                    FROM (V x)
                    SITU (V sb))
               )))




; -----------------------------------------------------------------------------
