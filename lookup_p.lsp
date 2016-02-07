; FUNCTION: LOOKUP-PHRASE
; PURPOSE:  Looks for the longest phrase at the start of sent that matches a
;           corresponding phrase in the input lex-mem, and if found, returns:
;           ((phrase frame demons) rest-sent)
;           ...for corresponding (phrase frame demons) triplet found in lex-mem
;           ...and rest-sent being the rest of the sentence minus the found phrase
;
;           If NOT found, returns:
;           ((phrase nil nil) rest-sent)
; INPUT:    sent: a list of English words
;           lex-mem: a lexical memory with ((phrase frame demon)*) triplets
; OUTPUT:   (see above in purpose)

(defun LIST-CONTAINS (f_word lex-men contain-list)
;return a list that contains the phrases that start with f_word
  (if (null lex-men)
    contain-list

    (let ( (phrase (first (first lex-men) ) )   )
          (if (equal (first phrase) f_word)
            (LIST-CONTAINS f_word (cdr lex-men) (cons phrase contain-list))
            
            (LIST-CONTAINS f_word (cdr lex-men) contain-list)
          )
      )

  )

)

(defun LIST-EQUAL (list1 list2)
;returns true if two lists are equal
  (if (equal (first list1) (first list2))
    (if (null (first list1) )
      t
      (LIST-EQUAL (cdr list1) (cdr list2) )
    )
    nil
  )
)

(defun GET-TRIPLET (phrase lex-men)
;returns the phrase-frame-demon triplet with phrase
  (if (null lex-men)
    nil
    (let ((cur-item (first lex-men))  )
      (let ((cur-phrase (first cur-item) )  )
        (if (LIST-EQUAL phrase cur-phrase)
          cur-item
          (GET-TRIPLET phrase (cdr lex-men))
        )
      )
    )
  )

  

)

(defun DELETE-FRONT (f-word phrase-list)
;for all the phrases in phrase-list, if the first word matches f-word, remove the first word from that phrase, otherwise remove the entire phrase
  (if (null phrase-list)
    nil
    (let ((recur-result (DELETE-FRONT f-word (cdr phrase-list) ) ) (phrase (first phrase-list)) (rest (cdr (first phrase-list))) )
      (if (and (equal f-word (first phrase) ) (not (null rest)) )
        (cons (cdr phrase) (DELETE-FRONT f-word (cdr phrase-list)) )

        (DELETE-FRONT f-word (cdr phrase-list))
      )
    )

  )
)

(defun WORD-IN-PHRASE-LIST (f-word phrase-list)
;returns true if any of the phrase in phrase-list contains f-word as the first word
  (if (null phrase-list)
    nil
    (if (equal f-word (first (first phrase-list)) )
      t
      (WORD-IN-PHRASE-LIST f-word (cdr phrase-list))
    )
  ) 
  
)

;IDEA for LONGEST-PHR:
;given a list of phrases as phrase-list,
;for the first word of sent
;for all the phrases in phrase-list, if phrase contains this word as the first word, then keep this phrase in the list but remove the first word from it
;otherwise, delete this phrase from the list
;recursively call LONGEST-PHR with (cdr sent) and the modified phrase-list as arguments


(defun LONGEST-PHR (sent phrase-list)
;returns the longest matching phrase in sent
  (if (null sent)
    nil
    (let ((f-word (first sent)) )
      (if (null (WORD-IN-PHRASE-LIST f-word phrase-list) ) 
        nil
        (let ((remove-front (DELETE-FRONT f-word phrase-list ) )  )  
          (cons f-word (LONGEST-PHR (cdr sent) remove-front))
        )
      )
    )
  )
)

(defun GET-REST (match sent)
;returns the rest of the sent (excluding the first few match words)
  (nthcdr (length match) sent)
)

(defun LOOKUP-PHRASE (sent lex-mem)
   (let ((containing (LIST-CONTAINS (first sent) lex-mem '() ))  )
      (if (null containing)
        (list (list (list (first sent)) nil nil) (cdr sent) )
        (let ((longest (LONGEST-PHR sent containing) ) )
          (list (GET-TRIPLET longest lex-mem) (GET-REST longest sent))
        )
        
      )

    ) 
)

;Algorithm:
;if lex-Mem doesn't contain the first word in sent, returns whatever
;else, check what is the longest matching phrase in sent that also appears in lex-mem
;get the corresponding triplet of this longest mecthing phrase

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
