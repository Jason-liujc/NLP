; CS 161 Winter 2016: HW2 Solution Skeleton

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
(load "../hw1/hw-1-solution")


; -----------------------------------------------------------------------------
; Helper functions
; -----------------------------------------------------------------------------

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



; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------


; FUNCTION: ADD-TO-LM
; PURPOSE:  Adds the given (phrase frame demon) triplet to the global LEX-MEM,
;           making sure to not add duplicate phrases
; INPUT:    phrase: a list of English words
;           frame: a frame associated with that phrase
;           demons: a list of 0 or more demon instantiations
; OUTPUT:   phrase-frame-demon triplet constructed
(defun ADD-TO-LM (phrase frame demons)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


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


; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names
(defun NEWGAPS (frame)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: IS-SUBCLASS
; PURPOSE:  Queries the global *TX for whether or not the given entity
;           ISA member of the given class
; INPUT:    entity, class: items in a class hierarchy
; OUTPUT:   boolean indicating hierarchical relationship
(defun IS-SUBCLASS (entity class)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: FIND-CON
; PURPOSE:  Returns the CONatom found by searching the *WM for a CONatom with a
;           pred of the given class starting at mycon in direction dir within
;           the global *WM
; INPUT:    mycon: where to start searching in *WM
;           dir: either 'BEF or 'AFT indicating search direction
;           class: predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if not found
(defun FIND-CON (mycon dir class)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: MAIN-ACT
; PURPOSE:  Returns the first CONatom in *WM whose frame is an ACT, and is NOT
;           embedded in another *WM frame
; INPUT:    N/A
; OUTPUT:   CONatom
(defun MAIN-ACT ()
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: GEN-DEMS
; PURPOSE:  Inserts the given CONATM at the front of every partial-demon-
;           instance in the given demons and then adds those completed
;           demon instantiations to the global *DM
; INPUT:    demons: a list of partial-demon-instantiations of the format:
;           ((demon-name arg2 arg3 ...)*)
;           conatm: a CONatom indicating which frame the demons work for
; OUTPUT:   current state of *DM after insertion
(defun GEN-DEMS (demons conatm)
    'UNIMPLEMENTED
)



; -----------------------------------------------------------------------------
; Here There Be Demons
; -----------------------------------------------------------------------------

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
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: DEM-AMEND
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given myfiller. 
;           Returns the found CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           myfiller: the filler of myslot to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-AMEND (mycon myslot myfiller dir class)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: DEM-REF
; PURPOSE:  Searches the *WM for a CONatom whose frame is a member of the type
;           of the given class. If found, then inserts the top-level slot given
;           by myslot in the found CONatom to the given mycon. 
;           Returns the found CONatom if found, NIL otherwise. 
; INPUT:    mycon: the conatm this demon works for in the WK-MEM
;           myslot: the slot-name to insert
;           dir: either 'BEF or 'AFT indicating search direction
;           class: the predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if nothing found
(defun DEM-REF (mycon myslot dir class)
    'UNIMPLEMENTED
)



; -----------------------------------------------------------------------------
; Workhorse Functions
; -----------------------------------------------------------------------------

; FUNCTION: DEM-EXEC
; PURPOSE:  Repeatedly calls each active demon instantiation within the global
;           *DM until all active demons return nil. Whenever a demon
;           returns something non-nil, we remove it from the global *DM
;           and then call each remaining demon again
; INPUT:    N/A
; OUTPUT:   Status of *DM after executing all active demons (a list of all
;           remaining, active demon instantiations)
(defun DEM-EXEC ()
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------


; FUNCTION: PARSE-SENT
; PURPOSE:  Performs a conceptual anaylsis of the input SENT using the known
;           phrases and interpretations within the global *LM.
; INPUT:    sent: list of English words comprising a sentence
; OUTPUT:   frame consisting of: (EXPAND (MAIN-ACT))
(defun PARSE-SENT (sent)
    'UNIMPLEMENTED
)

; -----------------------------------------------------------------------------

