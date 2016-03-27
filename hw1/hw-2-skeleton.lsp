; CS 161 Winter 2016: HW2 Solution Skeleton

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
;(load "../hw1/hw-1-solution")

; CS 161 Winter 2016: HW1
; Solution


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
; Main Functions
; -----------------------------------------------------------------------------

; FUNCTION: GET-SF
; PURPOSE:  Returns the filler of the given slot-name
; INPUTS:   slot: an atom designating the slot name
;           frame: a frame
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
(defun GET-SF (slot frame)
    (cond
        ; Base case: predicate with no slots (or empty frame)
        ((<= (length frame) 1) nil)
        ; If first slot matches, return its filler.
        ((equal slot (front-slot frame)) (front-filler frame))
        ; Else, first slot does not match, so test the rest of the slots
        (t (GET-SF slot (pop-slot frame)))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: GET-NESTED-SF
; PURPOSE:  Returns the filler at the end of the given slot-path, or nil if
;           the path does not exist in the given frame
; INPUTS:   slots: a path of slots
;           concept: a frame
; OUTPUT:   The requested filler, which will be a frame or gap
(defun GET-NESTED-SF (slots concept)
    (cond
        ; Base case: got to the last slot, so stop recursing
        ((null slots) concept)
        ; Base case: null concept
        ((null concept) nil)
        ; If our concept is a gap to expand, we need to recurse on the frame
        ; Return nil if it's a gap but not bound
        ((atom concept) (if (boundp concept) (GET-NESTED-SF slots (eval concept)) nil))
        ; Recursive case: continue following path on sub-frame matched by filler
        ; of current path element
        (t (GET-NESTED-SF (rest slots) (GET-SF (first slots) concept)))
    )
)

; -----------------------------------------------------------------------------


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


; FUNCTION: AMEND-SF-EXEC
; PURPOSE:  (Workhorse Helper for AMEND-SF)
;           Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist) [Same inputs / outputs]
(defun AMEND-SF-EXEC (slot filler frame)
    (cond
        ; Base case: single predicate, so add the slot
        ((<= (length frame) 1) (cons (first frame) (cons slot (list filler))))
        ; Base case: If first slot is target, replace the filler, keep rest slots
        ((equal slot (front-slot frame))
         (cons (first frame) (append (append (list slot) (list filler) (nthcdr 3 frame))))
        )
        ; Recursive case: First slot not target, so pop and recurse
        (t (append (AMEND-SF-EXEC slot filler (pop-slot frame)) (cons (front-slot frame) (list (front-filler frame)))))
    )
)

; FUNCTION: AMEND-SF
; PURPOSE:  Returns a copy of the input concept with the given slot-filler
;           pair added. If the slot already exists in the frame, its filler
;           will be replaced by the given input filler
; INPUTS:   slot: an atom representing the slot name
;           filler: a filler to place in the corresponding slot
;           frame: the frame being operated on
; OUTPUT:   Frame with added / replaced slot-filler pair
(defun AMEND-SF (slot filler frame)
    ; Check to see if we've been given a bound gap (just in case, you know?)
    (if (atom frame) 
        (if (boundp frame) (AMEND-SF-EXEC slot filler (eval frame)) frame)
        (AMEND-SF-EXEC slot filler frame)
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: EQUAL-SF-COMP
; PURPOSE:  Helper function that performs actual workhorse of frame comparison
;           on EXPANDed frame inputs
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: [EXPANDED] FRAME (first frame to compare)
;           frame2: [EXPANDED] FRAME (second frame to compare)
(defun EQUAL-SF-COMP (frame1 frame2)
    (cond
        ; Base case: empty frames match
        ((and (null frame1) (null frame2)) t)
        ; Base case: frames with different preds do not match
        ((not (equal (f-pred frame1) (f-pred frame2))) NIL)
        ; Base case: frames of different lengths do not match
        ((not (= (f-length frame1) (f-length frame2))) NIL)
        ; Base case: bare predicates (or matching symbols) match
        ((<= (f-length frame1) 1) t)
        ; Base case: variables match iff they have the same name
        ; [!] Not required for HW1, will not be tested
        ((equal (first frame1) 'V) (equal frame1 frame2))
        
        ; Recursive case: check frame1's front slot, then remove and recurse to
        ; check the rest of the slots.
        (t (let ((front (front-slot frame1))) 
            (and (EQUAL-SF-COMP (GET-SF front frame1) (GET-SF front frame2))
                 (EQUAL-SF-COMP (pop-slot frame1) (rm-slot front frame2)))
            )
        )
    )
)

; FUNCTION: EQUAL-SF
; PURPOSE:  Boolean predicate which compares two frames
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)
(defun EQUAL-SF (frame1 frame2)
    (let ((EX-frame1 (EXPAND frame1)) (EX-frame2 (EXPAND frame2)))
        (EQUAL-SF-COMP EX-frame1 EX-frame2)
    )
)

; -----------------------------------------------------------------------------


; Mark as included for examples file
; (this is not necessary for solution correctness, but avoids some warnings
; on the hw-1-examples.lsp)

(setq HW-1-SOLN-INCLUDED t)














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

;if phrase is an element of mem, remove that frame
(defun CHECK-DUPLICATE (phrase mem)
  (loop for x in mem
    do 
      (if (equal (car x) phrase) (setq *LM (remove x mem) )
      )
  )
  NIL
)

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

(defun NEWGAPS-HELP (list is-frame)
  (if (<= (length list) 1)  
      nil
      (if is-frame
          (cons (first list) (NEWGAPS-HELP (cdr list) nil))
          (let ((curr (second list)) )
              (if (listp curr)
                (append (list (first list) (NEWGAPS-HELP curr t)) (NEWGAPS-HELP (nthcdr 2 list) nil))
                (if (boundp curr)
                    (append (list (first list) curr) (NEWGAPS-HELP (nthcdr 2 list) nil))
                    
                      (let ((new-name (NEWATM curr)) )
                      
                           
                           
                           (let ((to-return (append (list (first list) new-name ) (NEWGAPS-HELP (nthcdr 2 list) nil))) )
                              
                              (set new-name nil)
                              ;don't use setq
                              to-return
                           )
                        
                      )
                    
                    
                )
              )
              
          )
      )
  )
  
)

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

(defun RECURSIVELY-CHECKS-FRAME (x v)
(cond ((null v) nil)
  ((not (atom (car v))) (RECURSIVELY-CHECKS-FRAME x (append (car v) (cdr v))))
  ((equal x (car v)) t)
  (t (RECURSIVELY-CHECKS-FRAME x (cdr v)))))

(defun CHECKS-AGAINST-WM (con)
  (let ((flag t))
    (loop for v in *WM
      do
        (if (equal (RECURSIVELY-CHECKS-FRAME con (eval v)) t)
          (setq flag nil)
        )
    )
    flag
  )
)

(defun CHECKS-IN-ACT (x)
  (if (and (IS-SUBCLASS (car (EVAL x)) 'ACT) (CHECKS-AGAINST-WM x))
    x
    nil
  )
)

(defun GEN-DH (demons conatm)
  (loop for x in demons 
    collect (append (list (car x) ) (list conatm) (cdr x)   )
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
    (CHECK-DUPLICATE phrase *LM)
   
  (setq *LM (cons (cons phrase (cons frame (cons demons ()))) *LM))

  (append (cons phrase (cons frame (cons demons ()))))
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

; -----------------------------------------------------------------------------


; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names
(defun NEWGAPS (frame)
       (NEWGAPS-HELP frame t)
)

; -----------------------------------------------------------------------------


; FUNCTION: IS-SUBCLASS
; PURPOSE:  Queries the global *TX for whether or not the given entity
;           ISA member of the given class
; INPUT:    entity, class: items in a class hierarchy
; OUTPUT:   boolean indicating hierarchical relationship
(defun IS-SUBCLASS (entity class)
    (SUBCLASS-HELPER *TX entity class )
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
    (loop for x in (LIST-DIR mycon dir) do
    ;(print (eval x))
       (if (IS-SUBCLASS (car (eval x )) class) 
        (return x)
      )
    nil
  )
)

; -----------------------------------------------------------------------------


; FUNCTION: MAIN-ACT
; PURPOSE:  Returns the first CONatom in *WM whose frame is an ACT, and is NOT
;           embedded in another *WM frame
; INPUT:    N/A
; OUTPUT:   CONatom
(defun MAIN-ACT ()
      (loop for x in *WM
      do 
      (if (not (equal (CHECKS-IN-ACT x) nil))
        (return x)
      )
  )
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
    (setq *DM (append (GEN-DH demons conatm)  *DM )  )
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
    (let* (( l (FIND-CON mycon dir class) ) (b (GET-SF myslot  (eval mycon) ))  )
      ;(print (eval l))
      ;(print b)
      (if (NULL l) nil)
      
      (set b l)

    )
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
      (let* ((l (FIND-CON mycon dir class)))
    (set l ;the con element
      (AMEND-SF myslot myfiller l  )
    )
    l
  )
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
    (let* ((l (FIND-CON mycon dir class)  ))
      (if (NULL l) nil)
      (set l (AMEND-SF myslot mycon l)  )
      
      l
  )
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
       (loop for x in *DM do 
      (eval (list x))
      
      (if (not (null x))
        (setq *DM (remove x *DM))
      )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: PARSE-SENT
; PURPOSE:  Performs a conceptual anaylsis of the input SENT using the known
;           phrases and interpretations within the global *LM.
; INPUT:    sent: list of English words comprising a sentence
; OUTPUT:   frame consisting of: (EXPAND (MAIN-ACT))
(defun PARSE-SENT (sent)
        (let ((x (LOOKUP-PHRASE sent *LM)))
    ; x is now the associated phrase-frame-demon triplet
      (let ((y (NEWATM 'con)))

        (setq y (NEWGAPS x))
        (append *WM y)

        (GEN-DEMS X Y)

        (DEM-EXEC)

        (PARSE-SENT (rest x))
      )
    )
)

; -----------------------------------------------------------------------------

