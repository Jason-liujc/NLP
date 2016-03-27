; CS 161 Winter 2016: HW2 Solution

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
(load "hw-1-solution")


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

; FUNCTION: ADD-TO-LM-EXEC
; PURPOSE:  Helper for ADD-TO-LM that simply checks for duplicate entries in LEX-MEM
;           before committing to a phrase addition
; INPUT:    phrase: a list of English words
;           frame: a frame associated with that phrase
;           demons: a list of 0 or more demon instantiations
;           lex-mem: current recursive state of input *LM
; OUTPUT:   current status of lexmem after execution
(defun ADD-TO-LM-EXEC (phrase frame demons lex-mem)
    (cond
        ; Base case: lex-mem is null so just insert
        ((null lex-mem) (list (list phrase frame demons)))
        ; Base case: we found a phrase match at the current front
        ; of lex-mem, so replace with our new definition
        ((equal (first (first lex-mem)) phrase) (cons (list phrase frame demons) (rest lex-mem)))
        ; Recursive case: haven't found our matching phrase in lex yet, so keep
        ; looking at the front and recursing
        (t (append (list (first lex-mem)) (ADD-TO-LM-EXEC phrase frame demons (rest lex-mem))))
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
    ; Perform the insertion or modification of *LM
    (setq *LM (ADD-TO-LM-EXEC phrase frame demons *LM))
    ; Return the created triplet
    (list phrase frame demons)
)

; -----------------------------------------------------------------------------

; FUNCTION: MATCH-PH
; PURPOSE:  Helper for LOOKUP-PHRASE that returns length of lexical index 
;           (word/phrase) if a match with the start of word,
;           0 otherwise
; INPUT:    sent: a list of English words
;           phrase: a lexical entry phrase
; OUTPUT:   length of lexical index match
(defun MATCH-PH (sent phrase)
    (let* ((len (length phrase)))
        (cond
            ; Not enough words left, so no dice
            ((< (length sent) len) 0)
            ; First len entries of words indeed match, so return len
            ((equal (subseq sent 0 len) phrase) len)
            ; Otherwise, return 0
            (t 0)
        )
    )
)

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
    (cond
        ; Base cases: return nil if either params null
        ((null sent) nil)
        ((null lex-mem) nil)
        
        ; Got to end of lex-mem, so return the best match, or special unknown
        ; word frame
        ((= (length lex-mem) 1)
            (let* (
                (lex (first lex-mem))
                (match-len (MATCH-PH sent (first lex)))
            )
            (if (> match-len 0)
                ; If match, build list of phrase, frame, demons as the first
                ; element, with the rest of the sentence tacked on after
                (list (list (subseq sent 0 match-len) (second lex) (third lex))
                      (nthcdr match-len sent))
                
                ; If no match, special return value
                (list (list
                    ; Unknown word goes as its own list in front, with NIL
                    ; frame and demons
                    (list (first sent)) NIL NIL)   
                    ; Remove first word for rest of sentence
                    (rest sent))
            ))
        )
        
        ; Recursive case: find best match between first two entries. Keep
        ; first entry if neither match.
        (t (let* (
                (match1 (MATCH-PH sent (first (first lex-mem))))
                (match2 (MATCH-PH sent (first (second lex-mem))))
            )
            ; Remove the worst match of the first 2 entries
            (if (>= match1 match2)
                (LOOKUP-PHRASE sent (cons (first lex-mem) (nthcdr 2 lex-mem)))
                (LOOKUP-PHRASE sent (rest lex-mem)))
            )
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: UNIQUE-GAPSLOTS (helper for NEWGAPS)
; PURPOSE:  Looks for gaps in a list of slot-filler pairs, dispatching
;           NEWGAPS on each filler.
; INPUTS:   sf: list of (slot filler) pairs
; OUTPUT:   List of slot-filler pairs with any gaps replaced by NEWATMs
(defun UNIQUE-GAPSLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch TOKENIZE on our first filler
        (t (append (append (list (first sf))                  ; rebuild our first slot-filler pair
                           (list (NEWGAPS (second sf))))      ; dispatch NEWGAPS on the filler
                           (UNIQUE-GAPSLOTS (nthcdr 2 sf))))  ; recurse on rest of sf
    )
)

; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names
(defun NEWGAPS (frame)
    (cond
        ; Base case: nil input -- just return nil
        ((null frame) nil)
        ; Base case: we got an atom, so uniquify it, and return it
        ((atom frame) (let* ((gap (NEWATM frame))) (set gap NIL) gap))
        ; Base case: empty, or single pred frame
        ((<= (length frame) 1) frame)
        ; Recursive case: dispatch UNIQUE-GAPSLOTS on our slot-filler list
        (t (cons (first frame) (UNIQUE-GAPSLOTS (rest frame))))
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: IS-SUBCLASS
; PURPOSE:  Queries the global *TX for whether or not the given entity
;           ISA member of the given class
; INPUT:    entity, class: items in a class hierarchy
; OUTPUT:   boolean indicating hierarchical relationship
(defun IS-SUBCLASS (entity class &optional (sub-hier *TX))
    (cond
        ; Base case: same predicates
        ((equal entity class) t)
        ; Base case: "MEMB atm1 atm2" is in the hierarchy
        ((member (list 'MEMB entity class) sub-hier) t)
        
        ; Recursive case: "MEMB entity predi" where "MEMB predi class" for some i
        (t (let*
             ; Find a parent of entity (hierarchy entry with second entry class)
             ((parent (find-if (lambda (p) (equal (second p) entity)) sub-hier))
              
              ; Need to remove that entry to avoid infinite loop
              (next-hier (remove parent sub-hier)))
             ; If predi MEMB class, then success. Else look for a different predi
             ; (predi is the third element of parent, since parent = (MEMB entity predi))
             (cond 
                 ; If we didn't find such a parent, we're done
                 ((null parent) nil)
                 ; Case 1: predi MEMB class: success
                 ((IS-SUBCLASS (third parent) class next-hier) t)
                 ; Case 2: entity MEMB predi ... MEMB class for some different predi
                 ((IS-SUBCLASS entity class next-hier) t)
                 ; Else: failure
                 (t nil)
            ))
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: START-FIND-CON
; PURPOSE:  A helper for FIND-CON that locates a particular atom inside wm, 
;           and returns the remainder of wm in the given direction
; INPUT:    mycon: where to start searching in *WM
;           wm: current recursive state of the *WM we're searching in
;           dir: either 'BEF or 'AFT indicating search direction
; OUTPUT:   list of remaining CONatoms in a given direction
(defun START-FIND-CON (mycon wm dir)
    (cond
        ; Why have two cases when we can have one? Just do a forward search in
        ; the reversed list for a BEF search.
        ((equal dir 'BEF) (START-FIND-CON mycon (reverse *WM) 'AFT))
        ; Base case: searched everything
        ((null wm) nil)
        ; Base case: found what we were looking for, so just return the rest
        ((equal (first wm) mycon) (rest wm))
        ; Recursive case: no dice, so keep looking
        (t (START-FIND-CON mycon (rest wm) dir))
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
(defun FIND-CON (mycon dir class &optional found (wm *WM))
    (cond
        ; As a first step, find our starting CON. Flag that we found it
        ; by setting optional parameter "found"
        ((not found) (FIND-CON mycon dir class t (START-FIND-CON mycon *WM dir)))
        
        ; Base case: searched everything
        ((null wm) nil)
        
        ; We already found the start atom, so start looking at predicates
        (t
             ; See if pred matches (have to expand the first atm in atmlst
             ; into its frame representation first)
             (if (IS-SUBCLASS (first (eval (first wm))) class)
                 ; A match, so return it
                 (first wm)
                 ; Else, no match; remove and try again
                 (FIND-CON mycon dir class t (rest wm))
             )
        )
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: ACT-WITHIN?
; PURPOSE:  Returns whether or not the given CONatom appears anywhere within
;           the given CONatom's frame
; INPUT:    candidate: CONatom to look for
;           frame: frame in which to look
; OUTPUT:   boolean designating whether or not it appears
(defun ACT-WITHIN? (candidate frame)
    (cond
        ; Base case: null frame
        ((null frame) nil)
        ; Complex case: we found an atom...
        ((atom frame) (cond
            ; Base case: frame is an atom? Check equivalence to candidate
            ; If it's equivalent, return t
            ((equal candidate frame) t)
            ; Recursive case: frame is an atom and bound? Check its eval
            ((and (boundp frame) (not (null (eval frame)))) (ACT-WITHIN? candidate (eval frame)))
            ; Default case: frame is an atom and unbound, so return nil since
            ; we already checked in the first case as to whether it was equal
            ; to the query conatm
            (t nil)
        ))
        ; Base case: frame is null, or single pred, so return nil
        ((<= (length frame) 1) nil)
        ; Recursive case: Check the front filler and then recurse on other
        ; slot-fillers in the current frame
        (t (or (ACT-WITHIN? candidate (front-filler frame)) (ACT-WITHIN? candidate (pop-slot frame))))
    )
)

; FUNCTION: UNUSED-ACT?
; PURPOSE:  Returns whether or not the given CONatom appears anywhere within
;           a CONatom's frame of the *WM
; INPUT:    candidate: CONatom to look for
; OUTPUT:   boolean designating whether or not it appears
(defun UNUSED-ACT? (candidate)
    ; Check if the candidate is within any of the CONatoms in the *WM
    ; We'll negate the output for nice semantic interpretation
    (not (loop for conatom in *WM do
        (if (ACT-WITHIN? candidate (eval conatom)) (return t))
    ))
)

; FUNCTION: MAIN-ACT
; PURPOSE:  Returns the first CONatom in *WM whose frame is an ACT, and is NOT
;           embedded in another *WM frame
; INPUT:    N/A
; OUTPUT:   CONatom
(defun MAIN-ACT (&optional (wk-mem *WM))
    (let* ((candidate (first wk-mem)))
        (cond
            ; Base case: ran out of candidates to check, so return nil
            ((null candidate) nil)
            ; Base case: first element is an ACT and is unused in the *WM,
            ; so we've found our MAIN-ACT
            ((and (IS-SUBCLASS (first (eval candidate)) 'ACT) (UNUSED-ACT? candidate)) candidate)
            ; Recursive case: Either the candidate wasn't an ACT or it was
            ; used somewhere in the *WM, so keep looking
            (t (MAIN-ACT (rest wk-mem)))
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
    (cond
        ((null demons) *DM)
        (t
            (let* (
                (dem (first demons))
                ; Construct new demon as ((first dem) conatm (rest dem))
                (newdemon (append (list (first dem)) (list conatm) (rest dem) )))
                ; Update global *DM
                (setq *DM (cons newdemon *DM))
                ; Recurse on any remaining demons
                (cons newdemon (GEN-DEMS (rest demons) conatm))
            )
        )
    )
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
    (let* ((frame (eval mycon))
           (gap (GET-SF myslot frame))
           (found (FIND-CON mycon dir class)))
        ; If we find a binding from our FIND-CON result, then set gap to found,
        ; and return found for having successfully completed; otherwise nil
        (if found (progn (set gap found) found) nil)
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
    (let* ((found (FIND-CON mycon dir class)))
        (if (null found)
            ; If we didn't find anything, then just return nil
            nil
            ; Otherwise, we found something, so perform the insertion
            (progn
                (set found (AMEND-SF myslot myfiller (eval found)))
                ; Finally, return the found CONatom
                found
            )
        )
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
    ; Functionally equivalent to calling DEM-AMEND with mycon for the filler
    (DEM-AMEND mycon myslot mycon dir class)
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
(defun DEM-EXEC (&optional no-change)
    (cond
        ; Base case: nothing to do, or all demons successful
        ((null *DM) nil)
        ; Flag for change based on last recursive call (no-change will
        ; be truthy when it is not null)
        (no-change *DM)
        ; Gather all unsuccessful demons
        (t (DEM-EXEC
               (let* (
                    ; old-dems stores the state before running demons
                    (old-dems *DM)
                    ; We'll collect all those demons that didn't die
                    (collected (loop for dem in *DM append
                        ; Call demon
                        (if (apply (first dem) (rest dem))
                             nil        ; If success, don't add back
                             (list dem) ; On failure, do add back
                    ))))
                    ; Update the *DM with remaining demons
                    (setq *DM collected)
                    ; We'll add as a param whether or not any change in *DM
                    ; occured based on its old value
                    (and (subsetp old-dems *DM) (subsetp *DM old-dems))
               ))
        )
        ; Recursive call will check whether or not we removed anything
    )
)

; -----------------------------------------------------------------------------


; FUNCTION: PARSE-SENT
; PURPOSE:  Performs a conceptual anaylsis of the input SENT using the known
;           phrases and interpretations within the global *LM.
; INPUT:    sent: list of English words comprising a sentence
; OUTPUT:   frame consisting of: (EXPAND (MAIN-ACT))
(defun PARSE-SENT (sent)
    ; match-result = (((phrase) frame (demons*)) (remaining-sent))
    (let* ((match-result (LOOKUP-PHRASE sent *LM))
           (pfd (first match-result))
           (phrase (first pfd))
           (frame (second pfd))
           (demons (third pfd))
           (next-sent (second match-result)))
    
    ; When we've exhausted the sentence, return the EXPAND of MAIN-ACT
    (if (null sent)
        (EXPAND (MAIN-ACT))
        ; Else, process frame/demons, then recurse
        (progn
            ; Create a new CONatom to house the newly found frame
            (let* ((newcon (NEWATM 'CON)))
                ; Bind that to the found frame
                (set newcon (NEWGAPS frame))
                (setq *WM (append *WM (list newcon)))
                ; Generate demons that now work for this conatom
                (GEN-DEMS demons newcon)
            )
            ; Call the active demons, which will update the DEMON-MEM after
            ; reaching quiescence
            (DEM-EXEC)
            ; Finally, recurse: process next part of sentence after removing
            ; matched phrase
            (PARSE-SENT next-sent)
        )
    ))
)

; -----------------------------------------------------------------------------

