; CS 161 Winter 2016: HW2 Solution Skeleton

; [!] Include OUR HW1 solution here!
; [!] IMPORTANT: Remove the below line when you submit your code!
;     We'll include the hw-1-solution using *our* path, which will likely
;     differ from yours
(load "hw-1-solution.lsp")


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
    (let* ((newSym (gensym (string symName))))
        (intern (string newSym))
    )
)



; -----------------------------------------------------------------------------
; Utility Functions
; -----------------------------------------------------------------------------

(defun CHECK-DUP (phrase mem)
  (loop for x in mem do 
      (if (equal (first x) phrase) 
        (setq *LM (remove x mem))
      )
  )
  nil
)


(defun ADD-TO-LM (phrase frame demons)
;check if there's duplicates 
  (CHECK-DUP phrase *LM)

;setup the lexicon memory
  (setq *LM (cons (cons phrase (cons frame (cons demons ()))) *LM))

;for returns
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




(defun GET-THREE (phrase LM)
  (if (null LM) nil
    
    (let* ((l (first LM))  )
    
      (let* ((b (first l) )  )
      
        (if (equal phrase b)
        
          l
          
          (GET-THREE phrase (REST LM))
          
        )
        
      )
      
      
    )
    
    
    
    
    
  )

  

)


(defun LIST-CONTAINS (firstWord LM contain-list)
;return a list that contains the phrases that start with firstWord
  (if (null LM)
    contain-list

    (let* ( (phrase (first (first LM) ) )   )
    
    
          (if (equal (first phrase) firstWord)
          ;if matches
            (LIST-CONTAINS firstWord (rest LM) (cons phrase contain-list))
        ;if not
            (LIST-CONTAINS firstWord (rest LM) contain-list)
          )
      )

  )

)

(defun DELETE-FRONT (firstWord phrase-list)
;for all the phrases in phrase-list, if the first word matches f-word
;, remove the first word from that phrase, otherwise remove the entire phrase
  (if (null phrase-list)
    nil
    (let* ( (phrase (first phrase-list)) (rest (cdr (first phrase-list))) )
      (if (and (equal firstWord (first phrase) ) (not (null rest)) )
        (cons (cdr phrase) (DELETE-FRONT firstWord (cdr phrase-list)) )

        (DELETE-FRONT firstWord (cdr phrase-list))
      )
    )

  )
)

(defun CONTAINS (firstWord phrase-list)
;returns true if any of the phrase in phrase-list contains f-word as the first word
  (if (null phrase-list)
    nil
    (if (equal firstWord (car (first phrase-list)) )
      t
      (CONTAINS firstWord (rest phrase-list))
    )
  ) 
  
)


(defun LONGEST-PHR (sent phrase-list)
;returns the longest matching phrase in sent
  (if (null sent)
    nil
    
    (let* ((f-word (first sent)) )
    
      (if (null (CONTAINS f-word phrase-list) ) 
      
        nil
        
        (let* ((remove-front (DELETE-FRONT f-word phrase-list ) )  )  
        
          (cons f-word (LONGEST-PHR (cdr sent) remove-front))
        )
        
        
      )
    )
    
  )
  
)


(defun LOOKUP-PHRASE (sent LM)
   (let ((l (LIST-CONTAINS (first sent) LM '() ))  )
   
      (if (null l)
      
        (list (list (list (first sent)) nil nil) (cdr sent) )
        
        (let* ((longest (LONGEST-PHR sent l) ) )
        
          (list (GET-THREE longest LM) (nthcdr (length  longest ) sent))
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

(defun NEWGAPS-HELPER (list is-frame)
  (if (<= (length list) 1)  
      nil
      (if is-frame
      
          (cons (first list) (NEWGAPS-HELPER (cdr list) nil))
          
          (let ((l (second list)) )
          
              (if (listp l)
              
                (append (list (first list) (NEWGAPS-HELPER l t)) (NEWGAPS-HELPER (nthcdr 2 list) nil))
                
                (if (boundp l)
                    (append (list (first list) l) (NEWGAPS-HELPER (nthcdr 2 list) nil))
                    
                      (let ((new-name (NEWATM l)) )
                      
                           
                           
                           (let ((to-return (append (list (first list) new-name ) (NEWGAPS-HELPER (nthcdr 2 list) nil))) )
                              
                              (set new-name nil)
                              to-return
                           )
                        
                      )
                    
                    
                )
              )
              
          )
      )
  )
  
)

(defun NEWGAPS (frame)
   (NEWGAPS-HELPER frame t)
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

;helper:
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



; -----------------------------------------------------------------------------


; FUNCTION: FIND-CON
; PURPOSE:  Returns the CONatom found by searching the *WM for a CONatom with a
;           pred of the given class starting at mycon in direction dir within
;           the global *WM
; INPUT:    mycon: where to start searching in *WM
;           dir: either 'BEF or 'AFT indicating search direction
;           class: predicate superclass for which to search
; OUTPUT:   found CONatom, or nil if not found
(defun FIND-AFT (mycon WM)
  (if (equal mycon (first WM)) 
      (cdr WM)
      (FIND-AFT mycon (cdr WM))
  )
)

(defun FIND-BEF (mycon WM)
  (FIND-AFT mycon (reverse WM) )
)

(defun FIND-DIR (mycon dir)
  (if (equal dir 'AFT)
     (FIND-AFT mycon *WM) 
     (FIND-BEF mycon *WM)
  )  
)

(defun FIND-CON (mycon dir class)
    (loop for x in (FIND-DIR mycon dir) do
       (if (IS-SUBCLASS (first (eval x)) class) 
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

(defun MAINCHECK(x para-list)
      (cond 
          ((null para-list) 
              nil
          )
         
          ((equal x (first para-list)) 
              t
          )
      		  ((not (atom (first para-list))) 
              (MAINCHECK x (append (first para-list) (cdr para-list)))
          )
          (t 
              (MAINCHECK x (cdr para-list))
          )
      )
)

(defun CHECKS-AGAINST-WM (mycon)
    (loop for x in *WM
      do
        (if (equal (MAINCHECK mycon (eval x)) t)
          (return-from CHECKS-AGAINST-WM nil)
        )
    )
    t
)

(defun CHECKS-IN-ACT (mycon)
  (if (and (IS-SUBCLASS (first (eval mycon)) 'ACT) (CHECKS-AGAINST-WM mycon))
    mycon
    nil
  )
)

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
(defun GEN-NEW (demons conatm)
  (loop for x in demons 
    collect (append (list (first x) ) (list conatm) (cdr x)   )
  )
)

(defun GEN-DEMS (demons conatm)
  (if (null demons)
    nil
    (setq *DM (append (GEN-NEW demons conatm) *DM))
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
    ;(return-from DEM_SRCH nil)
    (let* ((l (FIND-CON mycon dir class)) )
      (if (null l)
        nil
        (let* ((slot (GET-SF myslot (eval mycon)) ))
         (set slot l)
        )
      )
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
    (if (null l)
      (return-from DEM-AMEND nil)
    )
    (set l (AMEND-SF myslot myfiller l))
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
      (if (null l)
          (return-from DEM-REF nil)
      )
      (set l (AMEND-SF myslot mycon l))
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
   (if (not (null *DM))
      (loop for x in *DM do 
      (let* ((l (apply (first x) (cdr x))) )
        (if (not (null l))
            (setq *DM (remove x *DM))
        )
      )       
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
    (if (not (null sent))
      (let* ((l (LOOKUP-PHRASE sent *LM)) )
          (let* ((b (first l)) (newCon (NEWATM 'con)) )
            
            (set newCon (nth 1 b))
            (setq *WM (append *WM (list newCon)))

            (GEN-DEMS (car (last b)) newCon)
            
            (DEM-EXEC)
            (PARSE-SENT (nth 1 l))
          )
      )
    )
    
)

; -----------------------------------------------------------------------------



