; -----------------------------------------------------------------------------


; FUNCTION: NEWGAPS
; PURPOSE:  Replaces all gaps in the input frames with unique, unbound gap names
;           and returns a copy of the resulting frame
; INPUT:    frame: a frame
; OUTPUT:   frame with now unique gap-names

(defun NEWATM (symName)
    ; Generate the new symbol using gensym
    (let* ((new-sym (gensym (string symName))))
        (intern (string new-sym))
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

(defun NEWGAPS (frame)
   (NEWGAPS-HELP frame t)
)



