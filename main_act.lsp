; FUNCTION: MAIN-ACT
; PURPOSE:  Returns the first CONatom in *WM whose frame is an ACT, and is NOT
;           embedded in another *WM frame
; INPUT:    N/A
; OUTPUT:   CONatom

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

(defun CHECKS-AGAINST-WM (con)
  (loop for v in *WM
    do
      (if (not (IS-SUBCLASS (car (EVAL v)) 'ACT))
        (if (not(null (MEMBER con (eval v))))
          (return nil) ;;I WANT TO BREAK OUT OF HERE AND RETURN NIL FOR THIS FX
        )
      )
  )
  t
)

(defun CHECKS-IN-ACT (x)
  (print x)
  (print (CHECKS-AGAINST-WM x))
  (if (and (IS-SUBCLASS (car (EVAL x)) 'ACT) (CHECKS-AGAINST-WM x))
    x
  )
  nil
)

(defun MAIN-ACT ()
  (loop for x in *WM
    do 
      (if (not (equal (CHECKS-IN-ACT x) nil))
        (return x)
      )
  )
)    

;ALGORITHM
;Call each is-subclass on each subclass in TX with ACT as second variable
;if true, use get-sf, to test if it's embeded in another frame. 
; TA notes: need to use nested loop.

; -----------------------------------------------------------------------------
; Test Helper functions
; -----------------------------------------------------------------------------


    (setq *TX '(
      (MEMB INGEST PHYS-ACT)
      (MEMB COMMUN MENTAL-ACT)
      (MEMB TEACH MENTAL-ACT)
      (MEMB THINK ACT)
      (MEMB PHYS-ACT ACT)
      (MEMB MENTAL-ACT ACT)
    ))

    (setq *WM '(CON1 CON2 CON3 CON4 CON5))
    ;(setq *WM '(CON1 CON3 CON4 CON5 CON6))
    (setq CON1 '(HUMAN F-NAME (DORON) GENDER (MALE)))
    (setq CON2 '(THINK AGENT CON1 OBJECT CON4))
    (setq CON3 '(HUMAN F-NAME (MARY) GENDER (FEMALE)))
    (setq CON4 '(TEACH AGENT CON1 OBJECT CON4))
    (setq CON5 '(HUMAN F-NAME (BETTY) GENDER (FEMALE)))
    (setq CON6 '(HUMAN F-NAME (GEORGE) ACTION CON4))

(print 

(MAIN-ACT)

)

