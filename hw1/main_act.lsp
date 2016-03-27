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



;; CREATE A HELPER FUNCTION THAT IS RECURSION TO REPLACE MEMBER

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

(defun MAIN-ACT ()
  (loop for x in *WM
    do 
      (if (not (equal (CHECKS-IN-ACT x) nil))
        (return x)
      )
  )
)    

; ALGORITHM
; Loop through *WM, checks if x is a subclass of 'ACT, if so, check if x is used
; in any of the other cons in *WM (by recursion), if it is used, return nil. If it's not nil 
; after, return x

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
