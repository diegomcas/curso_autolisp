;Ejemplos de SI, entonces. NO, entonces.
(defun comparaciones()
  (princ "Antes de controlar los flujos de nuestro programa, tenemos que hacer comparaciones.\n")
  (princ "La primera y más básica es la de igualdad (idéntica) entre cosas. (eq)\n")
  (setq f1 '(a b c)) 
  (setq f2 '(a b c)) 
  (setq f3 f2)
  (eq f1 f3) ;Si bien contienen los mismos valores NO SON LA MISMA LISTA
  (eq f3 f2) ;SON LA MISMA LISTA
  
  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))
  
  (princ "\nAhora el =\n")
  (princ (= 4 4.0)) (princ "\n") ;T
  (princ (= 20 388)) (princ "\n") ;nil
  (princ (= 2.4 2.4 2.4)) (princ "\n") ;T
  (princ (= 499 499 500)) (princ "\n") ;nil
  (princ (= "me" "me")) (princ "\n") ;T
  (princ (= "me" "you")) (princ "\n") ;nil

  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (princ "\nAhora el >\n")
  (princ (> 120 17)) (princ "\n") ;T
  (princ (> "c" "b")) (princ "\n") ;T
  (princ (> 3.5 1792)) (princ "\n") ;nil
  (princ (> 77 4 2)) (princ "\n") ;T
  (princ (> 77 4 4)) (princ "\n");nil

  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (princ "\nAhora el <\n")
  (princ (< 10 20)) (princ "\n") ;T
  (princ (< "b" "c")) (princ "\n") ;T
  (princ (< 357 33.2)) (princ "\n") ;nil
  (princ (< 2 3 88)) (princ "\n");T
  (princ (< 2 3 4 4)) (princ "\n") ;nil

  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (princ "\nEl >= y el <= funcionana de la misma manera que el mayor y el menor, salvo que consideran la igualda como T.\n")
  
  (princ "\n")

  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (princ "\nTambién hay un NO igual (/= ).\n")
  (princ (/= 10 20)) (princ "\n") ;T
  (princ (/= "you" "you")) (princ "\n") ;nil
  (princ (/= 5.43 5.44)) (princ "\n") ;T
  (princ (/= 10 20 10 20 20)) (princ "\n") ;nil
  (princ (/= 10 20 10 20)) (princ "\n") ;T
  
  (princ "\nY hay un NEGADOR (not ).\n")
  (princ (not T)) (princ "\n") ;nil
  (princ (not nil)) (princ "\n") ;T
  (princ)
  
  (princ "La que es muy útil cuando se comparan números reales es o listas es (equal ).\n")
  (setq f1 '(a b c)) 
  (setq f2 '(a b c)) 
  (setq f3 f2)
  (setq a 1.123456) 
  (setq b 1.123457)
  
  (princ (equal f1 f3)) (princ "\n") ;T
  (princ (equal f3 f2)) (princ "\n") ;T
  (princ (equal a b)) (princ "\n")  ;nil
  (princ (equal a b 0.000001)) (princ "\n") ;T
  
  (princ)
)

;----------------------------------------------------------------------------
;Función es_primo
;parámetros:
; num: Un número entero.
;
;Devuelve:
; Una lista con los divisores del número o una lista vacía si es primo.
;----------------------------------------------------------------------------
(defun es_primo(num / tanteo lst_divs)
  (setq lst_divs '())
  (setq tanteo 2)
  (while (< tanteo num)
    ;(princ tanteo) (princ "\n")
    ;(princ (eq 0 (rem num tanteo))) (princ "\n")
    (if (eq 0 (rem num tanteo))
      (setq lst_divs (append lst_divs (list tanteo))) ;El resto de la división es 0 -> es divisor.
    )
    ;(princ lst_divs) (princ "\n")
    (setq tanteo (1+ tanteo))
  )
  lst_divs
)

(defun c:programon()
  (setq num_eval (getint "Ingrese un numero para evaluar si es primo:"))
  (setq lst_devuelta (es_primo num_eval))
  ; (princ "lst_devuelta: ") (princ lst_devuelta) (princ "\n")
  ; (princ "(length lst_devuelta): ") (princ (length lst_devuelta)) (princ "\n")
  ; (princ (> (length lst_devuelta) 0)) (princ "\n")
  
  (if (> (length lst_devuelta) 0)
    (princ 
      (strcat
        "El número "
        (itoa num_eval)
        " tiene divisores y son:\n"
      )
    )
    (princ
      (strcat
        "El número "
        (itoa num_eval)
        " es primo.\n"
      )
    )
  )
  
  (foreach val lst_devuelta
    (princ (strcat "- " (itoa val) " es divisor de " (itoa num_eval) ".\n"))
  )
  (princ)
)