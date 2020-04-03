;Ejemplos de SI, entonces. NO, entonces.
(defun comparaciones()
  (princ "Antes de controlar los flujos de nuestro programa, tenemos que hacer comparaciones.\n")
  (princ "La primera y más básica es la de igualdad (idéntica) entre cosas. (eq)\n")
  (princ "(setq f1 '(a b c))\n")
  (setq f1 '(a b c))
  (princ "(setq f2 '(a b c))\n")
  (setq f2 '(a b c))
  (princ "(setq f3 f2)\n")
  (setq f3 f2)
  (princ "\n(eq f1 f3) -> ")
  (princ (eq f1 f3)) ;Si bien contienen los mismos valores NO SON LA MISMA LISTA
  (princ "\n(eq f3 f2) -> ")
  (princ (eq f3 f2)) ;SON LA MISMA LISTA

  (princ "\n--------------------------------------\nAhora el =\n")
  (princ "(= 4 4.0) -> ") (princ (= 4 4.0)) (princ "\n") ;T
  (princ "(= 20 388) -> ") (princ (= 20 388)) (princ "\n") ;nil
  (princ "(= 2.4 2.4 2.4) -> ")(princ (= 2.4 2.4 2.4)) (princ "\n") ;T
  (princ "(= 499 499 500) -> ")(princ (= 499 499 500)) (princ "\n") ;nil
  (princ "(= \"me\" \"me\") -> ")(princ (= "me" "me")) (princ "\n") ;T
  (princ "(= \"me\" \"you\" -> ") (princ (= "me" "you")) (princ "\n") ;nil

  (princ "\n--------------------------------------\nAhora el >\n")
  (princ "(> 120 17) -> ") (princ (> 120 17)) (princ "\n") ;T
  (princ "(> \"c\" \"b\") -> ") (princ (> "c" "b")) (princ "\n") ;T
  (princ "(> 3.5 1792) -> ") (princ (> 3.5 1792)) (princ "\n") ;nil
  (princ "(> 77 4 2) -> ") (princ (> 77 4 2)) (princ "\n") ;T
  (princ "(> 77 4 4) -> ") (princ (> 77 4 4)) (princ "\n");nil

  (princ "\n--------------------------------------\nAhora el <\n")
  (princ "(< 10 20) -> ")(princ (< 10 20)) (princ "\n") ;T
  (princ "(< \"b\" \"c\") -> ") (princ (< "b" "c")) (princ "\n") ;T
  (princ "(< 357 33.2) -> ") (princ (< 357 33.2)) (princ "\n") ;nil
  (princ "(< 2 3 88) -> ") (princ (< 2 3 88)) (princ "\n");T
  (princ "(< 2 3 4 4) -> ") (princ (< 2 3 4 4)) (princ "\n") ;nil

  (princ "\nEl >= y el <= funcionana de la misma manera que el mayor y el menor, salvo que consideran la igualda como T.\n")
  
  (princ "\n")

  (princ "\n--------------------------------------\nTambién hay un NO igual (/= ).\n")
  (princ "(/= 10 20) -> ") (princ (/= 10 20)) (princ "\n") ;T
  (princ "(/= \"you\" \"you\") -> ") (princ (/= "you" "you")) (princ "\n") ;nil
  (princ "(/= 5.43 5.44) -> ") (princ (/= 5.43 5.44)) (princ "\n") ;T
  (princ "(/= 10 20 10 20 20) -> ") (princ (/= 10 20 10 20 20)) (princ "\n") ;nil
  (princ "(/= 10 20 10 20) -> ") (princ (/= 10 20 10 20)) (princ "\n") ;T
  
  (princ "\n--------------------------------------\nLa que es muy útil cuando se comparan números reales o listas es (equal ).\n")
  (princ "(setq f1 '(a b c))\n")
  (setq f1 '(a b c))
  (princ "(setq f2 '(a b c))\n")
  (setq f2 '(a b c)) 
  (princ "(setq f3 f2)\n")
  (setq f3 f2)
  (princ "(setq a 1.123456)\n")
  (setq a 1.123456)
  (princ "(setq b 1.123457)\n")
  (setq b 1.123457)
  
  (princ "(equal f1 f3) -> ") (princ (equal f1 f3)) (princ "\n") ;T
  (princ "(equal f3 f2) -> ") (princ (equal f3 f2)) (princ "\n") ;T
  (princ "(equal a b) -> ") (princ (equal a b)) (princ "\n")  ;nil
  (princ "(equal a b 0.000001) -> ") (princ (equal a b 0.000001)) (princ "\n") ;T
  
  (princ "\nEs muy útil cuando se comparan puntos (listas), por que lo hace de una.\n")
  
  (princ "(equal '(3.1415 2.34562 25.98) '(3.1415 2.34562 25.98)) -> ")
  (princ (equal '(3.1415 2.34562 25.98) '(3.1415 2.34562 25.98)))
  (princ "\nPeeeeeeero:\n")
  (princ "(equal '(3.1415 2.34562 25.98) '(3.1413 2.341 25.98054) 0.001) -> ")
  (princ (equal '(3.1415 2.34562 25.98) '(3.1413 2.345 25.98054) 0.001))
  (princ)
)
