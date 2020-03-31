;No es recomendable
(defun misuma()
  (+ num_a num_b) ;Notar que automáticamente retorna el último bloque de código evaluado
)

(defun c:miprograma()
  (setq num_a 4 num_b 10)
  (misuma)
)
;----------------------------------------------------------------------------------------------

;Recomendable
(defun misuma(sumando_1 sumando_1)
  (+ sumando_1 sumando_1)
)

(defun c:miprograma()
  (setq num_a 4 num_b 10)
  (misuma num_a num_b)
)
;----------------------------------------------------------------------------------------------

;un ejemplo más complejo y raro
(defun misumanegativa_v1(sumando_1 sumando_2)
  (setq s1negativo (* sumando_1 -1))
  (setq s2negativo (* sumando_2 -1))
  (+ sumando_1 sumando_2)
)

(defun c:otroprog_v1()
  ;Al inicio, por alguna razón tengo dos variables nombradas como s1negativo y s2negativo
  (setq s1negativo -16)
  (setq s2negativo -20)
  
  (setq num_a 4 num_b 10)
  ;Llamo a "misumanegativa"
  (setq lasumanegativa (misumanegativa_v1 num_a num_b))
  
  ;Veo los valores de las variables
  (princ "lasumanegativa vale: ") ;Ajá... una cosa nueva!!!
  (princ lasumanegativa)
  (princ "\n")
  
  (princ "s1negativo vale: ") (princ s1negativo) (princ "\n") ;Autolisp también interpreta los bloques de código ingresados de ésta manera
  (princ "s1negativo vale: ") (princ s2negativo) (princ "\n")
)
;----------------------------------------------------------------------------------------------

;Y para que no pase eso que hacemos?
(defun misumanegativa_v2(sumando_1 sumando_2 / s1negativo s2negativo)
  (setq s1negativo (* sumando_1 -1))
  (setq s2negativo (* sumando_2 -1))
  (+ sumando_1 sumando_1)
)

(defun c:otroprog_v2()
  (princ "Al inicio, por alguna razón tengo dos variables nombradas como s1negativo y s2negativo.\n")
  (setq s1negativo -16)
  (setq s2negativo -20)
  (princ "s1negativo vale: ") (princ s1negativo) (princ "\n") ;Autolisp también interpreta los bloques de código ingresados de ésta manera
  (princ "s2negativo vale: ") (princ s2negativo) (princ "\n")
  
  (setq num_a 4 num_b 10)
  ;Llamo a "misumanegativa"
  (setq lasumanegativa (misumanegativa_v2 num_a num_b))
  
  ;Veo los valores de las variables
  (princ "lasumanegativa vale: ")
  (princ lasumanegativa)
  (princ "\n")
  
  (princ "Al final s1negativo vale: ") (princ s1negativo) (princ "\n")
  (princ "Al final s1negativo vale: ") (princ s2negativo) (princ "\n")
)
;----------------------------------------------------------------------------------------------



