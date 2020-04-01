(defun c:mi_rect (/)
	;Dibujamos nuevamente un rectángulo, pero ahora con valores establecidos por el usuario

	;Pedimos al usuario el punto de inicio del rectángulo y lo asignamos en la variable "pt_inicio"
	(setq pt_inicio (getpoint "Ingrese el punto de Inicio del rectángulo: "))
	(princ "\n") ;Salto de línea

	;Le solicitamos al usuario que ingrese el valor del ancho del rectángulo
	;  y lo almacenamos en la variable "ancho"
	(setq ancho (getreal "Ingrese el ancho del rectángulo: "))
	(princ "\n") ;Salto de línea

	;Le solicitamos al usuario que ingrese el valor del ancho del rectángulo
	;  y lo almacenamos en la variable "ancho"
	(setq alto (getreal "Ingrese el alto del rectángulo: "))
	(princ "\n") ;Salto de línea

	;Con los valores obtenidos nos ponemos manos a la obra

	;Para dibujar elementos en autocad SIEMPRE (Bue, casi siempre) necesitamos coordenadas

	;Calculamos las coordenadas del siguiente punto, al que llamaremos "pt_2"
	;Para que sea más entendible, vamos a hacerlo paso a paso

	;Claculamos el x del pt_2
	(setq x (+ (car pt_inicio) ancho))
	(setq y (cadr pt_inicio))

	;Ahora si, armamos la lista con los datos del "pt_2"
	(setq pt_2 (list x y))

	;Vamos entonces por "pt_3"
	;Como a los valores que tienen las variables x e y no los necesitamos, las volvemos a usar
	;El valor de x del punto 3 es igual que para el punto 2, entonces no lo modificamos
	(setq y (+ alto (cadr pt_inicio)))

	;Armamos pt_2
	(setq pt_3 (list x y))

	;Vamos con el punto 4 (pt_4)
	;Analisemos el caso:
	;	La coordenada x del punto 4 es igual que la coordenada x del punto 1
	; y la coordena y del punto 4 es igual a la coordenada y del punto 3 (que queda igual)
	;Entonces resolvamos eso
	(setq x (car pt_inicio))

	;y aramamos el punto
	(setq pt_4 (list x y))
	
	;Podríamos hacer todo de una así -> (setq pt_4 (list (car pt_1) (cadr pt_3)))
	;pero es más complicado de ver :-)

	;Con todos los datos empezamos a dibujar el rectángulo

	;Hacemos las lineas desde pt_inicio a pt_2 a pt_3 a pt_4 y cerramos
	(command "_line" pt_inicio pt_2 pt_3 pt_4 "c")

	;Si llegamos hasta acá es que NO "pasaron cosas"
	;Por cortesía le contamos al usuario que hicimos

	;Para eso vamos a convertir los valores que tenemos como números a strings
	(setq str_x_ini (rtos (car pt_inicio) 2 4))
	(setq str_y_ini (rtos (cadr pt_inicio) 2 4))

	(setq str_ancho (rtos ancho 2 4))
	(setq str_alto (rtos alto 2 4))

	;Entonces se lo informamos...
	(princ
		(strcat
			"Dibujamos el rectángulo que inicia en x= "
			str_x_ini
			"; en y= "
			str_y_ini
			" y que tiene un ancho= "
			str_ancho
			" y una altura= "
			str_alto
		)
	)

	;Imprimimos en pantalla un salto de linea...
	(princ "\n")
	
	;y le avisamos al usuario que finalizamos el programa
	(princ "El programa finalizó correctamente.")

	;escribimos en la salida "nada" para que quede limpio el mensaje
	(princ)

	;Y TERMINAOS!!!
)