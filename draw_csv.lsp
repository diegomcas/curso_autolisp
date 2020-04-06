(defun is_integer(str)
	(= (itoa (atoi str)) str)
)

(defun is_real(str / )
	(= (rtos (atof str)) str)
)

(defun is_number(str / )
	(or (is_integer str) (is_real str))
)

;str_to_list
;***************************************************************************************************************
;*	Recibe una cadena de texto separada por el caracter str_Separador y devuelve una lista con cada elemento de la cadena
;*	Parámetros:
;*		str_cadenaCD = La cadena con los registros separados por str_Separador
;*		str_Separador = La cadena con el caracter que se utilizará como separador
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun str_to_list (str_CadenaCD str_Separador / lst_ListaStr int_Pos1 int_Pos2 val lst_ret)
	(if (/= str_Separador (substr str_CadenaCD 1 (strlen str_Separador)))
		(setq str_CadenaCD (strcat str_Separador str_CadenaCD)))

	(while (= str_Separador (substr str_CadenaCD (1+ (- (strlen str_CadenaCD) (strlen str_Separador)))))
		(setq str_CadenaCD (substr str_CadenaCD 1 (- (strlen str_CadenaCD) (strlen str_Separador)))))

	(while (setq int_Pos1 (vl-string-search str_Separador str_CadenaCD))
		(setq int_Pos1 (+ 1 int_Pos1 (strlen str_Separador)))
		(setq int_Pos2 (vl-string-search str_Separador str_CadenaCD int_Pos1))
		(if int_Pos2
			(setq lst_ListaStr (append lst_ListaStr (list (substr str_CadenaCD int_Pos1 (1+ (- int_Pos2 int_Pos1))))))
			(setq lst_ListaStr (append lst_ListaStr (list (substr str_CadenaCD int_Pos1)))))
		(setq str_CadenaCD (substr str_CadenaCD int_Pos1))
	)
  
  ;(prin1 lst_ListaStr) (princ "\n")
  
  (setq lst_ret '())
  (foreach val lst_ListaStr
    (if (is_integer val)
      (setq val (atoi val))
      (if (is_real val)
        (setq val (atof val))
			)
    )
    (setq lst_ret (append lst_ret (list val)))
  )
  
  lst_ret
)

(defun dibuja_punto (lst / x y z str_nro str_desc pto_ins)
  ;Recordemos... Nuestra lista debe ser así (nro_pto x y z descripción)
  (princ lst) (princ "\n")
  (princ (type (nth 4 lst)))
  
  ;pongamos las cosas en variables para que sea más entendible nuestro código
  (setq str_nro (car lst))
  (setq x (cadr lst))
  (setq y (caddr lst))
  (setq z (cadddr lst))
  (setq str_desc (nth 4 lst))
  
  ;Hagamos algunas verificaciones antes de dibujarlos
  ;x y z -> deben ser números reales
  (if (and 
        (or (= (type x) 'REAL) (= (type x) 'INT))
        (or (= (type y) 'REAL) (= (type y) 'INT))
        (or (= (type z) 'REAL)(= (type z) 'INT))
      )
    (progn
      ;Como al punto lo usaremos también como punto de inserción de los textos
      ;Creamos una variable con el punto por comodidad  
      (setq pto_ins (list x y z))
      
      ;Dibujamos el punto
      (command "_-layer" "s" "PUNTOS" "")
      (command "_point" pto_ins)
      
      ;Dibujamos el texto con el nro de punto
      ;vemos si str_nro no tiene un valor numérico
      (if (= (type str_nro) 'INT)
        (setq str_nro (itoa str_nro))
      )
      
      (if (= (type str_nro) 'REAL)
        ;Lo redondeamos y lo pasamos a string
        (setq str_nro (itoa (fix str_nro)))
      )
      (command "_-layer" "s" "NRO_PTO" "")
      (command "_text" pto_ins "2" "45d" str_nro)
      
      ;Dibujamos el texto con la cota del punto en decimal con dos dígitos 
      (command "_-layer" "s" "COTAS" "")
      (command "_text" pto_ins "2" "0" (rtos z 2 2))
      
      ;Dibujamos la descripción
      (command "_-layer" "s" "DESCRIPCION" "")
      (command "_text" pto_ins "2" "315" str_desc)
    )
    (progn
      ;Le avisamos al usuario que un valor de la coordenada no es válido (pero igual seguimos ejecutando)
      (princ "Los valores de coordenadas no son válidos: \n")
      (princ "  x = ") (princ x) (princ "\n")
      (princ "  y = ") (princ y) (princ "\n")
      (princ "  z = ") (princ z) (princ "\n")
    )
  )
  
  ;Como no necesitamos retornar un valor, limpiamos la salida para que no imprima "mugre"
  (princ)
)

(defun c:draw_csv( / nombre_archivo file_csv lst_str cnt)
  ;Configuramos algunas cosas del archivo
  
  ;Cambiamos la variable de autocad CMDECHO para que no sea tan verborrágico cuando dibujamos con command
  (setvar "CMDECHO" 0)
  
  ;Creamos unas capas (Layers)
  (command "_-layer" "M" "PUNTOS" "M" "COTAS" "M" "NRO_PTO" "M" "DESCRIPCION" "")

  ;Seleccionamos el nombre de archivo
  (setq nombre_archivo (getfiled "Selccione un archivo csv." "" "csv" 0))

  ;Verificamos que se elegió un archivo
  (if (not nombre_archivo)
    (progn
      (princ "No se seleccionó ningún nombre de archivo.\nEl programa se finaliza.\n")
      (exit)
    )
  )
  
  ;Abrimos el archivo
  (setq file_csv (open nombre_archivo "r"))
  
  ;Verificamos que se abrió correctamente el archivo
  (if (not file_csv)
    (progn
      (princ (strcat "Hubo un problema al abrir el archivo '" nombre_archivo "'.\nEl programa se finaliza.\n"))
      (exit)
    )
  )
  
  ;Inicializamos cnt en 0
  (setq cnt 0)
  
  ;Leemos el archivo linea a linea
  (while (setq str_line (read-line file_csv))
    ;Llamamos a un función que nos convierta la linea de texto a valores útiles
    (setq lst_str (str_to_list str_line ";"))
    
    ;llamamos a una función que nos dibuje el punto y los textos
    (dibuja_punto lst_str)
    
    ;Incrementamos cnt en 1
    (setq cnt (1+ cnt)) ;Acá no lo usamos para controlar el bucle, es solo para informarle al usuario al final
  )
  
  ;Cerramos el archivo (de otra manera queda bloqueado y no podremos editarlo hasta que cerremos autocad.)
  (close file_csv)
  
  ;Le decimos al usuario cuantos puntos dibujamos
  (princ (strcat "Se dibujaron " (itoa cnt) " puntos.\n"))
  
  ;Es lo mismo escribirlo en partes
  ;(princ "Se dibujaron ")
  ;(princ cnt) ;Notar que princ escribe números. El problema de convertirlos a string aparece cuando los concatenamos.
  ;(princ " puntos.\n")
  
  ;Hacemos un zoom extend
  (command "_zoom" "e")
  
  ;Limpiamos la salida por prolijidad
  (princ)
)