;********************************************************************************
;* Función is_integer
;*
;* Parámetros:
;*            - str: Una cadena a evaluar si representa un número entero
;*
;* Retorna:
;*         -T si la cadena representa un número entero, nil en caso contrario
;********************************************************************************
(defun is_integer(str)
	(= (itoa (atoi str)) str)
)

;********************************************************************************
;* Función is_real
;*
;* Parámetros:
;*            - str: Una cadena a evaluar si representa un número real
;*
;* Retorna:
;*         - T si la cadena representa un número real,
;*         - nil en caso contrario
;********************************************************************************
(defun is_real(str / )
	(and (= (rtos (atof str)) str) (wcmatch str "*`.*"))
)

;********************************************************************************
;* Función line_csv_to_list
;*
;* Parámetros:
;*            - str: Una cadena con valores definidos y separados según "sep"
;*            - sep: Una cadena de longitud 1 con el caracter que se separan los valores
;*
;* Retorna:
;*         - Una lista con cada uno de los valores contenidos en str.
;*
;* Ejemplo:
;*         str = "1; Hola mundo;3.1415   ;   34.67"
;*         lst = (1 "Hola mundo" 3.1415 34.67)
;*
;* Verificar que los strings no contengan "sep"
;********************************************************************************
(defun line_csv_to_list (str sep / pos lst len_sep len_str act_char act_val)
  (if (= (strlen sep) 1)
    (progn
			;Verificamos que la linea termine con un separador
	    (if (/= (substr str (strlen str)) sep)
		    (setq str (strcat str sep))
	    ) ; /if

      (setq lst '())
      (setq pos 1)
      (setq len_str (strlen str))
      (setq act_val "")

			;Recorro caracter a caracter
      (while (<= pos len_str)
				;Corto el caracter actual
        (setq act_char (substr str pos 1))

        (if (= act_char sep)
          (progn ;Si es separador...
            (setq act_val (vl-string-trim " " act_val)) ;Limpio espacios al inicio y al final
            (cond ; Opero según tipo de valor
              ((is_integer act_val)
                (setq act_val (atoi act_val))
              )
              ((is_real act_val)
                (setq act_val (atof act_val))
              )
            ) ; /cond

						;Agrego el valor a la lista
            (setq lst (append lst (list act_val)))

						;Vacío act_val para evitar seguir acumulando caracteres en el valor próximo
            (setq act_val "")
          ) ; /progn si
          (progn
						; Concateno el caracter actual a la cadena con el valor actual
            (setq act_val (strcat act_val act_char))
          ) ; /progn no
        ) ; /if
        
        (setq pos (1+ pos)) ;Incremento el contador de posición
      ) ; /while
    ) ; /progn
  ) ; /if
  
  lst
)

;********************************************************************************
;* Función read_csv
;*
;* Parámetros:
;*            - nom_file: Una cadena con el nombre de archivo a procesar
;*            - sep: Una cadena de longitud 1 con el caracter que se separan los valores
;*
;* Retorna:
;*         - Una lista de listas con cada una de las lineas contenidas en "nom_file:"
;*
;* Ejemplo:
;*         El archivo tiene un contenido similar a:
;*                1; Hola mundo;3.1415   ;   34.67
;*                2;   Chau mundo ;  5.5633;   73.54
;*
;*         lst_csv = (
;*                     (1 "Hola mundo" 3.1415 34.67)
;*                     (2 "Chau mundo" 5.5633 73.54)
;*                   )
;*
;* Verificar que los strings no contengan "sep"
;********************************************************************************
(defun read_csv (nom_file sep / file_desc str_linea lst_csv)
	;Abro el archivo
  (setq file_desc (open nom_file "r"))

	;inicializo un lista vacía para ir agregando cada lista con una linea del csv
	(setq lst_csv '())

	;Recorro el archivo linea a linea hasta el final del archivo
  (while (setq str_linea (read-line file_desc))
		;Agrego la lista procesada de la linea del archvio a una lista con todos los puntos
		(setq lst_csv (append lst_csv (list (line_csv_to_list str_linea sep))))
  )

	;Cierro el archivo
  (close file_desc)

  lst_csv
)

(defun dibuja_punto (lst / x y z str_nro str_desc pto_ins)
  ;Recordemos... Nuestra lista debe ser así (nro_pto x y z descripción)
	
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
      (command "_text" pto_ins "2" "45" str_nro)
      
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


(defun c:draw_csv( / nombre_archivo lista_csv registro)
	;1ero. Configuramos algunas cosas del archivo
  
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

	;Llamamos a la función read_csv que lee todo el archivo de texto y nos retorna una lista con los valores
	(setq lista_csv (read_csv nombre_archivo ";"))

	;Teniendo el contenido de TODO el archivo como una lista de listas lo dibujamos
	(foreach registro lista_csv
		(dibuja_punto registro)
	)

	;Hacemos un zoom extend
	(command "_zoom" "e")

	;Como no necesitamos retornar un valor, limpiamos la salida para que no imprima "mugre"
	(princ)
)