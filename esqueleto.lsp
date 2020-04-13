;fn_DibujaText
;***************************************************************************************************************
;*	Dibuja un Texto
;*	Parámetros:
;*		str_Texto = Valor del texto a escribir
;*		lst_Coords = Coordenadas del punto de insercion del texto
;*		num_Htxt = Altura del Texto
;*		str_Capa = Layer donde se dibujará el texto
;*		str_Style = Estilo del texto a dibujar
;*		int_Justh = Justificación horizontal
;*		int_Justv = Justificación Vertical
;*		Horizontal text justification type (optional, default = 0) integer codes (not bit-coded)
;*		0 = Left; 1= Center; 2 = Right
;*		3 = Aligned (if vertical alignment = 0)
;*		4 = Middle (if vertical alignment = 0)
;*		5 = Fit (if vertical alignment = 0)
;*
;*		Vertical text justification type (optional, default = 0): integer codes (not bit-coded):
;*		0 = Baseline; 1 = Bottom; 2 = Middle; 3 = Top
;*
;*		num_RotRadian = Angulo de rotación en radianes
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DibujaText(str_Texto lst_Coords num_Htxt str_Capa str_Style int_Justh int_Justv num_RotRadian / lst_ent)
	(setq lst_ent
		(list
			'(0 . "TEXT")
			'(100 . "AcDbEntity")
			(cons 8 str_Capa)
			'(100 . "AcDbText")
			(cons 10 lst_Coords)
			(cons 40 num_Htxt)
			(cons 1 str_Texto)
			(cons 50 num_RotRadian)
			'(41 . 1.0)
			'(51 . 0.0)
			(cons 7 str_Style)
			'(71 . 0)
			(cons 72 int_Justh)
			(cons 11 lst_Coords)
			'(100 . "AcDbText")
			(cons 73 int_Justv)
		)
	)
	(entmake lst_ent)
	(princ)
)

(defun valores_entidad (ent_def / lista_resultado)
  ;Buscamos los valores que nos interesan y los enlistamos
  
  ;Necesitamos coordenadas
  
  ;Necesitamos el valor del texto
  
  lista_resutltado ;Devolvemos la lista resultante
)

(defun dibuja_tabla (lista completa / )

  ;No nos interesaría que devuelva un valor
)

(defun escribe_archivo (lista_completa / nombre_archivo archivo)
  ;Definimos el nombre de archivo
  (setq nombre_archivo (getfiled "Archivo a guardar." "" "csv" 1) "w")
  
  ; Verificamos que sea válido
  (if nombre_archivo
    (progn
      ;Creamos el archivo
      (setq archivo (open nombre_archivo "w"))
      
      ;Empezamos a escribir en el archivo linea a linea
      (foreach lst_valor lista_completa
        ; Acá lst_valor es cada una de las listas de la lista general
      )
    )
    (progn
      ;Le informamos al usuario que no eligió nombre archivo, entonces hacemos nada
    )
  )

  ;No nos interesaría que devuelva un valor
)

;***************************************************************************************************************
;* Definimos la función que ejecuta el programa y llama a las demás funciones
;*
;******************************** TODO EMPIEZA POR ACÁ**********************************************************
;*
;* Porque la pongo al final del archivo?
;*    Solo tiene un sentido lógico. Esta función ejecuta las anteriores, por lo tanto deben estar cargadas antes
;*    que la función que las requiera.
;* Si no lo hago así, funciona?
;*    Si, funciona igual.
;* Entonces Diego, porque carajos lo hacés asi?
;*    Bueno, tengo costumbres pegadas de otros lenguajes con tipado fuerte donde no se pueden usar "las cosas"
;*    que no están definidas.
;* Que hace autocad cuando cargamos un archivo lisp con muchas funciones?
;*    Primero carga todo en memoria, entonces, al momento de ejecutar podemos estar seguros que las definiciones
;*    de funciones también están cargadas en memoria (y pueden ser referidas)
;* Que sentido tiene dividir nuestro programa en funciones?
;*    La idea de "fraccionar" un programa en partes funcionales más pequeñas tiene varias justificaciones:
;*    Por ejemplo:
;*         _Nos permite reutilizar código en otro programas
;*         _Es más fácil encontrar errores
;* Pero, que tanto tenemos que fraccionar nuestro programa?
;*    En teoría de la algorítmia se definen criterios que responden esto.
;*    Un buen resúmen es decir:
;*         "Cada función debe resolver UN SOLO problema y en lo posible en su forma GENERAL"
;*
;*  FIN DE LA TEDIOSA INFORMACIÓN. A programar...
;***************************************************************************************************************
(defun c:extrae_datos( / )
  ;1ero armamos la selección con ssget
  
  ;Verificamos que la selección no esté vacía
  (if ...
    (progn
  
      ;Creamos una variable para controlar la entidad actual en la selección
    
      ;Recorremos la selección mientas la variable de control se encuentre dentro de la selección
      (while ...
        ;Llamamos a un función que nos capture los valores de la entidad y le pasamos la definición de la entidad
        (valores_entidad ...)
        
        ;Acumulamos los valores de cada entidad en una lista (tendrá los valores de todas las entidades)
        
        ;Siempre incrementamos la variable de control!!!
      )
    ) ; /progn SI
    (progn
      ; Le informamos al usuario que la selección está vacía
    ) ; /progn NO
  )
  
  ; Cuando tenemos armada la lista con los valores de todas las entidades...
  
  ; Dibujamos la tabla en autocad
  ; Para eso podemos llamar a una función (Si queremos que en un futuro se comporte distinto nos concentraríamos solo en esa función)
  (dibuja_tabla ...)
  
  ;Y ahora lo mismo para escribir el archivo
  (escribe archivo ...)
    
  ;Terminamos
)