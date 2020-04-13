;fn_DibujaText
;***************************************************************************************************************
;*	Dibuja un Texto
;*	Par�metros:
;*		str_Texto = Valor del texto a escribir
;*		lst_Coords = Coordenadas del punto de insercion del texto
;*		num_Htxt = Altura del Texto
;*		str_Capa = Layer donde se dibujar� el texto
;*		str_Style = Estilo del texto a dibujar
;*		int_Justh = Justificaci�n horizontal
;*		int_Justv = Justificaci�n Vertical
;*		Horizontal text justification type (optional, default = 0) integer codes (not bit-coded)
;*		0 = Left; 1= Center; 2 = Right
;*		3 = Aligned (if vertical alignment = 0)
;*		4 = Middle (if vertical alignment = 0)
;*		5 = Fit (if vertical alignment = 0)
;*
;*		Vertical text justification type (optional, default = 0): integer codes (not bit-coded):
;*		0 = Baseline; 1 = Bottom; 2 = Middle; 3 = Top
;*
;*		num_RotRadian = Angulo de rotaci�n en radianes
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

  ;No nos interesar�a que devuelva un valor
)

(defun escribe_archivo (lista_completa / nombre_archivo archivo)
  ;Definimos el nombre de archivo
  (setq nombre_archivo (getfiled "Archivo a guardar." "" "csv" 1) "w")
  
  ; Verificamos que sea v�lido
  (if nombre_archivo
    (progn
      ;Creamos el archivo
      (setq archivo (open nombre_archivo "w"))
      
      ;Empezamos a escribir en el archivo linea a linea
      (foreach lst_valor lista_completa
        ; Ac� lst_valor es cada una de las listas de la lista general
      )
    )
    (progn
      ;Le informamos al usuario que no eligi� nombre archivo, entonces hacemos nada
    )
  )

  ;No nos interesar�a que devuelva un valor
)

;***************************************************************************************************************
;* Definimos la funci�n que ejecuta el programa y llama a las dem�s funciones
;*
;******************************** TODO EMPIEZA POR AC�**********************************************************
;*
;* Porque la pongo al final del archivo?
;*    Solo tiene un sentido l�gico. Esta funci�n ejecuta las anteriores, por lo tanto deben estar cargadas antes
;*    que la funci�n que las requiera.
;* Si no lo hago as�, funciona?
;*    Si, funciona igual.
;* Entonces Diego, porque carajos lo hac�s asi?
;*    Bueno, tengo costumbres pegadas de otros lenguajes con tipado fuerte donde no se pueden usar "las cosas"
;*    que no est�n definidas.
;* Que hace autocad cuando cargamos un archivo lisp con muchas funciones?
;*    Primero carga todo en memoria, entonces, al momento de ejecutar podemos estar seguros que las definiciones
;*    de funciones tambi�n est�n cargadas en memoria (y pueden ser referidas)
;* Que sentido tiene dividir nuestro programa en funciones?
;*    La idea de "fraccionar" un programa en partes funcionales m�s peque�as tiene varias justificaciones:
;*    Por ejemplo:
;*         _Nos permite reutilizar c�digo en otro programas
;*         _Es m�s f�cil encontrar errores
;* Pero, que tanto tenemos que fraccionar nuestro programa?
;*    En teor�a de la algor�tmia se definen criterios que responden esto.
;*    Un buen res�men es decir:
;*         "Cada funci�n debe resolver UN SOLO problema y en lo posible en su forma GENERAL"
;*
;*  FIN DE LA TEDIOSA INFORMACI�N. A programar...
;***************************************************************************************************************
(defun c:extrae_datos( / )
  ;1ero armamos la selecci�n con ssget
  
  ;Verificamos que la selecci�n no est� vac�a
  (if ...
    (progn
  
      ;Creamos una variable para controlar la entidad actual en la selecci�n
    
      ;Recorremos la selecci�n mientas la variable de control se encuentre dentro de la selecci�n
      (while ...
        ;Llamamos a un funci�n que nos capture los valores de la entidad y le pasamos la definici�n de la entidad
        (valores_entidad ...)
        
        ;Acumulamos los valores de cada entidad en una lista (tendr� los valores de todas las entidades)
        
        ;Siempre incrementamos la variable de control!!!
      )
    ) ; /progn SI
    (progn
      ; Le informamos al usuario que la selecci�n est� vac�a
    ) ; /progn NO
  )
  
  ; Cuando tenemos armada la lista con los valores de todas las entidades...
  
  ; Dibujamos la tabla en autocad
  ; Para eso podemos llamar a una funci�n (Si queremos que en un futuro se comporte distinto nos concentrar�amos solo en esa funci�n)
  (dibuja_tabla ...)
  
  ;Y ahora lo mismo para escribir el archivo
  (escribe archivo ...)
    
  ;Terminamos
)