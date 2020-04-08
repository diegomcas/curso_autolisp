(defun fn_DeclaraBiblioteca ()
	(eval '"1.00")
)

(defun fn_LstPosition (atm_Elemento lst_Lista / int_Cnt int_Pos lst_Elemento)
	(setq int_Cnt 0)
	(foreach lst_Elemento lst_Lista
		(if (equal lst_Elemento atm_Elemento 0.001)
			(setq int_Pos int_Cnt)
		)
		(setq int_Cnt (1+ int_Cnt))
	)
	(eval 'int_Pos)
)

(defun fn_Member (atm_Elemento lst_Lista / bool_Bandera lst_Elemento lst_Return)
	(setq bool_Bandera nil)
	(setq lst_Return nil)
	(foreach lst_Elemento lst_Lista
		(if (equal lst_Elemento atm_Elemento 0.001)
			(setq bool_Bandera T)
		)
		(if bool_Bandera
			(setq lst_Return (append lst_Return (list lst_Elemento)))
		)
	)
	(eval 'lst_Return)
)

;fn_CadCDToLista
;***************************************************************************************************************
;*	Recibe una cadena de texto separada por el caracter str_Separador y devuelve una lista con cada elemento de la cadena
;*	Parámetros:
;*		str_cadenaCD = La cadena con los registros separados por str_Separador
;*		str_Separador = La cadena con el caracter que se utilizará como separador
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_CadCDToLista(str_CadenaCD str_Separador / lst_ListaStr int_Pos1 int_Pos2)
;	Fuerza a que la cadena termine en str_Separador
	(if (/= (substr str_CadenaCD (strlen str_CadenaCD) 1) str_Separador)
		(setq str_CadenaCD (strcat str_CadenaCD str_Separador))
	)

	(setq int_Pos1 -1)
	(setq int_Pos2 (vl-string-position (ascii str_Separador) str_CadenaCD))

;	Se repite hasta que int_Pos2 sea nil
	(while int_Pos2
		(if (= int_Pos2 (1+ int_Pos1))
			(setq lst_ListaStr (append lst_ListaStr (list nil)))
			(setq lst_ListaStr (append lst_ListaStr (list (substr str_CadenaCD (1+ (1+ int_Pos1)) (- int_Pos2 int_Pos1 1)))))
		)
		(setq int_Pos1 int_Pos2)
		(if (<= (1+ int_Pos1) (strlen str_CadenaCD))
			(progn
				(setq int_Pos2 (vl-string-position (ascii str_Separador) str_CadenaCD (1+ int_Pos1)))
			)
			(progn
				(setq int_Pos2 nil)
			)
		)
	)
	(eval 'lst_ListaStr)
)

;fn_CadCDToLista1
;***************************************************************************************************************
;*	Recibe una cadena de texto separada por el caracter str_Separador y devuelve una lista con cada elemento de la cadena
;*	Parámetros:
;*		str_cadenaCD = La cadena con los registros separados por str_Separador
;*		str_Separador = La cadena con el caracter que se utilizará como separador
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_CadCDToLista1 (str_CadenaCD str_Separador / lst_ListaStr int_Pos1 int_Pos2)
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
	(eval 'lst_ListaStr)
)

;fn_VerticeCercano
;***************************************************************************************************************
;*	Retorna una lista con el vertice mas cercano al evento (Numero_Vertice Distancia)
;*	Parámetros:
;*	lst_Vertices = Lista de puntos de los vertices
;*	lst_PuntoElemento = Lista con las coordenadas del punto
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_VerticeCercano (lst_Vertices lst_PuntoElemento / int_Cont lst_DistanciaMinima num_Distancia)
	(setq int_Cont 0)
	(setq lst_DistanciaMinima nil)
	(while (< int_Cont (length lst_Vertices))
		(setq num_Distancia (distance lst_PuntoElemento (nth int_Cont lst_Vertices)))
		(if (= lst_DistanciaMinima nil)
			(setq lst_DistanciaMinima (list int_Cont num_Distancia))
		)
		(if (< num_Distancia (cadr lst_DistanciaMinima))
				(setq lst_DistanciaMinima (list int_Cont num_Distancia))
		)
		(setq int_Cont (1+ int_Cont))
	)
	(eval 'lst_DistanciaMinima)
)

;fn_3DTo2DPoint
;***************************************************************************************************************
;*	Convierte un punto 3d en 2d:
;*	Parámetros:
;*	lst_Punto = Coordenadas del punto
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_3DTo2DPoint (lst_Punto / lst_Return)
	(setq lst_Return (list (car lst_Punto) (cadr lst_Punto)))
	(eval 'lst_Return)
)

;fn_CoordenadaToSegmento
;***************************************************************************************************************
;*	Calcula la proyección de un punto sobre un segmento:
;*	Parámetros:
;*	pt_Segmento1 = Vertice 1 del segmento
;*	pt_Segmento2 = Vertice 2 del segmento
;*	lst_PuntoElemento = Lista con las coordenadas del punto
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_CoordenadaToSegmento(pt_1 pt_2 pt_Elemento / num_AnguloEvaluar num_Distancia pt_Proyectado1 pt_Proyectado2 pt_Resultado)
; Convierto las coordenadas de los puntos en 2D
	(setq pt_1 (fn_3DTo2DPoint pt_1))
	(setq pt_2 (fn_3DTo2DPoint pt_2))
	(setq pt_Elemento (fn_3DTo2DPoint pt_Elemento))

;	Usaré la distancia mayor a un vertice para calcular la intersección, asi me aseguro que lo verifica si o si.
	(if (> (distance pt_Elemento pt_1) (distance pt_Elemento pt_2))
		(setq num_Distancia (distance pt_Elemento pt_1))
		(setq num_Distancia (distance pt_Elemento pt_2))
	)

;	Calculo el ángulo perpendicular al al formado por el segmento original
	(setq num_AnguloEvaluar (+ (angle pt_1 pt_2) (* pi 0.5)))
;	Calculo el punto incógnita que formará el segmento con pt_Elemento con el que verificaré la intersección
	(setq pt_Proyectado1 (polar pt_Elemento num_AnguloEvaluar num_Distancia))

;	Calculo el ángulo perpendicular al al formado por el segmento original en el otro cuadrante
	(setq num_AnguloEvaluar (- (angle pt_1 pt_2) (* pi 0.5)))
;	Calculo el punto incógnita que formará el segmento con pt_Elemento con el que verificaré la intersección
	(setq pt_Proyectado2 (polar pt_Elemento num_AnguloEvaluar num_Distancia))

;	Calculo la intersección de los segmentos
	(setq pt_Resultado (inters pt_1 pt_2 pt_Proyectado1 pt_Proyectado2))

	(eval 'pt_Resultado)
)

;fn_ProgresivaPunto
;***************************************************************************************************************
;*	Calcula la progresiva de un punto sobre una traza
;*	Parámetros:
;*	lst_Vertices = Lista de puntos de los vertices
;*	lst_PuntoElemento = Lista con las coordenadas del punto
;*	int_Posicion = Posición del vertice anterior al elemento
;*
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_ProgresivaPunto (lst_Vertices lst_PuntoElemento int_Posicion / num_ProgAcum int_Cont)
	(setq int_Cont 0)
	(setq num_ProgAcum 0.0)
	(while (< int_Cont int_Posicion)
		(setq num_ProgAcum (+ num_ProgAcum (distance (nth int_Cont lst_Vertices) (nth (1+ int_Cont) lst_Vertices))))
		(setq int_Cont (1+ int_Cont))
	)
	(setq num_ProgAcum (+ num_ProgAcum (distance (nth int_Posicion lst_Vertices) lst_PuntoElemento)))
	(eval 'num_ProgAcum)
)

;fn_InterpolaCotas
;***************************************************************************************************************
;*	Interpola la cota para una progresiva
;*	Parámetros:
;*	lst_TablaCotas = Tabla con los datos de las cotas
;* 	num_Progresiva = Progresiva a calcula la cota
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_InterpolaCotas (lst_TablaCotas num_Progresiva / int_Indice num_Cota num_CotaAnt num_CotaPos num_ProgAnt num_ProgPos)
	(setq int_Indice (fn_BuscaProgresiva lst_TablaCotas num_Progresiva))
	(if (= int_Indice (1- (length lst_TablaCotas)))
		(progn
			(setq num_Cota (cdr (assoc "COTA" (nth int_Indice lst_TablaCotas)))))
		(progn
			(setq num_CotaAnt (cdr (assoc "COTA" (nth int_Indice lst_TablaCotas))))
			(setq num_CotaPos (cdr (assoc "COTA" (nth (1+ int_Indice) lst_TablaCotas))))
			(setq num_ProgAnt (cdr (assoc "PROGRESIVA" (nth int_Indice lst_TablaCotas))))
			(setq num_ProgPos (cdr (assoc "PROGRESIVA" (nth (1+ int_Indice) lst_TablaCotas))))

			(setq num_Cota
				(+ num_CotaAnt
					(/
						(* (- num_CotaPos num_CotaAnt) (- num_Progresiva num_ProgAnt))
						(- num_ProgPos num_ProgAnt))))))
	
	(eval 'num_Cota)
)

;fn_BuscaProgresiva
;***************************************************************************************************************
;*	Devuelve el indice anterior encontrado en una tabla cualquiera
;*	Parámetros:
;*	lst_Tabla = Tabla con los datos donde buscar Progresiva
;* 	num_Progresiva = Progresiva a buscar
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_BuscaProgresiva (lst_Tabla num_Progresiva / int_Cont)
	(setq int_Cont 0)
	(while (and (<= (cdr (assoc "PROGRESIVA" (nth int_Cont lst_Tabla))) num_Progresiva) (< int_Cont (length lst_Tabla)))
		(setq int_Cont (1+ int_Cont))
	)
	
	(if (/= int_Cont 0)
		(setq int_Cont (1- int_Cont)))
	(eval int_Cont)
)

;fn_CoordenadasToTraza
;***************************************************************************************************************
;*	Calcula punto sobre la traza y la distancia de un elemento puntual en una sucesión de vértices (UbicacionVerticeNuevo CoordenadasVerticeNuevo DistanciaElementoTraza ProgresivaElemento):
;*	Parámetros:
;*	lst_Vertices = Lista de puntos de los vertices
;*	lst_PuntoElemento = Lista con las coordenadas del punto
;*
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_CoordenadasToTraza(lst_Vertices lst_PuntoElemento / int_IndiceVerticeCercano pt_ProyectadoAnt pt_ProyectadoPos lst_Resultados)
;	Busco el vértice mas cercano al evento
	(setq int_IndiceVerticeCercano (car (fn_VerticeCercano lst_Vertices lst_PuntoElemento)))

;	Si no es primer vértice Verifico con vertice anterior
	(if (> int_IndiceVerticeCercano 0)
		(setq pt_ProyectadoAnt (fn_CoordenadaToSegmento (nth (1- int_IndiceVerticeCercano) lst_Vertices) (nth int_IndiceVerticeCercano lst_Vertices) lst_PuntoElemento))
	)
;	Si no es el último vértice Verifico con vertice posterior
	(if (< int_IndiceVerticeCercano (1- (length lst_Vertices)))
		(setq pt_ProyectadoPos (fn_CoordenadaToSegmento (nth int_IndiceVerticeCercano lst_Vertices) (nth (1+ int_IndiceVerticeCercano) lst_Vertices) lst_PuntoElemento))
	)

	(cond
		((and (= pt_ProyectadoAnt nil) (= pt_ProyectadoPos nil)) ;Está por fuera de la curva del caño, adopta la progresiva del vertice y la distancia al vértice
;			(princ "Está por fuera de la curva del caño, adopta la progresiva del vertice y la distancia al vértice")
;			(princ "\n")
			(setq lst_PuntoProyectado (nth int_IndiceVerticeCercano lst_Vertices))
			(setq lst_Resultados
				(list
					(cons "INDICE" int_IndiceVerticeCercano)
					(cons "PTPROY" lst_PuntoProyectado)
					(cons "DISTANCIA" (distance lst_PuntoElemento lst_PuntoProyectado))
					(cons "PROGRESIVA" (fn_ProgresivaPunto lst_Vertices lst_PuntoProyectado int_IndiceVerticeCercano))
				)
			)
;			(fn_DibujaPoint lst_PuntoProyectado "D")
		)
		((and (/= pt_ProyectadoAnt nil) (/= pt_ProyectadoPos nil)) ;Se verifica por los dos lados, elijo el mas cercano
;			(princ "Se verifica por los dos lados, elijo el mas cercano")
;			(princ "\n")
			(if (<= (distance pt_ProyectadoAnt lst_PuntoElemento) (distance pt_ProyectadoPos lst_PuntoElemento))
				(setq lst_Resultados
					(list
						(cons "INDICE" (1- int_IndiceVerticeCercano))
						(cons "PTPROY" pt_ProyectadoAnt)
						(cons "DISTANCIA" (distance pt_ProyectadoAnt lst_PuntoElemento))
						(cons "PROGRESIVA" (fn_ProgresivaPunto lst_Vertices pt_ProyectadoAnt (1- int_IndiceVerticeCercano)))
					)
				)
				(setq lst_Resultados
					(list
						(cons "INDICE" int_IndiceVerticeCercano)
						(cons "PTPROY" pt_ProyectadoPos)
						(cons "DISTANCIA" (distance pt_ProyectadoPos lst_PuntoElemento))
						(cons "PROGRESIVA" (fn_ProgresivaPunto lst_Vertices pt_ProyectadoPos int_IndiceVerticeCercano))
					)
				)
			)
		)
		((/= pt_ProyectadoAnt nil) ;Se verifica con el vertice anterior
;			(princ "Se verifica con el vertice anterior")
;			(princ "\n")
			(setq lst_Resultados
				(list
					(cons "INDICE" (1- int_IndiceVerticeCercano))
					(cons "PTPROY" pt_ProyectadoAnt)
					(cons "DISTANCIA" (distance pt_ProyectadoAnt lst_PuntoElemento))
					(cons "PROGRESIVA" (fn_ProgresivaPunto lst_Vertices pt_ProyectadoAnt (1- int_IndiceVerticeCercano)))
				)
			)
		)
		((/= pt_ProyectadoPos nil) ;Se verifica con el vertice posterior
;			(princ "Se verifica con el vertice posterior")
;			(princ "\n")
			(setq lst_Resultados
				(list
					(cons "INDICE" int_IndiceVerticeCercano)
					(cons "PTPROY" pt_ProyectadoPos)
					(cons "DISTANCIA" (distance pt_ProyectadoPos lst_PuntoElemento))
					(cons "PROGRESIVA" (fn_ProgresivaPunto lst_Vertices pt_ProyectadoPos int_IndiceVerticeCercano))
				)
			)
		)
	)
;	(fn_DibujaPoint (cadr lst_Resultados) "PuntoResultado")
	(eval 'lst_Resultados)
)

;fn_ProgresivaToCoord
;***************************************************************************************************************
;*	Tomando una progresiva y una lista de vertices retorna la coordenada
;*	Parámetros:
;*	lst_Traza = Lista de puntos de los vertices
;*	num_Progresiva = La Progresiva
;*
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_ProgresivaToCoord (lst_Traza num_Progresiva / pto_Coordenadas int_Cont num_ProgAcum lst_Result)
	(setq int_Cont 0)
	(setq num_ProgAcum 0)

	(if (> num_Progresiva 0.0)
		(progn
			(while (and (< num_ProgAcum num_Progresiva) (< int_Cont (1- (length lst_Traza))))
				(setq num_ProgAcum (+ num_ProgAcum (distance (nth int_Cont lst_Traza) (nth (1+ int_Cont) lst_Traza))))
				(setq int_Cont (1+ int_Cont))
			)
			(if (or (< num_Progresiva num_ProgAcum) (equal num_Progresiva num_ProgAcum 0.000001))
				(progn
					(setq pto_Coordenadas (polar (nth int_Cont lst_Traza) (angle (nth int_Cont lst_Traza) (nth (1- int_Cont) lst_Traza)) (- num_ProgAcum num_Progresiva)))
					(setq lst_Result (list (cons "PROGRESIVA" num_Progresiva) (cons "PTORIG" pto_Coordenadas) (cons "INDICE" (1- int_Cont))))
				)
				(progn
					(setq lst_Result nil)
				)
			)
		)
		(progn
			(setq pto_Coordenadas (car lst_Traza))
			(setq lst_Result (list (cons "PROGRESIVA" 0.0) (cons "PTORIG" pto_Coordenadas) (cons "INDICE" 0)))
		)
	)
	(eval 'lst_Result)
)

;fn_EventosBA
;***************************************************************************************************************
;*	Arma la tabla de Eventos (basandose en bloque con atributos)
;*	Parámetros:
;*	lst_Traza = Lista de puntos de los vertices
;*	lst_Filtro = Una lista con formato de entidades para filtrar (ej. ((8 . "NombreCapa")). Por defecto vale ((0 . "INSERT")(66 . 1))
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_EventosBA (lst_Traza lst_Filtro / int_Cont sset_Eventos str_Layer lst_Eventos ent_Name lst_PtoInsEvento lst_PtoInsEvento2D ent_Def lst_ElemProy lst_Atributos e1 e2)
;	Comprobación Parámetros
	(if (< (length lst_Traza) 2)
		(exit)
	)

;((0 . "INSERT")(66 . 1))
	(if lst_Filtro
		(setq lst_Filtro (append '((0 . "INSERT")(66 . 1)) lst_Filtro))
		(setq lst_Filtro (append '((0 . "INSERT")(66 . 1))))
	)

;	Inicializando
	(setq int_Cont 0)
	(setq lst_Eventos nil)
	(setq sset_Eventos (ssget "_X" lst_Filtro))

	(while (< int_Cont (sslength sset_Eventos))
		(setq ent_Name (ssname sset_Eventos int_Cont))
;		Saco el Layer del evento
		(setq str_Layer (cdr (assoc 8 (entget ent_Name))))
;		Saco el punto de inserción del evento
		(setq lst_PtoInsEvento (cdr (assoc 10 (entget ent_Name))))

;		Convierto el punto de inserción en 2D
		(setq lst_PtoInsEvento2D (fn_3DTo2DPoint lst_PtoInsEvento))

;		Recorro Atributos
		(setq lst_Atributos nil)
		(setq ent_Def (entnext ent_Name))

		(while (/= (cdr (assoc 0 (entget ent_Def))) "SEQEND")
			(setq lst_Atributos (append lst_Atributos (list (cons (cdr (assoc 2 (entget ent_Def))) (cdr (assoc 1 (entget ent_Def)))))))
			(setq ent_Def (entnext ent_Def))
		)

		(setq lst_ElemProy (fn_CoordenadasToTraza lst_Traza lst_PtoInsEvento2D))

;		((PROGRESIVA INDICE DISTANCIA_TRAZA PTO_INSERCION PTO_PROYECTADO EVENTO) (ATRIBUTOS))
		(setq lst_Eventos
			(append lst_Eventos
				(list
					(append lst_Atributos
						(list
							(assoc "PROGRESIVA" lst_ElemProy)
							(assoc "INDICE" lst_ElemProy)
							(assoc "DISTANCIA" lst_ElemProy)
							(cons "PTORIG" lst_PtoInsEvento)
							(assoc "PTPROY" lst_ElemProy)
							(cons "LAYER" str_Layer)
						)
					)
				)
			)
		)
		(setq int_Cont (1+ int_Cont))
	)

;	Ordenando registros por progresivas
	(setq lst_Eventos
		(vl-sort lst_Eventos
			(function (lambda (e1 e2)
				(< (cdr (assoc "PROGRESIVA" e1)) (cdr (assoc "PROGRESIVA" e2))))
			)
		)
	)

	(eval 'lst_Eventos)
)

;fn_TablaPuntos
;***************************************************************************************************************
;*	Arma la tabla de puntos (basandose en entidades Puntos)
;*	Parámetros:
;*	lst_Traza = Lista de puntos de los vertices
;*	lst_Filtro = Una lista con formato de entidades para filtrar (ej. ((8 . "NombreCapa")). Por defecto vale '((0 . "POINT"))
;*	
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_TablaPuntos (lst_Traza lst_Filtro / sset_Cotas lst_Cotas str_Layer int_Cont ent_Name lst_PtoCota lst_PtoCota2D lst_ElemProy)
;	Comprobación Parámetros
	(if (< (length lst_Traza) 2)
		(exit)
	)

;'((0 . "POINT")(8 . "COTAS"))
	(if lst_Filtro
		(setq lst_Filtro (append '((0 . "POINT")) lst_Filtro))
		(setq lst_Filtro '((0 . "POINT")))
	)

;	Inicializando
	(setq int_Cont 0)
	(setq lst_Cotas nil)
	
	(setq sset_Cotas (ssget "_X" lst_Filtro))

	(while (< int_Cont (sslength sset_Cotas))
;		Saco las coordenadas del punto
		(setq ent_Name (ssname sset_Cotas int_Cont))
		(setq lst_PtoCota (cdr (assoc 10 (entget ent_Name))))
		(setq lst_PtoCota2D (list (car lst_PtoCota) (cadr lst_PtoCota)))
		(setq str_Layer (cdr (assoc 8 (entget ent_Name))))

		(setq lst_ElemProy (fn_CoordenadasToTraza lst_Traza lst_PtoCota2D))

		(setq lst_Cotas
			(append lst_Cotas
				(list
					(list
						(assoc "PROGRESIVA" lst_ElemProy)
						(assoc "INDICE" lst_ElemProy)
						(assoc "DISTANCIA" lst_ElemProy)
						(cons "PTORIG" lst_PtoCota)
						(assoc "PTPROY" lst_ElemProy)
						(cons "LAYER" str_Layer)
						(cons "COTA" (caddr (cdr (assoc 10 (entget ent_Name)))))
					)
				)
			)
		)
		(setq int_Cont (1+ int_Cont))
	)

;	Ordenando registros por progresivas
	(setq lst_Cotas
		(vl-sort lst_Cotas
			(function (lambda (e1 e2)
				(< (cdr (assoc "PROGRESIVA" e1)) (cdr (assoc "PROGRESIVA" e2))))
			)
		)
	)
	(eval 'lst_Cotas)
)


;fn_2DpolyToPoints
;***************************************************************************************************************
;*	Seleccion de una 2Dpolyline y retorna una lista con las coordenadas de los vertices
;*	Parámetros:
;*		NINGUNO
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_2DpolyToPoints( / num_PolyElev lst_Points ent_Poly2D lst_Valor)
	(setq ent_Poly2D (entget (car (entsel "Seleccione 2d poly: ")))) (terpri)
	(if (or (/= (cdr (assoc 0 ent_Poly2D)) "LWPOLYLINE") (= ent_Poly2D nil))
		(progn
			(alert "La entidad seleccionada no es un polylinea 2D.")
			(exit)))
	(setq num_PolyElev (cdr (assoc 38 ent_Poly2D)))
	(foreach lst_Valor ent_Poly2D
		(if (= (car lst_Valor) 10)
			(if (not (equal (last lst_Points) (cdr (append lst_Valor (list num_PolyElev))) 0.00001))
				(setq lst_Points (append lst_Points (list (cdr (append lst_Valor (list num_PolyElev))))))
				(princ "Existen vertices superpuestos en la polilinea. Se han eliminado del listado.\n"))))
	(eval 'lst_Points)
)

;fn_2DpolyToPointsV1
;***************************************************************************************************************
;*	Seleccion de una 2Dpolyline y retorna una lista con las coordenadas de los vertices y el nombre de la entidad seleccionada
;*	Parámetros:
;*		NINGUNO
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_2DpolyToPointsV1( / num_PolyElev lst_Points ent_Poly2D lst_Valor ent_EntName)
	(setq ent_Poly2D (entget (car (entsel "Seleccione 2d poly: ")))) (terpri)
	(setq ent_EntName (cdr (assoc -1 ent_Poly2D)))
	(if (or (/= (cdr (assoc 0 ent_Poly2D)) "LWPOLYLINE") (= ent_Poly2D nil))
		(progn
			(alert "La entidad seleccionada no es un polylinea 2D.")
			(exit)))
	(setq num_PolyElev (cdr (assoc 38 ent_Poly2D)))

	(foreach lst_Valor ent_Poly2D
		(if (= (car lst_Valor) 10)
			(if (not (equal (last lst_Points) (cdr (append lst_Valor (list num_PolyElev))) 0.00001))
				(setq lst_Points (append lst_Points (list (cdr (append lst_Valor (list num_PolyElev))))))
				(princ "Existen vertices superpuestos en la polilinea. Se han eliminado del listado.\n"))))
	(setq lst_Points (cons ent_EntName lst_Points))
	(eval 'lst_Points)
)

;fn_2DpolyToPointsV2
;***************************************************************************************************************
;*	A partir del ename de una 2Dpolyline retorna una lista con las coordenadas de los vertices
;*	Parámetros:
;*		ent_EntName
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_2DpolyToPointsV2(ent_EntName / num_PolyElev lst_Points ent_Poly2D lst_Valor)
	(setq ent_Poly2D (entget ent_EntName))
	(if (or (/= (cdr (assoc 0 ent_Poly2D)) "LWPOLYLINE") (= ent_Poly2D nil))
		(progn
			(alert "La entidad seleccionada no es un polylinea 2D.")
			(exit)))
	(setq num_PolyElev (cdr (assoc 38 ent_Poly2D)))

	(foreach lst_Valor ent_Poly2D
		(if (= (car lst_Valor) 10)
			(if (not (equal (last lst_Points) (cdr (append lst_Valor (list num_PolyElev))) 0.00001))
				(setq lst_Points (append lst_Points (list (cdr (append lst_Valor (list num_PolyElev))))))
				(princ "Existen vertices superpuestos en la polilinea. Se han eliminado del listado.\n"))))
	(eval 'lst_Points)
)

;fn_3DpolyToPoints
;***************************************************************************************************************
;*	Seleccion de una 3Dpolyline y retorna una lista con las coordenadas de los vertices
;*	Parámetros:
;*		NINGUNO
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_3DpolyToPoints ( / lst_Points ena_Entname)
	(setq ena_Entname (car (entsel "Seleccione 3D poly: "))) (terpri)
	(if (or (/= (cdr (assoc 0 (entget ena_Entname))) "POLYLINE") (= ena_Entname nil))
		(progn
			(alert "La entidad seleccionada no es un polylinea 3D.")
			(exit)
		)
	)
	(setq ena_Entname (entnext ena_Entname))

	(while (/= (cdr (assoc 0 (entget ena_Entname))) "SEQEND")
		(setq lst_Points (append lst_Points (list (cdr (assoc 10 (entget ena_Entname))))))
		(setq ena_Entname (entnext ena_Entname))
	)
	(eval 'lst_Points)
)

;fn_3DpolyToPointsV1
;***************************************************************************************************************
;*	A partir del ename de una 3Dpolyline retorna una lista con las coordenadas de los vertices
;*	Parámetros:
;*		ent_EntName
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_3DpolyToPointsV1 (ent_EntName / lst_Points ena_Entname)
	(if (or (/= (cdr (assoc 0 (entget ent_EntName))) "POLYLINE") (= ent_EntName nil))
		(progn
			(alert "La entidad seleccionada no es un polylinea 3D.")
			(exit)
		)
	)
	(setq ent_EntName (entnext ent_EntName))

	(while (/= (cdr (assoc 0 (entget ent_EntName))) "SEQEND")
		(setq lst_Points (append lst_Points (list (cdr (assoc 10 (entget ent_EntName))))))
		(setq ent_EntName (entnext ent_EntName))
	)
	(eval 'lst_Points)
)



;fn_2DpolyToPointsV3
;***************************************************************************************************************
;*	A partir del ename de una 2Dpolyline retorna una lista con las coordenadas y las progresivas de los vertices
;*	Parámetros:
;*		ent_EntName
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_2DpolyToPointsV3(ent_EntName / num_PolyElev lst_Points ent_Poly2D lst_Valor num_ProgAcum lst_VertAnt lst_VertAct)
	(setq ent_Poly2D (entget ent_EntName))
	(if (or (/= (cdr (assoc 0 ent_Poly2D)) "LWPOLYLINE") (= ent_Poly2D nil))
		(progn
			(alert "La entidad seleccionada no es un polylinea 2D.")
			(exit)))
	(setq num_PolyElev (cdr (assoc 38 ent_Poly2D)))

	(setq lst_Points nil)
	(setq num_ProgAcum 0.0)
	(setq lst_VertAnt nil)
	(foreach lst_Valor ent_Poly2D
		(if (= (car lst_Valor) 10)
			(progn
				(setq lst_VertAct (append (cdr lst_Valor) (list num_PolyElev)))
				(if (not lst_VertAnt)
					(setq lst_VertAnt lst_VertAct))
				(setq num_ProgAcum (+ num_ProgAcum (distance (fn_3DTo2DPoint lst_VertAnt) (fn_3DTo2DPoint lst_VertAct))))
				(setq lst_Points (append lst_Points (list (list (cons "COORD" lst_VertAct)(cons "PROGRESIVA" num_ProgAcum)))))
				(setq lst_VertAnt lst_VertAct)
			)
		)
	)
	(eval 'lst_Points)
)

;fn_3DpolyToPointsV3
;***************************************************************************************************************
;*	A partir del ename de una 3Dpolyline retorna una lista con las coordenadas de los vertices
;*	Parámetros:
;*		ent_EntName
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_3DpolyToPointsV3 (ent_EntName / lst_Points num_ProgAcum lst_VertAnt lst_VertAct)
	(if (or (/= (cdr (assoc 0 (entget ent_EntName))) "POLYLINE") (= ent_EntName nil))
		(progn
			(alert "La entidad seleccionada no es un polylinea 3D.")
			(exit)
		)
	)
	(setq ent_EntName (entnext ent_EntName))

	(setq num_ProgAcum 0.0)
	(setq lst_VertAnt nil)
	(while (/= (cdr (assoc 0 (entget ent_EntName))) "SEQEND")
		(setq lst_VertAct (cdr (assoc 10 (entget ent_EntName))))
		(if (not lst_VertAnt)
			(setq lst_VertAnt lst_VertAct))
		(setq num_ProgAcum (+ num_ProgAcum (distance (fn_3DTo2DPoint lst_VertAnt) (fn_3DTo2DPoint lst_VertAct))))
		(setq lst_Points (append lst_Points (list (list (cons "COORD" lst_VertAct)(cons "PROGRESIVA" num_ProgAcum)))))
		(setq lst_VertAnt lst_VertAct)
		(setq ent_EntName (entnext ent_EntName))
	)
	(eval 'lst_Points)
)

;fn_ProgresivasPoints
;***************************************************************************************************************
;*	A partir de una lista con las coordenadas de los vertices retorna las progresivas de estos
;*	Parámetros:
;*		ent_EntName
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_ProgresivasPoints (lst_CoordPoints / num_ProgAcumulada lst_PtoAct lst_Temp lst_Progresivas)
	(setq num_ProgAcumulada 0.0)
	(setq lst_PtoAct (car lst_CoordPoints))
	(setq lst_Progresivas nil)
	(foreach lst_Temp lst_CoordPoints
		(setq num_ProgAcumulada (+ num_ProgAcumulada (distance lst_PtoAct lst_Temp)))
		(setq lst_Progresivas (append lst_Progresivas (list num_ProgAcumulada)))
		(setq lst_PtoAct lst_Temp)
	)
	(eval 'lst_Progresivas)
)

;FUNCIONES TRIGONOMÉTRICAS
;***************************************************************************************************************
;*	acos
;*	asin
;*	fn_AngTeoremaCoseno
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************

(defun acos (x) 
	(cond 
		((or (equal x -1.0 0.0000001) (equal x 1.0 0.0000001)) 0.0)
;		((or (= x -1.0) (= x 1.0)) 0.0)
		((or (> x 1.0) (< x -1.0)) nil)
		(T (atan (sqrt (- 1.0 (* x x))) x))
	)
)

(defun asin (z) 
	(cond
		((zerop z) 0.0)
    ((= z 1.0)  PI/2)
   	((= z -1.0) (- PI/2)) 
		(T (atan z (sqrt (- 1.0 (* z z))))) 
	)
)

(defun fn_AngTeoremaCoseno(num_a num_b num_c / num_Resultado)
;	(princ "ACOS de :")
;	(princ (/ (- (+ (expt num_a 2.0) (expt num_b 2.0)) (expt num_c 2.0)) (* 2.0 num_a num_b)))
;	(princ "\n")
	(setq num_Resultado (acos (/ (- (+ (expt num_a 2.0) (expt num_b 2.0)) (expt num_c 2.0)) (* 2.0 num_a num_b))))
	(eval 'num_Resultado)
)

;FINAL DEFINICIÓN FUNCIONES TRIGONOMÉTRICAS
;***************************************************************************************************************
;*	acos
;*	asin
;*	fn_AngTeoremaCoseno
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************

;fn_DibujaLine
;***************************************************************************************************************
;*	Dibuja una linea
;*	Parámetros:
;*		lst_Point1, lst_Point2 = Punto de inicio y de fin
;*		str_Layer = Layer donde se dibujará la linea
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DibujaLine (lst_Point1 lst_Point2 str_Layer / lst_ent)
	(setq lst_ent
		(list
			'(0 . "LINE")
			'(100 . "AcDbEntity")
			(cons 8 str_Layer)
			'(100 . "AcDbLine")
			(cons 10 lst_Point1)
			(cons 11 lst_Point2)
		)
	)
	(entmake lst_ent)
	(princ)
)

;fn_Dibuja3DPoly
;***************************************************************************************************************
;*	Dibuja una Polyline 3D
;*	Parámetros:
;*		lst_Points = Lista de coordenadas de Puntos ((x1 y1 z1)(x2 y2 z2) ... (xn yn zn))
;*		str_Layer = Layer donde se dibujará la linea
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_Dibuja3DPoly(lst_Points str_Layer / x lst_DefPoly)
	(if (/= str_Layer nil)
		(setq lst_DefPoly (list	(cons 0 "POLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDb3dPolyline") (cons 10 '(0.0 0.0 0.0)) (cons 70 8) (cons 8 str_Layer)))
		(setq lst_DefPoly (list	(cons 0 "POLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDb3dPolyline") (cons 10 '(0.0 0.0 0.0)) (cons 70 8)))
	)
	(entmake lst_DefPoly)
	(foreach x lst_Points
		(progn
			(entmake (list
					(cons 0 "VERTEX")
					(cons 100 "AcDbEntity")
					(cons 100 "AcDbVertex")
					(cons 100 " AcDb3dPolylineVertex")
					(cons 10 x)
					(cons 70 32)
					(cons 50 0.0)
				)
			)
		)
	)
	(entmake '((0 . "SEQEND")))
	(princ)
)

;fn_Dibuja2DPoly
;***************************************************************************************************************
;*	Dibuja una Polyline 2D
;*	Parámetros:
;*		lst_Points = Lista de coordenadas de Puntos ((x1 y1 z1)(x2 y2 z2) ... (xn yn zn)) o ((x1 y1)(x2 y2) ... (xn yn))
;*		str_Layer = Layer donde se dibujará la linea
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_Dibuja2DPoly(lst_Points str_Layer / lst_DefPoly x)
  (setq lst_DefPoly
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 str_Layer)
      (cons 100 "AcDbPolyline")
      (cons 90 (length lst_Points))
      '(70 . 1)
      '(43 . 0.0)
      '(38 . 0.0)
      '(39 . 0.0)
    )
  )

  (foreach x lst_Points
    (setq lst_DefPoly
    	(append lst_DefPoly
    		(list
    			(list 10 (car x) (cadr x))
    			'(40 . 0.0)
    			'(41 . 0.0)
    			'(42 . 0.0)
    		)
    	)
    )
  )
  (entmake lst_DefPoly)
  (princ)
)

;fn_Dibuja2DPolyV1
;***************************************************************************************************************
;*	Dibuja una Polyline 2D
;*	Parámetros:
;*		lst_Points = Lista de coordenadas de Puntos ((x1 y1 z1)(x2 y2 z2) ... (xn yn zn)) o ((x1 y1)(x2 y2) ... (xn yn))
;*		str_Layer = Layer donde se dibujará la linea
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_Dibuja2DPolyV1 (lst_Points str_Layer / lst_DefPoly x)
  (setq lst_DefPoly
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 str_Layer)
      (cons 100 "AcDbPolyline")
      (cons 90 (length lst_Points))
      '(70 . 0)
      '(43 . 0.0)
      '(38 . 0.0)
      '(39 . 0.0)
    )
  )

  (foreach x lst_Points
    (setq lst_DefPoly
    	(append lst_DefPoly
    		(list
    			(list 10 (car x) (cadr x))
    			'(40 . 0.0)
    			'(41 . 0.0)
    			'(42 . 0.0)
    		)
    	)
    )
  )
  (entmake lst_DefPoly)
  (princ)
)


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

;fn_DibujaTextV1
;***************************************************************************************************************
;*	Dibuja un Texto
;*	Parámetros:
;*		str_Texto = Valor del texto a escribir
;*		lst_Coords = Coordenadas del punto de insercion del texto
;*		num_Htxt = Altura del Texto
;*		str_Capa = Layer donde se dibujará el texto
;*		str_Style = Estilo del texto a dibujar
;*		num_Width = Width del texto
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
(defun fn_DibujaTextV1 (str_Texto lst_Coords num_Htxt str_Capa str_Style num_Width int_Justh int_Justv num_RotRadian / lst_ent)
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
			(cons 41 num_Width)
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


;fn_DibujaMText
;***************************************************************************************************************
;*	Dibuja un Texto
;*	Parámetros:
;*		str_Texto = Valor del texto a escribir
;*		lst_Coords = Coordenadas del punto de insercion del texto
;*		num_Htxt = Altura del Texto
;*		str_Capa = Layer donde se dibujará el texto
;*		str_Style = Estilo del texto a dibujar
;*
;*		Attachment point:
;*			1 = Top left; 2 = Top center; 3 = Top right
;*			4 = Middle left; 5 = Middle center; 6 = Middle right
;*			7 = Bottom left; 8 = Bottom center; 9 = Bottom right 
;*				Vertical text justification type (optional, default = 0): integer codes (not bit-coded):
;*
;*		num_RotRadian = Angulo de rotación en radianes
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DibujaMText (str_Texto lst_Coords num_Width num_Htxt str_Capa str_Style int_AtachmentPoint num_RotRadian / lst_ent)
  (setq lst_ent
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 str_Capa)
      '(100 . "AcDbMText")
      (cons 10 lst_Coords)
      (cons 40 num_Htxt)
      (cons 41 num_Width)
      (cons 71 int_AtachmentPoint)
      '(72 . 5)
      (cons 1 str_Texto)
      (cons 7 str_Style)
      (cons 11 lst_Coords)
      (cons 50 num_RotRadian)
      '(73 . 1)
      '(44 . 1.0)
     )
  )
  (entmake lst_ent)
)

;fn_DibujaMText1
;***************************************************************************************************************
;*	Dibuja un Texto
;*	Parámetros:
;*		str_Texto = Valor del texto a escribir
;*		lst_Coords = Coordenadas del punto de insercion del texto
;*		num_Htxt = Altura del Texto
;*		str_Capa = Layer donde se dibujará el texto
;*		str_Style = Estilo del texto a dibujar
;*
;*		Attachment point:
;*			1 = Top left; 2 = Top center; 3 = Top right
;*			4 = Middle left; 5 = Middle center; 6 = Middle right
;*			7 = Bottom left; 8 = Bottom center; 9 = Bottom right 
;*				Vertical text justification type (optional, default = 0): integer codes (not bit-coded):
;*
;*		num_RotRadian = Angulo de rotación en radianes
;* (fn_DibujaMText1 "Probando Pelotudeces.\nProbando Pelotudeces.\nProbando Pelotudeces." (list 0.0 0.0) 50 2.5 "Hola" "Standard" 1 0.0 1.5)
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DibujaMText1 (str_Texto lst_Coords num_Width num_Htxt str_Capa str_Style int_AtachmentPoint num_RotRadian num_LineSpacing / lst_ent)
  (setq lst_ent
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 str_Capa)
      '(100 . "AcDbMText")
      (cons 10 lst_Coords)
      (cons 40 num_Htxt)
      (cons 41 num_Width)
      (cons 71 int_AtachmentPoint)
      '(72 . 5)
      (cons 1 str_Texto)
      (cons 7 str_Style)
      (cons 11 lst_Coords)
      (cons 50 num_RotRadian)
      '(73 . 1)
      (cons 44 num_LineSpacing)
     )
  )
  (entmake lst_ent)
)


;fn_DibujaPoint
;***************************************************************************************************************
;*	Dibuja un Punto
;*	Parámetros:
;*		lst_Coordenadas = Lista de coordenadas del Punto
;*		str_Layer = Layer donde se dibujará el punto
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DibujaPoint (lst_Coordenadas str_Layer / ent_Punto)
	(setq ent_Punto
		(list
			'(0 . "POINT")
			'(100 . "AcDbEntity")
			(cons 8 str_Layer)
			'(100 . "AcDbPoint")
			(cons 10 lst_Coordenadas)
		)
	)
	(entmake ent_Punto)
)

;fn_DibujaCircle
;***************************************************************************************************************
;*	Dibuja un Punto
;*	Parámetros:
;*		lst_Coordenadas = Lista de coordenadas del Centro del Circulo
;*		str_Layer = Layer donde se dibujará el circulo
;*		num_Radio = Radio del circulo
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DibujaCircle (lst_Coordenadas num_Radio str_Layer / ent_Circle)
	(setq ent_Circle
		(list
			'(0 . "CIRCLE")
			'(100 . "AcDbEntity")
			(cons 8 str_Layer)
			'(100 . "AcDbCircle")
			(cons 10 lst_Coordenadas)
			(cons 40 num_Radio)
		)
	)
	(entmake ent_Circle)
)

;fn_MidPoint
;***************************************************************************************************************
;*	Devuelve el punto medio entre dos puntos
;*	Parámetros:
;*		lst_Pt1 y lst_Pt2 = Los puntos de los cuales se calculará el punto medio
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_MidPoint (lst_Pt1 lst_Pt2)
	(polar lst_Pt1 (angle lst_Pt1 lst_Pt2) (/ (distance lst_Pt1 lst_Pt2) 2.0))
)

;fn_DistEscala
;***************************************************************************************************************
;*	Retorna una distancia en unidades de dibujo posterior a aplicarle una escala
;*	Parámetros:
;*		num_Dist = La distancia en unidades reales
;*		num_Escala = La escala a ser aplicada
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DistEscala (num_Dist num_Escala)
	(* num_Dist (/ num_Escala 1000.0))
)

;fn_DistEscalaV1
;***************************************************************************************************************
;*	Retorna una distancia en unidades de dibujo posterior a aplicarle una escala
;*	Parámetros:
;*		num_Dist = La distancia en unidades reales
;*		num_Escala = La escala a ser aplicada
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DistEscalaV1 (num_Dist num_Escala)
	(/ num_Dist num_Escala)
)


;fn_AngRotTxt
;***************************************************************************************************************
;*	Corrige la rotación de un texto para que sea legible según IRAM
;*	Parámetros:
;*		num_AnguloTxt = El ángulo de rotación del texto sin corrección
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_AngRotTxt (num_AnguloTxt / num_AngCorregido)
	(cond
		((and (> num_AnguloTxt (/ pi 2.0)) (< num_AnguloTxt pi))
			(setq num_AngCorregido (+ num_AnguloTxt pi))
		)
		((and (>= num_AnguloTxt pi) (<= num_AnguloTxt (* pi 1.5)))
			(setq num_AngCorregido (- num_AnguloTxt pi))
		)
		(t
			(setq num_AngCorregido num_AnguloTxt)
		)
	)
	(eval 'num_AngCorregido)
)

(defun fn_CoordsDespl1 (lst_PtIns num_Desplazamiento num_AnguloTxt int_CodDespl)
; int_CodDespl = 0 Para Arriba; 1 Para Abajo
	(cond
		((= int_CodDespl 0)
			(polar lst_PtIns (+ (/ pi 2.0) num_AnguloTxt) num_Desplazamiento)
		)
		((= int_CodDespl 1)
			(polar lst_PtIns (- (/ pi 2.0) num_AnguloTxt) num_Desplazamiento)
		)
	)
)

;fn_HectareasDecToTextual
;***************************************************************************************************************
;*	Convierte hectareas con notacion tipo catastro en formato textual. Retorna un string con formato.
;*	El parámetro bool_Resume define como se escriben las definiciones (si T escribe resumido)
;*
;*	Parámetros:
;*		num_SupHec = El número que representa el área
;*		bool_Resume = Si se escriben las unidades resumidas o no
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_HectareasDecToTextual (num_SupHec bool_Resume / num_Hectareas num_Areas num_Centiareas num_Decimetros str_HACD lst_Abreviaturas)

	(setq num_Hectareas (fix num_SupHec))
	(setq num_Areas (fix (* (- num_SupHec num_Hectareas) 100.0)))
	(setq num_Centiareas (* (- num_SupHec num_Hectareas (/ num_Areas 100.0)) 10000.0))

	(if bool_Resume
		(setq lst_Abreviaturas (list " Hs. " " As. " " Cas. " " dm"))
		(setq lst_Abreviaturas (list " Hectareas " " Areas " " Centiareas " " Decímetros cuadrados"))
	)

	(setq str_HACD "")

	(if (> num_Hectareas 0)
		(setq str_HACD (strcat (itoa num_Hectareas) (nth 0 lst_Abreviaturas)))
		(setq str_HACD (strcat "0" (nth 0 lst_Abreviaturas)))
	)

	(if (> num_Areas 0)
		(if (< num_Areas 10)
			(setq str_HACD (strcat str_HACD "0" (itoa num_Areas)))
			(setq str_HACD (strcat str_HACD (itoa num_Areas)))
		)
		(setq str_HACD (strcat str_HACD "00"))
	)

	(setq str_HACD (strcat str_HACD (nth 1 lst_Abreviaturas)))

	(if (> num_Centiareas 0)
		(if (< num_Centiareas 10.0)
			(setq str_HACD (strcat str_HACD "0" (rtos num_Centiareas 2 0)))
			(setq str_HACD (strcat str_HACD (rtos num_Centiareas 2 0)))
		)
		(setq str_HACD (strcat str_HACD "00"))
	)

	(setq str_HACD (strcat str_HACD (nth 2 lst_Abreviaturas)))
	(vl-string-right-trim " " str_HACD)
)

;fn_Metros2ToHectareas
;***************************************************************************************************************
;*	Convierte metros2 en hectareas. Retorna una lista con los valores de Hectareas; Areas; Centiareas y Decímetros cuadrados.
;*	Y el valor en formato texto.
;*	El parámetro bool_Resume define como se escriben las definiciones (si T escribe resumido)
;*	El parametro bool_Decimetros define si se escriben los decimetros cuadrados
;*
;*	Parámetros:(fn_Metros2ToHectareas 1 T T)
;*		num_AreaM2 = El número que representa el área
;*		bool_Resume = Si se escriben las unidades resumidas o no
;*		bool_Decimetros = Si se escriben los decimetros o no
;*
;*	Programado por Diego Cassini - Ver_1.00 (fn_Metros2ToHectareas 0.4563 T nil)
;***************************************************************************************************************
(defun fn_Metros2ToHectareas (num_AreaM2 bool_Resume bool_Decimetros / num_Hectareas num_Areas num_Centiareas num_Decimetros str_HACD lst_Abreviaturas)
	(setq num_Hectareas (fix (/ num_AreaM2 10000.0)))
	(setq num_Areas (fix (/ (- num_AreaM2 (* num_Hectareas 10000.0)) 100.0)))
	(setq num_Centiareas (fix (- num_AreaM2 (* num_Hectareas 10000.0) (* num_Areas 100.0))))
	(setq num_Decimetros (fix (* (- num_AreaM2 (fix num_AreaM2)) 100.0)))

	(if bool_Resume
		(setq lst_Abreviaturas (list " Hs. " " As. " " Cas. " " dm"))
		(setq lst_Abreviaturas (list " Hectareas " " Areas " " Centiareas " " Decímetros cuadrados"))
	)

	(setq str_HACD "")

	(if (> num_Hectareas 0)
		(setq str_HACD (strcat (itoa num_Hectareas) (nth 0 lst_Abreviaturas)))
		(setq str_HACD (strcat "0" (nth 0 lst_Abreviaturas)))
	)

	(if (> num_Areas 0)
		(progn
			(if (< num_Areas 10)
				(setq str_HACD (strcat str_HACD "0" (itoa num_Areas)))
				(setq str_HACD (strcat str_HACD (itoa num_Areas)))
			)
		)
		(progn
			(setq str_HACD (strcat str_HACD "00"))
		)
	)

	(setq str_HACD (strcat str_HACD (nth 1 lst_Abreviaturas)))
	
	(if (> num_Centiareas 0)
		(progn
			(if (< num_Centiareas 10)
				(setq str_HACD (strcat str_HACD "0" (itoa num_Centiareas)))
				(setq str_HACD (strcat str_HACD (itoa num_Centiareas)))
			)
		)
		(progn
			(setq str_HACD (strcat str_HACD "00"))
		)
	)

	(setq str_HACD (strcat str_HACD (nth 2 lst_Abreviaturas)))

	(if bool_Decimetros
		(setq str_HACD (strcat str_HACD (itoa num_Decimetros) (nth 3 lst_Abreviaturas)))
	)

	(eval '(list str_HACD num_Hectareas num_Areas num_Centiareas num_Decimetros))
)

;fn_Metros2ToHectareasTextual
;***************************************************************************************************************
;*	Convierte metros2 en hectareas. Retorna el valor en formato textual.
;*	El parámetro bool_Resume define como se escriben las definiciones (si T escribe resumido)
;*	El parametro bool_Decimetros define si se escriben los decimetros cuadrados
;*
;*	Parámetros:
;*		num_AreaM2 = El número que representa el área
;*		bool_Resume = Si se escriben las unidades resumidas o no
;*		bool_Decimetros = Si se escriben los decimetros o no
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_Metros2ToHectareasTextual (num_AreaM2 bool_Resume bool_Decimetros / lst_Valor str_Hectareas str_Areas str_Centiareas str_Decimetros str_HACD lst_Abreviaturas)
	(setq lst_Valor (fn_Metros2ToHectareas num_AreaM2 bool_Resume bool_Decimetros))

	(if bool_Resume
		(setq lst_Abreviaturas (list " Hs." " As." " Cas." " dm"))
		(setq lst_Abreviaturas (list " Hectáreas" " Areas" " Centiareas" " Decímetros cuadrados" " Hectárea" " Area" " Centiarea" " Decímetro cuadrado"))
	)

	;Hecatareas
	(if (> (cadr lst_Valor) 0.0)
		(progn
			(if (and (= (cadr lst_Valor) 1.0) (not bool_Resume))
				(setq str_Hectareas (strcat (fn_NumerosEnLetras (itoa (cadr lst_Valor)) 1) (nth 4 lst_Abreviaturas)))
				(setq str_Hectareas (strcat (fn_NumerosEnLetras (itoa (cadr lst_Valor)) 1) (car lst_Abreviaturas)))
			)
		)
		(progn
			(setq str_Hectareas (strcat "Cero" (car lst_Abreviaturas)))
		)
	)

	;Areas
	(if (> (caddr lst_Valor) 0.0)
		(progn
			(if (and (= (caddr lst_Valor) 1.0) (not bool_Resume))
				(setq str_Areas (strcat (fn_NumerosEnLetras (itoa (caddr lst_Valor)) 0) (nth 5 lst_Abreviaturas)))
				(setq str_Areas (strcat (fn_NumerosEnLetras (itoa (caddr lst_Valor)) 0) (cadr lst_Abreviaturas)))
			)
		)
		(progn
			(setq str_Areas (strcat "Cero" (cadr lst_Abreviaturas)))
		)
	)

	;Centiareas
	(if (> (cadddr lst_Valor) 0.0)
		(progn
			(if (and (= (cadddr lst_Valor) 1.0) (not bool_Resume))
				(setq str_Centiareas (strcat (fn_NumerosEnLetras (itoa (cadddr lst_Valor)) 0) (nth 6 lst_Abreviaturas)))
				(setq str_Centiareas (strcat (fn_NumerosEnLetras (itoa (cadddr lst_Valor)) 0) (caddr lst_Abreviaturas)))
			)
		)
		(progn
			(setq str_Centiareas (strcat "Cero" (caddr lst_Abreviaturas)))
		)
	)

	(if bool_Decimetros
		(progn
			(if (> (nth 4 lst_Valor) 0.0)
				(progn
					(if (and (= (nth 4 lst_Valor) 1.0) (not bool_Resume))
						(setq str_Decimetros (strcat (fn_NumerosEnLetras (itoa (nth 4 lst_Valor)) 0) (nth 7 lst_Abreviaturas)))
						(setq str_Decimetros (strcat (fn_NumerosEnLetras (itoa (nth 4 lst_Valor)) 0) (cadddr lst_Abreviaturas)))
					)
				)
				(progn
					(setq str_Decimetros (strcat "Cero" (cadddr lst_Abreviaturas)))
				)
			)
		)
		(progn
			(setq str_Decimetros "")
		)
	)
	
	(if bool_Decimetros
		(setq str_HACD (strcat str_Hectareas ", " str_Areas ", " str_Centiareas ", " str_Decimetros))
		(setq str_HACD (strcat str_Hectareas ", " str_Areas ", " str_Centiareas))
	)

	(eval 'str_HACD)
)

;fn_BisectrizAnguloInterno NO ESPECIFICADO
;***************************************************************************************************************
;*	Calcula el ángulo intermedio para dibujar las dimenciones del angulo
;*	Parámetros:
;*		lst_Pt1; lst_Pt2; lst_Pt3 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_BisectrizAnguloInterno (lst_Pt1 lst_Pt2 lst_Pt3 / num_AnguloMedioDibujo)
	(princ)
)


;fn_LongitudLado
;***************************************************************************************************************
;*	Calcula la longitud de un lado del poligono
;*	Parámetros:
;*		lst_Coordenada1; lst_Coordenada2 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_LongitudLado (lst_Coordenada1 lst_Coordenada2)
	(distance lst_Coordenada1 lst_Coordenada2)
)

;fn_Azimut
;***************************************************************************************************************
;*	Calcula el azimut del lado de un poligono.
;*
;*	Parámetros:
;*		lst_Coordenada1; lst_Coordenada2 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_Azimut (lst_Coordenada1 lst_Coordenada2 / num_Azimut)
;	Si es mayor a 1 giro le resto el giro
	(setq num_Azimut (- (* pi 2.5) (angle lst_Coordenada1 lst_Coordenada2)))
	(if (>= num_Azimut (* 2 pi))
		(setq num_Azimut (- num_Azimut (* pi 2)))
	)
	(eval 'num_Azimut)
;	(angtos (- (* pi 2.5) (angle (lst_Coordenada1) (lst_Coordenada2))) 1 8)
)

;fn_AnguloInternoV1
;***************************************************************************************************************
;*	Calcula el angulo interno de un poligono.
;*	El Vertice del angulo a averiguar es la lst_Coordenada2
;*	Utiliza el método -> Azimut anterior menos azimut posterior
;*	Parámetros:
;*		lst_Coordenada1; lst_Coordenada2; lst_Coordenada3 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_AnguloInternoV1 (lst_Coordenada1 lst_Coordenada2 lst_Coordenada3 / num_AnguloInterno)
; El Vertice del angulo a averiguar es la 2
; Azimut anterior menos azimut posterior
	(setq num_AnguloInterno (- (fn_Azimut lst_Coordenada1 lst_Coordenada2) (fn_Azimut lst_Coordenada3 lst_Coordenada2)))
	(eval 'num_AnguloInterno)
)

;fn_AnguloInternoV2
;***************************************************************************************************************
;*	Calcula el angulo interno de un poligono.
;*	El Vertice del angulo a averiguar es la lst_Coordenada2
;*	Utiliza el método -> Azimut anterior menos azimut posterior y lo corrige según el sentido de giro
;*	Parámetros:
;*		lst_Coordenada1; lst_Coordenada2; lst_Coordenada3 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_AnguloInternoV2 (lst_Coordenada1 lst_Coordenada2 lst_Coordenada3 int_Giro / num_AnguloInterno)
; El Vertice del angulo a averiguar es la 2
; Azimut anterior menos azimut posterior
	(setq num_AnguloInterno (- (fn_Azimut lst_Coordenada1 lst_Coordenada2) (fn_Azimut lst_Coordenada3 lst_Coordenada2)))

	(if (< num_AnguloInterno 0.0)
		(setq num_AnguloInterno (+ num_AnguloInterno (* 2.0 pi))))

	(if (= int_Giro 1) ;Antihorario
		(setq num_AnguloInterno (- (* 2 pi) num_AnguloInterno)))
	(eval 'num_AnguloInterno)
)

;fn_AnguloInterno
;***************************************************************************************************************
;*	Calcula el angulo interno de un poligono.
;*	El Vertice del angulo a averiguar es la lst_Coordenada2
;*	Utiliza el método -> Azimut anterior menos azimut posterior
;*	Parámetros:
;*		lst_Coordenada1; lst_Coordenada2; lst_Coordenada3 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00 (fn_AnguloInternoV2 (getpoint)(getpoint)(getpoint) 1)
;***************************************************************************************************************
(defun fn_AnguloInterno (lst_Coordenada1 lst_Coordenada2 lst_Coordenada3 / num_AnguloInterno)
; El Vertice del angulo a averiguar es la 2
; Azimut anterior menos azimut posterior
	(setq num_AnguloInterno (- (fn_Azimut lst_Coordenada1 lst_Coordenada2) (fn_Azimut lst_Coordenada3 lst_Coordenada2)))
	(if (< num_AnguloInterno 0.0)
		(setq num_AnguloInterno (+ num_AnguloInterno (* 2.0 pi)))
	)
	(eval 'num_AnguloInterno)
)

;fn_RotTxtAngulo
;***************************************************************************************************************
;*	Calcula el angulo de rotacion para dibujar el texto con la medida angular.
;*	El Vertice del angulo a averiguar es la lst_Coordenada2
;*	Utiliza el método -> Azimut anterior menos azimut posterior
;*	Parámetros:
;*		lst_Coordenada1; lst_Coordenada2; lst_Coordenada3 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_RotTxtAngulo (lst_Coordenada1 lst_Coordenada2 lst_Coordenada3 / num_RotAng num_AngInt num_AngAnt)
	(setq num_AngInt (fn_AnguloInterno lst_Coordenada1 lst_Coordenada2 lst_Coordenada3))
	(setq num_AngAnt (angle lst_Coordenada1 lst_Coordenada2))
	(setq num_RotAng (+ num_AngAnt (/ num_AngInt 2.0)))
	(if (> num_RotAng (* 2 pi))
		(setq num_RotAng (- num_RotAng (* 2 pi))))
	(eval 'num_RotAng)
)

;fn_RumboDecTxt
;***************************************************************************************************************
;*	Calcula el rumbo de un segmento. Lo retorna con el formato de agrimensura SoN gggdmm'ss" EoW
;*	OJO!!! Es bueno pasar este resultado por la función "fn_CompletaAngTxt"
;*
;*	Parámetros:
;*		lst_Coordenada1; lst_Coordenada2 = Las coordenadas de los vertices
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_RumboDecTxt (lst_Coordenada1 lst_Coordenada2)
;	(angle lst_Coordenada1 lst_Coordenada2)
	(angtos (angle lst_Coordenada1 lst_Coordenada2) 4 4)
)

;fn_AreaPoligono
;***************************************************************************************************************
;*	Calcula el area de una polylinea a partir de su nombre de entidad
;*
;*	Parámetros:
;*		ent_eName = Nombre de la entidad polylinea
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_AreaPoligono (ent_eName int_Decimales)
	(rtos (vla-get-area (vlax-ename->vla-object ent_eName)) 2 int_Decimales)
)

;fn_DN-DE
;***************************************************************************************************************
;*	Calcula el Delta N y el Delta Este. Retorna una lista con los dos valores con el formato ( ("DN" . valorDN) ("DE" . valorDE) )
;*
;*	Parámetros:
;*		ent_eName = Nombre de la entidad polylinea
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_DN-DE (lst_CoordsIni lst_CoordsFin / num_DN num_DE)
	(setq num_DN (- (cadr lst_CoordsFin) (cadr lst_CoordsIni)))
	(setq num_DE (- (car lst_CoordsFin) (car lst_CoordsIni)))
	(list (cons "DN" num_DN) (cons "DE" num_DE))
)

;fn_TraduceAngTxt
;***************************************************************************************************************
;*	Reemplaza los W por O y los d por (chr 176 u 186)
;*
;*	Parámetros:
;*		str_Angulo = El ángulo en formato sexagecimal.
;*		str_DegresRep = El valor ascii del caracter que representará a "d" Degres
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_TraduceAngTxt (str_Angulo str_DegresRep)
	(vl-string-subst "O" "W" (vl-string-subst str_DegresRep "d" str_Angulo))
)

;fn_CompletaAngTxt
;***************************************************************************************************************
;*	Completa ceros en la representación de angulos (en formato azimut o angular) a partir del angulo expresado en sexagecimal
;*
;*	Parámetros:
;*		str_Angulo = El ángulo en formato sexagecimal.
;*		str_DegresRep = El valor ascii del caracter que representará a "d" Degres
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_CompletaAngTxt (str_Angulo str_DegresRep / str_Grados str_Minutos str_Segundos str_AnguloCorr str_RY str_RX int_Pos1 int_Pos2 int_Pos3 int_Pos4)
;	Si es rumbo "@ #"
;	Si es angulo #
	(if (= (strlen str_Angulo) 1)
		(progn
			(setq str_AnguloCorr str_Angulo)
		)
		(progn
			(setq str_RY "")
			(setq str_RX "")
			(if (wcmatch str_Angulo "? *")
				(progn
					(setq str_RY (substr str_Angulo 1 2))
					(setq str_RX (substr str_Angulo (- (strlen str_Angulo) 1) 2))
					(setq str_Angulo (substr (substr str_Angulo 3) 1 (- (strlen (substr str_Angulo 3)) 2)))
				)
			)
			(setq int_Pos1 (vl-string-position (ascii "d") str_Angulo))
			(setq int_Pos2 (vl-string-position (ascii "'") str_Angulo))
			(setq int_Pos3 (vl-string-position 34 str_Angulo))
			(setq int_Pos4 (vl-string-position (ascii ".") str_Angulo))
			(setq str_Grados (substr str_Angulo 1 (1+ int_Pos1)))
			(setq str_Minutos (substr str_Angulo (+ 2 int_Pos1) (- int_Pos2 int_Pos1)))
			(setq str_Segundos (substr str_Angulo (+ 2 int_Pos2) (- int_Pos3 int_Pos2)))
			(if int_Pos4 ;Tiene decimales en los segundos
				(progn
					(if (< (vl-string-position (ascii ".") str_Segundos) 2)
						(setq str_Segundos (strcat "0" str_Segundos))
					)
				)
				(progn
					(if (< (strlen str_Segundos) 3)
						(setq str_Segundos (strcat "0" str_Segundos))
					)
				)
			)
			(if (< (strlen str_Minutos) 3)
				(setq str_Minutos (strcat "0" str_Minutos))
			)
			(setq str_AnguloCorr (strcat str_RY str_Grados str_Minutos str_Segundos str_RX))
		)
	)
	(setq str_AnguloCorr (fn_TraduceAngTxt str_AnguloCorr str_DegresRep))
	(eval 'str_AnguloCorr)
;	(vl-string-subst "O" "W" (vl-string-subst "%%d" "d" (strcat str_RY str_Grados str_Minutos str_Segundos str_RX)))
)

;fn_NumerosEnLetras
;***************************************************************************************************************
;*	Ingresando un numero (en formato string) de longitud máxima 30 retorna una cadena con el texto del numero
;*	Parámetros:
;*		num_Numero = El numero en formato string
;*		int_TipoConv = 	Tipos 0=Masculino; 1=Femenino; 2=Pronombre
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_NumerosEnLetras (str_Numero int_TipoConv / str_Resultado str_ParamToStrEntero fn_Convierte1 fn_Convierte2 fn_Convierte3 fn_Convierte6 fn_ConvierteTodo)
	(defun fn_Convierte1 (int_Numero int_TipoConv / str_Numero lst_NombresIrregulares)
		(setq lst_NombresIrregulares (list "" "uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve"
					"diez" "once" "doce" "trece" "catorce" "quince" "dieciséis" "diecisiete" "dieciocho" "diecinueve"
					"veinte" "veintiuno" "veintidós" "veintitrés" "veinticuatro" "veinticinco" "veintiséis" "veintisiete" "veintiocho" "veintinueve")
		)

		(cond
			((and (= int_Numero 1)(= int_TipoConv 0))
				(setq str_Numero "un")
			)
			((and (= int_Numero 1)(= int_TipoConv 1))
				(setq str_Numero "una")
			)
			(t
				(setq str_Numero (nth int_Numero lst_NombresIrregulares))
			)
		)
		(eval 'str_Numero)
	)

	(defun fn_Convierte2 (int_Numero int_TipoConv / str_Numero lst_NombresIrregulares lst_Decenas)
		(setq lst_NombresIrregulares (list "" "uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve"
					"diez" "once" "doce" "trece" "catorce" "quince" "dieciséis" "diecisiete" "dieciocho" "diecinueve"
					"veinte" "veintiuno" "veintidós" "veintitrés" "veinticuatro" "veinticinco" "veintiséis" "veintisiete" "veintiocho" "veintinueve")
		)
		(setq lst_Decenas (list "" "" "" "treinta" "cuarenta" "cincuenta" "sesenta" "setenta" "ochenta" "noventa"))
		(setq str_Numero "")
		(cond
			((<= int_Numero 9)
				(setq str_Numero (strcat (fn_Convierte1 int_Numero int_TipoConv)))
			)
			((and (= int_Numero 21) (/= int_TipoConv 2))
				(if (= int_TipoConv 0)
						(setq str_Numero (strcat str_Numero "veintiún"))
						(setq str_Numero (strcat str_Numero "veintiuna"))
				)
			)
			((and (>= int_Numero 10) (<= int_Numero 29))
				(setq str_Numero (strcat str_Numero (nth int_Numero lst_NombresIrregulares)))
			)
			(T
				(setq str_Numero (strcat str_Numero (nth (fix (/ int_Numero 10)) lst_Decenas)))
				(if (/= (rem int_Numero 10) 0)
					(progn
						(setq str_Numero (strcat str_Numero " y "))
						(setq str_Numero (strcat str_Numero (fn_Convierte1 (rem int_Numero 10) int_TipoConv)))
					)
				)
			)
		)
		(eval 'str_Numero)
	)

	(defun fn_Convierte3 (int_Numero int_TipoConv / str_Numero lst_NombresIrregulares lst_Decenas lst_Centenas)
		(setq lst_NombresIrregulares (list "" "uno" "dos" "tres" "cuatro" "cinco" "seis" "siete" "ocho" "nueve"
					"diez" "once" "doce" "trece" "catorce" "quince" "dieciséis" "diecisiete" "dieciocho" "diecinueve"
					"veinte" "veintiuno" "veintidós" "veintitrés" "veinticuatro" "veinticinco" "veintiséis" "veintisiete" "veintiocho" "veintinueve")
		)
		(setq lst_Decenas (list "" "" "" "treinta" "cuarenta" "cincuenta" "sesenta" "setenta" "ochenta" "noventa"))
		(setq lst_Centenas (list "" "" "doscient" "trescient" "cuatrocient" "quinient" "seiscient" "setecient" "ochocient" "novecient"))
		(setq str_Numero "")
		(cond
			((<= int_Numero 99)
				(setq str_Numero (strcat str_Numero (fn_Convierte2 int_Numero int_TipoConv)))
			)
			((= int_Numero 100)
				(setq str_Numero (strcat str_Numero "cien"))
			)
			(t
				(if (and (>= int_Numero 101)(<= int_Numero 199))
					(progn
						(setq str_Numero (strcat str_Numero "ciento"))
					)
					(progn
						(setq str_Numero (strcat str_Numero (nth (fix (/ int_Numero 100)) lst_Centenas)))
						(setq str_Numero (strcat str_Numero (if (= int_TipoConv 1) "as" "os")))
					)
				)
				(if (> (rem int_Numero 100) 0)
					(progn
						(setq str_Numero (strcat str_Numero " "))
						(setq str_Numero (strcat str_Numero (fn_Convierte2 (rem int_Numero 100) int_TipoConv)))
					)
				)
			)
		)
		(eval 'str_Numero)
	)

	(defun fn_Convierte6 (int_Numero int_TipoConv / str_Numero int_TresPrimeros int_TresUltimos)
		(setq int_TresPrimeros (/ int_Numero 1000))
		(setq int_TresUltimos (rem int_Numero 1000))
		(setq str_Numero "")
		(cond
			((= int_TresPrimeros 1)
				(setq str_Numero (strcat str_Numero "mil"))
			)
			((>= int_TresPrimeros 2)
				(if (= int_TipoConv 3)
					(setq str_Numero (strcat str_Numero (fn_Convierte3 int_TresPrimeros 0)))
					(setq str_Numero (strcat str_Numero (fn_Convierte3 int_TresPrimeros int_TipoConv)))
				)
				(setq str_Numero (strcat str_Numero " mil"))
			)
		)
		(if (> int_TresUltimos 0)
			(progn
				(if (> int_TresPrimeros 0)
					(setq str_Numero (strcat str_Numero " "))
				)
				(setq str_Numero (strcat str_Numero (fn_Convierte3 int_TresUltimos int_TipoConv)))
			)
		)
		(eval 'str_Numero)
	)

	(defun fn_ConvierteTodo (str_Numero int_TipoConv / int_Cuenta int_Cuentamillones str_Resultado int_Inicio
													int_Longitud str_Temp int_i6 str_Resultado lst_NombreMillonesPlural lst_NombreMillonesSingular)
		(setq lst_NombreMillonesSingular (list "" "millón" "billón" "trillón" "cuatrillón"))
		(setq lst_NombreMillonesPlural (list "" "millones" "billones" "trillones" "cuatrillones"))
		(setq int_Cuenta (strlen str_Numero))
		(setq int_Cuentamillones 0)
		(setq str_Resultado "")
		(while (> int_Cuenta 0)
			(setq int_Inicio (if (>= (- int_Cuenta 6) 0) (- int_Cuenta 5) 1))
			(setq int_Longitud (if (> 6 int_Cuenta) int_Cuenta 6))
			(setq str_Temp (substr str_Numero int_Inicio int_Longitud))
			(setq int_i6 (atoi str_Temp))
			(if (and (> int_Cuentamillones 0) (> int_i6 0))
				(progn
					(if (> (strlen str_Resultado) 0)
						(setq str_Resultado (strcat " " str_Resultado))
					)
					(if (> int_i6 1)
						(setq str_Resultado (strcat (nth int_Cuentamillones lst_NombreMillonesPlural) str_Resultado))
						(setq str_Resultado (strcat (nth int_Cuentamillones lst_NombreMillonesSingular) str_Resultado))
					)
					(setq str_Resultado (strcat " " str_Resultado))
				)
			)
			(setq str_Resultado (strcat (fn_Convierte6 int_i6 int_TipoConv) str_Resultado))
			(if (= int_Cuentamillones 0)
				(setq int_TipoConv 0)
			)
			(setq int_Cuentamillones (1+ int_Cuentamillones))
			(setq int_Cuenta (- int_Cuenta 6))
		)
		(eval 'str_Resultado)
	)

	(setq str_ParamToStrEntero str_Numero)
	(if (> (strlen str_ParamToStrEntero) 30)
		(setq str_Resultado "Número demasiado grande. Llega hasta 30 cifras.")
		(setq str_Resultado (fn_ConvierteTodo str_ParamToStrEntero int_TipoConv))
	)
;	Primer caracter en mayusculas
	(setq str_Resultado (strcat (strcase (substr str_Resultado 1 1)) (substr str_Resultado 2)))
	(eval 'str_Resultado)
)

;fn_ValorCampo
;***************************************************************************************************************
;*	Devuelve el valor de un campo para la lista resultado de una consulta realizada con ADOLISP
;*	Parámetros:
;*		lst_ResultadoConsulta = La lista con los resultados de la consulta y el nombre de los campos
;*		str_Campo = El nombre del campo a devolver el valor
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_ValorCampo (lst_ResultadoConsulta str_Campo int_Registro / val_Ret)
	(if (nth (1+ int_Registro) lst_ResultadoConsulta)
		(setq val_Ret (nth (vl-position str_Campo (car lst_ResultadoConsulta)) (nth (1+ int_Registro) lst_ResultadoConsulta)))
		(setq val_Ret nil))
	val_Ret
)

;fn_ValorCampo1
;***************************************************************************************************************
;*	Devuelve el valor de un campo para la lista resultado de una consulta realizada con ADOLISP
;*	Parámetros:
;*		lst_ResultadoConsulta = La lista con los resultados de la consulta y el nombre de los campos
;*		str_Campo = El nombre del campo a devolver el valor
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_ValorCampo1 (lst_ResultadoConsulta str_Campo int_Registro SiNill / val_Return)
	(setq val_Return (nth (vl-position str_Campo (car lst_ResultadoConsulta)) (nth (1+ int_Registro) lst_ResultadoConsulta)))
	(if (not val_Return)
;		(progn
			(setq val_Return SiNill)
;			(princ (strcat "Entra por nil para " str_Campo "\n"))
;		)
	)
	(eval 'val_Return)
)

(defun fn_SeparaMiles (str_Numero / str_StringFormato int_Cont int_Rem)
	(setq int_Cont (/ (strlen str_Numero) 3))
	(setq int_Rem (rem (strlen str_Numero) 3))
	(setq str_StringFormato (substr str_Numero 1 int_Rem))
	(setq str_Numero (substr str_Numero (1+ int_Rem)))
	(repeat int_Cont
		(if (= (strlen str_StringFormato) 0)
			(setq str_StringFormato (substr str_Numero 1 3))
			(setq str_StringFormato (strcat str_StringFormato "." (substr str_Numero 1 3)))
		)
		(setq str_Numero (substr str_Numero 4))
	)
	(eval 'str_StringFormato)
)

(defun fn_NotacionNumConPunto(val_AConvertir int_CantDecimales / str_ParteDecimal str_ParteEntera str_NoNumerico str_NumeroConvertido)
	(cond
		((= (type val_AConvertir) "REAL")
			(setq str_ParteDecimal (itoa (fix (* (- val_AConvertir (fix val_AConvertir)) (expt 10 int_CantDecimales)))))
			(setq str_ParteEntera (itoa (fix val_AConvertir)))
			(setq str_NumeroConvertido (strcat (fn_SeparaMiles str_ParteEntera) "," str_ParteDecimal))
		)
		((= (type val_AConvertir) "INT")
			(setq str_NumeroConvertido (fn_SeparaMiles (itoa val_AConvertir)))
		)
	)
)

;fn_FechaAccesNow
;***************************************************************************************************************
;*	Devuelve el valor de la fecha de ahora (now) con el formato completo para access
;*	Parámetros:
;*		NINGUNO
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaAccesNow ( / num_Fecha int_Anio int_Mes int_Dia str_Mes str_Dia)

	(setq num_Fecha (getvar "CDATE"))
	(setq int_Anio (fix (/ (fix num_Fecha) 10000)))
	(setq int_Mes (- (/ (fix num_Fecha) 100) (* int_Anio 100)))
	(setq int_Dia (- (fix num_Fecha) (+ (* int_Anio 10000) (* int_Mes 100))))
	
	(if (< int_Mes 10)
		(setq str_Mes (strcat "0" (itoa int_Mes)))
		(setq str_Mes (itoa int_Mes)))

	(if (< int_Dia 10)
		(setq str_Dia (strcat "0" (itoa int_Dia)))
		(setq str_Dia (itoa int_Dia)))
			
	(strcat "{d'" (itoa int_Anio) "-" str_Mes "-" str_Dia "'}")
)

;fn_FechaEspanolToSystem
;***************************************************************************************************************
;*	Devuelve el valor de la fecha con el formato completo para access
;*	Parámetros:
;*		str_FechaAcces = La Fecha según Sql
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaEspanolToSystem (str_FechaEspanol / str_Anio str_Mes str_Dia str_FSys str_SSys lst_ConfFecha
																							lst_FechaSystem str_FechaSystem str_Parte)

	(setq lst_StrMesesEngS (list "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	(setq lst_StrMesesEspS (list "" "Ene" "Feb" "Mar" "Abr" "May" "Jun" "Jul" "Ago" "Sep" "Oct" "Nov" "Dic"))
	(setq lst_StrMesesEngL (list "" "January" "Febreruary" "March" "April" "May" "Jun" "July" "August" "September" "October" "November" "December"))
	(setq lst_StrMesesEspL (list "" "Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" "Julio" "Agosto" "Septiembre" "Octubre" "Noviembre" "Diciembre"))

	(setq str_FSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sShortDate"))
	(setq str_SSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sDate"))

	(if (or (= str_FechaEspanol "") (= str_FechaEspanol nil))
			(setq str_FechaEspanol (strcat "01" str_SSys "01" str_SSys "1900")))

	(setq lst_ConfFecha (fn_CadCDToLista1 str_FSys str_SSys))
	(setq lst_FechaSystem (fn_CadCDToLista1 str_FechaEspanol str_SSys))
	(setq str_Anio (caddr lst_FechaSystem))
	(setq str_Mes (cadr lst_FechaSystem))
	(setq str_Dia (car lst_FechaSystem))

	(setq lst_FechaSystem nil)
	(foreach str_Parte lst_ConfFecha
		(cond
			((or (= str_Parte "d")(= str_Parte "dd"))
				(setq lst_FechaSystem (append lst_FechaSystem (list str_Dia))))
			((or (= (substr str_Parte 1 1) "a") (= (substr str_Parte 1 1) "y"))
				(setq lst_FechaSystem (append lst_FechaSystem (list str_Anio))))
			((= (substr str_Parte 1 1) "M")
				(if (= str_Parte "MMM")
					(setq lst_FechaSystem (append lst_FechaSystem (list (nth (atoi str_Mes) lst_StrMesesEngS))))
					(setq lst_FechaSystem (append lst_FechaSystem (list str_Mes)))))))

	(setq str_FechaSystem "")
	(foreach str_Parte lst_FechaSystem
		(setq str_FechaSystem (strcat str_FechaSystem str_Parte str_SSys)))
		
	(substr str_FechaSystem 1 (1- (strlen str_FechaSystem)))
)

;fn_FechaAccesToSystem
;***************************************************************************************************************
;*	Devuelve el valor de la fecha con el formato completo para access
;*	Parámetros:
;*		str_FechaAcces = La Fecha según Sql
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaAccesToSystem (str_FechaAcces / str_Anio str_Mes str_Dia str_FSys str_SSys lst_ConfFecha
																							lst_FechaSystem str_FechaSystem str_Parte)

	(setq lst_StrMesesEngS (list "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	(setq lst_StrMesesEspS (list "" "Ene" "Feb" "Mar" "Abr" "May" "Jun" "Jul" "Ago" "Sep" "Oct" "Nov" "Dic"))
	(setq lst_StrMesesEngL (list "" "January" "Febreruary" "March" "April" "May" "Jun" "July" "August" "September" "October" "November" "December"))
	(setq lst_StrMesesEspL (list "" "Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" "Julio" "Agosto" "Septiembre" "Octubre" "Noviembre" "Diciembre"))

	(setq str_FSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sShortDate"))
	(setq str_SSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sDate"))

	(if (or (= str_FechaAcces "") (= str_FechaAcces nil))
			(setq str_FechaAcces (strcat "01" str_SSys "01" str_SSys "1900")))

	(setq lst_ConfFecha (fn_CadCDToLista1 str_FSys str_SSys))
	(setq lst_FechaSystem (fn_CadCDToLista1 str_FechaAcces "/"))
	(setq str_Anio (caddr lst_FechaSystem))
	(setq str_Mes (car lst_FechaSystem))
	(setq str_Dia (cadr lst_FechaSystem))

	(setq lst_FechaSystem nil)
	(foreach str_Parte lst_ConfFecha
		(cond
			((or (= str_Parte "d")(= str_Parte "dd"))
				(setq lst_FechaSystem (append lst_FechaSystem (list str_Dia))))
			((or (= (substr str_Parte 1 1) "a") (= (substr str_Parte 1 1) "y"))
				(setq lst_FechaSystem (append lst_FechaSystem (list str_Anio))))
			((= (substr str_Parte 1 1) "M")
				(if (= str_Parte "MMM")
					(setq lst_FechaSystem (append lst_FechaSystem (list (nth (atoi str_Mes) lst_StrMesesEngS))))
					(setq lst_FechaSystem (append lst_FechaSystem (list str_Mes)))))))

	(setq str_FechaSystem "")
	(foreach str_Parte lst_FechaSystem
		(setq str_FechaSystem (strcat str_FechaSystem str_Parte str_SSys)))
		
	(substr str_FechaSystem 1 (1- (strlen str_FechaSystem)))
)

;fn_FechaSystemToAcces
;***************************************************************************************************************
;*	Devuelve el valor de la fecha con el formato completo para access
;*	Parámetros:
;*		str_FechaSystem = La Fecha según el sistema
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaSystemToAcces (str_FechaSystem / int_Anio int_Mes int_Dia lst_StrMesesEng str_FSys str_SSys lst_ConfFecha
																				lst_FechaSystem str_FechaAcces str_Parte int_Indice)

	(setq lst_StrMesesEng (list "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	(setq lst_StrMesesEsp (list "" "Ene" "Feb" "Mar" "Abr" "May" "Jun" "Jul" "Ago" "Sep" "Oct" "Nov" "Dec"))
	(setq str_FSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sShortDate"))
	(setq str_SSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sDate"))

	(if (or (= str_FechaSystem "") (= str_FechaSystem nil))
			(setq str_FechaSystem (strcat "01" str_SSys "01" str_SSys "1900")))

	(setq lst_ConfFecha (fn_CadCDToLista1 str_FSys str_SSys))
	(setq lst_FechaSystem (fn_CadCDToLista1 str_FechaSystem str_SSys))

	(setq int_Indice 0)
	(foreach str_Parte lst_ConfFecha
		(cond
			((= str_Parte "dd")
				(setq int_Dia (atoi (nth int_Indice lst_FechaSystem))))
			((= str_Parte "d")
				(setq int_Dia (atoi (nth int_Indice lst_FechaSystem))))
			((= str_Parte "MMM")
				(setq int_Mes (vl-position (nth int_Indice lst_FechaSystem) lst_StrMesesEng))
				(if (not int_Mes)
					(setq int_Mes (vl-position (nth int_Indice lst_FechaSystem) lst_StrMesesEsp))))
			((= str_Parte "MM")
				(setq int_Mes (atoi (nth int_Indice lst_FechaSystem))))
			((= str_Parte "M")
				(setq int_Mes (atoi (nth int_Indice lst_FechaSystem))))
			((= str_Parte "yyyy")
				(setq int_Anio (atoi (nth int_Indice lst_FechaSystem))))
			((= str_Parte "yy")
				(setq int_Anio (atoi (nth int_Indice lst_FechaSystem)))))

		(setq int_Indice (1+ int_Indice))
	)

	(fn_FechaAcces int_Dia int_Mes int_Anio)
)

;fn_FechaAcces
;***************************************************************************************************************
;*	Devuelve el valor de la fecha con el formato completo para access
;*	Parámetros:
;*		int_Dia = El numero del Dia
;*		int_Mes = El numero del Mes
;*		int_Anio = El numero del año
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaAcces (int_Dia int_Mes int_Anio / str_Dia str_Mes)
;	(setq lst_StrMesesEng (list "" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
;	(strcat "(DateValue('" (nth int_Mes lst_StrMesesEng) " " (itoa int_Dia) ", " (itoa int_Anio) "'))")

	(if (< int_Mes 10)
		(setq str_Mes (strcat "0" (itoa int_Mes)))
		(setq str_Mes (itoa int_Mes)))

	(if (< int_Dia 10)
		(setq int_Dia (strcat "0" (itoa int_Dia)))
		(setq int_Dia (itoa int_Dia)))

	(strcat "{d'" (itoa int_Anio) "-" str_Mes "-" int_Dia "'}")
)

;fn_FechaLarga
;***************************************************************************************************************
;*	Devuelve el valor de la fecha con el formato completo para access
;*	Parámetros:
;*		int_Dia = El numero del Dia
;*		int_Mes = El numero del Mes
;*		int_Anio = El numero del año
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaLarga (int_Dia int_Mes int_Anio / str_FechaAcces lst_StrMesesEng)
	(setq lst_StrMesesEsp
		(list "" "Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" "Julio" "Agosto" "Setiembre" "Octubre" "Noviembre" "Diciembre"))
	(if (= int_Dia 0)
		(strcat (nth int_Mes lst_StrMesesEng) " DE " (itoa int_Anio))
		(strcat (itoa int_Dia) " DE " (nth int_Mes lst_StrMesesEng) " DE " (fn_NotacionNumConPunto (itoa int_Anio) 0))
	)
)

;fn_FechaCaratulaNow
;***************************************************************************************************************
;*	Devuelve el valor de la fecha de hoy con el formato de la caratula
;*	Parámetros:
;*		int_Dia = El numero del Dia
;*		int_Mes = El numero del Mes
;*		int_Anio = El numero del año
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaCaratulaNow ( / str_FechaAcces num_Fecha int_Anio int_Mes int_Dia lst_StrMesesEng str_Dia str_Mes)
	(setq lst_StrMesesEsp
		(list "" "Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" "Julio" "Agosto" "Setiembre" "Octubre" "Noviembre" "Diciembre"))

	(setq num_Fecha (getvar "CDATE"))
	(setq int_Anio (fix (/ (fix num_Fecha) 10000)))
	(setq int_Mes (- (/ (fix num_Fecha) 100) (* int_Anio 100)))
	(setq int_Dia (- (fix num_Fecha) (+ (* int_Anio 10000) (* int_Mes 100))))
	(strcat (nth int_Mes lst_StrMesesEsp) " de " (itoa int_Anio))
)

(defun fn_FechaNow ( / num_Fecha int_Anio int_Mes int_Dia lst_StrMesesEng)
	(setq num_Fecha (getvar "CDATE"))
	(setq int_Anio (fix (/ (fix num_Fecha) 10000)))
	(setq int_Mes (- (/ (fix num_Fecha) 100) (* int_Anio 100)))
	(setq int_Dia (- (fix num_Fecha) (+ (* int_Anio 10000) (* int_Mes 100))))
	(if (< int_Dia 10)
		(setq str_Dia (strcat "0" (itoa int_Dia)))
		(setq str_Dia (itoa int_Dia)))
	(if (< int_Mes 10)
		(setq str_Mes (strcat "0" (itoa int_Mes)))
		(setq str_Mes (itoa int_Mes)))
		
	(strcat str_Dia "/" str_Mes "/" (itoa int_Anio))
)

(defun fn_FechaSystemNow ( / num_Fecha int_Anio int_Mes int_Dia lst_StrMesesEng str_FSys str_SSys lst_ConfFecha
													str_Parte str_FechaReturn)
	(setq str_FSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sShortDate"))
	(setq str_SSys (vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International\\" "sDate"))

	(setq lst_ConfFecha (fn_CadCDToLista1 str_FSys str_SSys))

	(setq num_Fecha (getvar "CDATE"))
	(setq int_Anio (fix (/ (fix num_Fecha) 10000)))
	(setq int_Mes (- (/ (fix num_Fecha) 100) (* int_Anio 100)))
	(setq int_Dia (- (fix num_Fecha) (+ (* int_Anio 10000) (* int_Mes 100))))
	(if (< int_Dia 10)
		(setq str_Dia (strcat "0" (itoa int_Dia)))
		(setq str_Dia (itoa int_Dia)))
	(if (< int_Mes 10)
		(setq str_Mes (strcat "0" (itoa int_Mes)))
		(setq str_Mes (itoa int_Mes)))

	(setq str_FechaReturn "")

	(foreach str_Parte lst_ConfFecha
		(cond
			((or (= str_Parte "dd") (= str_Parte "d"))
				(setq str_FechaReturn (strcat str_FechaReturn str_Dia str_SSys)))

			((or (= str_Parte "MMM") (= str_Parte "MM") (= str_Parte "M"))
				(setq str_FechaReturn (strcat str_FechaReturn str_Mes str_SSys)))

			((or (= str_Parte "yyyy") (= str_Parte "yy"))
				(setq str_FechaReturn (strcat str_FechaReturn (itoa int_Anio) str_SSys)))))

	(substr str_FechaReturn 1 (1- (strlen str_FechaReturn)))
)

;fn_FechaEngToEsp
;***************************************************************************************************************
;*	Devuelve el valor de la fecha en formato espaniol "DD/MM/AAAA"
;*	Parámetros:
;*		str_FechaEng = La Fecha con formato MM/DD/AAAA
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaEngToEsp (str_FechaEng / str_Dia str_Mes str_Anio)
	(setq str_Mes (substr str_FechaEng 1 2))
	(setq str_Dia (substr str_FechaEng 4 2))
	(setq str_Anio (substr str_FechaEng 7 4))
	(strcat str_Dia "/" str_Mes "/" str_Anio)
)

;fn_FechaCadStr
;***************************************************************************************************************
;*	Devuelve el valor de la fecha (el día de hoy) en formato espaniol "DD/MM/AAAA"
;*	Parámetros:
;*		NINGUNO
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_FechaCadStr ( / num_Fecha int_Anio int_Mes int_Dia str_Fecha)
	(setq num_Fecha (getvar "CDATE"))
	(setq int_Anio (fix (/ (fix num_Fecha) 10000)))
	(setq int_Mes (- (/ (fix num_Fecha) 100) (* int_Anio 100)))
	(setq int_Dia (- (fix num_Fecha) (+ (* int_Anio 10000) (* int_Mes 100))))
	
	(if (< int_Dia 10)
		(setq str_Fecha (strcat "0" (itoa int_Dia) "/"))
		(setq str_Fecha (strcat (itoa int_Dia) "/")))

	(if (< int_Mes 10)
		(setq str_Fecha (strcat str_Fecha "0" (itoa int_Mes) "/"))
		(setq str_Fecha (strcat str_Fecha (itoa int_Mes) "/")))

	(setq str_Fecha (strcat str_Fecha (itoa int_Anio)))

	(eval 'str_Fecha)
)

;fn_ConvierteGeometria
;***************************************************************************************************************
;*	Convierte la lista de coordenadas de planas a geográficas usando el proj4 y llamando a un shell
;*	Parámetros:
;*		lst_Geomet = Toda la definición de las geometrías
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_ConvierteGeometria (lst_Geometrias / lst_Poligonos lst_Vertices lst_Vertice stm_ArchivoConv str_NomFile
														str_NomFile1 lst_ParamsConv int_Faja str_Cadena lst_Convertidos int_Cont)
	(setq lst_ParamsConv (list "+proj=tmerc +lat_0=90s +lon_0=58d46'51.05w +x_0=5616686.69 +y_0=0 +k=1 +ellps=WGS84"
														"+proj=tmerc +lat_0=90s +lon_0=72w +x_0=1500000 +y_0=0 +k=1 +ellps=WGS84"
														"+proj=tmerc +lat_0=90s +lon_0=69w +x_0=2500000 +y_0=0 +k=1 +ellps=WGS84"
														"+proj=tmerc +lat_0=90s +lon_0=66w +x_0=3500000 +y_0=0 +k=1 +ellps=WGS84"
														"+proj=tmerc +lat_0=90s +lon_0=63w +x_0=4500000 +y_0=0 +k=1 +ellps=WGS84"
														"+proj=tmerc +lat_0=90s +lon_0=60w +x_0=5500000 +y_0=0 +k=1 +ellps=WGS84"
														"+proj=tmerc +lat_0=90s +lon_0=57w +x_0=6500000 +y_0=0 +k=1 +ellps=WGS84"
														"+proj=tmerc +lat_0=90s +lon_0=54w +x_0=7500000 +y_0=0 +k=1 +ellps=WGS84"))

	(setq lst_Poligonos (cadr lst_Geometrias))
	(setq lst_Vertices (car lst_Geometrias))
;	Escribir Coordenadas a un archivo
	(setq str_NomFile (strcat (getvar "DWGPREFIX") "aconv.tmp"))
	(setq str_NomFile1 (strcat (getvar "DWGPREFIX") "conv.tmp"))
	(setq stm_ArchivoConv (open str_NomFile "w"))

	(foreach lst_Vertice lst_Vertices
		(setq int_Faja (fix (/ (cadr (caddr lst_Vertice)) 1000000.0)))
		(write-line
			(strcat
				(rtos (cadr (caddr lst_Vertice)) 2 8) (chr 9)
				(rtos (caddr (caddr lst_Vertice)) 2 8)
			) stm_ArchivoConv)
	)
	(close stm_ArchivoConv)
	;(princ (strcat "c:\\proj\\proj -I -W8 " (nth int_Faja lst_ParamsConv) " " str_NomFile " > " str_NomFile1))
	(command "_shell" (strcat "c:\\proj\\bin\\proj -I -W8 " (nth int_Faja lst_ParamsConv) " " str_NomFile " > " str_NomFile1))
	(princ)
	(setq stm_ArchivoConv (open str_NomFile1 "r"))
	(while (setq str_Cadena (read-line stm_ArchivoConv))
		(read-line stm_ArchivoConv)
;		(princ str_Cadena) (princ "\n")
		;(princ (vl-string->list str_Cadena)) (princ "\n")
		(setq lst_Cadena (fn_CadCDToLista str_Cadena (chr 9)))
;		(princ lst_Cadena) (princ "\n")
;		(princ (caddr lst_Cadena)) (princ "\n")
;		(princ (cons "LONGITUD" (fn_TraduceAngTxt (car lst_Cadena) (chr 176)))) (princ "\n")
;		(princ (cons "LATITUD" (fn_TraduceAngTxt (cadr lst_Cadena) (chr 176))))
		(setq lst_Convertidos (append lst_Convertidos (list (list
																									(cons "LONGITUD" (fn_TraduceAngTxt (car lst_Cadena) (chr 176)))
																									(cons "LATITUD" (fn_TraduceAngTxt (cadr lst_Cadena) (chr 176)))))))
;		(princ (strcat "Longitud= " (fn_TraduceAngTxt (car lst_Cadena) (chr 176)) "; Latitud= " (fn_TraduceAngTxt (cadr lst_Cadena) (chr 176)))) (princ "\n")
	)
	(close stm_ArchivoConv)
;	(princ lst_Convertidos)
	(setq int_Cont 0)
	(while (< int_Cont (length lst_Vertices))
;		(princ (append (nth int_Cont lst_Vertices) (list (nth int_Cont lst_Convertidos)))) (princ "\n")
;		(princ (nth int_Cont lst_Vertices)) (princ "\n")
		(setq lst_Vertices
			(subst
				(append (nth int_Cont lst_Vertices) (list (nth int_Cont lst_Convertidos)))
				(nth int_Cont lst_Vertices)
				lst_Vertices))
		(setq int_Cont (1+ int_Cont))
	)
	(princ (list lst_Vertices lst_Poligonos))
;proj -I -W8 +proj=tmerc +lat_0=90s +lon_0=57w +x_0=6500000 +y_0=0 +k=1 +ellps=WGS84 c:\\proj\\aconv.txt c:\\proj\\conv.txt
;	Ejecutar el comando de autocad que corre el proj4
; Reeler el archivo con las coordenadas convertidas
)

;fn_ConvierteCoords
;***************************************************************************************************************
;*	Convierte la lista de coordenadas de planas a geográficas usando el proj4 y llamando a un shell
;*	Parámetros:
;*		lst_Geomet = Toda la definición de las geometrías
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_ConvierteCoords (lst_Vertices / lst_Vertice stm_ArchivoConv stm_ArchivoAConv str_NomFile bool_Bucle
														str_NomFile1 lst_ParamsConv int_Faja str_Cadena lst_Convertidos int_Cont)

	(setq lst_ParamsConv (list "+proj=tmerc +lat_0=90s +lon_0=58d46'51.05w +x_0=5616686.69 +y_0=0 +k=1 +ellps=WGS84"
														 "+proj=tmerc +lat_0=90s +lon_0=72w +x_0=1500000 +y_0=0 +k=1 +ellps=WGS84"
														 "+proj=tmerc +lat_0=90s +lon_0=69w +x_0=2500000 +y_0=0 +k=1 +ellps=WGS84"
														 "+proj=tmerc +lat_0=90s +lon_0=66w +x_0=3500000 +y_0=0 +k=1 +ellps=WGS84"
														 "+proj=tmerc +lat_0=90s +lon_0=63w +x_0=4500000 +y_0=0 +k=1 +ellps=WGS84"
														 "+proj=tmerc +lat_0=90s +lon_0=60w +x_0=5500000 +y_0=0 +k=1 +ellps=WGS84"
														 "+proj=tmerc +lat_0=90s +lon_0=57w +x_0=6500000 +y_0=0 +k=1 +ellps=WGS84"
														 "+proj=tmerc +lat_0=90s +lon_0=54w +x_0=7500000 +y_0=0 +k=1 +ellps=WGS84"))

;	Escribir Coordenadas a un archivo
	(setq str_NomFile (strcat (getvar "DWGPREFIX") "aconv.tmp"))
	(setq str_NomFile1 (strcat (getvar "DWGPREFIX") "conv.tmp"))
	(if (findfile str_NomFile)
		(vl-file-delete str_NomFile)
	)
	(if (findfile str_NomFile1)
		(vl-file-delete str_NomFile1)
	)

	(setq obj_Acad (vlax-get-acad-object) obj_Doc (vla-get-activedocument obj_Acad))
	(setq int_Cont 0)
	(foreach lst_Vertice lst_Vertices
		(setq int_Faja (fix (/ (car lst_Vertice) 1000000.0)))
		(setq stm_ArchivoAConv (open str_NomFile "w"))
		(write-line (strcat (rtos (car lst_Vertice) 2 8) (chr 9) (rtos (cadr lst_Vertice) 2 8) (chr 9) (caddr lst_Vertice)) stm_ArchivoAConv)
		(close stm_ArchivoAConv)

		(if (findfile str_NomFile1)
			(vl-file-delete str_NomFile1)
		)

		(vla-sendcommand obj_Doc (strcat "_shell\nc:\\proj\\bin\\proj -I -W4 " (nth int_Faja lst_ParamsConv) " \"" str_NomFile "\" > \"" str_NomFile1 "\"\n"))
;		Timer de espera que se cree el archivo
		(while (not (findfile str_NomFile1))
			(princ))

;		Timer de espera que Proj.4 cargue valores al archivo
		(setq bool_Bucle T)
		(while bool_Bucle
			(setq stm_ArchivoConv (open str_NomFile1 "r"))
			(setq str_Cadena (read-line stm_ArchivoConv))
			(close stm_ArchivoConv)
			(if str_Cadena
				(progn
					(if (< (strlen str_Cadena) 3)
						(setq bool_Bucle T)
						(setq bool_Bucle nil))))
		)
		(setq lst_Cadena (fn_CadCDToLista str_Cadena (chr 9)))
		(setq lst_Convertidos (append lst_Convertidos (list (list
																	(cons "LONGITUD" (fn_TraduceAngTxt (car lst_Cadena) (chr 176)))
																	(cons "LATITUD" (fn_TraduceAngTxt (cadr lst_Cadena) (chr 176)))
																	(cons "DESCRIPCION" (caddr lst_Cadena))))))
		(setq int_Cont (1+ int_Cont))
	)
	(vlax-release-object obj_Doc)
	(vlax-release-object obj_Acad)
	(eval 'lst_Convertidos)
)

;(fn_ConvierteCoords '((6414684.5827 6581727.4852 "1") (6414687.7441 6581746.5246 "2") (6414684.5827 6581727.4852 "3") (6414684.5827 6581727.4852 "4") (6414684.5827 6581727.4852 "5")))

;fn_PuntoEnPoligono (Algoritmo Radial)
;***************************************************************************************************************
;*	Analiza la pertenencia de una punto dentro de un poligono utilizando el algoritmo "Radial"
;*	Devuelve T si esta dentro; nil si no lo está. En caso de error devuelve 0.
;*
;*	Parámetros:
;*		lst_Vertices = La lista de vertices del poligono
;*		lst_Pto = El punto del cual se analizará la pertenencia o no dentro del poligono
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_PuntoEnPoligono (lst_Vertices lst_Pto / num_DAng num_SumAng int_Cont num_AngActual val_Return)
	(setq num_SumAng 0)
	(setq int_Cont 0)

;	Verificar cierre (inicio distinto de fin)
	(if (equal (car lst_Vertices) (last lst_Vertices) 0.001)
		(setq lst_Vertices (reverse (cdr (reverse lst_Vertices))))
	)

	(while (< int_Cont (length lst_Vertices))
		(if (= int_Cont (1- (length lst_Vertices)))
			(setq num_AngActual (fn_AnguloInternoV1 (nth int_Cont lst_Vertices) lst_Pto (car lst_Vertices)))
			(setq num_AngActual (fn_AnguloInternoV1 (nth int_Cont lst_Vertices) lst_Pto (nth (1+ int_Cont) lst_Vertices))))

		(if (> num_AngActual pi)
			(setq num_AngActual (- num_AngActual (* pi 2))))
		(if (< num_AngActual (- pi))
			(setq num_AngActual (+ (* pi 2) num_AngActual)))

		(setq num_SumAng (+ num_SumAng num_AngActual))
		(setq int_Cont (1+ int_Cont))
	)

	(cond
		((or (equal num_SumAng (* pi 2) 0.0001) (equal num_SumAng (- (* pi 2)) 0.0001))
			(setq val_Return T)
		)
		((equal num_SumAng 0.0 0.0001)
			(setq val_Return nil)
		)
		(T
			(setq val_Return 0001)
		)
	)
	(eval 'val_Return)
)

;fn_PuntoEnSegmento
;***************************************************************************************************************
;*	Analiza la pertenencia de una punto dentro de una secuencia de Segmentos
;*	Devuelve T si esta dentro; nil si no lo está.
;*
;*	Parámetros:
;*		lst_Vertices = La lista de vertices de los segmentos
;*		lst_Pto = El punto del cual se analizará la pertenencia
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_PuntoEnSegmentos (lst_Vertices lst_Pto / lst_Vertice num_AngVert num_AngPto num_DistVert num_DistPto int_Cont val_Return)
	(setq int_Cont 0)
	(setq val_Return nil)
	
	(foreach lst_Vertice lst_Vertices
		(princ "Comprueba Primera Etapa")(princ "\n")
		(if (equal lst_Vertice lst_Pto 0.0001)
			(setq val_Return T))
	)

	(while (and (< int_Cont (1- (length lst_Vertices))) (not val_Return))
		(princ "Comprueba Segunda Etapa")(princ "\n")
		(setq num_AngVert (angle (nth int_Cont lst_Vertices) (nth (1+ int_Cont) lst_Vertices)))
		(setq num_DistVert (distance (nth int_Cont lst_Vertices) (nth (1+ int_Cont) lst_Vertices)))
		(setq num_AngPto (angle (nth int_Cont lst_Vertices) lst_Pto))
		(setq num_DistPto (distance (nth int_Cont lst_Vertices) lst_Pto))
		(if (and (equal num_AngVert num_AngPto 0.000001) (<= num_DistPto num_DistVert))
				(setq val_Return T))
		(setq int_Cont (1+ int_Cont))
	)
	(eval 'val_Return)
)

;fn_OrdenaSSetByLayer
;***************************************************************************************************************
;*	Ordena Alfabeticamente segun el layer una selection set
;*
;*	Parámetros:
;*		sset_AOrdenar = El selection set a ordenar
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_OrdenaSSetByLayer (sset_AOrdenar / int_Cont str_NomLayer lst_NomLayer sset_Ordenada)
	(setq int_Cont 0)
	(while (< int_Cont (sslength sset_AOrdenar))
		(setq lst_NomLayer (append lst_NomLayer (list (cdr (assoc 8 (entget (ssname sset_AOrdenar int_Cont)))))))
		(setq int_Cont (1+ int_Cont))
	)
	(setq lst_NomLayer (acad_strlsort lst_NomLayer))

;	(setq int_Cont 0)
	(setq sset_Ordenada (ssadd))
	(foreach str_NomLayer lst_NomLayer
		(setq int_Cont 0)
		(while (< int_Cont (sslength sset_AOrdenar))
;			(princ (cdr (assoc 8 (entget (ssname sset_AOrdenar int_Cont)))))
;			(princ str_NomLayer)
			(if (= (cdr (assoc 8 (entget (ssname sset_AOrdenar int_Cont)))) str_NomLayer)
				(setq sset_Ordenada (ssadd (ssname sset_AOrdenar int_Cont) sset_Ordenada))
			)
			(setq int_Cont (1+ int_Cont))
		)
	)
	(eval 'sset_Ordenada)
)

;fn_SumaRadialCtroLados (Algoritmo Radial)
;***************************************************************************************************************
;*	analiza el sentido de giro de una polilinia utilizando el algoritmo "Radial"
;*	Devuelve T si esta dentro; nil si no lo está. En caso de error devuelve 0.
;*
;*	Parámetros:
;*		ent_eName = Nombre de la entidad polilinea a procesar
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_SumaRadialCtroLados (ent_eName / lst_Vertices lst_Pto num_DAng num_SumAng int_Cont num_AngActual val_Return)
	(setq num_SumAng 0)
	(setq int_Cont 0)
	(setq lst_Pto (fn_Centroide0 ent_eName))
	(setq lst_Vertices (fn_2DpolyToPointsV2 ent_eName))
;	Verificar cierre (inicio distinto de fin)
	(if (equal (car lst_Vertices) (last lst_Vertices) 0.001)
		(setq lst_Vertices (reverse (cdr (reverse lst_Vertices))))
	)

	(while (< int_Cont (length lst_Vertices))
		(if (= int_Cont (1- (length lst_Vertices)))
			(setq num_AngActual (fn_AnguloInternoV1 (nth int_Cont lst_Vertices) lst_Pto (car lst_Vertices)))
			(setq num_AngActual (fn_AnguloInternoV1 (nth int_Cont lst_Vertices) lst_Pto (nth (1+ int_Cont) lst_Vertices))))

		(if (> num_AngActual pi)
			(setq num_AngActual (- num_AngActual (* pi 2))))
		(if (< num_AngActual (- pi))
			(setq num_AngActual (+ (* pi 2) num_AngActual)))

		(setq num_SumAng (+ num_SumAng num_AngActual))
		(setq int_Cont (1+ int_Cont))
	)

	(princ (angtos num_SumAng 1 4))
;	(eval 'val_Return)
)

;fn_Centroide0 (Algoritmo 0)
;***************************************************************************************************************
;*	Calculo el centro de gravedad de un poligono
;*	Devuelve las coordenadas del centro de gravedad.
;*
;*	Parámetros:
;*		ent_eName = Nombre de la entidad polilinea a procesar
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_Centroide0 (ent_eName / int_Cont lst_Vertices num_SupPoligono num_SumX num_SumY num_Xi num_Xi1 num_Yi num_Yi1)
	(setq int_Cont 0)
	(setq num_SumX 0.0)
	(setq num_SumY 0.0)
	(setq lst_Vertices (fn_2DpolyToPointsV2 ent_eName))
	(setq num_SupPoligono (vla-get-area (vlax-ename->vla-object ent_eName)))
	(if (equal (car lst_Vertices) (last lst_Vertices) 0.001)
		(setq lst_Vertices (reverse (cdr (reverse lst_Vertices))))
	)
	(while (< int_Cont (length lst_Vertices))
		(setq num_Xi (car (nth int_Cont lst_Vertices)))
		(setq num_Yi (cadr (nth int_Cont lst_Vertices)))
		(if (= int_Cont (1- (length lst_Vertices)))
			(progn
				(setq num_Xi1 (car (car lst_Vertices)))
				(setq num_Yi1 (cadr (car lst_Vertices)))
			)
			(progn
				(setq num_Xi1 (car (nth (1+ int_Cont) lst_Vertices)))
				(setq num_Yi1 (cadr (nth (1+ int_Cont) lst_Vertices)))
			)
		)

		(setq num_SumX (+ num_SumX
			(/ (* (- num_Yi num_Yi1) (+ (expt num_Xi 2.0) (* num_Xi num_Xi1) (expt num_Xi1 2.0)))
			(* 6.0 num_SupPoligono))))

		(setq num_SumY (+ num_SumY
			(/ (* (- num_Xi1 num_Xi) (+ (expt num_Yi 2.0) (* num_Yi num_Yi1) (expt num_Yi1 2.0)))
			(* 6.0 num_SupPoligono))))
		(setq int_Cont (1+ int_Cont))
	)
	(eval '(list (abs num_SumX) (abs num_SumY)))
)

;fn_Centroide1 (Algoritmo 1)
;***************************************************************************************************************
;*	Calculo el centro de gravedad de un poligono
;*	Devuelve las coordenadas del centro de gravedad.
;*
;*	Parámetros:
;*		ent_eName = Nombre de la entidad polilinea a procesar
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_Centroide1 (ent_eName / int_Cont lst_Vertices num_SupPoligono num_SumX num_SumY num_Xi num_Xi1 num_Yi num_Yi1)
	(setq int_Cont 0)
	(setq num_SumX 0.0)
	(setq num_SumY 0.0)
	(setq lst_Vertices (fn_2DpolyToPointsV2 ent_eName))
	(setq num_SupPoligono (vla-get-area (vlax-ename->vla-object ent_eName)))
	(if (equal (car lst_Vertices) (last lst_Vertices) 0.001)
		(setq lst_Vertices (reverse (cdr (reverse lst_Vertices))))
	)
	(while (< int_Cont (length lst_Vertices))
		(setq num_Xi (car (nth int_Cont lst_Vertices)))
		(setq num_Yi (cadr (nth int_Cont lst_Vertices)))
		(if (= int_Cont (1- (length lst_Vertices)))
			(progn
				(setq num_Xi1 (car (car lst_Vertices)))
				(setq num_Yi1 (cadr (car lst_Vertices)))
			)
			(progn
				(setq num_Xi1 (car (nth (1+ int_Cont) lst_Vertices)))
				(setq num_Yi1 (cadr (nth (1+ int_Cont) lst_Vertices)))
			)
		)

		(setq num_SumX (+ num_SumX (* (+ num_Xi num_Xi1) (- (* num_Xi num_Yi1) (* num_Xi1 num_Yi)))))
		(setq num_SumY (+ num_SumY (* (+ num_Yi num_Yi1) (- (* num_Xi num_Yi1) (* num_Xi1 num_Yi)))))

		(setq int_Cont (1+ int_Cont))
	)
	(setq num_SumX (/ num_SumX (* 6.0 num_SupPoligono)))
	(setq num_SumY (/ num_SumY (* 6.0 num_SupPoligono)))
	(eval '(list (abs num_SumX) (abs num_SumY)))
)

(defun fn_SubsIndice (int_Elemento val_Nuevo lst_AModif / int_Cont val_Elemento lst_Return)
	(setq int_Cont 0)
	(foreach val_Elemento lst_AModif
		(if (= int_Cont int_Elemento)
			(setq lst_Return (append lst_Return (list val_Nuevo)))
			(setq lst_Return (append lst_Return (list val_Elemento))))
		(setq int_Cont (1+ int_Cont)))
	(eval 'lst_Return)
)

(defun fn_Redondear (num_Valor / )
	(eval '(atof (rtos num_Valor 2 2))))

(defun fn_NroSaltoGrande ()
	(princ)
)

(defun fn_NroPoligono ()
	(princ)
)

(defun fn_TipoTitulo (str_ToChange / lst_Cadenas str_Temp str_Resultado)
	(setq lst_Cadenas (fn_CadCDToLista str_ToChange " "))
	(setq str_Resultado "")
	(foreach str_Temp lst_Cadenas
		(setq str_Resultado (strcat str_Resultado
			(strcase (substr str_Temp 1 1) nil)
			(strcase (substr str_Temp 2) T) " ")))
	(setq str_Resultado (vl-string-right-trim " " str_Resultado))
	(eval 'str_Resultado)
)

(defun fn_ApeNomTT (str_ToChange str_Separador / lst_Cadenas str_Temp str_Resultado)
	(princ)
)

;fn_StringChar
;***************************************************************************************************************
;*	Retorna un string de longitud solicitada del caracter dado
;*
;*	Parámetros:
;*		str_Char = Caracter a completar
;*		int_Long = Longitud Nueva del string
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_StringChar (str_Char int_Long / str_Str)
	(setq str_Str "")
	(repeat int_Long
		(setq str_Str (strcat str_Str str_Char))
	)
)

;fn_CerosIzq
;***************************************************************************************************************
;*	Retorna un string representando un nro entero completado de longitud dada completado con ceros a la izquierda
;*
;*	Parámetros:
;*		int_Nro  = Numero a convertir
;*		int_Long = Longitud Nueva del Número
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_CerosIzq (int_Nro int_Long / )
	(if (< (strlen (itoa int_Nro)) int_Long)
		(strcat (fn_StringChar "0" (- int_Long (strlen (itoa int_Nro)))) (itoa int_Nro))
		(itoa int_Nro)
	)
)

;fn_GiroPoligono
;***************************************************************************************************************
;*	Retorna un un entero con el resultado de calcular el sentido de giro de un poligono.
;*	Si el giro es horario retorna 0, Antihorario retorna 1
;*
;*	Parámetros:
;*		lst_Vertices  = Lista de vertices del poligono a evaluar
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_GiroPoligono (lst_Vertices / num_SumAng int_Giro int_IDVertAct int_IDVertAnt int_IDVertPos int_Cont)
	(setq int_Cont 0)
	(setq num_SumAng 0.0)
	(while (< int_Cont (length lst_Vertices))
		(setq int_IDVertAct (nth int_Cont lst_Vertices))
		(cond
			((= int_Cont 0) ;Es el primero. Anterior es el ultimo
				(setq int_IDVertAnt (last lst_Vertices))
				(setq int_IDVertPos (nth (1+ int_Cont) lst_Vertices))
			)
			((= int_Cont (1- (length lst_Vertices))) ;Es el ultimo. Posterior es el primero
				(setq int_IDVertAnt (nth (1- int_Cont) lst_Vertices))
				(setq int_IDVertPos (car lst_Vertices))
			)
			(T
				(setq int_IDVertAnt (nth (1- int_Cont) lst_Vertices))
				(setq int_IDVertPos (nth (1+ int_Cont) lst_Vertices))
			)
		)

		(setq num_SumAng (+ num_SumAng (fn_AnguloInterno int_IDVertAnt int_IDVertAct int_IDVertPos)))

		(setq int_Cont (1+ int_Cont))
	)
	
	(if (equal num_SumAng (* pi (- (length lst_Vertices) 2)) 0.00001)
		(setq int_Giro 0) ;Horario
		(setq int_Giro 1) ;Antihorario
	)
	(eval 'int_Giro)
)


(defun fn_DayOfYear ( / str_AEval)
	(setq str_AEval (strcat "ThisDrawing.SetVariable \"USERI1\"," "DatePart (\"y\",Now())"))
	(vla-eval (vlax-get-acad-object) str_AEval)
	(princ "\n")
	(princ (getvar "useri1"))
	(princ)
)

;fn_GiroPoligono
;***************************************************************************************************************
;*	Busca un archivo de nombre "str_NomFile" en todo el arbol de directorios a partir de "str_DirArranque"
;*	str_NomFile puede tener caracteres comodín
;*
;*	Parámetros:
;*		str_DirArranque  = Nombre de directorio donde se comenzará la busqueda
;*		str_NomFile = Nombre del archivo a ser buscado, puede tener caracteres comodín
;*
;*	Ejemplos:
;*		(fn_BuscaArchivo "E:\\00_SALTOGRANDE-I09" "*S-*-1.dwg")
;*		(fn_BuscaArchivoRec "E:\00_SALTOGRANDE-I09" "*S-*-1.dwg")
;*		(vl-directory-files "E:\00_SALTOGRANDE-I09" nil -1)
;*		(fn_BuscaArchivo "\\\\172.16.0.25\\d\\001_Agrimensura\\03_Otras empresas\\00_ SALTO GRANDE\\EXPEDIENTES" "*S-839-1*CTM*.dwg")
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_BuscaArchivo (str_DirArranque str_NomFile / lst_FileEncontradas)
;	Función recursiva
	(defun fn_BuscaArchivoRec (str_DirArr str_PatternFile / lst_Dirs str_NomDir lst_FullPath lst_Files)
		(if (/= (substr str_DirArr (strlen str_DirArr) 1) "\\")
			(setq str_DirArr (strcat str_DirArr "\\")))
		(setq lst_Dirs (vl-directory-files str_DirArr nil -1))
		(foreach str_NomDir lst_Dirs
			(if (not (or (= str_NomDir ".") (= str_NomDir "..")))
				(progn
					(setq lst_Files (vl-directory-files str_DirArr str_PatternFile 1))
					(if lst_Files
						(progn
							(setq lst_Files (mapcar '(lambda (str_File) (strcat str_DirArr str_File)) lst_Files))
							(setq lst_FileEncontradas (append lst_FileEncontradas lst_Files))))
					(fn_BuscaArchivoRec (strcat str_DirArr str_NomDir) str_PatternFile)
				)
			)
		)
	); Fin función recursiva

	(fn_BuscaArchivoRec str_DirArranque str_NomFile)
	lst_FileEncontradas
)

;fn_GetCurrSuppPathLst
;***************************************************************************************************************
;*	Retorna una lista con los nombres de directorios que actualmente están definidos como Support
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_GetCurrSuppPathLst ( / obj_CurrSuppPath str_CurrSuppPath)
	(setq obj_CurrSuppPath (vla-get-files (vla-get-preferences (vlax-get-acad-object))))
	(setq str_CurrSuppPath (vla-get-SupportPath obj_CurrSuppPath))
	(vlax-release-object obj_CurrSuppPath)
	(fn_CadCDToLista1 str_CurrSuppPath ";")
)

;fn_GetCurrSuppPathStr
;***************************************************************************************************************
;*	Retorna un string separado por ";" con los nombres de directorios que actualmente están definidos como Support
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
(defun fn_GetCurrSuppPathStr ( / obj_CurrSuppPath str_CurrSuppPath)
	(setq obj_CurrSuppPath (vla-get-files (vla-get-preferences (vlax-get-acad-object))))
	(setq str_CurrSuppPath (vla-get-SupportPath obj_CurrSuppPath))
	(vlax-release-object obj_CurrSuppPath)
	str_CurrSuppPath
)

;fn_AddDirToSupport
;***************************************************************************************************************
;*	Agrega los directorios de "lst_DirName" al Support Path
;*
;*	Parámetros:
;*		lst_DirName: Lista de strings con los nombres de directorios a ser agregados al SupportPath
;*
;*	Programado por Diego Cassini - Ver_1.00
;***************************************************************************************************************
;(defun fn_AddDirToSupport (lst_DirName / obj_CurrSuppPath str_CurrSuppPath str_AddPath)
;	(setq obj_CurrSuppPath (vla-get-files (vla-get-preferences (vlax-get-acad-object))))
;	(setq str_CurrSuppPath (vla-get-SupportPath obj_CurrSuppPath))
;	(mapcar '(lambda (str_AddPath)
;		(setq str_CurrSuppPath (strcat str_CurrSuppPath ";" str_AddPath))) lst_DirName)
;	(vla-put-SupportPath obj_CurrSuppPath str_CurrSuppPath)
;	(vlax-release-object obj_CurrSuppPath)
;	(princ)
;)

(defun fn_AddDirToSupport (lst_DirName / lst_CurrSuppPath str_CurrSuppPath str_AddPath)
	(setq lst_CurrSuppPath (fn_GetCurrSuppPathLst))
	(setq str_CurrSuppPath (fn_GetCurrSuppPathStr))

	(setq lst_CurrSuppPath (mapcar '(lambda (str_AddPath)
		(strcase str_AddPath)) lst_CurrSuppPath))

	(foreach str_AddPath lst_DirName
		(if (not (member (strcase str_AddPath) lst_CurrSuppPath))
			(setq str_CurrSuppPath (strcat str_CurrSuppPath ";" str_AddPath)))
	)
	(vla-put-SupportPath obj_CurrSuppPath str_CurrSuppPath)
	(vlax-release-object obj_CurrSuppPath)
	(princ)
)
