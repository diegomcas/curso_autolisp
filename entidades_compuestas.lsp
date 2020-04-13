;Para leer bloques con atributos (los valores de los atributos)
(defun c:bl_attribs( / ent_name ent_def)
  ;Seleccionamos el bloque y obtenemos el nombre de entidad
  (setq ent_name (car (entsel "Seleccione bloque: \n")))
  
  ;Capturamos todos los valores de la definición de la entidad
  (setq ent_def (entget ent_name))
  
  ;Verificamos que la entidad seleccionada sea una inserción
  (if (= (cdr (assoc 0 ent_def)) "INSERT")
    (progn
      ;Es una inserción de un bloque. Verificamos que sea el bloque que queremos manipular
      (if (= "Ejemplo" (cdr (assoc 2 ent_def)))
        (progn
          ; Es nuestro bloque con atributos, entonces la proxima entidad es un ATTRIB
          (setq ent_name (entnext ent_name))
          (setq ent_def (entget ent_name))
          ;Leemos atributos hasta que la entidad sea de tipo "SEQEND"
          (while (/= (cdr (assoc 0 ent_def)) "SEQEND")

            ;Escribimos el TAG del atributo y el valor
            (princ (cdr (assoc 2 ent_def)))
            (princ " = ")
            (princ (cdr (assoc 1 ent_def)))
            (princ "\n")

            (setq ent_name (entnext (cdr (assoc -1 ent_def))))
            (setq ent_def (entget ent_name))
          )
        )
        (progn
          (princ "El bloque seleccionado no es 'Ejemplo'.\n")
        )
      )
    )
    (progn
      (princ "La entidad seleccionada no es un bloque.\n")
    )
  )
  (princ)
)

;Para leer polilineas 3D (Es muy similar a leer los atributos de los bloques)
(defun c:3d_poly()
  (setq cnt 0)
  ;Seleccionamos la 3d poly y obtenemos el nombre de entidad
  (setq ent_name (car (entsel "Seleccione polilinea 3d: \n")))
  
  ;Capturamos todos los valores de la definición de la entidad
  (setq ent_def (entget ent_name))
  
  ;Verificamos que la entidad seleccionada sea una Polilinea 3D (recordar que las polilineas simples son LWPOLYLINE)
  (if (= (cdr (assoc 0 ent_def)) "POLYLINE")
    (progn
      ;Es una polilinea 3d, entonces la proxima entidad es un VERTEX
      (setq ent_name (entnext ent_name))
      (setq ent_def (entget ent_name))
      ;Leemos los VERTEX hasta "SEQEND"
      (while (/= (cdr (assoc 0 ent_def)) "SEQEND")

        ;Escribimos las coordenadas del vértice
        (princ "Las coordenadas del vértice son = ")
        (princ (cdr (assoc 10 ent_def)))
        (princ "\n")

        ;Leemos la siguiente entidad
        (setq ent_name (entnext (cdr (assoc -1 ent_def))))
        (setq ent_def (entget ent_name))
      )
    )
    (progn
      (princ "La entidad seleccionada no es una 3dpoly.\n")
    )
  )
  (princ)
)

; Pruebas mías ignorar o analizar por curiosidad
; (defun c:ent_props()
  ; (setq ent_name (car (entsel "Seleccione una entidad: \n")))
  ; (setq ent_obj (vlax-ename->vla-object ent_name))
  ; (vlax-dump-object ent_obj)
; )



; (defun c:ent_next()
  ; (setq cnt 0)
  ; ;Seleccionamos el bloque y obtenemos el nombre de entidad
  ; (setq ent_name (car (entsel "Seleccione entidad: ")))
  
  ; (while ent_name
    ; ;Capturamos todos los valores de la definición de la entidad
    ; (setq ent_def (entget ent_name))

    ; (princ (assoc 0 ent_def)) (princ "\n")
    ; (setq ent_name (entnext (cdr (assoc -1 ent_def))))
    ; (setq cnt (1+ cnt))
  ; )
  ; (princ cnt)
; )