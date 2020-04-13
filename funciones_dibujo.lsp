(defun draw_line(pt_01 pt_02 capa)
  (entmake
    (list
      (cons 0 "LINE")
      '(100 . "AcDbEntity")
      (cons 8 "La capa")
      '(100 . "AcDbLine")
      (cons 10 '(150 25))
      (cons 11 '(178.5 25))
    )
  )
)

;(setq lst_coords_pts (list '(10 10) '(20 20) '(10 20)))
(defun draw_lwpolyline(lst_coords_pts elevation capa closed / lst_poly)
  (setq lst_poly
    (list
      (cons 0 "LWPOLYLINE");Definimos el tipo de entidad
      (cons 100 "AcDbEntity")
      (cons 8 "Layer_rec");Defino el layer del rectángulo
      (cons 100 "AcDbPolyline")
      (cons 90 4)
      (if closed
        (cons 70 1)
        (cons 70 0)
      )
    )
	)
  
  (foreach pt lst_coords_pts
    (setq lst_poly (append lst_poly (list (cons 10 pt))))
  )
  
  (entmake lst_poly)
)