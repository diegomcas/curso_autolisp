;El ejemplo infaltable para cualquier tutorial de programación!!!
(defun c:hola_mundo( / su_nombre texto_a_escribir) ;Notar que al terminar la ejecución del programa "su_nombre" y "texto_a_escribir" dejan de existir
  (setq su_nombre (getstring T "Ingrese su nombre: ")) ;ufffff cuantas cosas nuevas
  (setq texto_a_escribir
    (strcat "Hola \"mundo\".\nSoy " su_nombre " y este es mi primer programa en autolisp.\n")) ;MAS COSAS NUEVAS... PARÃ LOCO
  (princ texto_a_escribir)
)

(defun c:cosas_con_strings()
  (setq str_a "Una cadena corta.")
  (setq str_b "AutoLISP es un lenguaje de programación derivado del lenguaje Lisp. Es utilizado para generar rutinas orientadas al uso especÃ­fico de AutoCAD y sus derivados. Permite desarrollar programas y funciones para el manejo de entidades del tipo grÃ¡fico. Los programas hechos en Autolisp amplÃ­an los comandos y aplicaciones de AutoCAD, creando asÃ­ una solución óptima para cada problema en particular, desde el simple trazo de una lÃ­nea hasta el diseÃ±o de un plano o pieza, llegando a cÃ¡lculos complejos, convirtiéndose en gran ayuda para las aplicaciones de ingenierÃ­a.")
  (setq mi_cadena (getstring T "Ingrese una cadena de texto: "))
  (setq mi_cadena_limitada (getstring nil "Ingrese una cadena de texto sin espacios: "))

  (princ "Que longitud tiene la cadena?\n")
  (princ "str_a tiene ") (princ (strlen str_a)) (princ " caracteres.\n")
  (princ "str_b tiene ") (princ (strlen str_b)) (princ " caracteres.\n")
  
  (initget 1 "Si") ;Ohia, veamos esto!!!
  (setq seguir (getkword "\nSeguimos? [Si]: ")) ;que funciona totalmente asociado a esto
  
  (if (eq seguir "Si")
    (concat str_a " Pero vale como ejemplo.")
  )
  
  (initget 1 "Si") ;Ohia, veamos esto!!!
  (setq seguir (getkword "\nSeguimos? [Si]: ")) ;que funciona totalmente asociado a esto

  (if (eq seguir "Si")
    (pasa_may_min str_a)
  )
  
  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (if (eq seguir "Si")
    (cortar str_a)
  )
  
  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (if (eq seguir "Si")
    (quitar_inicio_fin)
  )
  
  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (if (eq seguir "Si")
    (malditos_patrones)
  )
  
  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (if (eq seguir "Si")
    (cadenas_a_numeros)
  )
  
  (initget 1 "Si")
  (setq seguir (getkword "\nSeguimos? [Si]: "))

  (if (eq seguir "Si")
    (numeros_a_cadenas)
  )
  
  (princ)
)

(defun concat (str_a otro_str)
  (princ "la función strcat se usa para concatenar cadenas \(2 o más\). Por ejemplo:\n")
  (princ "-> concatenemos str_a con otro_str:\n")
  (princ (strcat str_a otro_str)) (princ "\n")
  (princ "-> concatenemos str_a con str_a varias veces y agregando literales (espacios en este caso y un salto de linea al final):\n")
  (princ (strcat str_a "  " str_a " " str_a " " str_a "\n")) ;Como ésto es lo último que evalúa la función, es lo que retorna.
  (princ)
)

(defun pasa_may_min (str)
  (princ "La función strcase se usa para pasar a mayúsculas o minúsculas una cadena. Por ejemplo:\n")
  (princ (strcat "La cadena original es: " str ".\n"))
  (princ (strcat "La cadena pasada a mayúsculas es: " (strcase str))) (princ "\n")
  (princ (strcat "La cadena pasada a minúsculas es: " (strcase str T))) (princ "\n")
  (princ)
)

(defun cortar(str)
  (princ "La funcion substr corta una cadena según algunos parámetros. Veamos:\n")
  (princ "Vamos a cortar la cadena desde la posición 4 hasta el final:\n")
  (princ (substr str 4)) (princ "\n")
  (princ "Vamos a cortar la cadena desde la posición 4 y una longitud de 6 caracteres:\n")
  (princ (substr str 4 6)) (princ "\n")
  (princ)
)

(defun quitar_inicio_fin()
  (princ "La función \"vl-string-left-trim\" quita caracteres específicos del inicio de una cadena:\n")
  (princ "Usaremos la cadena '12_Ejemplo para quitar strings al inicio.'\n")
  (princ "Le quiero sacar a la cadena cualquier caracter numérico y guiones que tenga.\n")
  (princ "El resultado sería entonces -> ")
  (princ (vl-string-left-trim "0123456789_-" "12_Ejemplo para quitar strings al inicio.")) (princ "\n")
  
  (princ "De la misma forma, la función \"vl-string-right-trim\" quita caracteres específicos del final de una cadena:\n")
  (princ "Usaremos la cadena 'Ejemplo para quitar strings al final...$123,45'\n")
  (princ "Le quiero sacar a la cadena cualquier caracter numérico, los puntos y signos.\n")
  (princ "El resultado sería entonces -> ")
  (princ (vl-string-right-trim ".,0123456789$%#&" "Ejemplo para quitar strings al final...$123,45.")) (princ "\n")
  (princ)
)

(defun malditos_patrones()
  (princ "No, no es un reclamo de clases!. Los patrones (patterns) se usan para evaluar si un string cumple los criterios que se definen en éste. Veamos:\n")
  ;# (pound)
  ;Matches any single numeric digit.

  ;@ (at)
  ;Matches any single alphabetic character.

  ;. (period)
  ;Matches any single nonalphanumeric character.

  ;* (asterisk)
  ;Matches any character sequence, including an empty one, and it can be used anywhere in the search pattern: at the beginning, middle, or end.
  
  ;? (question mark)
  ;Matches any single character.

  ;~ (tilde)
  ;If it is the first character in the pattern, it matches anything except the pattern.

  ;[...]
  ;Matches any one of the characters enclosed.

  ;[~...]
  ;Matches any single character not enclosed.

  ;- (hyphen)
  ;Used inside brackets to specify a range for a single character.

  ; , (comma)
  ;Separates two patterns.

  ;` (reverse quote)
  ;Escapes special characters (reads next character literally).
  
  (princ "Entonces la función wcmatch evalúa el texto que se le envía como parámetro con el patrón (también parámetro) y retorna T o nil (Verdadero o Falso) si lo cumple o no.\n")
  (princ "Usemos la cadena 'Un hermoso ejemplo. Tiene 0123456789 números y %$#& signos.\n")
  (setq la_cadena "Un hermoso ejemplo. Tiene 0123456789 números y %$#& signos.")
  (princ "Veamos si la cadena empieza con 'U'.\n")
  (princ (wcmatch la_cadena "U*")) (princ "\n")
  (princ "Veamos si la cadena tiene una 'U' en el medio.\n")
  (princ (wcmatch la_cadena "*U*")) (princ "\n")
  (princ "Veamos si la cadena NO tiene una 'z' en el medio.\n")
  (princ (wcmatch la_cadena "~*z*")) (princ "\n")
  (princ "Veamos si la cadena tiene una 'z' en el medio.\n")
  (princ (wcmatch la_cadena "*z*")) (princ "\n")
  (princ "Veamos si la cadena NO tiene una 'z' en el medio o empieza con 'U'.\n")
  (princ (wcmatch la_cadena "U*,~*z*")) (princ "\n")
  (princ "Veamos si la cadena empieza con un caracter cualquiera y lo sigue una 'n'.\n")
  (princ (wcmatch la_cadena "?n*")) (princ "\n")
  (princ "Veamos si la cadena 32-16983098-2 es válida como número de CUIT.\n")
  (princ (wcmatch "32-16983098-2" "##`-########`-#")) (princ "\n")
  (princ "Las combinaciones son infinitas y complejas, tanto así que muchos consideran este tipo de cosas un lenguaje aparte.\n")
  (princ)
)

(defun cadenas_a_numeros()
  (princ "Empecemos con las conversiones de tipo:\n")
  (princ "Si tenemos una cadena de texto, con las funciones atoi y atof las convertimos a enteros y flotantes (reales) respectivamente.\n")
  (princ "Pasemos la cadena '3.1415' a entero y a real:\n")
  (princ "'3.1415' pasado a entero -> ") (princ (atoi "3.1415")) (princ "\n")
  (princ "'3.1415' pasado a real -> ") (princ (atof "3.14151415")) (princ "\n")
  (princ "Tener mucho cuidado con las cadenas que no son convertibles.\n")
  (princ "'Hola, soy una cadena' pasado a entero -> ") (princ (atoi "Hola, soy una cadena")) (princ "\n")
  (princ "'Hola, soy una cadena' pasado a real -> ") (princ (atof "Hola, soy una cadena")) (princ "\n")
  (princ "Ven que retorna 0?. Eso es más peligroso que si retornara error y cortara el porgrama.\n")
  (princ)
)

(defun numeros_a_cadenas()
  (princ "Sigamos con las conversiones de tipo:\n")
  (princ "Hagamos la inversa de antes, tengamos números y 'hagamos' strings.\n")
  (princ "Un número entero se pasa a string con la función itoa.\n")
  (princ "El número 3 se convierte a -> ") (princ (itoa 3)) (princ "\n")
  (princ "En este caso, si la cadena pasada no es una representación de un número entero, retorna error.\n")
  
  (princ "Hagamos lo mismo con cadenas que representan reales.\n")
  (princ "Aquí podemos definir algunos parámetros que hacen a la representación de ese número.\n")
  (princ "El modo y la precisión.\n")
  (princ "Usemos el número real 3.1415 para ver ésto.\n")
  
  (princ "3.1415 Notación científica -> ") (princ (rtos 3.1415 1 5)) (princ "\n")
  (princ "3.1415 Notación decimal -> ") (princ (rtos 3.1415 2 5)) (princ "\n")
  (princ "3.1415 Notación de ingeniría (yanki) -> ") (princ (rtos 3.1415 3 5)) (princ "\n")
  (princ "3.1415 Notación de arquitectura (yanki) -> ") (princ (rtos 3.1415 4 5)) (princ "\n")
  (princ "3.1415 Notación de fraccional -> ") (princ (rtos 3.1415 5 5)) (princ "\n")
    
  (princ "Otra función que hace esto es angtos. Se usa para valores angulares. Considera un ángulo de entrada en radianes.\n")
  (princ "3.1415 grados -> ") (princ (angtos 3.1415 0 5)) (princ "\n")
  (princ "3.1415 grados/minutos/segundos -> ") (princ (angtos 3.1415 1 5)) (princ "\n")
  (princ "3.1415 grados decimales -> ") (princ (angtos 3.1415 2 5)) (princ "\n")
  (princ "3.1415 radianes -> ") (princ (angtos 3.1415 3 5)) (princ "\n")
  (princ "3.1415 Unidades de Agrimensura (O sea, el rumbo) -> ") (princ (angtos 3.1415 4 5)) (princ "\n")
  (princ)
)

(princ)
