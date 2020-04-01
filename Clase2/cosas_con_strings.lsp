;El ejemplo infaltable para cualquier tutorial de programaci�n!!!
(defun c:hola_mundo( / su_nombre texto_a_escribir) ;Notar que al terminar la ejecuci�n del programa "su_nombre" y "texto_a_escribir" dejan de existir
  (setq su_nombre (getstring T "Ingrese su nombre: ")) ;ufffff cuantas cosas nuevas
  (setq texto_a_escribir
    (strcat "Hola \"mundo\".\nSoy " su_nombre " y este es mi primer programa en autolisp.\n")) ;MAS COSAS NUEVAS... PARÁ LOCO
  (princ texto_a_escribir)
)

(defun c:cosas_con_strings()
  (setq str_a "Una cadena corta.")
  (setq str_b "AutoLISP es un lenguaje de programaci�n derivado del lenguaje Lisp. Es utilizado para generar rutinas orientadas al uso específico de AutoCAD y sus derivados. Permite desarrollar programas y funciones para el manejo de entidades del tipo gráfico. Los programas hechos en Autolisp amplían los comandos y aplicaciones de AutoCAD, creando así una soluci�n �ptima para cada problema en particular, desde el simple trazo de una línea hasta el diseño de un plano o pieza, llegando a cálculos complejos, convirti�ndose en gran ayuda para las aplicaciones de ingeniería.")
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
  (princ "la funci�n strcat se usa para concatenar cadenas \(2 o m�s\). Por ejemplo:\n")
  (princ "-> concatenemos str_a con otro_str:\n")
  (princ (strcat str_a otro_str)) (princ "\n")
  (princ "-> concatenemos str_a con str_a varias veces y agregando literales (espacios en este caso y un salto de linea al final):\n")
  (princ (strcat str_a "  " str_a " " str_a " " str_a "\n")) ;Como �sto es lo �ltimo que eval�a la funci�n, es lo que retorna.
  (princ)
)

(defun pasa_may_min (str)
  (princ "La funci�n strcase se usa para pasar a may�sculas o min�sculas una cadena. Por ejemplo:\n")
  (princ (strcat "La cadena original es: " str ".\n"))
  (princ (strcat "La cadena pasada a may�sculas es: " (strcase str))) (princ "\n")
  (princ (strcat "La cadena pasada a min�sculas es: " (strcase str T))) (princ "\n")
  (princ)
)

(defun cortar(str)
  (princ "La funcion substr corta una cadena seg�n algunos par�metros. Veamos:\n")
  (princ "Vamos a cortar la cadena desde la posici�n 4 hasta el final:\n")
  (princ (substr str 4)) (princ "\n")
  (princ "Vamos a cortar la cadena desde la posici�n 4 y una longitud de 6 caracteres:\n")
  (princ (substr str 4 6)) (princ "\n")
  (princ)
)

(defun quitar_inicio_fin()
  (princ "La funci�n \"vl-string-left-trim\" quita caracteres espec�ficos del inicio de una cadena:\n")
  (princ "Usaremos la cadena '12_Ejemplo para quitar strings al inicio.'\n")
  (princ "Le quiero sacar a la cadena cualquier caracter num�rico y guiones que tenga.\n")
  (princ "El resultado ser�a entonces -> ")
  (princ (vl-string-left-trim "0123456789_-" "12_Ejemplo para quitar strings al inicio.")) (princ "\n")
  
  (princ "De la misma forma, la funci�n \"vl-string-right-trim\" quita caracteres espec�ficos del final de una cadena:\n")
  (princ "Usaremos la cadena 'Ejemplo para quitar strings al final...$123,45'\n")
  (princ "Le quiero sacar a la cadena cualquier caracter num�rico, los puntos y signos.\n")
  (princ "El resultado ser�a entonces -> ")
  (princ (vl-string-right-trim ".,0123456789$%#&" "Ejemplo para quitar strings al final...$123,45.")) (princ "\n")
  (princ)
)

(defun malditos_patrones()
  (princ "No, no es un reclamo de clases!. Los patrones (patterns) se usan para evaluar si un string cumple los criterios que se definen en �ste. Veamos:\n")
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
  
  (princ "Entonces la funci�n wcmatch eval�a el texto que se le env�a como par�metro con el patr�n (tambi�n par�metro) y retorna T o nil (Verdadero o Falso) si lo cumple o no.\n")
  (princ "Usemos la cadena 'Un hermoso ejemplo. Tiene 0123456789 n�meros y %$#& signos.\n")
  (setq la_cadena "Un hermoso ejemplo. Tiene 0123456789 n�meros y %$#& signos.")
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
  (princ "Veamos si la cadena 32-16983098-2 es v�lida como n�mero de CUIT.\n")
  (princ (wcmatch "32-16983098-2" "##`-########`-#")) (princ "\n")
  (princ "Las combinaciones son infinitas y complejas, tanto as� que muchos consideran este tipo de cosas un lenguaje aparte.\n")
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
  (princ "Ven que retorna 0?. Eso es m�s peligroso que si retornara error y cortara el porgrama.\n")
  (princ)
)

(defun numeros_a_cadenas()
  (princ "Sigamos con las conversiones de tipo:\n")
  (princ "Hagamos la inversa de antes, tengamos n�meros y 'hagamos' strings.\n")
  (princ "Un n�mero entero se pasa a string con la funci�n itoa.\n")
  (princ "El n�mero 3 se convierte a -> ") (princ (itoa 3)) (princ "\n")
  (princ "En este caso, si la cadena pasada no es una representaci�n de un n�mero entero, retorna error.\n")
  
  (princ "Hagamos lo mismo con cadenas que representan reales.\n")
  (princ "Aqu� podemos definir algunos par�metros que hacen a la representaci�n de ese n�mero.\n")
  (princ "El modo y la precisi�n.\n")
  (princ "Usemos el n�mero real 3.1415 para ver �sto.\n")
  
  (princ "3.1415 Notaci�n cient�fica -> ") (princ (rtos 3.1415 1 5)) (princ "\n")
  (princ "3.1415 Notaci�n decimal -> ") (princ (rtos 3.1415 2 5)) (princ "\n")
  (princ "3.1415 Notaci�n de ingenir�a (yanki) -> ") (princ (rtos 3.1415 3 5)) (princ "\n")
  (princ "3.1415 Notaci�n de arquitectura (yanki) -> ") (princ (rtos 3.1415 4 5)) (princ "\n")
  (princ "3.1415 Notaci�n de fraccional -> ") (princ (rtos 3.1415 5 5)) (princ "\n")
    
  (princ "Otra funci�n que hace esto es angtos. Se usa para valores angulares. Considera un �ngulo de entrada en radianes.\n")
  (princ "3.1415 grados -> ") (princ (angtos 3.1415 0 5)) (princ "\n")
  (princ "3.1415 grados/minutos/segundos -> ") (princ (angtos 3.1415 1 5)) (princ "\n")
  (princ "3.1415 grados decimales -> ") (princ (angtos 3.1415 2 5)) (princ "\n")
  (princ "3.1415 radianes -> ") (princ (angtos 3.1415 3 5)) (princ "\n")
  (princ "3.1415 Unidades de Agrimensura (O sea, el rumbo) -> ") (princ (angtos 3.1415 4 5)) (princ "\n")
  (princ)
)

(princ)
