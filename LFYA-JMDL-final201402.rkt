#lang racket
(require racket/class)
(require racket/system)
(require racket/draw)
(require racket/trace)

;;.....
;la negacion
;formato : (neg <proposicion>) --> proposicion
;proposito: calcular la negacion de una proposicion
;ejemplos y casos de prueba:
;> (neg #t)
;#f
;> (neg #f)
;#t
;>
(define neg
  (λ (p)
    (if p #f #t)))


;la conjuncion
;formato : (y <proposicion> <proposicion>) --> proposicion
;proposito: calcular la conjuncion de dos proposiciones
;ejemplos y casos de prueba:
;> (y #t #t)
;#t
;> (y #t #f)
;#f
;> (y #f #f)
;#f
;> (y #f #t)
;#f
;>
(define y
  (λ (p q)
    (if p q #f)))


;la disyuncion
;formato : (o <proposicion> <proposicion>) --> proposicion
;proposito: calcular la disyucion de dos proposiciones
;ejemplos y casos de prueba:
;> (o #t #t)
;#t
;> (o #t #f)
;#t
;> (o #f #t)
;#t
;> (o #f #f)
;#f
;>
(define o
  (λ (p q)
    (if p #t q)))


;la disyuncion exclusiva
;formato : (ox <proposicion> <proposicion>) --> proposicion
;proposito: calcular la disyuncion exclusiva de dos proposiciones
;ejemplos y casos de prueba:
;> (ox #t #t)
;#f
;> (ox #t #f)
;#t
;> (ox #f #t)
;#t
;> (ox #f #f)
;#f
;> 
(define ox
  (λ (p q)
    (if p (neg q) q)))


;la implicacion
;formato : (implicacion <proposicion> <proposicion>) --> proposicion
;proposito: calcular implicacion de dos proposiciones
;ejemplos y casos de prueba:
;> (implicacion #t #t)
;#t
;> (implicacion #t #f)
;#f
;> (implicacion #f #t)
;#t
;> (implicacion #f #f)
;#t
;>
(define implicacion
  (λ (p q)
    (if p q (neg p))))


;la doble implicacion
;formato : (doble-implicacion <proposicion> <proposicion>) --> proposicion
;proposito: calcular la doble implicacion de dos proposiciones
;ejemplos y casos de prueba:
;> (doble-implicacion #t #t)
;#t
;> (doble-implicacion #t #f)
;#f
;> (doble-implicacion #f #t)
;#f
;> (doble-implicacion #f #f)
;#t
;> 
(define doble-implicacion
  (λ (p q)
    (if p q (neg q))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

;cardinalidad
; formato: (cardinalidad <conjunto>) --> <numero>
; proposito: Determinar el numero de elementos de un conjunto
; ejemplos:
; > (cardinalidad ’(3 8 2 9))
; 4
; >
(define card-v2
  (λ (A res)
    (if (empty? A)
        res
        (card-v2 (cdr A) (+ res 1)))))

(define cardinalidad
  (λ (A)    
    (card-v2 A 0)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;pertenencia 
; formato: (pertenece? <elemento> <conjunto>) --> <booleano>
; proposito: verificar si pertenece un elemento al conjunto devuelve #t o #f sea el caso
; ejemplos:
;> (pertenece? 'a '(a e i o u))
;#t
;> (pertenece? 34 '(12 23 35 1 0 9 78 65 13 25))
;#f
;>
;> (pertenece? '() '(1 2 3))
;#f
;>
(define pertenece?
  (λ (a A)
    (cond ((empty? A) #f)
          ((equal? a (car A)) #t)
          (else (pertenece? a (cdr A))))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; (subconjunto? <Conjunto> <Conjunto>) --> <Booleano>

;; Produce #t si el conjunto A es subconjunto del conjunto B

;; (plantilla)
;; (define subconjunto?
;;   (λ (A B)
;;     (cond((empty? A) #t)
;;          ((.......))
;;       (....))))

;; (ejemplos)
;(check-expect (subconjunto? '() '(1 2 3)) #t)
;(check-expect (subconjunto? '(r) '(1 2 3)) #f)
;(check-expect (subconjunto? '(a b c d e r) '()) #f)

(define subconjunto?
  (λ (A B)
    (cond ((empty? A) #t)
          ((pertenece? (car A) B) (subconjunto? (cdr A) B))
          (else #f))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; (conjuntos-iguales? <Conjunto> <Conjunto>) --> <Booleano>

;; Produce #t si el cuanjunto A y el conjunto B son iguales

;; (plantilla)
;; (define conjuntos-iguales?
;;   (λ (A B)
;;     (cond((y subconjunto? A B)...))
;;       (....))))

;; (ejemplos)
;(check-expect (conjuntos-iguales? '() '(a s e r)) #f)
;(check-expect (conjuntos-iguales? '(a s e r) '(a s e r)) #t)
;(check-expect (conjuntos-iguales? '(a s e r) '()) #f)

(define conjuntos-iguales?
  (λ (A B)
    (y (subconjunto? A B)
       (subconjunto? B A))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;;cunatificador universal

;; (paraTodo <Predicado> <Dominio>) --> <Booleano>

;; Dado un predicado <P> un dominio <A> devuelve #t si cada elemento del conjunto cumple con el predicado <P>

;; (plantilla)
;;(define paraTodo
;;  (λ (P A)
;;    (cond ((empty? A) #t)
;;          ((P (car A)) .....)
;;          (... #f))))
;; (ejemplos)


(define paraTodo
  (λ (P A)
    (cond ((empty? A) #t)
          ((P (car A)) (paraTodo P (cdr A)))
          (else #f))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;cuantificador existencial

;; (existeUn <Predicado> <Dominio>) --> <Booleano>

;;Dado un redicado <P> y un dominio <A> devuelve #t si al menos un elemento cumple con el  dominio <A>

;;(plantilla)
;;(define existeUn?
;;  (λ (P A)
;;    (cond ((empty? A) ...)
;;          ((P (car A)) ...)
;;          (....))))
;;(ejemplos)
;(check-expect (existeUn? (λ (x) (> x 2)) '(0 2 3 4 5)) #t)
;(check-expect (existeUn? (λ (x) (> x 3)) '(0 -2 -3 -4 5)) #t)
;(check-expect (existeUn? (λ (x) (> x 10)) '(7 -1 -3 9 5)) #f)

(define existeUn?
  (λ (P A)
    (cond ((empty? A) #f)
          ((P (car A)) #t)
          (else (existeUn? P (cdr A))))))
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;###########################LFYA###################################


;; un predicado para verificar que un alfabeto es el alfabeto vacio
;; (alfabetoVacio? Conjunto) → Booleano?
;; (alfabetoVacio? ’(0 1)) → #f
;; (alfabetoVacio? ’()) → #t
(define alfabetoVacio? empty?)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define simbolo?
  (λ (s)
    (o (number? s) (symbol? s))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; (esAlfabeto? <Conjunto>) --> <Booleano>
;; (esAlfabeto? ’(0 1) --> #t
;; (esAlfabeto? ’((0 1) 0 1)--> #f
;; (esAlfabeto? ’(a b 0 1) --> #t
;; (esAlfabeto? ’()) --> #t

(define esAlfabeto?
  (λ (S)
    (o (alfabetoVacio? S)
       (and (list? S)
            (< (cardinalidad S) +inf.0)
            (paraTodo (λ (s) (simbolo? s)) S)))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; se define el concepto subalfabeto? como un sinonimo de subconjunto?

(define subalfabeto? subconjunto?)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; (subalfabetos <Conjunto> <LConjunto>) --> (<Booleano> <Booleano>,...,<Boolenao>)
;; (subalfabetos '(0 1 2 3 4) '((2 3 4) (0 2 4) (3 4 5)))
;; --> '(#t #t #f)

(define subalfabetos
  (λ (S LS)
    (map (λ (s) (subalfabeto? s S)) LS)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Se crea un sinonimo de conjuntosIguales?
;; (alfabetosIguales? Alfabeto Alfabeto) --> Booleano?
;; (alfabetosIguales? ’(0 1 2) ’(2 0 1)) --> #t
(define alfabetosIguales? conjuntos-iguales?)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define LAlfabetos '((3 2 0 1) (0 2 1 3) (4 2 1 3) (4 3 1 2) (2 4 3 1) (1 2 3 0) 
                               (0 3 2 1) (3 1 4 2) (0 2 3 1) (3 2 1 4) (1 3 2 4) (2 1 0 3) 
                               (1 3 0 2) (0 1 2 3) (3 0 1 2) (2 1 4 3) (1 0 3 2) (3 1 2 0) 
                               (1 2 3 4) (4 1 3 2) (2 3 4 1) (3 2 4 1) (2 3 0 1) (2 4 1 3)))

(define separaAlfabetos
  (λ (LA)
    (let* ([C1 (car LA)])
      (list (filter (λ (c) (alfabetosIguales? C1 c)) LA) 
            (filter (λ (c) (neg (alfabetosIguales? C1 c))) LA)))))


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Se define la longitud de una palabra como el numero de simbolos que contiene
;; se establece como sinonimo de la cardinalidad
;; (longitud Palabra) → NumeroEnteroNoNegativo?
;; (longitud ’(1 1 1 0 1)) → 5
(define longitud cardinalidad)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; determina si el argumento es una lista de simbolos de un alfabeto
(define esPalabra?
  (λ (w S)
    (and (list? S) (paraTodo (λ (s) (y (simbolo? s) (pertenece? s S))) w))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; definimos en el concepto de palabra vacia

(define palabraVacia '())

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; definimos el predicado palabraVacia?
(define palabraVacia? empty?)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define palabrasIguales?
  (λ (w s)    
    (if (y (palabraVacia? w) (palabraVacia? s)) #t
        (if (y (pair? w) (pair? s))
            (if (equal? (car w) (car s)) (palabrasIguales? (cdr w) (cdr s)) #f) #f))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define palabrasEnAlfabeto?
  (λ (Lw S)
    (map (λ (wi) (esPalabra? wi S)) Lw)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define alfabetoDe
  (λ (w)
    (remove-duplicates w)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define concatenar
  (λ (w s)
    (if (y (esPalabra? w (alfabetoDe w))
           (esPalabra? s (alfabetoDe s)))
        (append w s)
        'Error-No-es-palabra)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define intercalar-aux
  (λ (w s res)
    (cond ((empty? w) res)
          (else (intercalar-aux (cdr w) (cdr s) (concatenar res (list (car w) (car s))))))))

(define intercalar
  (λ (w s)
    (if (y (esPalabra? w (alfabetoDe w))
           (esPalabra? s (alfabetoDe s)))
        (if (= (cardinalidad w) (cardinalidad s))
            (intercalar-aux w s '()) #f) #f)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define escribeIzq
  (λ (e w)
    (if (y (esPalabra? w (alfabetoDe w)) (simbolo? e))
        (cons e w) #f)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define escribeDer
  (λ (e w)
    (if (y (esPalabra? w (alfabetoDe w)) (simbolo? e))
        (concatenar w (escribeIzq e palabraVacia)) #f)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;potenciaPalabra recibe: una palabra y un numero No Negativo.

(define potenciaPalabra-aux
  (λ (w n res)
    (cond ((= n 0) res)
          ((> n 0) (potenciaPalabra-aux w (- n 1) (concatenar w res)))
          (else 'Error-en-potencia))))

(define potenciaPalabra
  (λ (w n)
    (potenciaPalabra-aux w n palabraVacia)))


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define prefijoIgual?
  (λ (s w)
    (cond ((> (cardinalidad s)  (cardinalidad w)) #f)
          ((palabraVacia? s) #t)
          ((equal? (car s) (car w)) (prefijoIgual? (cdr s) (cdr w)))
          (else #f))))

(define prefijo?
  (λ (s w)
    (if (esPalabra? w (alfabetoDe w))
        (prefijoIgual? s w) #f)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define sufijo?
  (λ (s w)
    (prefijo? (reverse s) (reverse w))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define beta
  (λ (a w res)
    (cond ((palabraVacia? w) w)
          ((palabrasIguales? a res) w)
          (else (beta a (cdr w) (escribeDer (car w) res))))))

(define obtenerP
  (λ (w n res)
    (cond ((= n 0) res)
          ((palabraVacia? w) res)
          (else (obtenerP (cdr w) (- n 1) (escribeDer (car w) res))))))

(define alfaYbeta
  (λ (s w a b n w0);;comentar los 6 elementos
    (cond ((palabrasIguales? s (obtenerP w n palabraVacia)) (list a (beta (concatenar a s) w0 palabraVacia)))
          ((palabraVacia? w) (list a (beta (concatenar a s) w0 palabraVacia)))
          (else (alfaYbeta s (cdr w) (escribeDer (car w) a) b n w0)))))

(define subpalabra?
  (λ (s w)
    (palabrasIguales? w (concatenar (car (alfaYbeta s w palabraVacia palabraVacia (cardinalidad s) w))
                                    (concatenar s (cadr (alfaYbeta s w palabraVacia palabraVacia (cardinalidad s) w)))))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define componentesDe
  (λ (s w)
    (if (subpalabra? s w) 
        (let* ([palabra (alfaYbeta s w palabraVacia palabraVacia (cardinalidad s) w)]
               [alfa (car palabra)]
               [beta (cadr palabra)])
          (list alfa s beta))
        #f)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define palabraInversa reverse)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define palindromo?
  (λ (w)
    (cond ((esPalabra? w (alfabetoDe w)) (palabrasIguales? w (palabraInversa w)))
          (else #f))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define agregar
  (λ (e C)
    (if (pertenece? e C)
        C
        (append C (list e)))))

(define union
  (λ (A B)
    (if (empty? B)
        A
        (union (agregar (car B) A) (cdr B)))))

(define union-generalizada-auxiliar
  (λ (A B)
    (cond((empty? A) B)
         (else (union-generalizada-auxiliar (cdr A) (union (car A) B))))))


(define union-generalizada
  (λ (A) (union-generalizada-auxiliar A '())))

(define alfabetoDe*
  (λ (L)
    (alfabetoDe (union-generalizada L))))


; (diferencia <conjunto> <conjunto>) --> <conjunto>
;;
;; Proposito: Construir un conjunto, haciendo la diferencia entre los primeros dos conjuntos
;;
;;(plantilla)
;;(define diferencia
;;  (λ (A B)
;;   (diferencia-auxiliar A B '())))

;;(ejemplos)
;(check-expect (diferencia '(1 2 3 4) '()) '(4 3 2 1))
;(check-expect (diferencia '() '()) '())
;(check-expect (diferencia '(a b c d 1) '(e f r t 1 a)) '(d c b))
;(check-expect (diferencia-auxiliar '(1 2 3 4) '() '()) '(4 3 2 1))

(define diferencia-auxiliar
  (λ (A B C)
    (cond((empty? A) C)
         ((pertenece? (car A) B) (diferencia-auxiliar (cdr A) B C))
         (else (diferencia-auxiliar (cdr A) B (agregar (car A) C))))))

;; > (diferencia '(1 2 3 4) '(a b c 2 4))
;; '(3 1)
(define diferencia
  (λ (A B)
    (diferencia-auxiliar A B '())))
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; (interseccion-auxilia <conjunto> <conjunto> <conjunto>) --> <conjunto>
;;
;; Proposito: Construir un conjunto, haciendo la intersccion de los primeros dos conjuntos
;;(plantilla)
;;(define interseccion-auxiliar
;;  (λ(A B C)
;;    (cond((empty? B) ...)
;;         ((empty? A) C)
;;         ((.... (car A) B) .....))
;;         (...(interseccion-auxiliar ....))) 

;; Ejemplos y casos de prueba:
;(check-expect (interseccion-auxiliar '() '(2 4 6 8 10) '()) '())
;(check-expect (interseccion-auxiliar '(2 4 6 8 10) '(11 12 13 14 15) '()) '())
;(check-expect (interseccion-auxiliar '(2 4 6 8 10) '(2 3 4 5) '()) '(4 2))


(define interseccion-auxiliar
  (λ(A B C)
    (cond((empty? B) '())
         ((empty? A) C)
         ((pertenece? (car A) B) (interseccion-auxiliar (cdr A) B (agregar (car A) C)))
         (else (interseccion-auxiliar (cdr A) B C))))) 

;;(interseccion <conjunto> <conjunto>) --> <conjunto>
;; Proposito: Construir un conjunto, haciendo la intersccion de los primeros dos conjuntos
;;ejemplos y casos de prueba
;(check-expect (interseccion '() '(2 4 6 8 10)) '())
;(check-expect (interseccion '(2 4 6 8 10) '(11 12 13 14 15)) '())
;(check-expect (interseccion '(2 4 6 8 10) '(2 3 4 5)) '(4 2))
(define interseccion
  (λ (A B)
    (interseccion-auxiliar A B '())))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;(interseccion-generalizada <LitaDe/Conjunto>)--> <Conjunto>
;; Proposito: calcular la interseccion generalizada de la lista de conjunto

;;(platilla)
;;(define interseccion-generalizada
;;   (λ (A)
;;       (if (empty? A)'() ...)))

;;(ejemplos)
;(check-expect (interseccion-generalizada '((1 2) (2 4 1 3) (8 1 9 2 6))) '(2 1))
;(check-expect (interseccion-generalizada '((1 2) (2 4 1 3) (8 9 6))) '())
;(check-expect (interseccion-generalizada '((2 d w e r) (2 w n) (m w n y) (0 9 w))) '(w))

(define interseccion-generalizada-auxiliar
  (λ (A B)
    (cond((empty? A) B)
         (else (interseccion-generalizada-auxiliar (cdr A) (interseccion (car A) B))))))

(define interseccion-generalizada
  (λ (A)
    (if (empty? A) '()
        (interseccion-generalizada-auxiliar A (car A)))))

;;############################################################
;> (producto-cartesiano '(1 2 3) '(a b c))
;'((1 a) (2 a) (3 a) (1 b) (2 b) (3 b) (1 c) (2 c) (3 c))

(define agrega-por-derecha
  (λ (e C)
    (append C (list e))))

(define agrega-a-todos-aux 
  (λ(C e)
    (map (λ (c) (agrega-por-derecha e c)) C)))

(define agrega-a-todos*
  (λ(res LC)
    (apply append
            (map (λ(e) (agrega-a-todos-aux res e)) LC))))

(define producto-cartesiano-aux 
  (λ (res . LC)
    (cond ((empty? LC) res)
          (else (apply producto-cartesiano-aux  (cons (agrega-a-todos* res (car LC)) (cdr LC)))))))

(define producto-cartesiano
  (λ LC
    (apply producto-cartesiano-aux (cons '(()) LC))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define lenguajeNulo '())

(define lenguajeNulo? empty?)

(define lenguajeId '(()))

(define lenguajeUn
  (λ (s)
    (map (λ (a) (escribeDer a palabraVacia)) s)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Determina si una palabra pertenece a un lenguaje
;; (enLenguaje? Palabra Lenguaje) → Booleano?
;; (enLenguaje? ’(h o) ’((k o) (m o) (h o) (s o))) → #t
;; (enLenguaje? ’(h o) ’((k e) (m e) (h e) (s e))) → #f

(define enLenguaje?
  (λ (w L)
    (cond ((lenguajeNulo? L) #f)
          ((palabrasIguales? w (car L)) #t)
          (else (enLenguaje? w (cdr L))))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define sublenguaje?
  (λ (L1 L2)
    (paraTodo (λ (l) (enLenguaje? l L2)) L1)))


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define L '(() (0) (1) (1 0) (0 1 1 0) (0 0) (1 1) (0 0 0) (0 0 1) (0 1 0) (0 0 0 1) (1 1 0 0) (0 0 1 1) (0 1 0 1) (1 0 1 0)))

(define Ll '(((0 1) (1 0)) ((0 0 1) (0 1 0) (1 0 0)) ((1 1 0 0) (0 1 1 0) (0 0 1 1)) ((1 0 1) () (0 0 0 1))
                           ((0 1 1) (0 1 0) (0 1) (1 1) ()) ((0 0 0 1) (0 0 0) (0 0) (1) (0)) ((1 0 1 0) (0 0 1) (1 1 1 0) (0 0 0 1)) ((1 0) (1) (0) ())))

(define sublenguajes
  (λ (Ll L)
    (map (λ (l) (sublenguaje? l L)) Ll)))

;;> (sublenguajes Ll L)
;'(#f #f #t #f #f #t #f #t)

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define lenguajesIguales?
  (λ (L1 L2)
    (y (sublenguaje? L1 L2 ) (sublenguaje? L2 L1 ))))

;;###############################TEST##########################
;;;#
;(define T1
;  (λ (L1 L2)
;    (let* ([t1 (current-inexact-milliseconds)])
;      (define lenguajesIgualesT1?
;        (λ (L1 L2)
;          (y (sublenguaje? L1 L2 ) (sublenguaje? L2 L1 ))))
;      (lenguajesIgualesT1? L1 L2)
;      (let* ([t2 (current-inexact-milliseconds)])
;        (- t2 t1)))))
;
;
;(define T2
;  (λ (L1 L2)
;    (let* ([t1 (current-inexact-milliseconds)])
;      (define lenguajesIgualesT2?
;        (λ (L1 L2)
;          (if (= (cardinalidad L1) (cardinalidad L2))
;              (subconjunto? L1 L2) #f)))
;      (lenguajesIgualesT2? L1 L2)
;      (let* ([t2 (current-inexact-milliseconds)])
;        (- t2 t1)))))

;;#############################################################


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define concatenarLeng
  (λ (L1 L2)
    (apply append (map (λ (l1) (map (λ (l2) (concatenar l1 l2)) L2)) L1))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define potenciaLeng-aux
  (λ (L n res)
    (cond ((= n 0) res)
          (else (potenciaLeng-aux L (- n 1) (concatenarLeng L res))))))

(define potenciaLeng
  (λ (L n)
    (potenciaLeng-aux L n lenguajeId)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Genera un lenguaje que corrensponde a
;; la cerradura finita de Kleene porque genera las palabras de
;; longitud 0 hasta longitud n, para un n fijo.
;; (nKleene* Alfabeto Numero) -->Lenguaje
;; (nKleene* ’(0 1) 3) -->’((1 1) (1 0) (0 1) (0 0) () (0) (1)
;; (0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))

(define nKleene-aux
  (λ (S n res) ;;recibe S: alfabeto, n: un numero >= 0
    (cond ((= n 0) (union res (potenciaLeng S n)))
          (else (nKleene-aux S (- n 1) (union res (potenciaLeng S n)))))))

(define nKleene*
  (λ (S n)
    (nKleene-aux (lenguajeUn S) n lenguajeId)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;verifica si es un lenguaje un alista
;;(esLenguaje? <Lista/deListas>)-> <Boolen>
;;(esLenguaje? '(0 1 2) --> #f
;;(esLenguaje? '((i) (0 9)) --> #t

(define esLenguaje?
  (λ (L)
    (paraTodo (λ (l) (list? l)) L)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; verifica que una palabra o lenguaje pertenezcan a Kleene* de S
;; (Kleene*? (o Palabra Lenguaje) Alfabeto) -->Boolean?
;; (Kleene*? ’(1 0 0) ’(0 1)) -->#t
;; (Kleene*? ’(1 0 2) ’(0 1)) -->#f
;; (Kleene*? ’((1 0 0) () (1) (1 0 0)) ’(0 1)) -->#t
;; (Kleene*? ’((1 0 0) () (1) (1 2 0)) ’(0 1)) -->#f
(define Kleene*?
  (λ (X S)
    (cond ((esLenguaje? X) (alfabetosIguales? (alfabetoDe (union-generalizada X)) S))
          ((esPalabra? X (alfabetoDe X)) (alfabetosIguales? (alfabetoDe X) S))
          (else #f))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;;;examen 1erP
;;################################################

;(define ELE '(a (b (c)) (d (e (f g) h i))))
;(car (car (cdr (car (cdr (car (cdr (cdr ELE))))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (cdr ELE)))))))))

(define Pr1
  (λ (x C)
    (neg (paraTodo (λ (a) (= (modulo a (* 2 x)) 0)) C))))

;(define S1 '(0 1))
;(define S2 '(a b c))
;(define L1 '((1 1 1) (1 0 0) (1 0 1) (0 1 0) (0 1 1) (0 0 1)))
;(define L2 '(() (a a) (a b) (a c) (b a) (c c) (a a c)))
;(define L3 (filter (λ (s) (= (longitud s) 2)) (nKleene* S1 3)))
;(define L4 (filter (λ (s) (= (longitud s) 2)) (nKleene* S2 3)))


;;################################################

;;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$44
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@2do Parcial Automatas finitos deterministas@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


;;====================ejemplo de una clase===============
;;=======================================================
;; Definicion de una clase para modelar una relacion matematica
;; Se debe dar la lista de pares relacionados, el dominio y codominio

(define relacion%
  (class object%
    (init rel dom cod); <-- argumentos de inicializacion
    (define RE rel) ; <-- campo los pares de la relacion
    (define DO dom) ; <-- campo el dominio
    (define CO cod) ; <-- campo el codominio
    (super-new) ; <-- inicializacion de la superclase
    (define/public (Rel) ; Metodo publico. La relacion
      RE)
    (define/public (Dom) ; Metodo publico el dominio
      DO)
    (define/public (Cod) ; Metodo publico. El codominio
      CO)
    (define/public (Im x) ; Metodo publico. Imagen de un elemento
      (filter (λ (y) (en? (list x y) RE)) DO))
    (define/private (en? elem Conj) ; <-- Metodo privado
      (ormap (λ (e) (equal? e elem)) Conj))
    ))

;;========================================================

;;         ################################ PINTA UN AUTOMATA #############################

(define  pinta-afd-aux
  (λ (TR salida nombre);;recibe TR: una lista de transiciones; salida: un archivo de salida; nombre: con el que se va aguardar la imagen
    (cond((empty? TR) (display "\n}" salida) (close-output-port salida)
                      (if (system (string-append "dot -Tpng autom.dot -o" (string-append nombre".png"))) 
                          (make-object bitmap% (string-append nombre".png") 'png) 'No-Imagen$))         
         (else (fprintf salida "~a~a~s~a~a~a~s~a~a~a~s~a~a~n" "\t" "\""(caar TR)"\"" " -> " "\""(caddr (car TR))"\""
                        " [fontcolor=blue label=" "\""(cadr (car TR))"\"" "];") 
               (pinta-afd-aux (cdr TR) salida nombre)))))
;%%(cadr (cdr (car TR))) (caddr (car TR))

;;ejemplo
;;(pinta-automata A5 "afd_Act4_6") --> <Imagen>
(define pinta-afd
  (λ (automata nombre);;recibe un afd% y un nombre: para la imagen que se va a guardar    
    (let ([TR (send automata transiciones)]
          [salida (open-output-file "autom.dot" #:exists 'replace)]
          [ace (send automata aceptores)])           
      (display "digraph AutomataFinitoD \n{\n\tsize=\"7\";\n\trankdir=LR;\n\tnode [shape=circle];\n\toverlap=false\n" salida)
      (for-each (λ (a) (fprintf salida "~a~a~s~a~a~n" "\t" "\""a"\"" " [shape=doublecircle,color=magenta];")) ace)
      (fprintf salida "~a~a~s~a~a~n" "\tInicio [shape=none];\n\tInicio -> " "\""(send automata edoIni)"\"" ";")
      (pinta-afd-aux TR salida nombre))))

;;##############################################################################################

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%calse afd%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4
(define afd%
  (class object%
    (init EST SIM EIN ACE TRA) ; los argumentos de inicializacion
    (define E EST); Los estados
    (define S SIM); Los simbolos del alfabeto
    (define e0 EIN); El estado inicial
    (define A ACE); Los estados aceptores
    (define T TRA); Las reglas de transicion en lista de tripletas
    (super-new)
    ; .... Metodos publicos y privados
    (define/public (estados) E)
    (define/public (simbolos) S)
    (define/public (edoIni) e0)      
    (define/public (aceptores) A)
    (define/public (transiciones) T)
    (define/private (3de tupla) (caddr tupla));;devuelve el tercer lemento de una 3-trupla '(a 0 b) --> 'b
    (define/public (Tr e s) (3de (car (filter (λ (t) (conjuntos-iguales? (list e s) (list (car t) (cadr t)))) T))))
    (define/public (TR w e) (cond ((palabraVacia? w) e) (else (TR (cdr w) (Tr e (car w))))))
    ;;devuelve #t si las palabra X y Y desde un mismo estado inicial e son congruentes;que ambas lleguen al mismo estado final
    ;;(send A4 congruentes? '(1 1 0 1) '(0 1 1) 'a) #t
    (define/public (congruentes? x y e) (equal? (TR x e) (TR y e)))
    ;;determina si un estado del automata es aceptor o no lo es; (send A1 aceptor? 'b) --> #t 
    (define/public (aceptor? edo) (pertenece? edo A))
    ;;recibe w en S*;;devuelve #t si es aceptada por el automata, #f en otro caso;; (send A4 acepta? '(1 1 0 0)) --> #t
    (define/public (acepta? w) (aceptor? (TR w e0)))
    ;;redefine el estado inicial del automata;;recibe un estado "e";;donde "(pertenece? "e" (send <afd%> estados)) -->#t"
    (define/public (e0! e) (if (pertenece? e E) (set! e0 e) #f))   
    ;;calcula el conjunto de palabras propias de longitud menor o igual a un 
    ;;entero positivo k;;retorna un conjunto de palabras propias en S*
    (define/public (kPropias k)
      (let* ([nk (nKleene* S k)])
        (define propias
          (λ (e)
            (filter (λ (w) (acepta? w)) nk)))
        (interseccion-generalizada (map (λ (e) (propias (e0! e))) E))))
    
    ;;calcula el conjunto de palabras impropias de longitud menor o igual a un 
    ;;entero positivo k;;retorna un conjunto de palabras impropias en S*
    (define/public (kImpropias k)
      (let* ([nk (nKleene* S k)])
        (define impropias
          (λ (e)
            (filter-not (λ (w) (acepta? w)) nk)))
        (interseccion-generalizada (map (λ (e) (impropias (e0! e))) E))))
    ;;calcula el conjunto de palabras hasta de longitud k que con aceptadas por el lenguaje
    (define/public (lenguaje k)
      (let* ([nk (nKleene* S k)])        
        (filter (λ (w) (acepta? w)) nk)))
    
    (define/public (pintagrafo)
     (pinta-afd this "graphRacket"))
    
    ))

(define A1 (new afd% [EST '(a b)]
                [SIM '(0 1)]
                [EIN 'a]
                [ACE '(b)]
                [TRA '((a 0 a) (a 1 b) (b 0 b) (b 1 a))]))


;;#################################################################################################
;;---------------------------------------------------------------------

(define A5 (new afd%
                [EST '(q1 q2 q3 q4 q5 q6)]
                [SIM '(a b c)]
                [EIN 'q1]
                [ACE '(q4 q5 q6)]
                [TRA '((q1 a q2) (q1 b q3) (q1 c q1)
                                 (q2 a q3) (q2 b q2) (q2 c q4)
                                 (q3 a q3) (q3 b q2) (q3 c q4)
                                 (q4 a q5) (q4 b q6) (q4 c q4)
                                 (q5 a q4) (q5 b q6) (q5 c q5)
                                 (q6 a q5) (q6 b q4) (q6 c q6))]))

;;#########################################################################################
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%LEER ARCHIVO DE AUTOMATA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;recibe un list de string de la forma '(">>" "q0" "a" "q0" "b" "q1" "c" "q2"); 
;;retorna una lista '(>> q0 a q0 b q1 c q2)
(define castL
  (λ (S res) 
    (cond ((empty? S) res)
          ((string->number (car S)) (castL (cdr S) (append res (list (string->number (car S))))))
          (else (castL (cdr S) (append res (list (string->symbol (car S)))))))))

(define simbolos
  (λ (L res) ;;recibe una lista '(<e> <s> <e> <s>,...) de estados y simbolos y una lista vacia; retorna una lista de simbolos
    (cond ((empty? (cdr L)) res)
          (else (simbolos (cddr L) (append res (list (cadr L))))))))

(define crea-transiciones
  (λ (L q res) ;;recibe una lista '(<e> <s> <e> <s>,...) de estados y simbolos y una lista vacia; retorna una lista de transiciones
    (cond ((empty? L) res)
          (else (crea-transiciones (cddr L) q (append res (list (list q (car L) (cadr L)))))))))

;;estructura del archivo

;;los primeros simbolos son importantes solo se aceptan los siguientes: '(>> >* ** --) 
;; >> "para definir el estado inicial"
;; >* "para definir el estado inicial y que tambien es estado aceptor"
;; ** "para definir los estados aceptores"
;; -- "indica que no es estado inicial y ninguno de los estados aceptores

;; es importante dar los espacios entre cada simbolo

;; archivo.dat o archivo.txt
;;  ----------------------
;; | >> q0 a q1 b q2 c q0 |
;; | -- q1 a q0 b q2 c q3 | 
;; | -- q2 a q1 b q0 c q3 |
;; | -- q3 a q3 b q4 c q5 |
;; | ** q4 a q4 b q4 c q5 |
;; | ** q5 a q5 b q5 c q4 |;sin cambio de linea en la ultima linea
;;  ----------------------
;; ejemplo
;; (define fg (txt->afd "datos.dat"))
;; (object:afd% ...)

(define txt->afd
  (λ (nombrearchivo) ;;recibe el nombre del archivo
    (let* ([archivo (string->path nombrearchivo)]
           [LS (file->lines archivo)]          
           [LL (map (λ (s) (castL (string-split s) '())) LS)]
           [ESTx (map (λ (est) (cadr est)) LL)]
           [SIMx (simbolos (cdr (car LL)) '())]
           [EINIx (car (map (λ (ini) (cadr ini)) (filter (λ (ein) (or (equal? (car ein) '>>) (equal? (car ein) '>*))) LL)))] 
           [ACEx (map (λ (ace) (cadr ace)) (filter (λ (ei) (or (equal? (car ei) '**) (equal? (car ei) '>*))) LL))]
           [TRAx (apply append (map (λ (tra) (crea-transiciones (cddr tra) (car (cdr tra)) '())) LL))])
      
      (new afd% [EST ESTx]
           [SIM SIMx]
           [EIN EINIx]
           [ACE ACEx]
           [TRA TRAx]))
    
    ))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;recibe un (afd%) y un alfabeto '((0 1 0..) ...(2 0 1));; devuelve una lista de verdad,
;;por cada palabra  indicando si es verdadero que el automata 
;;acepta la palabra o alfabeto en caso contrario.
(define clasificador1
  (λ (afd A)
    (map (λ (palabra) (send afd acepta? palabra)) A)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define C (new afd%
               [EST '(e a f b c)]
               [SIM '(0 1 2)]
               [EIN 'e]
               [ACE '(a c e)]
               [TRA '((e 0 f) (e 1 d) (e 2 e) (a 0 e) (a 1 d) (a 2 b)
                              (f 0 c) (f 1 a) (f 2 f) (b 0 a) (b 1 c) (b 2 b)
                              (c 0 f) (c 1 d) (c 2 c) (d 0 d) (d 1 e) (d 2 a))]))

;;(clasificador1 C '((0 2 0) (2 1 0 2) (2 1 1 0) (0 2 1 1) (2 1 2 1) (0 2 1 2) (2 2 0) (1 1 0)))


;;#######################Automata Finito No Deterministico ##################

;;############################## PINTA UN AFN ###########################################
(define  pinta-afn-aux
  (λ (TR salida nombre);;recibe TR: una lista de transiciones; salida: un archivo de salida; nombre: con el que se va aguardar la imagen
    (cond((empty? TR) (display "\n}" salida) (close-output-port salida)
                      (if (system (string-append "dot -Tpng autom.dot -o" (string-append nombre".png"))) 
                          (make-object bitmap% (string-append nombre".png") 'png) 'No-Imagen$))         
         ((empty? (caddr (car TR))) (pinta-afn-aux (cdr TR) salida nombre))
         (else (for-each (λ (L) (fprintf salida "~a~a~s~a~a~a~s~a~a~a~s~a~a~n" "\t" "\""(caar TR)"\"" " -> " "\"" L"\""
                                         " [fontcolor=blue label=" "\""(cadar TR)"\"" "];")) (caddr (car TR))) 
               (pinta-afn-aux (cdr TR) salida nombre)))))

;;ejemplo
;;(pinta-auf A5 "afn_Act4_6") --> <Imagen>
(define pinta-afn
  (λ (automata nombre);;recibe un afn% y un nombre: para la imagen que se va a guardar    
    (let ([TR (send automata transiciones)]
          [salida (open-output-file "autom.dot" #:exists 'replace)]
          [ace (send automata aceptores)])           
      (display "digraph AutomataFinitoN \n{\n\tsize=\"7\";\n\trankdir=LR;\n\tnode [shape=circle];\n" salida)
      (for-each (λ (a) (fprintf salida "~a~a~s~a~a~n""\t" "\"" a "\"" " [shape=doublecircle,color=magenta];")) ace)
      (fprintf salida "~a~a~s~a~a~n" "\tInicio [shape=none];\n\tInicio -> " "\""(send automata edoIni)"\"" ";" )
      (pinta-afn-aux TR salida nombre))))
;;#################################################################################


(define afn%
  (class object%
    (init EST SIM EIN ACE TRA) ; los argumentos de inicializacion
    (define E EST); Los estados
    (define S SIM); Los simbolos del alfabeto
    (define e0 EIN); El estado inicial
    (define A ACE); Los estados aceptores
    (define T TRA); Las reglas de transicion en lista de tripletas
    (super-new)
    ; .... Metodos publicos y privados
    (define/public (estados) E)
    (define/public (simbolos) S)
    (define/public (edoIni) e0)      
    (define/public (aceptores) A)
    (define/public (transiciones) T)
    (define/private (3de tupla) (caddr tupla));;devuelve el tercer lemento de una 3-trupla '(a 0 (b)) --> '(b)
    (define/public (Tr e s) (3de (car (filter (λ (t) (equal? (list e s) (list (car t) (cadr t)))) T))))
    
    ;;calcula el estado final del automata cuando analiza una apalabra empezando desde un estado
    ;;funcion de transicion extenndida
    ;;w: es la palabra a analizar ;; e: el estado de donde partira la afuncion a analizar la palabra
    ;;retorna el estado final al analizar la palabra w ;;;> (send n03 TR* '(1 1) 'q2) --> '(q5 q6)
    (define/public (TR w e)
      (cond ((palabraVacia? w) (list e))
            (else (union-generalizada (map (λ (es) (TR (cdr w) es)) (Tr e (car w)))))))
    
    ;;determina si un estado del automata es aceptor o no lo es; (send A1 aceptor? 'b) --> #t 
    (define/public (aceptor? edo) (pertenece? edo A))
    
    ;;recibe w en S*;;devuelve #t si es aceptada por el automata, #f en otro caso;; (send A4 acepta? '(1 1 0 0)) --> #t
    (define/public (acepta? w) (existeUn? (λ (ef) (aceptor?  ef)) (TR w e0)))
    
    ;;redefine el estado inicial del automata;;recibe un estado "e";;donde "(pertenece? "e" (send <afn%> estados)) -->#t"
    (define/public (e0! e) (if (pertenece? e E) (set! e0 e) #f))
    
    ;;calcula el conjunto de palabras propias de longitud menor o igual a un 
    ;;entero positivo k;;retorna un conjunto de palabras propias en S*
    (define/public (kPropias k)
      (let* ([nk (nKleene* S k)])
        (define propias
          (λ (e)
            (e0! e) (filter (λ (w) (acepta? w)) nk)))
        (interseccion-generalizada (map (λ (e) (propias e)) E))))
    
    ;;calcula el conjunto de palabras impropias de longitud menor o igual a un 
    ;;entero positivo k;;retorna un conjunto de palabras impropias en S*
    (define/public (kImpropias k)
      (let* ([nk (nKleene* S k)])
        (define impropias
          (λ (e)
            (e0! e) (filter-not (λ (w) (acepta? w)) nk)))
        (interseccion-generalizada (map (λ (e) (impropias e)) E))))
    
    ;;calcula el conjunto de palabras hasta de longitud k que con aceptadas por el lenguaje
    (define/public (lenguaje k)
      (let* ([nk (nKleene* S k)])        
        (filter (λ (w) (acepta? w)) nk)))
    
    (define/public (pintagrafo)
     (pinta-afn this "graphRacket"))
    
    ))
;;##############################################################################

;;recibe una, L: "lista '(q3 q4 1) que salio de '(>> q0 0 q3 q4 1)" al reer el archivo, 
;;E: estados del automata, res: resto '();; retorna una lista de la forma : '((q4 q5) (()))
(define crea-tr-aux
  (λ (SIM s Lt q res)
    (cond ((empty? Lt) (list (list q s res) Lt))
          ((pertenece? (car Lt) SIM) (list (list q s res) Lt))
          (else (crea-tr-aux SIM s (cdr Lt) q (append res (list (car Lt)))))))) 

;;crea las tansiciones
;;recibe E: estados del automata;  S: los sinbolos del automata
;; q : un estado del automata ; Le: una "lista '(q3 q4 1) que salio de '(>> q0 0 q3 q4 1)" al leer el archivo; res: resto '()
;;retorna una lista de transiciones del estado q,,>> '((q4 0 (q3)) (q4 1 ()))

(define crea-tr
  (λ (SIM EST Lt q res)
    (cond ((empty? Lt) res)
          (else (let* ([tr (crea-tr-aux SIM (car Lt) (cdr Lt) q '())])
                        (crea-tr SIM EST (cadr tr) q (append res (list (car tr))))
                  )))))
                       
                                              
;;----------------------------LEER UN AFN DESDE UN ARCHIVO----------------------
;;estructura del archivo

;;los primeros simbolos son importantes solo se aceptan los siguientes: '(>> >* ** --) 
;; >> "para definir el estado inicial"
;; >* "para definir el estado inicial y que tambien es estado aceptor"
;; ** "para definir los estados aceptores"
;; -- "indica que no es estado inicial y ninguno de los estados aceptores

;; es importante dar los espacios entre cada simbolo

;; archivo.dat o archivo.txt

;;    ejemplo: datos.dat 
;;   >>con Simbolos '(0 1) y Estados '(q0 q1 q2 q3 q4q q5)<<
;;  ----------------------
;;  |>> q0 0 q1 q3 1     |
;;  |-- q1 0 1 q2        |
;;  |** q2 0 1           |
;;  |-- q3 0 1 q4 q5     |
;;  |-- q4 0 q3 1        |
;;  |** q5 0 1           |;sin cambio de linea en el ultimo renglon
;;  ----------------------
;; NOTA: es importante que el archivo tenga la estructura adecuada, respetando todos los 
;; simbolos para que funcione adecuadamente; siempre hay que poner todos los simbolos en cada renglon
;; aunque no haya transicion  |** q2 0 1 2 3 ... n  |
;; ejemplo
;; (define fg (txt->afn "datos.dat"))
;; (object:afn% ...)

(define txt->afn
  (λ (nombrearchivo) ;;recibe el nombre del archivo
    (let* ([archivo (string->path nombrearchivo)]
           [LS (file->lines archivo)]          
           [LL (map (λ (s) (castL (string-split s) '())) LS)]
           [ESTx (map (λ (est) (cadr est)) LL)]
           [SIMx (filter-not (λ (s) (pertenece? s ESTx)) (cdr (car LL)))]
           [EINIx (car (map (λ (ini) (cadr ini)) (filter (λ (ein) (or (equal? (car ein) '>>) (equal? (car ein) '>*))) LL)))] 
           [ACEx (map (λ (ace) (cadr ace)) (filter (λ (ei) (or (equal? (car ei) '**) (equal? (car ei) '>*))) LL))]
           [TRAx (apply append (map (λ (Lr e) (crea-tr SIMx ESTx (cddr Lr) e '())) LL ESTx))])
      
      (new afn% [EST ESTx]
           [SIM SIMx]
           [EIN EINIx]
           [ACE ACEx]
           [TRA TRAx]))  
    ))
;;--------------------------------------------------------

(define n03 (new afn%
                 [EST '(q0 q1 q2 q3 q4 q5 q6 q7)]
                 [SIM '(0 1)]
                 [EIN 'q3]
                 [ACE '(q6 q7)]
                 [TRA '((q0 0 (q2)) (q0 1 (q1 q3)) (q1 0 (q4)) (q1 1 ()) (q2 0 (q4)) (q2 1 (q3 q5)) (q3 0 ()) (q3 1 (q6))
                                    (q4 0 (q5 q7)) (q4 1 (q6)) (q5 0 (q5)) (q5 1 (q5)) (q6 0 ()) (q6 1 ()) (q7 0 ()) (q7 1 ()))]))

(define n033 (new afn%
                  [EST '(q0 q1 q2)]
                  [SIM '(0 1)]
                  [EIN 'q0]
                  [ACE '(q2 q0)]
                  [TRA '((q0 0 (q2)) (q0 1 (q0 q1)) (q1 0 (q0)) (q1 1 ()) (q2 0 (q1)) (q2 1 (q0 q2)))]))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;tansforma un automata afd% aun afn%
;;recibe un afd% 
;;devuelve un afn%
(define afd->afn
  (λ (afd)
    (new afn%
         [EST (send afd estados)]
         [SIM (send afd simbolos)]
         [EIN (send afd edoIni)]
         [ACE (send afd aceptores)]
         [TRA (map (λ (t) (list (car t) (cadr t) (cddr t))) (send afd transiciones))])))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;pertenece
(define pertenece-c? 
  (λ (a A)
    (cond ((empty? A) #f)
          ((conjuntos-iguales? a (car A)) #t)
          (else (pertenece-c? a (cdr A))))))

(define casT1
  (λ (SoN);;recibe un simbolo o un numero;;combierte de un simbolo a string o un nuemero a un string
    (cond ((symbol? SoN) (symbol->string SoN))
          (else (number->string SoN)))))

(define casT2
  (λ (SoN);;recibe un string;;combierte de un string a simbolo o numero
    (cond ((string->number SoN) (string->number SoN))
          (else (string->symbol SoN)))))


;;crea transiciones auxiliar
;;recibe afn: afn%, s: unsimbolo de los simbolos del afn%, Lq: unalista de estados de Ed; res: '()
(define crea-transiciones-afn-aux2
  (λ (afn s Lq res)
    (cond ((empty? Lq) res)
          (else (crea-transiciones-afn-aux2 afn s (cdr Lq) (union res (send afn Tr (car Lq) s)))))))

;; crea trnasiciones auxiliar
;;recibe un afn%, Es: en lista '(q0 q1) que es el estado;
;;          SIM: los simbolos del afn%, res:'()
(define crea-transiciones-afn-aux
  (λ (afn Es SIM res)
    (cond ((empty? SIM) res)
          (else (crea-transiciones-afn-aux afn Es (cdr SIM) 
                                           (append res (list (list Es (car SIM) 
                                                                   (crea-transiciones-afn-aux2 afn (car SIM) Es '())))))))))

;;(map (λ (s) (casT2 s)) (map (λ (n) (string-append (casT1 'q) (casT1 n))) (build-list 10 values)))
(define ordenarEstados
  (λ (Lestados)
    (map (λ (le) (map (λ (e) (casT2 e)) le))
         (map (λ (le) (sort (map (λ (e) (casT1 e)) le) string<?)) Lestados))))

;;crea las transiciones del afn a afd
;;recibe ;; afn un afn%, EST: estados unitarios del afn%
;;          SIM: simbolos del afn, Est: estados unitarios del afn%, tr :'() donde se guardara el resultado
;;retorna las transciones del nuevo afd%, con los estados modificados del afd%
(define crea-transiciones-afn
  (λ (afn EST SIM Estnew tr)
    (let* ([t (apply append (map (λ (Es) (crea-transiciones-afn-aux afn Es SIM '())) Estnew))]
           [trr (map (λ (er) (caddr er)) t)]
           [ESTNew (diferencia (ordenarEstados trr) EST)]);;(filter-not (λ (es) (pertenece-c? es EST)) trr)])
          ;; [ESTNew2 (filter-not (λ (es) (pertenece? es EST)) ESTNew)])
      (cond((empty? ESTNew) (list (append tr t) EST))
           (else (crea-transiciones-afn afn (append EST ESTNew) SIM ESTNew (append tr t)))))))


;; procedimiento auxiliar
;; (bfs-aux <afd%> <Cola> <lista> <lista>) --> <listaDe/estados>>
(define bfs-aux
  (λ (afd Q SIM BFS expl)
    (cond ((empty? Q) BFS)
          ((pertenece-c? (car Q) expl) (bfs-aux afd (cdr Q) SIM BFS expl))
          (else (bfs-aux afd 
                         (append Q (map (λ (s) (send afd Tr (car Q) s)) SIM)) SIM
                         (agregar (car Q) BFS) (agregar (car Q) expl))))))
;;Búsqueda en anchura
;; procedimiento auxiliar
;; (bfs-aux <afd%>) --> <listaDe/estados>>
(define bfs
  (λ (afd)
    (bfs-aux afd (list (send afd edoIni)) (send afd simbolos) '() '())))
;;---------------------------------------------------------------------------------

(define renombrar-aux
  (λ (q est tran res)
    (append res (map (λ (t) (list (if (equal? est (car t)) q (car t)) (cadr t) 
                                  (if (equal? est (caddr t)) q (caddr t)))) tran))))

(define renombrar ;;renombra las transiciones
  (λ (qest EST TRAN)
    (cond ((empty? qest) TRAN)
          (else (renombrar (cdr qest) (cdr EST) (renombrar-aux (car qest) (car EST) TRAN '()))))))

(define estIni ;; renombra el estado inicial
  (λ (Ei ES Enw)
    (cond ((conjuntos-iguales? Ei (car ES)) (car Enw))
          (else (estIni Ei (cdr ES) (cdr Enw))))))

(define acepT ;renombra los estados aceptores
  (λ (acp ES Enw res)
    (cond ((empty? ES) res)
          ((pertenece-c? (car ES) acp) (acepT acp (cdr ES) (cdr Enw) (append res (list (car Enw)))))
          (else (acepT acp (cdr ES) (cdr Enw) res)))))


;;crea un afd% a partir de un afn%
;; (afn->afd <afn%>) --> afd%
(define afn->afd
  (λ (afn #:min [min #f]) ;;#:min  es un parametro Booleano para indicar si el afd% va a ser minimizado;; por de fault es #f
    (let* ([EST (lenguajeUn (send afn estados))]
           [SIMx (send afn simbolos)]
           [EINx (car (filter (λ (q) (and (pertenece? (send afn edoIni) q) (= (cardinalidad q) 1))) EST))]
           [TR (crea-transiciones-afn afn EST SIMx EST '())]
           [TRAxx (map (λ (t) (list (map (λ (b) (casT2 b)) (sort (map (λ (a) (casT1 a)) (car t)) string<?))
                                    (cadr t) 
                                    (map (λ (b) (casT2 b)) (sort (map (λ (a) (casT1 a)) (caddr t)) string<?)))) (car TR))]          
           [ESTxx (map (λ (e) (car e)) TRAxx)]
           [ACExx (filter-not (λ (e) (empty? (interseccion (send afn aceptores) e))) ESTxx)]
           [NewAFD (new afd% [EST ESTxx] [SIM SIMx] [EIN EINx] [ACE ACExx] [TRA TRAxx])] ;;creo un afd auxiliar
           [ESTx (bfs NewAFD)]          
           [TRAx (filter (λ (t) (pertenece-c? (car t) ESTx)) TRAxx)]
           [ACEx (filter-not (λ (e) (empty? (interseccion (send afn aceptores) e))) ESTx)]
           ;;----------------------------------------------------------------------------- 
           ;;-------------------------renombrado--------------------------------
           [estN (map (λ (s) (casT2 s)) (map (λ (n) (string-append (casT1 'q) (casT1 n))) (build-list (cardinalidad ESTx) values)))]
           [esiniN (estIni EINx ESTx estN)]
           [aceN (acepT ACEx ESTx estN '())]
           [transi (renombrar estN ESTx TRAx)]
           ;;-----------------------------------------------------------------------------
           )
      
      ;;(if min (new afd% [EST ESTx] [SIM SIMx] [EIN EINx] [ACE ACEx] [TRA TRAx]) NewAFD) ;;no renombrado
      (if min (new afd% [EST estN] [SIM SIMx] [EIN esiniN] [ACE aceN] [TRA transi]) NewAFD) ;;renombrado
      
      )))

;;verifica que dos automatas sean equivalentes
(define eqvAF?
  (λ (A1 A2)
    (let* ([k (apply min (list (cardinalidad (send A1 estados)) (cardinalidad (send A2 estados))))]
           [L1 (send A1 lenguaje k)]
           [L2 (send A2 lenguaje k)])
      (if (conjuntos-iguales? L1 L2) #t #f))))



;;###########################################################################################


(define NG (new afn% [EST '(q1 q2 q3 q4 q5)]
                [SIM '(0 1)]
                [EIN 'q1]
                [ACE '(q4 q5)]
                [TRA '((q1 0 ()) (q1 1 (q2 q3))
                                 (q2 0 (q3)) (q2 1 (q4))
                                 (q3 0 (q4 q5)) (q3 1 ())
                                 (q4 0 ()) (q4 1 (q4 q5))
                                 (q5 0 (q4 q5)) (q5 1 ()))]))

(define P (new afd% [EST '(q1 q2 q3)]
               [SIM '(0 1)]
               [EIN 'q1]
               [ACE '(q1 q3)]
               [TRA '((q1 0 q2) (q1 1 q1)
                                (q2 0 q3) (q2 1 q2)
                                (q3 0 q3) (q3 1 q1))]))

;;############################################################
;;pertenceA
;;devuelve un simbolo >> o >* o ** o -- dependiendo aque estado pertenece
;; :q un estado;; ini:: un estado inicial;; ace: estados aceptores
(define perteneceA
  (λ (q ini ace)
    (cond ((and (equal? q ini) (neg (pertenece? q ace))) '>>)
          ((and (equal? q ini) (pertenece? q ace)) '>*)
          ((pertenece? q ace) '**)
          (else '--))))

;;escribe:: escribe en un archivo
;;entrada: q: un estado, TR: una transicion; z: un simbolo de la forma >> o >* o ** o -- ;; salida:: un archivo
;;
(define  escribe
  (λ (q TR z salida)
    (begin 
      (fprintf salida "~s ~s" z q) 
      (for-each (λ (a) (begin (fprintf salida " ~s" (cadr a)) (if (list? (caddr a))
                                                                  (for-each (λ (x) (fprintf salida " ~s" x)) (caddr a))
                                                                  (fprintf salida " ~s" (caddr a)))))
                (filter (λ (ei) (equal? (car ei) q)) TR))
      (fprintf salida "~n"))))

;;af->txt: escribe en un archivo de texto aun automata del tipo afd%,afn% afe%
;;recibe un afd% o afn% o afe% y un nombre para el archivo
;;devuelve:: un archivo de texto en el directorio actual de este codigo fuente..

;;NOTA: para escribir automatas que fueron transformados de afn -> afd , el 
;;      automata resultante debe estar renombrado, los estados no deben de ser listas
;;      el automata debe tener el formato de un afd...

(define af->txt
  (λ (automata nombre)
    (let* ([salida (open-output-file (string-append nombre ".dat") #:exists 'replace)]
           [est (send automata estados)]
           [ini (send automata edoIni)]
           [ace (send automata aceptores)]
           [TR (send automata transiciones)])
      (begin
        (for-each (λ (e) (escribe e TR (perteneceA e ini ace) salida)) est)        
        (close-output-port salida)))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;actividad 5-14

;;2.5
;;a)
(define 25a (new afn% [EST '(q0 q1 q2)]
                [SIM '(0 1)]
                [EIN 'q0]
                [ACE '(q2)]
                [TRA '((q0 0 (q1)) (q0 1 (q0))
                       (q1 0 (q2)) (q1 1 ())
                       (q2 0 (q2)) (q2 1 (q0)))]))

(define 25b (new afn% [EST '(q0 q1 q2 q3)]
                [SIM '(0 1)]
                [EIN 'q0]
                [ACE '(q3)]
                [TRA '((q0 0 (q1)) (q0 1 (q0))
                       (q1 0 (q2)) (q1 1 ())
                       (q2 0 (q3)) (q2 1 ())
                       (q3 0 (q3)) (q3 1 (q3)))]))

(define 25c (new afn% [EST '(q0 q1 q2 q3)]
                [SIM '(0 1)]
                [EIN 'q0]
                [ACE '(q0 q1 q3)]
                [TRA '((q0 0 (q2)) (q0 1 (q1))
                       (q1 0 ()) (q1 1 (q1))
                       (q2 0 (q3)) (q2 1 ())
                       (q3 0 ()) (q3 1 (q3)))]))

(define 25d (new afn% [EST '(q0 q1 q2 q3)]
                [SIM '(0 1)]
                [EIN 'q0]
                [ACE '(q0 q3)]
                [TRA '((q0 0 ()) (q0 1 (q1))
                       (q1 0 (q2)) (q1 1 ())
                       (q2 0 (q3)) (q2 1 (q3))
                       (q3 0 ()) (q3 1 ()))]))

(define 25e (new afn% [EST '(q0 q1 q2 q3 q4 q5)];;.....pensando
                [SIM '(0 1)]
                [EIN 'q0]
                [ACE '(q5)]
                [TRA '((q0 0 ()) (q0 1 (q1))
                       (q1 0 (q2)) (q1 1 (q2))
                       (q2 0 (q3)) (q2 1 (q3))
                       (q3 0 (q4)) (q3 1 (q4))
                       (q4 0 (q5)) (q4 1 (q5))
                       (q5 0 (q0)) (q5 1 (q0)))]))

;---------------------------------------------------------------------------------

;;############################## PINTA UN AFE ###########################################
(define  pinta-afe-aux
  (λ (TR salida nombre);;recibe TR: una lista de transiciones; salida: un archivo de salida; nombre: con el que se va aguardar la imagen
    (cond((empty? TR) (display "\n}" salida) (close-output-port salida)
                      (if (system (string-append "dot -Tpng autom.dot -o" (string-append nombre".png"))) 
                          (make-object bitmap% (string-append nombre".png") 'png) 'No-Imagen$))         
         ;;;((empty? (caddr (car TR))) (pinta-afe-aux (cdr TR) salida nombre))
         (else (for-each (λ (L) (fprintf salida "~a~a~s~a~a~a~s~a~a~a~s~a~a~n" "\t" "\""(caar TR)"\"" " -> " "\"" L"\""
                                         " [fontcolor=blue label=" "\""(cadar TR)"\"" "];")) (caddr (car TR))) 
               (pinta-afe-aux (cdr TR) salida nombre)))))

;;ejemplo
;;(pinta-afe A5 "afe_Act4_6") --> <Imagen>
(define pinta-afe
  (λ (automata nombre);;recibe un afe% y un nombre: para la imagen que se va a guardar    
    (let ([TR (send automata transiciones)]
          [salida (open-output-file "autom.dot" #:exists 'replace)]
          [ace (send automata aceptores)])           
      (display "digraph AutomataFinitoE \n{\n\tsize=\"7\";\n\trankdir=LR;\n\tnode [shape=circle];\n" salida)
      (for-each (λ (a) (fprintf salida "~a~a~s~a~a~n""\t" "\"" a "\"" " [shape=doublecircle,color=magenta];")) ace)
      (fprintf salida "~a~a~s~a~a~n" "\tInicio [shape=none];\n\tInicio -> " "\""(send automata edoIni)"\"" ";" )
      (pinta-afe-aux TR salida nombre))))

;;#################################################################################

(define afe%
  (class object%
    (init EST SIM EIN ACE TRA) ; los argumentos de inicializacion
    (define E EST); Los estados
    (define S SIM); Los simbolos del alfabeto
    (define e0 EIN); El estado inicial
    (define A ACE); Los estados aceptores
    (define T TRA); Las reglas de transicion en lista de tripletas
    (super-new)
    ; .... Metodos publicos y privados
    (define/public (estados) E)
    (define/public (simbolos) S)
    (define/public (edoIni) e0)      
    (define/public (aceptores) A)
    (define/public (transiciones) T)
    
    ;;devuelve el tercer lemento de una 3-trupla '(a 0 (b)) --> '(b)
    (define/private (3de tupla) (caddr tupla))
    
     ;;calcula la transicion siguiente desde un estado con un simbolo
    (define/public (Tr e s)     
      (3de (car (filter (λ (t) (equal? (list e s) (list (car t) (cadr t)))) T))))
    
    ;;eCerra:: entrada: un estado <q> del afe%;;
    ;;devuelve:: una lista de estados que tal que cada estado es E-alcanzable por <q>
    (define/public (eCerr-aux q res)
      (let* ([tr (Tr q 'ε)]
             [dif (diferencia tr res)])
        (cond ((empty? dif) res)
            (else (union-generalizada (list (apply append (map (λ (qi) (eCerr-aux qi (union dif res))) dif)) (list q)))))))
    
     (define/public (eCerr q)
       (eCerr-aux q (list q)))
      
     ;;calcula la transicion extendida auxiliar
    ;;recibe un apalabra w;; y una lista de estados iniciales e
    (define/private (TR-aux w e)
      (cond ((empty? w) e)
            (else (TR-aux (cdr w) 
                          (union-generalizada 
                           (map (λ (ei) (eCerr ei)) (apply append (map (λ (ek) (Tr ek (car w))) e))))))))
    
    ;;calcula la transicion extendida
    ;;entrada: w: una palabra ; e: un estado 
    
    (define/public (TR w e)
      (TR-aux w (eCerr e)))
    
     ;;determina si un estado del automata es aceptor o no lo es; (send A1 aceptor? 'b) --> #t 
    (define/public (aceptor? edo) (pertenece? edo A))
    
    ;;recibe w en S*;;devuelve #t si es aceptada por el automata, #f en otro caso;; (send A4 acepta? '(1 1 0 0)) --> #t
    (define/public (acepta? w) (existeUn? (λ (ef) (aceptor?  ef)) (TR w e0)))
    
    ;;redefine el estado inicial del automata;;recibe un estado "e";;donde "(pertenece? "e" (send <afn%> estados)) -->#t"
    (define/public (e0! e) (if (pertenece? e E) (set! e0 e) #f))
    
    ;;calcula el conjunto de palabras hasta de longitud k que con aceptadas por el lenguaje
    (define/public (lenguaje k)
      (let* ([nk (nKleene* S k)])        
        (filter (λ (w) (acepta? w)) nk)))
    
   (define/public (pintagrafo)
     (pinta-afe this "graphRacket"))
     
    ))

(define AFE  (new afe% 
                  [EST '(q0 q1 q2 q3 q4)]
                  [SIM '(0 1)]
                  [EIN 'q0]
                  [ACE '(q1 q3)]
                  [TRA '((q0 0 (q0 q3)) (q0 1 (q3)) (q0 ε (q1))
                         (q1 0 (q1)) (q1 1 (q3)) (q1 ε (q2))
                         (q2 0 (q2)) (q2 1 (q4)) (q2 ε ())
                         (q3 0 (q1)) (q3 1 ()) (q3 ε ())
                         (q4 0 ()) (q4 1 ()) (q4 ε ()))]))

(define ejmafe  (new afe% 
                  [EST '(q1 q2 q3 q4 q5 q6 q7 q8)]
                  [SIM '(0 1)]
                  [EIN 'q1]
                  [ACE '(q7 q8)]
                  [TRA '((q1 0 (q5)) (q1 1 (q2)) (q1 ε ())
                         (q2 0 ()) (q2 1 ()) (q2 ε (q3 q6))
                         (q3 0 ()) (q3 1 (q7)) (q3 ε (q4))
                         (q4 0 (q8)) (q4 1 ()) (q4 ε ())
                         (q5 0 ()) (q5 1 ()) (q5 ε (q1))
                         (q6 0 ()) (q6 1 ()) (q6 ε (q1))
                         (q7 0 ()) (q7 1 ()) (q7 ε (q6))
                         (q8 0 ()) (q8 1 ()) (q8 ε (q7)))]))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;crea transiciones afe% recibe
;; q: une stado;; Ls:lista de simbolos;; res: '() y un afe%
(define crea-trAFN
  (λ (q Ls res afe)
    (cond ((empty? Ls) res)
          (else (crea-trAFN q (cdr Ls) (append res (list (list q (caar Ls) (send afe TR (car Ls) q)))) afe)))))
     
;;recibe un afe% y devuelve un afn%     
(define afe->afn
  (λ (afe)
    (let* ([ESTx (send afe estados)]
           [SIMx (send afe simbolos)]
           [EINx (send afe edoIni)]
           [ac (send afe aceptores)]
           [ACEx (filter-not (λ (ei) (empty? (interseccion (send afe eCerr ei) ac))) ESTx)]
           [TRAx (apply append (map (λ (ei) (crea-trAFN ei (lenguajeUn SIMx) '() afe)) ESTx))])
      
      (new afn% [EST ESTx][SIM SIMx] [EIN EINx] [ACE ACEx] [TRA TRAx]) ;;creo un afn%
      
      ))) 

;;####################################################################
;;recibe un afe% y devuelve un afd%
(define afe->afd
  (λ (afe)
    (afn->afd (afe->afn afe) #:min #t)))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;;lee un txt.dat con el formato de un afe% ;;
;;recibe el nombre del archivo del afe%
;;devuelve un afe%
(define txt->afe
  (λ (nombrearchivo) ;;recibe el nombre del archivo
    (let* ([archivo (string->path nombrearchivo)]
           [LS (file->lines archivo)]          
           [LL (map (λ (s) (castL (string-split s) '())) LS)]
           [ESTx (map (λ (est) (cadr est)) LL)]
           [SIMx (filter-not (λ (s) (pertenece? s ESTx)) (cdr (car LL)))]
           [EINIx (car (map (λ (ini) (cadr ini)) (filter (λ (ein) (or (equal? (car ein) '>>) (equal? (car ein) '>*))) LL)))] 
           [ACEx (map (λ (ace) (cadr ace)) (filter (λ (ei) (or (equal? (car ei) '**) (equal? (car ei) '>*))) LL))]
           [TRAx (apply append (map (λ (Lr e) (crea-tr SIMx ESTx (cddr Lr) e '())) LL ESTx))])
      
      (new afe% [EST ESTx]
           [SIM (remove 'ε SIMx)]
           [EIN EINIx]
           [ACE ACEx]
           [TRA TRAx]))
    
    ))
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
      
(define afefn (new afe% [EST '(q1 q2)][SIM '(0 1)] [EIN 'q1] [ACE '(q2)] [TRA '((q1 0 ()) (q1 1 ()) (q1 ε (q2))
                                                                                (q2 0 (q2)) (q2 1 (q2)) (q2 ε ()))]))

(define afeE (new afe% [EST '(q1 q2 q3 q4)][SIM '(0 1)] [EIN 'q1] [ACE '(q3)] [TRA '((q1 0 ()) (q1 1 (q3)) (q1 ε (q2))
                                                                                (q2 0 (q2)) (q2 1 (q2)) (q2 ε (q4))
                                                                                (q3 0 (q3)) (q3 1 (q3)) (q3 ε ())
                                                                                (q4 0 ()) (q4 1 (q3)) (q4 ε (q1)))]))

;;;................AFEs..................
;(define afe1 (afe->afd (txt->afe "EjercicioAutomatas/AF1.dat")))
;(define afe4 (afe->afd (txt->afe "EjercicioAutomatas/AF4.dat")))
;(define afe5 (afe->afd (txt->afe "EjercicioAutomatas/AF5.dat")))
;(define afe6 (afe->afd (txt->afe "EjercicioAutomatas/AF6.dat")))
;(define afe7 (afe->afd (txt->afe "EjercicioAutomatas/AF7.dat")))
;(define afe11 (afe->afd (txt->afe "EjercicioAutomatas/AF11.dat")))
;(define afe12 (afe->afd (txt->afe "EjercicioAutomatas/AF12.dat")))
;(define afe16 (afe->afd (txt->afe "EjercicioAutomatas/AF16.dat")))
;(define afe17 (afe->afd (txt->afe "EjercicioAutomatas/AF17.dat")))
;(define afe20 (afe->afd (txt->afe "EjercicioAutomatas/AF20.dat")))
;
;(define AFEs (list afe1 afe4 afe5 afe6 afe7 afe11 afe12 afe16 afe17 afe20))
;      
;;;.................AFNs..................
;(define afe2 (afn->afd (txt->afn "EjercicioAutomatas/AF2.dat") #:min #t))
;(define afe3 (afn->afd (txt->afn "EjercicioAutomatas/AF3.dat") #:min #t))
;(define afe8 (afn->afd (txt->afn "EjercicioAutomatas/AF8.dat") #:min #t))
;(define afe9 (afn->afd (txt->afn "EjercicioAutomatas/AF9.dat") #:min #t))
;(define afe10 (afn->afd (txt->afn "EjercicioAutomatas/AF10.dat") #:min #t))
;(define afe13 (afn->afd (txt->afn "EjercicioAutomatas/AF13.dat") #:min #t))
;(define afe14 (afn->afd (txt->afn "EjercicioAutomatas/AF14.dat") #:min #t))
;(define afe15 (afn->afd (txt->afn "EjercicioAutomatas/AF15.dat") #:min #t))
;(define afe18 (afn->afd (txt->afn "EjercicioAutomatas/AF18.dat") #:min #t))
;(define afe19 (afn->afd (txt->afn "EjercicioAutomatas/AF19.dat") #:min #t))
;
;(define AFNs (list afe3 afe8 afe9 afe10 afe13 afe14 afe15 afe18 afe19 afe2))
;      
;(define equivalente
;  (λ (a Lb)
;    (cond ((empty? Lb) (list a 'no))
;          ((eqvAF? a (car Lb)) (list a (car Lb)))
;          (else (equivalente a (cdr Lb))))))
;
;
;(define eqv (map (λ (af) (equivalente af AFNs)) AFEs))
;;
;(map (λ (af) (if (equal? (cadr af) 'no) (list (pinta-afd (car af) "prueba") "falta!!!!") 
;                 (list (pinta-afd (car af) "prueba") (pinta-afd (cadr af) "prueba")))) eqv)
;
(define K (new afn% [EST '(q0 q1 q2)]
                    [SIM '(0 1)]
                    [EIN 'q0]
                    [ACE '(q2 q0)]
                    [TRA '((q0 0(q2))(q0 1 (q0 q1))
                           (q1 0(q0))(q1 1())
                           (q2 0(q1))(q2 1(q0 q2)))]))
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555
;;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
(define A4 (new afd%
                [EST '(a b c d e f)]
                [SIM '(0 1)]
                [EIN 'a]
                [ACE '(d)]
                [TRA '((a 0 d) (a 1 c) (b 0 d) (b 1 e)
                               (c 0 b) (c 1 c) (d 0 f) (d 1 d)
                               (e 0 e) (e 1 f) (f 0 f) (f 1 f))]))
;;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

(define crear-trns-afd 
  (λ (TR LE)
    (cond ((empty? LE) TR)
          (else (let* ([tr (map (λ (t) (let* ([a (if (pertenece? (car t) (car LE)) (caar LE) (car t))]
                                              [b (if (pertenece? (caddr t) (car LE)) (caar LE) (caddr t))])
                                         (list a (cadr t) b))) TR)])
                  (crear-trns-afd  tr (cdr LE)))))))


;; procedimiento auxiliar
;; (bfs-aux <afd%> <Cola> <lista> <lista>) --> <listaDe/estados>>
(define bfss-aux
  (λ (afd Q SIM BFS expl)
    (cond ((empty? Q) BFS)
          ((pertenece? (car Q) expl) (bfss-aux afd (cdr Q) SIM BFS expl))
          (else (bfss-aux afd 
                         (append Q (map (λ (s) (send afd Tr (car Q) s)) SIM)) SIM
                         (agregar (car Q) BFS) (agregar (car Q) expl))))))
;;Búsqueda en anchura
;; procedimiento auxiliar
;; (bfs-aux <afd%>) --> <listaDe/estados>>
(define bfss
  (λ (afd)
    (bfss-aux afd (list (send afd edoIni)) (send afd simbolos) '() '())))

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;particiona un conjunto de estados
;recibe una particion,un afd% y un simbolo del afd%
     
(define particionar-aux
  (λ (part afd s)
    (let* ([prt1 (filter (λ (e) (pertenece? (send afd Tr e s) part)) part)]
           [prt2 (map (λ (e) (if (neg (pertenece? (send afd Tr e s) part)) (list e) '())) part)])           
       (filter-not (λ (a) (equal? a '())) (apply append (list (list prt1)  prt2))))))



;;particona los estados 
;;recibe un afd% los simbolos del afd% dos veces,, y la particicon inicial
;;que son los estados aceptores y los que no son aceptores
;devuelve una lista de los estados particionados
(define particionar
  (λ (afd sim1 sim2 particonAct)    
      (cond ((empty? sim1) particonAct)                        
            (else (let* ([partNew (apply append (map (λ (p) (particionar-aux p afd (car sim1))) particonAct))])
                    (if (= (cardinalidad partNew) (cardinalidad particonAct)) (particionar afd (cdr sim1) sim2 partNew)                   
                   (particionar afd sim2 sim2 partNew)))))))

;;####################################################################

(define semejante-aux
  (λ (tp Le)
    (filter-not (λ (tl) (empty? (interseccion tp tl))) Le)))

(define semejante
  (λ (LT res)
    (cond((empty? LT) res)
         (else (let* ([sem (semejante-aux (car LT) (cdr LT))]
                      [lt (remove* (append sem (list (car LT))) LT)])
                 (if (empty? sem) (semejante (cdr LT) (append res (list (car LT))))
                     (semejante lt (append res (list (union-generalizada (append sem (list (car LT)))))))))))))
     
(define Equivalentes-aux2
  (λ (dis eq afd sim res)
    (cond ((empty? eq) (list res dis))
          (else (if (existeUn? (λ (s) (pertenece-c? (list (send afd Tr s (caar eq))
                             (send afd Tr s (cadar eq))) dis)) sim)
        (Equivalentes-aux2 (append dis (list (car eq))) (cdr eq) afd sim res) 
        (Equivalentes-aux2 dis (cdr eq) afd sim (append res (list (car eq)))))))))
;;(trace Equivalentes-aux2)
    
    
(define Equivalentes-aux
  (λ (afd sim estaDif estDis res)
    (cond ((empty? estaDif) (Equivalentes-aux2 estDis res afd sim '()))
          (else (let* ([es (car (ordenarEstados (list (append (list (send afd Tr (car sim) (caar estaDif)) 
                                                                  (send afd Tr (car sim) (cadar estaDif)))))))])
                  (if (pertenece? es estDis)
                      (Equivalentes-aux afd sim (cdr estaDif) (append estDis (list (car estaDif))) res)
                      (Equivalentes-aux afd sim (cdr estaDif) estDis (append res (list (car estaDif))))))))))

   
(define estdosEquivalentes
  (λ (afd sim no-acep acep estDis res)
    (let* ([estaDif (remove-duplicates 
                     (ordenarEstados                                         
                      (filter-not (λ (p) (equal? (car p) (cadr p))) 
                        (union (producto-cartesiano no-acep no-acep) (producto-cartesiano acep acep)))))]
           [est-eq0 (Equivalentes-aux afd sim estaDif (ordenarEstados estDis) res)]
           [est-eq (car (Equivalentes-aux2 (cadr est-eq0) (car est-eq0) afd sim '()))]
           [est-eqfn (append (lenguajeUn (filter-not (λ (e) (pertenece? e (append* est-eq))) (send afd estados))) est-eq)])
           est-eqfn)))

(define clases-eq-afd
  (λ (afd)
    (semejante (estdosEquivalentes afd (send afd simbolos)
                                   (diferencia (send afd estados) (send afd aceptores))
                                   (send afd aceptores)
                                   (producto-cartesiano (diferencia (send afd estados) (send afd aceptores)) (send afd aceptores))
                                   '()) '())))
    
           
;########################################################################3
;; este procedimeinto minimiza un afd, dejandolo con el minimo de estados 
;;si es posible
;recibe un afd% -> aun afd% minimizado
(define minimizar-afd
  (λ (afd)
    (let* ([particion (list (filter-not (λ (e) (send afd aceptor? e)) (send afd estados)) (send afd aceptores))]
           ;;---------creo todas las particones de los estados------------------------
           ;;..........................beta1........................
           ;;[partfinal (particionar afd (send afd simbolos) (send afd simbolos) particion)]
           ;;...........................listo"".............................
           [partfinal (clases-eq-afd afd)]
           ;;------------------------------------------------------------------------
           [ace (map (λ (ac) (car ac)) (filter-not (λ (e) (empty? (interseccion e (send afd aceptores)))) partfinal))]
           [estsust (filter (λ (pr) (> (cardinalidad pr) 1)) partfinal)]
           [estados  (remove* (apply append (map (λ (le) (cdr le)) estsust)) (send afd estados))]
           [edoini (car (apply append (filter-not (λ (e) (empty? (interseccion e (list (send afd edoIni))))) partfinal)))]
           [simbolos (send afd simbolos)]          
           [trans (remove-duplicates (crear-trns-afd (send afd transiciones) estsust))]
           ;;---------------------------quitar estados no alcados desde el estado inicila--------------
           [estAlcz (bfss (new afd%  [EST estados] [SIM simbolos] [EIN edoini] [ACE ace] [TRA trans]))]
           ;;-----------------------------------------------------------------------------------------
           [TRAx (filter (λ (t) (pertenece? (car t) estAlcz)) trans)]
           [ACEx (filter (λ (e) (pertenece? e ace)) estAlcz)])
      ;;partfinal
           ;;(new afd%  [EST estados] [SIM simbolos] [EIN edoini] [ACE ace] [TRA trans])
      (new afd%  [EST estAlcz] [SIM simbolos] [EIN edoini] [ACE ACEx] [TRA TRAx])
      
      )))

;;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

(define afn->afd-mz
  (λ (afn)
    (minimizar-afd (afn->afd afn))))

(define afe->afd-mz
  (λ (afe)
    (minimizar-afd (afe->afd afe))))

;;##########################################################
;;creo pares de estados acteriores con estados nuevos por renombrar
(define crea-par-est
  (λ (Q R res)
    (cond ((empty? Q) res)
          (else (crea-par-est (cdr Q) (cdr R) (append res (list (list (car Q) (car R)))))))))

;;busco un estado dentro de una lista de pares
;;q un estado;; Lpares: un alista de pares
(define busca
  (λ (q Lpares)
    (cadar (filter (λ (p) (equal? (car p) q)) Lpares))))

;;renombro un atupla
;;tupla una tripleta '(q1 0 2 '(q3 q4));; Lpares: un alista de pares equivalentes
(define renombrar-tran
 (λ (tupla Lpares)
   (let* ([a (busca (car tupla) Lpares)]
          [b  (if (list? (last tupla)) (map (λ (le) (busca le Lpares)) (last tupla)) (busca (last tupla) Lpares))])
     (list a (cadr tupla) b))))

;;renombro un afd con los estados que le envio
;;af: afd% o afn% o afe%;; Ln: un lista de nuemros que seran los esatos '(1 2 3 4 ...) 
;;de acuerdo a la cardinalidad de los estados del af%
;;devuelve un af% renombrado con sus estados                                                                
(define renombrar-af
  (λ (af Ln)
    (let* ([pares (crea-par-est (send af estados) Ln '())]
           [estados Ln]
           [edoini (busca (send af edoIni) pares)]
           [aceptores (map (λ (p) (cadr p)) (filter (λ (par) (pertenece? (car par) (send af aceptores))) pares))]
           [sim (send af simbolos)]
           [trans (map (λ (t) (renombrar-tran t pares)) (send af transiciones))])
      
      (cond ((is-a? af afd%) (new afd%  [EST estados] [SIM sim] [EIN edoini] [ACE aceptores] [TRA trans]))
            ((is-a? af afn%) (new afn%  [EST estados] [SIM sim] [EIN edoini] [ACE aceptores] [TRA trans]))
            (else (new afe%  [EST estados] [SIM sim] [EIN edoini] [ACE aceptores] [TRA trans]))))))

;;crea un alista de estados a partir de una lista de nuemros
;;LNum : '(1 2 3 4 ...) --> '(q1 q2 q3 q4 ...)
(define crea-estados
  (λ (LNum)
    (map (λ (s) (casT2 s)) (map (λ (n) (string-append (casT1 'q) (casT1 n))) LNum))))

;;hago la union con dos af
;;af1: afd% o afn% o afe%;; af2: afd% o afn% o afe%;
;;devuelve un afe%
(define af-union
  (λ (af1 af2)
    (let* ([tam1 (cardinalidad (send af1 estados))]
           [tam2 (cardinalidad (send af2 estados))]
           ;;------renombro los dos af-----------------
           [af1* (renombrar-af af1 (crea-estados (range 1 (+ tam1 1))))]
           [af2* (renombrar-af af2 (crea-estados (range (+ tam1 1) (+ tam1 tam2 1))))]
           [nuevoEst (casT2 (string-append (casT1 'q) (casT1 (+ tam1 tam2 1))))]
           [estados (append* (send af1* estados) (send af2* estados) (list (list nuevoEst)))]
           [estini nuevoEst]
           [sim (union (send af1* simbolos) (send af2* simbolos))]
           [acep (append (send af1* aceptores) (send af2* aceptores))]
           [est1sim2 (map (λ (es) (append es '(()))) (producto-cartesiano (send af1* estados) (send af2* simbolos)))]
           [est2sim1 (map (λ (es) (append es '(()))) (producto-cartesiano (send af2* estados) (send af1* simbolos)))]
           [traneps (map (λ (e) (list e 'ε '())) (append (send af1* estados) (send af2* estados)))]
           [tranepsvacio (map (λ (s) (list nuevoEst s '())) sim)]
           [transi (map (λ (t) (if (list? (last t)) t (list (car t) (cadr t) (list (last t))))) 
                        (append* (send af1* transiciones) (send af2* transiciones) 
                                 est1sim2 est2sim1 traneps  tranepsvacio
                                 (list (list (list estini 'ε (list (send af1* edoIni) (send af2* edoIni)))))))])
      
      (new afe%  [EST estados] [SIM sim] [EIN estini] [ACE acep] [TRA transi])
      )))


;;##############################################################
;;hago la concatenacion con dos af
;;af1: afd% o afn% o afe% ;; af2: afd% o afn% o afe% ;
;;devuelve un afe%
(define af-concatenar
  (λ (af1 af2)
    (let* ([tam1 (cardinalidad (send af1 estados))]
           [tam2 (cardinalidad (send af2 estados))]
           ;;-----------renombro los dos af-----------------
           [af1* (renombrar-af af1 (crea-estados (range 1 (+ tam1 1))))]
           [af2* (renombrar-af af2 (crea-estados (range (+ tam1 1) (+ tam1 tam2 1))))]
           [estados (append (send af1* estados) (send af2* estados))]
           [estini (send af1* edoIni)]
           [estini2 (send af2* edoIni)]
           [sim (union (send af1* simbolos) (send af2* simbolos))]
           [acep (send af2* aceptores)]
           [est1sim2 (map (λ (es) (append es '(()))) (producto-cartesiano (send af1* estados) (send af2* simbolos)))]
           [est2sim1 (map (λ (es) (append es '(()))) (producto-cartesiano (send af2* estados) (send af1* simbolos)))]
           [traneps (map (λ (e) (list e 'ε '())) (append (send af1* estados) (send af2* estados)))]           
           [nuevaTr (map (λ (ac) (list ac 'ε estini2)) (send af1* aceptores))]
           [transi (map (λ (t) (if (list? (last t)) t (list (car t) (cadr t) (list (last t))))) 
                        (append* (send af1* transiciones) (send af2* transiciones) traneps est1sim2 est2sim1 (list nuevaTr)))])
      
     (new afe%  [EST estados] [SIM sim] [EIN estini] [ACE acep] [TRA transi]))))
           


;;###########################################################
;;recibe un a
;,crea un afe% que solo tiene un estado y es aceptor
(define crea-afe
  (λ (af)
    (let* ([est (list 'q0)]
           [sim (send af simbolos)]
           [tra (map (λ (s) (list (car est) s '())) sim)]
           [acep est]
           [edini (car est)]
           [traneps (append tra (list (list (car est) 'ε '())))])
      
      (new afe%  [EST est] [SIM sim] [EIN edini] [ACE acep] [TRA traneps]))))

      
;;recibe un af: afd% o afn% o afe% ;;
;;k un nuemro no negativo
;;devuelve la k-esima concatenacion de af consigo mismo
(define af-potencia-aux
   (λ (af af2 n k)
     (cond ((= n k) af)
           (else (af-potencia-aux (af-concatenar af af2) af2 (+ n 1) k)))))

(define af-potencia
  (λ (af k)
    (cond((= k 0) (crea-afe af))
         ((= k 1) af)
         (else (af-potencia-aux af af 1 k)))))
;;##################################################

(define af-nKleene*-aux
  (λ (af af2 k)
    (cond((= k 0) (af-union (af-potencia af k) af2))
         (else (af-nKleene* (af-union (af-potencia af k) af2 ) (- k 1))))))

;;recibe un af: afd% o afn% o afe% ;;
;;k un nuemro no negativo
;;devuelve la union de las k-potencias af
(define af-nKleene*
  (λ (af k)
    (af-nKleene*-aux af af k)))

;;###############################################################


;;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   3er PARCIAL  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;calse de Gramatica Regular
(define gr%
  (class object%
    (init V T R S0) 
    (define var V); ;conjunto de variables
    (define ter T); list/terminales
    (define reg R); list/reglas
    (define sin S0); simbolo inicial
    
    (super-new)
    ; .... Metodos publicos y privados
    (define/public (variables) var)
    (define/public (terminales) ter)
    (define/public (reglas) reg)      
    (define/public (s0) sin)
    
    ;;metodo auxiliar para aplicar regla
    ;;V: variable;;S: simbolos constantes;;T palabra;;res: '()
    (define/private (aplica-regla-aux V S T res)
      (cond ((empty? T) res)
            ((equal? V (car T)) (aplica-regla-aux V S (cdr T) (append res S)))
            (else (aplica-regla-aux V S (cdr T) (append res (list (car T)))))))
    ;;aplica regla
    ;;R: regla '(B (b a));; T: palabra '(a a B A)
    (define/public (aplica-regla R T)   
        (aplica-regla-aux (car R) (cadr R) T '()))
    
    ;;verifica si T* pertenece a la gramatica
    ;T* :palabra '(a a b a A)
    ;;devuelve #t o #f si es derivable o no lo es 
    (define/public (es-derivable T*)      
      (Kleene*? T* ter))
    ))

(define gr1 (new gr% [V '(B A)] [T '(a b)] [R '(B (b A))] [S0 'B]))

(define Tr (new afn% [EST '(S T U Z)] [SIM '(a b)] [EIN 'S] [ACE '(Z)] [TRA '((S a (S)) (S b (T)) (T a (S)) (T b (U))
                                                                                      (U a (U Z)) (U b (U Z)) (Z a ()) (Z b ()))]))