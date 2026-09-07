#lang racket
(require racket/match)

(provide empty-queue)
(provide make-queue)
(provide queue-empty?)
(provide rotate)             ; pentru testare
(provide enqueue)
(provide dequeue)
(provide top)

(provide (struct-out queue)) ; pentru testare

;; În etapa 3 am implementat TDA-ul queue asigurând cost amortizat O(1)
;; atât pentru enqueue cât și pentru dequeue.
;; Am reprezentat coada ca pe o colecție de 2 stive:
;; - stiva left: pentru scoaterea de elemente la dequeue 
;; - stiva right: pentru adăugarea de elemente la enqueue 
;;
;; Singurul caz în care o operație nu era O(1) era dequeue când stiva left era goală.
;; Un asemenea dequeue era O(n), din cauza mutării tuturor elementelor din right în left.
;; Ne dorim să îmbunătățim costul operației dequeue pe cazul cel mai
;; defavorabil, de la O(n) la O(1).
;;
;; Soluția: păstrăm reprezentarea cu 2 stive, dar ne asigurăm că, la dequeue, 
;; stiva left nu este niciodată goală, menținând invariantul:
;;        size(left) ≥ size(right)
;; Oricând o operație duce la violarea invariantului, efectuăm o rotație:
;;        <left, right>   devine   <left ++ (reverse right), []>
;; Cât timp stivele sunt liste Racket, o rotație are complexitate O(n),
;; cauzată de append și de reverse. Există o reprezentare mai bună?
;;
;; Da! Vom reprezenta stiva left ca pe un flux.
;; Spre deosebire de append (notat aici ++) pe liste (O(n)),
;; append pe fluxuri este o operație incrementală:
;; - elementele din rezultat sunt furnizate unul câte unul, atunci când este nevoie
;; - ex: A = fluxul [1,2,3,4,5], reprezentat ca (stream-cons 1 <calcul-întârziat-rest>)
;;       B = un flux oarecare
;;       A ++ B va fi (stream-cons 1 <calcul-întârziat-append-între-restA-și-B>)
;;   (acest rezultat se obține în timp O(1))
;; Astfel rezolvăm complexitatea operației append din expresia
;;       "left ++ (reverse right)"
;;
;; Cum rezolvăm complexitatea operației reverse din aceeași expresie?
;; Cum append este deja o operație incrementală, ideea este să efectuăm câte 
;; un pas de reverse de fiecare dată când efectuăm un pas de append.
;; Acest truc termină ambele operații cam în același timp,
;; întrucât rotațiile se declanșează când right devine mai lungă decât left,
;; adică size(right) = size(left) + 1.
;; Amintiți-vă codul pentru append, respectiv reverse cu recursivitate pe coadă:
;; (define (append A B)                     (define (reverse L Acc)
;;   (if (null? A)                            (if (null? L)
;;       B                                        Acc
;;       (cons (car A) (append (cdr A) B))))      (reverse (cdr L) (cons (car L) Acc))))
;;
;; Implementăm o rotație conform axiomelor următoare
;; (observați fuziunea de append și reverse):
;; rotate([], [y], Acc)        = y : Acc                    
;; rotate((x:xs), (y:ys), Acc) = x : rotate(xs, ys, y : Acc)
;; Obs: 
;; - x : rotate(...) reprezintă un pas de append ( : înseamnă cons)
;; - y : Acc         reprezintă un pas de reverse


; Structura queue nu se modifică.
; Ceea ce se modifică este implementarea câmpului left
; - din listă, left devine flux
; - acest lucru nu este vizibil în definiția structurii queue,
;   ci în implementarea operațiilor tipului 
(define-struct queue (left right size-l size-r) #:transparent) 


; RESTRICȚII (maxim 50p)
;  - Implementați funcțiile conform specificației
;  - O funcție implementată diferit se depunctează în totalitate.


; TODO 1 (5p)
; Definiți valoarea care reprezintă o coadă goală.
(define empty-queue
  (make-queue empty-stream '() 0 0))
; *** functie analoaga etapei 3

; TODO 2 (5p)
; Implementați o funcție care verifică dacă o coadă este goală.
(define (queue-empty? q)
  (if (and (null? (queue-right q)) (stream-empty? (queue-left q))) 
      #t
      #f))
; *** functia este analoaga etapei 3

; TODO 3 (10p)
; Implementați funcția rotate, conform axiomelor de mai sus.
; Atenție: ce tip trebuie să aibă Acc?
(define (rotate left right Acc)
  (if (stream-empty? left)
      (stream-append right Acc)
      (if (null? right)
          (stream-cons (stream-first left) (rotate (stream-rest left) right Acc))
          (stream-cons (stream-first left) (rotate (stream-rest left) (cdr right) (stream-cons (car right) Acc))))))
; Raspuns *** Acc trebuie sa fie un stream pentru ca la cazul de baza facem y : Acc
; *** (si deci am stabilit ca append trebuie sa fie O(1) si asta e doar pentru stream uri
; *** unde rotate x = left, y = right
; *** functia a fost facuta respectand axiomele 

; TODO 4 (10p)
; Implementați o funcție care adaugă un element la
; sfârșitul unei cozi. Întoarceți coada actualizată.
; Atenție: în urma adăugării, poate fi necesară o rotație!
(define (enqueue x q)
  (let ((q2 (struct-copy queue q
                         [right (cons x (queue-right q))]
                         [size-r (+ (queue-size-r q) 1)])))
    (if (>= (queue-size-l q2) (queue-size-r q2))
        q2 ;* se respecta  inv
        (struct-copy queue q
                     [left (rotate (queue-left q2) (queue-right q2) empty-stream)]
                     [size-l (+ (queue-size-l q2) (queue-size-r q2))]
                     [right '()]
                     [size-r 0]))))

; ***OBS: rotatia e necesara daca NU se respecta invariantul size(left) ≥ size(right)
; *** daca nu se respecta invariantul, in left punem rezultatul rotatiei, si acum right e lista vida

; TODO 5 (10p)
; Implementați o funcție care scoate primul element
; dintr-o coadă nevidă. Întoarceți coada actualizată.
; Obs: dequeue pe coada vidă este firesc să dea eroare.
; Atenție: în urma extragerii, poate fi necesară o rotație!
(define (dequeue q)
  (let ((q2 (struct-copy queue q
                         [left (stream-rest (queue-left q))]
                         [size-l (- (queue-size-l q) 1)])))
    
    (if (>= (queue-size-l q2) (queue-size-r q2))
        q2 ;* se respecta  inv
        (struct-copy queue q
                     [left (rotate (queue-left q2) (queue-right q2) empty-stream)]
                     [size-l (+ (queue-size-l q2) (queue-size-r q2))]
                     [right '()]
                     [size-r 0]))))

; ***OBS: pentru ca avem invariantul, asta inseamna ca putem sa scoatem din left fara sa mai facem verificari
; *** In schimb, daca campurile au lungimi egale, atunci nu se respecata invariantul, si deci reparam ca in ex anterior


; TODO 6 (10p)
; Implementați o funcție care obține primul element
; dintr-o coadă nevidă. Întoarceți elementul.
; Obs: top pe coada vidă este firesc să dea eroare.
(define (top q)
  (stream-first (queue-left q)))

; *** Invariantul ne garanteaza (asa cum putem sterge oricand) si ca putem simplu sa luam primul element din left
