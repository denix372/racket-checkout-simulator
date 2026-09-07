#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

; Ca sa mearga TODO 0 am trebui sa ne redeclaram noi iar definitia structurii
(define-struct counter (index is-open tt et queue) #:transparent)
;-------------------------------------------------------------------------------

; TODO (0p)
; Aveți libertatea să vă structurați programul cum doriți
; (dar cu restricțiile de mai jos), astfel încât
; funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - va exista în continuare funcția (empty-counter index)
; - veți reprezenta cozile folosind noul TDA queue
(define (empty-counter index)
  (make-counter index #t 0 0 empty-queue))
; *** Functia veche era asa (make-counter index 0 0 empty-queue))
  
; TODO 7 (70p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 6 tipuri de cereri:
;   4 moștenite din etapa 3:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă deschisă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al caselor deschise depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
;   plus 2 noi:
;   - (close <index>) - închide casa cu indexul <index> (casa există deja)
;   - (open <index>) - deschide casa cu indexul <index> (casa există deja)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa DESCHISĂ cu tt minim la care are voie;
;   se garantează că persoana poate fi distribuită la o casă
; - nicio modificare pentru situația când o casă suferă o întârziere
; - dacă tt-ul mediu pentru toate casele DESCHISE > <average>,
;   adaugă case slow până când media <= <average>
; - nicio modificare în modelarea trecerii timpului
; - o casă care se închide nu mai primește clienți noi și:
;   - primul client (dacă există) își continuă treaba la această casă
;   - restul clienților se redistribuie la celelalte case,
;     în ordinea în care erau așezați la coadă
; - o casă care se deschide redevine disponibilă pentru clienți
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista cozilor nevide în starea finală, sortată după indexul casei
;   - elementele listei au forma (index_casă . coadă) (coada este de tip queue)


;-------------------------------------------------------------------------------
; O sa preluam functiile din etapa 3

(define (update f counters index)
  (update-helper f counters '() index))

(define (update-helper f L acc index)
  (cond
    ((null? L) (reverse acc))
    ((equal? (counter-index (car L)) index)
     (update-helper f (cdr L) (cons (f (car L)) acc) index))
    (else
     (update-helper f (cdr L) (cons (car L) acc) index))))

(define tt+
  (λ (minutes)
    (λ (C)
      (struct-copy counter C [tt (+ (counter-tt C) minutes)] ))))

(define et+
  (λ (minutes)
    (λ (C)
      (struct-copy counter C [et (+ (counter-et C) minutes)] ))))

(define is-open+
  (λ (flag)
    (λ (C)
      (struct-copy counter C [is-open flag] ))))


(define ((add-to-counter name items) C) ; testată de checker
  (if (queue-empty? (counter-queue C)) ; *** folosim queue-empty? in loc de null?            
      (struct-copy counter C
                   [tt (+ (counter-tt C) items)]
                   [et (+ (counter-et C) items)]
                   ;** [queue (append (counter-queue C) (list (cons name n-items)))])
                   ;** x = (cons name n-items) elementul adaugat
                   ;** folosim enqueue in loc de append
                   [queue (enqueue (cons name items) (counter-queue C))])

      (struct-copy counter C
                   [tt (+ (counter-tt C) items)]
                   [queue (enqueue (cons name items) (counter-queue C))])))   ; nu modificați signatura!

(define min-field
  (λ (access-field)
    (λ (counters)
      (cond
        ((null? (cdr counters))
         (cons (counter-index (car counters)) (access-field (car counters))))

        ((< (access-field (second counters)) (access-field (first counters)))
         ((min-field access-field) (cdr counters)))

        ((and (= (access-field (second counters)) (access-field (first counters)))
              (< (counter-index (second counters)) (counter-index (first counters)))) 
         ((min-field access-field) (cdr counters)))

        (else
         ((min-field access-field) (cons (car counters) (cddr counters))))))))

(define min-tt (min-field counter-tt))
(define min-et  (min-field counter-et))

(define (remove-first-from-counter C)   ; testată de checker
  (define first-person (top (counter-queue C))) ; *** prima persoana acum este definita de functia top
  (define queue-elements (rotate (queue-left (counter-queue C)) (queue-right (counter-queue C)) empty-stream))
  (define sum-of-all (sum-of-stream queue-elements))
  ; (apply + (map cdr
  ;              (append (queue-left (counter-queue C))
  ;                     (queue-right (counter-queue C))) )))
  
  (define new-tt (- sum-of-all (cdr first-person)))
  ; *** unde (apply + (map cdr (liste_combinate))) reprezinta suma elementelor
  ; *** (desfacem structura queue intr-o lista cu tt-uri)
  ; *** noul tt e definit prima suma lor (datorita faptului ca nu exista intarzieri)
  (define new-et (if (queue-empty? (dequeue (counter-queue C)))
                     0
                     (cdr (top (dequeue (counter-queue C))))))
  ; **** unde (top (dequeue (counter-queue C))) e a 2-a persoana din coada (nu mai face cadr)

  (struct-copy counter C
               [tt new-tt]
               [et new-et]
               [queue (dequeue (counter-queue C))]))

; *Aici functia se modifica la accesul cand facem sum-of-all
;** NU exista o functie gen stream-apply :(, o sa facem functia de mana
;** o sa desfacem si fiecare element in propriul tt sa putem calcula suma
(define (sum-of-stream s)
  (if (stream-empty? s)
      0
      (+ (cdr (stream-first s)) (sum-of-stream (stream-rest s)))))

(define ((pass-time-through-counter minutes) C)
  (define new-tt (if (< (- (counter-tt C) minutes) 0) 0 (- (counter-tt C) minutes)))
  (define new-et (if (< (- (counter-et C) minutes) 0) 0 (- (counter-et C) minutes)))
  (struct-copy counter C
               [tt new-tt]
               [et new-et]))

; - 1 functie care ne ajuta pentru remove-first -
; *** functie care dintr-o lista de case, returneaza acele case care au clienti folosind fileter [1]
; *** evident, ne asteptam ca daca nicio casa nu are clienti, sa returneze lista nula '()
(define (list-of-clients L)
  (filter (λ (C) (if (queue-empty? (counter-queue C)) #f #t)) L))

; - 3 functii care ne ajuta pentru ensure
;*** functie care determina urmatorul index pentru o lista de care sortate crescator
;*** deci e ultimul index + 1
(define (next-index L)
  (+ 1 (counter-index (car (reverse L)))))


; *** functia avg calculeaza media aritmetica cu virgula a listei L
; *** folosim map pentru a extrage din lista de care doar tt ul si apply pentru a calcula suma [2]
(define (avg L)
  (if (null? L)
      0
      (/ (apply + (map (λ (C) (counter-tt C)) L)) (length L))))
 
;*** folosind cele ultimele 2 functii, adaugam recursiv pana cand ajungem cu media mai mica decat nr dat
; *** o casa noua este reprezentata de prima functie a etapei, (empty-counter index)
(define (ensure-the-balance fast slow average)
  (if (<= (avg (filter-open-couters (append fast slow))) average)
      slow
      (ensure-the-balance fast (append slow (list (empty-counter (next-index slow)))) average)))

; Functie care scoate persoanele din case in ordine cronologica si le stocheaza in crono-people
; Returneaza o pereche de 3 campuri modificate (fast-counters slow-counters crono-people)
(define (remove-min-et fast-counters slow-counters x crono-people)
  (let ((clients (list-of-clients (append slow-counters fast-counters)))) ; ** [1]
    (if (null? clients)
        (list
         (map (pass-time-through-counter x) fast-counters)
         (map (pass-time-through-counter x) slow-counters)
         crono-people)
        
        (let* ((first-min (min-et (list-of-clients (append slow-counters fast-counters)))) ; ** [2]
               (index (car first-min))
               (literal-min (cdr first-min))
               (person (top (counter-queue (car (filter (λ (cnt) (if (= (counter-index cnt) index) #t #f)) (append fast-counters slow-counters)))))))
          (cond
            ((> (cdr first-min) x)
             (list
              (map (pass-time-through-counter x) fast-counters)
              (map (pass-time-through-counter x) slow-counters)
              crono-people)) ;*** best case, min-et ul e mai mic deci doar scadem din toate casele, crono-people nu se modifica

            (else (remove-min-et
                   (update remove-first-from-counter (map (pass-time-through-counter literal-min) fast-counters) index)
                   (update remove-first-from-counter (map (pass-time-through-counter literal-min) slow-counters) index)
                   (- x literal-min)
                   (cons (cons index (car person)) crono-people))))))))


; -------------------- Functii noi pentru aceasta etapa ------------------------
; *** Personal, pentru am separa logica pentru case, am ales sa introduc eu un nou camp
; *** in structura unei case "is-open" care sa fie true sau false in functie de starea casei
; ** avand acest camp nou, ne e usor sa facem functia filter-open-couters care selecteaza
; ** acele sase care au campul is-open true dintr-o Lista de case

(define (filter-open-couters L)
  (filter (λ (counter) (counter-is-open counter)) L))

; *** functie care primeste o casa si ia toate persoanele din queue si formeaza o lista de cereri de punere in case
; *** de asemenea returneaza case cu o singura persoana in queue
; *** functia returneaza o pereche intre nouna casa si lista de cereri
(define (people-redistribution C acc)
  (let* ((first-person (top (counter-queue C)))
        (q (dequeue (counter-queue C)))
        (new-counter (struct-copy counter C
               [tt (counter-et C)]
               [queue (enqueue first-person empty-queue)])))
    
    (let loop ((q q) (acc acc))
      (if (queue-empty? q)
          (cons new-counter (reverse acc))
          (let* ((first-person (top q))
                 (request (list (car first-person) (cdr first-person))))
            (loop (dequeue q) (cons request acc)))))))

; *** Functie helper care primeste o Lista de care, si modifica o casa cu functia precedenta
; *** Returneaza o pereche cu punct intre noua lista ce case (acceasi lista, doar cu o casa modificata) si lista de cereri
(define (counter-redistribution counters index new_list new_req)
  (cond
    ((null? counters) (cons (reverse new_list) new_req))
    ((and (= (counter-index (car counters)) index) (not (queue-empty? (counter-queue (car counters)))))
     ;*** daca gasim casa SI ARE persone la coada (daca nu are persoane nu avem ce scoate)
     (let* ((result (people-redistribution (car counters) '()))
            (new_counter (car result))
            (requests (cdr result)))
       (counter-redistribution (cdr counters) index (cons new_counter new_list) requests)))
    (else (counter-redistribution (cdr counters) index (cons (car counters) new_list) new_req))))


; ------------------------- Logica functiei server -----------------------------


(define (serve requests fast-counters slow-counters)
  ; *** functia serve2 e o functia ajutatoare pe care doar serve o apeleaza
  ; *** este facuta cu named let, are aceeasi parametri ca functi cu aceleasi
  ; ** initializari si parametrul crono-people care pleaca de la null (0 persoane plecate)
  (let serve2 ((requests requests)
               (fast-counters fast-counters)
               (slow-counters slow-counters)
               (crono-people '()))
    (if (null? requests)
        (cons (reverse crono-people)
              (filter (λ (pair) (not (queue-empty? (cdr pair))))
                      (map (λ (counter) (cons (counter-index counter) (counter-queue counter))) (append fast-counters slow-counters))))
        ; ** daca nu mai avem request uri, returnam lista de persoane in ordine cronologica si perechile cu punct intre
        ; ** index. coada filtrand cozile vide
        
        (match (car requests)
          [(list 'delay index minutes)
           (serve2 (cdr requests)
                   (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)
                   (update (et+ minutes) (update (tt+ minutes) slow-counters index) index)
                   crono-people)
           ] ; *** nu s-a modificat in aceasta etapa
          
          [(list 'ensure average)
           (serve2 (cdr requests)
                   fast-counters
                   (ensure-the-balance fast-counters slow-counters average)
                   crono-people)
           ;*** nu s-a modificat in aceasta etapa, dar acum facem media pe casele deschise
           ]
          
          
          [(list 'close index)
           (let* ((fast-rest (counter-redistribution fast-counters index '() '()))
                  (new-fast (car fast-rest))
                  (new_req (cdr fast-rest))
                  (slow-rest (counter-redistribution slow-counters index '() '()))
                  (new-slow (car slow-rest))
                  (new_req2 (if (null? new_req) (cdr slow-rest) new_req)))
             ;*** facem operatiile necesarae sa obtinem noile liste de care si lista de cereri
             ; *** se observa ca daca nu se gaseste casa, se returneaza acelasi lucru si lista de cereri new_req e nula
             
             (serve2 (append new_req2 (cdr requests))
                     (update (is-open+ #f) new-fast index)
                     (update (is-open+ #f) new-slow index)
                     crono-people))
           ] ;*** Redistribuire inseamna ca am pus persoanele in lista de requests inaintea celorlalte operatii
          ; *** nu uitam sa marcam casele ca fiind inchise
          [(list 'open index)
           (serve2 (cdr requests)
                   (update (is-open+ #t) fast-counters index)
                   (update (is-open+ #t) slow-counters index)
                   crono-people)
           ] ; ** analog marcam casele fiind deschise

          [(list name n-items)
           (if (<= n-items ITEMS)
               (serve2 (cdr requests)
                       (update (add-to-counter name n-items) fast-counters (car (min-tt (filter-open-couters (append fast-counters slow-counters)))))
                       (update (add-to-counter name n-items) slow-counters (car (min-tt (filter-open-couters (append fast-counters slow-counters)))))
                       crono-people)
               (serve2 (cdr requests)
                       fast-counters
                       (update (add-to-counter name n-items) slow-counters (car (min-tt (filter-open-couters slow-counters))))
                       crono-people))

           ; Nu se modifica fata de etapa 3, in schimb cauta acum min-tt ul in casele deschise
           ]
          
          [x
           ; *** voi defini o functie ajutatoare "remove-min-et" care imi actualizeaza casele
           ; *** aceastra returneaza o lista cu 3 elemente reprezentand
           ; *** starea cele 2 tipuri de case si lista de persoane dupa trecerea timpului x
           (let* ((result (remove-min-et fast-counters slow-counters x crono-people))
                  (fast-pass (first result))
                  (slow-pass (second result))
                  (crono-pass (third result)))
             
             (serve2 (cdr requests) fast-pass slow-pass crono-pass))
             
           ] ;*** nu se modifica fata de etapa3
          
          ))))

