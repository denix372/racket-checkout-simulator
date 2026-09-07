#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Este necesar să implementați întâi
;;          TDA-ul queue în fișierul queue.rkt.
;; Reveniți la acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Se modifică însă implementarea câmpului queue:
; - în loc de listă, acesta va fi o structură de tip queue
; - modificarea nu este vizibilă în definiția structurii,
;   ci în implementarea operațiilor tipului counter
(define-struct counter (index tt et queue) #:transparent)


; TODO 6 (20p)
; Actualizați funcțiile de mai jos conform cu 
; noua reprezentare a cozii de persoane.
; Elementele cozii rămân perechi (nume . nr_produse).
; RESTRICȚII (5p per abatere)
;  - Respectați "bariera de abstractizare", adică 
;    operați cu coada folosind exclusiv interfața:
;    - empty-queue
;    - queue-empty?
;    - enqueue
;    - dequeue
;    - top
; Obs: Doar câteva funcții necesită actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))
; *** respectand bariera de abstractizare, folosim empty-queue implementata in celalalt fisier

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

;*** functiile anterioare sunt preluate din etapa2

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

; *** am rescris functia de la etapa2 in care am folosit operatiile TDA ului de enqueue si queue-empty?
; ** se observa ca functia add-to-counter inca se poate aplica partial

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
  (define sum-of-all
    (apply + (map cdr
                  (append (queue-left (counter-queue C))
                          (queue-right (counter-queue C))) )))
  
  (define new-tt (- sum-of-all (cdr first-person)))
  ; *** unde (apply + (map cdr (liste_combinate))) reprezinta suma elementelor
  ; *** (desfacem strcutura queue intr-o lista cu tt-uri)
  ; *** noul tt e definit prima suma lor (datorita faptului ca nu exista intarzieri)
  (define new-et (if (queue-empty? (dequeue (counter-queue C)))
                     0
                     (cdr (top (dequeue (counter-queue C))))))
  ; **** unde (top (dequeue (counter-queue C))) e a 2-a persoana din coada (nu mai face cadr)
                           
  (struct-copy counter C
               [tt new-tt]
               [et new-et]
               [queue (dequeue (counter-queue C))]))

; ** in etapa trecuta am facut (apply + (map cdr (counter-queue C)))
; *** aceasta implementare nu mai e valida intrucat queue acum este o structura cu 2 liste
; *** putem sa unificam cele 2 liste sa obtinem o lista cu toate elementele din care extragem items si aflam suma

; TODO 7 (10p)
; Implementați o funcție care calculează starea
; unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp
; nu iese nimeni din coadă, deci se modifică
; doar câmpurile tt și et.
; Este responsabilitatea utilizatorului să nu apeleze
; funcția cu minutes > et și coadă nevidă.
; La casele fără clienți, este responsabilitatea
; voastră să nu produceți timpi negativi.
(define ((pass-time-through-counter minutes) C)
  (define new-tt (if (< (- (counter-tt C) minutes) 0) 0 (- (counter-tt C) minutes)))
  (define new-et (if (< (- (counter-et C) minutes) 0) 0 (- (counter-et C) minutes)))
  (struct-copy counter C
               [tt new-tt]
               [et new-et]))
; *** daca se produce timp negativ, v-om evalua noile campul la 0
; *** se observa ca functia pass-time-through-counter poate fi aplicata partial

; TODO 8 (60p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri:
;   3 moștenite din etapa 2:
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
;   plus noutatea:
;   - <x> - actualizează starea caselor conform cu trecerea a <x> minute
;           de la ultima cerere (afectează câmpurile tt, et, queue)
; Obs: Cererile (remove-first) din etapa 2 sunt înlocuite de un mecanism  
; mai sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul procesează cererile în ordine, astfel:
; - nicio modificare pentru cererile moștenite din etapa 2
; - când timpul prin sistem avansează cu <x> minute, starea caselor
;   se actualizează pentru a reflecta trecerea timpului;
;   ieșirile clienților din coadă se rețin în ordine cronologică.
; Funcția serve întoarce o pereche cu punct între:
; - lista clienților care au părăsit magazinul, sortată cronologic
;   - elementele listei au forma (index_casă . nume)
;   - când mai mulți clienți ies simultan, sortați după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Sugestii:
; - gestionați cronologia folosind în mod repetat funcția min-et 
; - pentru a menține lista clienților plecați, definiți o funcție ajutătoare
; (cu un parametru în plus față de serve), pe care serve doar o apelează.
; RESTRICȚII (5p per abatere)
;  - Folosiți minim un let și un let* (care nu ar putea fi let). (2*5p)
;  - Respectați "bariera de abstractizare" oricând operați cu tipul queue.
(define (serve requests fast-counters slow-counters)
  ; *** functia serve2 e o functia ajutatoare pe care doar serve o apeleaza
  ; *** este facuta cu named let, are aceeasi parametri ca functi cu aceleasi
  ; ** initializari si parametrul crono-people care pleaca de la null (0 persoane plecate)
  (let serve2 ((requests requests)
               (fast-counters fast-counters)
               (slow-counters slow-counters)
               (crono-people '()))
    (if (null? requests)
        (cons (reverse crono-people) (append fast-counters slow-counters))
        ; ** daca nu mai avem request uri, returnam lista de persoane in ordine cronologica si cele 2 tipuri de case
        
        (match (car requests)
          [(list 'delay index minutes)
           (serve2 (cdr requests)
                   (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)
                   (update (et+ minutes) (update (tt+ minutes) slow-counters index) index)
                   crono-people)
           ]
          [(list 'ensure average)
           (serve2 (cdr requests)
                   fast-counters
                   (ensure-the-balance fast-counters slow-counters average)
                   crono-people)
           ;*** functia ensure balance adauga case in slow-counters pana cand media aritmetica a tt-urilor
           ;*** devine <= decat numarul dat "average"
           ]
          
          [(list name n-items)
           (if (<= n-items ITEMS)
               (serve2 (cdr requests)
                       (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters))))
                       (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters))))
                       crono-people)
               (serve2 (cdr requests)
                       fast-counters
                       (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))
                       crono-people))

           ; *** analog temei 1, daca n-items <= ITEMS, cautam index ul in toate casele (listele concatenate)
           ; *** modificam cu update lista si folosim functia add-to-counter sa adaugam persoana
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
             
           ]))))

; ------------------------Functii preluate din etapa 2------------------------------------

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
  (if (<= (avg (append fast slow)) average)
      slow
      (ensure-the-balance fast (append slow (list (empty-counter (next-index slow)))) average)))

;*** OBSERVATIE: am folosit 3 functionale predefinte din rakcet (filter[1] si map si apply [2])

;------------------- Functie helper etapa 3 ----
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
; *** altfel, scoatem persoana cu cel mai mic et. (Intr-ucat dupa trecerea a min-et minute, acea casa are exit ul 0)
; *** intrucat nu a trecut tot timpul x, scatem min-et ul cel mai mic din  x si continuam sa scaotem persoane din lista
; *** persoana scoasa (salvada in campul "person" este adaugata in crono-people, se observa ca
; *** pentru ca scoatem persoane dupa min-et, ele vor fi puse in lista in ordine cronologica (si pentru ca folosim cons, ele sunt in ordine inversa)

; *** [1] Am folosit un let care salveaza in campul "clients" lista de case, acelea care nu au coada goala
; *** [2] Am folosit un let* care nu poate fi let* pentru ca in index si litera-min salvam componentele perechii returnate de min-et (index . et)


       
