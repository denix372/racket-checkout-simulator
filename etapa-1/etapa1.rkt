#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un magazin.
;; C1 acceptă doar clienți care au cumpărat maxim ITEMS produse
;; (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întârzieri (delay).
;; La un moment dat, la fiecare casă există
;; 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă
;; timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei +
;; întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO 1 (10p)
; Implementați o funcție care întoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat
; o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '() ))
; *** am definit structura cu make-counter, index ul dat, tt = 0 si queue = '() adica lista nula



; TODO 2 (10p)
; Implementați o funcție care crește tt-ul unei case
; cu un număr dat de minute.
(define (tt+ C minutes)
  (struct-copy counter C [tt (+ (counter-tt C) minutes)] ))
; *** am facut apelul la camp cu counter-tt C, si am adunat minutes la el
; *** am returnat o copie a structuri in care am modificat doar campul cerut (modificarea si accesul sunt luate din tutorial.rkt



; TODO 3 (20p)
; Implementați o funcție care primește o listă nevidă 
; de case și întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt,
; este preferată casa cu indexul cel mai mic
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe coadă.
(define (min-tt counters)
  ; *** initializam acumulatorul cu primul element din countets
  (min-tt-helper counters (cons (counter-index (car counters)) (counter-tt (car counters)))))

; *** daca elementul curent (car counters) are tt ul mai mic decat acc ul nostru, atunci il updatam
; *** daca tt sunt egale, il luam pe ala care are cel mai mic index
(define (min-tt-helper counters acc)
  (cond
    ((null? counters)
     acc)
    
    ((< (counter-tt (car counters)) (cdr acc))
     (min-tt-helper (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters)))))

    ((and (= (counter-tt (car counters)) (cdr acc))
          (< (counter-index (car counters)) (car acc)))
     (min-tt-helper (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters)))))

    (else
     (min-tt-helper (cdr counters) acc))))
      
; TODO 4 (20p)
; Implementați aceeași funcționalitate de mai sus,
; cu recursivitate pe stivă.
; RESTRICȚII (20p):
;  - Folosiți recursivitate pe stivă.
(define (min-tt-stack counters)
  (cond
    ((null? (cdr counters))
     (cons (counter-index (car counters)) (counter-tt (car counters))))

    ((< (counter-tt (car counters))
        (cdr (min-tt-stack (cdr counters))))
     (cons (counter-index (car counters)) (counter-tt (car counters))))

    ((and (= (counter-tt (car counters))
             (cdr (min-tt-stack (cdr counters))))
          (< (counter-index (car counters))
             (car (min-tt-stack (cdr counters)))))
     (cons (counter-index (car counters)) (counter-tt (car counters))))

    (else
     (min-tt-stack (cdr counters)))))
  
; *** parcurg lista si pastez primul element ca fiind elementul cerut
; *** am eliminat din lista orice alt element care are tt ul mai mic, sau index ul mai mare in caz egal
; *** cazul de baza e cand lista mai are un element (exact ce trebuie)
     


; TODO 5 (10p)
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei,
; n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii
; (name . n-items) la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (struct-copy counter C
               [tt (+ (counter-tt C) n-items)]
               [queue (append (counter-queue C) (list (cons name n-items)))]))
; *** adunam n-items la tt ca asa zice la inceputu fisierului si punem (cons name n-items) la final
; *** din checker mi-am dat seama ca (name n-items) trebuie sa fie si ea o lista pe care o pun in queue
  


; TODO 6 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - așază persoana <name> la coadă la o casă
; - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (conform logicii implementate de min-tt)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)
  
  ; Puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele
  ;   requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; Puteți să vă definiți funcții ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți testa fiecare funcție imediat ce ați implementat-o
  ; Nu este obligatoriu să definiți funcții ajutătoare.

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes)

         (cond
           ((equal? index 1)
            (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4))
           
           ((equal? index 2)
            (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4))
           
           ((equal? index 3)
            (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4))
           
           ((equal? index 4)
            (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes)))
           
           (else
            (serve (cdr requests) C1 C2 C3 C4)))]
        ; *** daca e delay, atunci pentru fiecare casa C1 ...C4 adun minutele folosind functia tt+ de la TODO2
        [(list name n-items)
         (if (<= n-items ITEMS)
             (cond
               ((= (car (min-tt (list C1 C2 C3 C4))) 1)
                (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4))

               ((= (car (min-tt (list C1 C2 C3 C4))) 2)
                (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))

               ((= (car (min-tt (list C1 C2 C3 C4))) 3)
                (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))

               (else
                (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))))
             ;*** daca n-items <= ITEMS atunci putem sa cautam si in C1 si pentru fiecare caz de casa, adaugam persoana la acea casa
             ;*** casa cu cel mai mic tt a fost gasita cu functia min-tt de la TODO3 si adaugarea persoanei la casa cu functia de la TODO5
         
             (cond
               ((= (car (min-tt (list C2 C3 C4))) 2)
                (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))

               ((= (car (min-tt (list C2 C3 C4))) 3)
                (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))

               (else
                (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items)))))
         ; acelasi lucru si doar ca acum nu includem casa C1 in cautare
         ])))
