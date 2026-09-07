#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul
;; până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client
;; + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat Ana cu 3 produse, apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute =>
;; et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO 1 (5p)
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))
;*** am acutalizat comanda declarad et=0 si tt=0, queue='(), indexul e dat

; TODO 2 (15p)
; Implementați o funcție care aplică o transformare f
; casei cu un anumit index.
; f = funcție unară cu un parametru de tip casă,
; counters = listă de case,
; index = indexul casei care trebuie transformată
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index,
; întoarceți lista nemodificată.
(define (update f counters index)
  ;*** trebuie sa modificam un element intr-o lista (inlocuim) -> reconstruim lista de la 0
  ;*** presupunem ca indexul este unic in lista
  (update-helper f counters '() index))

(define (update-helper f L acc index)
  (cond
    ((null? L) (reverse acc))
    ((equal? (counter-index (car L)) index) ; * daca este, atunci il modificam
     (update-helper f (cdr L) (cons (f (car L)) acc) index))
    (else
     (update-helper f (cdr L) (cons (car L) acc) index))))
;*** varianta recusiva modifica elementul cu indexul dat cand il gaseste
;*** se observa ca daca elementul nu este gasit atunci functia va returna
;*** aceeasi lista

; TODO 3 (7.5p)
; Memento: tt+ crește tt-ul unei case cu un număr de minute.
; Obs: tt+ afectează doar câmpul tt, nu și câmpul et.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument
;   funcției update în cel mai facil mod
; Obs: Facil înseamnă că o aplicație parțială a funcției tt+ 
; va produce o funcție unară cu parametru de tip casă, fără
; să fie nevoie de funcții anonime sau funcții auxiliare.
; Scheletul nu menționează parametrii funcției tt+, întrucât
; trebuie să determinați voi înșivă cum este cel mai bine
; ca tt+ să își primească parametrii.
;
; Apoi implementați funcția checker-tt+, care apelează funcția
; tt+ pe o casă și un număr de minute.
; Funcția checker-tt își precizează clar parametrii și
; poate fi testată, acesta este singurul său rol.
; RESTRICȚII (5p)
;  - Implementați tt+ conform cerinței anterioare.
(define tt+
  (λ (minutes)
    (λ (C)
      (struct-copy counter C [tt (+ (counter-tt C) minutes)] ))))

; *** vechea implementare pentru t++ era asta 
; (define (tt+ C minutes)
;  (struct-copy counter C [tt (+ (counter-tt C) minutes)] ))
; *** acum pentru a o face functie curry folosim lambda pentru aplicarea partiala
; *** funcita va fi putea fi transmisa drept parametru pentru viitoarele update-uri

(define (checker-tt+ C minutes)
  ((tt+ minutes) C))


; TODO 4 (7.5p)
; Implementați o funcție care crește et-ul unei case
; cu un număr dat de minute.
; Obs: et+ afectează doar câmpul et, nu și câmpul tt.
; Păstrați formatul folosit pentru tt+.
; Apoi implementați funcția checker-et+ care apelează
; et+, pentru testare.
; RESTRICȚII (5p)
;  - Implementați et+ conform cerinței anterioare.
(define et+
  (λ (minutes)
    (λ (C)
      (struct-copy counter C [et (+ (counter-et C) minutes)] ))))
;*** analog functiei tt+, folosim lambda pentru a face functia curry
;*** o sa ne ajute mai tarziu sa primim minutele inainte de lista cu elemente

(define (checker-et+ C minutes)
  ((et+ minutes) C))

; TODO 5 (10p)
; Memento: add-to-counter adaugă o persoană
; (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter din aceleași
; rațiuni pentru care ați actualizat funcția tt+.
; Atenție la cum se modifică tt și et!
; Apoi implementați funcția checker-add-to-counter
; care apelează add-to-counter, pentru testare.
; RESTRICȚII (5p)
;  - Implementați add-to-counter conform cerinței anterioare.
(define add-to-counter
  ; *** daca coada e nula acutalizam et
  ; *** daca nu e nula nu actualizam et pt ca trebuie sa reprezinte timpul primul om din coada
  (λ (name)
    (λ (n-items)
      (λ (C)
        (if (null? (counter-queue C))            
            (struct-copy counter C
                         [tt (+ (counter-tt C) n-items)]
                         [et (+ (counter-et C) n-items)]
                         [queue (append (counter-queue C) (list (cons name n-items)))])

            (struct-copy counter C
                         [tt (+ (counter-tt C) n-items)]
                         [queue (append (counter-queue C) (list (cons name n-items)))]))))))
; *** functia de la etapa1 a fost aceasta
;(define (add-to-counter C name n-items)
;  (struct-copy counter C
;               [tt (+ (counter-tt C) n-items)]
;               [queue (append (counter-queue C) (list (cons name n-items)))]))
; *** din aceasta am substras cazurile si am transmis parametrii pe rand
(define (checker-add-to-counter C name n-items)
  (((add-to-counter name) n-items) C))


; TODO 6 (15p)
; Întrucât vom folosi atât min-tt (implementat în etapa 1)
; cât și min-et (funcție nouă), definiți o funcție mai abstractă
; din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă nevidă de case și
; întoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (la același et, este preferată casa cu indexul cel mai mic)
; Obs: în etapele 2-4, listele de case sunt sortate după index.
; RESTRICȚII (10p - 2*5p)
;  - min-tt și min-et vor fi aplicații parțiale ale funcției abstracte.

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

;*** am luat functia din etapa1 care sterge elemente de la inceputul liste
;*** daca acestea nu au tt/et ul. Functia returneaza o lista cu un element (perechea ceruta)
; *** se observa ca rezolvarea depinde de campul de acces pe care il transmitem
; *** drept parametru 

(define min-tt (min-field counter-tt)); folosind funcția de mai sus
(define min-et (min-field counter-et)) ; folosind funcția de mai sus

; TODO 7 (10p)
; Implementați o funcție care scoate prima persoană
; din coada unei case.
; Funcția presupune, fără să verifice, că există
; minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin
; modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva,
; înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (define first-person (car (counter-queue C)))
  (define sum-of-all (apply + (map cdr (counter-queue C))))
  
  (define new-tt (- sum-of-all (cdr first-person)))
  ; *** unde (apply + (map cdr (counter-queue C))) reprezinta suma elementele (desfacem queue intr-o lista cu tt-uri)
  ; *** noul tt e definit prima suma lor (datorita faptului ca nu exista intarzieri)
  (define new-et (if (null? (cdr (counter-queue C))) 0 (cdr (cadr (counter-queue C)))))
  ; **** unde (cadr (counter-queue C)) e a 2-a persoana din coada

  (struct-copy counter C
               [tt new-tt]
               [et new-et]
               [queue (cdr (counter-queue C))]))

; TODO 8 (50p)
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a procesa liste de case)
; - requests conține 4 tipuri de cereri (două în plus față de etapa 1):
;   - (<name> <n-items>) - așază persoana <name> la coadă la o casă
;   - (delay <index> <minutes>) - întârzie casa <index> cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care este
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor depășește 
;                          <average>, adaugă case fără restricții (case slow)
; Sistemul procesează cererile în ordine, astfel:
; - așază persoana la casa cu tt minim la care are voie
;   (ca înainte, dar folosind fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc
;   (chiar dacă nu are clienți)
; - persoana cea mai avansată este prima persoană la casa cu et-ul minim
;   (dintre casele care au clienți)
;   (dacă nicio casă nu are clienți, ignoră cererea)
; - dacă tt-ul mediu pentru toate casele > <average>,
;   adaugă case slow până când media <= <average>
;   (puteți determina matematic de câte case noi este nevoie sau
;   să adăugați recursiv una câte una cât timp este necesar)
; Considerați casele indexate de la 1 și mereu sortate după index.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie adăugați întâi casa 16, apoi casa 17, etc.
; RESTRICȚII (25p - 5*5p)
;  - Folosiți minim două funcționale predefinite în Racket. (2*5p)
;  - Nu apelați checker-tt+, checker-et+, checker-add-to-counter,
;    ci doar tt+, et+, add-to-counter. (3*5p) 
(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'delay index minutes)
         (serve (cdr requests)
                (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)
                (update (et+ minutes) (update (tt+ minutes) slow-counters index) index))
         ]

         ; *** vom creste tt+ si et+ cu minutes pentru fiecare casa
         ; *** intrucat functia update returneaza aceeasi lista daca nu gaseste index atunci putem sa o folosim pe ambele liste
         ; *** functia returneaza lista cu valoarea modificata, deci facem update de 2 ori la ea
         
        [(list 'remove-first)
         (if (null? (list-of-clients (append slow-counters fast-counters)))
             (serve (cdr requests) fast-counters slow-counters)
             (serve (cdr requests)
                    (update remove-first-from-counter fast-counters (car (min-et (list-of-clients (append slow-counters fast-counters)))))
                    (update remove-first-from-counter slow-counters (car (min-et (list-of-clients (append slow-counters fast-counters)))))))
         ]
         ;*** folosim filter sa luam casele care au clienti [1]
         ;*** vom face min-et pe aceasta casa pentru a gasi index ul casei cautate de la care sa scoatem casa

        [(list 'ensure average)
          (serve (cdr requests)
                 fast-counters
                 (ensure-the-balance fast-counters slow-counters average))
          ;*** functia ensure balance adauga case in slow-counters pana cand media aritmetica a tt-urilor
          ;*** devine <= decat numarul dat "average"
         ]
        [(list name n-items)
         (if (<= n-items ITEMS)
             (serve (cdr requests)
                    (update ((add-to-counter name) n-items) fast-counters (car (min-tt (append fast-counters slow-counters))))
                    (update ((add-to-counter name) n-items) slow-counters (car (min-tt (append fast-counters slow-counters)))))
             (serve (cdr requests)
                    fast-counters
                    (update ((add-to-counter name) n-items) slow-counters (car (min-tt slow-counters)))))

         ; *** analog temei 1, daca n-items <= ITEMS, cautam index ul in toate casele (listele concatenate)
         ; *** modificam cu update lista si folosim functia add-to-counter sa adaugam persoana
         ])))

; - 1 functie care ne ajuta pentru remove-first -
; *** functie care dintr-o lista de case, returneaza acele case care au clienti folosind fileter [1]
; *** evident, ne asteptam ca daca nicio casa nu are clienti, sa returneze lista nula '()
(define (list-of-clients L)
  (filter (λ (C) (if (null? (counter-queue C)) #f #t)) L))

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

