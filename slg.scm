(use-modules
  (opencog)
  (opencog nlp)
  (opencog sheaf)
  (ice-9 receive)
  (srfi srfi-1))

; ---------- Parameters ---------- ;
; Probability of linking to an existing word in the sentence
; instead of creating a new one
(define prob-link-exist 1)

; How many words the sentence should have (not being used yet)
; A negative value means it's unlimited
(define word-limit -1)

; ---------- Shared ---------- ;
; The LEFT-WALL (the beginning of a sentence)
(define left-wall "###LEFT-WALL###")

; Some internal states
(define complete? #f)
(define sentence (list))

; ---------- The Entry ---------- ;
(define (slg . seeds)
"
  slg . SEEDS

  Start the stochastic language generation.

  One or more words can be passed to the function as SEEDS,
  the sentence being generated will grow from the SEEDS.

  The order of the SEEDS being passed will be perserved
  in the sentence being generated, based on the current
  implementation.

  The generation will start from left (LEFT-WALL) to right
  if no word has been passed as SEEDS.
"
  ; Reset the states
  (set! complete? #f)
  (set! sentence (list))

  ; Start the actual generation
  (generate
    (map
      WordNode
      ; If no seed is given,
      ; start generating from the LEFT-WALL
      (if (null? seeds) (list left-wall) seeds))
    (list)
    (list)
    (null? seeds))

  ; Return the result
  (if (not complete?)
    (begin
      (format #t "Unable to generate a sentence with the given seed(s)!\n")
      (list))
    (string-join (map cog-name sentence))))

; ---------- Main ---------- ;
; Will be called recursively until the generation is complete
(define (generate words links chosen left-wall?)
  ; Randomly pick one word-index that has not been explored
  (define germ-idx
    (rand-pick (lset-difference = (iota (length words)) chosen)))

  ; Get the actual germ
  (define germ (list-ref words germ-idx))

  ; Get all the Sections with this germ
  (define sections (get-germ-sections germ))

  ; Get the connectors that a Section should have, e.g.
  ; if we already have W1 linking to W2:
  ;         +--->---+
  ;         |       |
  ;         W1      W2
  ; then when we get to pick a Section of W2, we have
  ; to make sure that the Section being selected will
  ; have a connector linking to W1 in the opposite
  ; direction, so as to complete the "jigsaw puzzle"
  (define target-cntrs
    (append-map
      (lambda (l)
        (cond
          ((= germ-idx (car l))
           (list
             (Connector
               (list-ref words (cdr l))
               (ConnectorDir "+"))))
          ((= germ-idx (cdr l))
           (list
             (Connector
               (list-ref words (car l))
               (ConnectorDir "-"))))
          (else (list))))
      links))

  ; Filter out Sections that don't have those target connectors
  (define filtered-sections
    (filter
      (lambda (section)
        (define s-cntrs (cog-outgoing-set (gdr section)))
        (every
          (lambda (t-cntr) (member t-cntr s-cntrs))
          target-cntrs))
      sections))

  ; For picking a Section based on their weights
  (define (pick-section candidate-sections)
    (weighted-pick
      candidate-sections
      (map
        (lambda (s) (cog-tv-count (cog-tv s)))
        candidate-sections)))

  (format #t "\n>> ~a (~d) <<\n" (cog-name germ) germ-idx)
  (format #t "Remaining Sections = ~d\n" (length filtered-sections))

  (while (not (or complete? (null? filtered-sections)))
    (let ((section (pick-section filtered-sections)))
      (add-words section target-cntrs germ-idx words links chosen left-wall?)

      (if (not complete?)
        (begin
          ; The Section picked got rejected, try a different one!
          (format #t "==================== Backtracking!\n")
          (print-state words links chosen)
          (set! filtered-sections (delete section filtered-sections))
          (format #t "\n>> ~a (~d) <<\n" (cog-name germ) germ-idx)
          (format #t "Remaining Sections = ~d\n" (length filtered-sections)))))))

; For adding words from a Section to the sentence
(define (add-words section target-cntrs germ-idx sent links chosen left-wall-added?)
  (define reject-section? #f)

  ; For adding words to the left of the germ
  (define (add-to-left l-cntrs from-idx)
    (if (null? l-cntrs)
      (format #t "Finished adding words to the left\n")
      (let ((l-cntr (car l-cntrs))
            (to-idx germ-idx)
            (exist-pos (list))
            (new-pos (list)))
        (if (member l-cntr target-cntrs)
          ; If the current word has already linked to this
          ; germ, no operation is needed, just make sure the
          ; next word will be added to its right
          (begin
            (set! from-idx
              (car
                (find
                  (lambda (link)
                    (and (= (cdr link) germ-idx)
                         (equal? (list-ref sent (car link))
                                 (gar l-cntr))))
                  links)))
            (format #t "---> Already added ~a- at ~d\n"
              (cog-name (gar l-cntr)) from-idx)
            (add-to-left (cdr l-cntrs) (1+ from-idx)))
          ; Otherwise, find a valid spot to add this word to the sentence
          (begin
            (format #t "---> Trying to add: ~a-\n" (cog-name (gar l-cntr)))
            ; The order matters, so before exploring, check if the next
            ; word has already linked to this germ, and if so, make sure
            ; the current word will be added to the left of the next one,
            ; as we are going from far to near
            (if (and (> (length l-cntrs) 1)
                     (member (list-ref l-cntrs 1) target-cntrs))
              (set! to-idx
                (car
                  (find
                    (lambda (link)
                      (and (= (cdr link) germ-idx)
                           (equal? (list-ref sent (car link))
                                   (gar (list-ref l-cntrs 1)))))
                    links))))
            (format #t "Going to explore from ~d to ~d\n" from-idx to-idx)
            ; If it's the LEFT-WALL, make sure it's added to position 0, always
            (if (string=? left-wall (cog-name (gar l-cntr)))
              (if left-wall-added?
                ; If the LEFT-WALL has been added, see if we can link to it
                (if (link-cross? (cons 0 germ-idx) links)
                  (format #t "i = 0\nX existing LEFT-WALL\n")
                  (set! exist-pos (list 0)))
                ; If the LEFT-WALL hasn't been added, see if we can add it from here
                (if (link-cross? (cons 0 (1+ germ-idx)) (update-links links 0))
                  (format #t "i = 0\nX new LEFT-WALL\n")
                  (set! new-pos (list 0))))
              ; For words (viz not LEFT-WALL), go through from 'from-idx'
              ; to 'to-index' and see where we can possibly add it to
              (do ((i from-idx (1+ i)))
                  ((> i to-idx))
                (format #t "i = ~d\n" i)
                ; It's possible that there is already such a word
                ; in the sentence that hasn't been linked with the current
                ; germ, but it's valid to do so
                (if (and (not (link-cross? (cons i germ-idx) links))
                         (equal? (list-ref sent i) (gar l-cntr)))
                  (set! exist-pos (insert-at exist-pos i))
                  (format #t "X existing word\n"))
                ; Apart from the above it's almost always possible
                ; to insert a word right next to the germ, or somewhere else
                (if (link-cross? (cons i (1+ germ-idx)) (update-links links i))
                  (format #t "X new word\n")
                  (set! new-pos (insert-at new-pos i)))))

            (if (and (null? new-pos) (null? exist-pos))
              ; If no valid spot has been discovered because of the
              ; no-link-cross constraint, reject this Section, as
              ; all the connections within a Section has to be satisfied
              ; as a whole
              (set! reject-section? #t)
              ; If we are here, that means there is one or more valid
              ; ways of connecting this word from the germ -- either
              ; link to the same existing word, or create a new one
              (if (and (not (null? exist-pos))
                       (occur? prob-link-exist))
                ; Link to an existing word in the sentence
                (let ((pos-picked (rand-pick exist-pos)))
                  (format #t "exist-pos: ~a\nnew-pos: ~a\npos-picked: ~d\n"
                    exist-pos new-pos pos-picked)
                  ; Update the links but no new words will be added
                  (set! links (insert-at links (cons pos-picked germ-idx)))
                  ; Was that the LEFT-WALL?
                  (if (equal? left-wall (cog-name (gar l-cntr)))
                    (set! left-wall-added? #t))
                  ; Continue, from the right of this word
                  (add-to-left (cdr l-cntrs) (1+ pos-picked)))
                ; Add a new word to the sentence
                (let ((pos-picked (rand-pick new-pos)))
                  (format #t "exist-pos: ~a\nnew-pos: ~a\npos-picked: ~d\n"
                    exist-pos new-pos pos-picked)
                  ; Actually add the word to the sentence and update the link etc
                  (set! sent (insert-at sent (gar l-cntr) pos-picked))
                  (set! links
                    (insert-at
                      (update-links links pos-picked)
                      (cons pos-picked (1+ germ-idx))))
                  (set! chosen (map (lambda (i) (update-index i pos-picked)) chosen))
                  ; Was that the LEFT-WALL?
                  (if (equal? left-wall (cog-name (gar l-cntr)))
                    (set! left-wall-added? #t))
                  ; Increment the index by one, as a word has been
                  ; added to the left
                  (set! germ-idx (1+ germ-idx))
                  ; Continue, from the right of this word
                  (add-to-left (cdr l-cntrs) (1+ pos-picked))))))))))

  ; For adding words to the right of the germ
  (define (add-to-right r-cntrs from-idx)
    (if (null? r-cntrs)
      (format #t "Finished adding words to the right\n")
      (let ((r-cntr (car r-cntrs))
            (to-idx (length sent))
            (new-pos (list))
            (exist-pos (list)))
        ; If the current word has already linked to this
        ; germ, no operation is needed, just make sure the
        ; next word will be added to its right
        (if (member r-cntr target-cntrs)
          (begin
            (set! from-idx
              (cdr
                (find
                  (lambda (link)
                    (and (= (car link) germ-idx)
                         (equal? (list-ref sent (cdr link))
                                 (gar r-cntr))))
                  links)))
            (format #t "---> Already added ~a+ at ~d\n"
              (cog-name (gar r-cntr)) from-idx)
            (add-to-right (cdr r-cntrs) (1+ from-idx)))
          (begin
            (format #t "---> Trying to add: ~a+\n" (cog-name (gar r-cntr)))
            ; The order matters, so before exploring, check if the next
            ; word has already linked to this germ, and if so, make sure
            ; the current word will be added to the left of the next one,
            ; as we are going from far to near
            (if (and (> (length r-cntrs) 1)
                     (member (list-ref r-cntrs 1) target-cntrs))
              (set! to-idx
                (cdr
                  (find
                    (lambda (link)
                      (and (= (car link) germ-idx)
                           (equal? (list-ref sent (cdr link))
                                   (gar (list-ref r-cntrs 1)))))
                    links))))
            (format #t "Going to explore from ~d to ~d\n" from-idx to-idx)
            ; Go through from 'from-idx' to 'to-index' and see where we
            ; can possibly add it to
            (do ((i from-idx (1+ i)))
                ((> i to-idx))
              (format #t "i = ~d\n" i)
              ; It's possible that there is already such a word
              ; in the sentence that hasn't been linked with the current
              ; germ, but it's valid to do so
              (if (and (not (link-cross? (cons germ-idx i) links))
                       (< i (length sent))
                       (equal? (list-ref sent i) (gar r-cntr)))
                (set! exist-pos (insert-at exist-pos i))
                (format #t "X existing word\n"))
              ; Apart from the above it's almost always possible
              ; to insert a word right next to the germ, or somewhere else
              (if (link-cross? (cons germ-idx i) (update-links links i))
                (format #t "X new word\n")
                (set! new-pos (insert-at new-pos i))))

            (if (and (null? new-pos) (null? exist-pos))
              ; If no valid spot has been discovered because of the
              ; no-link-cross constraint, reject this Section, as
              ; all the connections within a Section has to be satisfied
              ; as a whole
              (set! reject-section? #t)
              ; If we are here, that means there is one or more valid
              ; ways of connecting this word from the germ, either
              ; link to the same existing word, or create a new one
              (if (and (not (null? exist-pos))
                       (occur? prob-link-exist))
                ; Link to an existing word in the sentence
                (let ((pos-picked (rand-pick exist-pos)))
                  (format #t "exist-pos: ~a\nnew-pos: ~a\npos-picked: ~d\n"
                    exist-pos new-pos pos-picked)
                  ; Update the links but no new words will be added
                  (set! links (insert-at links (cons germ-idx pos-picked)))
                  ; Continue, from the right of this word
                  (add-to-right (cdr r-cntrs) (1+ pos-picked)))
                ; Add a new word to the sentence
                (let ((pos-picked (rand-pick new-pos)))
                  (format #t "exist-pos: ~a\nnew-pos: ~a\npos-picked: ~d\n"
                    exist-pos new-pos pos-picked)
                  ; Actually add the word to the sentence and update the link etc
                  (set! sent (insert-at sent (gar r-cntr) pos-picked))
                  (set! links
                    (insert-at
                      (update-links links pos-picked)
                      (cons germ-idx pos-picked)))
                  (set! chosen (map (lambda (i) (update-index i pos-picked)) chosen))
                  ; Continue, from the right of this word
                  (add-to-right (cdr r-cntrs) (1+ pos-picked))))))))))

  ; Try to add each of the words from that Section one by one while
  ; making sure that there is no link crossed with the existing ones
  (receive (l-cntrs r-cntrs)
    ; Partition the conntectors into two list:
    ; the left ("-") and the right ("+"), for convenience
    (partition
      (lambda (cntr) (string=? "-" (cog-name (gdr cntr))))
      (cog-outgoing-set (gdr section)))

    ; Record the chosen index (word) that we are exploring now
    (set! chosen (append chosen (list germ-idx)))

    ; First of all, go through the left connectors and see if it's possible
    ; to add those words to the left of this germ
    ; The order of the left connectors, as sorted in a Section,
    ; goes from far to near
    (add-to-left
      l-cntrs
      ; If a LEFT-WALL has been added to the sentence, start exploring
      ; from position 1 (i.e. after the LEFT-WALL)
      (if left-wall-added? 1 0))

    ; Then go through the right connectors
    ; The order of the right connectors, as sorted in a Section,
    ; goes from near to far
    (if (not reject-section?)
      (add-to-right r-cntrs (1+ germ-idx)))

    (if (not reject-section?)
      ; Continue, or end the generation here
      (begin
        (print-state sent links chosen)
        ; Every single word has to be explored once (and only once),
        ; to make sure the usage is complete
        (if (< (length chosen) (length sent))
          (generate sent links chosen left-wall-added?)
          (begin
            (set! complete? #t)
            (set! sentence sent)))))))

; ---------- Utilities ---------- ;
; For debugging
(define (print-state sent links chosen)
  (format #t "Sentence: ~a\n" (map cog-name sent))
  (format #t "Links: ~a\n"
    (sort links
      (lambda (x y)
        (if (= (car x) (car y))
          (< (cdr x) (cdr y))
          (< (car x) (car y))))))
  (format #t "Chosen: ~a\n" (sort chosen <)))

; Generate a random number in [0, n)
(define (rand-num n)
  (random n (random-state-from-platform)))

; Randomly pick one of the elements in the list
(define (rand-pick lst)
  (list-ref lst (rand-num (length lst))))

; Do a weighted random selection over the elements in the list
(define* (weighted-pick lst weights #:optional rand)
  (define new-rand
    (- (if rand rand (rand-num (apply + weights)))
       (car weights)))

  (if (< new-rand 0)
    (car lst)
    (weighted-pick (cdr lst) (cdr weights) new-rand)))

; Decide whether an event should occur
(define (occur? prob)
  (> prob (random:uniform)))

; Check if the new link being added satisfy the
; no-link-cross constraint
(define (link-cross? lk ex-lks)
  (any
    (lambda (ex-lk)
      (define iln (car lk))
      (define irn (cdr lk))
      (define ile (car ex-lk))
      (define ire (cdr ex-lk))
      (or
        (and (< iln ile) (< ile irn) (< irn ire))
        (and (< ile iln) (< iln ire) (< ire irn))))
    ex-lks))

; Update the indexes of the existing links if the
; index is larger than or equal to the chosen index
(define (update-links lks idx)
  (fold
    (lambda (lk rtn)
      (append
        rtn
        (list
          (cons
            (update-index (car lk) idx)
            (update-index (cdr lk) idx)))))
    (list)
    lks))

; Update an exising index, if needed, by checking
; if there is a new word being added to the left
; of the word that we are looking at right now
(define (update-index ex-idx new-idx)
  (if (>= ex-idx new-idx)
    (1+ ex-idx)
    ex-idx))

; Insert an element into a list at a specific
; position, if given, otherwise append it to the end
(define* (insert-at lst elmt #:optional pos)
  (if (null? lst)
    (list elmt)
    (if pos
      (append
        (list-head lst pos)
        (list elmt)
        (list-tail lst pos))
      (append lst (list elmt)))))
