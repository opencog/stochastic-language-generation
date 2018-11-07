(use-modules
  (opencog)
  (opencog exec)
  (opencog nlp)
  (opencog nlp lg-parse)
  (opencog sheaf)
  (ice-9 optargs)
  (ice-9 receive)
  (srfi srfi-1))

; ---------- Parameters ---------- ;
; Probability of linking to an existing word in the sentence
; instead of creating a new one
(define prob-link-exist 1)

; Max. no. of words the sentence can have
; A negative value means it's unlimited
(define max-words 10)

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

  SEEDS can either be atoms (WordNode/WordClassNode), or
  strings, but not a mix of both. If strings are passed,
  they will all be treated as words. Categories, aka
  word classes, can only be passed as atoms (WordClassNode).

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
    (cond
      ; If no seed is given,
      ; start generating from the LEFT-WALL
      ((null? seeds) (list (Word left-wall)))
      ((every cog-atom? seeds) seeds)
      ((every string? seeds) (map WordNode seeds))
      (else (throw 'WrongInputType seeds)))
    (list)
    (list)
    (null? seeds))

  ; Return the result
  (if (not complete?)
    (begin
      (format #t "Unable to generate a sentence with the given seed(s)!\n")
      (list))
    (string-join
      (map
        (lambda (w)
          (cond
            ; If it's a (WordNode . WordClassNode) pair, pick the WordNode
            ((pair? w) (cog-name (car w)))
            ; If it's a WordClassNode, pick a WordNode from that class
            ((equal? 'WordClassNode (cog-type w))
             (cog-name (pick-word-from-class w)))
            ; Otherwise, it's a WordNode so just get the name of it
            (else (cog-name w))))
        sentence))))

(define*-public (slg-replace sentence #:optional (lg-dict "en"))
"
  slg-replace SENTENCE [LG-DICT]

  Replace one or more words in SENTENCE with other words that
  belong to the same linguistic categories.

  It's assumed that words with the same link at the same position (left/right)
  are in the same category. For example:

    +-Ss*b+      +-Ss*b+
    |     |      |     |
    this is      that is

  \"this\" and \"that\" are assumed to be in the same category, so a sentence
  like \"this is a cat\" may be transformed into \"that is a cat\".

  An optional argument LG-DICT can be passed to indicate what Link Grammar
  dictionary will be used by the Link Parser for parsing this sentence. The
  default dictionary is \"en\".

  If a custom LG dictionary is passed, it's assumed that the dictionary is
  placed under \"/usr/local/share/link-grammar\", with all the other required
  files (like those 4.0.*) in place already.
"
  (define (generate-output-sentence word-instances)
    (string-join
      (map
        (lambda (wi) (cog-name (word-inst-get-word wi)))
        word-instances)))

  (define* (pick-and-replace words #:optional (new-sent words))
    ; Randomly pick a word from the word list
    (define chosen-word (rand-pick words))

    ; Get the categories that the chosen word belongs to, in the form
    ; of EvaluationLinks
    (define categories
      (cog-get-pred chosen-word 'LinkGrammarRelationshipNode))

    ; Randomly pick a category from the category list
    (define chosen-category (rand-pick categories))

    ; Check if the chosen word is the source of the chosen LG relationship
    (define is-source? (equal? (gadr chosen-category) chosen-word))

    ; Find other members of the chosen category
    (define members
      (delete-duplicates
        (filter
          (lambda (m)
            (not (equal?
              (word-inst-get-word chosen-word)
              (word-inst-get-word m))))
          (map
            (lambda (l) (if is-source? (gar l) (gdr l)))
            (cog-outgoing-set
              (cog-execute!
                (Get
                  (VariableList
                    (TypedVariable
                      (Variable "$source")
                      (Type "WordInstanceNode"))
                    (TypedVariable
                      (Variable "$target")
                      (Type "WordInstanceNode")))
                  (Evaluation
                    (gar chosen-category)
                    (List
                      (Variable "$source")
                      (Variable "$target"))))))))
        (lambda (x y)
          (equal? (word-inst-get-word x) (word-inst-get-word y)))))

    (if (null? members)
      ; If there is no other members in the same category,
      ; try again with a different word! Unless there is
      ; no more unexplored words.
      (begin
        (format #t "No members found form \"~a\"\nSentence: ~a\n"
          (cog-name (gar chosen-category))
          (map cog-name (map word-inst-get-word new-sent)))
        (if (= 1 (length words))
          (generate-output-sentence new-sent)
          (pick-and-replace (delete chosen-word words) new-sent)))
      ; Otherwise, replace the word in the sentence with
      ; the chosen member.
      (let* ((chosen-member (rand-pick members))
             (words-replaced
               (map
                 (lambda (wi) (if (equal? chosen-word wi) chosen-member wi))
                 new-sent)))
        (format #t "---> Replacing \"~a\" with \"~a\" from \"~a\"\nSentence: ~a\n"
          (cog-name (word-inst-get-word chosen-word))
          (cog-name (word-inst-get-word chosen-member))
          (cog-name (gar chosen-category))
          (map cog-name (map word-inst-get-word words-replaced)))
        ; Randomly decide whether to continue the process or end it here.
        (if (and (occur? 0.5) (> (length words) 1))
          (pick-and-replace (delete chosen-word words) words-replaced)
          (generate-output-sentence words-replaced)))))

  ; Parse the sentence using the given dictionary
  (define sent-node
    (cog-execute!
      (LgParse
        (Phrase sentence)
        (LgDict lg-dict)
        (Number 1))))

  (format #t "\n>> ~a <<\n" sentence)

  ; Get the full list of words of the sentence, with both
  ; ###LEFT-WALL### and ###RIGHT-WALL### removed, and start
  ; the process.
  (pick-and-replace
    (filter
      (lambda (wi)
        (define nstr
          (cog-name (word-inst-get-word wi)))
        (not (or (string=? "###LEFT-WALL###" nstr)
                 (string=? "###RIGHT-WALL###" nstr))))
      (car (sent-get-words-in-order sent-node)))))


; ---------- Main ---------- ;
; Will be called recursively until the generation is complete
(define (generate sent links chosen left-wall?)
  ; Randomly pick one word-index that has not been explored
  (define germ-idx
    (rand-pick (lset-difference = (iota (length sent)) chosen)))

  ; Get the actual germ
  ; This could be a (WordNode . WordClassNode) pair sometimes,
  ; and it happens when both the word and the word-class has
  ; already been selected in the previous iteration, so we'll
  ; have to stick with them strictly
  (define germ (list-ref sent germ-idx))

  ; Get all the Sections with this germ
  ; The germ may be a member of one or more word-classes,
  ; get those Sections as well, assuming the format remains:
  ;   MemberLink
  ;     WordNode
  ;     WordClassNode
  ; and the germ connects to all of the word-classes it belongs
  ; to even if the word-classes may be hierarchical in structure
  (define sections
    (if (pair? germ)
      (append-map
        get-germ-sections
        (list (car germ) (cdr germ)))
      (append-map
        get-germ-sections
        (append
          (list germ)
          (cog-chase-link 'MemberLink 'WordClassNode germ)))))

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
           (let ((w (list-ref sent (cdr l))))
             ; If it's a WordNode-WordClassNode pair
             ; make this target connector a pair, and the
             ; Sections we want only need to have either
             ; one of them
             (if (pair? w)
               (list
                 (cons
                   (Connector (car w) (ConnectorDir "+"))
                   (Connector (cdr w) (ConnectorDir "+"))))
               (list (Connector w (ConnectorDir "+"))))))
          ((= germ-idx (cdr l))
           (let ((w (list-ref sent (car l))))
             ; If it's a WordNode-WordClassNode pair
             ; make this target connector a pair, and the
             ; Sections we want only need to have either
             ; one of them
             (if (pair? w)
               (list
                 (cons
                   (Connector (car w) (ConnectorDir "-"))
                   (Connector (cdr w) (ConnectorDir "-"))))
               (list (Connector w (ConnectorDir "-"))))))
          (else (list))))
      links))

  ; Filter out Sections that don't have those target connectors
  (define filtered-sections
    (filter
      (lambda (section)
        (define s-cntrs (cog-outgoing-set (gdr section)))
        (every
          (lambda (t-cntr)
            (cond
              ; If the target connector is a pair, it needs
              ; to have either one
              ((pair? t-cntr)
               (or (member (car t-cntr) s-cntrs)
                   (member (cdr t-cntr) s-cntrs)))
              ; If the target connector is a class, it has to
              ; have either a connector of the same class, or
              ; a word that belongs to that class
              ((equal? 'WordClassNode (cog-type (gar t-cntr)))
               (or (member t-cntr s-cntrs)
                   (any
                     (lambda (s-cntr)
                       (word-in-class? (gar s-cntr) (gar t-cntr)))
                     s-cntrs)))
              ; Otherwise, just check if it has the exact same
              ; connector
              (else (member t-cntr s-cntrs))))
          target-cntrs))
      sections))

  ; For picking a Section based on their weights
  (define (pick-section candidate-sections)
    (weighted-pick
      candidate-sections
      (map
        (lambda (s) (cog-tv-count (cog-tv s)))
        candidate-sections)))

  (format #t "\n>> ~a (~d) <<\n"
    (if (pair? germ)
      (map cog-name (list (car germ) (cdr germ)))
      (cog-name germ))
    germ-idx)
  (format #t "Remaining Sections = ~d\n" (length filtered-sections))

  (while (not (or complete? (null? filtered-sections)))
    (let ((section (pick-section filtered-sections)))
      (if (and (equal? 'WordClassNode (cog-type (gar section)))
               (not (pair? germ))
               (equal? 'WordNode (cog-type germ)))
        ; If the 'germ' being passed is a word, while the actual germ of
        ; the Section selected is a word-class, record both the word
        ; and the word-class as a pair
        (add-words section target-cntrs germ-idx
          (replace-at sent (cons germ (gar section)) germ-idx)
            links chosen left-wall?)
        (add-words section target-cntrs germ-idx sent links chosen left-wall?))

      (if (not complete?)
        (begin
          ; The Section picked got rejected, try a different one!
          (format #t "==================== Backtracking!\n")
          (print-state sent links chosen)
          (set! filtered-sections (delete section filtered-sections))
          (format #t "\n>> ~a (~d) <<\n" (cog-name germ) germ-idx)
          (format #t "Remaining Sections = ~d\n" (length filtered-sections)))))))

; For adding words from a Section to the sentence
(define (add-words section target-cntrs germ-idx sent links chosen left-wall-added?)
  (define reject-section? #f)

  (define (is-target-cntr? cntr)
    (any
      (lambda (t-cntr)
        (if (pair? t-cntr)
          (or (equal? cntr (car t-cntr))
              (equal? cntr (cdr t-cntr)))
          (equal? cntr t-cntr)))
      target-cntrs))

  ; For adding words to the left of the germ
  (define (add-to-left l-cntrs from-idx)
    (if (null? l-cntrs)
      (format #t "Finished adding words to the left\n")
      (let ((l-cntr (car l-cntrs))
            (to-idx germ-idx)
            (exist-pos (list))
            (new-pos (list)))
        (if (is-target-cntr? l-cntr)
          ; If the current word has already linked to this
          ; germ, no operation is needed, just make sure the
          ; next word will be added to its right
          (begin
            (set! from-idx
              (car
                (find
                  (lambda (link)
                    (and (= (cdr link) germ-idx)
                         (is-word-or-in-class?
                           (list-ref sent (car link)) (gar l-cntr))))
                  links)))
            (format #t "---> Already added ~a- at ~d\n"
              (cog-name (gar l-cntr)) from-idx)
            (add-to-left (cdr l-cntrs) (1+ from-idx)))
          ; Otherwise, find a valid spot to add this word to the sentence
          (begin
            (format #t "---> Trying to add: ~a-\n" (cog-name (gar l-cntr)))
            ; The order matters, so before exploring, check if any of the
            ; remaining words has already linked to this germ, and if so,
            ; make sure the current word will be added to the left of that
            ; as we are going from far to near
            (if (find is-target-cntr? l-cntrs)
              (set! to-idx
                (car
                  (find
                    (lambda (link)
                      (and (= (cdr link) germ-idx)
                           (is-word-or-in-class?
                             (list-ref sent (car link))
                             (gar (find is-target-cntr? l-cntrs)))))
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
                         (is-word-or-in-class? (list-ref sent i) (gar l-cntr)))
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
                (let* ((pos-picked (rand-pick exist-pos))
                       (wi (list-ref sent pos-picked))
                       (cntr-w (gar l-cntr))
                       (cntr-type (cog-type cntr-w)))
                  (format #t "exist-pos: ~a\nnew-pos: ~a\npos-picked: ~d\n"
                    exist-pos new-pos pos-picked)
                  ; Record both the word and the word-class if we are matching
                  ; a word to a word-class or visa versa
                  (if (not (pair? wi))
                    (cond
                      ((and (equal? 'WordClassNode cntr-type)
                            (equal? 'WordNode (cog-type wi)))
                       (set! sent (replace-at sent (cons wi cntr-w) pos-picked)))
                      ((and (equal? 'WordNode cntr-type)
                            (equal? 'WordClassNode (cog-type wi)))
                       (set! sent (replace-at sent (cons cntr-w wi) pos-picked)))))
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
        (if (is-target-cntr? r-cntr)
          (begin
            (set! from-idx
              (cdr
                (find
                  (lambda (link)
                    (and (= (car link) germ-idx)
                         (is-word-or-in-class?
                           (list-ref sent (cdr link)) (gar r-cntr))))
                  links)))
            (format #t "---> Already added ~a+ at ~d\n"
              (cog-name (gar r-cntr)) from-idx)
            (add-to-right (cdr r-cntrs) (1+ from-idx)))
          (begin
            (format #t "---> Trying to add: ~a+\n" (cog-name (gar r-cntr)))
            ; The order matters, so before exploring, check if any of the
            ; remaining words has already linked to this germ, and if so,
            ; make sure the current word will be added to the left of that,
            ; as we are going from far to near
            (if (find is-target-cntr? r-cntrs)
              (set! to-idx
                (cdr
                  (find
                    (lambda (link)
                      (and (= (car link) germ-idx)
                           (is-word-or-in-class?
                             (list-ref sent (cdr link))
                             (gar (find is-target-cntr? r-cntrs)))))
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
                       (is-word-or-in-class? (list-ref sent i) (gar r-cntr)))
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
                (let* ((pos-picked (rand-pick exist-pos))
                       (wi (list-ref sent pos-picked))
                       (cntr-w (gar r-cntr))
                       (cntr-type (cog-type cntr-w)))
                  (format #t "exist-pos: ~a\nnew-pos: ~a\npos-picked: ~d\n"
                    exist-pos new-pos pos-picked)
                  ; Record both the word and the word-class if we are matching
                  ; a word to a word-class or visa versa
                  (if (not (pair? wi))
                    (cond
                      ((and (equal? 'WordClassNode cntr-type)
                            (equal? 'WordNode (cog-type wi)))
                       (set! sent (replace-at sent (cons wi cntr-w) pos-picked)))
                      ((and (equal? 'WordNode cntr-type)
                            (equal? 'WordClassNode (cog-type wi)))
                       (set! sent (replace-at sent (cons cntr-w wi) pos-picked)))))
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

    ; Stopping condition(s)
    (set! reject-section?
      (or reject-section?
        ; Cannot have more than 'max-words'
        (> (length sent) max-words)))

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
  (format #t "Sentence: ~a\n"
    (map
      (lambda (w)
        (if (pair? w)
          (cons (cog-name (car w)) (cog-name (cdr w)))
          (cog-name w)))
      sent))
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

  (if (or (= (length lst) 1) (< new-rand 0))
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

; Replace the element at the particular position of the given list
(define (replace-at lst elmt pos)
  (append (list-head lst pos) (list elmt) (list-tail lst (1+ pos))))

; Check if the word is a member of the given class
(define (word-in-class? word class)
  (member word (cog-chase-link 'MemberLink 'WordNode class)))

; Randomly pick one of the WordNodes in the given WordClassNode
(define (pick-word-from-class class)
  (let ((word-picked
          (rand-pick (cog-chase-link 'MemberLink 'WordNode class))))
    (format #t "---> Picked \"~a\" from the class \"~a\"\n"
      (cog-name word-picked) (cog-name class))
    word-picked))

; Check if two words are identical
; 'wcp' can be a (WordNode . WordClassNode) pair, or just either one
; 'wc' can be either a WordNode or a WordClassNode, but not a pair
(define (is-word-or-in-class? wcp wc)
  (if (pair? wcp)
    (or (equal? wc (car wcp))
        (equal? wc (cdr wcp))
        (word-in-class? (car wcp) wc)
        (word-in-class? wc (cdr wcp)))
    (or (equal? wcp wc)
        (word-in-class? wcp wc)
        (word-in-class? wc wcp))))
