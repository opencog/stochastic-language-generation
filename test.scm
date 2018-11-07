(use-modules
  (opencog)
  (opencog exec)
  (opencog nlp)
  (opencog nlp lg-parse)
  (opencog test-runner)
  (ice-9 optargs)
  (srfi srfi-64))

; ---------- Utilities ---------- ;
(define slg-test "Stochastic Language Generation")
(define left-wall "###LEFT-WALL###")

(define (W word)
  (WordNode word (ctv 1 0 (random 1000))))

(define (WC word-class)
  (WordClassNode word-class))

(define* (make-section germ l-cntrs r-cntrs #:optional (cnt (random 100)))
  (Section (ctv 1 0 cnt)
    germ
    (ConnectorSeq
      (map
        (lambda (lc)
          (Connector
            lc
            (ConnectorDir "-")))
        l-cntrs)
      (map
        (lambda (rc)
          (Connector
            rc
            (ConnectorDir "+")))
          r-cntrs))))

; ---------- Data ---------- ;
(define (load-data-1)
  (define lw (W left-wall))
  (define well (W "well"))
  (define play (W "play"))
  (make-section lw (list) (list well))
  (make-section well (list lw) (list play))
  (make-section play (list well) (list)))

(define (load-data-2)
  (define lw (W left-wall))
  (define i (W "I"))
  (define like (W "like"))
  (define cats (W "cats"))
  (define dot (W "."))
  (make-section lw (list) (list i like dot))
  (make-section i (list lw) (list like))
  (make-section like (list lw i) (list cats))
  (make-section cats (list like) (list))
  (make-section dot (list lw) (list)))

(define (load-data-3)
  (define lw (W left-wall))
  (define she (W "she"))
  (define tried (W "tried"))
  (define cheese (W "cheese"))
  (define qmark (W "?"))
  ; Manipulating the counts just to make sure the "dead-ends"
  ; will be selected first, so as to test backtracking
  (make-section lw (list) (list she tried qmark) 0)
  (make-section lw (list) (list cheese) 100)
  (make-section she (list lw) (list tried) 0)
  (make-section she (list) (list cheese qmark) 100)
  (make-section tried (list lw she) (list cheese) 0)
  (make-section tried (list) (list qmark) 100)
  (make-section cheese (list tried) (list) 0)
  (make-section cheese (list she) (list) 100)
  (make-section qmark (list lw) (list) 0)
  (make-section qmark (list cheese) (list) 100))

(define (load-data-4)
  (define jay (W "Jay"))
  (define plays (W "plays"))
  (define tennis (W "tennis"))
  (define subj (WC "subject"))
  (define verb (WC "verb"))
  (define obj (WC "object"))
  (MemberLink jay subj)
  (MemberLink plays verb)
  (MemberLink tennis obj)
  (make-section subj (list) (list verb))
  (make-section verb (list subj) (list obj))
  (make-section obj (list verb) (list)))

(define (load-data-5)
  (define kay (W "Kay"))
  (define teaches (W "teaches"))
  (define yoga (W "yoga"))
  (define female (WC "female"))
  (define action (WC "action"))
  (define practice (WC "practice"))
  (MemberLink kay female)
  (MemberLink teaches action)
  (MemberLink yoga practice)
  (make-section female (list) (list action yoga))
  (make-section action (list kay) (list practice))
  (make-section practice (list female action) (list)))

(define (load-data-6)
  (cog-execute!
    (LgParse
      (Phrase "that was my cat")
      (LgDict "en")
      (Number 1))))

; ---------- Test ---------- ;
(opencog-test-runner)

(test-begin slg-test)

; First of all, given the Sections in data-1, it should be
; able to generate the expected sentence, straightforwardly.
; No backtracking is required.
; Also test when different number of seeds are given.
(load-data-1)
(test-equal "###LEFT-WALL### well play" (slg))
(test-equal "###LEFT-WALL### well play" (slg "well"))
(test-equal "###LEFT-WALL### well play" (slg "play"))
(test-equal "###LEFT-WALL### well play" (slg "well" "play"))
(clear)

; Slightly more complicated, some of the Sections in data-2
; may link to more than one word on the same direction (left/right),
; so the near-far relationship should be taken into account as well.
(load-data-2)
(test-equal "###LEFT-WALL### I like cats ." (slg))
(test-equal "###LEFT-WALL### I like cats ." (slg "I"))
(test-equal "###LEFT-WALL### I like cats ." (slg "like"))
(test-equal "###LEFT-WALL### I like cats ." (slg "cats"))
(test-equal "###LEFT-WALL### I like cats ." (slg "."))
(clear)

; Sometimes it may hit a dead-end, and it should be able to backtrack
; and try again with a different Section.
(load-data-3)
(test-equal "###LEFT-WALL### she tried cheese ?" (slg))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "she"))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "tried"))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "cheese"))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "?"))
; Now remove one of the required Sections, it should backtrack and
; give up eventually
(cog-extract (make-section (W "cheese") (list (W "tried")) (list)))
(test-equal (list) (slg))
(test-equal (list) (slg "she"))
(test-equal (list) (slg "tried"))
(test-equal (list) (slg "cheese"))
(test-equal (list) (slg "?"))
; Same if it's given a seed that doesn't exist in the atomspace.
(test-equal (list) (slg "asana"))
(clear)

; Also try to generate sentences with categories in place. Sections in
; data-4 show the relationships between various categories.
(load-data-4)
(test-equal "Jay plays tennis" (slg "Jay"))
(test-equal "Jay plays tennis" (slg "plays"))
(test-equal "Jay plays tennis" (slg "tennis"))
(clear)

; Similar to the above, but the Sections in data-5 mix words and categories
; together.
(load-data-5)
(test-equal "Kay teaches yoga" (slg "Kay"))
(test-equal "Kay teaches yoga" (slg "teaches"))
(test-equal "Kay teaches yoga" (slg "yoga"))
(clear)

; 'slg' also accept atoms (WordNode / WordClassNode) as seeds, reuse data-5
; to test it.
(load-data-5)
(test-equal "Kay teaches yoga" (slg (W "Kay")))
(test-equal "Kay teaches yoga" (slg (W "teaches")))
(test-equal "Kay teaches yoga" (slg (W "yoga")))
(test-equal "Kay teaches yoga" (slg (WC "female")))
(test-equal "Kay teaches yoga" (slg (WC "action")))
(test-equal "Kay teaches yoga" (slg (WC "practice")))
(clear)

; Test basic word replacement -- given a sentence, replace a word in the sentence
; by another word that belongs to the same category.
(load-data-6)
(test-assert
  (member
    (slg-replace "this is a test")
    (list
      "that is a test"
      "this was a test"
      "this is my test"
      "this is a cat"
      "that was a test"
      "that is my test"
      "that is a cat"
      "this was my test"
      "this was a cat"
      "this is my cat"
      "this was my cat"
      "that was my cat")))
(clear)

(test-end slg-test)
