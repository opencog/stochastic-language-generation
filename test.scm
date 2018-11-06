(use-modules
  (opencog)
  (opencog nlp)
  (opencog test-runner)
  (srfi srfi-64))

; ---------- Utilities ---------- ;
(define slg-test "Stochastic Language Generation")
(define left-wall "###LEFT-WALL###")

(define (W word)
  (WordNode word (ctv 1 0 (random 1000))))

(define (WC word-class)
  (WordClassNode word-class))

(define (make-section germ l-cntrs r-cntrs)
  (Section (ctv 1 0 (random 1000))
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

(define (clear-sections)
  (for-each cog-extract-recursive (cog-get-atoms 'Section)))

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
  (define he (W "he"))
  (define tried (W "tried"))
  (define played (W "played"))
  (define cheese (W "cheese"))
  (define tennis (W "tennis"))
  (define qmark (W "?"))
  (make-section lw (list) (list she tried qmark))
  (make-section lw (list) (list he played qmark))
  (make-section she (list lw) (list tried))
  (make-section she (list lw) (list played))
  (make-section he (list lw) (list tried))
  (make-section tried (list lw she) (list cheese))
  (make-section tried (list lw he) (list tennis))
  (make-section played (list lw he) (list cheese))
  (make-section cheese (list tried) (list))
  (make-section cheese (list played) (list))
  (make-section tennis (list played) (list))
  (make-section qmark (list lw) (list)))

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
(clear-sections)

; Slightly more complicated, some of the Sections in data-2
; may link to more than one word on the same direction (left/right),
; so the near-far relationship should be taken into account as well.
(load-data-2)
(test-equal "###LEFT-WALL### I like cats ." (slg))
(test-equal "###LEFT-WALL### I like cats ." (slg "I"))
(test-equal "###LEFT-WALL### I like cats ." (slg "like"))
(test-equal "###LEFT-WALL### I like cats ." (slg "cats"))
(test-equal "###LEFT-WALL### I like cats ." (slg "."))
(clear-sections)

; Sometimes it may hit a dead-end, and it should be able to backtrack
; and try again with a different Section, or give up if it has already
; try every single one of them.
(load-data-3)
(test-equal "###LEFT-WALL### she tried cheese ?" (slg))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "she"))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "tried"))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "cheese"))
(test-equal "###LEFT-WALL### she tried cheese ?" (slg "?"))
(test-equal (list) (slg "he"))
(test-equal (list) (slg "played"))
(test-equal (list) (slg "tennis"))

(test-end slg-test)
