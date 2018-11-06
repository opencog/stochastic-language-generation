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

; Slightly more complicated, some of the Sections in data-2
; may link to more than one word on the same direction (left/right),
; so the near-far relationship should be taken into account as well.
(load-data-2)
(test-equal "###LEFT-WALL### I like cats ." (slg))
(test-equal "###LEFT-WALL### I like cats ." (slg "I"))
(test-equal "###LEFT-WALL### I like cats ." (slg "like"))
(test-equal "###LEFT-WALL### I like cats ." (slg "cats"))
(test-equal "###LEFT-WALL### I like cats ." (slg "."))

(test-end slg-test)
