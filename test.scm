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
  (make-section (W left-wall) (list) (list (W "well")))
  (make-section (W "well") (list (W left-wall)) (list (W "play")))
  (make-section (W "play") (list (W "well")) (list)))

(define (load-data-2)
  (make-section (W left-wall) (list) (list (W "I") (W "like") (W ".")))
  (make-section (W "I") (list (W left-wall)) (list (W "like")))
  (make-section (W "like") (list (W left-wall) (W "I")) (list (W "cats")))
  (make-section (W "cats") (list (W "like")) (list))
  (make-section (W ".") (list (W left-wall)) (list)))

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
