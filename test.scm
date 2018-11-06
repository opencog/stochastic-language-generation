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

(test-end slg-test)
