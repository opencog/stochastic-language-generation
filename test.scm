(use-modules
  (opencog)
  (opencog nlp)
  (srfi srfi-64))

; ---------- Utilities ---------- ;
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
(define (load-slg-test-1-data)
  (make-section (W "I") (list) (list (W "like")))
  (make-section (W "I") (list (W "###LEFT-WALL###")) (list (W "am")))
  (make-section (W "I") (list) (list (W "eat")))
  (make-section (W "I") (list) (list (W "love")))
  (make-section (W "I") (list) (list (W "hate")))
  (make-section (W "like") (list (W "I")) (list (W "cats")))
  (make-section (W "like") (list (W "they")) (list))
  (make-section (W "like") (list (W "we")) (list))
  (make-section (W "cats") (list (W "like")) (list))
  (make-section (W "cats") (list (W "break")) (list))
  (make-section (W "cats") (list (W "love")) (list))
  (make-section (W "cats") (list (W "hate")) (list))
)

; ---------- Test ---------- ;
; Given the Sections, it should be able to generate
; one and only one sentence from them, backtrack and
; try again if it hits a dead-end, or end the generation
; if it's not possible to generate any sentence from
; the given seed(s)
(test-begin "slg-test-1")
(load-slg-test-1-data)
(test-equal "I like cats" (slg "I"))
(test-equal "I like cats" (slg "like"))
(test-equal "I like cats" (slg "cats"))
(test-equal "I like cats" (slg "I" "like"))
(test-equal "I like cats" (slg "I" "cats"))
(test-equal "I like cats" (slg "like" "cats"))
(test-equal "I like cats" (slg "I" "like" "cats"))
(test-end "slg-test-1")
