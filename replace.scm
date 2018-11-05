(use-modules
  (opencog)
  (opencog exec)
  (opencog nlp)
  (opencog nlp lg-parse)
  (srfi srfi-1))

(define lg-dict "en")

(define-public (set-lg-dict dict)
"
  set-lg-dict DICT

  Set the Link Grammar dictionary that will be used by the Link Parser.
  It's assumed that the dictionary is placed under \"/usr/local/share/link-grammar\",
  with all the other required files (like those 4.0.*) in place already.
"
  (set! lg-dict dict))

(define-public (slg-replace sentence)
"
  slg-replace SENTENCE

  Replace one or more words in SENTENCE with other words that
  belong to the same linguistic categories.

  It's assumed that words with the same link at the same position (left/right)
  are in the same category. For example:

    +-Ss*b+      +-Ss*b+
    |     |      |     |
    this is      that is

  \"this\" and \"that\" are assumed to be in the same category, so a sentence
  like \"this is a cat\" may be transformed into \"that is a cat\".

  It's recommended to set the LG dictionary first using `set-lg-dict`, otherwise
  \"en\" will be used as the default dictionary.
"
  ; Parse the sentence using the given dictionary
  (define sent-node
    (cog-execute!
      (LgParse
        (Phrase sentence)
        (LgDict lg-dict)
        (Number 1))))

  ; Get the full list of words of the sentence, with both
  ; ###LEFT-WALL### and ###RIGHT-WALL### removed
  (define words
    (filter
      (lambda (wi)
        (define nstr
          (cog-name (car (cog-chase-link 'ReferenceLink 'WordNode wi))))
        (not (or (string=? "###LEFT-WALL###" nstr)
                 (string=? "###RIGHT-WALL###" nstr))))
      (car (sent-get-words-in-order sent-node))))

  ; Randomly pick a word from the word list
  (define chosen-word
    (list-ref words (random (length words) (random-state-from-platform))))

  ; Get the categories that the chosen word belongs to, in the form
  ; of EvaluationLinks
  (define categories
    (cog-get-pred chosen-word 'LinkGrammarRelationshipNode))

  ; Randomly pick a category from the category list
  (define chosen-category
    (list-ref categories (random (length categories) (random-state-from-platform))))

  ; Check if the chosen word is the source of the chosen LG relationship
  (define is-source?
    (equal? (gadr chosen-category) chosen-word))

  ; Find other members of the chosen category
  (define members
    (filter
      (lambda (m)
        (not (equal?
          (car (cog-chase-link 'ReferenceLink 'WordNode chosen-word))
          (car (cog-chase-link 'ReferenceLink 'WordNode m)))))
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
                  (Variable "$target")))))))))

  ; Randomly pick a member from the member list
  (define chosen-member
    (list-ref members (random (length members) (random-state-from-platform))))

  ; Return the final sentence
  (string-join
    (map
      (lambda (wi)
        (cog-name
          (if (equal? chosen-word wi)
            (car (cog-chase-link 'ReferenceLink 'WordNode chosen-member))
            (car (cog-chase-link 'ReferenceLink 'WordNode wi)))))
      words))
)
