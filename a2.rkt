#lang racket

;; The `[data #f]` is a default value for the 2nd positional argument.
;; That way, this function can take 1 arg or 2 args.
(define (token type [data #f])
  (list type data))

;;;; Token creator functions
;;;;
;;;; Given a string matching a regexp, return a token for that input or #f for
;;;; no token.

;; WHITE SPACE
(define (skip-match str) #f)

;; PUNCTUATION
(define (punctuation-token str)
  (token
    (case str
      [("(") 'OPAREN]
      [(")") 'CPAREN]
      [("{") 'OBRACE]
      [("}") 'CBRACE]
      [(",") 'COMMA]
      [(";") 'SEMICOLON]
      [(".") 'PERIOD])))

;; NUMBER LITERALS
;;;; integer
(define (integer-token str)
  (token 'INT (string->number str)))

;;;; float
(define (float-token str)
  (token 'FLOAT (string->number str)))

;; STRING LITERAL
(define (string-token str)
  token 'STRING(str))


(define (name-or-keyword-token str)
  (case str
    [("def" "fun" "if" "not" "and" "or")
     (token (string->symbol (string-upcase (string-trim str))))]
    [else (token 'NAME (string->symbol str))]))

;;;; Lexing rules table
;;;;
;;;; Each item in the table is a 2-tuple (i.e. list of 2 elements):
;;;; 1. a regexp to detect a token at the beginning of a string
;;;; 2. a function (from above) to take the matched string and create a token

(define re-table
  (list
    (list #rx"^[ \r\n\t]+" skip-match) ; works
    (list #rx"^//[^\n]+(\n|$))" skip-match) ; works
    (list #rx"^[(){};,.]" punctuation-token) ; works
    (list #rx"[-]?[0-9][0-9]*" integer-token) ;works
    (list #rx"[-]?[0-9][0-9]*\\.[0-9][0-9]*" float-token) ;works
    (list #rx"\".\"" string-token)
    (list #rx"^[A-Za-z]+|=|<|>|-" name-or-keyword-token))) ;works

;; Lex function: returns a list, and filters out #f 

;; Find match: INPUT (findmatch "def").... OUTPUT '(#f #f #f #f #f #f "def")--> returns a list 
(define (findmatch str)
  ;(filter identity (flatten (map (lambda (entry) (regexp-match (first entry) str)) re-table)))
  (flatten (map (lambda (entry) (regexp-match (first entry) str)) re-table))
)

;; Make Token: INPUT (maketoken "1.5").... OUTPUT '(FLOAT 1.5)
(define (maketoken str)
  (if (or (equal? str "(") (equal? str ")")
           (equal? str "{") (equal? str "}")
           (equal? str ",") (equal? str ";")
           (equal? str "."))
      (punctuation-token str)
      (if (or (equal? str "def") (equal? str "fun")
           (equal? str "if") (equal? str "not")
           (equal? str "and") (equal? str "or"))
          (name-or-keyword-token str)
          (if (integer? (string->number str))
              (integer-token str)
              (if (and (number? (string->number str)) (not (integer? (string->number str))))
                  (float-token str)
                  (name-or-keyword-token str))))))



(define (combo str)
  (maketoken (car (filter identity (findmatch str))))
 )


(define (lex str)
    (define str_lst (flatten(string-split str))) ;; produces list with all values
    (letrec ([helper2
              (lambda (lst)
              (if (empty?  lst)
                  '()
                  (cons (combo (first lst))
                        (helper2 (rest lst)))))])
      (helper2 str_lst)))






          
        




                 

  



           

  