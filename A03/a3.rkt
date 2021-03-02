#lang racket
(require (only-in (file "lex.rkt") lex))

;; global variable tokens
(define tokens (make-parameter '()))

;HELPER FUNCTIONS
;; define CONSUME function (from lecture)
(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))  ; update tokens: remove first token
    token))

;; define CHECK function (from lecture, with modifications)
;;; returns the type of first token 
(define (check)
  (if (empty? (tokens))
      'EMPTY
      (first (first (tokens)))))

;; isTypeNum?
(define (isTypeNum? type)
  (or (equal? type 'INT)
      (equal? type 'FLOAT)))



; PARSE RULES
;; program     := exprList
(define (parse-program)
  (list 'program
        (parse-exprList)))

;; exprList    := expr optExprList
(define (parse-exprList)
  (list 'exprList
        (parse-expr)
        (parse-optExprList)))
  
;; optExprList := ɛ | exprList
(define (parse-optExprList)
  (let ([t (check)]) ;;get type of first token
    (if (or (equal? t 'CPAREN)
            (equal? t 'EMPTY))
        '(optExprList)
        (list 'optExprList (parse-exprList)))))

;; expr        := atom | invocation
(define (parse-expr)
  (list 'expr
        (if (equal? (check) 'OPAREN)
            (parse-invocation)
            (parse-atom))))

;; atom        := NAME | STRING | number
(define (parse-atom)
  (list 'atom
        (if (isTypeNum? (check))
            (parse-number)
            (if (equal? (check) 'STRING)
                (consume 'STRING)
                (consume 'NAME)))))

;; number      := INT | FLOAT
(define (parse-number)
  (list 'number
        (if (equal? (check) 'FLOAT)
            (consume 'FLOAT)
            (consume 'INT))))

;; invocation  := OPAREN exprList CPAREN
(define (parse-invocation)
  (list 'invocation
        (consume 'OPAREN)
        (parse-exprList)
        (consume 'CPAREN)))



;PARSE FUNCTION
(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))
  
