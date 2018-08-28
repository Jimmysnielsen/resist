#lang racket/base
(require racket/math
         rackunit)

;;;; resist version 0
;;;; by jimmy s. nielsen
;;;; contact jimmysnielsen@ginishi.dk
;;;; init 2018-08-27
;;;; purpose: solve a resistor network
;;;; loosely based on similar program in LISP on uLisp.com

;;; constants
(module+ test
  (define test-circuit '((a d ?) (a b 10)(b a 15)(b d 5)(a c 5)(c d 6)))) ;used for testing, see graphical representation at /pictures/resistornetwork.gif 

;; Number Number -> Number
;; return value for resistors in series
(define series (lambda (x y)
                 (+ x y)))
(module+ test
  (check-equal? (series 1 1) 2)
  (check-equal? (series 47.8 47.8e3)(+ 47.8 47.8e3)))

;; Number Number -> Number
;; return value for resistors in parallel
(define parallel (lambda (x y)
                   (/ (+ (/ x) (/ y)))))

(module+ test
  (check-equal? (parallel 10 10) (/ (+ (/ 10) (/ 10))))
  (check-equal? (parallel 10 15) (/ (+ (/ 10) (/ 15)))))

;; List-of-X Number -> (list List-of-X List-of-X)
;; return subset of loX
#;(define split-set (lambda (lox b)
                      (list '() '()))) ; stub

(define split-set (lambda (lox b)
                    (let* ((len (length lox))
                           (padding (make-string len #\0))
                           (rawstring (string-append padding (number->string b 2)))
                           (bitstring (substring rawstring (- (string-length rawstring) len))))
                      (define (split-collect bitstr lox lox1 lox2)
                        (cond ((equal? "" bitstr) (list lox1 lox2))
                              ((equal? "0" (substring bitstr 0 1))
                               (split-collect (substring bitstr 1) (cdr lox) lox1 (append lox2 (list (car lox)))))
                              ((equal? "1" (substring bitstr 0 1))
                               (split-collect (substring bitstr 1) (cdr lox) (append lox1 (list (car lox))) lox2))
                              (#t "should never reach this")))
                      ;do it
                      (split-collect bitstring lox null null))))

(module+ test 
  (check-equal? (split-set '() #b1) (list '() '())) 
  (check-equal? (split-set '(a b c) #b0) (list '() '(a b c))) 
  (check-equal? (split-set '(a b c d) #b1101) (list '(a b d) '(c)))
  (check-equal? (split-set '(a b c d) 13) (list '(a b d) '(c))))


;!!! to do
(define series-parallel (lambda (l x y) null));stub

(module+ test
  (check-equal? (series-parallel test-circuit '(a b 10) '(b a 15)) '((b a 6.0)))
  (check-equal? (series-parallel test-circuit '(a c 5) '(c d 6)) '((a d 11)))
  (check-equal? (series-parallel test-circuit '(a b 10) '(b d 5)) null))

;!!! todo
(define countlinks (lambda (l x) null));stub

;!!! todo
(define simplify (lambda (l fn n) null)); stub

;!!! todo
(define solve (lambda (circuit) null)) ;stub

(module+ test
  (check-equal? (solve test-circuit) 5.5))

;!!! todo
(define delta-wye (lambda (l x y z) null)) ;stub

(module+ test
  (check-equal? (delta-wye '(a b 3) '(b c 1) '(c a 2)) ;;used for testing, see graphical representation at /pictures/deltawye.gif 
                '((a 1 1) (b 1 0.5) (c 1 1/3))))

;!!! todo
(define floating (lambda (l) null)); stub

; final test
;!!!
#;(module+ test
    (define *final-test* '((a f ?) (a b 220)(a d 100)(b d 120)(b c 270)(d e 150)(c e 220)(c f 270)(e f 110)))
    (check-equal? (solve *final-test*) 0.0)) ;!!! 







                   