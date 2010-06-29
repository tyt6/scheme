;; -*- mode:lisp; encoding:utf-8; package: :scheme.test -*-
;;! /usr/bin/sbcl --noinform --load ~/share/lib/lisp/scheme/test.lisp

(require 'fare-utils)
(require 'fare-matcher)
(require 'cl-ppcre)
(require 'scheme)
(common-lisp:defpackage :scheme.test
  (:use :common-lisp :scheme )
  (:export
   
   ))

;; --------------------------------
(common-lisp:in-package :scheme.test)

(defmacro is (ans test)
  `(progn
     (princ (prin1-to-string ',test))
          (let ((tyt-is-a ,ans)
                (tyt-is-b
                 (block is-b
                   (handler-bind
                       ((error (lambda (e) (declare (ignorable e))(return-from is-b 'is-b-test-exception)) ))
                     ,test))))
            (if (equal tyt-is-a tyt-is-b)
                (progn
                  (format t "ok~%")
                  t)
                ;; format
                (progn
                  (mapcar #'princ
                          (list
                           "predict " (prin1-to-string tyt-is-a)
                           ", but got" (prin1-to-string tyt-is-b)
                           ))
                  (format t "~%")
                  nil)))))

;; intern is case-sensitive
(is 'it (intern (symbol-name 'it)))
(is 'it (intern (subseq (symbol-name 'it1) 0 2)))
(is '|it| (intern(subseq (format nil "~a" (intern "it1")) 0 2)))
(is 'IT  (intern "IT"))

;; ^  alias-for lambda
(is  1 (funcall (^ (x) x) 1))
(is '(4 3 2 1)
    (dolist-do (e '(1 2 3 4 ) ret)
        ((ret '() (cons e ret)))))

(is 1
    (if-bind it 1
      it))

(is 1
    (aif 1
      it))

(is 1
    (when-bind it 1
      12345
      it))

(is 1
    (awhen 1
      12345
      it))

(is 3
    (and-bind it 1
              2
              3
              it))

(is 3
    (aand  1
           2
           3
           it))


;;;; let1
(is 3 (let1 it (+ 1 2) it))
;; named-let
(is '(3 2 1)
    (letp lp ((lis '(1 2 3))
              (ret '()))
      (if lis
          (lp (cdr lis) (cons (car lis) ret))
          ret)))
(is nil (letp lp () nil))

;; pathname
(is  t (and (rxmatch "/home/[a-zA-Z0-9]+/lisp" (x->string(expand-home "lisp"))) t))

;; This may be a security hole in rare case.
(is "abadcafe"
    (progn
      (with-output-file (oport "/tmp/scheme-test-a.txt")
        (format oport "abad~a" "cafe"))
      (with-input-file (iport "/tmp/scheme-test-a.txt")
        (read-line iport nil 1))
      ))
;; unlink "/tmp/scheme-test-a.txt"
(is
 '(3 2 1)
 (foldl #'xcons nil '(1 2 3)))

(is
 '(3 6 2 5 1 4) 
 (foldl (^ (kn x y) (cons* x y kn )) nil '(1 2 3) '(4 5 6) ))

(is
 '(3 6 9 2 5 8 1 4 7)
 (foldl (^ (kn x y z) (cons* x y z kn)) nil '(1 2 3) '(4 5 6) '(7 8 9) ))

(is
 '(3 6 9 2 5 8 1 4 7)
 (foldln (^ (kn x y z) (cons* x y z kn)) nil '((1 2 3) (4 5 6) (7 8 9))))


(is
 "test"
 (with-output-to-string (*standard-output*)
   (display "test")))

(is
 "test"
 (with-output-to-string (out)
   (display "test" out)))

;; displaying symbol is not case-sensitive
;; and complex number's format is #C(x y).
(is
 "(test 'TESTS TESTK 1 1.1 1/4 #C(1 2))"
 (with-output-to-string (out)
   (display '("test" 'tests :testk 1 1.1 1/4 #c(1 2)) out)))

;;(display '("test" 'tests :testk 1 1.1 1/4 1+2i)) => "(test 'tests testk 1 1.1 1/4 1.0+2.0i)"

(is t (symbol? 'eq))
(is nil (symbol? :eq))
(is t (eq (symbol? :eq) (not (symbolp :eq))))

(is t (keyword? :eq))
(is '(t t nil) (mapcar string? '("" "" eq)))
(is '(t t t t nil nil)
    (mapcar number?
            '(1 1.1 1/4 #C(1 1) (1) #(1))))

;; (is '(t t t t nil nil) (map number? '(1 1.1 1/4 1+i (1) #(1))))

(is '(nil nil t) (mapcar pair? '(() 1 (1 . 2))))
(is '(t nil nil) (mapcar null? '(() 1 (1 . 2))))
(is  "(1 test TESTK TESTS #C(1 2))" (x->string '(1 "test" :testk tests #c(1 2))))

(is  "(1 test TESTK TESTS #C(1 2))" (x->string '(1 "test" :testk tests #c(1 2))))

;; (eq? '|it| (string->symbol "it")) => #t
(is  '|it| (string->symbol "it"))
(is 'IT (string->symbol "IT"))

;; this is not good symbol->string take 2nd argument
(is nil (eq 'it (string->symbol "IT" :scheme)))

;; symbol->string is case-insensitive & this can take keywords.
(is "TEST" (symbol->string 'test))

;; (is 'is-b-test-exception (symbol->string :test))

;; This test will pass only first time.
(is 'is-b-test-exception
    (mapcar proper-list? '()))

(function-to-variable-table)

(is '()
    (mapcar proper-list? '()))

(is '(1 1.1 1/4 #C(1 1))
    (mapcar string->number '("1" "1.1" "1/4" "#C(1 1)")))

(is 5 (begin 1 3 4 5))
(is 1 (begin0 1 3 4 5))
(is 5 (receive (x y) (values 1 4)
        (+ x y)))
(is 5 (let1 x 1 (+ x 4)))
(define (test-x x . xs) (apply '* (+ 1 x) xs))
(is 36 (test-x 5 2 3))
(let ((x 99)
      (y nil))
  (is 101 (inc! x 2))
  (is 102 (inc! x))
  (is 101 (dec! x))
  (is 99 (dec! x 2))
  (push! y 1)
  (is '(1)  y))

(is
 t
 (progn
   (with-output-file (oport "/tmp/scheme-test-a.txt")
     (format oport ""))
   (eof-object?
    (print 
     (with-input-file (iport "/tmp/scheme-test-a.txt")
       (read-line iport nil +eof-object+))))))

(is nil (equal #(1) #(1)))
(is nil (equal (lambda (x) x) (lambda (x) x)))


;; gauche do not return multiple values.
(is (list '( (1 2)
            ("test" #C(1 2) )
            ) t)
    (progn
      (with-output-file (oport "/tmp/scheme-test-a.txt")
        (format oport "(1 2) ( \"test\" #C(1 2)) "))
      (multiple-value-list (file->sexp-list "/tmp/scheme-test-a.txt"))))

;; gauche do not return multiple values.
(is '(("test1""test2""test3") t)
    (progn
      (with-output-file (oport "/tmp/scheme-test-a.txt")
        (format oport "test1~%test2~%test3"))
      (multiple-value-list (file->string-list "/tmp/scheme-test-a.txt"))))

;; letl test
(is 1
    (letl (x) '(1)
      x))

(is '(1 . 2)
    (letl (x y) '(1 2)
      (cons x y)))

;; This pattern is not match. Because of pattern is list who have 4 element,
;;  but given list have 5 element.
(is 'is-b-test-exception
    (letl (x y z a) '(1 2 3 4 5)
      (list* x y z a)))

(is '(1 2)
    (letl (x . y) '(1 2)
      (cons x y)))

(is '(1 2 3 4 5)
    (letl (x y z . a) '(1 2 3 4 5)
      (list* x y z a)))

(is '(1 2 (1 2) 3)
    (letl ((x y) . res) '((1 2) (1 2) 3)
      (cons* x y res)))

(is 'is-b-test-exception
    (letl   ( (x1  x2  x3) . res1)  '((1 2 3 4 5) 6)
      (cons* x1 x2 x3 res1)))

(is '(1 2 1 2 3)
    (letl ((x1 x2) (y1 y2) . res) '((1 2) (1 2) 3)
      (cons* x1 x2 y1 y2 res)))

(is '(1 (2) 1 (2) 3)
    (letl ((x . xs) (y . ys) . res) '((1 2) (1 2) 3)
      (cons* x xs y ys res)))

(is '(30 (31) ((32 33) 21)  1)
    (letl (((x1 . xs) . res2) . res1) '(((30 31) (32 33) 21) 1)
      (cons* x1 xs res2 res1)))

(is '(30 (31) (32 33) (21) 1) 
    (letl   ( ( (x1 . x2) . (y1 . y2) ) . res1)  '(((30 31) (32 33) 21) 1)
      (cons* x1 x2 y1 y2 res1)))

;; umatch
(is 2
    (umatch '("str" b)
      ( ("st".  xs)  1)
      ( ("str".  xs)  2)
      (_ t)))

(is 2
    (umatch '(sym b)
      ( ('sy .  xs)  1)
      ( ('sym .  xs)  2)
      (_ t)))

(is 2
    (umatch '(12345678 b)
      ( (1234567 .  xs)  1)
      ( (12345678 .  xs)  2)
      (_ t)))

(is (list 'a 'b)
    (umatch '( a b )
      ( ((? symbol? a) (? symbol? b))
        (list a b))))

;;(let1 ht (make-hash-table)
;;  (match ht
;;   ( ("key" -> value) (setf (gethash 1 ht) 2) ht)))

(is
 '(2 a 1 "b")
 (umatch #( (a b) (1 2) ("a" "b"))
   ( #( (x 'baz) (c 3) ("az" f)) (list 1 x  c f))
   ( #( (x 'b) (c 2) ("a" f)) (list 2 x  c f))
   (x (list x))))

(is 0
    (umatch '()
      ( '() 0)
      ( () 1)
      ( nil 2)
      (t 3)))

(defclass foo ()
  ((x :initarg :x :accessor foo-x)
   (y :initarg :y :accessor foo-y)))
(defclass bar (foo)
  ((z :initarg :z :accessor bar-z)))

;; match list pattern.
(is nil ;; '(1 2)
    (umatch  (make-instance 'foo :x 1 :y 2)
      ((instance foo :x (? numberp x) :y (? numberp y)) (list x y))))
(is nil ;; '(1 2)
    (umatch (make-instance 'bar :x 1 :y 2 :z 3)
      ((instance foo :x x :y y) (list x y))))
(is nil ;; '(1 3)
    (umatch (make-instance 'bar :x 1 :y 2 :z 3)
      ((slot* (x a) (z b)) (list a b))))
(is nil ;; '(1 3)
    (umatch (make-instance 'bar :x 1 :y 2 :z 3)
      ((accessor* (foo-x a) (bar-z b)) (list a b))))

(is t (proper-list? '()))
(is nil (proper-list? 'a))
(is t (dotted-list? 'a))

(is t (proper-list? '(1 2)))
(is nil (circular-list? '(1 2)))
(is nil (dotted-list? '(1 2)))

(is nil (proper-list? (cons 1 2)))
(is nil (circular-list? (cons 1 2)))
(is t (dotted-list? (cons 1 2)))

(is nil (proper-list? (circular-list 1 2)))
(is t (circular-list? (circular-list 1 2)))
(is nil (dotted-list? (circular-list 1 2)))

(is 4950 (apply #'+ (iota 100)))

(is 'is-b-test-exception (take 100 '()))
(is '(1 2) (take 2 '(1 2 3)))

(is '() (take* 100 '()))
(is '(1 2) (take* 2 '(1 2 3)))

(is 'is-b-test-exception (drop 100 '()))
(is '(3) (drop 2 '(1 2 3)))
(is '(1 3 5 7 9) (filter 'oddp (iota 10)))
(is 4 (list-ref (iota 10) 4))
(is (iota 10 10)(take-while (cut < <> 20) (iota 20 10)))

;; string
(is #\t (string-ref "test" 0))
(let* ((x "2")
       (y (string-copy x)))
  (setf (aref x 0) #\3)
  (is "3" x)
  (is "2" y))

(is (list #\a #\b #\c)(string->list "abc"))
(is "abc"(list->string '(#\a #\b #\c)))
(is "a,b,c" (string-join '("a" "b" "c") ","))
(is "13c" (string-append "1" "3" "c"))
(is "abc" (string-concatenate '("a" "b" "c")))
(is '("a" "b" "c") (string-split "a,b,c" #\,))
;;(is '("a" "b" "c") (string-split "a,b,c" ","))
(is "test" (string-trim-both " test "))

(is " " (string-trim-both "test test" "test"))
;; (is "13cあ" (string-append "1" "3" "c" "あ"))
;; 
;; (string-split "/aa/bb//cc" #\/)    => ("" "aa" "bb" "" "cc")
;; (string-split "/aa/bb//cc" "/")    => ("" "aa" "bb" "" "cc")
;; (string-split "/aa/bb//cc" "//")   => ("/aa/bb" "cc")
;; (string-split "/aa/bb//cc" #[/])   => ("" "aa" "bb" "cc")
;; (string-split "/aa/bb//cc" #/\/+/) => ("" "aa" "bb" "cc")
;; (string-split "/aa/bb//cc" #[\w])  => ("/" "/" "//" "")
;; (string-split "/aa/bb//cc" char-alphabetic?) => ("/" "/" "//" "")
;; 
;; ;; some boundary cases
;; (string-split "abc" #\/) => ("abc")
;; (string-split ""    #\/) => ("")

(is
 '((3 6 9) 3)
 (let ((x 2))
   (list
    (mapcar (cute * (inc! x 1) <>) '(1 2 3))
    x)))

(use :parse-number)
(is 1 (parse-number"1"))

(defclass <x> ()
  (x))

(is 1
    (let((x (make-instance '<x> )))
      (setf (slot-ref x 'x) 1)
      (slot-ref x 'x)))

(is 2
    (assoc-ref '((a . 2) (b . 3)) 'a))

(is 3
    (assoc-ref '((a . 2) (b . 3)) 'b))

(is 4
    (assoc-ref '((a . 2) (b . 3)) 'c 4))

;; rxmatch
(is nil (rxmatch "^test" "sx"))
(is "1234"  (funcall (rxmatch "^test([0-9]+)" "test1234") 1))
(is '("1234" "abad" "CAFE")
    (rxmatch-if ("^test([0-9]+)([a-z]+)([A-Z]+)$" "test1234abadCAFE")
        (_ num az laz) (list num az laz)))

(is :nil
    (rxmatch-if ("^test([0-9]+)([a-z]+)([A-Z]+)$" "")
        (_ num az laz) (list num az laz)
      :nil))

(rxmatch-if ("^test([0-9]+)([a-z]+)([A-Z]+)$" "")
        (_ num az laz) (list num az laz)
      :nil)

(defun sort-alist (alist &key (test #'<))
  (sort alist test :key #'car))

(let ((ht (make-hash-table :test #'equal)))
  (hash-table-put! ht "test" 1)
  (hash-table-put! ht "test3" 4)
  (is '(1 :nil)
      (list (hash-table-get ht "test") (hash-table-get ht "test2" :nil)))
  (is '(("test" . 1) ("test3" . 4))
      (sort-alist (hash-table->alist ht) :test #'string<))
  (is '("test" "test3")
      (sort (hash-table-keys ht) #'string<))
  (is '(1 4)
      (sort (hash-table-values ht) #'<))
  )


(is '(1 2 3 4 5)
    (nub '(1 2 1 2 1 2 3 4 5)))
(is '(1 1 1 1 1) (mapcar (const 1) '(1 2 3 4 5)))
;; (is '( ) (mapcar (const (random 26)) (iota 10)))
(is t (== '(1 2 3) '( 1 2 3)))
(is t (/== '(1 2 3) '( 1 2 3 4)))
;; nub [1,2,1,2,1,2,3,4,5]

;; inline aif
(defun test222(x)
  (scheme-test-function x))

(defun test223(x)
  (scheme-test-function2 x))

(is '(100 100) (test222 100))
(is nil (test222 nil))
(is '(12 12) (test223 12))
(is nil (test223 nil))

(is '(3 nil)
    (umatch '(1 :plus 2)
      ((x :plus y . xs) (list (+ x y) xs))))

(is 3
    (letl (x :plus y) '(1 :plus 2)
      (+ x y)))



;; Bench Marking
;; (defpure +)
;; 
;; Known functions compiled intelligence way.
;;(time
;; (dotimes (i 10000000)
;;   (funcall + 1 2)))
;;(time
;; (dotimes (i 10000000)
;;   (+ 1 2)))

