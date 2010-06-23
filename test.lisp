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
                (format t "ok~%")
                ;; format
                (progn
                  (mapcar #'princ
                          (list
                           "predict " (prin1-to-string tyt-is-a)
                           ", but got" (prin1-to-string tyt-is-b)
                           ))
                  (format t "~%"))))
          t))

;; intern is case-sensitive
(is 'it (intern (symbol-name 'it)))
(is 'it (intern (subseq (symbol-name 'it1) 0 2)))
(is '|it| (intern(subseq (format nil "~a" (intern "it1")) 0 2)))
(is 'IT  (intern "IT"))

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

;; let-match test
(is 1
    (letl (x) '(1)
      x))

(is '(1 . 2)
    (letl (x y) '(1 2)
      (cons x y)))

(is '(1 2 . 3)
    (letl (x y z) '(1 2 3)
      (list* x y z)))

;; This pattern is not match. Because of pattern is list who have 4 element,
;;  but get list who have 5 element.
(is 'is-b-test-exception
    (letl (x y z a) '(1 2 3 4 5)
      (list* x y z a)))

(is '(1 2  3 4 . 5)
    (letl (x y z a b) '(1 2 3 4 5)
      (list* x y z a b)))

(is '(1 2)
    (letl (x . y) '(1 2)
      (cons x y)))

(is '(1 2 3)
    (letl (x y . z) '(1 2 3)
      (list* x y z)))

(is '(1 2 3 4 5)
    (letl (x y z . a) '(1 2 3 4 5)
      (list* x y z a)))

(is '(1 2  3 4 5)
    (letl (x y z a . b) '(1 2 3 4 5)
      (list* x y z a b)))

(is '(1 2 (1 2) 3)
    (letl ((x y) . res) '((1 2) (1 2) 3)
      (cons* x y res)))

(is 'is-b-test-exception
    (letl   ( (x1  x2  x3) . res1)  '((1 2 3 4 5) 6)
      (cons* x1 x2 x3 res1)))

(is '(1 (2) (1 2) 3)
    (letl ((x . y) z . res) '((1 2) (1 2) 3)
      (cons* x y z res)))

(is '(1 2 (1 2) 3)
    (letl ((x y) z . res) '((1 2) (1 2) 3)
      (cons* x y z res)))

(is '(1 (2) (1 2) 3)
    (letl ((x . xs) . res) '((1 2) (1 2) 3)
      (cons* x xs res)))

(is '(1 2 (3 4 5) 6)
    (letl   ( (x1  x2 . xs) . res1)  '((1 2 3 4 5) 6)
      (cons* x1 x2 xs res1)))

(is '(1 2 3 (4 5) 6)
    (letl   ( (x1 x2 x3 . xs) . res1)  '((1 2 3 4 5) 6)
      (cons* x1 x2 x3 xs res1)))

(is '(1 2 1 2 3)
    (letl ((x1 x2) (y1 y2) . res) '((1 2) (1 2) 3)
      (cons* x1 x2 y1 y2 res)))

(is '(1 (2) 1 (2) 3)
    (letl ((x . xs) (y . ys) . res) '((1 2) (1 2) 3)
      (cons* x xs y ys res)))

(is '(30 31 ((32 33) 21)  1)
    (letl (((x1 x2) . res2) . res1) '(((30 31) (32 33) 21) 1)
      (cons* x1 x2 res2 res1)))

(is '(30 (31) ((32 33) 21)  1)
    (letl (((x1 . xs) . res2) . res1) '(((30 31) (32 33) 21) 1)
      (cons* x1 xs res2 res1)))

(is '(30 (31) ((32 33) 21)  1)
    (letl (((x1 . xs) . res2) . res1) '(((30 31) (32 33) 21) 1)
      (cons* x1 xs res2 res1)))

(is '(30 (31) (32 33) (21) 1)
    (letl   ( ( (x1 . x2) y1 . y2) . res1)  '(((30 31) (32 33) 21) 1)
      (cons* x1 x2 y1 y2 res1)))

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

;; inline aif
(defun test222(x)
  (scheme-test-function x))

(defun test223(x)
  (scheme-test-function2 x))

(is '(100 100) (test222 100))
(is nil (test222 nil))
(is '(12 12) (test223 12))
(is nil (test223 nil))
