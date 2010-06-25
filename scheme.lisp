;;; -*-encoding:utf-8; mode:Lisp -*-

;;;   Copyright (c) 2010  tyt6  <abadcafe7@gmail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  /scheme/scheme.lisp,v 0.0.0

(defpackage :scheme
  (:use :common-lisp :fare-matcher
        )
  (:import-from :fare-utils with-gensyms)
  (:export
   #:+eof-object+

   #:^
   ;; flow control
   #:if-bind #:aif #:when-bind #:awhen #:and-bind #:aand
   #:dolist-do
   ;;
   #:macroexpand-recursive
   #:defun-expand  #:defun-inline
   #:defun-alias #:defun-alias-scheme
   #:expand-home
   
   ;; IO
   #:with-input-file #:with-output-file
   
   ;; not scheme
   #:foldl #:foldl1 #:foldl2 #:foldln
   ;; scheme
   #:display #:symbol? #:string? #:number? #:keyword? #:pair? #:null?
   #:x->string #:string->symbol #:symbol->string #:string->number
   #:define #:begin #:begin0 #:receive
   #:let1 #:letp
   #:eq? #:eqv? #:equal?
   ;; port
   #:eof-object? #:file->list
   #:file->sexp-list #:file->string-list

   ;; util.match
   #:umatch #:letl

   ;; srfi-1
   #:proper-list? #:dotted-list? #:circular-list?
   #:iota #:xcons
   #:cons*  #:circular-list
   #:take #:take*
   #:drop #:reverse! #:filter #:list-ref
   #:take-while

   ;; 13
   #:string-append 
   
   ;; 26
   #:cut #:cutr

   ;;gauche
   #:use
   ;; object
   #:slot-ref

   ;;regexp
   #:rxmatch
   #:rxmatch-if
   
   ;; #:hash-table
   #:hash-table-put! #:hash-table-get
   #:hash-table->alist #:hash-table-keys #:hash-table-values
   
   ;; haskell
   #:nub
   #:const
   #:==
   #:/==
   ;; #:defpure
   
   ;; for testing
   #:scheme-test-function
   #:scheme-test-function2
   ))

;; --------------------------------
(common-lisp:in-package :scheme)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (speed 0)(safety 0) (debug 3) (compilation-speed 3) (space 0)))
  ;; Constatns
  (defconstant +hash-table-size+ 16)
  (defconstant +eof-object+ :eof)
  (defconstant +length*-dot+ -1)
  (defconstant +length*-circular+ -2)
  
  (declaim (inline eof-object? hash-table-get))

  (defmacro ^ (args &rest body)
    `(lambda ,args .,body))
  
  (defun symbol-eq-without-package (a b)
    (and (symbolp a) (symbolp b) (string= (symbol-name a) (symbol-name b))))
  ;; (symbol-eq-without-package 'fare-matcher::match 'match)
  
  (defmacro dolist-do  ((var lis &rest rest) vars &rest body)
    (let ((%lis (gensym "dolist-do")))
      `(do* ,(append vars (list `(,%lis ,lis (cdr ,%lis)) `(,var (car ,%lis) (car ,%lis))))
            ((null ,%lis) ,@rest)
         ,@body)))
  ;; flow control
  (defmacro if-bind (var test then &rest else)
    `(let ((,var ,test))
       (if ,var ,then ,@else)))
  
  (defmacro aif (test then &rest else)
    (let ((%it (intern (symbol-name 'it))))
      `(if-bind ,%it ,test ,then ,@else)))
  
  (defmacro when-bind (var test &rest body)
    `(if-bind ,var ,test (progn ,@body)))

  (defmacro awhen ( test &rest body)
    (let ((%it (intern (symbol-name 'it))))
      `(when-bind ,%it ,test ,@body)))

  (defmacro and-bind (var &rest forms)
    (if (consp forms)
        (if (consp (cdr forms))
            `(if-bind ,var ,(car forms)
               (and-bind ,var ,@(cdr forms))
               nil)
            (car forms))
        t))

  (defmacro aand (&body body)
    (let ((%it (intern (symbol-name 'it))))
      `(and-bind ,%it ,@body)))

  ;;(defmacro function->variable (func)
  ;;  `(progn (defvar ,func  nil)
  ;;          (setq ,func (symbol-function ',func))))
  ;; (function->variable mapcar)

  (defmacro defun-alias-scheme (name func)
    `(progn
       (defvar ,name nil)
       (setq ,name (symbol-function ',func))
       ;;(defconstant ,name (symbol-function ',func))
       (setf (fdefinition ',name) (SYMBOL-FUNCTION ',FUNC))
       ))
  
  (defmacro defun-alias (name func)
    `(progn
       (setf (fdefinition ',name) (SYMBOL-FUNCTION ,FUNC)) 
       ))
  
  (defun macroexpand-recursive (lis)
    ;; listp is better but a little bit slow
    (cond ((consp lis)
           (macroexpand (cons (car lis) (mapcar #'macroexpand-recursive (cdr lis)))))
          (t lis)))
  
  (defmacro defun-expand  (name vars &body body)
    `(defun ,name ,vars
       ,@(mapcar #'macroexpand-recursive body)))
  
  (defmacro defun-inline (name vars &body body)
    `(progn
       (declaim (inline ,name))
       (defun-expand ,name ,vars ,@body)))
  
  (defun expand-home(path)
    (merge-pathnames path (user-homedir-pathname)))


  ;; --------------------------------IO
  (defmacro with-input-file ((port path &rest options &key (element-type 'base-char) if-exists if-does-not-exist)
                             &body body)
    (declare (ignorable  options))
    `(with-open-file (,port ,path :direction :input :element-type ',(intern (symbol-name element-type))
                            ,@(if if-exists (list :if-exists if-exists) '())
                            ,@(if if-does-not-exist (list :if-exists if-does-not-exist) '())
                            )
       ,@body))

  (defmacro with-output-file ((port path &rest options &key (element-type 'base-char) (if-exists :supersede) if-does-not-exist)
                              &body body)
    (declare (ignorable  options))
    `(with-open-file (,port ,path :direction :output :element-type ',(intern (symbol-name element-type))
                            ,@(if if-exists (list :if-exists if-exists) '())
                            ,@(if if-does-not-exist (list :if-exists if-does-not-exist) '())
                            )
       (if ,port
           (progn ,@body)
           (throw :file-open-error nil))))
  ;; --------------------------------
  ;; this is not a scheme's one
  (defun foldl1 (term knil lis)
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
             (type function term))
    (if lis
        (foldl1 term (funcall term knil (car lis)) (cdr lis))
        knil))

  (defun foldl2 (term knil lis lis2)
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
             (type function term))
    (if (and lis lis2)
        (foldl2 term (funcall term  knil (car lis) (car lis2)) (cdr lis) (cdr lis2))
        knil))
  
  (defun foldln (term knil llis)
    (declare (type function term)
             (type list llis))
    (if (member nil llis :test 'eq)
        knil
        (foldln term
                (apply term knil (mapcar #'car llis))
                (mapcar #'cdr llis))))
    
  (defun foldl (term knil lis &rest rest)
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
             (type function term))
    (if rest
        (if (cdr rest)
            (foldln term knil (cons lis rest))
            (foldl2 term knil lis (car rest)))
        (foldl1 term knil lis)))
  

  ;; scheme like function

  ;; DISPLAY
  ;; symbol and keyword is case-insensitive
  ;; and complex number's format is #C(x y) not a a.b+c.di.
  ;; gosh > (print 1+i) => 1.0+1.0i
   (defun display (obj &optional (stream *standard-output*))
    (format stream "~a" obj))
  
  (defun symbol? (x)
    (and (symbolp x) (not (keywordp x))))
  (defun-alias-scheme keyword? keywordp)
  
  (defun-alias-scheme string? stringp)
  (defun-alias-scheme number? numberp)
  (defun-alias-scheme pair? consp)
  (defun-alias-scheme null? null)
  ;;
  (defun-alias-scheme eq? eq)
  (defun-alias-scheme eqv? eql)
  ;; gauche's  `equal?` compares vector and vector.
  (defun-alias-scheme equal? equal)
  

  ;; symbol and keyword is case-insensitive
  ;; and complex number's format is #C(x y) not a a.b+c.di.
  (defun x->string (o)
    (format nil "~a" o))

  ;; this is not good symbol->string take 2nd argument
  (defun-alias-scheme string->symbol intern)

  ;; is not good this take :keywords
  (defun-alias-scheme symbol->string symbol-name)

  ;;
  (defun-alias-scheme string->number parse-number::parse-number)

  (defmacro begin (&body body)
    `(progn ,@body))

  (defmacro begin0 (&body body)
    `(prog1 ,@body))

  (defmacro receive (vars value &body body)
    `(multiple-value-bind ,vars ,value
       ,@body))

  (defmacro letp (name vars &rest body)
    (or (listp vars) (error "malformed letp (not list vars)"))
    `(labels ((,name ,(mapcar 'car vars)
                (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
                .,body))
       (,name .,(mapcar #'cadr vars))))
  
  (defmacro let1 (var val &body body)
    `(let ((,var ,val))
       .,body))
  
  ;; not debugging
  (defmacro define (name+vars &body body)
    (let ((func (if (pair? name+vars) (car name+vars) name+vars)))
      (labels ((define-dot->rest (vars)
                 (letp lp ((vars vars) (ret '()))
                   (cond ((consp vars)
                          (lp (cdr vars) (cons (car vars) ret)))
                         ((null vars)
                          (nreverse ret))
                         (t (nreverse (list* vars '&rest ret)))))))
        `(progn
           ,(if (consp name+vars)
                `(defun ,(car name+vars) ,(if (proper-list? (cdr name+vars))
                                              (cdr name+vars)
                                              (define-dot->rest (cdr name+vars)))
                   ,@body)
                ;; TODO : 
                ;; (define x (lambda (y) y)) make intern function table
                ;; (define x (cut + <> <>)) too.
                `(defvar ,name+vars ,@body))
           ))))
  ;;  (define (x ) 1) 
  
  ;; port
  (defun eof-object? (o)
    (eq o +eof-object+))
  
  (defun file->list (reader path)
    (declare (type string path))
    (let (ret)
      (declare (type list ret))
      (with-input-file (port path)
        (handler-bind
            ((error (lambda (e) (return-from file->list (values (nreverse ret) nil)))))
          (do ((sexp (funcall reader port nil +eof-object+) (funcall reader port nil +eof-object+)))
              ((eof-object? sexp)
               (values (nreverse ret) t))
            (declare (type (or string keyword) sexp))
            (setf ret (cons sexp ret)))))))

  ;; gauche do not return multiple values.
  (defun file->sexp-list (path)
    (file->list #'read path))
  
  ;; gauche do not return multiple values.
  (defun file->string-list (path)
    (file->list #'read-line path))
  
  (defun make-umatch-pattern (x)
    (cond  ;; ((and (consp x) (eq (car x) (slot*) ))
           ;;  ) ;; this must be recursive!
           ((and (listp x) (symbol-eq-without-package (car x) '?) (>= (length x) 3))
            `(,(intern (symbol-name 'and)) ,(caddr x) (,(intern (symbol-name 'when)) (,(cadr x) ,(caddr x) ,@(cdddr x)))))
           ((and (consp x) (consp (cdr x))
                 (symbolp (car x))
                 (symbol-eq-without-package (car x)'quote))
            (let ((%g (gensym "umatch")))
              `(,(intern (symbol-name 'and))
                 ,%g (,(intern (symbol-name 'when))
                       (,(intern (symbol-name 'eq)) ,%g ,x)
                       ))))
           ((consp x)
            (cond ((proper-list? x) `(,(intern (symbol-name 'list)) ,@(mapcar #'make-umatch-pattern x)))
                  (t (list (intern (symbol-name 'cons)) (make-umatch-pattern (car x)) (make-umatch-pattern (cdr x))))))
           ((stringp x)
            (let ((%g (gensym "umatch")))
              `(,(intern (symbol-name 'and))
                 ,%g (,(intern (symbol-name 'when))
                       (,(intern (symbol-name 'string=)) ,%g ,x)
                       ))))
           ((vectorp x)
            (list* (intern (symbol-name 'vector))
                   (map 'list #'make-umatch-pattern x)))
           ((numberp x)
            (let ((%g (gensym "umatch")))
              `(,(intern (symbol-name 'and))
                 ,%g (,(intern (symbol-name 'when))
                       (,(intern (symbol-name '=)) ,%g ,x)
                       ))))
           (t  x)))

  ;; like gauche's util.match
  (defmacro umatch ( expr &body clause)
    (let ((ret '()))
      (dolist (e clause `(match ,expr ,@(nreverse ret)))
        (push (cons (make-umatch-pattern (car e)) (cdr e)) ret))))
  
  ;; 
  (defmacro letl (pattern val &body body)
    `(letm ,(make-umatch-pattern pattern) ,val .,body))

  ;; --------------------------------
  ;; srfi-1
  (defun-alias-scheme cons* list*)
  (defun xcons (b a) (cons a b))
    
  (defun length* (lis)
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
    (letp lp ((slow lis)
              (lis lis)
              (n 0))
      (declare (type fixnum n))
      (cond ((consp lis)
             (incf n)
             (setf lis (cdr lis))
             (cond ((consp lis)
                    (if (eq lis slow)
                        +length*-circular+
                        (lp (cdr slow) (cdr lis) (1+ n))))
                   ((null lis)
                    n)
                   (t +length*-dot+)))
            ((null lis)
             n)
            (t +length*-dot+))))

  (defun proper-list? (lis)
    (> (length* lis) 0))

  (defun dotted-list? (lis)
    (= (length* lis) +length*-dot+))

  (defun circular-list? (lis)
    (= (length* lis) +length*-circular+))
  
  (defun iota (n &optional (a 0) (step 1))
    (declare (type number a step)
             (type integer n))
    (do ((n n (- n 1))
         (ret '() (cons a ret))
         (a (or a 0) (+ a step)))
        ((<= n 0) (nreverse ret))
      (declare (type fixnum n)
               (type number a step)
               (type list ret))))

  (defun circular-list (&rest rest)
    (setf (cdr (last rest)) rest))

  (defun take (n lis)
    (declare (type sequence lis))
    (subseq lis 0 n))

  (defun take* (n lis &optional fill padding)
    (declare (type sequence lis))
    (if (<= (length lis) n)
        lis
        (subseq lis 0 n)))
  
  (defun drop (n lis)
    (declare (type sequence lis))
    ;; (nthcdr 2 '(1 2 3 4))
    (subseq lis n))

  (defun filter (pred lst)
    (declare (type sequence lst))
    (remove-if-not pred lst))

  ;; remove is a common lisp function
  
  (defun-alias-scheme reverse! nreverse)

  (defun list-ref(lis n)
    (nth n lis))

 (defun take-while (pred lis)
    (do ((ret '() (cons (car lis) ret))
         (lis lis (cdr lis)))
        ((not (and lis (funcall pred (car lis))))
         (nreverse ret))))

  ;;srfi-13
  (defun string-append (&rest args)
    (apply #'concatenate 'string args))

  ;; srfi-26
  (defmacro cut (func &rest forms)
    (let (args body)
      (dolist (elem forms)
        (cond ((symbol-eq-without-package elem '<>)
               (let ((var (gensym "cut:")))
                 (push var args)
                 (push var body)))
              (t (push elem body))))
      `(lambda ,(nreverse args) (,func ,@(nreverse body)))))

  (defun cutr-make-variable (sym ret-var)
    (let ((str (symbol-name sym)))
      (cond ((rxmatch "^<[0-9]+>$" str)
             (let* ((n (- (length ret-var) 1 (parse-integer (subseq str 1 (1- (length str))))));; (rxmatchi reg str 1)でいい。
                    (var (nth n ret-var)))
               (if var
                   (cons ret-var var)
                   (let ((sym (gensym "cutr-")))
                     (acons sym ret-var sym)))))
            (t (cons ret-var sym)))))
  (defmacro cutr (func &rest forms)
    (labels ((cutrin (lis ret-var)
               (cond ((null lis)
                      (cons ret-var '())) ;; 
                     ((consp lis)
                      (destructuring-bind (aret-var . aret) (cutrin (car lis) ret-var)
                        (destructuring-bind (dret-var . dret) (cutrin (cdr lis) aret-var)
                          (cons dret-var (cons aret dret)))))
                     ((symbol-eq-without-package lis '<>)
                      (let ((it (gensym "cutr-")))
                        (acons it ret-var it)))
                     ((symbolp lis)
                      (cutr-make-variable lis ret-var))
                     (t
                      (cons ret-var lis)))))
      (destructuring-bind (vars . ret) (cutrin forms '())
        `(lambda ,(nreverse vars) (,func ,@ret)))))

  ;; --------------------------------
  ;; gauche
  (defmacro use (&rest args)
    `(progn ,@(mapcar #'use-package args)))

  ;; object
  (defun slot-ref (obj key &optional (def nil))
    (if (slot-boundp obj key)
        (slot-value obj key)
        def))
  (defun (setf slot-ref) (new-value obj key)
    (setf (slot-value obj key) new-value))

  ;; --------------------------------

  ;; regexp
  (defun rxbase (reg str)
    (declare (string str reg))
    (let ((scanner (cl-ppcre:create-scanner reg :case-insensitive-mode nil :multi-line-mode nil)))
      (cl-ppcre:scan scanner str)))

  (defun rxmatch (reg str)
    (declare (string str reg))
    (multiple-value-bind (start end sts ens) (rxbase reg str)
      (and start (lambda (i) (if (= i 0) (subseq str start end)(subseq str (aref sts (- i 1)) (aref ens (- i 1))))))))
  
  (defmacro rxmatch-if ((reg str) result then &body else)
    (with-gensyms (%str  %sts %ens %start %end)
      (let ((result? (symbol-eq-without-package (car result) '_))
            (nullstr? (and (stringp str) (string= str ""))))
        `(let* ((,%str ,str))
           (multiple-value-bind (,%start ,%end ,%sts ,%ens) (rxbase ,reg ,%str)
             ,@(if result? `((declare (ignorable ,%end ,@(if nullstr? (list %ens) nil)))) nil)
             (if ,%start
                 (let ,(let ((sexp (mapcar (lambda (var i) (if nullstr?
                                                               `(,var (scheme::if-bind it (aref ,%sts ,i) ""))
                                                               `(,var (scheme::if-bind it (aref ,%sts ,i) (subseq ,%str it (aref ,%ens ,i))))))
                                           (cdr result) (iota (length (cdr result))))))
                            (if result?
                                sexp
                                (cons (if nullstr?
                                          (list (car result) "")
                                          `(,(car result) (subseq ,%str ,%start ,%end)))
                                      sexp)))
                   ,then)
                 ,@else))))))

  ;; --------------------------------
  
  ;; hash-table
  (defmacro hash-table-put! (ht k v)
    `(setf (gethash ,k ,ht) ,v))

  (defun hash-table-get (ht k &optional default)
    (gethash k ht default))

  (defun hash-table->alist (ht)
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
             )
    (let ((ret '()))
      (maphash (^ (k v) (push (cons k v) ret)) ht)
      ;;(nreverse ret)
      ret))

  (defun hash-table-keys (ht &optional order)
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0))
             )
    (let (ret)
      (maphash (lambda (k v) (push k ret))ht)
      (if order (nreverse ret) ret)))

  (defun hash-table-values (ht &optional order)
    (let (ret)
      (maphash (lambda (k v) (push v ret))ht)
      (if order (nreverse ret) ret)))

  ;; --------------------------------
  ;; Haskell
  (defun nub (lis)
    (dolist-do (e lis (nreverse ret))
        ((ret '() (if (member e ret) ret (cons e ret))))))
  
  (defmacro const (x)
    `(lambda (&rest args) (declare (ignorable args)) ,x))

  ;; generic == 
  (defun-alias-scheme == equal)
  (defun /== (a b)
    (not (equal a b)))

  ;; func should be compiled.
  ;;(defmacro defpure (func)
  ;;  `(defconstant ,func (symbol-function ',func)))
  
  ;; test
  (declaim (inline scheme-test-function))
  (defun-expand scheme-test-function (x)
    (aif x (list it x)))
  
  (defun-inline scheme-test-function2 (x)
    (aif x (list it x)))

  )
