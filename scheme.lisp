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
;;;  /scheme/scheme.lisp,v 0.0.1

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
   #:letl    #:cutr

   ;; 
   #:function-to-variable-table
    
   ;; IO
   #:with-input-file #:with-output-file
   
   ;; not scheme's one
   #:foldl #:foldl1 #:foldl2 #:foldln

   ;; r5rs

   ;; r6rs
   
   ;; scheme
   #:display #:symbol? #:string? #:number? #:keyword? #:pair? #:null?
   #:x->string #:string->symbol #:symbol->string #:string->number
   #:define #:begin #:begin0 #:receive
   #:let1 #:letp
   #:eq? #:eqv? #:equal?
   ;; macros
   #:push! #:inc! #:dec!
   ;; port
   #:eof-object? #:file->list
   #:file->string #:file->string-list #:file->sexp-list
   #:string->file #:string-list->file #:sexp->file
   
   ;; util.match
   #:umatch

   ;; srfi-1
   #:proper-list? #:dotted-list? #:circular-list?
   #:iota #:xcons
   #:cons*  #:circular-list
   #:take #:take*
   #:drop #:reverse! #:filter #:list-ref
   #:take-while

   ;; 13
   #:string-ref #:string-copy
   #:string-null? #:string->list #:list->string
   #:string-append #:string-trim-both
   #:string-concatenate
   #:string-join  #:string-split
   
   ;; 26
   #:cut #:cute
   
   ;;gauche
   #:use

   ;; util.list
   #:assoc-ref
   
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
  (defmacro define-constant (name value &optional doc)
    "A version of DEFCONSTANT for, cough, /strict/ CL implementations."
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc))))
  
  (proclaim '(optimize (speed 0)(safety 0) (debug 3) (compilation-speed 3) (space 0)))
  ;; Constatns
  (defconstant +hash-table-size+ 16)
  (defconstant +eof-object+ :eof)
  (defconstant +length*-dot+ -1)
  (defconstant +length*-circular+ -2)
  (define-constant +white-space+ (concatenate 'string '(#\Space #\Tab #\Newline)))
  
  (declaim (inline eof-object? hash-table-get))

  (defmacro ^ (args &rest body)
    `(lambda ,args .,body))

  (defun intern* (symbol &optional package)
    (if package (intern (symbol-name symbol) package)
        (intern (symbol-name symbol))))
  
  (defun symbol-eq-without-package (a b)
    (and (symbolp a) (symbolp b) (string= (symbol-name a) (symbol-name b))))
  ;; (symbol-eq-without-package 'fare-matcher::match 'match)
  
  (defmacro dolist-do  ((var lis &rest rest) vars &rest body)
    (let ((%lis (gensym "dolist-do")))
      `(do* ,(append vars (list `(,%lis ,lis (cdr ,%lis)) `(,var (car ,%lis) (car ,%lis))))
            ((null ,%lis) ,@rest)
         ,@body)))

  (defmacro cons-if (pred ret &optional e)
    (let ((g (gensym "cons-if")))
      `(if  ,pred (cons ,e ,ret) ,ret)))

  (defmacro cons-if-not (pred ret e)
    (let ((g (gensym "cons-if-not")))
      `(if  ,pred ,ret (cons ,e ,ret))))
  
  ;; flow control
  (defmacro if-bind (var test then &rest else)
    `(let ((,var ,test))
       (if ,var ,then ,@else)))
  
  (defmacro aif (test then &rest else)
    `(if-bind ,(intern* 'it) ,test ,then ,@else))
  
  (defmacro when-bind (var test &rest body)
    `(if-bind ,var ,test (progn ,@body)))

  (defmacro awhen ( test &rest body)
    `(when-bind ,(intern* 'it) ,test ,@body))

  (defmacro and-bind (var &rest forms)
    (if (consp forms)
        (if (consp (cdr forms))
            `(if-bind ,var ,(car forms)
               (and-bind ,var ,@(cdr forms))
               nil)
            (car forms))
        t))

  (defmacro aand (&body body)
    `(and-bind ,(intern* 'it) ,@body))

  (defmacro function->variable (func)
    `(progn (defvar ,func  nil)
            (setq ,func (symbol-function ',func))))
  ;; (function->variable mapcar)

  ;; On next version, defun-alias-scheme is not used in this file.
  ;; use function-to-variable-table.
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
       (if ,port
           (progn ,@body)
           (throw :file-open-error nil))))

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
    (if (some #'null llis)
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
  ;; gosh > (display 1+i) => 1.0+1.0i
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
  
   ;;(defun-alias-scheme negative? negativep)
   ;;(defun-alias-scheme positive? positivep)
   (defun negative? (obj)
     (< obj 0))
   (defun positive? (obj)
     (> obj 0))
   (defun-alias-scheme zero? zerop)
   (defun-alias-scheme odd? oddp)
   (defun-alias-scheme even? evenp)
   
   (defun-alias-scheme module mod)
   (defun-alias-scheme quotient truncate)
   (defun-alias-scheme remainder rem)
   
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

   ;; named-let
   (defmacro letp (name vars &rest body)
     (or (listp vars) (error "malformed letp (`vars` is not list)"))
     `(labels ((,name ,(mapcar 'car vars)
                 (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))
                 .,body))
        (,name .,(mapcar #'cadr vars))))
   
   (defmacro let1 (var val &body body)
     `(let ((,var ,val))
        .,body))
   
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

   ;;macros
   (defmacro inc! (x &optional (val 1))
     `(incf ,x ,val))

   (defmacro dec! (x &optional (val 1))
     `(decf ,x ,val))
   
   (defmacro push! (x &optional (val 1))
     `(push ,val ,x))

   ;; port
   (defun eof-object? (o)
     (eq o +eof-object+))

   (defun port->list (reader iport)
     (let (ret)
       (declare (type list ret))
       (handler-bind ((error (lambda (e) (return-from port->list (values (nreverse ret) nil)))))
         (do ((sexp (funcall reader iport nil +eof-object+) (funcall reader iport nil +eof-object+)))
             ((eof-object? sexp)
              (values (nreverse ret) t))
           (declare (type (or string keyword) sexp))
           (push sexp ret)))))

   (defun file->list (reader path)
     (declare (type string path))
     (with-input-file (iport path)
       (port->list reader iport)))

   ;; gauche do not return multiple values.
   (defun file->sexp-list (path)
     (file->list #'read path))
   
   (defun file->string-list (path)
     (file->list #'read-line path))

   (defun file->string (path)
     ;; TODO: Add carriage return for Windows.
     (string-join (file->list #'read-line path) (string #\Newline)))
   
   (defun string->file (str file)
     (with-output-file (out file)
       (write-string str out)))

   (defun string-list->file (strs file)
     (with-output-file (out file)
       (dolist (str strs)
         (write-string str out)
         (write-char #\Newline out))))
   
   (defun sexp->file (sexp file)
     (with-output-file (out file)
       (write sexp :stream out)))

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
     (>= (length* lis) 0))

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
   ;; (make-string 4)
   ;; (string #\Newline)
   (defun string-ref  (str n)
     (aref str n))
   
   (defun string-copy (str)
     (copy-seq str))
   ;;(defun string-contains (str c/s/p)
   ;;  (find c/s/p str))
   ;;   string-fill!
   (defun string-null? (str)
     (and (string? str) (string= "" str)))
   
   (defun string->list (str)
     (coerce str 'list))
   
   (defun list->string (lis)
     (coerce  lis 'string))
   
   (defun string-join (seq &optional (delim ",") (grammer 'infix))
     (format nil (string-append "~{~a~^" delim "~}") seq))
   ;; (string-join '()  ","  'infix)
   ;;(string-join '() "," 'strict-infix) => error
   ;;(string-join  '("usr" "bin" "sbcl") "/" 'prefix)
   ;;     suffix
   ;;(string-join '("test" "test" ))
   
   (defun string-append (&rest args)
     (apply #'concatenate 'string args))

   (defun string-concatenate (lis)
     (apply #'concatenate 'string lis))

   ;; Separator can only be a character.
   (defun string-split (seq c) ;; c/s/p
     (split-sequence:split-sequence c seq))
   
   (defun string-trim-both (s &optional (char-bag +white-space+) start end)
     (cond ((and start end)
            (string-trim char-bag (subseq s start end)))
           (start (string-trim char-bag (subseq s start)))
           (end (string-trim char-bag (subseq s 0 end)))
           (t (string-trim char-bag s))))
   
   ;;  (required :usocket)
   ;;
   ;;  (usocket:with-client-socket (socket stream "www.example.com" 80)
   ;;    (format stream "GET / HTTP/1.0~c~c~2:*~c~c" #\Cr #\Lf)
   ;;    (force-output stream)
   ;;    (loop for line = (read-line stream nil nil)
   ;;       while line
   ;;       do (write-line line)))
   
   ;; srfi-26
   (defmacro cut (func &rest forms)
     (let* ((<...> (intern "<...>"))
            (<> (intern "<>"))
            (forms (if (member <...> forms)
                       (if (eq <...> (car (last forms)))
                           `(apply (function ,func) ,@forms)
                           (error "malformed cut"))
                       (cons func forms)))
            args body)
       (dolist (elem forms `(lambda ,(nreverse args) ,(nreverse body)))
         (cond ((eq elem <>)
                (let ((var (gensym "cut:")))
                  (push var args)
                  (push var body)))
               ((eq elem <...>)
                (let ((var (gensym "cut:")))
                  (push '&rest args)
                  (push var args)
                  (push var body)))
               (t (push elem body))))))
   ;;(funcall (cut + <...>) 1 2  3 4)
   ;;(funcall (cut + <> <>) 1 2 )
   
   (defmacro cute (func &body forms)
     (let* ((<...> (intern "<...>"))
            (<> (intern "<>"))
            (form (if (member <...> forms)
                      (if (eq <...> (car (last forms)))
                          (list* 'apply `(function ,func) forms)
                          (error "mulformed cute "))
                      (cons func forms)))
            (args)
            (body)
            (binds))
       (dolist (e form `(let ,binds (lambda ,(nreverse args) ,(nreverse body))))
         (cond ((eq e <>)
                (let ((var (gensym "cute")))
                  (push var args)
                  (push var body)))
               ((eq  e <...>)
                (let ((var (gensym "cute")))
                  (push '&rest args)
                  (push var args)
                  (push var body)))
               (t
                (if (symbolp e)
                    (push e body)
                    (let ((var (gensym "cute")))
                      (push `(,var ,e) binds)
                      (push var body))))))))

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

   ;; util.list
   (defun assoc-ref (alist k &optional default (test #'equal))
     (aif (assoc k alist :test test)
         (cdr it)
       default))
   
   (defun (setf assoc-ref) (v alist k &optional (test #'equal))
     (aif (assoc k alist :test test)
         (progn (setf (cdr it) v) alist)
       (acons k v alist)))
   
   
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
     ;; Considering nullstr? erase sbcl's warnnings.
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

   (defun (setf hash-table-get) (new-value ht k)
     (setf (gethash k ht) new-value))

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
         ((ret '() (cons-if-not (member e ret) ret e)))))
   
   (defmacro const (x)
     `(lambda (&rest args) (declare (ignorable args)) ,x))

   ;; generic == 
   (defun-alias-scheme == equal)
   (defun /== (a b)
     (not (equal a b)))

   (defmacro function-to-variable-table (&rest args)
     (unless args
       (setq args
             '(display symbol? string? number? keyword? pair? null?
               x->string string->symbol symbol->string string->number
               eq? eqv? equal?
               ;; port
               eof-object? file->list
               file->string file->string-list file->sexp-list
               string->file string-list->file sexp->file
               
               ;; srfi-1
               proper-list? dotted-list? circular-list?
               iota xcons
               cons*  circular-list
               take take*
               drop reverse! filter list-ref
               take-while
               
               ;; 13
               string-ref string-copy
               string-null? string->list list->string
               string-append string-trim-both
               string-concatenate
               string-join  string-split
               assoc-ref
               slot-ref
               
               rxmatch
               hash-table-get hash-table->alist hash-table-keys hash-table-values
               
               nub == /==
               )))
     `(progn ,@(mapcar (^(e) `(function->variable ,e))  args)))
   
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

