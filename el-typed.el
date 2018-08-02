;; Provides useful typing information in how we write Elisp, to make it more readable.

(defmacro defn (types name args &rest rest)
  "Type safe defun."
  (let ((types (cl-remove-if
                (lambda (x) (or (equal '-> x) (equal '→ x))) types)))
    `(progn (defun ,name ,args
              ,@(cl-loop
                 for arg in args
                 for type in types
                 collect `(cl-check-type ,arg ,type))
              ,@rest)
            ;; This is silently ignored by Emacs Lisp, blargh.
            ;; This means we can't really have compile time return value checking.
            ;; As such, we may as well comment it out for now.
            ;; (cl-declaim (ftype (cl-function ,(butlast types) ,@(last types)) ,name))
            )))

(defn (number → number) echo-number (x) (1+ x))

(echo-number "b")

(defun bad-call () (echo-number "x"))

;; Could we make useful EIEIO class macros?
(defclass el-typed-class ()
  ((foo
    :initarg :foo
    :initform 3
    :type number)
   (bar
    :initarg :bar
    :initform 4
    :type number)))

(defclass el-typed-sclass ()
  ((foo
    :initarg :foo
    :initform "3"
    :type string)
   (bar
    :initarg :bar
    :initform "4"
    :type string)))

(defclass el-typed-composite ()
  ((n :initarg :n :type el-typed-class)
   (s :initarg :s :type el-typed-sclass)))

(cl-deftype even-number () '(satisfies evenp))
(cl-deftype odd-number () '(satisfies oddp))
(cl-deftype number-list () '(satisfies (lambda (n) (and (listp n) (every #'integerp n)))))
(typep 2 'even-number)                  ; t
(typep 2 'odd-number)                   ; nil

(defun ++ (a b)
  (cl-typecase a
    (string (format "%s%s" a b))
    (integer (+ a b))
    (t (error "Woops"))))

(defun n-dispatch (n)
  (cl-typecase n
    (number-list (message "Nice list of numbers"))
    (even-number (message "Even number"))
    (odd-number (message "Odd number"))
    (t (message "Probably not a number..."))))

(defclass even-number () ((v :type even-number :initarg :-)))

(defmethod abc1 ((it even-number))
  (message "Even"))

(even-number :- 2)

(defclass blub ()
  ((v :initarg :v :initform 1 :type odd-number)))

(defmethod el-typed-add ((it el-typed-class) &optional x)
  (+ (oref it foo) (oref it bar) x))

(defvar el-typed-stub (el-typed-class :foo 1 :bar 2))
(el-typed-add el-typed-stub 3)          ; 6

;; What if we
;; How about if we could do the defmethod with auto-bound slots, ala anaphoric.
;; Such a thing should be fine, if we treated these instances as immutable basically.
(defm el-typed-class et-add (+ foo bar))
(defm el-typed-sclass et-add (format "%s%s" foo bar))

(provide 'el-typed)
