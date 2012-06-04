(in-package :named-values)

(defvar *named-values-types* (make-hash-table))
(defvar *named-values-documentation* (make-hash-table))
(defvar *with-named-values* nil)

(defun infer-names (args)
  (loop for name in args by #'cddr
        collect (if (keywordp name)
                    (intern (symbol-name name))
                    (error "Name not a keyword: ~A" name))))

(defmethod documentation ((x symbol) (doc-type (eql 'named-values)))
  (gethash x *named-values-documentation*))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'named-values)))
  (setf (gethash x *named-values-documentation*) new-value))

(defmacro define-named-values (type args &rest options)
  "Define `TYPE` as a named-value type, taking `ARGS`.  For `OPTIONS`,
`:documentation` is currently useful, and available via `DOCUMENTATION`
when called as per the following:

```lisp
 (documentation <named-value-type> 'named-values:named-values)
```

Other `OPTIONS` are ignored."
  (setf (gethash type *named-values-types*) args)
  (setf (documentation type 'named-values)
        (cadr (find :documentation options :key #'car)))
  (values))

(defmacro named-values (type &body args)
  "=> values

Return values with the protocol `TYPE`.  This should probably be the
name of the enclosing function by default, but may be another symbol
as desired.  This generates a warning if a different set of names is
used with the same `TYPE`.  This may become an error in the future.

```lisp
 (named-values TYPE [(ARGS)] [:NAME0 VALUE0 [...]])
```

The values specified will be returned as per `VALUES`, but an extra note
is made if called within a `NAMED-VALUES` form."
  (let ((existing-names (gethash type *named-values-types*))
        (names (if (listp (car args))
                   (pop args)
                   (infer-names args))))
    (unless (or (null existing-names) (equalp names existing-names))
      (warn (make-condition
             'simple-warning
             :format-control "Existing NAMED-VALUES type ~A redefined with differing arguments:~%  New: ~A~%  Old: ~A"
             :format-arguments (list type names existing-names))))
    (setf (gethash type *named-values-types*) names)
    `(progn
       (when *with-named-values*
         (setf *with-named-values* ',type))
       (funcall (lambda (&key ,@names)
                  (values ,@names))
                ,@args))))

(defun make-named-bindings (type names)
  (let ((all-names (gethash type *named-values-types*)))
    (unless all-names
      (error "Unknown NAMED-VALUES type: ~A" type))
    (loop for name in all-names
          as match-name = (if (typep (car names) 'list) (caar names) (car names))
          as real-name = (if (typep (car names) 'list) (cadar names) (car names))
          collect (if (string= (symbol-name name)
                               (symbol-name match-name))
                      (progn (pop names) real-name)
                      (gensym)))))

(defun make-named-value-lambda (type args body)
  (let* ((binds (make-named-bindings type args))
         (rest (gensym)))
    `(lambda ,(concatenate 'list binds
                `(&rest ,rest))
       (declare (ignorable ,@binds ,rest))
       (unless (eq *with-named-values* ',type)
         (error "NAMED-VALUE type ~A expected, got ~A"
                ',type *with-named-values*))
       ,@body)))

(defmacro nbind (&rest args)
  "`NBIND` takes an optional type `TYPE`, a list of value names `NAME*`,
a named-value form `FORM`, and a body `BODY`, as per the following:

```lisp
 (nbind [TYPE] (NAME*)
    FORM
  BODY*)
```

Each name `NAME` is a symbol corresponding to specified named values,
or a list in the form `(NAME ALIAS)`, where `NAME` is the given name,
and `ALIAS` is the name to be used in `BODY`.  Symbols are compared by
`SYMBOL-NAME` at compile time, and do not have to originate from the
same package.

If `TYPE` is not specified, it is assumed to be the first symbol in
the list `FORM`.  Thus, for simple cases, it is convenient to define
the type of a named value clause to match its function name.

It is an error if `TYPE` does not match the actual type, or if any
`NAME` is not specified by the original `TYPE`."
  (let* ((type (if (symbolp (car args))
                   (pop args)
                   (caadr args)))
         (lambda-args (pop args))
         (value-form (pop args)))
    `(let ((*with-named-values* t))
       (multiple-value-call
           ,(make-named-value-lambda type lambda-args args)
         ,value-form))))

(defmacro values-map-names (type value-form &optional names)
  "=> values

Take values from `VALUE-FORM` and map them to type `TYPE` with
names `NAMES`.  Primarily for remapping passed-through values.

`NAMES` may be unspecified if `TYPE` is previously-defined."
  (let* ((names (or names (gethash type *named-values-types*)))
         (vars (mapcar (lambda (x) (declare (ignore x)) (gensym)) names)))
    `(multiple-value-bind ,vars ,value-form
       (named-values ,type ,names
         ,@(loop for var in vars
                 for name in names
                 collect (intern (symbol-name name) :keyword)
                 collect var)))))

(defmacro ncase (named-value-form &body clauses)
  "This works like `CASE`, but for the `TYPE` of the values returned
by `NAMED-VALUE-FORM`:

```lisp
 (ncase NAMED-VALUE-FORM
  (TYPE (NAME*) FORM*)*)
```

If a type `TYPE` is found that matches the actual named values type,
`FORM*` are evaluated.  Otherwise, it is an error.

It is possible to specify `T` or `OTHERWISE`.  In this case, `NAME*`
must be a lambda list which takes `TYPE` followed by an arbitrary
number of parameters (bound to returned values), probably via
`&rest`."
  (let ((args (gensym)))
    `(let ((*with-named-values* t))
       (multiple-value-call
           (lambda (&rest ,args)
             (case *with-named-values*
               ,@(loop for clause in clauses
                       collect
                       (if (or (eq t (car clause))
                               (eq 'otherwise (car clause)))
                           `(,(car clause)
                             (apply (lambda ,(cadr clause)
                                      ,@(cddr clause))
                                    *with-named-values* ,args))
                           `(,(car clause)
                             (apply ,(make-named-value-lambda
                                      (car clause) (cadr clause)
                                      (cddr clause))
                                    ,args))))))
         ,named-value-form))))
