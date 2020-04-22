(defun demo-one (list)
  "It has a documentation string ... I think?"
  (princ list))

(defun demo-two (list)
  "This I believe will also have a doc string"
  (princ list))

(defun print-doc (func)
  (print (documentation func 'function)))

(defparameter *funcs* '(demo-one demo-two))

(defun print-all-docs (list)
  (princ (mapcar #'print-doc list))
)

(print-all-docs *funcs*)