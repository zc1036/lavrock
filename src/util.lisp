
;;;; utilities

(in-package :lavrock-util)

(defmacro elet (bindings &body body)
	"Extended LET; provides the ability to quickly bind multiple
    'single or multiple values'. It is used exactly like LET except
    that bindings can consist of N objects rather than just two. In
    each binding, the first N - 1 places will be bound to the
    evaluated Nth form. The order amongst the bindings is undefined."
	(let ((multiple-value-bindings) (last-multiple-value-bind) (new-let-bindings))
		(dolist (binding bindings)
			(if (> (length binding) 2)
				(let ((new-bind (list 'multiple-value-bind (subseq binding 0 (- (length binding) 1)) (car (last binding)))))
					(if last-multiple-value-bind
						(setf (cdr (last last-multiple-value-bind)) (list new-bind))
						(setf multiple-value-bindings new-bind))
					(setf last-multiple-value-bind new-bind))
				(setf new-let-bindings (cons binding new-let-bindings))))

		(if last-multiple-value-bind
			(setf (cdr (last last-multiple-value-bind)) (list `(let ,new-let-bindings ,@body)))
			(setf multiple-value-bindings `(let ,new-let-bindings ,@body)))

		multiple-value-bindings))

;;(define-symbol-macro λ lambda)

(defmacro λ (args &body body)
	`(lambda ,args ,@body))

(defun recursive-length% (list len)
	(check-type list list)

	(typecase list
	  (cons
	   (recursive-length% (cdr list)
						  (+ (if (typep (car list) 'list)
								 (recursive-length% (car list) 0)
								 1)
							 len)))
	  (t len)))

(defun recursive-length (list)
	"Returns the length of the list and all sublists. Ignores non-list
    objects in the cdr of conses."
	(recursive-length% list 0))

(defun unzip-lists (lists)
	(loop
	   for list in lists
	   when (not list)
	   do (return (list nil nil))
	   collecting (car list) into cars
	   collecting (cdr list) into cdrs
	   finally (return (list cars cdrs))))

(defun multiple-value-mapcar* (func &rest lists)
	"Map FUNC across LISTS, accumulating the values returned by each invocation
     of FUNC into a single list."
	(loop
	   for (args rest-lists) = (unzip-lists lists) then (unzip-lists rest-lists)
	   with newlist-head and newlist-tail
	   while args
	   for newitem = (multiple-value-list (apply func args))
	   when newitem
	   do
		 (if newlist-tail
			 (setf (cdr newlist-tail) newitem)
			 (setf newlist-head newitem))
		 (setf newlist-tail (last newitem))
	   finally (return newlist-head)))
