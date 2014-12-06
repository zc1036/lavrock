
;;;; Code generator for AVR assembly

(in-package :codegen)

;; first 70 code bytes are reserved for the interrupt vector (program memory is word-addressable)
(defparameter interrupt-vector-start 0)
(defparameter interrupt-vector-end 70)

(defun org (address)
	(format t ".ORG 0x~X~%" address))

(defun in-code-segment ()
	(format t "~%.CSEG~%"))

(defun include-asm (filename)
	(format t "~%.INCLUDE \"~a\"~%" filename))

(defun header-comment (comment)
	(format t ";; ~a~%" comment))

(defun line-comment (comment)
	(format t " ; ~a~%" comment))

(defun ! (&optional (comment ""))
	(line-comment comment))

(defun !! (&rest lines)
	(dolist (line lines)
		(funcall lavrock::*instr-printer* (format nil ";; ~a" line)))

	(when (null lines)
		(funcall lavrock::*instr-printer* ";;")))

(defun !!! (&rest lines)
	(dolist (line lines)
		(format t " ;;; ~a~%" line))

	(when (null lines)
		(format t " ;;;~%")))

(defmacro genloop (var keyword value &body body)
	`(loop for ,var ,keyword ,value do
		  ,@body))

(defun defconstant-string-imp (value)
	(format t ".DB ")
	(genloop c across value
		(check-type (char-int c) (or lavrock::asm-label (integer -128 255)))
		(format t "0x~X, " (char-int c)))

	(when (eql (mod (length value) 2) 1)
		(format t "0x0~%")))

(defun defconstant-list-imp (name-len value)
	(let ((i 0))
		(genloop c in value
			(check-type c (or lavrock::asm-expr (integer -128 255)))

			(when (eql (mod i 2) 0)
				(format t ".DB "))

			(if (typep c 'integer)
				(format t "0x~X, " c)
				(format t "~a, " (lavrock::asm-expr-repr c)))

			(when (eql (mod i 2) 1)
				(format t "~%~v@{~A~:*~}" (+ name-len 2) #\space))

			(incf i))

		(when (eql (mod i 2) 1)
			(format t "0x0~%"))))

(defmacro defconstant-bytes (name value)
	"Defines an array of constant bytes in program memory and assigns them the given label."
	(let ((value-sym (gensym)) (sanitized-name (lavrock::sanitize-symbol-name name)))
		`(let ((,value-sym ,value))
			 (format t "~%~a: " ,sanitized-name)
			 (if (typep ,value-sym 'string)
				 (defconstant-string-imp ,value-sym)
				 (defconstant-list-imp ,(length sanitized-name) ,value-sym))
			 (princ #\newline)
			 (defparameter ,name (lavrock::make-asm-label :name ,sanitized-name)))))

(defmacro defconstant-asm (name value)
	(let ((val-name (gensym)) (const-value (gensym)))
		`(let* ((,val-name ,value)
				(,const-value (if (typep ,val-name 'lavrock::asm-constant)
								  (lavrock::asm-constant-value ,val-name)
								  ,val-name)))
			 (check-type ,val-name (or lavrock::asm-constant (integer -32768 65535)))
			 (defparameter ,name (lavrock::make-asm-constant :name ',name
															 :value ,const-value))

			 (when (eq (lavrock::vm-mode lavrock::*current-vm*) :output)
				 (format t ".EQU ~a = ~a~%" (lavrock::sanitize-symbol-name ',name) ,const-value)))))

(defmacro defexpr-asm (name val)
	`(let ((value ,val))
		 (defparameter ,name (if (typep value 'lavrock::asm-expr)
								 value
								 (lavrock::make-asm-expr :repr value)))
		 (when (eq (lavrock::vm-mode lavrock::*current-vm*) :output)
			 (format t ".EQU ~a = ~a~%" (lavrock::sanitize-symbol-name ',name) (lavrock::asm-expr-repr-or-value value)))))

(defun export-asm-func (func)
	(lavrock::vm-push-func-called lavrock::*current-vm* func))

(defun dollar-reader (stream char)
	(declare (ignore char))

	(list 'lavrock::asm-constant-value (read stream t nil t)))

;; To "dereference" asm constants.
(set-macro-character #\$ #'dollar-reader)

(defmacro defregister-synonym (name register)
	`(let ((new-reg-name ,(lavrock::sanitize-symbol-name name)))
		 (when (eq (lavrock::vm-mode lavrock::*current-vm*) :output)
			 (format t ".DEF ~a=~a~%" new-reg-name (register-name ,register)))

		 (defparameter ,name (lavrock::make-register :name new-reg-name :address (lavrock::register-address ,register)))))

(defparameter X-lo r26)
(defparameter X-hi r27)
(defparameter Y-lo r28)
(defparameter Y-hi r29)
(defparameter Z-lo r30)
(defparameter Z-hi r31)

(defun print-interrupt-table (vm)
	(in-code-segment)
	(org interrupt-vector-start)

	(header-comment "Interrupt table")

	;; We rebind *instr-printer* so that we won't get newlines after instructions
	(let ((lavrock::*instr-printer* (lambda (val) (princ val))))
		(block nil ;; so reti doesn't complain
			(loop
			   for int being the elements of (lavrock::vm-interrupt-vector vm)
			   for intdesc in lavrock::interrupts do
				 (if (null int)
					 (instructions:reti)
					 (instructions:jmp int)) ;; use jmp and not rjmp because jmp is 4 bytes and rjmp is 2

				 (line-comment (cdr intdesc))

				 (when (null int)
					 (instructions:nop) ;; add another 2-byte instr so we don't misalign the IVT, for which each vector is two entries
					 (princ #\newline)))))

	(princ #\newline))

(defun invoke-called-asm-funcs (vm &optional (seen-funcs (make-hash-table)))
	(let ((vm-called-funcs (lavrock::vm-funcs-called vm)))
		(when (not (null vm-called-funcs))
			;;; set the called-funcs list to nil and invoke all the functions that have been called.
			;;; the called functions will add their immediate children to the called-funcs list,
			;;; and then we invoke ourself to repeat until the called-funcs list stays nil.

			(setf (lavrock::vm-funcs-called vm) nil)

			;; func is a pair (name . function)
			(dolist (func vm-called-funcs)
				(when (not (gethash func seen-funcs nil))
					(setf (gethash func seen-funcs) func)
					(format t "~%~a:~%" (lavrock::asm-func-name func))
					(lavrock::asm-func-call func t)))

			(invoke-called-asm-funcs vm seen-funcs))))

(defmacro in-file (filename vm is-main-file &body body)
	(let ((vm-sym (gensym)))
		`(let ((,vm-sym ,vm))
			 (with-vm ,vm-sym
				 (format t "~%generating file ~a~%" ,filename)

				 (when ,is-main-file
					 (format t "~%.include \"m128def.inc\"~%~%"))

				 (in-code-segment)
				 (org interrupt-vector-end)

				 ,@body

				 (invoke-called-asm-funcs ,vm-sym)
				 
				 (when ,is-main-file
					 (print-interrupt-table ,vm-sym))
				 
				 (values)))))
