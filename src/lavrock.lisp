
;;;; lavrock

(in-package :lavrock)

(defconstant sram-size 4096 "The size of SRAM in bytes")

(defparameter interrupts '((reset . "Reset")
						   (int0 . "External Interrupt 0")
						   (int1 . "External Interrupt 1")
						   (int2 . "External Interrupt 2")
						   (int3 . "External Interrupt 3")
						   (int4 . "External Interrupt 4")
						   (int5 . "External Interrupt 5")
						   (int6 . "External Interrupt 6")
						   (int7 . "External Interrupt 7")
						   (timer2-comp . "Timer/Counter2 Compare Match")
						   (timer2-ovf . "Timer/Counter2 Overflow")
						   (timer1-capt . "Timer/Counter1 Capture Event")
						   (timer1-compa . "Timer/Counter1 Compare Match A")
						   (timer1-compb . "Timer/Counter1 Compare Match B")
						   (timer1-ovf . "Timer/Counter1 Overflow")
						   (timer0-comp . "Timer/Counter0 Compare Match")
						   (timer0-ovf . "Timer/Counter0 Overflow")
						   (spi-stc . "Serial Transfer Complete")
						   (usart0-rx . "USART 0 Rx Complete")
						   (usart0-udre . "USART 0 Data Register Empty")
						   (usart0-txc . "USART 0 Tx Complete")
						   (adc . "ADC Conversion Complete")
						   (ee-ready . "EEPROM Ready")
						   (analog-comp . "Analog Comparator")
						   (timer1-compc . "Timer/Counter1 Compare Match C")
						   (timer3-capt . "Timer/Counter3 Capture Event")
						   (timer3-compa . "Timer/Counter3 Compare Match A")
						   (timer3-compb . "Timer/Counter3 Compare Match B")
						   (timer3-compc . "Timer/Counter3 Compare Match C")
						   (timer3-ovf . "Timer/Counter3 Overflow")
						   (usart1-rx . "USART 1 Rx Complete")
						   (usart1-udre . "USART 1 Data Register Empty")
						   (usart1-txc . "USART 1 Tx Complete")
						   (twi . "Two-wire Serial Interface")
						   (smp-ready . "Store Program Memory Ready")))

(loop
   for interrupt in interrupts
   for num from 0 do
	 (eval `(defparameter ,(car interrupt) ',(cons num interrupt))))

(defstruct vm
  (sram (make-array sram-size))
  (mode :emulate) ;; mode can be :emulate for emulation or :output for printing the assembly to a file
  funcs-called ;; the set of symbols whose functions have been called in this VM
  interrupts-enabled
  (interrupt-vector (make-array (length interrupts) :initial-element nil)))

(defstruct asm-expr repr)

(defun asm-expr-repr-or-value (asm-expr-or-value)
	(if (typep asm-expr-or-value 'lavrock::asm-expr)
		(lavrock::asm-expr-repr asm-expr-or-value)
		asm-expr-or-value))

(defstruct (asm-constant (:include asm-expr) (:constructor create-asm-constant))
  value
  name)

(defun make-asm-constant (&key value name)
	(create-asm-constant :value value :name name :repr (sanitize-symbol-name name)))

(defun vm-push-func-called (vm func)
	(setf (vm-funcs-called vm)
		  (adjoin func (vm-funcs-called vm))))

(defparameter *current-vm* nil)

(defparameter *label-indent* 0)
(defparameter *instr-indent* 4)

(defparameter *instr-printer*
  ;; Print *instr-indent* spaces followed by the argument.
  (lambda (val) (format t "~v@{~A~:*~}~*~a~%" *instr-indent* #\space val)))

(defun processor-tick ())

(defun write-sram-byte (address value)
	"Writes a byte to an address in SRAM in the current VM. VALUE must
    be an integer between -128 and 255, and ADDRESS must be in the
    range [0, sram-size)."

	(check-type value (integer -128 255))
	(assert (< -1 address sram-size))

	(setf (aref (vm-sram *current-vm*) address) value)
	(values))

(defun read-sram-byte (address)
	"Reads a byte from SRAM in the current VM. [address] must be in
    the range [0, sram-size)"

	(assert (< -1 address sram-size))
	(aref (vm-sram *current-vm*) address))

(defstruct register name address io-address (size 1))

(defun register-value (reg)
	"Takes a register and returns its value in the current VM."

	(read-sram-byte (register-address reg)))

(defun register-is-io (reg)
	(not (null (register-io-address reg))))

(defun (setf register-value) (value reg)
	(check-type value (integer -128 255))
	(write-sram-byte (register-address reg) value))

(defun defregister (name address &optional io-address)
	"Defines a register in the register file and returns the register
    object; internal use only."

	(eval `(defparameter ,name (make-register :name ',name :address ,address :io-address ,io-address))))

(defun sanitize-label-name (name)
	(substitute #\_ #\- name))

(defun sanitize-symbol-name (name)
	(sanitize-label-name (symbol-name name)))

;; Create the 32 general-purpose registers named R0 to R31
(loop for i from 0 to 31 do
	 (defregister (intern (concatenate 'string "R" (write-to-string i))) i))

(defstruct register16 name address lo hi)

(defparameter X (make-register16 :name 'X :address #x1A :lo r26 :hi r27))
(defparameter Y (make-register16 :name 'Y :address #x1C :lo r28 :hi r29))
(defparameter Z (make-register16 :name 'Z :address #x1E :lo r30 :hi r31))

(defparameter X+ (make-register16 :name 'X+ :address #x1A :lo r26 :hi r27))
(defparameter Y+ (make-register16 :name 'Y+ :address #x1C :lo r28 :hi r29))
(defparameter Z+ (make-register16 :name 'Z+ :address #x1E :lo r30 :hi r31))

(defparameter -X (make-register16 :name '-X :address #x1A :lo r26 :hi r27))
(defparameter -Y (make-register16 :name '-Y :address #x1C :lo r28 :hi r29))
(defparameter -Z (make-register16 :name '-Z :address #x1E :lo r30 :hi r31))

;; Create the SP* registers
(defregister 'SPL #x5E #x3D)
(defregister 'SPH #x5D #x3E)

;; Status register
(defregister 'SREG #x5F #x3F)

;; Defined in m128def.inc

(dolist (reg '(SREG-C SREG-Z SREG-N SREG-V SREG-S SREG-H SREG-T SREG-I))
	(eval `(defparameter ,reg (quote ,(intern (sanitize-symbol-name reg))))))

(defparameter zero nil) ;; left undefined for user programs to fill in

;; MCU Control Register
(defregister 'MCUCR #x55 #x35)

;; PORT registers
(defregister 'DDRA #x3A #x1A) ;; Port A Data Direction Register
(defregister 'PORTA #x3B #x1B) ;; Port A Data register

;; USART Registers
(defregister 'UDR0 #x2C #x0C) ;; USART0 Data Register
(defregister 'UCSR0A #x2B #x0B) ;; USART0 Control and Status Register A
(defregister 'UCSR0B #x2A #x0A) ;; USART0 Control and Status Register B
(defregister 'UCSR0C #x95) ;; USART0 Control and Status Register C (memory)
(defregister 'UBRR0H #x90) ;; USART Baud Rate Register High (memory)
(defregister 'UBRR0L #x29 #x09) ;; USART Baud Rate Register Low

(defmacro def-checked-asm-instr (name args precond-test text-output &body body)
	"Defines a new assembly instruction; TEXT-OUTPUT should be a form
    that evaluates to the textual representation of the assembly
    instruction. PRECOND-TEST is a form evaluated before the body in
    both emulate and output mode."

	;; We have to generate a macro rather than a function so that we can use (return) inside instructions.
	`(defmacro ,name ,args
		 `(let ,(mapcar #'list ',args (list ,@args))
			  ,',precond-test
			  (if (or (null *current-vm*) (eq (vm-mode *current-vm*) :output))
				  (funcall *instr-printer* ,',text-output)
				  (progn
					  ,(cons 'progn ',body)
					  (processor-tick))))))

(defmacro def-asm-instr (name args text-output &body body)
	"A shortcut for (def-checked-asm-instr name args nil text-output body)"
	`(def-checked-asm-instr ,name ,args nil ,text-output ,@body))

(defmacro with-vm (vm &body body)
	"Execute BODY in the given VM."
	`(let ((*current-vm* ,vm))
		 (codegen:defconstant-asm ramend #x10FF)
		 (codegen:defconstant-asm rambegin #x100)

		 ,@body))

(defstruct (asm-label (:include asm-expr) (:constructor create-asm-label))
  name)

(defun make-asm-label (&key name)
	(create-asm-label :name name :repr name))

(defun lshifti (val amt)
	(make-asm-expr :repr (format nil "(~a)<<(~a)" (asm-expr-repr-or-value val) (asm-expr-repr-or-value amt))))

(defun bitor (val amt)
	(make-asm-expr :repr (format nil "(~a)|(~a)" (asm-expr-repr-or-value val) (asm-expr-repr-or-value amt))))

(defun lobyte (val)
	(typecase val
	  (asm-expr (make-asm-expr :repr (format nil "low(~a)" (asm-expr-repr val))))
	  (asm-constant (make-asm-expr :repr (format nil "low(~a)" (sanitize-symbol-name (asm-constant-name val)))))
	  (t (logand val #xFF))))

(defun hibyte (val)
	(typecase val
	  (asm-expr (make-asm-expr :repr (format nil "high(~a)" (asm-expr-repr val))))
	  (asm-constant (make-asm-expr :repr (format nil "high(~a)" (sanitize-symbol-name (asm-constant-name val)))))
	  (t (ash (logand val #xFF00) -8))))

(defun label (label)
	(check-type label asm-label)

	(format t "~a:~%" (asm-label-name label)))

(defparameter label-counter 0)

(defmacro with-labels (labels &body body)
	`(let ,(mapcar (lambda (x)
					   (list x
							 `(lavrock::make-asm-label :name (sanitize-label-name (concatenate 'string
																							   ,(symbol-name x)
																							   "_"
																							   (write-to-string (incf label-counter)))))))
				   labels)
		 ,@body))

(defstruct (asm-func (:include asm-label))
  thunk
  local-registers
  argument-registers
  all-registers)

(defun asm-func-call (func &rest rest)
	(apply (asm-func-thunk func) rest))

(defun gen-register-address (r)
	(if (typep r 'register)
		(register-address r)
		(register16-address r)))

(defun register-equals (r1 r2)
	(eql (gen-register-address r1) (gen-register-address r2)))

(defparameter callee-preserved-registers
  ;; Generate the registers R16 to R31, not including r24/r25.
  (append
   (loop for i from 26 to 31 collect (eval (intern (concatenate 'string "R" (write-to-string i)))))
   (loop for i from 16 to 23 collect (eval (intern (concatenate 'string "R" (write-to-string i)))))))

(defun filter-caller-preserved-registers (list)
	nil)

(defun filter-callee-preserved-registers (list)
	(intersection list callee-preserved-registers :test #'register-equals))

(defparameter asm-func-preamble-hook (λ ()))

(defparameter asm-func-epilogue-hook (λ ()))

(defparameter preserved-register-byte-count nil)

(defparameter this-func nil)

(defmacro defun-asm (name arguments local-registers &body body)
	"Sets NAME to an ASM-FUNC where the lambda will evaluate BODY if
	given a non-nil argument or the current VM is in emulate-mode."
	`(let ((preamble-hook asm-func-preamble-hook) (epilogue-hook asm-func-epilogue-hook)) ;; we need to save the hooks
		 (defparameter ,name
		   (make-asm-func
			:name ',(sanitize-symbol-name name)
			:repr ',(sanitize-symbol-name name)
			:thunk (lambda (&optional print)
					   (declare (special ,name))

					   ;; Actually evaluate the function body if we are in emulate mode
					   ;; (to execute the instructions) or if we are in print mode AND the VM
					   ;; tells us to print the body of the function. (We don't want to spill
					   ;; the body if we're in print mode, we just emit a CALL instruction,
					   ;; which is done in the definition of the CALL instruction.)
					   (if (or print (eq (vm-mode *current-vm*) :emulate))
						   ;; body is encased in a block named nil so (return) works inside
						   ;; instructions like ret and reti
						   (block nil
							   (let* ((this-func ,name)
									  (registers-to-preserve (filter-callee-preserved-registers (asm-func-all-registers this-func)))
									  (reversed-registers-to-preserve (reverse registers-to-preserve))
									  (preserved-register-byte-count (+ (count-if #'register16-p registers-to-preserve)
																		(length registers-to-preserve))))

								   ;; let customizers insert code before the register preservation
								   (funcall preamble-hook)

								   (instructions:push-registers registers-to-preserve)

								   ;; these two flets can't be combined, because ret-bypass-register-restoration
								   ;; refers to the original instructions:ret, and both the inner ones refer to the outer one,
								   ;; so neither a single FLET nor a LABELS would work.
								   (flet ((restore-registers ()
											  (instructions:pop-registers reversed-registers-to-preserve)))
									   (flet ((instructions:ret ()
												  (restore-registers)
												  ;; epilogue hook goes between restoring registers and returning from function
												  (funcall epilogue-hook)
												  (instructions:ret))
											  (ret-bypass-register-restoration ()
												  (funcall epilogue-hook)
												  (instructions:ret))
											  (ret-bypass-register-restoration-and-epilogue ()
												  (instructions:ret)))
										   
										   ,@body
										   (instructions:ret)))))
						   (vm-push-func-called *current-vm* ,name)))
			:argument-registers (list ,@arguments)
			:local-registers (list ,@local-registers)
			:all-registers (list ,@local-registers ,@arguments)))))

;; int is a list (int-number int-name . int-description)
(defmacro definterrupt (int name &body body)
	"Sets the given interrupt in the current VM to an ASM-FUNC where
	the lambda will evaluate BODY if given a non-nil argument or
	the current VM is in emulate-mode."
	`(let* ((int-func (lambda (&optional print)
						  (when (or print (eq (vm-mode *current-vm*) :emulate))
							  (block nil ,@body (instructions:reti)))))
			(int-name-func (make-asm-func :name (sanitize-symbol-name ',name)
										  :thunk int-func)))
		 (setf (aref (vm-interrupt-vector *current-vm*) (car ,int)) int-name-func)
		 (vm-push-func-called *current-vm* int-name-func)))
