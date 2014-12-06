
;;; Assembly instructions

(in-package :instructions)

;; Load immediate into register
(lavrock::def-checked-asm-instr ldi (reg imm)
	(progn
		(check-type reg register)
		(check-type imm (or lavrock::asm-expr (integer -128 255)))) ;; typecheck

	(format nil "ldi ~a, ~a"                                        ;; textual format
			(register-name reg)
			(lavrock::asm-expr-repr-or-value imm)))

;; Load from data space indirectly from X, Y, or Z
(lavrock::def-checked-asm-instr ld (dst src)
	(progn
		(check-type dst register)
		(check-type src register16))

	(format nil "ld ~a, ~a" (register-name dst) (register16-name src)))

;; Load indirect from program memory from Z
(lavrock::def-checked-asm-instr lpm (dst src)
	(progn
		(check-type dst register)
		(assert (or (eq src Z) (eq src Z+) (eq src -Z))))

	(format nil "lpm ~a, ~a" (register-name dst) (register16-name src)))

;; Copy the contents of a register to another register.
(lavrock::def-checked-asm-instr mov (regdst regsrc)
	(progn (check-type regdst register) (check-type regsrc register))

	(format nil "mov ~a, ~a" (register-name regdst) (register-name regsrc))
	
	(setf (register-value regdst) (register-value regsrc)))

;; Output data to an IO port.
(lavrock::def-checked-asm-instr out (regdst regsrc)
	(progn (check-type regdst (and register (satisfies register-is-io)))
		   (check-type regsrc (and register (not (satisfies register-is-io)))))

	(format nil "out ~a, ~a" (register-name regdst) (register-name regsrc))
	
	(setf (register-value regdst) (register-value regsrc)))

;; Get data from an IO port.
(lavrock::def-checked-asm-instr in (regdst regsrc)
	(progn (check-type regsrc (and register (satisfies register-is-io)))
		   (check-type regdst (and register (not (satisfies register-is-io)))))

	(format nil "in ~a, ~a" (register-name regdst) (register-name regsrc))
	
	(setf (register-value regdst) (register-value regsrc)))

;; Add two registers without carry and store the result in the first register.
(lavrock::def-checked-asm-instr add (regdst reg2)
	(progn (check-type regdst register) (check-type reg2 register))

	(format nil "add ~a, ~a" (register-name regdst) (register-name reg2))

	(setf (register-value regdst) (+ (register-value regdst) (register-value reg2))))

;; Add two registers with carry and store the result in the first register.
(lavrock::def-checked-asm-instr adc (regdst reg2)
	(progn (check-type regdst register) (check-type reg2 register))

	(format nil "adc ~a, ~a" (register-name regdst) (register-name reg2)))

;; Add an immediate to a word
(lavrock::def-checked-asm-instr adiw (registers imm)
	(progn
		(check-type registers cons)
		(check-type (first registers) register)
		(check-type (second registers) register)
		(check-type imm (or lavrock::asm-expr (integer -128 255))))

	(format nil "adiw ~a:~a, ~a"
			(register-name (first registers))
			(register-name (second registers))
			(lavrock::asm-expr-repr-or-value imm)))

;; Subtract an immediate from a byte
(lavrock::def-checked-asm-instr subi (reg imm)
	(progn
		(check-type reg register)
		(check-type imm (or lavrock::asm-expr (integer 0 255))))

	(format nil "subi ~a, ~a" (register-name reg) (lavrock::asm-expr-repr-or-value imm)))

(lavrock::def-checked-asm-instr sbc (lhs rhs)
	(progn
		(check-type lhs register)
		(check-type rhs register))

	(format nil "sbc ~a, ~a" (register-name lhs) (register-name rhs)))

;; Bitwise OR a register with an immediate
(lavrock::def-checked-asm-instr ori (dst imm)
	(progn
		(check-type dst register)
		(check-type imm (or lavrock::asm-expr (integer 0 255))))

	(format nil "ori ~a, ~a" (register-name dst) (lavrock::asm-expr-repr-or-value imm)))

;; Bitwise AND a register with an immediate
(lavrock::def-checked-asm-instr andi (dst imm)
	(progn
		(check-type dst register)
		(check-type imm (or lavrock::asm-expr (integer 0 255))))

	(format nil "andi ~a, ~a" (register-name dst) (lavrock::asm-expr-repr-or-value imm)))

;; push a byte onto the stack and decrement SP
(lavrock::def-checked-asm-instr pushreg (reg)
	(check-type reg register)

	(format nil "push ~a" (register-name reg)))

;; pop a byte from the stack, stores it in the register, and increments SP
(lavrock::def-checked-asm-instr popreg (reg)
	(check-type reg register)

	(format nil "pop ~a" (register-name reg)))

;; enter a low power state
(lavrock::def-asm-instr go-to-sleep ()
	"sleep")

;; logical shift left one bit
(lavrock::def-checked-asm-instr lsl (reg)
	(check-type reg register)

	(format nil "lsl ~a" (register-name reg)))

;; logical shift right one bit
(lavrock::def-checked-asm-instr lsr (reg)
	(check-type reg register)

	(format nil "lsr ~a" (register-name reg)))

;; logical shift left one bit and pulls the carry bit into the LSB
(lavrock::def-checked-asm-instr rol (reg)
	(check-type reg register)

	(format nil "rol ~a" (register-name reg)))

;; logical shift right one bit and pulls the carry bit into the MSB
(lavrock::def-checked-asm-instr ror (reg)
	(check-type reg register)

	(format nil "ror ~a" (register-name reg)))

;; clear register
(lavrock::def-checked-asm-instr clr (reg)
	(check-type reg register)

	(format nil "clr ~a" (register-name reg)))

(defun clear-reg- (reg)
	(check-type reg (or register register16))

	(if (typep reg 'register16)
		(progn
			(clr (register16-hi reg))
			(clr (register16-lo reg)))
		(clr reg)))

(defun clear-reg (&rest regs)
	(dolist (reg regs)
		(if (typep reg 'list)
			(clear-reg reg)
			(clear-reg- reg))))

;; test if a register is zero or negative
(lavrock::def-checked-asm-instr tst (reg)
	(check-type reg register)

	(format nil "tst ~a" (register-name reg)))

;; store direct to data space
(lavrock::def-checked-asm-instr sts (dst src)
	(progn
		(check-type dst (or lavrock::asm-expr symbol string (integer -32768 65535)))
		(check-type src register))

	(format nil "sts ~a, ~a"
			(lavrock::asm-expr-repr-or-value dst)
			(register-name src)))

;; store to X, Y or Z in data space
(lavrock::def-checked-asm-instr st (dst src)
	(progn
		(check-type dst register16)
		(check-type src register))

	(format nil "st ~a, ~a" (register16-name dst) (register-name src)))

;; load direct from data space
(lavrock::def-checked-asm-instr lds (dst src)
	(progn
		(check-type dst register)
		(check-type src (or (and register (satisfies register-is-io)) lavrock::asm-expr symbol string (integer -32768 65535))))

	(format nil "lds ~a, ~a"
			(register-name dst)
			(if (typep src 'register)
				(format nil "~a + 0x20" (register-name src))
				(lavrock::asm-expr-repr-or-value src))))

(defun word-shift-n (reg lobyte-instr hibyte-instr times)
	(let ((reg-name-msb (gensym)) (reg-name-lsb (gensym)))
		`(destructuring-bind (,reg-name-msb ,reg-name-lsb) (list ,@reg)
			 (loop repeat ,times do
				  (,lobyte-instr ,reg-name-lsb)
				  (,hibyte-instr ,reg-name-msb)))))

(defun byte-shift-n (reg instr times)
	(let ((reg-name (gensym)))
		`(let ((,reg-name ,reg))
			 (loop repeat ,times do
				  (,instr ,reg-name)))))

(defmacro left-shift-n (reg times)
	(if (consp reg)
		(word-shift-n reg 'lsl 'rol times)
		(byte-shift-n reg 'lsl times)))

(defmacro right-shift-n (reg times)
	(if (consp reg)
		(word-shift-n (reverse reg) 'lsr 'ror times) ;; we reverse the list so that the high byte is shifted first (for carry bit)
		(byte-shift-n reg 'lsr times)))

(defun push-registers (registers)
	(dolist (register registers)
		(if (typep register 'register16)
			(progn
				(pushreg (register16-lo register))
				(pushreg (register16-hi register)))
			(pushreg register))))

(defun pop-registers (registers)
	(dolist (register registers)
		(if (typep register 'register16)
			(progn
				(popreg (register16-hi register))
				(popreg (register16-lo register)))
			(popreg register))))

(defun intersection-caller-preserved-registers (caller callee)
	(intersection
	 (lavrock::filter-caller-preserved-registers (lavrock::asm-func-local-registers caller))
	 (lavrock::filter-caller-preserved-registers (lavrock::asm-func-local-registers callee))))

(defun push-caller-preserved-registers (caller callee)
	(push-registers (intersection-caller-preserved-registers caller callee)))

(defun pop-caller-preserved-registers (caller callee)
	(pop-registers (nreverse (intersection-caller-preserved-registers caller callee))))

;; Call the given function without automatically preserving callee-preserved registers
(lavrock::def-checked-asm-instr call-without-preserving-registers (func)
	(progn
		(check-type func lavrock::asm-func)

		;; we call the following in output as well as emulate mode,
		;; because it will either add itself to the list of called
		;; funcs, or execute its body, as appropriate.
		(lavrock::asm-func-call func))

	(format nil "call ~a" (lavrock::asm-func-name func)))

;; Call the given function and preserve callee-preserved registers
(defmacro call (func)
	(let ((func-name (gensym)))
		`(let ((,func-name ,func))
			 (when lavrock::this-func
				 (push-caller-preserved-registers lavrock::this-func ,func-name))
			 (call-without-preserving-registers ,func-name)
			 (when lavrock::this-func
				 (pop-caller-preserved-registers lavrock::this-func ,func-name)))))

;; indirect call
(lavrock::def-asm-instr icall ()
	(format nil "icall"))

;; absolute jump
(lavrock::def-checked-asm-instr jmp (target)
	(progn
		(check-type target (or lavrock::asm-label symbol string))

		(when (typep target 'lavrock::asm-func)
			(lavrock::asm-func-call target)))

	(format nil "jmp ~a" (lavrock::asm-label-name target)))

;; relative jump
(lavrock::def-checked-asm-instr rjmp (target)
	(progn
		(check-type target (or lavrock::asm-label symbol string))

		(when (typep target 'lavrock::asm-func)
			(lavrock::asm-func-call target))) ;; we call the function so it gets added to the called-list

	(format nil "rjmp ~a"
			(if (typep target 'lavrock::asm-label)
				(lavrock::asm-label-name target)
				target)))

;; no-op
(lavrock::def-asm-instr nop ()
	"nop")

;; return from interrupt
(lavrock::def-asm-instr reti ()
	"reti"

	(setf (lavrock::vm-interrupts-enabled lavrock::*current-vm*) t)
	(return)) ;; this (return) jumps out of the (block nil) that encapsulates the body of every function defined with defun-asm.

;; return from function
(lavrock::def-asm-instr ret ()
	"ret"

	(return)) ;; this (return) jumps out of the (block nil) that encapsulates the body of every function defined with defun-asm.

;; set T bit in SREG
(lavrock::def-asm-instr set-t ()
	"set")

;; clear T bit in SREG
(lavrock::def-asm-instr clear-t ()
	"clt")

;; increment a register
(lavrock::def-checked-asm-instr inc (reg)
	(check-type reg register)
	
	(format nil "inc ~a" (register-name reg)))

;; decrement a register
(lavrock::def-checked-asm-instr dec (reg)
	(check-type reg register)
	
	(format nil "dec ~a" (register-name reg)))

;; compare two registers and set SREG bits accordingly.
(lavrock::def-checked-asm-instr cp (reg1 reg2)
	(progn (check-type reg1 register) (check-type reg2 register))

	(format nil "cp ~a, ~a" (register-name reg1) (register-name reg2)))

;; compare two registers and skip the next instruction if they're equal.
(lavrock::def-checked-asm-instr cpse (reg1 reg2)
	(progn (check-type reg1 register) (check-type reg2 register))

	(format nil "cpse ~a, ~a" (register-name reg1) (register-name reg2)))

;; cp with carry
(lavrock::def-checked-asm-instr cpc (reg1 reg2)
	(progn (check-type reg1 register) (check-type reg2 register))

	(format nil "cpc ~a, ~a" (register-name reg1) (register-name reg2)))

;; cp immediate
(lavrock::def-checked-asm-instr cpi (reg1 imm)
	(progn (check-type reg1 register) (check-type imm (or (integer -128 255) lavrock::asm-expr)))

	(format nil "cpi ~a, ~a" (register-name reg1) (lavrock::asm-expr-repr-or-value imm)))

;; skip if bit in register is cleared
(lavrock::def-checked-asm-instr sbrc (reg bit)
	(progn
		(check-type reg register)
		(check-type bit (or lavrock::asm-expr symbol string (integer 0 7))))

	(format nil "sbrc ~a, ~a"
			(register-name reg)
			(lavrock::asm-expr-repr-or-value bit)))

;; skip if bit in register is set
(lavrock::def-checked-asm-instr sbrs (reg bit)
	(progn
		(check-type reg register)
		(check-type bit (or lavrock::asm-expr symbol string (integer 0 7))))

	(format nil "sbrs ~a, ~a"
			(register-name reg)
			(lavrock::asm-expr-repr-or-value bit)))

;; branch if equal
(lavrock::def-checked-asm-instr breq (label)
	(check-type label (or lavrock::asm-label string symbol))

	(format nil "breq ~a"
 			(if (typep label 'lavrock::asm-label)
				(lavrock::asm-label-name label)
				label)))

;; Branch if not equal
(lavrock::def-checked-asm-instr brne (label)
	(check-type label (or lavrock::asm-label string symbol))

	(format nil "brne ~a"
 			(if (typep label 'lavrock::asm-label)
				(lavrock::asm-label-name label)
				label)))

;; Branch if positive
(lavrock::def-checked-asm-instr brpl (label)
	(check-type label (or lavrock::asm-label string symbol))

	(format nil "brpl ~a"
 			(if (typep label 'lavrock::asm-label)
				(lavrock::asm-label-name label)
				label)))

;; Branch if negative
(lavrock::def-checked-asm-instr brmi (label)
	(check-type label (or lavrock::asm-label string symbol))

	(format nil "brmi ~a"
 			(if (typep label 'lavrock::asm-label)
				(lavrock::asm-label-name label)
				label)))

;; Branch if T is cleared
(lavrock::def-checked-asm-instr brtc (label)
	(check-type label (or lavrock::asm-label string symbol))

	(format nil "brtc ~a"
 			(if (typep label 'lavrock::asm-label)
				(lavrock::asm-label-name label)
				label)))

;; branch if the first operand is less than the second (unsigned)
(lavrock::def-checked-asm-instr brlo (label)
	(check-type label (or lavrock::asm-label string symbol))

	(format nil "brlo ~a"
 			(if (typep label 'lavrock::asm-label)
				(lavrock::asm-label-name label)
				label)))

;; copy the given register pair to another register pair
(lavrock::def-checked-asm-instr movw (rega1 rega2 regb1 regb2)
	(progn
		(check-type rega1 register)
		(check-type rega2 register)
		(check-type regb1 register)
		(check-type regb2 register))

	(format nil "movw ~a:~a, ~a:~a"
			(register-name rega1)
			(register-name rega2)
			(register-name regb1)
			(register-name regb2)))

(lavrock::def-asm-instr break-debugger ()
	"break")

(defmacro with-registers (registers &body body)
	(let ((regs-name (gensym)))
		`(let ((,regs-name (list ,@registers)))
			 (flet ((with-registers-early-restore ()
						(pop-registers (reverse ,regs-name))))
				 (push-registers ,regs-name)
				 ,@body
				 (with-registers-early-restore)))))

(defun word-cp (lhs rhs)
	(check-type lhs cons)
 	(check-type rhs cons)
 	(assert (eql (length lhs) 2))
 	(assert (eql (length rhs) 2))
 	(check-type (first lhs) register)
 	(check-type (second lhs) register)
 	(check-type (first rhs) register)
 	(check-type (second rhs) register)
	
	(destructuring-bind ((lhs-hi lhs-lo) (rhs-hi rhs-lo)) (list lhs rhs)
		(cp lhs-lo rhs-lo)
		(cpc lhs-hi rhs-hi)))

(defun word-cpi (lhs imm)
	(declare (ignore lhs))
	(declare (ignore imm))
	(error "Word CPI is unsupported"))

(defun word-is-zero (reg)
	(check-type reg cons)
	(assert (eql (length reg) 2))
	(check-type (first reg) register)
	(check-type (second reg) register)

	(destructuring-bind (reg-hi reg-lo) reg
		(cp reg-lo zero)
		(cpc reg-hi zero)))

;;; List of lists of the form (SYMBOL-NAME BRANCH-INSTR
;;; DOUBLE-BRANCH-INSTR COMPARE-FUN DOUBLE-COMPARE-FUN). These
;;; instructions are the inverse of the desired operation so that the
;;; branch will be taken if the condition is not true.
(eval-when (:compile-toplevel :load-toplevel :execute)
	(defparameter branches '(("=" brne brne cp word-cp)
							 ("I=" brne brne cpi word-cpi)
							 ("!=" breq breq cp word-cp)
							 ("I!=" breq breq cpi word-cpi)
							 (">=" brlo brlo cp word-cp)
							 ("ZERO" brne brne tst word-is-zero)
							 ("NONZERO" breq breq tst word-is-zero) ;; breq branches if SREG(Z) = 1, tst does that if operand is 0
							 ("NEGATIVE" brpl brpl tst word-cp)
							 ("POSITIVE" brmi brmi tst word-cp))))

(defmacro if-else (condition success failure)
	(let* ((branch-test-name (symbol-name (first condition)))
		   (branch-spec (assoc branch-test-name branches :test #'string=))
		   (double (consp (second condition)))) ;; test if the second arg is a cons
		(when (null branch-spec)
			(error "Invalid condition function ~a in branch-if" branch-test-name))
		
		`(progn
			 ;; the compare instruction
			 ,(if (eql (length condition) 2)
				  (list (if double (fifth branch-spec) (fourth branch-spec)) ;; one argument
						(if double (cons 'list (second condition)) (second condition)))
				  (list (if double (fifth branch-spec) (fourth branch-spec)) ;; two arguments
						(if double (cons 'list (second condition)) (second condition))
						(if double (cons 'list (third condition)) (third condition))))

			 (with-labels (failure-condition-start failure-condition-end)
				 ;; the branch instruction
				 ;; if the condition succeeds, skip over success
				 (,(if double (third branch-spec) (second branch-spec)) failure-condition-start)
				 ,success
				 (jmp failure-condition-end)

				 (label failure-condition-start)
				 ,failure
				 (label failure-condition-end)))))

(defmacro if-only (condition &body success)
	(let* ((branch-test-name (symbol-name (first condition)))
		   (branch-spec (assoc branch-test-name branches :test #'string=))
		   (double (consp (second condition))))
		(when (null branch-spec)
			(error "Invalid condition function ~a in branch-if" branch-test-name))

		`(progn
			 ,(if (eql (length condition) 2)
				  (list (if double (fifth branch-spec) (fourth branch-spec))
						(if double (cons 'list (second condition)) (second condition)))
				  (list (if double (fifth branch-spec) (fourth branch-spec))
						(if double (cons 'list (second condition)) (second condition))
						(if double (cons 'list (third condition)) (third condition))))

			 (with-labels (success-condition-end)
				 (,(if double (third branch-spec) (second branch-spec)) success-condition-end)
				 ,@success
				 (label success-condition-end)))))

(defmacro loop-while (spec &body body)
	(destructuring-bind (&key (call nil) test) spec
		`(with-labels (BEGIN-LOOP END-LOOP)
			 (label BEGIN-LOOP)
			 ,call

			 (flet ((break-while ()
						(jmp END-LOOP)))
				 (if-only ,test
					 ,@body
					 (jmp BEGIN-LOOP)))

			 (label END-LOOP))))

(defmacro loop-forever (&body body)
	`(with-labels (BEGIN-LOOP END-LOOP)
		 (label BEGIN-LOOP)

		 (flet ((break-while ()
					(jmp END-LOOP)))
			 ,@body)

		 (label END-LOOP)))

;; (defun word-cp->= (lhs rhs)
;; 	(check-type lhs cons)
;; 	(check-type rhs cons)
;; 	(assert (eql (length lhs) 2))
;; 	(assert (eql (length rhs) 2))
;; 	(check-type (first lhs) register)
;; 	(check-type (second lhs) register)
;; 	(check-type (first rhs) register)
;; 	(check-type (second rhs) register)

;; 	(clear-t)

;; 	(destructuring-bind ((lhs-hi lhs-lo) (rhs-hi rhs-lo)) (list lhs rhs)
;; 		(if-only (>= lhs-hi rhs-hi)
;; 				 (if-else (= lhs-hi rhs-hi)
;; 						  (if-only (>= lhs-lo rhs-lo)
;; 								   (set-t))
;; 						  (set-t)))))

(defun load-imm (reg imm)
	(cond
	  ((typep reg 'register16)
	   (ldi (register16-lo reg) (lobyte imm))
	   (ldi (register16-hi reg) (hibyte imm)))

	  ((and (typep reg 'list) (typep (first reg) 'register) (typep (second reg) 'register))
	   (ldi (first reg) (hibyte imm))
	   (ldi (second reg) (lobyte imm)))

	  ((typep reg 'register)
	   (ldi reg imm))

	  (t (error "Unsupported destination in LOAD-IMM (~a ~a)" reg imm))))
