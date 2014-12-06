
;;;; Tests for the assembly instructions.

;;; Each test should treat the values of all registers and memory as
;;; being undefined, because right now all the tests are run on the
;;; same vm in the run-tests function. The tests modify the values of
;;; registers and memory, so do not use a test and expect the state of
;;; the vm be the same as it was before.

(in-package :instruction-tests)

(defun test-instr-ldi ()
	(ldi R2 3)

	(assert (= 3 (register-value R2))))

(defun test-instr-mov ()
	(ldi R6 9)
	(mov R1 R6)

	(assert (= 9 (register-value R1))))

(defun test-instr-add ()
	(ldi R1 100)
	(ldi R2 50)
	(add R1 R2)

	(assert (= 150 (register-value R1)))
	(assert (= 50 (register-value R2))))

(defun run-tests ()
	(with-vm (make-vm)
		(test-instr-ldi)
		(test-instr-mov)
		(test-instr-add)))

;;; We can't do this because we want the tests to be run in a specific order
;; (defun run-tests ()
;; 	(with-vm (make-vm)
;; 		(do-external-symbols (sym (find-package :lavrock-tests))
;; 			(when (and (< (length "TEST-INSTR-") (length (symbol-name sym)))
;; 					   (string= (subseq (symbol-name sym) 0 (length "TEST-INSTR-")) "TEST-INSTR-"))
;; 				(funcall sym)))))
