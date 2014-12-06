
;;; Assembly instructions

(in-package :asm-funcs)

(defun-asm fatal-error () ()
	(break-debugger)
	(go-to-sleep)
	(rjmp fatal-error))
