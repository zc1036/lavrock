
(in-package :asdf-user)

(defsystem "lavrock"
	:description "A library for writing AVR assembly with lisp and a partial emulator for the ATMega128"
	:version "0.1"
	:author "Zach"
	:licence "GPL 3.0"
	:components ((:file "packages")
				 (:file "util")
				 (:file "lavrock")
				 (:file "instructions")
				 (:file "codegen")
				 (:file "asm-funcs"))
	;; :depends-on ("hunchentoot" "cl-json" "ironclad" "sqlite")
	)
