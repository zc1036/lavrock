
;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;   load asdf systems
;;   )

(defpackage :lavrock-util
  (:use :cl)
  (:export :elet :λ :recursive-length :multiple-value-mapcar*))

(defpackage :lavrock
  (:use :cl :lavrock-util)
  (:export :register
		   :register16
		   :register-value
		   :register-name
		   :register16-hi
		   :register16-lo
		   :register16-name
		   :register-is-io
		   :register-equals

		   :with-vm
		   :make-vm

		   :label
		   :with-labels

		   :defun-asm
		   :definterrupt

		   :read-sram-byte
		   :write-sram-byte

		   :rambegin
		   :ramend
		   :sram-size

		   :lobyte
		   :hibyte

		   :lshifti
		   :bitor

		   :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7 :R8 :R9 :R10 :R11 :R12 :R13 :R14 :R15 :R16
		   :R17 :R18 :R19 :R20 :R21 :R22 :R23 :R24 :R25 :R26 :R27 :R28 :R29 :R30 :R31
		   :SPH :SPL :SREG :ZERO
		   :X :Y :Z
		   :X+ :Y+ :Z+
		   :-X :-Y :-Z

		   :MCUCR :PORTA :DDRA

		   :SREG-C :SREG-Z :SREG-N :SREG-V :SREG-S :SREG-H :SREG-T :SREG-I

		   :asm-func-preamble-hook
		   :asm-func-epilogue-hook

		   :int0 :int1 :int2 :int3 :int4 :int5 :int6 :int7 :timer2-comp :timer2-ovf
		   :timer1-capt :timer1-compa :timer1-compb :timer1-ovf :timer0-comp :reset
		   :timer0-ovf :spi-stc :usart0-rx :usart0-udre :usart0-txc :adc :ee-ready
		   :analog-comp :timer1-compc :timer3-capt :timer3-compa :timer3-compb
		   :timer3-compc :timer3-ovf :usart1-rx :usart1-udre :usart1-txc :twi :smp-ready

		   :restore-registers
		   :ret-bypass-register-restoration
		   :ret-bypass-register-restoration-and-epilogue
		   :preserved-register-byte-count))

(defpackage :instructions
  (:use :cl :lavrock)
  (:export :ldi
		   :ld
		   :lpm
		   :lds
		   :mov
		   :movw
		   :st
		   :sts
		   :in
		   :out
		   :add
		   :adc
		   :adiw
		   :subi
		   :sbc
		   :ori
		   :andi
		   :inc
		   :dec
		   :tst
		   :cp
		   :cpc
		   :cpi
		   :cpse
		   :clr
		   :lsl
		   :lsr
		   :rol
		   :ror
		   :nop
		   :reti
		   :ret
		   :set
		   :clear-t
		   :call
		   :icall
		   :call-without-preserving-registers
		   :jmp
		   :rjmp
		   :pushreg
		   :popreg
		   :sbrc
		   :sbrs
		   :breq
		   :brlo
		   :brne
		   :brpl
		   :brmi
		   :brlo
		   :go-to-sleep

		   :break-debugger

		   :left-shift-n
		   :right-shift-n
		   :clear-reg

		   :with-registers
		   :with-registers-early-restore

		   :if-else
		   :if-only
		   :loop-while
		   :loop-forever
		   :break-while

		   :load-imm

		   :push-registers
		   :pop-registers

		   :push-caller-preserved-registers
		   :pop-caller-presered-registers))

(defpackage :instruction-tests
  (:use :cl :lavrock :instructions)
  (:export :test-instr-ldi :test-instr-mov :test-instr-add :run-tests))

(defpackage :codegen
  (:use :cl :lavrock :instructions)
  (:export :in-file
		   :defconstant-bytes
		   :defconstant-asm
		   :defregister-synonym
		   :defexpr-asm
		   :export-asm-func

		   :X-lo
		   :X-hi
		   :Y-lo
		   :Y-hi
		   :Z-lo
		   :Z-hi

		   :header-comment
		   :line-comment
		   :! :!! :!!!))

(defpackage :asm-funcs
  (:use :cl :lavrock :instructions :codegen)
  (:export :fatal-error))
