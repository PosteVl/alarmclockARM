;Clock timer which increments a location in memory using SVC calls.
;
;It can be stopped by pressing upper button, continued by pressing lower button, and reseted by holding upper button
;for 1 second.
;
;										Written by Postelnicu Vlad



Max_SVC	EQU	16			; declare value outside of the jump table for comparison

;------- SVC should find the SVC_entry at address 08 so we need to force it there ---------------------------------------------------------



	B	Reset				; Create two 4 bytes offset in memory
	NOP 
	B 	SVC_cal

;--------------------------------- SVC Dispatch Address 08 --------------------------------------------------------------------------------
SVC_cal PUSH	{LR}			; Push scratch register
	LDR	R14, [LR, #-4]		; Read SVC instruction
	BIC	R14, R14, #&FF000000	; Mask off opcode



SVC_entry				
	CMP	R14, #Max_SVC
	BHI	Out_of_range		; if we did not initiate SVC, jump away from the table
	ADR	R11, Jump_table
	LDR	PC, [R11, R14, LSL #2]	; Calculate address where Jump table should be

Jump_table
	DEFW	SVC_0			; read button ports
	DEFW	SVC_1			; printchar subroutine
	DEFW	SVC_2			; read timer bits



Out_of_range
;------------------------------------subroutine to read the portB bits---------------------------------------------------------------------
SVC_0		LDRB	R6, [R8]	; read portB bits
		POP	{LR}		; return execution
		MOVS	PC, LR		
;------------------------------------subroutine to read the timer--------------------------------------------------------------------------
SVC_2		LDRB	R2, [R0]	; read timer, R2 is never used again in the program, this is why i do not put it on a stack
		POP	{LR}		; return execution
		MOVS	PC, LR		
		;POP{PC}^		; return execution ->does MOVS and POP


;----------------------------------- subroutine to print a single character ----------------------------------------------------------------
SVC_1   LDRB	R0, [R8]		; load PORT B
	ORR	R0, R0, #&04		; set R/W = 1
	ORR	R0, R0, #&20		; LCD backlight on
	AND	R0, R0, #&FD 		; set RS = 0
	AND	R0, R0, #&EF		; LED lights off
	STRB    R0, [R8]
	B	step2

step2   LDRB	R0, [R8]
	ORR	R0, R0, #&01		; set Enable bus E=1
	STRB	R0, [R8]
	B	step3

step3	LDRB    R2, [R7]		; read LCD status byte
	B	step4

step4	LDRB	R0, [R8]
	AND	R0, R0, #&FE		; disable bus E=0
	STRB 	R0, [R8]
	B	step5

step5	AND	R2, R2, #&80		; see if status bit is set
	CMP	R2, #&80		
	BEQ 	step2
	B	step6

step6	LDRB	R0, [R8]
	AND	R0, R0, #&FB		; set R/W = 0
	ORR	R0, R0, #&02		; set RS = 1
	STRB	R0, [R8]
	B	step7
	
step7	STRB	R5, [R7] 		; output letter
	B	step8

step8	LDRB	R0, [R8]
	ORR	R0, R0, #&01		; set Enable bus E=1
	STRB	R0, [R8]
	B	step9

step9	AND	R0, R0, #&FE		; disable bus E=0
	STRB	R0, [R8]
	POP	{LR}
	MOVS	PC, LR




Reset		ADR	SP, _Sstack           		; address supervisor stack

;-----------------------------------------Perform change to user mode----------------------------------------------------------------------
		MOV	R14, #&D0			; put user mode bytes into R14(LR)
		MSR	SPSR, R14			; update stack pointer status register with content of LR
		ADR	R14, User_codestart		; which points to where user code should start
		MOVS	PC, R14				; perform the switch
;----------------------------------------port and variables declarations-------------------------------------------------------------------
User_codestart	B 	main


Timer_port      EQU     &10000008			; set timer port
Buttons_port	EQU	&10000004			; set buttons port
max_8bit        EQU     255				; 8 bit timer can only count to 255

count           DEFW    0				; memory location that we write to
resetCount	DEFW	0				; set 1 second reset timer


timeCount       MOV     R0, #Timer_port	
		MOV	R8, #Buttons_port		
                MOV     R1, #max_8bit
;------------------------------------------Count program-----------------------------------------------------------------------------------
branchReset     MOV     R4, #0				; register holding the counter Hz delay 
		STR	R4, count			; store a new 0 value at the memory location
		LDR	R5, count			; put memory we increment to R5
		

                SVC	2				; start with the first value the free counter stores
		MOV	R3, R2				; move it in another register

loop		SVC	0				; load port bits
		TST     R6, #0b01000000			; check if upper button is pressed
		BNE	Pause            		; if yes, branch to Pause state
		SVC	2				; load the same value ( because CPU is way faster, we will need to check many times)
                CMP     R2, R3				; compare the two sequential values
                BEQ     loop				; if they are equal, keep polling until value has changed
                ADD     R4, R4, #1			; otherwise add 1 to our loop counter		
                MOV     R3, R2				; get rid of one of the values
                CMP     R4, #1000			; compare the loop counter to 1000
                BNE     loop				; if not equal, repeat steps

restart_loop	MOV 	R4, #0

                LDR     R5, count			; if equal, increment desired memory location 
                ADD     R5, R5, #1
                STR     R5, count			; and store it in memory
						
                CMP     R5, #50				
                BEQ     end				; if we're finished, branch out
	
                B       loop				; else repeat process

pause_loop	SVC	0				; reload port bits
Pause		TST	R6, #0b10000000			; check if lower buttor has been pressed
		BNE	restart_loop			; if so, continue counting
		TST     R6, #0b01000000			; check if upper button is still pressed
		BNE	stop_reset			; if so, see if it is hold pressed for one second
		B	pause_loop			; otherwise, keep polling for the button 

stop_reset	MOV     R9, #0				; register holding the counter reset delay
		SVC	2				; start with the first value the free counter stores
		MOV	R3, R2				; put it in a free register

resetLoop	SVC	0				; reload port bits
		TST     R6, #0b01000000			; check if upper button is still pressed
		BEQ	Pause            		; if no, branch to Pause state	
		SVC	2				; load a second value from the counter 
		CMP	R2, R3				; as long as counter values are the same, 
		BEQ	resetLoop			; keep getting a new value (less than 1/1000th of a second passed)
		ADD	R9, R9, #1			; if values are different, we can increment 1/1000th of a second
		MOV	R3, R2
		CMP	R9, #1000			; for a thousand times
		BLT	resetLoop			
		B 	branchReset			; until we have a second, in which case we reset the counter

end             B       end

;----------------------------------------main----------------------------------------------------------------------------------------------
main		B	timeCount

DEFS	100				; define supervisor stack
_Sstack	
