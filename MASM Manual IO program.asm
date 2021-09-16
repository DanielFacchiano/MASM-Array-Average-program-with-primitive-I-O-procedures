TITLE Project 6     (Proj6_Facchiad.asm)

; Author:	Daniel	Facchiano
; Last Modified:	5/31/2021
; OSU email address: facchiad@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:   6      Due Date: 6/6/2021
; Description: A Program that uses macros to read and convert strings of ascii characters
; intos digits and manipluate them. The program manually converts these strings as they 
; are read into their corresponding numeric value. This is acomplished by using string 
; primitives to load chracters into registers. These characters are evaluated to see if 
; they correspond to a number, if so they are converted (sub 48) into there numeric value 
; and algebraicly appended to a Dword representing that number. If the number has no 
; invalid characters and fits in a 32 bit register, the number is saved to a data location. 
; We save 10 of these numbers in an array by looping this procedure 10 times in an 
; incrementing array. We then loop through this array and use a procedure to convert 
; these numbers into a string by manually evaulating each digit in the number, adding
; 48 to it, and appending it to a string from right to left after padding unused string 
; slots with 0s. We print these strings within the procedure. Next we derive a sum
; from the array of numbers by counting every number in the array in a loop and print it.
; Finally, we get an average from the sum of numbers by dividing the sum by ARRAYSIZE and 
; print it. We print these numbers to screen using the same method we used to print the 
; other digits. We say goodbye to the user with the display macro and end the program.
;
;
INCLUDE Irvine32.inc

; ---------------------------------------------------------------------------------
; Name: mGetString
;
; Prompts the user and gets a string from the user, puts it in memory locaction.
; inputstring address. Also retrieves the count of the string and puts it in a 
; variable.
;
; Preconditions: must use memory address as parameter (except count) Amount of
; allowed characters inside readCount parameter
;
; Receives:
; promptAddress = address of string to prompt user
; inputStringAddress = address of the string user entering information into
; readCount = amount of characters allowed for input
; inputStringBytesAddress = The address of the variable holding entered bytes
;
; returns: inputStringAddress = string entered by user
; inputStringBytesAddress = the amount bytes the user entered
; ---------------------------------------------------------------------------------
mGetString   MACRO promptAddress, inputStringAddress, readCount, inputStringBytesAddress
	PUSH	EDX
	PUSH	ECX
	PUSH	EAX
	PUSH	EDI

	mov		edx, promptAddress						;print prompt to user
	call	WriteString	
	mov		ECX, readCount							;max length in readcount
	mov		EDX, inputStringAddress					;outstring address prepped
	call	ReadString
	MOV		EDI, inputStringBytesAddress			;output variable in edi
	mov		[EDI], EAX								;put output byes in inputStringBytes
	
	POP		EDI
	POP		EAX
	POP		ECX
	POP		EDX
ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; Takes a string address as a paremeter, pushes that strings address into the edx
; uses the irvine function to print the string to the screen
;
; Preconditions: Paremeter must be the address of a string
;
; Receives: outStringAddress = address of string to print to screen
;
; returns: string at outStringAddress printed to the screen
; ---------------------------------------------------------------------------------
mDisplayString   MACRO outStringAddress
	push	edx
	mov		edx, outStringAddress					;print whatever string passed in
	call	WriteString
	pop		edx
ENDM


MAXSIZE= 100										;length of any string user trys to enter in
OUTPUT_SIZE = 12									;Max size of output strings
ARRAYSIZE = 10										;max amount of elements in the array


.data
programIntro	BYTE		"Program 6: Low-level I/O and String Primitives ",13,10,0
authorIntro		BYTE		"Program by: Daniel Facchiano",13,10,0	
amountPrompt	BYTE		"Please enter in 10 signed decimal integers.",13,10,0	
programReqs		BYTE		"The numbers need to contain no invalid characters and must fit inside a 32 bit register.",13,10,0	
programInfo		BYTE		"After entering 10 numbers, the integers will be shown along with a sum and an average.",13,10,0	
errorMessage	BYTE		"Error: Number too large, too small, or contains invalid characters",0
inputPrompt		BYTE		"Please enter a string of numbers: ",0
inString		BYTE		MAXSIZE DUP(?)			; Where the user input String is held
inStringCount	DWORD		LENGTHOF inString		; Total Size of user String array in bytes
inStringBytes	DWORD		0						; The Amount of bytes the user input actually is 
outDigitCount	DWORD		0
outString		BYTE		OUTPUT_SIZE	DUP(1)		; Where the derived String is held
outStringCount	DWORD		LENGTHOF outString
boolNegative	DWORD		0						;Helpful parameter to determine negative or not
numArray		DWORD		ARRAYSIZE DUP(?)		;Where we are reading values into
outputNum		DWORD		0
outputPrompt	BYTE		"These are the numbers you entered in: ",0
outputPad		BYTE		", ",0
outputSum		BYTE		"The sum of all of your numbers is: ",0
outputAverage	BYTE		13,10,"The truncated average is: ",0
numSum			DWORD		?						;holds sum derived from array
numAverage		DWORD		?						;holds average derived from sum
goodBye			BYTE		13,10,"Thank you for using my program, Good Bye",0

.code
main PROC
; ----------------------------------------------------
;Print the users name and the title of the program and then
;	Prints a description of what the program does
; ----------------------------------------------------
	mDisplayString	OFFSET	programIntro
	mDisplayString	OFFSET	authorIntro
	call			crlf
	mDisplayString	OFFSET	amountPrompt
	mDisplayString	OFFSET	programReqs
	mDisplayString	OFFSET	programInfo
	call			crlf

	mov				ecx, ARRAYSIZE					;We prep ecx to loop for ARRAYSIZE (10) values
	mov				edi, OFFSET numArray			;Edi points to beginning of emtpy array


; ----------------------------------------------------
; This loop calls the readval function and puts result in num array location
;	The loop increments array to the next array location and calls ReadVal 
;	to fill it	with a number. We do this until the array has 10 values/is full.
; ----------------------------------------------------

readLoop:
	;Push parameters for ReadVal and Call
	PUSH			OFFSET errorMessage				;[EBP+28] Message for invalid input
	PUSH			edi								;[EBP+24] Destination location of number derived via string
	PUSH			OFFSET inputPrompt				;[EBP+20] String to prompt the user for input
	PUSH			OFFSET inString					;[EBP+16] Where the user input will be kept 
	PUSH			MAXSIZE							;[EBP+12] Max characters for a line of input
	PUSH			OFFSET inStringBytes			;[EBP+8] Tells us how many characters were read
	CALL			ReadVal							;Get a string of "numbers", convert them to a number and put it into array location
	;Output number is in the array index
	add				edi, 4							;Edi points at next index
	LOOP			readLoop						;repeat until 10 values read
		
	call			crlf

	mDisplayString	OFFSET outputPrompt				;Prompt user that they are going to see there numbers again
	call			crlf

	mov				ecx, ARRAYSIZE					;Prepare to loop through ARRAYSIZE values
	mov				esi, OFFSET numArray
	
;----------------------------------------------------
;This loop prints the DWORDS in the array by passings its value to the writeval 
;	procedure in the loop ARRAYSIZE(10) times
;----------------------------------------------------
writeLoop:
	;Push parameters for WriteVal and Call
	push			OFFSET	boolNegative			;[EBP+16] Helper boolean, less annoying then pushing/popping registers
	PUSH			[esi]							;[EBP+12] Number we are converting
	PUSH			OFFSET outString				;[EBP+8]  Array to hold the string to Print
	CALL			WriteVal
	CMP				ECX, 1							;skip over grammar for last iteration
	JE				postGrammar
	mDisplayString	OFFSET outputPad				;adds comma and space to make numbers readable
postGrammar:
	add				esi, 4							;increment the array
	loop			writeLoop						;next iteration
	call			crlf

;----------------------------------------------------
;Loop to sum all of the numbers in the array we created
;----------------------------------------------------
	mov				ecx, ARRAYSIZE					;prep for ARRAYSIZE iterations
	mov				esi, OFFSET numArray			;put value source in esi
	mov				eax, 0							;sum starts at 0
sumLoop:
	mov				ebx, [esi]						;move value in array to ebx
	add				eax, ebx						; add it to running total
	add				esi, 4							;increment to next array iteration
	loop			sumLoop							;next iteration
	mov				numSum, eax						;put the result in the numSum data label

	mDisplayString	OFFSET outputSum				;Tell the user they are going to see the sum

	;call writeval to print the sum we just created
	push			OFFSET	boolNegative			;[EBP+16] Helper boolean, less annoying then pushing/popping registers
	PUSH			numSum							;[EBP+12] Number we are converting
	PUSH			OFFSET outString				;[EBP+8]  Array to hold the string to Print
	CALL			WriteVal

	;Calculate average value and move into datalabel
	MOV				EBX, ARRAYSIZE					;prepare to divide by ARRAYSIZE
	CDQ												;needed for 32bit operations with idiv
	IDIV			EBX								;divide whats in the eax(sum) by ARRAYSIZE(elements)
	MOV				numAverage, EAX

	mDisplayString	OFFSET outputAverage			;tell user they are going to see the average

	;call WriteVal for the average and say goodbye
	push			OFFSET	boolNegative			;[EBP+16] Helper boolean, less annoying then pushing/popping registers
	PUSH			numAverage						;[EBP+12] Number we are converting
	PUSH			OFFSET outString				;[EBP+8]  Array to hold the string to Print
	CALL			WriteVal
	call			crlf
	mDisplayString	OFFSET goodBye					;Say goodbye to the user
	call			crlf


main ENDP
	Invoke ExitProcess,0	; exit to operating system

; ---------------------------------------------------------------------------------
; Name: WriteVal
;
; Takes a number passed to it and converts that number into a string. Then prints that String.
; Accomplishes this by  first detecting if the number is negative. This tells us we need
; to insert - and pad 1 less 0. Next we count the characters in the string by dividing it by
; 10 (remove digit) and counting how many times we do that until no digits remain. Next we 
; pad 0s by subtracting the amount of characters from the total size of the string (OUTPUT_SIZE) 
; and putting that many 0s in the string from the end of the string to the "left". We do this 
; by storing 0s from the end of the string, with the amount of 0s in the ecx with STD set, 0 in al 
; and calling rep STOSB. at this point the EDI is pointing at the place where the 'rightmost' digits
; belongs. We divide the number version of the string we are making by 10, this produce the
; rightmost digit in the edi remainder. We add 48 to that (make ascii) and use stosb
; to put it in the next slot of the array. We do this until 0 is in the eax (no more digits)
; signifying that all digits have been appended to the string. Finally we print this string
; with the mDisplayString macro.
; 
;
; Preconditions: Valid Signed DWORD in the [eb+12] parameter
; string address at [ebp+8] points to a string that is large enough for 11 characters
;
; Postconditions: none.
;
; Receives:
; [ebp+16] = boolNegative = Boolean to tell us if the number we are converting is negative
; [ebp+12] = converNumber = copy of the number we are converting to string and printing 
; [ebp+8] = outString = Address of array to hold string we are producing and printing 
; OUTPUT_SIZE = Size of the string we are deriving from the number. Needed for padding 
;
; returns: String derived from number in outString ([ebp+8]). 
; String Derived from number printed to screen.
; ---------------------------------------------------------------------------------
WriteVal PROC
	PUSH		EBP
	MOV			EBP, ESP
	PUSH		EBX
	PUSH		ECX
	PUSH		ESI
	PUSH		EDX
	PUSH		EAX
	PUSH		EDI
	
	;prep registers
	MOV			EDI, [EBP+8]				;Address of outString in the EDI
	MOV			ESI, [EBP+16]				;Prep boolNegative Boolean
	mov			ebx, 0
	MOV			[ESI], ebx					; reset it

; First thing we do is detect if our DWORD is negative. 
	mov			eax, [EBP+12]				; number we are converting copied into the EAX
	cmp			eax, 0
	JNS			PostSetNegative1
	MOV			ebx, 1						;isNegative boolean set to 1
	MOV			[ESI], ebx					; set it

	MOV			EBX, -1						;derive positive version
	IMUL		EBX
PostSetNegative1:

;	We have the positive version of our negative in the EAX with the "sign" boolean set Lets get the count
	mov			ecx, 0						;we hold count in ecx
	
	;To get the count, we count divides by 10 until the number is 0 (tells us amount of digits)
countingLoop:
	mov			edx, 0
	mov			ebx, 10
	div			ebx
	inc			ecx							;count++
	cmp			eax, 0						;Are we there yet?
	JE			postCountingLoop
	jmp			countingLoop				;Loop forever (until eax is 0)

postCountingLoop:
;amount of digits in the ecx

; Before we calculate our string, we must check if we have a negative, if so we must place the negative sign in the first bit and pad 0s
	MOV			ESI, [EBP+16]
	MOV			EBX, 1
	CMP			[ESI], EBX
	JNE			positivePadding
	;negative padding is here
	CLD
	MOV			AL, 45
	STOSB		;put the subtraction symbol in the front of the string
	mov			eax, OUTPUT_SIZE			;We need to calculate amoung of 0s to pad for string
	inc			ecx							;count+1 for the negative symbol(of digits to NOT pad)
	sub			eax, ecx				
	mov			ecx, eax					;Amount of 0s to pad in the ecx
	;we must now jump to the end of the string
	mov			ebx, OUTPUT_SIZE
	sub			ebx, 2
	add			edi, ebx					;edi pointing at the end of the string
	STD										;set direction flag to 1 (bkwards)
	MOV			AL, 0						;put 0 in the al
	REP			STOSB						;pad 0 going backwards ECX times
	JMP			postPadding
	;if its not negative, we need to pad 0s to, but the amount is different0
positivePadding:
	MOV			EAX, OUTPUT_SIZE			;We must calculate 0s to pad
	SUB			EAX, ECX
	MOV			ECX, EAX					;Amount of 0s to pad in the ecx
	MOV			EBX, OUTPUT_SIZE			;We must jump to the end of the string
	DEC			EBX
	ADD			EDI, EBX					;edi points at the end of the string
	STD										;set directions backwards
	MOV			AL, 0						;put 0 in the al for padding
	REP			STOSB						;pad ecx 0s
postPadding:

; We need to get the positive version of our number back in the EAX, if negative, or just the number if positive 
	mov			eax, [EBP+12]				; number to convert copied into the EAX
	cmp			eax, 0
	JNS			PostSetNegative2
	
	;if so we flip the number to positive
	MOV			EBX, -1
	IMUL		EBX
PostSetNegative2:
	;The positive(if needed) version of the number is in the eax

; ----------------------------------------------------
; In this loop we divide the number in the eax by 10, convert its 
;	remainder into an ascii digit, and append that digit to the location the 
;	edi is pointing at, we do this until we our out of digits
; ----------------------------------------------------

	STD
stringNumberLoop:
	
	mov			EDX, 0						;clear edx for dividing
	MOV			EBX, 10						;divisor is 10
	DIV			EBX							;divide
	MOV			EBX, EAX					;save the eax number we are dividing to work with al
	MOV			AL, DL						;move remainder digit in dl into the al
	ADD			AL, 48						;Convert number to ascii coressponding digit
	STOSB									;Store digit in al in string and decrement pointer
	MOV			EAX, EBX					;restore eax number we are dividing
	CMP			EAX, 0						;check if its time to exit the loop
	JE			stringLoopExit				;if so do it
	jmp			stringNumberLoop			;if not, repeat the loop until eax is 0

stringLoopExit:

	mDisplayString	[EBP+8]				;Display the string we have created with the passed number

; We pass the original offest of the string [ebp+8 ] to the mDisplayString Macro

	POP			EDI
	POP			EAX
	POP			EDX
	POP			ESI
	POP			ECX
	POP			EBX
	POP			EBP
	RET			12							;3 parameters
WriteVal ENDP

; ---------------------------------------------------------------------------------
; Name: ReadVal
;
; Reads a string from the user. Then converts that string into a number and stores that number.
; Accomplishes this by first getting a user input string using the mGetString macro.
; mGetString puts the users string in inputString and returns the amount of characters produced.
; We put that amount in the ecx for counting. Next we determine if our number is positive or
; negative by using LODSB to examine the first byte of the user string. If it is a plus or minus
; we set our edx(which we are using as a boolean) to 1 or 0 to signify if the number is negative.
; We also lower the ecx to signify that the + or - should not be counted as a digit. If there is 
; no sign, we increment the ESI back to the beginning of the user string because we want it in the
; decoding loop. In the decoding loop, we analyze the digits loaded into the al by STOSB. If they 
; are not digits, we call give an invalid message and reset the input. If They are valid, turn it
; into a number by subtracting 48 from its ascii value. We add these digits to our numberHolder.
; Each time we add a digit to this numberHolder, we "move the previous digit over" one slot to 
; the left by multiplying it by 10. If the number we are producing overflows the register,
; it is invalid and we reset input. (unless the number overflowing it is 2147483648 and the
; negative Boolean is set, if this is the case we manually put in -2147483648). Once the ecx is 0
; we have fully decoded the absolute value of the number. If the number is supposed to by negative
; we multiply the number in the numberHolder by -1 and we are done. If not negative, we are done
; and the procedure exits with the derived number in [ebp+24].
;
; Preconditions: [ebp+12] string size is large enough for the user input
;				 [ebp+28 and epb+20] point to valid strings
;				 [ebp+16] large enough to hold the users string
;
; Postconditions: none.
;
; [ebp+28] = errorMessage = Holds message for when invalid string entered
; [ebp+24] = numberHolder = Destination address for the number we derive from input string
; [ebp+20] = inputPrompt = The prompt which asks the user to input a number
; [ebp+16] = inputString = The location of which will hold the users input string
; [ebp+12] = stringSize = The size of the string the input will accept from the user
; [ebp+8] = instring bytes = The amount of bytes the users string used in the string array
;
; returns: Number derived from Ascii characters in [ebp+16] put in variable at [ebp+24]
;			variable at [ebp+8] holds the amount of bytes the user input
; ---------------------------------------------------------------------------------
ReadVal PROC

	PUSH		EBP
	MOV			EBP, ESP
	push		EBX
	push		ecx
	push		esi
	push		edx
	push		eax
	push		edi

_invalidString:
	mGetString		[EBP+20], [EBP+16], [EBP+12], [EBP+8]	;String in addr [ebp+16], count in [[ebp+8]]
	MOV		EDI, [EBP+8]									;Put the byte counter address in edi
	mov		ecx, [EDI]										;Prime the primtive string counter for looping over characters
	mov		esi, [ebp+16]									;Put first address of string into the esi
	
	;Clear out old value in DWORD value holding parameter [ebp+24]
	mov		ebx, 0
	mov		edi, [EBP+24]
	mov		[edi], ebx										;Clear out the old  value 

	;We need to evaluate the first byte of the inString,
	mov		edx, 0											;pseudo boolean, if 0 output positive, if 1 negative
	CLD
	LODSB													;First character of user string in the al, esi incremented

	; If its a + we increment the string pointer, leave sign at boolean 0, and decrement ecx
	cmp		al, 43
	JNE		_postPlusCheck									;if not positive, we go see if negative
	dec		ecx												;dec so that we have the amount of digits in ecx (subtract + from count)
	JMP		_decodeLoop										;after dealing with the + we continue to the loop
_postPlusCheck:

	; If its a - we increment the string pointer and set the sign holder to 1(negative) and decrement ecx
	cmp		al, 45
	JNE		postMinusCheck									;if its not a - either, we continue to decrement esi and loop
	mov		edx, 1											;set "sign boolean" to 1 (negative)
	dec		ecx												;dec so that we have the amount of digits in ecx (subtract - from count)
	jmp		_decodeLoop
postMinusCheck:
	; If its neither we move the esi back so its pointing at the first number again
	dec		esi		

; ----------------------------------------------------
; Loop uses string primitives to get value from the string. We check if string is in number range,
;	if it is not in the number range, we print invalid input and go all the way back to input(invalid input label)
;	if it is valid we "numberize" it (subtract 48) and add it to the previous total multiplied by 10.
;	this repeats until ecx loops to 0 and we have not encountered invalid input
; ----------------------------------------------------


_decodeLoop:
	LODSB												;First Byte of the esi in the AL, valid char range is 48-57
	CMP		AL, 48
	JL		_invalidMessage
	CMP		AL,	57
	JG		_invalidMessage
	
	;Our value is a valid digit, we must put it into the dword ( [[EBP+24]] ) variable...
	mov		BL,	al										;Store our value in the bl
	sub		BL, 48										;Set our byte to correct number
	MOV		EDI, [EBP+24]
	MOV		EAX, [EDI]									;We move decoding number into the eax so we can add new digits and return it
	PUSH	EDX											;We need three registers, store negative boolean and continue

	;Multiply old DWORD value by 10 to signify moving decimal place
	MOV		EDX, 10										;We multiply the decoding value by 10 to make room for the next digit
	iMUL	EDX											;Proper value in eax, imul will set off the overflow if O-F occurs
	jno		postOfCheck1								;if overflow by multiplication
	pop		edx											;restore edx and try again
	jmp		_invalidMessage
postOfCheck1:

	movzx	EDX, BL										;Sign extend the BL so we can add it
	ADD		EAX, EDX									;Add to the eax the next digit
	
	;check if were overflow, if so check if were on last iteration and negative, if 2147483648 edge case label
	jno		postOfCheck2
	pop		edx											;restore edx before jumping back to reset or edge case
	cmp		ecx, 1										;Check if we are on the last digit
	JNE		_invalidMessage								;if not just invalid, retry
	cmp		EDX, 1										;Check if we are dealing with a negative
	JNE		_invalidMessage								;if not invalid, retry
	CMP		EAX, 2147483648								;detects single negative edge that causes overflow, jump to special instructions to handle it
	JE		NegativeEdgeCase

	jmp		_invalidMessage
postOfCheck2:

	POP		EDX											;restore the edx (negative boolean)
	MOV		EDI, [EBP+24]								;put paramter holding accumlating number back in edi
	MOV		[EDI], EAX									;Store the current iteration of the accumulating, decoding number
	LOOP	_decodeLoop
	JMP		_postInvalidMessage

	;if invalid, jump to this codeblock, print invalid message, jump to input macro to try again
_invalidMessage:
	mDisplayString	[EBP+28]							
	CALL	crlf
	JMP		_invalidString
_postInvalidMessage:

	;we convert the string to its negative value if the edx is set to 1
	CMP		EDX, 1
	JE		_isNegative
	JMP		_postIsNegative

	;Get the number out of the number holding paremeter, and set it to negative, then put it back
_isNegative:
	mov		EDI, [EBP+24]
	MOV		EAX, [EDI]
	MOV		EBX, -1
	IMUL	EBX
	MOV		[EDI], EAX
_postIsNegative:
	jmp		PostNegativeEdgeCase					;jump over edge case code

	;it occurs to me now that I could have just put -2147483648 in the eax, but this is more fun.
NegativeEdgeCase:
	mov		eax, 2147483647							;if we have -2147483648 we derive it from
	mov		ebx, -1									;multiplying its postive -1 by -1 and 
	imul	ebx
	dec		EAX										;decrementing to rep of -2147483648
	mov		EDI, [EBP+24]
	MOV		[EDI], EAX								;Store number in correct location
PostNegativeEdgeCase:

	;Number derived from string in data label (or whatever [ebp+24] is)

	pop			edi
	pop			eax
	pop			edx
	pop			esi
	pop			ecx
	pop			EBX
	POP			EBP

	RET			24						; 6 parameterS
ReadVal ENDP

END MAIN

