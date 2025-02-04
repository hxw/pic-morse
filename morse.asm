  title "morse.asm Display Morse code"

  ifdef __16C71
    include "p16c71.inc"
  elifdef __16C84
    include "p16c84.inc"
  elifdef __16F84
    include "p16f84.inc"
  else
    messg "only supports PIC 16C71 nad 16[CF]84"
  endif

  __fuses _HS_OSC & _WDT_OFF & _CP_ON

  list
  expand
  radix dec
  assume 0                      ; assume in bank 0

;;; timing parameters
;;; system clock = 4MHz  16MHz  20MHz
;;; cycle time   = 1uS  250ns  200nS
CPU_ClockMHz equ .16


;;; for 100uS delay
;;; delay 7+3n cycles (including call)

  if CPU_ClockMHz == .4
Time_OneHundred equ .31         ; 4MHz
  elif CPU_ClockMHz == .16
Time_OneHundred equ .131        ; 16MHz
  elif CPU_ClockMHz == .20
Time_OneHundred equ .164        ; 20MHz
  else
    messg "unsupported clock frequency"
  endif

;;; debugging
Debug equ .0                    ; set to .1 for debug prints


;;; the default Morse speed
DefaultDit equ .60              ; 60 ms = 20 WPM (based on PARIS timing)
DefaultDah equ .3 * DefaultDit  ; - = 3 * .
DefaultTime equ (DefaultDit + DefaultDah) / .2
DefaultCharacterTime equ .3 * DefaultDit
DefaultWordTime equ .7 * DefaultDit
MaxDitTime equ .240             ; so we do not go out of range (5 WPM)
MaxDahTime equ .3 * MaxDitTime  ; ..
debounce equ .10                ; milliseconds

;;; PORTA bits

MorseIn equ .0                  ; RA.0 = Morse code input

;;; PORTB bits

LCD_E equ .7                    ; RB.7 = LCD Enable
LCD_RW equ .6                   ; RB.6 = LCD R/W
LCD_RS equ .5                   ; RB.5 = LCD register select
LCD_BackLight equ .4            ; RB.4 = LCD back light 1=>on, 0=>off
LCD_BusyBit equ .7              ; msb = LCD busy bit
LCD_DataMask equ 0x0f           ; RB.0 .. RB.3 = LCD data bus
LCD_DataInput equ b'00001111'
LCD_DataOutput equ b'00000000'

;;; LCD commands

LCD_PowerOn equ 0x33            ; Power on configuration
LCD_4BitMode1 equ 0x22          ; configure to 4 bit bus
LCD_4BitMode2 equ 0x28          ; configure to 4 bit bus (2 lines)
LCD_ClearScreen equ 0x01        ; erase all display
LCD_home equ 0x02               ; home cursor
LCD_increment equ 0x06          ; entry mode
LCD_off equ 0x08                ; Disable display
LCD_on equ 0x0e                 ; Enable display
LCD_CursorAddress equ 0x80      ; cursor address set
LCD_CursorMask equ 0x7f         ; ..

;;; LCD size

LCD_Width equ .20               ; columns
LCD_Height equ .4               ; lines

LCD_line_1 equ 0x00             ; offsets into LCD memory
LCD_line_2 equ 0x40             ; ..
LCD_line_3 equ 0x14             ; ..
LCD_line_4 equ 0x54             ; ..

;;; registers

;;;  org 0x0c
  udata 0x0c

temp res 1                      ; used by LCD Send routines
TempNibble res 1                ; used by LCD Send routines
DelayCount1 res 1               ; Delay routine
DelayCount2 res 1               ; Delay routine
PulseTime res 2                 ; counter for Morse code pulse length
AverageTime res 2               ; (dit+dah)/2
WordSeparatorTime res 2         ; words
CharSeparatorTime res 2         ; characters
DitTime res 2                   ; for autocalibration
DahTime res 2                   ; for autocalibration
morse res 2                     ; .=01, -=10

TempByte res 1                  ; DEBUGGING

cursor res 1                    ; soft cursor
ReadCursor res 1                ; soft cursor
counter res 1                   ; for scrolling
read res 1                      ; last char read from LCD

;;; general constants

BitsPerByte equ .8              ; number of bits in one byte
ByteMSB equ .7                  ; position of MSB in a byte
ByteLSB equ .0                  ; position of LSB in a byte


;;;* Reset and Interrupt vectors

reset code 0x0000             ; reset vector
;;;  pagesel main                ;  ; ...
  goto main                     ; ...


INTR71 code 0x0004              ; interrupt vector
  retfie                        ; just return from interrupt


  code

;;;* Set the cursor position
;;;*
;;;* Inputs
;;;*   W = cursor address
;;;* Outputs
;;;*   none

LCD_SetCursor
  andlw LCD_CursorMask          ; cursor position
  addlw -LCD_Width
  bc LCD_SetCursor_l2
LCD_SetCursor_l1
  addlw LCD_Width + LCD_line_1
  goto LCD_SetCursor_exec
LCD_SetCursor_l2
  addlw -LCD_Width
  bc LCD_SetCursor_l3
  addlw LCD_Width + LCD_line_2
  goto LCD_SetCursor_exec
LCD_SetCursor_l3
  addlw -LCD_Width
  bc LCD_SetCursor_l4
  addlw LCD_Width + LCD_line_3
  goto LCD_SetCursor_exec
LCD_SetCursor_l4
  addlw LCD_line_4

LCD_SetCursor_exec
   iorlw LCD_CursorAddress      ; cursor position

;;;* Transmit a byte to the LCD control register
;;;*
;;;* Inputs
;;;*   W = byte to send
;;;* Outputs
;;;*   none

LCD_SendControl

  movwf temp                    ; save W
  call LCD_WaitReady
  swapf temp,W                  ; get high nibble
  call LCD_SendControlNibble
  movfw temp                    ; get low nibble

LCD_SendControlNibble

  bcf PORTB,LCD_E               ; enable off
  bcf PORTB,LCD_RS              ; command register
  bcf PORTB,LCD_RW              ; write

SendToLCD
  movwf TempNibble              ; copy W low nibble to Port B low nibble

  bcf PORTB,0
  btfsc TempNibble,0
  bsf PORTB,0

  bcf PORTB,1
  btfsc TempNibble,1
  bsf PORTB,1

  bcf PORTB,2
  btfsc TempNibble,2
  bsf PORTB,2

  bcf PORTB,3
  btfsc TempNibble,3
  bsf PORTB,3

  nop
  nop
  bsf PORTB,LCD_E               ; enable on
  nop
  nop
  nop
  nop
  nop
  bcf PORTB,LCD_E               ; enable off

  retlw 0

;;;* Transmit a byte to the LCD data register
;;;*
;;;* Inputs
;;;*   W = byte to send
;;;* Outputs
;;;*   none

LCD_SendChar
  call LCD_SendData

  call LCD_WaitReady

  incf cursor,W
  xorlw LCD_Height * LCD_Width
  bz LCD_Scroll
  incf cursor,F
  movfw cursor
  goto LCD_SetCursor

LCD_SendData
  movwf temp                    ; save W
  call LCD_WaitReady

  swapf temp,W                  ; get high nibble
  call LCD_SendDataNibble
  movfw temp                    ; get low nibble

LCD_SendDataNibble
  bcf PORTB,LCD_E               ; enable off
  bsf PORTB,LCD_RS              ; data register
  bcf PORTB,LCD_RW              ; write
  goto SendToLCD


;;;* Wait LCD ready
;;;*
;;;* Inputs
;;;*   none
;;;* Outputs
;;;*   none

LCD_WaitReady

  bsf PORTB,0
  bsf PORTB,1
  bsf PORTB,2
  bsf PORTB,3

  banksel TRISB                 ; select page 1
  movlw LCD_DataInput
  movwf TRISB
  banksel PORTB                 ; select page 0

LCD_WaitReadyLoop

  bcf PORTB,LCD_E               ; enable off
  bcf PORTB,LCD_RS              ; command register
  bsf PORTB,LCD_RW              ; read

  nop
  nop
  bsf PORTB,LCD_E               ; enable on
  nop
  nop
  movfw PORTB                   ; high nibble
  bcf PORTB,LCD_E               ; enable off

  andlw LCD_DataMask
  movwf TempNibble
  swapf TempNibble,F            ; save high nibble

  bsf PORTB,LCD_E               ; enable on
  nop
  nop
  movfw PORTB                   ; low nibble
  bcf PORTB,LCD_E               ; enable off
  bcf PORTB,LCD_RW              ; write

  andlw LCD_DataMask
  iorwf TempNibble,F            ; temp = 8 bits read
  btfsc TempNibble,LCD_BusyBit  ; busy is active high
  goto LCD_WaitReadyLoop

  banksel TRISB                 ; select page 1
  movlw LCD_DataOutput
  movwf TRISB
  banksel PORTB                 ; select page 0

  retlw 0


;;;* Read LCD character
;;;*
;;;* Inputs
;;;*   W = cursor
;;;* Outputs
;;;*   W = read = character from LCD

LCD_ReadChar

  call LCD_SetCursor
  call LCD_WaitReady

  bsf PORTB,0
  bsf PORTB,1
  bsf PORTB,2
  bsf PORTB,3

  banksel TRISB                 ; select page 1

  movlw LCD_DataInput
  movwf TRISB

  banksel PORTB                 ; select page 0

  bcf PORTB,LCD_E               ; enable off
  bsf PORTB,LCD_RS              ; data register
  bsf PORTB,LCD_RW              ; read

  nop
  nop
  bsf PORTB,LCD_E               ; enable on
  nop
  nop
  movfw PORTB                   ; high nibble
  bcf PORTB,LCD_E               ; enable off

  andlw LCD_DataMask
  movwf read
  swapf read,F                  ; save high nibble

  bsf PORTB,LCD_E               ; enable on
  nop
  nop
  movfw PORTB                   ; low nibble
  bcf PORTB,LCD_E               ; enable off
  bcf PORTB,LCD_RW              ; write

  andlw LCD_DataMask
  iorwf read,F                  ; temp = 8 bits read

  banksel TRISB                 ; select page 1

  movlw LCD_DataOutput
  movwf TRISB

  banksel PORTB                 ; select page 0

  movfw read                    ; the character read
  return


;;;* Scroll LCD
;;;*
;;;* Inputs
;;;*   none
;;;* Outputs
;;;*   none

LCD_Scroll
  movlw LCD_Width * (LCD_Height - 1)
  movwf counter                 ; all but top line
  movlw LCD_Width               ; start of line 2
  movwf ReadCursor

LCD_Scroll_loop
  movfw ReadCursor
  call LCD_ReadChar
  movfw ReadCursor
  addlw -LCD_Width
  call LCD_SetCursor
  movfw read
  call LCD_SendData
  incf ReadCursor,F
  decfsz counter,F
  goto LCD_Scroll_loop

  movlw LCD_Width               ; start of line 2
  movwf counter
  movlw LCD_Width * (LCD_Height - 1)
  call LCD_SetCursor
LCD_Scroll_blank
  movlw ' '
  call LCD_SendData
  decfsz counter,F
  goto LCD_Scroll_blank

  movlw LCD_Width * (LCD_Height - 1)
  movwf cursor
  goto LCD_SetCursor


;;;* Initialise LCD
;;;*
;;;* Inputs
;;;*   none
;;;* Outputs
;;;*   none

LCD_initialise

  movlw .250                    ; milliseconds
  call Delay

  movlw LCD_PowerOn             ; Power on configuration
  call LCD_SendControlNibble

  movlw .10                     ; milliseconds
  call Delay

  movlw LCD_PowerOn             ; Power on configuration
  call LCD_SendControlNibble

  movlw .5                      ; milliseconds
  call Delay

  movlw LCD_PowerOn             ; Power on configuration
  call LCD_SendControlNibble

  call LCD_WaitReady

  movlw LCD_4BitMode1
  call LCD_SendControlNibble

  movlw LCD_4BitMode2
  call LCD_SendControl
  movlw LCD_off
  call LCD_SendControl
  movlw LCD_ClearScreen
  call LCD_SendControl
  movlw LCD_home
  call LCD_SendControl
  movlw LCD_increment
  call LCD_SendControl
  movlw LCD_on
  call LCD_SendControl

  call LCD_WaitReady

  clrf cursor                   ; soft cuiror to Home
  retlw 0


;;;* Timing delays
;;;*
;;;* Inputs
;;;*   W = milliseconds to delay
;;;* Outputs
;;;*   none

Delay
  movwf DelayCount1
DelayLoop
  call DelayOneMillisecond
  decfsz DelayCount1,F          ; (1/2)
  goto DelayLoop                ; (2)
  retlw 0


DelayOneMillisecond             ; delay 7+3n cycles (including call)
  movlw Time_OneHundred         ; (1) 100uS  1
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  2
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  3
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  4
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  5
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  6
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  7
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  8
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS  9
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS 10

DelayMicro
  movwf DelayCount2             ; (1)
DelayMicroLoop
  decfsz DelayCount2,F          ; (1/2)
  goto DelayMicroLoop           ; (2)
  retlw 0                       ; (2)


;;;* Print one byte in hex
;;;*
;;;* Inputs
;;;*   W = byte to print
;;;* Outputs
;;;*   none

Hex_PrintByte
  movwf TempByte                ; byte to output
  swapf TempByte,W              ; high nibble

  call Hex_PrintNibble

  movfw TempByte                ; low nibble

Hex_PrintNibble
  call Hex_table
  call LCD_SendChar
  retlw 0


Hex_table
  andlw 0x0f                    ; extract nibble
  addwf PCL,F                   ; computed goto
  retlw '0'
  retlw '1'
  retlw '2'
  retlw '3'
  retlw '4'
  retlw '5'
  retlw '6'
  retlw '7'
  retlw '8'
  retlw '9'
  retlw 'a'
  retlw 'b'
  retlw 'c'
  retlw 'd'
  retlw 'e'
  retlw 'f'
Hex_TableEnd
  if ((Hex_table & 0xff) >= (Hex_TableEnd & 0xff))
    messg "Warning - Hex_table crosses page boundary"
  endif


;;;* Macro to compare a 16 bit value
;;;*
;;;* Inputs
;;;*   value1 = first byte of two byte value (big endian)
;;;*   value2 = first byte of two byte value (big endian)
;;;* Outputs
;;;*   W = used
;;;*   less = branch to here if value1 < value2
;;;*   greater = branch to here if value1 > value2
;;;*   . = execute in-line if values are equal

Compare16 macro _value1_,_value2_,_less_,_greater_
  movfw _value1_+0
  subwf _value2_+0,W            ; High:  value2 - value1
  bnc _greater_
  bnz _less_
  movfw _value1_+1
  subwf _value2_+1,W            ; Low:   value2 - value1
  bnc _greater_
  bnz _less_
  endm


;;;* Macro to compare a 16 bit value
;;;*
;;;* Inputs
;;;*   value = first byte of two byte value (big endian)
;;;*   lit = 16 bit constant value
;;;* Outputs
;;;*   W = used
;;;*   less = branch to here if value1 < value2
;;;*   greater = branch to here if value1 > value2
;;;*   . = execute in-line if values are equal

CompareLit16 macro _value_,_lit_,_less_,_greater_
  movfw _value_+0
  sublw (_lit_ >> .8) & 0xff    ; High:  value2 - value1
  bnc _greater_
  bnz _less_
  movfw _value_+1
  sublw _lit_ & 0xff            ; Low:   value2 - value1
  bnc _greater_
  bnz _less_
  endm


;;;* Macro to compare a 16 bit Morse value value for equality
;;;*
;;;* Inputs
;;;*   value = first byte of two byte value (big endian)
;;;*   lit = 16 bit constant value
;;;*   code = 8 bit value to return
;;;* Outputs
;;;*   W = retlw code if equal
;;;*   . = execute in-line if values are not equal

Morse16 macro _value_,_lit_,_code_
  local done
  movfw _value_+0
  sublw (_lit_ >> .8) & 0xff    ; High:  value2 - value1
  bnz done
  movfw _value_+1
  sublw _lit_ & 0xff            ; Low:   value2 - value1
  bnz done
  retlw _code_
done
  endm


;;;* Macro to add a Morse token to 16 bit Morse
;;;*
;;;* Inputs
;;;*   value = first byte of two byte value (big endian)
;;;* Outputs
;;;*   W = retlw code if equal

Dah16 macro _value_
  setc
  rlf _value_+1,F
  rlf _value_+0,F
  clrc
  rlf _value_+1,F
  rlf _value_+0,F
  endm


;;;* Macro to add a Morse token to 16 bit Morse
;;;*
;;;* Inputs
;;;*   value = first byte of two byte value (big endian)
;;;* Outputs
;;;*   W = retlw code if equal

Dit16 macro _value_
  clrc
  rlf _value_+1,F
  rlf _value_+0,F
  setc
  rlf _value_+1,F
  rlf _value_+0,F
  endm


;;;* Macro to increment a 16 bit value
;;;*
;;;* Inputs
;;;*   value = first byte of two byte value (big endian) to increment
;;;* Outputs
;;;*   value = value + 1

Increment16 macro _value_
  local done
  incfsz _value_+1,F            ; low byte
  goto done
  incfsz _value_+0,F            ; high byte
  goto done
  movlw 0xff
  movwf _value_+0               ; prevent wrap to zero
  movwf _value_+1
done
  endm


;;;* Macro to initialise a 16 bit value
;;;*
;;;* Inputs
;;;*   init = constant 16 bit number
;;;* Outputs
;;;*   W = used
;;;*   value = first byte of two byte value (big endian) to init

Set16 macro _value_,_init_
  movlw (_init_ >> .8) & 0xff
  movwf _value_+0               ; high byte
  movlw _init_ & 0xff
  movwf _value_+1               ; low byte
  endm


;;;* Macro to copy a 16 bit value
;;;*
;;;* Inputs
;;;*   source = first byte of two byte value (big endian) used as input value
;;;* Outputs
;;;*   W = used
;;;*   dest = first byte of two byte value (big endian)

Move16 macro _dest_,_source_
  movfw _source_+0
  movwf _dest_+0                ; high byte
  movfw _source_+1
  movwf _dest_+1                ; low byte
  endm


;;;* Macro to add a 16 bit value
;;;*
;;;* Inputs
;;;*   source = first byte of two byte value (big endian) used as input value
;;;* Outputs
;;;*   W = used
;;;*   dest = first byte of two byte value (big endian) dest += source

Add16 macro _dest_,_source_
  local nocarry
  movfw _source_+1              ; low byte
  addwf _dest_+1,F              ; low byte
  movfw _source_+0              ; high byte
  btfsc STATUS,C
  incf _source_+0,W             ; high byte + 1
  addwf _dest_+0,F              ; high byte
  endm


;;;* Macro to Shift Right a 16 bit unsigned value
;;;*
;;;* Inputs
;;;*   value = first byte of two byte value (big endian)
;;;* Outputs
;;;*   value <<= 1 (unsigned)

Shr16 macro _value_
  clrc
  rrf _value_+1
  rrf _value_+0
  endm


;;;* Morse to ASCII
;;;*
;;;* Inputs
;;;*   morse = 16 bit morse value
;;;* Outputs
;;;*   W = ASCII value

MorseToASCII                    ; .=01 -=10
  Morse16 morse,b'0110','A'
  Morse16 morse,b'10010101','B'
  Morse16 morse,b'10011001','C'
  Morse16 morse,b'100101','D'
  Morse16 morse,b'01','E'
  Morse16 morse,b'01011001','F'
  Morse16 morse,b'101001','G'
  Morse16 morse,b'01010101','H'
  Morse16 morse,b'0101','I'
  Morse16 morse,b'01101010','J'
  Morse16 morse,b'100110','K'
  Morse16 morse,b'01100101','L'
  Morse16 morse,b'1010','M'
  Morse16 morse,b'1001','N'
  Morse16 morse,b'101010','O'
  Morse16 morse,b'01101001','P'
  Morse16 morse,b'10100110','Q'
  Morse16 morse,b'011001','R'
  Morse16 morse,b'010101','S'
  Morse16 morse,b'10','T'
  Morse16 morse,b'010110','U'
  Morse16 morse,b'01010110','V'
  Morse16 morse,b'011010','W'
  Morse16 morse,b'10010110','X'
  Morse16 morse,b'10011010','Y'
  Morse16 morse,b'10100101','Z'
  Morse16 morse,b'1010101010','0'
  Morse16 morse,b'0110101010','1'
  Morse16 morse,b'0101101010','2'
  Morse16 morse,b'0101011010','3'
  Morse16 morse,b'0101010110','4'
  Morse16 morse,b'0101010101','5'
  Morse16 morse,b'1001010101','6'
  Morse16 morse,b'1010010101','7'
  Morse16 morse,b'1010100101','8'
  Morse16 morse,b'1010101001','9'
  Morse16 morse,b'0110011001','+'
  Morse16 morse,b'100101010110','-'
  Morse16 morse,b'011001100110','.'
  Morse16 morse,b'101001011010',','
  Morse16 morse,b'1001011001','/'
  Morse16 morse,b'010110100101','?'
  Morse16 morse,b'0110010101','&'
  Morse16 morse,b'011010101001','\''
  Morse16 morse,b'011010011001','@'
  Morse16 morse,b'100110100110','('
  Morse16 morse,b'1001101001',')'
  Morse16 morse,b'101010010101',':'
  Morse16 morse,b'1001010110','='
  Morse16 morse,b'100110011010','!'
  Morse16 morse,b'011001011001','\"'
  retlw '~'                     ; unassigned code


;;;* Main program
;;;*
;;;* Inputs
;;;*   none
;;;* Outputs
;;;*   none

main

  banksel temp                  ; select bank 0

  clrw                          ; set W = 0
;;;  clrf FlagBits              ; reset all system flags

  Set16 AverageTime,DefaultTime ; set inital rate
  Set16 WordSeparatorTime,DefaultWordTime
  Set16 CharSeparatorTime,DefaultCharacterTime
  Set16 morse,0                 ; clear the morse accumulator

  banksel TRISA                 ; select bank 1

  movlw b'11111111'             ; Port A as all input
  movwf TRISA

  ifdef __16C71
  banksel ADCON1
  movlw b'00000011'             ; Port A as digital input
  movwf ADCON1
  endif

  movlw LCD_DataOutput          ; configure Port B
  movwf TRISB

  banksel temp                  ; select bank 0

  clrf PORTB                    ; set all Port B low

  call LCD_initialise

;;; print a title

  movlw 'M'
  call LCD_SendChar
  movlw 'o'
  call LCD_SendChar
  movlw 'r'
  call LCD_SendChar
  movlw 's'
  call LCD_SendChar
  movlw 'e'
  call LCD_SendChar
  movlw ' '
  call LCD_SendChar

  bsf PORTB,LCD_BackLight       ; backlight on


;;; wait for synchronisation assuming an active low input

;; WaitPulse
;; WaitLow
;;   btfsc PORTA,MorseIn
;;   goto WaitLow
;;   call DelayOneMillisecond
;;   btfsc PORTA,MorseIn
;;   goto WaitLow

;;   Set16 PulseTime,1
;; WaitHigh
;;   call DelayOneMillisecond
;;   Increment16 PulseTime
;;   btfss PORTA,MorseIn
;;   goto WaitHigh


WaitPulse
WaitLow
  btfsc PORTA,MorseIn           ; active low input
  goto WaitLow                  ; ..
  movlw debounce                ; debounce time
  call Delay                    ; ..
  btfsc PORTA,MorseIn
  goto WaitLow

  Set16 PulseTime,debounce

WaitHigh
  call DelayOneMillisecond
  Increment16 PulseTime
  btfss PORTA,MorseIn
  goto WaitHigh

  Compare16 PulseTime,AverageTime,dot,dash

dash
  Move16 DahTime,PulseTime
  CompareLit16 DahTime,MaxDahTime,PrintDash,RestrictDah
RestrictDah
  Set16 DahTime,MaxDahTime
PrintDash
  Dah16 morse
  movlw '-'
  goto PrintChar

dot
  Move16 DitTime,PulseTime
  CompareLit16 DitTime,MaxDitTime,PrintDot,RestrictDit
RestrictDit
  Set16 DitTime,MaxDitTime
PrintDot
  Dit16 morse
  movlw '.'

PrintChar                       ; display '.' or '-'
  call LCD_SendChar

  if Debug                      ; display the pulse time
  movfw PulseTime+0
  call Hex_PrintByte
  movfw PulseTime+1
  call Hex_PrintByte
  endif

;;;Recalibrate
;;;  Move16 PulseTime,DitTime
;;;  Add16 PulseTime, DahTime      ; dit * 4
;;;  Shr16 PulseTime
;;;  Move16 AverageTime,PulseTime  ; dit * 2
;;;  Shr16 PulseTime               ; dit * 1
;;;  Move16 CharSeparatorTime,PulseTime
;;;  Add16 CharSeparatorTime,AverageTime; dit * 3
;;;  Move16 WordSeparatorTime,CharSeparatorTime
;;;  Add16 WordSeparatorTime,AverageTime; dit * 7

  movlw debounce                ; debounce return to high
  call Delay
  Set16 PulseTime,debounce
CheckSpaceTime
  call DelayOneMillisecond
  Increment16 PulseTime
  Compare16 PulseTime,WordSeparatorTime,CheckSpaceTime_ok,PrintWordSeparator

CheckSpaceTime_ok
  btfsc PORTA,MorseIn           ; check if key goes down again
  goto CheckSpaceTime           ; key is still released

  Compare16 PulseTime,CharSeparatorTime,WaitPulse,PrintCharSeparator

PrintCharSeparator              ; just output the character
  call MorseToASCII
  call LCD_SendChar
  Set16 morse,0
  goto WaitPulse

PrintWordSeparator              ; output the character followed by space
  call MorseToASCII
  call LCD_SendChar
  movlw ' '
  call LCD_SendChar
  Set16 morse,0
  goto WaitPulse


;;; hold backlight on for a time

BackLightHold
  movlw .30*.4
  movfw temp
BackLightHold1
  movlw .250                    ; 1/4 second delay
  call Delay
  decfsz temp,F
  goto BackLightHold1
  bcf PORTB,LCD_BackLight       ; backlight off

;;; lock up - reset to initiate next cycle

DynamicStop
  goto DynamicStop

  end
