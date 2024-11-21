  title "serial.asm Display Inverter Security Code Number"

  list p=16c71

  include "p16c71.inc"

  __fuses _HS_OSC & _WDT_OFF & _CP_ON

; timing parameters
; system clock = 4MHz  16MHz  20MHz
; cycle time   = 1uS  250ns  200nS
CPU_ClockMHz equ 16


; for 100uS delay
; delay 7+3n cycles (including call)

  if CPU_ClockMHz == 4
Time_OneHundred equ 31  ; 4MHz
  else
  if CPU_ClockMHz == 16
Time_OneHundred equ 131 ; 16MHz
  else
  if CPU_ClockMHz == 20
Time_OneHundred equ 164 ; 20MHz
  else
    messg unsupported clock frequency
  endif
  endif
  endif


;the default Morse speed
;DefaultDit = 120
DefaultDit = 60                 ; 20 WPM
DefaultDah = 3 * DefaultDit
DefaultTime equ (DefaultDit + DefaultDah) / 2
DefaultCharacterTime = 3 * DefaultDit
DefaultWordTime = 5 * DefaultDit
MaxDitTime = 240                ; so we do not go out of range (5 WPM)
MaxDahTime = 3 * MaxDitTime

; PORTA bits

MorseIn equ .0                  ; RA.0 = Morse code input

; PORTB bits

LCD_E equ .7                    ; RB.7 = LCD Enable
LCD_RW equ .6                   ; RB.6 = LCD R/W
LCD_RS equ .5                   ; RB.5 = LCD register select
LCD_BackLight equ .4            ; RB.4 = LCD back ligkt 1=>on, 0=>off
LCD_BusyBit equ .7              ; msb = LCD busy bit
LCD_DataMask equ 0x0f           ; RB.0 .. RB.3 = LCD data bus
LCD_DataInput equ b'00001111'
LCD_DataOutput equ b'00000000'

; LCD commands

LCD_PowerOn equ 0x33            ; Power on configuration
LCD_4BitMode1 equ 0x22          ; configure to 4 bit bus
LCD_4BitMode2 equ 0x28          ; configure to 4 bit bus (2 lines)
LCD_ClearScreen equ 0x01        ; erase all display
LCD_home equ 0x02               ; home cursor
LCD_increment equ 0x06          ; entry mode
LCD_off equ 0x08                ; Disable display
LCD_on equ 0x0e                 ; Enable display


; registers

  org h'0c'

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
; general constants

BitsPerByte equ .8              ; number of bits in one byte
ByteMSB equ .7                  ; position of MSB in a byte
ByteLSB equ .0                  ; position of LSB in a byte


;* Reset and Interrupt vectors

  org 0                         ; reset vector
  goto main                     ; ...

INTR71 equ 4
  org INTR71                    ; interrupt vector
  retfie                        ; just return from interrupt


;* Transmit a byte to the LCD control register
;*
;* Inputs
;*   W = byte to send
;* Outputs
;*   none

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

;* Transmit a byte to the LCD data register
;*
;* Inputs
;*   W = byte to send
;* Outputs
;*   none

LCD_SendChar

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


;* Wait LCD ready
;*
;* Inputs
;*   none
;* Outputs
;*   none

LCD_WaitReady

  bsf PORTB,0
  bsf PORTB,1
  bsf PORTB,2
  bsf PORTB,3

  bsf STATUS,RP0                ; select page 1
  movlw LCD_DataInput
  movwf TRISB
  bcf STATUS,RP0                ; select page 0

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
  swapf TempNibble              ; save high nibble

  bsf PORTB,LCD_E               ; enable on
  nop
  nop
  movfw PORTB                   ; low nibble
  bcf PORTB,LCD_E               ; enable off
  bcf PORTB,LCD_RW              ; write

  andlw LCD_DataMask
  iorwf TempNibble              ; temp = 8 bits read
  btfsc TempNibble,LCD_BusyBit; busy is active high
  goto LCD_WaitReadyLoop

  bsf STATUS,RP0                ; select page 1
  movlw LCD_DataOutput
  movwf TRISB
  bcf STATUS,RP0                ; select page 0

  retlw 0


;* Initialise LCD
;*
;* Inputs
;*   none
;* Outputs
;*   none

LCD_initialise

  movlw 250                     ; milliseconds
  call Delay

  movlw LCD_PowerOn             ; Power on configuration
  call LCD_SendControlNibble

  movlw 10                      ; milliseconds
  call Delay

  movlw LCD_PowerOn             ; Power on configuration
  call LCD_SendControlNibble

  movlw 5                       ; milliseconds
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

  retlw 0


;* Timing delays
;*
;* Inputs
;*   W = milliseconds to delay
;* Outputs
;*   none

Delay
  movwf DelayCount1
DelayLoop
  call DelayOneMillisecond
  decfsz DelayCount1            ; (1/2)
  goto DelayLoop                ; (2)
  retlw 0


DelayOneMillisecond             ; delay 7+3n cycles (including call)

  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)
  movlw Time_OneHundred         ; (1) 100uS
  call DelayMicro               ; (2)

DelayMicro

  movwf DelayCount2             ; (1)

DelayMicroLoop
  decfsz DelayCount2            ; (1/2)
  goto DelayMicroLoop           ; (2)
  retlw 0                       ; (2)


  ;* Print one byte in hex
;*
;* Inputs
;*   W = byte to print
;* Outputs
;*   none

Hex_PrintByte
  movwf TempByte        ; byte to output
  swapf TempByte,W      ; high nibble

  call Hex_PrintNibble

  movfw TempByte        ; low nibble

Hex_PrintNibble

  call Hex_table
  call LCD_SendChar
  retlw 0


Hex_table
  andlw 0x0f            ; extract nibble
  addwf PCL             ; computed goto
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


;* Macro to compare a 16 bit value
;*
;* Inputs
;*   value1 = first byte of two byte value (big endian)
;*   value2 = first byte of two byte value (big endian)
;* Outputs
;*   W = used
;*   less = branch to here if value1 < value2
;*   greater = branch to here if value1 > value2
;*   . = execute in-line if values are equal

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


;* Macro to compare a 16 bit value
;*
;* Inputs
;*   value = first byte of two byte value (big endian)
;*   lit = 16 bit constant value
;* Outputs
;*   W = used
;*   less = branch to here if value1 < value2
;*   greater = branch to here if value1 > value2
;*   . = execute in-line if values are equal

CompareLit16 macro _value_,_lit_,_less_,_greater_
  movfw _value_+0
  sublw (_lit_ >> 8) & 0xff     ; High:  value2 - value1
  bnc _greater_
  bnz _less_
  movfw _value_+1
  sublw _lit_ & 0xff            ; Low:   value2 - value1
  bnc _greater_
  bnz _less_
  endm


;* Macro to compare a 16 bit Morse value value for equality
;*
;* Inputs
;*   value = first byte of two byte value (big endian)
;*   lit = 16 bit constant value
;*   code = 8 bit value to return
;* Outputs
;*   W = retlw code if equal
;*   . = execute in-line if values are not equal

Morse16 macro _value_,_lit_,_code_
  local done
  movfw _value_+0
  sublw (_lit_ >> 8) & 0xff     ; High:  value2 - value1
  bnz done
  movfw _value_+1
  sublw _lit_ & 0xff            ; Low:   value2 - value1
  bnz done
  retlw _code_
done
  endm


;* Macro to add a Morse token to 16 bit Morse
;*
;* Inputs
;*   value = first byte of two byte value (big endian)
;* Outputs
;*   W = retlw code if equal

Dah16 macro _value_
  setc
  rlf _value_+1
  rlf _value_+0
  clrc
  rlf _value_+1
  rlf _value_+0
  endm


;* Macro to add a Morse token to 16 bit Morse
;*
;* Inputs
;*   value = first byte of two byte value (big endian)
;* Outputs
;*   W = retlw code if equal

Dit16 macro _value_
  clrc
  rlf _value_+1
  rlf _value_+0
  setc
  rlf _value_+1
  rlf _value_+0
  endm


;* Macro to increment a 16 bit value
;*
;* Inputs
;*   value = first byte of two byte value (big endian) to increment
;* Outputs
;*   value = value + 1

Increment16 macro _value_
  local done
  incfsz _value_+1              ; low byte
  goto done
  incfsz _value_+0              ; low byte
  goto done
  movlw 0xff
  movwf _value_+0               ; prevent wrap to zero
  movwf _value_+1
done
  endm


;* Macro to initialise a 16 bit value
;*
;* Inputs
;*   init = constant 16 bit number
;* Outputs
;*   W = used
;*   value = first byte of two byte value (big endian) to init

Set16 macro _value_,_init_
  movlw (_init_ >> 8) & 0xff
  movwf _value_+0               ; high byte
  movlw _init_ & 0xff
  movwf _value_+1               ; low byte
  endm


;* Macro to copy a 16 bit value
;*
;* Inputs
;*   source = first byte of two byte value (big endian) used as input value
;* Outputs
;*   W = used
;*   dest = first byte of two byte value (big endian)

Move16 macro _dest_,_source_
  movfw _source_+0
  movwf _dest_+0                ; high byte
  movfw _source_+1
  movwf _dest_+1                ; low byte
  endm


;* Macro to add a 16 bit value
;*
;* Inputs
;*   source = first byte of two byte value (big endian) used as input value
;* Outputs
;*   W = used
;*   dest = first byte of two byte value (big endian) dest += source

Add16 macro _dest_,_source_
  movfw _source_+1
  addwf _dest_+1                ; low byte
  movfw _source_+0
  addwf _dest_+0                ; high byte
  endm


;* Macro to Shift Right a 16 bit unsigned value
;*
;* Inputs
;*   value = first byte of two byte value (big endian)
;* Outputs
;*   value <<= 1 (unsigned)

Shr16 macro _value_
  clrc
  rrf _value_+1
  rrf _value_+0
  endm



;* Morse to ASCII
;*
;* Inputs
;*   morse = 16 bit morse value
;* Outputs
;*   W = ASCII value

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
  retlw '~'                     ; unassigned code


;* Main program
;*
;* Inputs
;*   none
;* Outputs
;*   none

main

  clrw                          ; set W = 0
;  clrf FlagBits                ; reset all system flags

  Set16 AverageTime,DefaultTime ; set inital rate
  Set16 WordSeparatorTime,DefaultWordTime
  Set16 CharSeparatorTime,DefaultCharacterTime
  Set16 morse,0

  bsf STATUS,RP0                ; select page 1
  bcf STATUS,RP1                ; ...

  movlw b'11111111'             ; Port A as all input
  movwf TRISA
  movlw b'00000011'             ; Port A as digital input
  movwf ADCON1

  movlw LCD_DataOutput          ; configure Port B
  movwf TRISB

  bcf STATUS,RP0                ; select page 0

  clrf PORTB                    ; set all Port B low

  call LCD_initialise

; print a title

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

; wait for synchronisation assuming an active low input

WaitPulse
WaitLow
  btfsc PORTA,MorseIn
  goto WaitLow
  call DelayOneMillisecond
  btfsc PORTA,MorseIn
  goto WaitLow

  Set16 PulseTime,1
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

PrintChar
  call LCD_SendChar
;  movfw PulseTime+0
;  call Hex_PrintByte
;  movfw PulseTime+1
;  call Hex_PrintByte

Recalibrate
;  Move16 PulseTime,DitTime
;  Add16 PulseTime, DahTime      ; dit * 4
;  Shr16 PulseTime
;  Move16 AverageTime,PulseTime  ; dit * 2
;  Shr16 PulseTime               ; dit * 1
;  Move16 CharSeparatorTime,PulseTime
;  Add16 CharSeparatorTime,AverageTime; dit * 3
;  Move16 WordSeparatorTime,CharSeparatorTime
;  Add16 WordSeparatorTime,AverageTime; dit * 5

  Set16 PulseTime,0
CheckSpaceTime
  call DelayOneMillisecond
  Increment16 PulseTime
  Compare16 PulseTime,WordSeparatorTime,CheckSpaceTime_ok,PrintWordSeparator
CheckSpaceTime_ok
  btfsc PORTA,MorseIn
  goto CheckSpaceTime
  Compare16 PulseTime,CharSeparatorTime,WaitPulse,PrintCharSeparator

PrintCharSeparator
  call MorseToASCII
  call LCD_SendChar
  Set16 morse,0
  goto WaitPulse

PrintWordSeparator
  call MorseToASCII
  call LCD_SendChar
  movlw ' '
  call LCD_SendChar
  Set16 morse,0
  goto WaitPulse


; hold backlight on for a time

BackLightHold
  movlw 30*4
  movlw 250                     ; 1/4 second delay
  call Delay
  decfsz temp
  goto BackLightHold
  bcf PORTB,LCD_BackLight; backlight off

; lock up - reset to initiate next cycle

DynamicStop
  goto DynamicStop

  end
