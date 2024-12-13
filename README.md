# Morse code decoder

This is an old PIC Assembly program from 1995 and was originally for a
PIC16C71.  Since these need UV top erase them the code has been change
to support PIC16F84 which is electrically erasable.

The original code did not compensate for the awkward memory layout of
the 20x4 LCD module so this was updated to simply scroll the display
when the cursor reaches the end of line 4.  Since there is
insufficient RAM on low end PICs, therefore the display data is read
directly from the display and rewritten to its new position.

The input is active low and the LCD is operated in 4 bit mode.  See
the code for pin connections.

The program sets the Dit to be 60ms and Dah to be 180ms, see below for
details.


## Morse timings

~~~

PARIS
.--.          .-        .-.         ..        ...
1+3+3+1+ 3+3 +1+3+ 1+3 +1+3+1+ 2+3 +1+1+ 1+3 +1+1+1+ 2+7 = 50
.     = 1
-     = 3
inter = 1 (inside a char)
intra = 3 (between chars)
space = 7 (between words)

wpm = 20

. time = one_min / (wpm * paris)   [seconds]
= 60 / (20 * 50) = 0.06 seconds = 60 ms

. =  60 ms
- = 180 ms
S = 420 ms

~~~
