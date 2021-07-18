;-----------------------------------------------------------------
WaitVBL:
;-----------------------------------------------------------------
;Waits for the next vertical retrace. Interrupts must be enabled.
	ld hl,23672
	ld a,(hl)
1       cp (hl)
	jr z, 1B
	ret

;-----------------------------------------------------------------
Pause:
;-----------------------------------------------------------------
;Does nothing for bc 50ths of a second. So if bc is 50, wait one second.

        ld hl,23672
2       ld a,(hl)
1       cp (hl)
        jr z,1B

        dec bc
        ld a,b
        add a,c
        jr nz,2B
        ret

;-----------------------------------------------------------------
TestKey:
;-----------------------------------------------------------------
;Checks if a key is being pressed - sets zero flag to 0 if key IS pressed
;Args: hl=pointer to Key record in memory
;Key record is two bytes: 1) High byte of port 2) Bit to check
            ld c,$FE      ;Set low port byte, always $FE
            ld b,(hl)     ;Set high port byte from Key record
            inc hl
            ld a,(hl)     ;Get bit to check
            in h,(c)      ;Check port
            and h         ;AND with bit to check - set zero flag
            ret

;-----------------------------------------------------------------
GetType:
;-----------------------------------------------------------------
;Waits until a new key is pressed and returns character code in a.
;Doesn't return port information or nuffink.
KSTATE: equ 23556
            ld a,255             ;First, loop until no key is being pressed.
            ld hl,KSTATE
1           cp (hl)
            jr nz,1B
1           cp (hl)              ;Now loop until a key IS pressed.
            jr z,1B
            ld a,(hl)
            ret

;-----------------------------------------------------------------
TestAnyKey:
;-----------------------------------------------------------------
;Tests the keyboard and returns character code of any key pressed in a.
;Or 0 if no key pressed.
;Also puts port and bit of key into var LastKeyCheck
;Can't return more than one key pressed simultaneously.

            ld c,$FE
            ld hl,.KeyGrps

.GrpLoop:   ld a,(hl)    ;Get lower bit of port
            and a
            ret z
            ld b,(hl)
            in a,(c)     ;Check port
            inc hl
            ld d,16
            bit 4,a
            jr z,.GotKey
            inc hl
            ld d,8
            bit 3,a
            jr z,.GotKey
            inc hl
            ld d,4
            bit 2,a
            jr z,.GotKey
            inc hl
            ld d,2
            bit 1,a
            jr z,.GotKey
            inc hl
            ld d,1
            bit 0,a
            jr z,.GotKey
            inc hl
            jr .GrpLoop

.GotKey:
            ld a,(hl)
            ld hl,LastKeyCheck
            ld (hl),b
            inc hl
            ld (hl),d
            ret

.KeyGrps:
            defb $F7,'5','4','3','2','1'
            defb $EF,'6','7','8','9','0'
            defb $FB,'T','R','E','W','Q'          ;$FB FE
            defb $DF,'Y','U','I','O','P'          ;$DF FE
            defb $FD,'G','F','D','S','A'          ;$FD FE
            defb $BF,'H','J','K','L',33      ;$BF FE
            defb $FE,'V','C','X','Z',33       ;$FE FE
            defb $7F,'B','N','M',33, ' '      ;$7F FE
            defb 0

;Default Keys: Two bytes each, first port upper byte, then bit to check
KEY_UP:        defb $FB,1          ;'q'
KEY_DOWN:      defb $FD,1          ;'a'
KEY_LEFT:      defb $DF,2          ;'o'
KEY_RIGHT:     defb $DF,1          ;'p'
KEY_PAUSE:     defb $7F,1
KEY_RESTART:   defb $DF,16
KEY_QUIT:      defb $FB,16         ;'t'
LastKeyCheck:  defb $00,0          ;Last key pressed in TestAnyKey

;Keyboard ports
;   bit  4    3    2    1    0
;        16   8    4    2    1

;63486 - 5,   4,   3,   2,   1          $F7 FE
;61438 - 6,   7,   8,   9,   0          $EF FE
;64510 - t,   r,   e,   w,   q          $FB FE
;57342 - y,   u,   i,   o,   p          $DF FE
;65022 - g,   f,   d,   s,   a          $FD FE
;49150 - h,   j,   k,   l,   ENTER      $BF FE
;65278 - v,   c,   x,   z,   CAPS       $FE FE
;32766 - b,   n,   m,   SYM, SPACE      $7F FE

;---------------------------------------------------------
ScreenToMemory:
;---------------------------------------------------------
;arguments
;b: x coordinate
;c: y coordinate
;uses a,b,c,h,l
                ;Load starting address of display data in speccy's RAM
                ld hl,SCREEN
                ;Offset based on which third of the screen we're in: top, middle or bottom
                ld a,c
                and 192
                srl a;shift right
                srl a
                srl a
                add a,h
                ld h,a
                ;Offset based on which 8 pixel row we're in
                ld a,c
                and 56
                sla a;shift left
                sla a
                add a,l
                ld l,a
                ;Offset based on which 8-pixel column we're in
                ld a,b
                srl a;shift right
                srl a
                srl a
                add a,l
                ld l,a
                ;Offset based on which pixel in the row we're in (0 to 7)
                ld a,c
                and 7
                add a,h
                ld h,a  
                ;Work out which bit we need to write to get the right pixel in the column (0 to 7)
                ld a,b
                and 7
                ret

;---------------------------------------------------------
ScreenToAttr:
;---------------------------------------------------------
;Args: b,c: screen coordinates in pixels
;Returns: h,l: Attribute memory location for char

               srl b
               srl b
               srl b
               ld a,0
               sla c
               rl a
               sla c
               rl a
               or 88
               ld h,a
               ld a,b
               or c
               ld l,a
               ret




;---------------------------------------------------------
IncRow:
;---------------------------------------------------------
; call : hl = display screen address
; returns : hl = address of next row down.
                inc h                           ; move down one row
                ld a,h
                and 7                           ; moved over a character cell?
                ret nz                          ; nz = no, all done
                ld a,l
                add a,32        ; update character row number
                ld l,a
                ret c
                ld a,h
                sub 8                           ; if no carry, need to re-adjust h
                ld h,a
                ret

;---------------------------------------------------------
DecRow:
;---------------------------------------------------------
		dec h
		ld a,h
		and 7
		cp 7
		ret nz
		ld a,l
		sub 32
		ld l,a
		ret c
		ld a,h
		add a,8
		ld h,a
		ret

;---------------------------------------------------------
IncChr:
;---------------------------------------------------------
;Moves screen address hl 8 lines down
                ld a,l
                add a,32
                ld l,a
                ret nc
                ld a,h
                add a,8
                ld h,a
                ret

;---------------------------------------------------------
Down16Rows:
;---------------------------------------------------------
                ld a,l
                add a,64
                ld l,a
                ret nc
                ld a,h
                add a,8
                ld h,a
                ret

	; screen address to attribute address
	; call: de = screen address
	; returns: de = attribute address

;---------------------------------------------------------
ClearScreen:
;---------------------------------------------------------
	// call with a = ink colour.

	ld e,a
	ld hl,16384
	ld bc,6144
1B	ld a,0
	ld (hl),a
	inc hl
	dec bc
	ld a,b
	or c
	jr nz, 1B

	ld bc,768
1B	ld (hl),e
	inc hl
	dec bc
	ld a,b
	or c
	jr nz, 1B
	ret