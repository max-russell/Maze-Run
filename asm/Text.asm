;---------------------------------------------------------
PrintMultiLine:
;---------------------------------------------------------
;Prints multiple lines of text at col,row bc.
;Strings is at de, each line terminated with a '0'
;And two in a row to finish.
;h is attributes (no multiline)
                 ld a,0
                 push bc
                 push hl
                 call Print
                 pop hl
                 pop bc
                 inc de
                 ld a,(de)
                 cp 0
                 ret z
                 inc c     ;Down to next row
                 jr PrintMultiLine

;---------------------------------------------------------
Print:
;---------------------------------------------------------
;Print a string starting at de in memory to screen at column,row bc.
;a is 0 for normal height, non-zero for double
;h is attributes for text (only top half if double height, lower half in l)

                ld (.DblHt),a
                ld a,h
                ld (.Attr1),a
                ld a,l
                ld (.Attr2),a
                sla b  ;Multiply col & row by 8 to get pixel position
                sla b
                sla b
                sla c
                sla c
                sla c
                push bc
                call ScreenToAttr
                push de
                ld c,32
                ld b,0
.AttrLoop       ld a,(de)
                and a
                jr z,.DisplayMap
                ld a,(.Attr1)
                ld (hl),a

                ld a,(.DblHt)
                and a
                jr z,1F
                add hl,bc
                ld a,(.Attr2)
                ld (hl),a
                scf
                sbc hl,bc
                inc hl
1               inc de
                inc hl
                jr .AttrLoop

.DisplayMap     pop de
                pop bc
                call ScreenToMemory ;Convert to screen address in hl
.Loop:          ld a,(de)
                and a
                ret z
                push de
                ex de,hl

                ;Translate character byte to character graphic.
                sub 32  ;Subtract 32
                ld l,a
                ld h,0
                sla l   ;Multiply by 8
                rl h
                sla l
                rl h
                sla l
                rl h
                ld bc, MazeRunFont
                add hl,bc  ;Add start address of the font data
                ex de,hl

                ld a,(.DblHt)
                and a
                jr z,1F
                call DrawCharDoubleHeight
                jr 2F
1               call DrawChar
2               inc hl
                pop de
                inc de
                jr .Loop

.Attr1:         defb 0
.Attr2:         defb 0
.DblHt:         defb 0

;---------------------------------------------------------
PrintNumber:
;---------------------------------------------------------
;Same as print, but prints a number from 0 to 99 stored in d.
                ld e,a ;Shove double height info in e for now
                ld a,d
                call ConvertToDecimal    ;Convert number to binary-coded decimal
                and 11110000b  ;Does the number have two digits?
                jr z,.OneDigit
.TwoDigits:     srl a          ;Shift tens digit along
                srl a
                srl a
                srl a
                add a,48        ;Add 48 to get character code.
                ld (.s_Num),a   ;Place in string store
                ld a,d          ;Get original number again
                call ConvertToDecimal
                and 00001111b
                add a,48        ;Get character code for units digit
                ld (.s_Num+1),a ;Place in second char of string store
                jr .DoPrint
.OneDigit:      ld a,d          ;Only one digit required.
                add a,48
                ld (.s_Num),a
.DoPrint:       ld a,e          ;Get double height info back in a
                ld de,.s_Num    ;Set string for print routine to number string.
                call Print
                ld a,0          ;Make sure string store is cleared ready for next
                ld (.s_Num),a   ;time routine is called.
                ld (.s_Num+1),a
                ret

.s_Num:         defb 0,0,0      ;Number string store.

;---------------------------------------------------------
ConvertToDecimal:
;---------------------------------------------------------
                cp 10    ;11
                ret c
                add a,6
                cp 26    ;21 +6
                ret c
                add a,6
                cp 42    ;31 + 12
                ret c
                add a,6
                cp 58    ;41 + 18
                ret c
                add a,6
                cp 74    ;51 + 24
                ret c
                add a,6
                cp 90    ;61 + 30
                ret c
                add a,6
                cp 106    ;71 + 36
                ret c
                add a,6
                cp 122    ;81 + 42
                ret c
                add a,6
                cp 138    ;91 + 48
                ret c
                add a,6
                ret


;---------------------------------------------------------
DrawChar:
;---------------------------------------------------------
;Draw character starting at de in memory, to screen address hl
;
                push hl
                ld b,8
.Loop           ld a,(de)
                ld (hl),a
                inc de
                call IncRow
                djnz .Loop
                pop hl
                ret
                
;---------------------------------------------------------
DrawCharDoubleHeight:
;---------------------------------------------------------
;Draw character starting at de in memory, to screen address hl
;
                push hl
                ld b,8
.Loop           ld a,(de)
                ld (hl),a
                call IncRow
                ld a,(de)
                ld (hl),a
                inc de
                call IncRow
                djnz .Loop
                pop hl
                ret