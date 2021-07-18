;-----------------------------------------------------------------
PlaceSprite:
;-----------------------------------------------------------------
;Use this to put a sprite down for the first time.
;Args: hl=Pointer to sprite record
                 push hl

                 ld (hl),1           ;Set draw mode to 1 as sprite does not have an old position that needs erasing.
                 inc hl

                 ld b,(hl)           ;Coordinates should already be in sprite record: Put them into b,c
                 inc hl
                 ld c,(hl)
                 inc hl

                 ;ld (hl),b           ;Store coordinates
                 ;inc hl
                 ;ld (hl),c
                 ;inc hl

                 ld (hl),b           ;Make Old ccordinates same as new coordinates
                 inc hl
                 ld (hl),c
                 inc hl

                 ex de,hl
                 call ScreenToMemory ;Convert new coordinates to screen address
                 ex de,hl

                 ld (hl),e           ;Write screen address to memory. It's a double-byte number so WRONG WAY ROUND!
                 inc hl
                 ld (hl),d
                 inc hl
                 
                 inc hl
                 inc hl
                 
                 ld (hl),e           ;Write old screen address to memory. Same as new address as sprite is new
                 inc hl
                 ld (hl),d

                 pop hl
                 ret

;-----------------------------------------------------------------
MoveSprite:
;-----------------------------------------------------------------
;Use this to move a sprite that's already on the screen
;Args: hl=Pointer to sprite record   b,c=x,y coordinate offset

                push hl            ;Push record pointer so we can get it back

                ld (hl),3          ;Set draw mode to erase old sprite position next time we draw
                inc hl

                ld d,(hl)          ;Get current x position from sprite record & put in d
                ld a,d
                add a,b            ;Add new x movement to x-coord to get new coord
                ld b,a             ;Store new position in b
                ld (hl),a          ;Update sprite record with new x pos
                inc hl

                ld e,(hl)          ;Get current y position and out in e
                ld a,e
                add a,c            ;Add new y movement to y-coord to get new coord
                ld c,a             ;Store new position in c
                ld (hl),a          ;Update sprite record with new y pos
                inc hl

.AbsSkip        ld (hl),d          ;X pos becomes old x position
                inc hl
                ld (hl),e          ;Y pos becomes old y position
                inc hl

                ex de,hl
                call ScreenToMemory ;Convert new coordinates to screen address
                ex de,hl

                ld c,(hl)           ;Get old screen address from record and store it in bc for now
                ld (hl),e           ;Then get our new screen address and replace it in the sprite record
                inc hl              ;REMEMBER: WHEN WRITING 2-BYTE NUMBERS TO MEMORY BYTES ARE ALWAYS THE WRONG WAY ROUND!!
                ld b,(hl)
                ld (hl),d
                inc hl

                inc hl              ;Do nothing to the graphic for now
                inc hl

                ld (hl),c           ;Move stored old screen address from bc to old pos in the sprite record. Bytes wrong way round!
                inc hl
                ld (hl),b
                inc hl

                pop hl              ;Retrieve our pointer position at start of record
                ret
                
;-----------------------------------------------------------------
MoveSpriteAbsolute:
;-----------------------------------------------------------------
;Move sprite at hl to absolute coordinates b,c
                push hl
                ld (hl),3
                inc hl
                ld d,(hl)
                ld (hl),b
                inc hl
                ld e,(hl)
                ld (hl),c
                inc hl
                jr MoveSprite.AbsSkip ;Main movesprite proc can take over from here.

;-----------------------------------------------------------------
NextFrame:
;-----------------------------------------------------------------

                ld a,(hl)           ;If sprite switched off, quit.
                and a
                ret z
                cp 2                ;If sprite being erased, quit
                ret z

                push hl
                ld (hl),3

                ld b,0
                ld c,7               ;Skip forward to the current graphic address
                add hl,bc

                ld e,(hl)
                inc hl               ;Read current graphic address to de
                ld d,(hl)
                inc hl

                inc hl               ;Skip over old screen address, la la la.
                inc hl

                ld (hl),e            ;Put graphic address from de into old graphic address
                inc hl
                ld (hl),d
                inc hl

                inc hl               ;Skip some more...

                ld b,(hl)            ;Till we get to frame number, put it in B
                inc b                ;B will now hold our Next frame
                inc hl
                ld a,(hl)            ;Load no of frames into A
                cp 1                 ;Is there only one frame in this animation?
                jr z,1F              ;If so, just quit
                dec a
                cp b                 ;Compare current frame with no of frames
                ld a,b
                jr c,.l2             ;If done last frame go over there

                ex de,hl   ;4          ;Add 32 to graphic address to get next frame
                ld b,0     ;7
                ld c,32    ;7
                add hl,bc  ;11
                ex de,hl   ;4   33

.l1:            dec hl
                ld (hl),a            ;Put new frame number back into sprite record

                ld b,0
                ld c,5     ;7        ;Subtract 6 from hl, but we set c to 5 because...
                scf        ;4        ;...scf sets carry flag to 1
                sbc hl,bc  ;15       ;And this is subtracted from hl too.

                ld (hl),d            ;Put new graphic into record - Because we're going backwards here it goes in d,e rather than e,d
                dec hl
                ld (hl),e
1               pop hl
                ret

.l2:            inc hl
                ld e,(hl)
                inc hl
                ld d,(hl)
                ld a,0               ;Set new frame number in A to the first one
                dec hl
                dec hl
                jr .l1
                
;---------------------------------------------------------
DrawSprite:
;---------------------------------------------------------
;Draw a 16x16 pixel sprite to screen, erasing where it used to be
;Args: hl points to sprite record

;b: counter to loop through each line of sprite
;de: pointer to graphic
;hl: pointer to screen address
;de',hl': Same but for the old sprite pos for erasing sprite

;Example Sprite Record:
;               defb 1             ;Draw mode: 0=disabled (don't draw or erase at all)
                                   ;           1=Don't erase from old pos, but draw at new pos
                                   ;           2=Erase from old pos but don't draw at new pos
                                   ;           3=Erase from old pos and draw at new pos
;sprManX:       defb 32            ;x position
;sprManY:       defb 16            ;y position
;               defb 0             ;old x position
;               defb 0             ;old y position
;sprManAddr:    defw 16384         ;screen address
;sprManGfx:     defw gfx_ManStill  ;Graphic
;               defw 16384         ;old screen address
;               defw gfx_ManStill  ;Old graphic

;               defb 0             ;Sprite Color
;               defb 0             ;Sprite Frame no


         ;PART 1: Read data from sprite record and initialise registers and alternate registers
         ;Starting Graphic address and Screen address will be put in de & hl, while the same
         ;for the old sprite position will be put in the alternative registers (de' and hl')
         ;c (and c' for erasing) will contain the x bit offset
         ;b is left for now so it can be our y counter for going through each line

                ld a,0            ;Presume reverse line draw mode is off at the start
                ld (SpReverse),a

                ld a,(hl)    ;Load draw mode
                and a
                ret z        ;If draw mode is 0, sprite is disabled, so quit
                ld (SpDrawMode),a ;Store draw mode in SpDrawMode
                cp 2
                jr nz,1F
                ld a,0
                ld (hl),a
1               inc hl

                ld a,(hl)    ;Get Sprite X pos
                and 7        ;Convert x coordinate to just the bit offset (0 to 7)
                ld (SpOffset),a   ;store in SpOffset for now
                inc hl
                ld b,(hl)    ;Put new Y-pos in b
                inc hl

                ld a,(hl)    ;Get old Sprite x pos
                and 7        ;Convert to offset
                ld (SpOldOffset),a   ;Get old Sprite X pos, store in OldSpX for now
                inc hl
                ld a,(hl)    ;Put old Y-pos in a
                inc hl

                sub b        ;Subtract old y-pos from new y-pos
                jr z,1F      ;If zero, normal order
                and 240      ;Check If difference is between 1 and 15
                jr z, 1F
                ld a,1       ;If it is, reverse line drawing order
                ld (SpReverse),a
1
                ld e,(hl)    ;Get Screen address, put in de - WRONG WAY ROUND!
                inc hl
                ld d,(hl)
                inc hl
                ld a,(hl)    ;Get Graphic address, put in bc   WRONG WAY ROUND! and also put in SpGraphic
                ld (SpGraphic+1),a
                ld c,a
                inc hl
                ld a,(hl)
                ld (SpGraphic),a
                ld b,a
                inc hl

                push hl      ;Push position in sprite record for now
                ex de,hl     ;Move screen address into hl
                ld d,b       ;Move graphic address into de
                ld e,c

                ld a,(SpOffset)   ;Store in c the bit of screen address to write to (kindof a x offset)
                ld c,a

                exx          ;Switch to alternate register set, where we will store the old sprite pos
                pop hl       ;Restore our position in the sprite record back into hl

                ld e,(hl)    ;Get old Screen address, put in de - WRONG WAY ROUND
                inc hl
                ld d,(hl)
                inc hl
                ld c,(hl)    ;Get old Graphic address, put in bc - WRONG WAY ROUND - and put SpGraphic into old graphic address
                ld a,(SpGraphic+1)   ;...This is so the old graphic address is automatically filled with the one currently being drawn
                ld (hl),a
                inc hl
                ld b,(hl)
                ld a,(SpGraphic)
                ld (hl),a

                inc hl
                ld a,(hl)
                ld (SpColour),a

                ex de,hl     ;Switch screen address into hl
                ld d,b       ;Now switch graphic address into de
                ld e,c
                ld a,(SpOldOffset)
                ld c,a       ;Store in c the bit of char to write to

         ;PART 2: Main loop

                ld a,(SpDrawMode)      ;Depending on draw mode, jump to a different
                dec a                  ; main loop, or carry on to draw and erase.
                jp z,DS_NoEraseDraw
                dec a
                jp z,DS_EraseNoDraw

                ld a,(SpReverse)       ;Should we be reversing this loop?
                and a
                jr nz, DS_RvrsLoop       ;If so, jump to that loop.

                ;Loop for Erasing AND Drawing
                exx
                call ColourSpriteLine
                exx
                ld b,8               ;b is our counter for each line of sprite
1                 call EraseSpriteLine
                  call IncRow
                  exx
                  call DrawSpriteLine
                  call IncRow
                  exx
                  djnz 1B        ;Decrement b, and if not finished last line, loop back
                exx
                call ColourSpriteLine
                exx
                ld b,8
1                 call EraseSpriteLine
                  call IncRow
                  exx
                  call DrawSpriteLine
                  call IncRow
                  exx
                  djnz 1B        ;Decrement b, and if not finished last line, loop back
                exx                   ;Make sure we finish subroutine with the standard registers!
                ret

DS_RvrsLoop:
                exx                   ;Back to the normal registers
                ld a,e                ;Move new graphic address to the last line
                add a,32
                ld e,a
                ld a,d
                adc a,0
                ld d,a
                call Down16Rows       ;Move hl down to the final line
                call DecRow
                exx                   ;And move to alternative registers again.
                ld a,e
                add a,32
                ld e,a
                ld a,d
                adc a,0
                ld d,a
                call Down16Rows
                call DecRow

                exx
                call ColourSpriteLine
                exx
                ld b,8
1                 call EraseSpriteLine
                  call DecRow
                  exx
                  call DrawSpriteLine
                  call DecRow
                  exx
                  djnz 1B
                exx
                call ColourSpriteLine
                exx
                ld b,8
1                 call EraseSpriteLine
                  call DecRow
                  exx
                  call DrawSpriteLine
                  call DecRow
                  exx
                  djnz 1B
                exx
                ret

DS_NoEraseDraw: ;Loop for not erasing but drawing
                exx       ;We're not erasing here, so we can forget our alternative registers
                call ColourSpriteLine
                ld b,8
1                 call DrawSpriteLine
                  call IncRow
                  djnz 1B
                call ColourSpriteLine
                ld b,8
1                 call DrawSpriteLine
                  call IncRow
                  djnz 1B
                ret

DS_EraseNoDraw: ;Loop for erasing but not drawing
                ld b,16
1                 call EraseSpriteLine
                  call IncRow
                  djnz 1B
                exx       ;Make sure we finish subroutine with the standard registers!
                ret

ColourSpriteLine:
                push hl
                ld a,h            ;Convert screen address to attribute address
		rra
		rra
		rra
		and 3
		or 0x58
		ld h,a
		ld a,(SpColour)
		ld b,a
                ld a,(LevelAttrib)
                add a,b
                ld b,a
		ld a,c            ;Load sprite x offset
		and 7             ;...If it goes into 8 we colour 2 squares along, otherwise 3
		ld a,b
		jr z,1F
		ld (hl),a
		inc hl
1		ld (hl),a
                inc hl
                ld (hl),a
                pop hl
                ret

;----------------
DrawSpriteLine:
;----------------
;This draws a single line of graphic (stored at address pointed to by de) to the screen pointed to by hl
;c Should contain the bit offset - if this isn't 0 then the bits need some rotating
                push bc              ;Save bc for now

                ex de,hl             ;We swap these for now so we can use opcodes with (hl)

                ld a,(SpReverse)
                and a
                ld a,c               ;Put bit offset into a
                jr z,1F

                dec hl               ;\
                ld c,(hl)            ;| REVERSE MOVEMENT: Same as below but proceeds backwards.
                dec hl               ;|
                ld b,(hl)            ;/
                jr 2F

1               ld b,(hl)            ;\ FORWARD MOVEMENT
                inc hl               ;| Put both bytes of graphic information for the line into b and c
                ld c,(hl)            ;| and move our Graphic pointer along
                inc hl               ;|
2               ex de,hl             ;/


                and a                ;This bit basically jumps to where we want to go to do the right
                jr z,DS_NoRotate     ;bit rotations. If the x offset is over 4 then we do left rotations
                dec a                ;rather than right rotations as fewer rotations will then be needed.
                jr z,DS_Right1
                dec a
                jr z,DS_Right2
                dec a
                jr z,DS_Right3
                dec a
                jr z,DS_Left4
                dec a
                jr z,DS_Left3
                dec a
                jr z,DS_Left2
                dec a
                jr DS_Left1

DS_Right3:      srl b      ;Shift right once, carrying bits over into the a register.
                rr c
                rr a
DS_Right2:      srl b      ;Shift right again.
                rr c
                rr a
DS_Right1:      srl b      ;And one more time.
                rr c
                rr a

                inc hl     ;Skip forward to the last char of the line. We have to, really, because we need the a register free
                inc hl     ;So we'll draw the bytes from right to left, A, C, B
                or (hl)    ;Combine graphic bits with screen bits
                ld (hl),a  ;And then write this to the screen
                dec hl
                ld a,c
                or (hl)
                ld (hl),a
                dec hl
                ld a,b
                or (hl)
                ld (hl),a
                pop bc      ;Get bc back
                ret         ;Finished

DS_Left4:       sla c       ;Here's our left rotations
                rl b
                rl a
DS_Left3:       sla c
                rl b
                rl a
DS_Left2:       sla c
                rl b
                rl a
DS_Left1:       sla c
                rl b
                rl a

                or (hl)     ;This time the a register contains the leftmost char, so we can go from left to right: A, B, C
                ld (hl),a
                inc hl
                ld a,b
                or (hl)
                ld (hl),a
                inc hl
                ld a,c
                or (hl)
                ld (hl),a
                dec hl      ;And go back to the start of the line
                dec hl
                pop bc
                ret
DS_NoRotate:                ;This is much easier as no rotation is needed, so just draw bytes in b & c to the screen address
                ld a,b
                or (hl)
                ld (hl),a
                inc hl
                ld a,c
                or (hl)
                ld (hl),a
                dec hl
                pop bc
                ret

;----------------
EraseSpriteLine:
;----------------
;This is EXACTLY the same as DrawSpriteLine except we invert bits and then AND instead of just OR when we write to the screen, thus erasing our image.

                push bc              ;Save bc for now
                ex de,hl             ;We swap these for now so we can use opcodes with (hl)

                ld a,(SpReverse)
                and a
                ld a,c               ;Put bit offset into a
                jr z,1F

                dec hl               ;\
                ld c,(hl)            ;| REVERSE MOVEMENT: Same as below but proceeds backwards.
                dec hl               ;|
                ld b,(hl)            ;/
                jr 2F

1               ld b,(hl)            ;\ FORWARD MOVEMENT
                inc hl               ;| Put both bytes of graphic information for the line into b and c
                ld c,(hl)            ;| and move our Graphic pointer along
                inc hl               ;|
2               ex de,hl             ;/

                and a
                jr z,DS_eNoRotate
                dec a
                jr z,DS_eRight1
                dec a
                jr z,DS_eRight2
                dec a
                jr z,DS_eRight3
                dec a
                jr z,DS_eLeft4
                dec a
                jr z,DS_eLeft3
                dec a
                jr z,DS_eLeft2
                dec a
                jr DS_eLeft1

DS_eRight3:      srl b      ;Shift right once, carrying bits over into the a register.
                rr c
                rr a
DS_eRight2:      srl b      ;Shift right again.
                rr c
                rr a
DS_eRight1:      srl b      ;And one more time.
                rr c
                rr a

                inc hl     ;Skip forward to the last char of the line. We have to, really, because we need the a register free
                inc hl
                cpl
                and (hl) ;xor (hl)   ;XOR this time, to erase
                ld (hl),a
                dec hl
                ld a,c
                cpl
                and (hl);xor (hl)
                ld (hl),a
                dec hl
                ld a,b
                cpl
                and (hl);xor (hl)
                ld (hl),a
                pop bc      ;Get bc back
                ret

DS_eLeft4:       sla c
                rl b
                rl a
DS_eLeft3:       sla c
                rl b
                rl a
DS_eLeft2:       sla c
                rl b
                rl a
DS_eLeft1:       sla c
                rl b
                rl a

                cpl
                and (hl);xor (hl)
                ld (hl),a
                inc hl
                ld a,b
                cpl
                and (hl);xor (hl)
                ld (hl),a
                inc hl
                ld a,c
                cpl
                and (hl);xor (hl)
                ld (hl),a
                dec hl
                dec hl
                pop bc
                ret
DS_eNoRotate:
                ld a,b
                cpl
                and (hl);xor (hl)
                ld (hl),a
                inc hl
                ld a,c
                cpl
                and (hl);xor (hl)
                ld (hl),a
                dec hl
                pop bc
                ret


;Memory store for DrawSprite routine
SpDrawMode: defb 0
SpOffset: defb 0
SpOldOffset: defb 0
SpGraphic: defw 0
SpReverse: defb 0
SpColour:     defb 0