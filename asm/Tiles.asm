;-----------------------------------------------------------------
DrawTile:
;-----------------------------------------------------------------
;A tile is an image with its own attribute data that must be drawn
;to coordinates that are multiples of 8.
;Args: b,c = Screen coordinates in pixels to draw to (must be multiple of 8)
;      DE = Address of tile object in mem
;Tile objects follow the format: width (in chars), height (in chars), pixel data, attribute data

                call GetTileHeader
                call DrawTilePixels
                call DrawTileAttributes
                ret

.TileX: defb 0
.TileY: defb 0
.TileWidth: defb 0      ;Place in mem to store cirrent tile width and height (for DrawTile procedure)
.TileHeight: defb 0
.TileTransparent: defb 0 ;If this is 1 the paper colour is the same as the default for the level, and no bright or flash is allowed.

;---------------------------------------------------------
DrawTileNoAttributes:
;---------------------------------------------------------
                call GetTileHeader
                call DrawTilePixels
                ret

;---------------------------------------------------------
DrawSpriteAsTile:
;---------------------------------------------------------
;Used on the help screens. a is colour to draw as
                push de
                ld de,.tempAttribs
                ld (de),a
                inc de
                ld (de),a
                inc de
                ld (de),a
                inc de
                ld (de),a
                pop de

                ld a,b
                ld (DrawTile.TileX),a
                ld a,c
                ld (DrawTile.TileY),a
                ld a,0
                ld (DrawTile.TileTransparent),a
                ld a,2
                ld (DrawTile.TileWidth),a
                ld a,2
                ld (DrawTile.TileHeight),a
                call ScreenToMemory     ;Convert screen coords to memory address in hl
                
                call DrawTilePixels
                ld de,.tempAttribs
                call DrawTileAttributes
                ret

.tempAttribs:   defb 0,0,0,0

;---------------------------------------------------------
DrawTileKeyOnPanel:
;---------------------------------------------------------
                call GetTileHeader
                call DrawTilePixels
                ld a,1
                ld (DrawTile.TileTransparent),a
                ld a,(LevelAttrib)
                ld (.tempLevelAttrib),a
                ld a,PAPER_WHITE
                ld (LevelAttrib),a
                call DrawTileAttributes
                ld a,(.tempLevelAttrib)
                ld (LevelAttrib),a
                ret

.tempLevelAttrib: defb 0

;-----------------------------------------------------------------
Draw16x16Box:
;-----------------------------------------------------------------

                call ScreenToMemory
                ld c,%11111111
                ld d,%10000000
                ld e,%00000001
                ld (hl),c
                inc hl
                ld (hl),c
                dec hl
                call IncRow
                ld b,14
1               ld a,(hl)
                or d
                ld (hl),a
                inc hl
                ld a,(hl)
                or e
                ld (hl),a
                dec hl
                call IncRow
                djnz 1B
                ld (hl),c
                inc hl
                ld (hl),c
                ret
                
;-----------------------------------------------------------------
Draw16x16BoxOpaque:
;-----------------------------------------------------------------

                push bc
                call ScreenToMemory
                ld c,%11111111
                ld d,%10000000
                ld e,%00000001
                ld (hl),c
                inc hl
                ld (hl),c
                dec hl
                call IncRow
                ld b,14
1               ld (hl),d
                inc hl
                ld (hl),e
                dec hl
                call IncRow
                djnz 1B
                ld (hl),c
                inc hl
                ld (hl),c
                pop bc
                call ScreenToAttr
                ld b,PAPER_WHITE+INK_BLACK
                ld (hl),b
                inc hl
                ld (hl),b
                ld a,l
                add a,31
                ld l,a
                ld a,h
                adc a,0
                ld h,a
                ld (hl),b
                inc hl
                ld (hl),b
                ret

;-----------------------------------------------------------------
GetTileHeader:
;-----------------------------------------------------------------
                ld a,b
                ld (DrawTile.TileX),a
                ld a,c
                ld (DrawTile.TileY),a
                call ScreenToMemory     ;Convert screen coords to memory address in hl

                ld a,(de)               ;Store tile's width and height for later
                ld (DrawTile.TileWidth),a
                inc de
                ld a,(de)
                ld (DrawTile.TileHeight),a
                inc de                  ;Then move on to the start of the pixel data

                ld a,(de)
                ld (DrawTile.TileTransparent),a
                inc de
                ret

;-----------------------------------------------------------------
DrawTilePixels:
;-----------------------------------------------------------------
                ld a,(DrawTile.TileHeight)          ;loop through y with b as counter
                ld b,a
                sla b         ;Shift TileHeight left three times (effectively multiplying by 8) to get height in pixels
                sla b
                sla b
.NextRow:       ld a,(DrawTile.TileWidth)          ;loop through x with c as counter
                ld c,a
.NextCol:       ld a,(de)
                ld (hl),a     ;Draw byte to screen
                inc de        ;Move to next column of source image
                inc hl        ;Move to next char of screen address
                dec c         ;decrement c counter (for x pos)
                jp nz,.NextCol    ;If not loop back

                ld a,(DrawTile.TileWidth) ;Get our tilewidth so we can go back to the first column of this row
                ld c,a
                ld a,b        ;Save b register for now, so we can use it for our double-register subtract
                ld b,0
                add a,0       ;This is just so the carry flag is reset for the sbc command
                sbc hl,bc     ;Subtract TileWidth from hl to put us back at the first column of the row
                ld b,a        ;Put the b counter back to what it should be
                call IncRow   ;Move 1 down to next row of screen
                djnz .NextRow     ;Loop back to start next row
                ret
                
;-----------------------------------------------------------------
DrawTileAttributes:
;-----------------------------------------------------------------
                ld a,(DrawTile.TileX)   ;Now for attributes. Load up our tile position into b & c
                ld b,a
                ld a,(DrawTile.TileY)
                ld c,a
                call ScreenToAttr ;Convert to address in attributes and put in hl

                ld a,(DrawTile.TileHeight)
                ld c,a
.NextRow2:      ld a,(DrawTile.TileWidth)
                ld b,a

.NextCol2:      ld a,(DrawTile.TileTransparent)
                cp 1
                jr z,1F
                ld a,(de)         ;Read attribute value
                jr 2F
1               push bc           ;Preserve default level attribute
                ld a,(de)
                and 7             ;Discard everything but ink from tile attributes
                ld b,a
                ld a,(LevelAttrib)
                add a,b
                pop bc
2               ld (hl),a         ;Set our screen to it
                inc de
                inc hl
                djnz .NextCol2         ;Decrement b - If finished all columns on this row, loop back

                ld a,(DrawTile.TileWidth)           ;Move down one row (By moving forward 32 - TileWidth bytes)
                sub 32        ;Instead of subtracting TileWidth from 32, we do the opposite and then
                neg           ; invert the accumulator. This saves us a register.
                add a,l
                ld l,a
                ld a,h
                adc a,0
                ld h,a

                dec c            ;Advance the counter
                jp nz,.NextRow2  ;Loop back for next row
                ret

;---------------------------------------------------------
EraseSquare:
;---------------------------------------------------------
;Erases one 16x16 area of the screen
;If a is 0, replace attribute with level default, otherwise ignore attributes
;Args: b,c = Screen coordinates in pixels to draw to (must be multiple of 8)

                cp 0
                jr nz,.SkipAttributes

                ld d,b
                ld e,c
                call ScreenToAttr
                ld a,(LevelAttrib)
                ld (hl),a
                inc hl
                ld (hl),a
                ld b,0
                ld c,31
                add hl,bc
                ld (hl),a
                inc hl
                ld (hl),a
                ld b,d
                ld c,e
.SkipAttributes:
                call ScreenToMemory
                ld b,16
1               ld (hl),0
                inc hl
                ld (hl),0
                dec hl
                call IncRow
                djnz 1B

                ret