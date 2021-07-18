
;MOVE RIGHT

;If pX does not go into 16 then
;   Exit: Move right.
;Else
;   If pY goes into 16 then
;      If CheckTile(pX + 16,pY) is clear then
;         Exit: Move right (Activate square at pX+16,py)
;      Else
;         Exit: No Move
;      End If
;   Else
;       pYOffset = pY Mod 16
;       pYRounded = Int(pY / 16) * 16
;       chk1 = CheckTile(pX+16,pYRounded)
;       chk2 = CheckTile(pX+16,pYRounded+16)
;       if chk1 = clear and chk2 = clear then
;          Exit: Move right (Activate squares at pX+16,pYOffset and pX+16,pYOffset+16)
;       Else If chk1 = clear and chk2 = Not clear Then
;          If pYOffset > 8 then
;             Exit: No Move
;          Else
;             Exit: Move up, facing right
;          End If
;       Else if chk1 = Not clear and chk2 = clear
;          If pYOffset < 8 then
;             Exit: No Move
;          Else
;             Exit: Move Down, facing right
;          End If
;       Else if chk1 = not clear and chk2 = not clear then
;          Exit: No move
;       End If
;   End If
;End If

;-----------------------------------------------------------------
TryToMoveUp:
;-----------------------------------------------------------------
                 ld a,(PlayerMapPos)  ;Put player map pos in e
                 ld e,a
                 ld a,(sprManX)    ;Put player coordinates into b,c
                 ld b,a
                 ld a,(sprManY)
                 ld c,a

                 and 15            ;If pY doesn't go into 16...
                 jr z,1F
                   ld a,DIR_UP
                   call MoveUp            ;... Move up, facing left
                   ld a,TRUE                    ;Return success
                   ret

1                ld a,e            ;Take 16 from PlayerMapPos to get tile above player
                 sub 16
                 ld e,a

                 ld a,b            ;If pX goes into 16...
                 and 15
                 jr nz,2F
                   ld a,e
                   call CheckTile  ;Check tile directly above player
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any item above
                     ld a,DIR_UP       ;Move up, facing up.
                     call MoveUp
                     ld a,TRUE
                     ret
1                  ld a,FALSE          ;Blocked, so quit
                   ret

2                ld a,e
                 call CheckTile   ;Check tile above, left of player.
                 and a
                 jr nz,2F
                   ;ld a,e           ;Add 16 to e to go down 1 square.
                   ;add a,16
                   ;ld e,a
                   inc e            ;Add 1 to e to get next tile to right
                   ld a,e
                   call CheckTile   ;Check tile above, further right than player.
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any items in both left and right tiles above.
                     ;ld a,e
                     ;sub 16
                     ;ld e,a
                     dec e
                     ld a,e
                     call CollectItem
                     ld a, DIR_UP  ;Both tiles above are clear
                     call MoveUp
                     ld a,TRUE
                     ret
1                  ld a,b           ;Above tile to left is clear, right is blocked
                   and 15
                   cp SLIDE_DISTANCE
                   ld a,FALSE
                   ret nc             ;Too far down to slide left, so quit.
                   ld a,DIR_UP     ;Slide left (Move left, facing up)
                   call MoveLeft
                   ld a,TRUE
                   ret
2                ;ld a,e             ;Tile to the left above player is blocked.
                 ;add a,16           ;Add 16 to e to go down 1 square.
                 ;ld e,a
                 inc e
                 ld a,e
                 call CheckTile     ;Check tile above to left of player
                 and a
                 ld a,FALSE         ;If both tiles are blocked, quit returning failure
                 ret nz
                 ld a,b             ;Tile above to left is blocked, to right is clear
                 and 15
                 cp 16-SLIDE_DISTANCE
                 ld a,FALSE
                 ret c              ;Too far up to slide right, so quit
                 ld a,DIR_UP
                 call MoveRight      ;Slide down.
                 ld a,TRUE
                 ret

;-----------------------------------------------------------------
TryToMoveDown:
;-----------------------------------------------------------------
                 ld a,(PlayerMapPos)  ;Put player map pos in e
                 ld e,a
                 ld a,(sprManX)    ;Put player coordinates into b,c
                 ld b,a
                 ld a,(sprManY)
                 ld c,a

                 and 15            ;If pY doesn't go into 16...
                 jr z,1F
                   ld a,DIR_DOWN
                   call MoveDown            ;... Move down, facing down
                   ld a,TRUE                    ;Return success
                   ret

1                ld a,e            ;Add 16 to PlayerMapPos to get tile below player
                 add a,16
                 ld e,a

                 ld a,b            ;If pX goes into 16...
                 and 15
                 jr nz,2F
                   ld a,e
                   call CheckTile  ;Check tile directly below player
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any item below
                     ld a,DIR_DOWN       ;Move up, facing up.
                     call MoveDown
                     ld a,TRUE
                     ret
1                  ld a,FALSE          ;Blocked, so quit
                   ret

2                ld a,e
                 call CheckTile   ;Check tile below, left of player.
                 and a
                 jr nz,2F
                   inc e            ;Add 1 to e to get next tile to right
                   ld a,e
                   call CheckTile   ;Check tile below, further right than player.
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any items in both left and right tiles above.
                     dec e
                     ld a,e
                     call CollectItem
                     ld a, DIR_DOWN  ;Both tiles below are clear
                     call MoveDown
                     ld a,TRUE
                     ret
1                  ld a,b           ;Below tile to left is clear, right is blocked
                   and 15
                   cp SLIDE_DISTANCE
                   ld a,FALSE
                   ret nc             ;Too far down to slide left, so quit.
                   ld a,DIR_DOWN     ;Slide left (Move left, facing down)
                   call MoveLeft
                   ld a,TRUE
                   ret
2                inc e               ;Add 1 to e to go across 1 square
                 ld a,e
                 call CheckTile     ;Check tile below to right of player
                 and a
                 ld a,FALSE         ;If both tiles are blocked, quit returning failure
                 ret nz
                 ld a,b             ;Tile above to left is blocked, to right is clear
                 and 15
                 cp 16-SLIDE_DISTANCE
                 ld a,FALSE
                 ret c              ;Too far up to slide right, so quit
                 ld a,DIR_DOWN
                 call MoveRight      ;Slide down.
                 ld a,TRUE
                 ret

;-----------------------------------------------------------------
TryToMoveLeft:
;-----------------------------------------------------------------
;Try to move player left from current position, collecting or triggering anything on the way.
;a register returns whether we succeeded
                 ld a,(PlayerMapPos)  ;Put player map pos in e
                 ld e,a
                 ld a,(sprManY)    ;Put player coordinates into b,c
                 ld c,a
                 ld a,(sprManX)
                 ld b,a

                 and 15            ;If pX doesn't go into 16...
                 jr z,1F
                   ld a,DIR_LEFT
                   call MoveLeft            ;... Move left, facing left
                   ld a,TRUE                    ;Return success
                   ret

1                dec e              ;Take 1 from PlayerMapPos to get tile to left of player

                 ld a,c            ;If pY goes into 16...
                 and 15
                 jr nz,2F
                   ld a,e
                   call CheckTile  ;Check tile to the left of player
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any item to the left
                     ld a,DIR_LEFT     ;Move left, facing left.
                     call MoveLeft
                     ld a,TRUE
                     ret
1                  ld a,FALSE          ;Blocked, so quit
                   ret

2                ld a,e
                 call CheckTile   ;Check tile to the left, higher up than player.
                 and a
                 jr nz,2F
                   ld a,e           ;Add 16 to e to go down 1 square.
                   add a,16
                   ld e,a
                   call CheckTile   ;Check tile to the left, lower down than player.
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any items in both upper and lower tiles to the left.
                     ld a,e
                     sub 16
                     ld e,a
                     call CollectItem
                     ld a, DIR_LEFT  ;Both tiles to the left are clear
                     call MoveLeft
                     ld a,TRUE
                     ret
1                  ld a,c           ;Tile above is clear, below is blocked
                   and 15
                   cp SLIDE_DISTANCE
                   ld a,FALSE
                   ret nc             ;Too far down to slide up, so quit.
                   ld a,DIR_LEFT     ;Slide up.
                   call MoveUp
                   ld a,TRUE
                   ret
2                ld a,e             ;Tile to the left above player is blocked.
                 add a,16           ;Add 16 to e to go down 1 square.
                 ld e,a
                 call CheckTile     ;Check tile to the left lower down than player
                 and a
                 ld a,FALSE         ;If both tiles are blocked, quit returning failure
                 ret nz
                 ld a,c             ;Tile above is blocked, below is clear
                 and 15
                 cp 16-SLIDE_DISTANCE
                 ld a,FALSE
                 ret c              ;Too far up to slide down, so quit
                 ld a,DIR_LEFT
                 call MoveDown      ;Slide down.
                 ld a,TRUE
                 ret

;-----------------------------------------------------------------
TryToMoveRight:
;-----------------------------------------------------------------
;Try to move player right from current position, collecting or triggering anything on the way.
;a register returns whether we succeeded
                 ld a,(PlayerMapPos)  ;Put player map pos in e
                 ld e,a
                 ld a,(sprManY)    ;Put player coordinates into b,c
                 ld c,a
                 ld a,(sprManX)
                 ld b,a

                 and 15            ;If pX doesn't go into 16...
                 jr z,1F
                   ld a,DIR_RIGHT
                   call MoveRight            ;... Move right, facing right
                   ld a,TRUE                    ;Return success
                   ret

1                inc e              ;Add 1 to PlayerMapPos to get tile to right of player

                 ld a,c            ;If pY goes into 16...
                 and 15
                 jr nz,2F
                   ld a,e
                   call CheckTile  ;Check tile to the right of player
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any item to the right
                     ld a,DIR_RIGHT ;If it's clear, move right
                     call MoveRight
                     ld a,TRUE
                     ret
1                  ld a,FALSE          ;Blocked, so quit
                   ret

2                ld a,e
                 call CheckTile   ;Check tile to the right, higher up than player.
                 and a
                 jr nz,2F
                   ld a,e           ;Add 16 to e to go down 1 square.
                   add a,16
                   ld e,a
                   call CheckTile   ;Check tile to the right, lower down than player.
                   and a
                   jr nz,1F
                     ld a,e
                     call CollectItem  ;Collect any items in both tiles to the right.
                     ld a,e
                     sub 16
                     ld e,a
                     call CollectItem
                     ld a, DIR_RIGHT  ;Both tiles to the right are clear
                     call MoveRight
                     ld a,TRUE
                     ret
1                  ld a,c           ;Tile above is clear, below is blocked
                   and 15
                   cp SLIDE_DISTANCE
                   ld a,FALSE
                   ret nc             ;Too far down to slide up, so quit.
                   ld a,DIR_RIGHT     ;Slide up.
                   call MoveUp
                   ld a,TRUE
                   ret
2                ld a,e             ;Tile to the right above player is blocked.
                 add a,16
                 ld e,a
                 call CheckTile     ;Check tile to the right lower down than player
                 and a
                 ld a,FALSE         ;If both tiles are blocked, quit
                 ret nz
                 ld a,c             ;Tile above is blocked, below is clear
                 and 15
                 cp 16-SLIDE_DISTANCE
                 ld a,FALSE
                 ret c              ;Too far up to slide down, so quit
                 ld a,DIR_RIGHT
                 call MoveDown      ;Slide down.
                 ld a,TRUE
                 ret

;-----------------------------------------------------------------
TryToFall:
;-----------------------------------------------------------------
;Involuntary going down movement for if player has collected weights.
                 ld a,(PlayerMapPos)  ;Put player map pos in e
                 ld e,a
                 ld a,(sprManX)    ;Put player coordinates into b,c
                 ld b,a
                 ld a,(sprManY)
                 ld c,a

                 and 15            ;If pY doesn't go into 16...
                 jr z,1F
                   ;and MOVE_RATE_FAST       ;Only fall at MOVE_RATE_FAST if y coordinate
                   ;jr z,4F                  ;goes into MOVE_RATE_FAST. Otherwise use normal speed for now.
                   ;ld d,MOVE_RATE_FAST
                   ld a,DIR_FALL
                   call MoveDown            ;... Move down, facing down
                   ld a,TRUE                    ;Return success
                   ld d,MOVE_RATE
                   ret

1                ld a,e            ;Add 16 to PlayerMapPos to get tile below player
                 add a,16
                 ld e,a

                 ld a,b
                 and 15            ;If pX doesn't go into 16, skip
                 ld a,FALSE
                 jr nz,2F
                 ld a,e
                 call CheckTile  ;Check tile directly below player
                 and a
                 jr nz,1F
                   ld a,e
                   call CollectItem  ;Collect any item below
                   ld a,DIR_FALL       ;Move down, falling.
                   ld d,MOVE_RATE_FAST
                   call MoveDown
                   ld d,MOVE_RATE
                   ld a,TRUE
                   ret
1                ld a,FALSE          ;Blocked, so quit
                 ret
                 
2                ld a,e
                 call CheckTile   ;Check tile below, left of player.
                 and a
                 ld a,FALSE
                 ret nz
                   inc e          ;Add 1 to e to get next tile to right
                   ld a,e
                   call CheckTile   ;Check tile below, further right than player.
                   and a
                   ld a,FALSE
                   ret nz
                     ld a,e            ;Both tiles are free
                     call CollectItem  ;Collect any items in both left and right tiles above.
                     dec e
                     ld a,e
                     call CollectItem
                     ld a,DIR_FALL    ;Both tiles below are clear
                     ld d,MOVE_RATE_FAST
                     call MoveDown
                     ld d,MOVE_RATE
                     ld a,TRUE
                     ret

;-----------------------------------------------------------------
CheckTile:
;-----------------------------------------------------------------
;Check the map tile at address a + CurrentLevelMap
;Returns 0 if nothing is blocking the player, non-zero otherwise.

                 ld hl,CurrentLevelMap  ;First we get the starting byte on the level map.
                 add a,l  ;Add a to it
                 ld l,a
                 ld a,h
                 adc a,0
                 ld h,a
                 ld a,(hl)
                 ;cp 0             ;If space is completely empty, quit with a=0
                 ;ret z
                 cp 56            ;If space has no collectable, quit
                 ret c

                 cp ITEM_EXIT     ;If the exit is here, quit with a=0 only if we have collected all the gems we need
                 jr nz,1F
                 ld a,(GemsLeft)
                 and a
                 ret
1                cp ITEM_REDDOOR
                 jr nz,1F
                 ;call Sound_Laser
                 ld a,(KeysYouGot)
                 cpl              ;Invert
                 ;bit 0,a
                 and 1

                 ret
1                cp ITEM_YELLOWDOOR
                 jr nz,1F
                 ;call Sound_Laser
                 ld a,(KeysYouGot)
                 cpl              ;Invert
                 ;bit 1,a
                 and 2
                 ret
1                cp ITEM_BLUEDOOR
                 jr nz,1F
                 ;call Sound_Laser
                 ld a,(KeysYouGot)
                 cpl              ;Invert
                 ;bit 2,a
                 and 4
                 ret
1                cp ITEM_GREENDOOR
                 jr nz,1F
                 ;call Sound_Laser
                 ld a,(KeysYouGot)
                 cpl              ;Invert
                 ;bit 3,a
                 and 8
                 ret
1                ld a,0
                 ret

                 ;sub 16             ;zero flag is on if there's nothing blocking up, 1 otherwise
                 ;ret c              ;Quit with non-zero if wall here
                 ;ld a,0             ;If other collectable here, quit with 0
                 ;ret

;-----------------------------------------------------------------
DoNotMove:
;-----------------------------------------------------------------
                 ld b,0
                 ld c,0
                 ld a,DIR_NONE
                 ld hl,ManDir
                 cp (hl)             ;Check if we are already going in this direction
                 jp z,1F            ;If so, no need to do the following, so skip.
                   ld (ManDir),a       ;Set direction
                   ld hl,gfx_ManStill
                   ld a,1
                   call SetPlayerAnim.Set  ;Because we set hl to the right address ourselves, we go straight to Set
1                ld hl,sprMan
                 call MoveSprite
                 ret

;-----------------------------------------------------------------
MoveUp:
;-----------------------------------------------------------------
;Move up, by standard move rate
                 ld e,a

                 ;ld a,d              ;Proper Wall & Item collisions depend on the player's coordinates
                 ;cp MOVE_RATE        ;being a multiple of the current movement rate. If we've just
                 ;jp z,.NormalMove    ;just changed to fast movement that might not be the case.
                 ;ld a,(sprManY)      ;So this bit adjusts things to cover this eventuality.
                 ;and MOVE_RATE_FAST
                 ;jr z,.NormalMove
                 ;ld d,MOVE_RATE

.NormalMove      ld a,(sprManY)
                 and 15               ;If player has moved into next square, update PlayerMapPos
                 sub d;MOVE_RATE
                 jr nc, 1F
                 ld a,(PlayerMapPos)
                 sub 16
                 ld (PlayerMapPos),a

1                ld b,0
                 ld a,d
                 neg
                 ld c,a
                 ld a,e
                 ld hl,ManDir
                 cp (hl)              ;Check if we are already going in this direction
                 jr z,1F              ;If so, no need to do the following, so skip.
                 ld (ManDir),a        ;Set animation for new direction
                 call SetPlayerAnim
1                ld hl,sprMan         ;Do the move on the player sprite.
                 call MoveSprite
                 ret

;-----------------------------------------------------------------
MoveDown:
;-----------------------------------------------------------------
;Move down, by standard move rate
                 ld e,a
                 ld a,(sprManY)
                 and 15               ;If player has moved into next square, update PlayerMapPos
                 add a,d;MOVE_RATE
                 cp 16
                 jr c, 1F
                 ld a,(PlayerMapPos)
                 add a,16
                 ld (PlayerMapPos),a

1                ld b,0
                 ld c,d;MOVE_RATE
                 ld a,e
                 ld hl,ManDir
                 cp (hl)              ;Check if we are already going in this direction
                 jr z,1F              ;If so, no need to do the following, so skip.
                 ld (ManDir),a        ;Set animation for new direction
                 call SetPlayerAnim
1                ld hl,sprMan         ;Do the move on the player sprite.
                 call MoveSprite
                 ret

;-----------------------------------------------------------------
MoveLeft:
;-----------------------------------------------------------------
;Move left, by standard move rate
                 ld e,a
                 ld a,(sprManX)
                 and 15               ;If player has moved into next square, update PlayerMapPos
                 sub d;MOVE_RATE
                 ;cp 16
                 jr nc, 1F
                 ld a,(PlayerMapPos)
                 dec a
                 ld (PlayerMapPos),a

1                ;ld b,-MOVE_RATE
                 ld a,d
                 neg
                 ld b,a
                 ld c,0
                 ld a,e
                 ld hl,ManDir
                 cp (hl)              ;Check if we are already going in this direction
                 jr z,1F              ;If so, no need to do the following, so skip.
                 ld (ManDir),a        ;Set animation for new direction
                 call SetPlayerAnim
1                ld hl,sprMan         ;Do the move on the player sprite.
                 call MoveSprite
                 ret

;-----------------------------------------------------------------
MoveRight:
;-----------------------------------------------------------------
;Move right, by standard move rate
                 ld e,a
                 ld a,(sprManX)
                 and 15               ;If player has moved into next square, update PlayerMapPos
                 add a,d;MOVE_RATE
                 cp 16
                 jr c, 1F
                 ld a,(PlayerMapPos)
                 inc a
                 ld (PlayerMapPos),a

1                ld b,d;MOVE_RATE
                 ld c,0
                 ld a,e
                 ld hl,ManDir
                 cp (hl)              ;Check if we are already going in this direction
                 jr z,1F              ;If so, no need to do the following, so skip.
                 ld (ManDir),a        ;Set animation for new direction
                 call SetPlayerAnim
1                ld hl,sprMan         ;Do the move on the player sprite.
                 call MoveSprite
                 ret



;-----------------------------------------------------------------
SetPlayerAnim:
;-----------------------------------------------------------------
;Changes player's animation to the sequence for direction a

                cp DIR_UP
                jr nz,1F
                ld hl,gfx_ManRunUp
                ld a,5
                jr .Set
1               cp DIR_DOWN
                jr nz,1F
                ld hl,gfx_ManRunDown
                ld a,5
                jr .Set
1               cp DIR_LEFT
                jr nz,1F
                ld hl,gfx_ManRunLeft
                ld a,5
                jr .Set
1               cp DIR_RIGHT
                jr nz,1F
                ld hl,gfx_ManRunRight
                ld a,5
                jr .Set
1               cp DIR_FALL
                jr nz,1F
                ld hl,gfx_ManFall
                ld a,2
                jr .Set
1               ld hl,gfx_ManStill
                ld a,1

.Set            ld (sprManFrms),a   ;Set no of frames
                ld (sprManGfx),hl   ;Set graphic
                ld (sprManGfxB),hl  ;Set base graphic
                ld a,4
                ld (FrameCycle),a   ;Set FrameCycle
                ld a,0
                ld (sprManFrmNo),a  ;Put us on the first frame
                ret

;-----------------------------------------------------------------
CollectItem:
;-----------------------------------------------------------------

                 ld hl,CurrentLevelMap  ;First we get the starting byte on the level map.
                 add a,l  ;Add a to it
                 ld l,a
                 ld a,h
                 adc a,0
                 ld h,a
                 ld a,(hl)

                 cp 56                   ;Quit if nothing here to collect
                 ret c
                 sub 56
                 sla a
                 push de
                 push hl
                 ld de,.ChangeMe+1       ;This clever bit alters the machine code for the call below
                 ld hl,.LookupTable      ;so that it gosubs to the correct label from the lookup table.
                 add a,l ;Add to hl
                 ld l,a
                 ld a,h
                 adc a,0
                 ld h,a
                 ld a,(hl)
                 ld (de),a
                 inc de
                 inc hl
                 ld a,(hl)
                 ld (de),a
                 pop hl
.ChangeMe        call $0000
                 pop de
                 ret
                 
.LookupTable     
                 defw CollectDiamond
                 defw ReachExit
                 defw CollectClock
                 defw CollectCoffee
                 defw CollectPowerPill
                 defw CollectScaryMask
                 defw CollectX
                 defw CollectWeights
                 defw CollectThief
                 defw ReachTeleport1
                 defw ReachTeleport2
                 defw ReachTeleport3
                 defw ReachTeleport4
                 defw ReachTeleport5
                 defw ReachTeleport6
                 defw CollectRedKey
                 defw CollectYellowKey
                 defw CollectBlueKey
                 defw CollectGreenKey
                 defw EraseItem ;RedDoor
                 defw EraseItem ;YellowDoor
                 defw EraseItem ;BlueDoor
                 defw EraseItem ;GreenDoor



;-----------------------------------------------------------------
CollectDiamond:
;-----------------------------------------------------------------
                 ld (hl),0         ;Erase diamond from map
                 call MapToCoords
                 ;push de
                 call EraseSquare
                 ;pop de
                 ;What happens when player collects a diamond.
                 ld a,(GemsLeft)
                 cp 0    ;Is gems needed already on 0?
                 ret z
                 dec a   ;If not, decrement gems needed.
                 ld (GemsLeft),a
                 ld a,1
                 ld b,232
                 ld c,72
                 ;push de
                 call EraseSquare
                 ld a,(GemsLeft)
                 ld d,a  ;Update number on panel.
                 ld a,1
                 ld b,29
                 ld c,9
                 ld h,BRIGHT+PAPER_BLACK+INK_GREEN
                 ld l,PAPER_BLACK+INK_GREEN
                 call PrintNumber
                 
                 call Sound_Ascending2

                 ld a,(GemsLeft)
                 cp 0
                 call z,ActivateExit ;If we now have all the gems, activate the exit.
                 ;pop de

                 ret
                 
;-----------------------------------------------------------------
CollectClock:
;-----------------------------------------------------------------
                 call EraseItem
                 ld a,(ActiveFX)
                 set 0,a
                 ld (ActiveFX),a
                 ;push de
                 ld b,200                  ;Draw graphic onto panel.
                 ld c,104
                 ld de,gfx_Clock
                 call DrawTileNoAttributes
                 ld b,200                  ;With a box around it.
                 ld c,104
                 call Draw16x16Box
                 call Sound_Peow
                 ;pop de
                 ret
                 

                 
;-----------------------------------------------------------------
CollectCoffee:
;-----------------------------------------------------------------
                 call EraseItem
                 ld a,(ActiveFX)
                 set 1,a
                 ld (ActiveFX),a
                 
                 pop af         ;Because we're changing movement rate (in d) right now, we need to get it off the stack.
                 pop de         ; - Here it is.

                 ld d,MOVE_RATE_FAST ;Set this so we go at double speed straight away.
                 ;push de
                 ld b,216
                 ld c,104
                 ld de,gfx_Coffee
                 call DrawTileNoAttributes
                 ld b,216
                 ld c,104
                 call Draw16x16Box

                 ld a,(sprManX)           ;Align sprite
                 and 256-MOVE_RATE_FAST
                 ld b,a
                 ld a,(sprManY)
                 and 256-MOVE_RATE_FAST
                 ld c,a
                 ld hl,sprMan         ;Do the move on the player sprite.
                 call MoveSpriteAbsolute
                 call Sound_Peow
                 ;pop de
                 pop af        ;pop off return to TryToMove_
                 ld a,TRUE
                 ret           ;Return directly to main game loop.

;-----------------------------------------------------------------
CollectPowerPill:
;-----------------------------------------------------------------
                 call EraseItem
                 ld a,(ActiveFX)
                 set 2,a
                 ld (ActiveFX),a
                 ;push de
                 ld b,232
                 ld c,104
                 ld de,gfx_PowerPill
                 call DrawTileNoAttributes
                 ld b,232
                 ld c,104
                 call Draw16x16Box
                 call Sound_Peow
                 ;pop de
                 ret
                 
;-----------------------------------------------------------------
CollectScaryMask:
;-----------------------------------------------------------------
                 call EraseItem
                 ld a,(ActiveFX)
                 set 3,a
                 ld (ActiveFX),a
                 call ActivateScaryMask
                 ;push de
                 ld b,200
                 ld c,120
                 ld de,gfx_ScaryMask
                 call DrawTileNoAttributes
                 ld b,200
                 ld c,120
                 call Draw16x16Box
                 call Sound_Peow
                 ;pop de
                 ret
                 
;-----------------------------------------------------------------
CollectX:
;-----------------------------------------------------------------
                 call EraseItem
                 ld a,0
                 ld (ActiveFX),a        ;Cancel all effects
                 call DeactivateScaryMask
                 ;push de
                 ld b,200
                 ld c,104
                 call Draw16x16BoxOpaque
                 ld b,216
                 ld c,104
                 call Draw16x16BoxOpaque
                 ld b,232
                 ld c,104
                 call Draw16x16BoxOpaque
                 ld b,200
                 ld c,120
                 call Draw16x16BoxOpaque
                 ld b,216
                 ld c,120
                 call Draw16x16BoxOpaque
                 ld b,232
                 ld c,120
                 call Draw16x16BoxOpaque
                 call Sound_Peow
                 ;pop de
                 ret
                 
;-----------------------------------------------------------------
CollectWeights:
;-----------------------------------------------------------------
                 call EraseItem
                 ld a,(ActiveFX)
                 set 4,a
                 ld (ActiveFX),a

                 pop af
                 pop de

                 ld b,216
                 ld c,120
                 ld de,gfx_Weights
                 call DrawTileNoAttributes
                 ld b,216
                 ld c,120
                 call Draw16x16Box

                 ld a,(sprManX)           ;Align sprite on y
                 ld b,a
                 ld a,(sprManY)
                 and 256-MOVE_RATE_FAST
                 ld c,a
                 ld hl,sprMan
                 call MoveSpriteAbsolute
                 call Sound_Peow

                 pop af        ;pop off return to TryToMove_
                 ld a,TRUE
                 ret           ;return direct to main game loop

                 
;-----------------------------------------------------------------
CollectThief:
;-----------------------------------------------------------------
                 call EraseItem
                 ld a,0
                 ld (KeysYouGot),a
                 ld b,200
                 ld c,144
                 call Draw16x16BoxOpaque
                 ld b,216
                 ld c,144
                 call Draw16x16BoxOpaque
                 ld b,232
                 ld c,144
                 call Draw16x16BoxOpaque
                 ld b,200
                 ld c,160
                 call Draw16x16BoxOpaque
                 call Sound_Peow
                 ret
                 
;-----------------------------------------------------------------
CollectRedKey:
;-----------------------------------------------------------------
                 ld a,(KeysYouGot)
                 set 0,a
                 ld (KeysYouGot),a
                 call EraseItem
                 ld b,200
                 ld c,144
                 ld de,gfx_KeyRed
                 call DrawTileKeyOnPanel
                 ld b,200
                 ld c,144
                 call Draw16x16Box
                 call Sound_Phasor
                 ret

;-----------------------------------------------------------------
CollectYellowKey:
;-----------------------------------------------------------------
                 ld a,(KeysYouGot)
                 set 1,a
                 ld (KeysYouGot),a
                 call EraseItem
                 ld b,216
                 ld c,144
                 ld de,gfx_KeyYellow
                 call DrawTileKeyOnPanel
                 ld b,216
                 ld c,144
                 call Draw16x16Box
                 call Sound_Phasor
                 ret
;-----------------------------------------------------------------
CollectBlueKey:
;-----------------------------------------------------------------
                 ld a,(KeysYouGot)
                 set 2,a
                 ld (KeysYouGot),a
                 call EraseItem
                 ld b,232
                 ld c,144
                 ld de,gfx_KeyBlue
                 call DrawTileKeyOnPanel
                 ld b,232
                 ld c,144
                 call Draw16x16Box
                 call Sound_Phasor
                 ret
;-----------------------------------------------------------------
CollectGreenKey:
;-----------------------------------------------------------------
                 ld a,(KeysYouGot)
                 set 3,a
                 ld (KeysYouGot),a
                 call EraseItem
                 ld b,200
                 ld c,160
                 ld de,gfx_KeyGreen
                 call DrawTileKeyOnPanel
                 ld b,200
                 ld c,160
                 call Draw16x16Box
                 call Sound_Phasor
                 ret

;-----------------------------------------------------------------
ReachTeleport1:
;-----------------------------------------------------------------
                 ld bc,31
                 jr ReachTeleport
;-----------------------------------------------------------------
ReachTeleport2:
;-----------------------------------------------------------------
                 ld bc,47
                 jr ReachTeleport
;-----------------------------------------------------------------
ReachTeleport3:
;-----------------------------------------------------------------
                 ld bc,63
                 jr ReachTeleport
;-----------------------------------------------------------------
ReachTeleport4:
;-----------------------------------------------------------------
                 ld bc,79
                 jr ReachTeleport
;-----------------------------------------------------------------
ReachTeleport5:
;-----------------------------------------------------------------
                 ld bc,95
                 jr ReachTeleport
;-----------------------------------------------------------------
ReachTeleport6:
;-----------------------------------------------------------------
                 ld bc,111
                 jr ReachTeleport
;-----------------------------------------------------------------
ReachTeleport:
;-----------------------------------------------------------------
                 ld hl,CurrentLevelMap
                 add hl,bc
                 ld a,(hl)
                 ld (PlayerMapPos),a
                 and 240
                 ld c,a
                 ld b,(hl)
                 sla b
                 sla b
                 sla b
                 sla b
                 ld hl,sprMan
                 call MoveSpriteAbsolute ;Move sprite to new coordinates
                 call Sound_UFO
                 pop af                  ;Cancel CollectItem gosub return
                 pop de                  ;Pop de we put on stack in CollectItem
                 pop af                  ;Cancel TryToMove__ gosub return
                 ld a,TRUE               ;Return that TryToMove___ has been successful.
                 ret

;-----------------------------------------------------------------
EraseItem:
;-----------------------------------------------------------------
                 ld (hl),0         ;Erase thing from map
                 call MapToCoords
                 ;push de
                 call EraseSquare
                 ;pop de
                 ret

;-----------------------------------------------------------------
ActivateExit:
;-----------------------------------------------------------------

                 ld hl,CurrentLevelMap
                 ld de,ATTRIBUTES

                 ld c,0
.Loop2:          ld b,0

.Loop1:          ld a,(hl)    ;Check this square on map
                 cp ITEM_EXIT         ;Is it an exit?
                 jr nz,1F

                 ld a,(de)            ;Make the square flash.
                 or FLASH             ;4 8x8 chars.
                 ld (de),a
                 inc de
                 ld a,(de)
                 or FLASH
                 ld (de),a
                 ld a,e
                 add a,31
                 ld e,a
                 ld a,d
                 adc a,0
                 ld d,a
                 ld a,(de)
                 or FLASH
                 ld (de),a
                 inc de
                 ld a,(de)
                 or FLASH
                 ld (de),a

                 ld a,e
                 sub 33
                 ld e,a
                 ld a,d
                 sbc a,0
                 ld d,a

1                inc b
                 inc de
                 inc de
                 inc hl
                 ld a,b
                 cp 12
                 jr nz,.Loop1

                 inc hl
                 inc hl
                 inc hl
                 inc hl
                 ld a,e
                 add a,40
                 ld e,a
                 ld a,d
                 adc a,0
                 ld d,a
                 inc c

                 ld a,c
                 cp 12
                 jr nz,.Loop2

                 ret

;-----------------------------------------------------------------
ReachExit:
;-----------------------------------------------------------------
                 call Sound_Bird

                 pop af           ;Cancel all the gosubs we should be in.
                 pop de
                 pop af
                 pop af
                 ld a,(CurrentLevel) ;Go to next level
                 cp NO_OF_LEVELS
                 jr z,WinTheGame
                 inc a
                 ld (CurrentLevel),a
                 jp LevelStartUp  ;Restart from top

;-----------------------------------------------------------------
WinTheGame
;-----------------------------------------------------------------
                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen
                 
                 ld de,.s_1
                 ld b,0
                 ld c,10
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call PrintMultiLine

                 call GetType
                 jp RestartGame

                ;         1         2         3
                ;12345678901234567890123456789012
.s_1:      defm " WELL I'LL BE! YOU'VE ONLY JUST ",0
           defm "  GONE AND WON THE ENTIRE GAME! ",0
           defm " ",0
           defm "     NOW GO AND DO SOMETHING    ",0
           defm "    WORTHWHILE WITH YOUR LIFE   ",0
           defm "       YOU MISERABLE FOOL.",0,0

