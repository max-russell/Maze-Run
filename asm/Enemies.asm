
;----------------------------------------------------------------------------------------------------
MoveEnemy:
;----------------------------------------------------------------------------------------------------
                 ld a,(hl)         ;If enemy is switched off, quit.
                 and a
                 ret z
                 cp 2
                 ret z

                 push hl           ;Save start of the record location for later
                 inc hl
                 ld b,(hl)         ;Get enemy coordinates into b,c
                 inc hl
                 ld c,(hl)
                 ld de,16
                 add hl,de
                 ld d,(hl)         ;Get type of enemy
                 inc hl
                 ld e,(hl)         ;Get direction

                 ld a,b
                 or c
                 and 15            ;Does x and y coordinate go into 16?
                 jr nz, .MoveForward ;If not, just keep moving in current direction

                 ld a,d               ;Check which type of enemy this is, and gosub an appropriate place.
                 cp ENEMY_ROBOT
                 jp z, .BackandForthMovement
                 cp ENEMY_ANGRYCHEF
                 jp z, .BackandForthMovement
                 cp ENEMY_FUZZY
                 jp z, .FuzzyMovement
                 cp ENEMY_SMASHER
                 jp z, .SmasherMovement
                 cp ENEMY_SLIMER1
                 jp z, .Slimer1Movement
                 cp ENEMY_SLIMER2
                 jp z, .Slimer2Movement
                 cp ENEMY_CREEPYGIRL
                 jp z, .CreepyGirlMovement
                 cp ENEMY_TEDDYHEAD
                 jp z, .TeddyHeadMovement
                 cp ENEMY_PACMAN
                 jp z, .PacmanMovement
                 cp ENEMY_GORGON
                 jp z, .GorgonMovement
                 jp .StayPut

.SlimerChangeDirMoveForward:
                 ld a,e
                 cp DIR_LEFT
                 jr nz,1F
                 ld (hl),e
                 pop hl
                 ld a,2
                 ld bc,gfx_SlimerLeft
                 call SetAnimEnemy
                 jr .MoveForward+1
1                cp DIR_RIGHT
                 jr nz,.ChangeDirMoveForward
                 ld (hl),e
                 pop hl
                 ld a,2
                 ld bc,gfx_SlimerRight
                 call SetAnimEnemy
                 jr .MoveForward+1

.PacmanChangeDirMoveForward:
                 ld a,e
                 cp DIR_LEFT
                 jr nz,1F
                 ld (hl),e
                 pop hl
                 ld a,2
                 ld bc,gfx_PacmanLeft
                 call SetAnimEnemy
                 jr .MoveForward+1
1                cp DIR_RIGHT
                 jr nz,.ChangeDirMoveForward
                 ld (hl),e
                 pop hl
                 ld a,2
                 ld bc,gfx_PacmanRight
                 call SetAnimEnemy
                 jr .MoveForward+1

.ChangeDirMoveForward:
                 ld (hl),e
.MoveForward:
                 pop hl
                 ld a,e              ;Check direction and go to appropriate place.
                 cp DIR_UP
                 jr z,.MoveUp
                 cp DIR_DOWN
                 jr z,.MoveDown
                 cp DIR_LEFT
                 jr z,.MoveLeft
                 cp DIR_RIGHT
                 jr z,.MoveRight
                 jr .StayPut

.MoveUp:         ld b,0              ;Move enemy sprite
                 ld a,d
                 cp ENEMY_SMASHER
                 jr z,1F
                 ld c,-MOVE_RATE
                 call MoveSprite
                 ret
1                ld c,-MOVE_RATE_SLOW
                 call MoveSprite
                 ret
.MoveDown:       ld b,0
                 ld a,d
                 cp ENEMY_SMASHER
                 jr z,1F
                 ld c,MOVE_RATE
                 call MoveSprite
                 ret
1                ld c,MOVE_RATE_FAST
                 call MoveSprite
                 ret
.MoveLeft:       ld b,-MOVE_RATE
                 ld c,0
                 call MoveSprite
                 ret
.MoveRight:      ld b,MOVE_RATE
                 ld c,0
                 call MoveSprite
                 ret

.StayPut:        pop hl
                 ld (hl),3           ;Draw mode set to Erase old & Draw new
.StayPut2:       inc hl
                 ld b,(hl)           ;Retrieve coordinates
                 inc hl
                 ld c,(hl)
                 inc hl
                 ld (hl),b           ;Put coordinates in old coordinates
                 inc hl
                 ld (hl),c
                 inc hl              ;Retrieve screen address
                 ld b,(hl)
                 inc hl
                 ld c,(hl)
                 inc hl
                 inc hl
                 inc hl
                 ld (hl),b           ;Put old screen address in screen address
                 inc hl
                 ld (hl),c
                 ld b,0
                 ld c,9
                 add hl,bc
                 ld (hl),e           ;Direction
                 ret

.BackandForthMovement:                ;Unique behaviour for robot enemy.
                 call CheckMapInDirection
                 and a
                 jr z,.MoveForward
                 rrc e
                 rrc e                ;Reverse direction (by rotating twice)
                 jr .StayPut

.FuzzyMovement:
                 call CheckMapInDirection  ;Try the direction we're already going in
                 and a
                 jr z,.MoveForward         ;If it's free, go for it.
                 ld a,r;(Randomizer)         ;Try right or left, at random.
                 and 1
                 jr z,1F
                 rrc e                     ;Now try to the right
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 rlc e                     ;Try to the left
                 rlc e
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 rlc e
                 jr 2F
1                rlc e                     ;Now try to the left
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 rrc e                     ;Try to the right
                 rrc e
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 rrc e
2                call CheckMapInDirection  ;Ok, turning around
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut               ;Just stay still then.

.SmasherMovement: ld a,e              ;Unique behaviour for robot enemy.
                 cp DIR_UP
                 jr nz, .SmasherDown

                 call CheckMapInDirection
                 and a
                 jr nz,1F
                 ld b,0
                 ld c,-MOVE_RATE_SLOW
                 pop hl
                 call MoveSprite
                 ret
1                ld e,DIR_DOWN
                 jp .StayPut ;.SetDirRight     ;Otherwise, set direction to right
.SmasherDown:
                 call CheckMapInDirection
                 and a
                 jp nz,1F
                 ld b,0
                 ld c,MOVE_RATE_FAST
                 pop hl
                 call MoveSprite
                 ret
1                ld e,DIR_UP
                 jp .StayPut
                 
.Slimer1Movement:
                 rrc e                              ;First check to the right
                 call CheckMapInDirection
                 and a
                 jp z,.SlimerChangeDirMoveForward
                 rlc e
                 call CheckMapInDirection           ;Then check straight ahead.
                 and a
                 jp z,.MoveForward
                 rlc e
                 call CheckMapInDirection           ;Then check to the left
                 and a
                 jp z,.SlimerChangeDirMoveForward
                 rlc e
                 call CheckMapInDirection           ;Then check behind
                 and a
                 jp z,.SlimerChangeDirMoveForward
                 jp .StayPut

.Slimer2Movement:
                 rlc e                              ;First check to the left
                 call CheckMapInDirection
                 and a
                 jp z,.SlimerChangeDirMoveForward
                 rrc e
                 call CheckMapInDirection           ;Then ahead.
                 and a
                 jp z,.MoveForward
                 rrc e
                 call CheckMapInDirection           ;Then right.
                 and a
                 jp z,.SlimerChangeDirMoveForward
                 rrc e
                 call CheckMapInDirection           ;Then behind
                 and a
                 jp z,.SlimerChangeDirMoveForward
                 jp .StayPut
                 
.CreepyGirlMovement:
                 ld e,0
                 ld a,(sprManX)
                 sub b                   ;a = PlayerX - EnemyX
.MOD_GrlMask1:       jr nc,1F           ;If carry flag, EnemyX > PlayerX, player is to the left of enemy    ;NOT CARRY to chase, CARRY to flee
                 cpl                     ;Convert negative value to positive
                 inc e
1                ld d,a
                 ld a,(sprManY)
                 sub c                   ;a = PlayerY - EnemyY
.MOD_GrlMask2:       jr nc,1F           ;If carry flag, EnemyY > PlayerY, player is above enemy             ;NOT CARRY to chase, CARRY to flee
                 cpl
                 inc e
                 inc e
1                cp d                    ;Abs(PlayerY-EnemyY) - Abs(PlayerX-EnemyX)
                 ld d,0
                 ld a,e
                 ld (.WhichWay),a
                 jr c,.CrpGrlTryX        ;If carry flag, x distance is greater than y distance, so do x distance first
.CrpGrlTryY:     ld a,(.WhichWay)
                 and 2                   ;Enemy must move up
                 jr z,1F
                 ld e,DIR_UP
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 ld a,d
                 and a
                 jp nz,.StayPut
                 inc d
                 jp .CrpGrlTryX
1                ld e,DIR_DOWN
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 ld a, d
                 and a
                 jp nz,.StayPut
                 inc d
.CrpGrlTryX:     ld a,(.WhichWay)
                 and 1                   ;Enemy must move left
                 jr z,1F
                 ld e,DIR_LEFT
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 ld a,d
                 and a
                 jp nz,.StayPut
                 inc d
                 jp .CrpGrlTryY
1                ld e,DIR_RIGHT
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 ld a, d
                 and a
                 jp nz,.StayPut
                 inc d
                 jp .CrpGrlTryY
.WhichWay        defb 0

.TeddyHeadMovement:                              ;Chases player, but only if he's in line horizontally or vertically.
                 ld a,(sprManX)
                 cp b                            ;Compare eX to pX
                 jr nz,2F                        ;If not the same skip to vertical check
                 ld a,(sprManY)
                 sub c
.MOD_TedMask1:     jr nc,1F                      ;NOT CARRY for chase, CARRY for flee
                 ld e,DIR_UP
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut
1                ld e,DIR_DOWN
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut

2                ld a,(sprManY)
                 cp c                            ;Compare eY to pY
                 jp nz,.StayPut                  ;If not the same, stay put.
                 ld a,(sprManX)
                 sub b
.MOD_TedMask2:     jr c,1F                         ;CARRY for chase, NOT CARRY for flee
                 ld e,DIR_RIGHT
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut
1                ld e,DIR_LEFT
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut

.PacmanMovement:                                 ;Pacman chases player but Y position stays the same
                 ld a,(sprManX)
                 cp b                            ;Compare pX to enemy X    eX-pX
.MOD_PacMask:        jp z,.StayPut
                     jr nc,1F;nc,1F                  ;NOT CARRY to make Pacman chase, CARRY to make Pacman flee
                 ld e,DIR_LEFT                   ;If eX-pX < 0 Then pX < eX so go left
                 call CheckMapInDirection
                 and a
                 jp z,.PacmanChangeDirMoveForward
                 jp .StayPut
1                ld e,DIR_RIGHT
                 call CheckMapInDirection
                 and a
                 jp z,.PacmanChangeDirMoveForward
                 jp .StayPut

.PacFleeOnly:    ld e,DIR_LEFT
                 call CheckMapInDirection
                 and a
                 jp z,.PacmanChangeDirMoveForward
                 ld e,DIR_RIGHT
                 call CheckMapInDirection
                 and a
                 jp z,.PacmanChangeDirMoveForward
                 jp .StayPut


.GorgonMovement:
                 ld a,(sprManY)
                 cp c
.MOD_GorMask:        jp z,.StayPut
                     jr nc,1F                        ;NOT CARRY to make Gorgon chase, CARRY to make Pacman flee
                 ld e,DIR_UP
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut
1                ld e,DIR_DOWN
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut
.GorFleeOnly:    ld e,DIR_UP
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 ld e,DIR_DOWN
                 call CheckMapInDirection
                 and a
                 jp z,.ChangeDirMoveForward
                 jp .StayPut

;-----------------------------------------------------------------
SetAnimEnemy:
;-----------------------------------------------------------------
;Set animation for sprite record starting at hl, to graphic starting at bc
;a is the number of frames.

                 push de
                 push hl
                 
                 ld de,7
                 add hl,de
                 
                 ;ld e,(hl) ;Store graphic already there to de, & put new graphic in
                 ;ld (hl),c
                 inc hl
                 ;ld d,(hl)
                 ;ld (hl),b
                 inc hl
                 inc hl
                 inc hl
                 ;ld (hl),e ;Put de into old graphic
                 inc hl
                 ;ld (hl),d
                 inc hl
                 inc hl
                 ld d,254    ;Set current frame to -1, so that update frame doesn't update the animation this turn.
                 ld (hl),d
                 inc hl
                 ld (hl),a
                 inc hl
                 ld (hl),c ;Set Base graphic
                 inc hl
                 ld (hl),b
                 pop hl
                 pop de
                 ret
                 
;-----------------------------------------------------------------
CheckCollision:
;-----------------------------------------------------------------
;Check if an enemy (hl) has collided with the player.
;Not pixel perfect so as to not slow the game down too much.

COLLISIONOFFSETX: equ 6
COLLISIONOFFSETY: equ 2

                 ld a,(hl) ;Return if draw mode is set to off, or to erase-only
                 and 1
                 ret z

                 inc hl
                 ld b,(hl)         ;Get enemy coordinates into b,c
                 inc hl
                 ld c,(hl)

                 ;a = px-16
                 ;if a >= b   then No collide

                 ;a = px+16
                 ;If a < b    then No collide
                 
                 ;a = py-16
                 ;If a >= c   then no collide
                 
                 ;a = py+16
                 ;If a < c    then no collide

                 ld a,(sprManX)
                 sub (16-COLLISIONOFFSETX)
                 cp b
                 ret nc

                 add a,(16-COLLISIONOFFSETX)*2
                 cp b
                 ret c
                 
                 ld a,(sprManY)
                 sub (16-COLLISIONOFFSETY)
                 cp c
                 ret nc

                 add a,(16-COLLISIONOFFSETY)*2
                 cp c
                 ret c
                 
                 ld a,(ActiveFX)   ;Check if we've taken a power pill.
                 bit 2,a
                 jr nz,.PowerPill

                 IF NO_COLLISIONS=1
                     ret
                 ELSE
                     call Sound_Mysterious
                     pop af       ;Pop stack pointer off stack, as we're not gosubbing back anymore.
                     jp LevelStartUp
                 ENDIF
                 
.PowerPill
                 dec hl   ;Get back to start of sprite record
                 dec hl
                 ld a,2   ;Set draw mode to erase
                 ld (hl),a
                 ld e,DIR_NONE
                 call MoveEnemy.StayPut2 ;Bit of a shortcut to save repeating a bit from MoveEnemy
                 ld a,(ActiveFX) ;Remove power pill
                 res 2,a
                 ld (ActiveFX),a
                 ld b,232                  ;Clear power pill graphic off panel
                 ld c,104
                 call Draw16x16BoxOpaque

                 call Sound_Alarm

                 ret

;-----------------------------------------------------------------
CheckMapInDirection:
;-----------------------------------------------------------------
;Check the map tile one tile from position b,c in direction e
;Puts in A reg the tile there and sets zero flag if there's nothing blocking

                 push bc
                 push hl
                 ld a,e
                 cp DIR_UP
                 jr nz, 1F
                 ld a,c
                 sub 16
                 ld c,a
                 jr 2F
1                cp DIR_RIGHT
                 jr nz, 1F
                 ld a,b
                 add a,16
                 ld b,a
                 jr 2F
1                cp DIR_DOWN
                 jr nz, 1F
                 ld a,c
                 add a,16
                 ld c,a
                 jr 2F
1                cp DIR_LEFT
                 jr nz, 1F
                 ld a,b
                 sub 16
                 ld b,a
                 jr 2F
1                ld a,0
                 cp 0
                 pop hl
                 pop bc
                 ret
2
                 ld hl,CurrentLevelMap  ;First we get the starting byte on the level map.
                 srl b            ;Shift x-coordinate right 4 times to divide by 16
                 srl b            ;This gets us the right tile across
                 srl b
                 srl b
                 ld a,c
                 and 240          ;AND y coordinate by 240 gets us the right tile down
                 add b            ;Add the x across tile to that
                 ld c,a
                 ld b,0
                 add hl,bc        ;hl should now be right address in the level map
                 ld a,(hl)
                 pop hl
                 pop bc
                 cp 0             ;zero flag is on if there's nothing blocking up, 1 otherwise
                 ret