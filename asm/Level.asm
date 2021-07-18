;---------------------------------------------------------
LoadMaze:
;---------------------------------------------------------
;This basically moves the map for the level we're on (CurrentLevel)
;into the 192 bytes at CurrentLevelMap. This is so we can make changes to the
;map while playing the level and still keep the original data for the level.

                ld hl,LevelMaps      ;Scroll through the level maps until we get
                ld b,0               ;to the one we want.
                ld c,192
                ld a,(CurrentLevel)
1               dec a
                jr z,.DoTheCopy
                add hl,bc
                jr 1B

.DoTheCopy      ld de,CurrentLevelMap
                ldir                  ;This command copies bc (ie,192) bytes from hl to de

                ld hl,CurrentLevelMap
                ld bc,15
                add hl,bc
                ld a,(hl)
                ld (GemsLeft),a

                ret

;---------------------------------------------------------
InitialiseMaze:
;---------------------------------------------------------
;This goes through the level map before the level starts, finds where the enemies have been put
;and converts them into proper sprites, removing them from the level map itself.

                ld a,0
                ld (NoOfEnemies),a ;Reset number of enemies
                ld (ActiveFX),a
                ld (KeysYouGot),a
                call DeactivateScaryMask
                ld de,sprEnemies
                ld hl,CurrentLevelMap
                ld c,0         ;d,e will go through each map coordinate
1               ld b,0
2
                ld a,(hl)      ;Get what is at this square
                cp 255
                jr z,.PlacePlayer
                cp (ENEMY_MAP_OFFSET+1) ;IF it's ENEMY_MAP_OFFSET or greater, it's an enemy,
                call nc,.PlaceEnemy ;so put it down.


.NextSquare     inc hl
                inc b
                ld a,12
                cp b
                jr nz,2B
                inc hl         ;There are four extra bytes at the end of each
                inc hl         ;row that we skip
                inc hl
                inc hl
                inc c
                ld a,12
                cp c
                jr nz,1B
                ex de,hl
                ld (hl),-1     ;Last extra byte of enemy data block should be -1 so when we iterate through
                               ;each enemy in the main game loop we know when we have reached the last one.
                ld a,(CurrentLevel)
                ld d,a
                ld a,1
                ld b,29
                ld c,7
                ld h,BRIGHT+PAPER_BLACK+INK_MAGENTA
                ld l,PAPER_BLACK+INK_MAGENTA
                call PrintNumber

                ld a,(GemsLeft)
                ld d,a
                ld a,1
                ld b,29
                ld c,9
                ld h,BRIGHT+PAPER_BLACK+INK_GREEN
                ld l,PAPER_BLACK+INK_GREEN
                call PrintNumber
                ret



.PlacePlayer
                ld (hl),0
                push bc
                sla b             ;Convert map coordinate to pixel coordinates. (Multiply by 16)
                sla b
                sla b
                sla b
                sla c
                sla c
                sla c
                sla c
                ld a,b
                ld (sprManX),a
                ld a,c
                ld (sprManY),a
                call CoordsToMap
                ld (PlayerMapPos),a
                pop bc
                jr .NextSquare

.PlaceEnemy:
                sub ENEMY_MAP_OFFSET
                ld (.StartingDir),a;.EnemyType),a
                srl a
                srl a
                ld (.EnemyType),a
                ld a,(.StartingDir)
                and 3
                ld (.StartingDir),a

                ld a,(NoOfEnemies)        ;If we have the maximum number of enemies already, don't bother
                cp MAX_ENEMIES
                ret nc

                ld (hl),0         ;Remove baddie from the level map, it's going to have it's own sprite record instead
                push bc           ;Preserve these registers for when we gosub back.
                push hl
                ex de,hl          ;Switch registers so we have position in sprite record array in hl

                sla b             ;Convert map coordinate to pixel coordinates. (Multiply by 16)
                sla b
                sla b
                sla b
                sla c
                sla c
                sla c
                sla c

                inc a
                ld (NoOfEnemies),a        ;Increase NoOfEnemies counter

                ld (hl),1          ;Set draw mode
                inc hl
                ld (hl),b          ;Set x position
                inc hl
                ld (hl),c          ;Set y position
                inc hl
                ld (hl),0          ;old x
                inc hl
                ld (hl),0          ;old y
                inc hl
                ld (hl),$00        ;Screen address : default 16384
                inc hl
                ld (hl),$40
                inc hl
                push de            ;d will temporarily store our sprite colour, e our direction.
                
                ld a,(.StartingDir)
                cp 0
                jr nz,1F
                ld e,DIR_UP
                jr 2F
1               cp 1
                jr nz,1F
                ld e,DIR_DOWN
                jr 2F
1               cp 2
                jr nz,1F
                ld e,DIR_LEFT
                jr 2F
1               ld e,DIR_RIGHT
2
                ld a,(.EnemyType)
                cp ENEMY_ROBOT
                jr nz,.Else1
                ld bc,gfx_RobotRight
                ld d,INK_RED
                ;ld e,DIR_RIGHT
                ld a,2
                jp .EndIf
.Else1          cp ENEMY_ANGRYCHEF
                jr nz,.Else2
                ld bc,gfx_AngryChef
                ld d,INK_WHITE
                ;ld e,DIR_DOWN
                ld a,2
                jr .EndIf
.Else2          cp ENEMY_FUZZY
                jr nz,.Else3
                ld bc,gfx_FuzzyAlien
                ld d,INK_MAGENTA
                ;ld e,DIR_RIGHT
                ld a,2
                jr .EndIf
.Else3          cp ENEMY_SMASHER
                jr nz,.Else4
                ld bc,gfx_Smasher
                ld d,INK_YELLOW
                ;ld e,DIR_UP
                ld a,1
                jr .EndIf
.Else4          cp ENEMY_SLIMER1
                jr nz,.Else5
                ;ld bc,gfx_SlimerRight
                ld d,INK_GREEN
                ;ld e,DIR_RIGHT
                ;ld a,2
                jr .SlimerCont
.Else5          cp ENEMY_SLIMER2
                jr nz,.Else6
                ;ld bc,gfx_SlimerLeft
                ld d,INK_CYAN
                ;ld e,DIR_LEFT
.SlimerCont     ld a,e
                cp DIR_RIGHT
                jr nz,1F
                ld bc,gfx_SlimerRight
                jr 2F
1               ld bc,gfx_SlimerLeft
2               ld a,2
                jr .EndIf
.Else6          cp ENEMY_CREEPYGIRL
                jr nz,.Else7
                ld bc,gfx_CreepyGirl
                ld d,INK_MAGENTA
                ;ld e,DIR_LEFT
                ld a,2
                jr .EndIf
.Else7          cp ENEMY_TEDDYHEAD
                jr nz,.Else8
                ld bc,gfx_TeddyHead
                ld d,INK_YELLOW
                ;ld e,DIR_LEFT
                ld a,2
                jr .EndIf
.Else8          cp ENEMY_PACMAN
                jr nz,.Else9
                ;ld bc,gfx_PacmanRight
                ld a,e
                cp DIR_RIGHT
                jr nz,1F
                ld bc,gfx_PacmanRight
                jr 2F
1               ld bc,gfx_PacmanLeft
2               ld d,INK_YELLOW
                ;ld e,DIR_RIGHT
                ld a,4
                jr .EndIf
.Else9          ld bc,gfx_Gorgon
                ld d,INK_GREEN
                ;ld e,DIR_UP
                ld a,4
.EndIf

                ld (hl),c          ;Graphic
                inc hl
                ld (hl),b
                inc hl
                ld (hl),$00        ;Old Screen address
                inc hl
                ld (hl),$40
                inc hl
                ld (hl),c          ;Old graphic
                inc hl
                ld (hl),b
                inc hl
                ld (hl),d    ;Sprite colour
                inc hl
                ld (hl),0            ;Current Frame no
                inc hl
                ld (hl),a            ;No of frames
                inc hl
                ld (hl),c            ;Base graphic
                inc hl
                ld (hl),b
                inc hl
                ld a,(.EnemyType)
                ld (hl),a            ;Type of enemy
                inc hl

                ld (hl),e            ;Direction
                inc hl               ;Position in enemy sprite record block should now be placed ready at the start
                                     ;of where the next enemy will go.
                pop de
                ex de,hl             ;Get everything back ready for the levelmap loop
                pop hl
                pop bc
                ret

.EnemyType:      defb 0
.StartingDir:    defb 0

;---------------------------------------------------------
DrawMaze:
;---------------------------------------------------------

                ld hl,CurrentLevelMap   ;Get default attribute for level.
                ld bc,12
                add hl,bc
                ld a,(hl)
                ld (LevelAttrib),a

                ld de,CurrentLevelMap
                ld bc,0
.WhatsHere?:    push bc ;Push registers we need so that DrawTile and EraseSquare can use them
                push de
                ld a,(de)
                cp 0       ;If there's no wall here...
                jr z,.EraseaSquare ;Then erase the square
                push hl
                cp 56  ;Carry:Walls NotCarry:Items
                jr nc,1F                           ;Find the right graphic by checking the lookup tables.
                ld hl,.LookupTable_Walls
                dec a ;Less one
                sla a   ;Multiply by 2
                jr 2F
1               ld hl,.LookupTable_Items
                sub 56
                sla a
2               add a,l ;Add to hl
                ld l,a
                ld a,h
                adc a,0
                ld h,a
                ld a,(hl)
                ld e,a
                inc hl
                ld a,(hl)
                ld d,a
                pop hl



.DrawaSquare:
                call DrawTile
                jr .NextSquare
.EraseaSquare:
                ld a,0
                call EraseSquare
.NextSquare:
                pop de  ;Retrieve our old registers
                pop bc
                inc de  ;Move to next byte in the level map
                ld a,b  ;Check if we're at the last column of map
                add a,16
                cp 192
                ld b,a
                jr nz,.WhatsHere?    ;If so skip on
                ld a,c     ;Check if we're at the last row of the map
                add a,16
                cp 192
                ld c,a
                ld b,0
                inc de
                inc de
                inc de
                inc de
                jr nz,.WhatsHere?
                ret

.LookupTable_Walls:
                defw gfx_Wall1 ;1
                defw gfx_Wall2 ;2
                defw gfx_Wall3 ;3

.LookupTable_Items:
                defw gfx_Diamond
                defw gfx_Exit
                defw gfx_Clock
                defw gfx_Coffee
                defw gfx_PowerPill
                defw gfx_ScaryMask
                defw gfx_X
                defw gfx_Weights
                defw gfx_Thief
                defw gfx_Teleport
                defw gfx_Teleport
                defw gfx_Teleport
                defw gfx_Teleport
                defw gfx_Teleport
                defw gfx_Teleport
                defw gfx_KeyRed
                defw gfx_KeyYellow
                defw gfx_KeyBlue
                defw gfx_KeyGreen
                defw gfx_DoorRed
                defw gfx_DoorYellow
                defw gfx_DoorBlue
                defw gfx_DoorGreen


;---------------------------------------------------------
LevelStartMessage:
;---------------------------------------------------------

                ld de,.s_GetReady
                ld b,7
                ld c,7
                ld a,0
                ld h,BRIGHT+PAPER_BLACK+INK_RED
                ld l,PAPER_BLACK+INK_RED
                call Print
                
                ld de,.s_ForLevel
                ld b,6
                ld c,9
                ld a,0
                ld h,BRIGHT+PAPER_BLACK+INK_YELLOW
                ld l,PAPER_BLACK+INK_YELLOW
                call Print

                ld a,(CurrentLevel)
                ld d,a
                ld b,16
                ld c,9
                ld a,1
                ld h,BRIGHT+PAPER_BLACK+INK_YELLOW
                ld l,PAPER_BLACK+INK_YELLOW
                call PrintNumber
                
                ld hl,CurrentLevelMap
                ld de,.s_LevelName1
                ld bc,13
                add hl,bc
                ld b,12
1               ld a,(hl)
                ld (de),a
                ld a,l
                add a,16
                ld l,a
                ld a,h
                adc a,0
                ld h,a
                inc de
                djnz 1B

                ld hl,CurrentLevelMap
                ld de,.s_LevelName2
                ld bc,14
                add hl,bc
                ld b,12
1               ld a,(hl)
                ld (de),a
                ld a,l
                add a,16
                ld l,a
                ld a,h
                adc a,0
                ld h,a
                inc de
                djnz 1B
                
                ld hl,CurrentLevelMap
                ld de,.s_Password
                ld bc,28
                add hl,bc
                ld b,10
1               ld a,(hl)
                ld (de),a
                ld a,l
                add a,16
                ld l,a
                ld a,h
                adc a,0
                ld h,a
                inc de
                djnz 1B
                
                ld de,.s_LevelName1
                ld b,0
                ld c,12
                ld a,0
                ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                ld l,PAPER_BLACK+INK_YELLOW
                call Print

                ld de,.s_LevelName2
                ld b,12
                ld c,12
                ld a,0
                ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                ld l,PAPER_BLACK+INK_YELLOW
                call Print

                ld de,.s_Pass1
                ld b,3
                ld c,14
                ld a,1
                ld h,BRIGHT+PAPER_BLACK+INK_CYAN
                ld l,PAPER_BLACK+INK_CYAN
                call Print

                call Sound_Ascending

                call GetType
                ret


.s_GetReady:    defm "GET READY",0
.s_ForLevel:    defm "FOR LEVEL",0
.s_LevelName1:  defm "            ",0
.s_LevelName2:  defm "            ",0
.s_Pass1:       defm "PASSWORD: "
.s_Password:    defm "          ",0

;-----------------------------------------------------------------
ActivateScaryMask:
;-----------------------------------------------------------------
;Makes enemies that chase the player, run away instead. Done by making
;a few kludgy rewrites to the enemy movement machine code routine itself.
                ld a,$38 ;Z80 code for 'jr c,DIS'
                ld (MoveEnemy.MOD_GrlMask1),a
                ld (MoveEnemy.MOD_GrlMask2),a
                ld (MoveEnemy.MOD_PacMask+3),a
                ld (MoveEnemy.MOD_GorMask+3),a
                ld (MoveEnemy.MOD_TedMask1),a
                ld a,$30 ;Z80 code for 'jr nc,DIS'
                ld (MoveEnemy.MOD_TedMask2),a
                ;ld a,$00 ;Z80 code for 'nop'
                ;ld (MoveEnemy.MOD_PacMask),a
                ;ld (MoveEnemy.MOD_PacMask+1),a
                ;ld (MoveEnemy.MOD_PacMask+2),a
                ;ld (MoveEnemy.MOD_GorMask),a
                ;ld (MoveEnemy.MOD_GorMask+1),a
                ;ld (MoveEnemy.MOD_GorMask+2),a
                ld bc,MoveEnemy.PacFleeOnly ;Get jump address here
                ld a,c
                ld (MoveEnemy.MOD_PacMask+1),a ;Put it back in code
                ld a,b
                ld (MoveEnemy.MOD_PacMask+2),a

                ld bc,MoveEnemy.GorFleeOnly ;Get jump address here
                ld a,c
                ld (MoveEnemy.MOD_GorMask+1),a ;Put it back in code
                ld a,b
                ld (MoveEnemy.MOD_GorMask+2),a
                ret

;-----------------------------------------------------------------
DeactivateScaryMask:
;-----------------------------------------------------------------
;Deactivate the scary mask. Put machine code back to normal.
                ld a,$30 ;Z80 code for 'jr nc,DIS'
                ld (MoveEnemy.MOD_GrlMask1),a
                ld (MoveEnemy.MOD_GrlMask2),a
                ld (MoveEnemy.MOD_PacMask+3),a
                ld (MoveEnemy.MOD_GorMask+3),a
                ld (MoveEnemy.MOD_TedMask1),a
                ld a,$38 ;Z80 code for 'jr c,DIS'
                ld (MoveEnemy.MOD_TedMask2),a
                ;ld a,$CA ;Z80 code for 'jp z,nnnn'
                ;ld (MoveEnemy.MOD_PacMask),a
                ;ld (MoveEnemy.MOD_GorMask),a

                ld bc,MoveEnemy.StayPut ;Get jump address here
                ld a,c
                ld (MoveEnemy.MOD_PacMask+1),a ;Put it back in code
                ld (MoveEnemy.MOD_GorMask+1),a
                ld a,b
                ld (MoveEnemy.MOD_PacMask+2),a
                ld (MoveEnemy.MOD_GorMask+2),a
                ret

;-----------------------------------------------------------------
CoordsToMap:
;-----------------------------------------------------------------
;Converts screen coordinates in b,c to address in map data minus CurrentLevelMap.

                 push bc
                 ;ld hl,CurrentLevelMap  ;First we get the starting byte on the level map.
                 srl b            ;Shift x-coordinate right 4 times to divide by 16
                 srl b            ;This gets us the right tile across
                 srl b
                 srl b
                 ld a,c
                 and 240          ;AND y coordinate by 240 gets us the right tile down
                 add a,b            ;Add the x across tile to that

                 ;ld c,a
                 ;ld b,0
                 ;add hl,bc        ;hl should now be right address in the level map
                 ;ld a,(hl)
                 pop bc
                 ret

;-----------------------------------------------------------------
MapToCoords:
;-----------------------------------------------------------------
;Converts map address in hl, to coordinates in b,c
                 push hl
                 scf
                 ld bc,CurrentLevelMap-1
                 sbc hl,bc
                 ld a,l
                 and 240
                 ld c,a
                 sla l
                 sla l
                 sla l
                 sla l
                 ld b,l
                 pop hl
                 ret


;Coords to pos
;xsquare = (xco >> 4) + CurrentLevelMap
;ysquare = (yco & 240) + CurrentLevelMap

CurrentLevelMap:
          block 192,0
          
LevelName1:     defm "                   ",0
LevelName2:     defm "                   ",0

LevelMaps:
                incbin "Levels1-26.bin"
;Level 1
;          defb  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  BRIGHT+PAPER_BLACK ,' ','N',2 ;<-Gems needed here
;          defb  1, 57,  0,  0,  0,  1,  0, 0,  0,  0,  0,  1, 'C','F','I',20 ;<-Up to 6 teleport coords down the side here
;          defb  1,  2,  2,  0,  1,  1,  0,  3,  0,  0,  0,  1,  'R','I','C',0        ;128=Robot, 129=Chef,130=Fuzzy,131=Smasher, 132=Slimer1
;          defb  1,  0,  2,  0,  0,  0,  0,  0,  0,  2,  0,  1,  'U','S','E',0        ;133=Slimer2 134=CreepyGirl 135=TeddyHead 136=Pacman
;          defb  1,  0,  2, 62,  3,  3,  0,  2,  2,  2,  0,  1,  'D','H',' ',0        ;137=Gorgon
;          defb  1,  0,  2,  0,  3, 56,  0,  0,  0,  2,  0,  1,  'B',' ','P',0
;          defb  1,132,  0,  0,  3,  3,  3,  3,  0,  2,  0,  1,  'U','M','A',0        ;-1=Player Start
;          defb  1,  0,  3,  3,  3,  0,  0,  0,  0,  0,  0,  1,  'C','A','S',0        ;1,2,3..14=Normal walls
;          defb  1,  0,  3, 56,  3,  0,  2, 56,  2,  0,  1,  1,  'K','K','T',0        ;56=Diamond  ;57=Exit
;          defb  1,  0,  3,  3,  3,  0,  2,  2,  2, 77,  1,  1,  'E','E','I',0
;          defb  1,  0,  0,  0,  0,  0, 61,  0,  0,  0, -1,  1,  'T',' ','E',0
;          defb  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,' ','S',0
;Level 2
;          defb  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, BRIGHT+PAPER_BLACK,'G',' ',10
;          defb  1, 0, 0, 0, 0,56, 0,56, 0, 0, 0, 1, 'F','I','F',0
;          defb  1, 0, 0, 0, 0,56, 0, 0, 0, 0, 0, 1, 'I','V','R',0
;          defb  1,134, 0,56,56,56, 0,-1,59, 0, 2, 1, 'S','E','U',0
;          defb  1, 0, 0,56, 0, 0, 0, 0, 0, 0, 0, 1, 'H',' ','I',0
;          defb  1,56,56,56, 71, 72, 73, 74, 0, 0, 0, 1, 'P','M','T',0
;          defb  1, 58, 59, 61, 62,  0,60, 58, 0, 0, 0, 1,'A','E',',',0
;          defb  1, 2, 2, 2, 0, 0, 0, 0,57, 0,137, 1, 'S',' ',' ',0
;          defb  1, 0, 0, 2, 2, 0, 0,56, 0, 0, 0, 1, 'T','S','B',0
;          defb  1,135, 0, 2, 0, 75, 76, 77, 78, 0, 0, 1, 'E','O','A',0
;          defb  1, 0, 0, 2, 2, 0, 0,136, 0, 0, 0, 1, 0,'M','B',0
;          defb  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,'E','Y',0