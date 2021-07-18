SCREEN:          equ 16384
ATTRIBUTES:      equ 22528

;Attribute constants
FLASH:           equ 128
BRIGHT:          equ 64
INK_BLACK:       equ 0
INK_BLUE:        equ 1
INK_RED:         equ 2
INK_MAGENTA:     equ 3
INK_GREEN:       equ 4
INK_CYAN:        equ 5
INK_YELLOW:      equ 6
INK_WHITE:       equ 7
PAPER_BLACK:     equ 0
PAPER_BLUE:      equ 8
PAPER_RED:       equ 16
PAPER_MAGENTA:   equ 24
PAPER_GREEN:     equ 32
PAPER_CYAN:      equ 40
PAPER_YELLOW:    equ 48
PAPER_WHITE:     equ 56

;Direction constants
DIR_NONE:         equ 0
DIR_UP:           equ 10001000b ;By giving the direction constants these values, we can literally
DIR_RIGHT:        equ 01000100b ;rotate something's direction left or right by rotating the bits
DIR_DOWN:         equ 00100010b ;in the byte left or right. Clever, huh?
DIR_LEFT:         equ 00010001b
DIR_FALL:         equ 255


;Enemy constants
ENEMY_MAP_OFFSET  equ 96;127 ;Add this to the enemy constants below to get what each enemy is stored as on the map
ENEMY_ROBOT       equ 0;1
ENEMY_ANGRYCHEF   equ 1;2
ENEMY_FUZZY       equ 2;3
ENEMY_SMASHER     equ 3;4
ENEMY_SLIMER1     equ 4;5
ENEMY_SLIMER2     equ 5;6
ENEMY_CREEPYGIRL  equ 6;7
ENEMY_TEDDYHEAD   equ 7;8
ENEMY_PACMAN      equ 8;9
ENEMY_GORGON      equ 9;10

ITEM_DIAMOND      equ 56
ITEM_EXIT         equ 57
ITEM_CLOCK        equ 58  ;*
ITEM_COFFEE       equ 59  ;*
ITEM_POWERPILL    equ 60  ;*
ITEM_SCARYMASK    equ 61  ;*
ITEM_X            equ 62
ITEM_WEIGHTS      equ 63  ;*
ITEM_THIEF        equ 64
ITEM_TELEPORT1    equ 65
ITEM_TELEPORT2    equ 66
ITEM_TELEPORT3    equ 67
ITEM_TELEPORT4    equ 68
ITEM_TELEPORT5    equ 69
ITEM_TELEPORT6    equ 70
ITEM_REDKEY       equ 71
ITEM_YELLOWKEY    equ 72
ITEM_BLUEKEY      equ 73
ITEM_GREENKEY     equ 74
ITEM_REDDOOR      equ 75
ITEM_YELLOWDOOR   equ 76
ITEM_BLUEDOOR     equ 77
ITEM_GREENDOOR    equ 78

;Misc constants
MOVE_RATE:        equ 2         ;How much sprites move (normal)
MOVE_RATE_SLOW:   equ 1         ;How much extra slow things move
MOVE_RATE_FAST:   equ 4         ;How much extra fast things move
MAX_ENEMIES:      equ 7         ;Number of enemies allowed per level
STARTING_LEVEL:   equ 1         ;Level we start on.
NO_OF_LEVELS:     equ 26         ;How many levels there are.
NO_COLLISIONS:    equ 0         ;Set to 1 and player can pass through enemies.
SLIDE_DISTANCE:   equ 12        ;How much the player will automatically slide round corners.
FALSE:            equ 0
TRUE:             equ 1

;System Variables
LAST_K:          equ $5C08
BORDCR:          equ $5C48

                 org 32768;30000
                 ei                     ;Enable interrupts
                 call WaitVBL
                 ld a,INK_BLACK         ;Set border colour to black
                 out (254),a
                 ld (BORDCR),a          ;Set border colour in system variable. (This is so we keep the right border colour
                                        ;when we run the Beep routine from ROM.


RestartGame:     ld a,STARTING_LEVEL
                 ld (CurrentLevel),a

                 call DoTitleScreen

LevelStartUp:    call WaitVBL
                 ;ld a,INK_BLACK         ;Set border colour to yellow
                 ;out (254),a

                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen

                 ld b,192        ;Set panel graphic coordinates
                 ld c,0
                 ld de,gfx_Panel
                 call DrawTile

                 call LoadMaze
                 call InitialiseMaze
                 call LevelStartMessage
                 call DrawMaze

                 ld a,DIR_NONE
                 ld (ManDir),a
                 ;ld hl,gfx_ManStill
                 ;ld a,1
                 call SetPlayerAnim
                 ld hl,sprMan
                 call PlaceSprite
                 ;call DrawSprite

                 ld hl,sprEnemies
1                ld a,-1
                 cp (hl)
                 jr z, 1F
                 ;push hl
                 call PlaceSprite
                 ;call DrawSprite
                 ;pop hl
                 ld de,SPRITE_RECORD_SIZE
                 add hl,de
                 jr 1B
1
                 ld a,4
                 ld (FrameCycle),a

                 ;call WaitVBL

MainGameLoop:

;--------------------------------------------------DRAW STUFF TO SCREEN----------------------------------------------------------

                 ld hl,sprEnemies
1                ld a,-1
                 cp (hl)        
                 jr z, 1F;.FinishDrawStuff
                 push hl
                 call DrawSprite
                 pop hl
                 ld de,SPRITE_RECORD_SIZE
                 add hl,de
                 jr 1B

1                ld hl,sprMan
                 call DrawSprite
.FinishDrawStuff
                 call WaitVBL

;-------------------------------------------------CHECK FOR COLLISIONS-----------------------------------------------------------
                 ld hl,sprEnemies
1                ld a,-1
                 cp (hl)
                 jr z, .FinishColCheck
                 push hl
                 call CheckCollision
                 pop hl
                 ld de,SPRITE_RECORD_SIZE
                 add hl,de
                 jr 1B
.FinishColCheck

;-----------------------------CHECK FOR PAUSE, RESTART OR QUIT-----------------------------------------------------

                 ld hl,KEY_PAUSE
                 call TestKey
                 jr nz,1F
                 call GetType
1
                 ld hl,KEY_RESTART
                 call TestKey
                 jp z,LevelStartUp
                 
                 ld hl,KEY_QUIT
                 call TestKey
                 jp z,RestartGame

;-----------------------------MOVE PLAYER--------------------------------------------------------------------------
                 ld a,(ActiveFX)          ;If player is currently high on caffeine, go at double movement speed.
                 bit 1,a                  ;Keep speed we should be going at in d register.
                 jr z,1F
                 ld d,MOVE_RATE_FAST
                 jr .CheckWeights
1                ld d,MOVE_RATE

.CheckWeights:   bit 4,a
                 jp z,.TryUp              ;Special Player movement for carrying weights.
                 call TryToFall           ;Try to go down, regardless of whether pressing a key.
                 and a                    ;TryToFall is different from TryToMoveDown because there's no sliding.
                 jr nz,.FinishMovePlayer
                 jr .TryLeft               ;Otherwise, player can go left or right, but not up.

.TryUp:          ld hl,KEY_UP
                 call TestKey
                 jr nz, .TryDown
                 call TryToMoveUp
                 and a
                 jr nz,.FinishMovePlayer

.TryDown:        ld hl,KEY_DOWN
                 call TestKey
                 jr nz,.TryLeft
                 call TryToMoveDown
                 and a
                 jr nz,.FinishMovePlayer

.TryLeft:        ld hl,KEY_LEFT
                 call TestKey
                 jr nz,.TryRight
                 call TryToMoveLeft
                 and a
                 jr nz,.FinishMovePlayer

.TryRight:       ld hl,KEY_RIGHT
                 call TestKey
                 jr nz,.TryNothing
                 call TryToMoveRight
                 and a
                 call z,DoNotMove
                 jr .FinishMovePlayer

.TryNothing:     call DoNotMove
.FinishMovePlayer:

;--------------------------------------------------MOVE ENEMIES------------------------------------------------------------------

                 ld a,(ActiveFX)             ;Skip if clock active
                 bit 0,a
                 jr nz, .FinishMoveEnemies

                 ld hl,sprEnemies
1                ld a,-1
                 cp (hl)
                 jr z, .FinishMoveEnemies
                 push hl
                 call MoveEnemy
                 pop hl
                 ld de,SPRITE_RECORD_SIZE
                 add hl,de
                 jr 1B

.FinishMoveEnemies:

                   call WaitVBL
;--------------------------------------------------UPDATE ANIMATION FRAMES-------------------------------------------------------

                 ld a,(FrameCycle)
                 dec a
                 ld (FrameCycle),a
                 jr nz,.FinishUpdateAnims
                 ld a,4
                 ld (FrameCycle),a
                 ld hl,sprMan
                 call NextFrame

                 ld a,(ActiveFX)             ;Skip if clock active
                 bit 0,a
                 jr nz, .FinishUpdateAnims

                 ld hl,sprEnemies
1                ld a,-1
                 cp (hl)
                 jr z, .FinishUpdateAnims
                 call NextFrame
                 ld de,SPRITE_RECORD_SIZE
                 add hl,de
                 jr 1B
.FinishUpdateAnims

;------------------------------------------------------------------------------------------------------------------------------
                 jp MainGameLoop

;---------------------------------------------------^ END OF MAIN LOOP ^-------------------------------------------------------

                include "Player.asm"
                include "Enemies.asm"
                include "Level.asm"
                include "Sprites.asm"
                include "Tiles.asm"
                include "Text.asm"
                include "TitleScreen.asm"
                include "Misc.asm"
                include "Sounds.asm"


;Sprite data structures
SPRITE_RECORD_SIZE: equ 20
sprMan:                                                                                 ;Offset from record start:
               defb 1             ;Draw mode: 0=disabled (don't draw or erase at all)     ;0
                                   ;  1=Don't erase from old pos, but draw at new pos
                                   ;  2=Erase from old pos but don't draw at new pos
                                   ;  3=Erase from old pos and draw at new pos
sprManX:       defb 160            ;x position                                            ;1
sprManY:       defb 160            ;y position                                            ;2
               defb 0             ;old x position                                         ;3
               defb 0             ;old y position                                         ;4
sprManAddr:    defw 0         ;screen address                                             ;5
sprManGfx:     defw gfx_ManStill;Current Graphic address                                  ;7
               defw 16384         ;old screen address                                     ;9
               defw gfx_ManStill;Old graphic address                                      ;11
               defb INK_WHITE;Sprite Color                                                ;13
sprManFrmNo:   defb 0             ;Sprite Frame no                                        ;14
sprManFrms:    defb 1             ;Number of frames of animation                          ;15
sprManGfxB:    defw gfx_ManStill;Graphic base address                                     ;16
               defb 0             ;Type of Enemy (0 if it's the player)                   ;18
               defb 0             ;Current Direction                                      ;19

;Sprite data structures for all the enemies (7 allowed in total) 20 * 7 = 140 bytes (plus one as a terminator) = 141
sprEnemies:
               block MAX_ENEMIES*SPRITE_RECORD_SIZE+1,-1
               ;defb -1             ;(This is a kinda terminator byte for the enemies block)

PlayerMapPos   defb 0
LevelAttrib:   defb 0             ;Default Attributes for the current level.
GemsLeft:      defb 0             ;How many gems still need collecting on the level.
ManDir:        defb DIR_NONE      ;Direction player is going in.
FrameCycle:    defb 0             ;Counter to count when to advance frames
NoOfEnemies:   defb 0
CurrentLevel:  defb 0
ActiveFX:      defb 0             ;Contains effects active on level in bits. Bit 0:Clock 1:Coffee 2:PowerPill 3:ScaryMask 4:Weights
KeysYouGot:    defb 0             ;Contains keys collected: Bit 0:Red 1:Yellow 2:Blue 3:Green
KempstonOn:    defb 0
;Randomizer:    defb 0             ;This is used by the Fuzzy Alien enemy to kinda pseudo-randomise its movement.

            include "images.asm"
MazeRunFont:
            incbin "MazeRunFont.bin"
            savesna "MazeRun.sna",32768 ;0000 ;SjASM-only command
