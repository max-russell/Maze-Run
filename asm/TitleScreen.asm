;-------------------------------------------------------------
DoTitleScreen:
;-------------------------------------------------------------

                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen
                 ld a,BRIGHT + PAPER_BLACK
                 ld (LevelAttrib),a

                 ld b,64
                 ld c,16
                 ld de,gfx_Title
                 call DrawTile

                 ld de,.s_Start
                 ld b,7
                 ld c,16
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_CYAN
                 ;ld l,PAPER_BLACK+INK_RED
                 call Print
                 ld de,.s_Redefine
                 ld b,7
                 ld c,17
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_YELLOW
                 ;ld l,PAPER_BLACK+INK_RED
                 call Print
                 ld de,.s_Password
                 ld b,7
                 ld c,18
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_MAGENTA
                 ;ld l,PAPER_BLACK+INK_RED
                 call Print
                 ld de,.s_Help
                 ld b,7
                 ld c,19
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_GREEN
                 ;ld l,PAPER_BLACK+INK_RED
                 call Print
                 
                 ld a,BRIGHT + PAPER_RED + INK_YELLOW
                 ld b,32
                 ld hl,ATTRIBUTES
1                ld (hl),a
                 inc hl
                 djnz 1B
                 ld b,32
                 ld hl,ATTRIBUTES+736
1                ld (hl),a
                 inc hl
                 djnz 1B



.TitleLoop
                 call WaitVBL

                 ld a,(.StripeFrame)
                 bit 7,a
                 jr nz,1F
                 scf
                 rl a
                 jr 2F
1                sla a
2                ld (.StripeFrame),a
                 ld d,a

                 ;ld d,%00000000
                 ld hl,SCREEN
                 call FunkyStripes
                 
                 ld a,(.StripeFrame)
                 ld d,a
                 ld hl,SCREEN + 4320
                 call FunkyStripes

                 ;ld d,%00000000
                 ;ld hl,SCREEN+256
                 ;call FunkyStripes



                 call TestAnyKey
                 and a
                 jp z,.TitleLoop

                 cp '3'
                 jp z,GetHelp
                 cp '2'
                 jp z,EnterPassword
                 cp '1'
                 jr z,RedefineKeys
                 cp '0'
                 ret z

                 jp .TitleLoop

.s_Start:        defm "0  START GAME",0
.s_Redefine:     defm "1  REDEFINE KEYS",0
.s_Password:     defm "2  ENTER PASSWORD",0
.s_Help:         defm "3  GET HELP",0
.StripeFrame     defb %10000000


FunkyStripes:
                 ld e,224
                 ld c,8
2                ld b,16
1                ld a,d
                 ld (hl),a
                 inc hl
                 cpl
                 ld (hl),a
                 inc hl
                 djnz 1B
                 dec c
                 ret z

                 ld a,l
                 add a,e
                 ld l,a
                 ld a,h
                 adc a,0
                 ld h,a
                 ld a,d
                 and 128
                 jr nz,1F
                 scf
                 rl d
                 jr 2B
1                sla d
                 jr 2B

;-------------------------------------------------------------
RedefineKeys:
;-------------------------------------------------------------
                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen

                 ;Wait 1/2 second
                 ld bc,10
                 call Pause

                 ld de,.s_Press
                 ld b,8
                 ld c,4
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call Print

                 ld de,.s_Up
                 ld hl,KEY_UP

                 ld a,6
                 ld (.ypos),a

.Loop            push hl
                 ld b,8
                 ld a,(.ypos)
                 ld c,a
                 ;inc a
                 ;ld (.ypos),a
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call Print
                 inc de
                 pop hl
                 push de
                 push hl

1                call TestAnyKey
                 and a
                 jr z,1B

                 ld (.s_NewKey),a
                 pop hl
                 ld a,(LastKeyCheck)
                 ld (hl),a
                 inc hl
                 ld a,(LastKeyCheck+1)
                 ld (hl),a
                 inc hl
                 push hl

                 ld de,.s_NewKey
                 ld b,22
                 ld a,(.ypos)
                 ld c,a
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call Print
                 ld de,.s_Dotdotdot
                 ld b,16
                 ld a,(.ypos)
                 ld c,a
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call Print

                 ;Wait 1/2 second
                 ld bc,10
                 call Pause

                 pop hl
                 pop de

                 ld a,(.ypos)
                 inc a
                 inc a
                 ld (.ypos),a
                 cp 20
                 jr nz,.Loop

                 jp DoTitleScreen

.ypos            defb 10

.s_Press:        defm "PRESS A KEY FOR",0
.s_Up:           defm "UP",0
.s_Down:         defm "DOWN",0
.s_Left:         defm "LEFT",0
.s_Right:        defm "RIGHT",0
.s_Pause:        defm "PAUSE",0
.s_Restart:      defm "RESTART",0
.s_Quit:         defm "QUIT",0
.s_Dotdotdot:    defm "...",0
.s_NewKey:       defb 0,0

;-------------------------------------------------------------
EnterPassword:
;-------------------------------------------------------------
                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen

                 ld de,.s_1
                 ld b,1
                 ld c,6
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call Print
                 ld de,.s_2
                 ld b,6
                 ld c,8
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call Print
                 ld de,.s_3
                 ld b,1
                 ld c,10
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_WHITE
                 call Print

                 ld b,11                ;Clear password string.
                 ld hl,.s_Password+10
1                ld (hl),0
                 dec hl
                 djnz 1B
                 inc hl
                 ld b,10

1                push hl
                 call GetType
                 pop hl
                 cp 13                            ;If pressed enter, finish
                 jr z,.FindPassword

                 ld d,a
                 ld a,b   ;No more characters allowed, so just loop back
                 cp 20    ;until we press enter.
                 jr z,1B
                 ld a,d

                 push bc
                 push hl
                 ld (hl),a
                 ld (.s_NewKey),a
                 ld de,.s_NewKey
                 ;ld b,5
                 ld c,13
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_GREEN
                 call Print
                 pop hl
                 pop bc
                 inc b
                 inc hl
                 jr 1B

.FindPassword:   ld d,1             ;d will store the level we're checking
                 ld hl,LevelMaps+28
.Loop2           ld bc,.s_Password
                 push hl
.Loop1           ld a,(bc)
                 cp (hl)
                 jr nz,.NotThisLevel
                 cp 0               ;Is it the end of the password?
                 jr z,.FoundLevel
                 inc bc
                 ld a,l
                 add a,16
                 ld l,a
                 ld a,h
                 adc a,0
                 ld h,a
                 ;add hl,16
                 jr .Loop1

.NotThisLevel:   ld a,NO_OF_LEVELS  ;Are there no more levels to check?
                 cp d
                 jr z,.WrongPassword
                 inc d
                 pop hl
                 ;add hl,192
                 ld a,l
                 add a,192
                 ld l,a
                 ld a,h
                 adc a,0
                 ld h,a
                 jr .Loop2


                 jp DoTitleScreen

.FoundLevel:     pop hl
                 ld a,d               ;Level to start at is d
                 ld (CurrentLevel),a
                 
                 ld b,30        ;Print found level message
                 ld c,17
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_MAGENTA
                 call PrintNumber
                 ld de,.s_4
                 ld b,1
                 ld c,17
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_CYAN
                 call Print
                 call GetType
                 jp DoTitleScreen

.WrongPassword:  pop hl
                 ld de,.s_5    ;Print password not recognised message
                 ld b,7
                 ld c,17
                 ld a,0
                 ld h,BRIGHT+PAPER_BLACK+INK_CYAN
                 call Print
                 call GetType
                 jp DoTitleScreen

.s_1:            defm "TYPE YOUR PASSWORD NOW, BUDDY",0
.s_2:            defm "AND THEN PRESS ENTER",0
.s_3:            defm "PASSWORD FOR LEVEL 1 IS 'START'",0
.s_4:            defm "GAME WILL NOW START AT LEVEL",0
.s_5:            defm "INVALID PASSWORD",0
.s_NewKey:       defb 0,0
.s_Password:     block 11,0

;-------------------------------------------------------------
GetHelp:
;-------------------------------------------------------------
                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen
                 
                 ld de,.s_1
                 ld b,0
                 ld c,0
                 ld h,BRIGHT+PAPER_BLACK+INK_CYAN
                 call PrintMultiLine

                 ld de,.s_2
                 ld b,0
                 ld c,4
                 ld h,PAPER_BLACK+INK_WHITE
                 call PrintMultiLine

                 call GetType
                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen
                 
                 ld de,.s_3
                 ld b,0
                 ld c,0
                 ld h,PAPER_BLACK+INK_WHITE
                 call PrintMultiLine
                 
                 ld bc,0
                 ld de,gfx_RobotRight
                 ld a,BRIGHT + INK_RED
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,24
                 ld de,gfx_AngryChef
                 ld a,BRIGHT + INK_WHITE
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,48
                 ld de,gfx_Smasher
                 ld a,BRIGHT + INK_YELLOW
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,72
                 ld de,gfx_FuzzyAlien
                 ld a,BRIGHT + INK_MAGENTA
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,96
                 ld de,gfx_SlimerRight
                 ld a,BRIGHT + INK_GREEN
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,120
                 ld de,gfx_SlimerLeft
                 ld a,BRIGHT + INK_CYAN
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,160
                 ld de,gfx_TeddyHead
                 ld a,BRIGHT + INK_YELLOW
                 call DrawSpriteAsTile

                 call GetType

                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen
                 ld de,.s_4
                 ld b,0
                 ld c,0
                 ld h,PAPER_BLACK+INK_WHITE
                 call PrintMultiLine

                 ld b,0
                 ld c,0
                 ld de,gfx_PacmanRight
                 ld a,BRIGHT + INK_YELLOW
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,32
                 ld de,gfx_Gorgon
                 ld a,BRIGHT + INK_GREEN
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,72
                 ld de,gfx_CreepyGirl
                 ld a,BRIGHT + INK_CYAN
                 call DrawSpriteAsTile
                 ld b,0
                 ld c,112
                 ld de,gfx_Coffee
                 call DrawTile
                 ld b,0
                 ld c,136
                 ld de,gfx_Clock
                 call DrawTile
                 ld b,0
                 ld c,160
                 ld de,gfx_PowerPill
                 call DrawTile

                 call GetType

                 ld a,BRIGHT + PAPER_BLACK + INK_WHITE ;Set default screen attributes
                 call ClearScreen
                 ld de,.s_5
                 ld b,0
                 ld c,0
                 ld h,PAPER_BLACK+INK_WHITE
                 call PrintMultiLine
                 ld b,0
                 ld c,0
                 ld de,gfx_ScaryMask
                 call DrawTile
                 ld b,0
                 ld c,24
                 ld de,gfx_Weights
                 call DrawTile
                 ld b,0
                 ld c,48
                 ld de,gfx_X
                 call DrawTile
                 ld b,0
                 ld c,72
                 ld de,gfx_KeyRed
                 call DrawTile
                 ld b,16
                 ld c,72
                 ld de,gfx_DoorBlue
                 call DrawTile
                 ld b,0
                 ld c,96
                 ld de,gfx_Thief
                 call DrawTile
                 ld b,0
                 ld c,120
                 ld de,gfx_Teleport
                 call DrawTile
                 ;ld b,0
                 ;ld c,144
                 ;ld de,gfx_Earthquake
                 ;call DrawTile

                 call GetType
                 jp DoTitleScreen
                ;         1         2         3
                ;12345678901234567890123456789012
.s_1:      defm "HOW TO PLAY THE DELIGHTFUL GAME ",0
           defm " OF MAZE RUN BY THE INIMITABLE  ",0
           defm "   SPECCY MASTER, MAX RUSSELL   ",0,0

.s_2:      defm "EASY! USE UP, DOWN, LEFT AND",0
           defm "RIGHT KEYS TO MOVE AROUND EACH",0
           defm "MAZE, AVOIDING BAD GUYS AND",0
           defm "COLLECTING ENOUGH GEMS. ON THE",0
           defm "RIGHT IT TELLS YOU HOW MANY GEMS",0
           defm "YOU IS GONNA NEED FOR THE LEVEL.",0
           defm "GET THAT MUCH AND YOU WILL",0
           defm "UNLOCK YON' EXIT. GO THROUGH IT",0
           defm "TO WIN THE LEVEL!",0                 ;13
           defm " ",0                                 ;14
           defm "OH, WOULD THAT IT WERE SO EASY!",0     ;15
           defm "TO COMPLICATE MATTERS, THERE ARE",0                                           ;16
           defm "BAD GUYS DETERMINED TO CATCH YOU",0    ;17
           defm "AS WELL AS ITEMS LITTERED ABOUT",0  ;18
           defm "ALL OF WHICH DO DIFFERENT THINGS",0   ;19
           defm " ",0  ;20
           defm "PRESS A KEYS TO LEARN ABOUT SOME",0                                           ;21
           defm "OF THE TERRIFYING AND PUZZLING",0                                           ;22
           defm "ENTITIES YOU WILL ENCOUNTER...",0,0                                           ;23;
                                                      ;24
                ;         1         2         3
               ;"12345678901234567890123456789012"
.s_3:      defm "   ROBOTS GO LEFT AND RIGHT WITH",0 ;1
           defm "   EVIL ROBOTIC CONSISTENCY.",0
           defm " ",0
           defm "   CHEFS ARE VERY VERY ANGRY AND",0
           defm "   THUS HOP UP AND DOWN.   ",0
           defm " ",0
           defm "   SMASHERS DO EXACTLY THAT - NO",0
           defm "   MORE AND NO LESS. SMASHING!",0
           defm " ",0
           defm "   FUZZY ALIENS ENJOY BOUNCING  ",0 ;10
           defm "   OFF THE WALLS. UNPREDICTABLE!",0
           defm " ",0
           defm "   GREEN SLIMERS FOLLOW ALONG   ",0
           defm "   OUTSIDE WALLS, ALWAYS GOING  ",0
           defm "   CLOCKWISE. THEY ARE ENGAGED",0
           defm "   IN A LONG RUNNING DISPUTE",0
           defm "   WITH THEIR BLUE BRETHREN",0
           defm "   WHO INSIST ANTICLOCKWISE IS",0
           defm "   THE ONLY CORRECT DIRECTION.",0
           defm " ",0
           defm "   NORMALLY DOCILE, TEDDIES HATE",0
           defm "   SHARING A COORDINATE WITH YOU",0 ;22
           defm "   AND WILL CHARGE IF YOU DO.",0,0

                ;         1         2         3
               ;"12345678901234567890123456789012"
.s_4:      defm "   PACMEN TRY TO FOLLOW YOU, BUT",0
           defm "   HAVE YET TO LEARN THE KNACK  ",0
           defm "   OF GOING UP AND DOWN.",0
           defm " ",0
           defm "   IF THE GORGONS JUST LEARNT",0
           defm "   TO COOPERATE WITH THE PACMEN",0
           defm "   THEY COULD SHARE SOME USEFUL",0
           defm "   INFORMATION.",0
           defm " ",0
           defm "   CREEPY GIRLS HATE YOU WITH A",0
           defm "   PASSION AND THEY'RE VERY",0
           defm "   CREEPY INDEED. MY ADVICE:",0
           defm "   RUN AWAY!",0
           defm " ",0
           defm "   COFFEE, DRINK OF THE GODS!",0
           defm "   LETS YOU TRAVEL SUPER FAST!",0
           defm " ",0
           defm "   CLOCKS FREEZE ALL THE BADDIES",0
           defm "   IN THEIR TRACKS.",0
           defm " ",0
           defm "   POWER PILLS LET YOU BEAT UP",0
           defm "   THE NEXT BADDIE YOU RUN INTO",0,0

.s_5:      defm "   SCARY MASKS CAUSE ENEMIES",0
           defm "   THAT CHASE YOU TO RUN AWAY!",0
           defm " ",0
           defm "   WEIGHTS SWITCH ON GRAVITY,",0
           defm "   MAKING YOU FALL DOWN!",0
           defm " ",0
           defm "   THE X CANCELS ALL ACTIVE FX",0
           defm "   FOR BETTER OR WORSE!",0
           defm " ",0
           defm "     KEYS LET YOU OPEN DOORS OF",0
           defm "     THE SAME COLOUR.",0
           defm " ",0
           defm "   BUT THE THIEF WILL STEAL ALL",0
           defm "   YOUR KEYS. HE'S A BAD MAN!",0
           defm " ",0
           defm "   TELEPORTS INSTANTLY MOVE YOU",0
           defm "   SOMEWHERE ELSE IN THE MAZE.",0
           defm " ",0
           defm "   THE EARTHQUAKE CAUSES ALL",0
           defm "   NON-STRUCTURALLY-SOUND WALLS",0
           defm "   TO SHATTER.",0,0

