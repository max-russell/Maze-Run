
RomBeep: equ 949

Sound_Bomb:
        ld      b,01h
.l0002: push    bc
        ld      hl,0000h
.l0006: ld      de,0001h
        push    hl
        call    RomBeep
        ld      bc,0014h
        ld      de,0c00h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l0006
        pop     bc
        djnz    .l0002
        ret     

Sound_Phasor:
        ld      b,01h
.l0026:  push    bc
        ld      hl,0000h
.l002a:  ld      de,0001h
        push    hl
        call    RomBeep
        ld      bc,0001h
        ld      de,0164h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l002a
        pop     bc
        djnz    .l0026
        ret  

Sound_Phasor2:   
        ld      b,0ah
.l004a: push    bc
        ld      hl,0000h
.l004e: ld      de,0001h
        push    hl
        call    RomBeep
        ld      bc,0001h
        ld      de,0164h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l004e
        pop     bc
        djnz    .l004a
        ret     

Sound_Raspberry:
        ld      b,01h
.l006e:  push    bc
        ld      hl,0a00h
.l0072:  ld      de,0001h
        push    hl
        call    RomBeep
        ld      bc,0064h
        ld      de,1e00h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l0072
        pop     bc
        djnz    .l006e
        ret     

Sound_UFO:
        ld      b,14h
.l0092:  push    bc
        ld      hl,0400h
.l0096:  ld      de,0001h
        push    hl
        call    RomBeep
        ld      bc,0032h
        ld      de,0600h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l0096
        pop     bc
        djnz    .l0092
        ret     

Sound_Mysterious:
        ld      b,05h
.l00b6:  push    bc
        ld      hl,0a00h
.l00ba:  ld      de,000ah
        push    hl
        call    RomBeep
        ld      bc,0200h
        ld      de,1300h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l00ba
        pop     bc
        djnz    .l00b6
        ret     

Sound_Alarm:
        ld      b,0ah
.l00da:  push    bc
        ld      hl,0000h
.l00de:  ld      de,0064h
        push    hl
        call    RomBeep
        ld      bc,0100h
        ld      de,0300h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l00de
        pop     bc
        djnz    .l00da
        ret     

Sound_Laser:
;        ld      b,19h
;.l00fe:  push    bc
;        ld      hl,0000h
;.l0102:  ld      de,;l0006
;        push    hl
;        call    RomBeep
;        ld      bc,0032h
;        ld      de,0100h
;        pop     hl
;        add     a,00h
;        adc     hl,bc
;        push    hl
;        add     a,00h
;        sbc     hl,de
;        pop     hl
;        jr      c,.l0102
 ;       pop     bc
;        djnz    .l00fe
;        ret
         defb 6,25,197,33,0,0,17,6
         defb 0,229,205,181,3,1,50,0
         defb 17,0,1,225,198,0,237
         defb 74,229,198,0,237,82
         defb 225,56,230,193,16,223
         defb 201


Sound_Peow:
        ld      b,01h
.l0122:  push    bc
        ld      hl,0000h
.l0126:  ld      de,0005h
        push    hl
        call    RomBeep
        ld      bc,0001h
        ld      de,0100h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l0126
        pop     bc
        djnz    .l0122
        ret     

Sound_Bird:
        ld      b,0eh
.l0146:  push    bc
        ld      hl,0000h
.l014a:  ld      de,0028h
        push    hl
        call    RomBeep
        ld      bc,0019h
        ld      de,00f0h
        pop     hl
        add     a,00h
        adc     hl,bc
        push    hl
        add     a,00h
        sbc     hl,de
        pop     hl
        jr      c,.l014a
        pop     bc
        djnz    .l0146
        ret     

Sound_Ascending:
        ld      b,01h
.l016a:  push    bc
        ld      hl,03e8h
.l016e:  ld      de,0001h
        push    hl
        call    RomBeep
        pop     hl
        ld      bc,0001h
        add     a,00h
        sbc     hl,bc
        jr      nc,.l016e
        pop     bc
        djnz    .l016a
        ret     

Sound_Ascending2:     
        ld      b,01h
.l018e:  push    bc
        ld      hl,02e8h
.l0192:  ld      de,0001h
        push    hl
        call    RomBeep
        pop     hl
        ld      bc,000ah
        add     a,00h
        sbc     hl,bc
        jr      nc,.l0192
        pop     bc
        djnz    .l018e
        ret     

