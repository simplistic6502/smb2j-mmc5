.segment "NES2HDR"
  .byte $4E,$45,$53,$1A                           ;  magic signature
  .byte 4                                         ;  PRG ROM size in 16384 byte units
  .byte 2                                         ;  CHR
  .byte $52                                       ;  mirroring type and mapper number lower nibble
  .byte $08                                       ;  mapper number upper nibble
  .byte $00,$00,$90,$00,$00,$00,$00,$00

;-------------------------------------------------------------------------------------
;DEFINES

PPU_CTRL              = $2000
PPU_MASK              = $2001
PPU_STATUS            = $2002
PPU_SCROLL            = $2005
PPU_ADDR              = $2006
PPU_DATA              = $2007

DMC_FREQ              = $4010
APU_STATUS            = $4015
JOY1                  = $4016
JOY2_FRAME            = $4017

FDSBIOS_NMIFlag       = $0100
FDSBIOS_IRQFlag       = $0101
FDSBIOS_ResetFlag1    = $0102
FDSBIOS_ResetFlag2    = $0103

FDS_WRITEDATA         = $4024
FDS_STATUS            = $4030
FDS_READDATA          = $4031

.include "mmc5_defines.inc"

.import SM2SAVE_Header
.import GamesBeatenCount

;-------------------------------------------------------------------------------------
.segment "FDSBIOS"
.org $e000

Reset:
    sei                     ;mmc5 init
	lda #$03
    sta MMC5_PRGMODE
    lsr
    sta MMC5_EXRAMPROTECT2
    asl
    sta MMC5_EXRAMPROTECT1
	sta MMC5_EXRAMMODE
    sta MMC5_IRQSTATUS
    lda MMC5_IRQSTATUS
    ldx #$00
	stx MMC5_CHRMODE
    stx MMC5_PRG_6000
    stx MMC5_CHR_1C00
    inx
    stx MMC5_PRG_8000
    inx
    stx MMC5_PRG_A000
    inx
    stx MMC5_PRG_C000

	lda #$10				;replicate init code present in FDS BIOS
	sta PPU_CTRL
	cld
	lda #$06
	sta PPU_MASK
	ldx #$02
VBlank:
	lda PPU_STATUS
	bpl VBlank
	dex
	bne VBlank
	stx JOY1
	stx DMC_FREQ
	lda #$c0
	sta JOY2_FRAME
	lda #$0f
	sta APU_STATUS
	ldx #$ff
	txs

	lda #$c0
	sta FDSBIOS_NMIFlag    ;PC action on NMI
	lda #$80
	sta FDSBIOS_IRQFlag    ;PC action on IRQ
	lda FDSBIOS_ResetFlag1 ;mimic warm boot check in FDS BIOS
	cmp #$35
	bne ColdBoot           ;$0102 must be $35 for a warm boot
	lda FDSBIOS_ResetFlag2
	cmp #$53
	beq WarmBoot           ;$0103 will be $53 if game was soft-reset
	cmp #$ac
	bne ColdBoot           ;$0103 will be $ac if first boot of game
	lda #$53               ;if $0103 is $ac, change to $53 to indicate
	sta FDSBIOS_ResetFlag2 ;that the user did a soft-reset
	bne WarmBoot           ;unconditional branch to run the game

ColdBoot:
    lda #$35               ;cold boot, must init PRG-RAM and CHR-ROM
	sta FDSBIOS_ResetFlag1 ;PC action on reset
	lda #$ac
	sta FDSBIOS_ResetFlag2 ;PC action on reset
    jsr FDSBIOS_LOADFILES
	.word BootFiles	;padding
	.word BootFiles
WarmBoot:	
	lda PPU_STATUS         ;FDS BIOS stuff
	lda #$00
	sta PPU_SCROLL
	sta PPU_SCROLL
	lda #$10
	sta PPU_STATUS
	cli
	jmp ($dffc)            ;run game
BootFiles:
	.byte $01,$05,$0f,$ff

.res $e149 - *, $ff
Delay132:
	pha
	lda #$16
	sec
DelayLoop:
	sbc #$01
	bcs DelayLoop
	pla
	rts

.res $e18b - *, $ff
FDSBIOS_NMI:
	bit FDSBIOS_NMIFlag
	bpl NMI_Vector1
	bvc NMI_Vector2
	jmp ($dffa)
NMI_Vector2:
	jmp ($dff8)
NMI_Vector1:
	bvc VINTWait
	jmp ($dff6)
VINTWait:
	lda $ff
	and #$7f
	sta $ff
	sta PPU_CTRL
	lda PPU_STATUS
	pla
	pla
	pla
	pla
	sta FDSBIOS_NMIFlag
	pla
	rts

.res $e1c7 - *, $ff
FDSBIOS_IRQ:
	bit FDSBIOS_IRQFlag
	bmi prg_e1ea
	bvc prg_e1d9
	ldx FDS_READDATA
	ldx FDS_WRITEDATA
	pla
	pla
	pla
	txa
	rts
prg_e1d9:
	pha
	lda FDSBIOS_IRQFlag
	sec
	sbc #$01
	bcc prg_e1e8
	sta FDSBIOS_IRQFlag
	lda FDS_READDATA
prg_e1e8:
	pla
	rti
prg_e1ea:
	bvc prg_e1ef
	jmp ($dffe)
prg_e1ef:
	pha
	lda FDS_STATUS
	jsr Delay132
	pla
	rti

.res $e1f8 - *, $ff
FDSBIOS_LOADFILES:
	pla
	clc
	adc #$03
	sta $08
	pla
	adc #$00
	sta $09
	ldy #$00
	lda ($08),y
	sta $0a
	iny
	lda ($08),y
	sta $0b
	jsr DoFileShit
	inc $08
	bne NoIncReturn
	inc $09
NoIncReturn:
	lda $09
	pha
	lda $08
	pha
	lda #$00
	rts

.res $e239 - *, $ff
FDSBIOS_WRITEFILE:
	pla
	clc
	adc #$04
	sta $08
	pla
	adc #$00
	pha
	lda $08
	pha
	lda #$00
	rts

.res $f000 - *, $ff
DoFileShit:
	lda GamesBeatenCount
	sta $5c00
	ldy #$ff
HandleNextFile:
	iny
	cpy #20
	bcs LoadedFiles
	lda ($0a),y
	cmp #$ff
	bne StartFileList
LoadedFiles:
	rts
StartFileList:
	ldx #EndFileList-FileList-1
CheckFileList:
	cmp FileList,x
	beq LoadCurrentFile
	dex
	bpl	CheckFileList
	bmi HandleNextFile
LoadCurrentFile:
	cmp #$0f
	beq SaveFile
	lda FileSrcBank,x
	bpl CHRFile
	sta $06
	lda FileSrcLow,x
	sta $00
	lda FileSrcHigh,x
	sta $01
	lda FileDestLow,x
	sta $02
	lda FileDestHigh,x
	pha
	and #%00011111
	ora #%01100000
	sta $03
	pla
	lsr
	lsr
	lsr
	lsr
	lsr
	sec
	sbc #$03
	sta $07
	lda FileLenLow,x
	sta $04
	lda FileLenHigh,x
	sta $05
	tya
	pha
	jsr LoadPRG
	pla
	tay
	jmp HandleNextFile
CHRFile:
	lda FileSrcBank,x
	sta MMC5_CHR_1C00
	jmp HandleNextFile
SaveFile:
        ldx #$06                ;init counter
SChkLp: lda SM2Header,x         ;check all seven bytes of the save data header
        cmp SM2SAVE_Header,x    ;and see if it is identical to what it should be
        bne InitializeSaveData  ;if any byte does not match, wipe existing save data
        dex
        bpl SChkLp              ;if not gone through all bytes, loop back
		lda $5c00
		sta GamesBeatenCount
		jmp HandleNextFile
InitializeSaveData:
        ldx #$00                ;init counter
        stx GamesBeatenCount    ;wipe number of games beaten
		jmp HandleNextFile
SM2Header:
        .byte "SM2SAVE"

FileList:
	.byte $01,$05,$0f,$10,$20,$30,$40
EndFileList:
FileSrcLow:
	.byte $00,$00,$9f,$00,$00,$2f,$00
FileSrcHigh:
	.byte $00,$c0,$d2,$00,$c0,$ce,$c0
FileSrcBank:
	.byte $00,$80,$83,$01,$84,$84,$85
FileDestLow:
	.byte $00,$00,$9f,$00,$70,$d0,$b4
FileDestHigh:
	.byte $00,$60,$d2,$00,$c4,$c5,$c2
FileLenLow:
	.byte $00,$00,$01,$00,$2f,$cf,$4c
FileLenHigh:
	.byte $00,$80,$00,$00,$0e,$0c,$0f

LoadPRG:
	ldx #$00
	ldy #$00
UpdateBanks:
	lda $06
	sta MMC5_PRG_C000
	lda $07
	sta MMC5_PRG_6000
	lda #$00
	sta $0c
PRGLoop:
	lda $01
	cmp #$e0
	bcc SrcInRange
	sbc #$20
	sta $01
	inc $06
	inc $0c
SrcInRange:
	lda $03
	cmp #$80
	bcc DestInRange
	sbc #$20
	sta $03
	inc $07
	inc $0c
DestInRange:
	cpx $05
	bcc DoNextByte
	cpy $04
	bcc DoNextByte
	lda #$03
    sta MMC5_PRG_C000
    lda #$00
    sta MMC5_PRG_6000
	rts
DoNextByte:
	lda $0c
	bne UpdateBanks
	lda ($00),y            ;copy byte from ROM
	sta ($02),y            ;store in PRG-RAM
	iny
	bne PRGLoop            ;loop until page is finished
	inx
	inc $01                ;increment for next page
	inc $03
	bne PRGLoop
  
.res $fffa - *, $ff
    .word FDSBIOS_NMI
    .word Reset
    .word FDSBIOS_IRQ

.segment "SM2CHAR1"
    .incbin "SM2CHAR1.chr"
.segment "SM2CHAR2"
    .incbin "SM2CHAR2.chr"