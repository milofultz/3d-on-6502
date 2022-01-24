include "64cube.inc"            ; Include 64cube headers

ENUM $0                         ; Enum values as bytes starting at $0000
  ready       rBYTE 1           ; bool: If true, activates Main loop
  src_l       rBYTE 1
  src_h       rBYTE 1           ; Addresses to source of frame graphics
  wide        rBYTE 1
  high        rBYTE 1           ; Width/height of the sprite to be drawn
  mask        rBYTE 1
  xpos        rBYTE 1
  ypos        rBYTE 1
  dst_l       rBYTE 1
  dst_h       rBYTE 1
  temp        rBYTE 1
  frames      rBYTE 1
  fcount      rBYTE 1
  counter     rBYTE 1
ENDE


  org $200                      ; Insert the following at $0200
Boot:
  sei                           ; Set interrupt
  ldx #$ff
  txs                           ; Set stack pointer address to $ff

  lda #$f
  sta VIDEO                     ; Set address for VIDEO to $f000

  lda #$5
  sta COLORS                    ; Set address for COLORS to $5000

  lda #0
  sta xpos
  lda #64-21
  sta ypos                      ; Set xpos and ypos to 28

  _setw IRQ, VBLANK_IRQ         ; Set address to IRQ in VBLANK_IRQ
  cli                           ; Clear interrupt


Main:
  lda ready                     ; If A is zero,
  beq Main                      ;   Go to Main
                                ; Else,
  jsr Clear                     ;   Jump to subroutine 'Clear'

  _setb 64,wide                 ; Set 'wide' to 64, full width of the screen
  _setb 19,high                 ; Set 'high' to 10

  ldx fcount                    ; Load 'fcount' into X

  lda SpriteLoPtr,x
  sta src_l                     ; Store sum of 'SpriteLoPtr' and X in 'src_l'
  lda SpriteHiPtr,x
  sta src_h                     ; Store sum of 'SpriteHiPtr' and X in 'src_h'

  lda #%01000000
  sta dst_l
  lda #%11111011
  sta dst_h                     ; Set the starting position to draw the sprite,
                                ;   where every 64 bits is one row. This starts
                                ;   at $fb40, meaning row 45 ($bf0 / 64) and
                                ;   column 0 ($b40 % 64).

  jsr Draw                      ; Draw the sprite at the given destination

  lda #0
  sta ready                     ; Set 'ready' to 0 (false)
  jmp Main                      ; Jump to 'Main'

; Every interrupt,
;   Set ready to 1 (true)
;   When counter hits 6, go to the next frame and reset counter
;   If fcount is 6, go to frame 0

IRQ:
  lda #1
  sta ready                     ; Set 'ready' to 1 (true)

Timer:
  lda counter
  cmp #6                        ; If 'counter' !== 6,
  bne +                         ;   Go to nearest '+' label
  inc fcount                    ; Else, increment 'fcount'
  lda fcount
  cmp #6                        ; If 'fcount' !== 6,
  bne +++                       ;   Go to nearest '+++' label
  lda #0                        ; Else,
  sta fcount                    ;   Set 'fcount' to 0
+++
  lda #0
  sta counter                   ; Set 'counter' to 0
+
  inc counter                   ; Increment counter

  rti                           ; Return from interrupt


Draw:
  ; X: Current position of sprite column pointer
  ; Y: Current position of sprite row pointer

    ldx #$00
  -
    ldy #$00
  --
    lda (src_l),y               ; Load current pixel into the accumulator
    sta (dst_l),y               ; Store current pixel in destination pixel

    iny                         ; Increment column pointer
    cpy wide                    ; If column pointer matches the width of
                                ;     the sprite,
    bne --                      ;   Draw the next column
                                ; Else,
    _addwb src_l,wide,src_l     ; Add `wide` to the sprite's pointer (go to the
                                ;   next 'row' within the sprite)
    _addwi dst_l,64,dst_l       ; Move destination pointer to next row on screen

    inx                         ; Increment current height
    cpx high                    ; If current height is not desired height,
    bne -                       ;   Draw next row of sprite
  rts


Clear:
  ; Clear whole video page to prepare for redraw

    ldx #0
  -
    lsr
    sta $f000,x
    sta $f100,x
    sta $f200,x
    sta $f300,x
    sta $f400,x
    sta $f500,x
    sta $f600,x
    sta $f700,x
    sta $f800,x
    sta $f900,x
    sta $fa00,x
    sta $fb00,x
    sta $fc00,x
    sta $fd00,x
    sta $fe00,x
    sta $ff00,x
    dex
    bne -
  rts


  org $0500

palette:
  hex 000000 ffffff

  SpriteLoPtr:
  db <(floor0)
  db <(floor1)
  db <(floor2)
  db <(floor3)
  db <(floor4)
  db <(floor5)

  SpriteHiPtr:
  db >(floor0)
  db >(floor1)
  db >(floor2)
  db >(floor3)
  db >(floor4)
  db >(floor5)

  org $a000

floor0:
  incbin "roms/3dworld/floor_0.raw"  ; Include the binary code of the file
floor1:
  incbin "roms/3dworld/floor_1.raw"
floor2:
  incbin "roms/3dworld/floor_2.raw"
floor3:
  incbin "roms/3dworld/floor_3.raw"
floor4:
  incbin "roms/3dworld/floor_4.raw"
floor5:
  incbin "roms/3dworld/floor_5.raw"
