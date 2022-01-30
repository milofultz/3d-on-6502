include "64cube.inc"            ; Include 64cube headers

ENUM $0                         ; Enum values as bytes starting at $0000
  ready         rBYTE 1         ; bool: If true, activates Main loop
  flr_l         rBYTE 1
  flr_h         rBYTE 1         ; Addresses to source of floor graphics
  flr_wide      rBYTE 1
  flr_high      rBYTE 1         ; Width/height of the floor sprite to be drawn
  dude_l        rBYTE 1
  dude_h        rBYTE 1         ; Addresses to source of dude graphics
  dude_x        rBYTE 1
  dude_y        rBYTE 1         ; Position of "dude" on screen
  dude_wide     rBYTE 1
  dude_high     rBYTE 1         ; Width/height of the dude
  dude_bound_x  rBYTE 1
  dude_bound_y  rBYTE 1         ; Bounds for traversal of the dude
  dst_l         rBYTE 1
  dst_h         rBYTE 1
  temp          rBYTE 1
  frames        rBYTE 1
  fcount        rBYTE 1
  counter       rBYTE 1
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

  _setb 64,flr_wide             ; Set 'flr_wide' to 64, full width of the screen
  _setb 19,flr_high             ; Set 'flr_high' to 10

  _setb 8,dude_wide
  _setb 8,dude_high             ; Set 'dude_wide' and 'dude_high'

  clc
  lda dude_wide
  ror
  sta temp
  clc
  lda #$21
  sbc temp
  sta dude_x
  clc
  lda dude_high
  ror
  sta temp
  clc
  lda #$21
  sbc temp                      ; Set dude_x and dude_y starting position to
  sta dude_y                    ;   the center of the screen, using dimensions

  sec
  lda #64
  sbc dude_wide
  sta dude_bound_x
  sec
  lda #64
  sbc dude_high                 ; Set 'dude_bounds' to screen dimensions minus
  sta dude_bound_y              ;   the size of the dude

  _setw IRQ, VBLANK_IRQ         ; Set address to IRQ in VBLANK_IRQ
  cli                           ; Clear interrupt


Main:
  lda ready                     ; If A is zero,
  beq Main                      ;   Go to Main
                                ; Else,
  jsr Clear                     ;   Jump to subroutine 'Clear'

  ldx fcount                    ; Load 'fcount' into X

  lda FloorSpriteLoPtr,x
  sta flr_l                     ; STA sum of 'FloorSpriteLoPtr' and X in 'flr_l'
  lda FloorSpriteHiPtr,x
  sta flr_h                     ; STA sum of 'FloorSpriteHiPtr' and X in 'flr_h'

  sec
  lda dude_y
  cmp #40
  bcs +

  lda DudeMSprLoPtr
  sta dude_l                    ; STA sum of 'DudeMSprLoPtr' and X in 'dude_l'
  lda DudeMSprHiPtr
  sta dude_h                    ; STA sum of 'DudeMSprHiPtr' and X in 'dude_h'
  jmp ++
+
  lda DudeLSprLoPtr
  sta dude_l                    ; STA sum of 'DudeLSprLoPtr' and X in 'dude_l'
  lda DudeLSprHiPtr
  sta dude_h                    ; STA sum of 'DudeLSprHiPtr' and X in 'dude_h'

++
  lda #%01000000                ; Set the starting position to draw the floor,
  sta dst_l                     ;   where every 64 bits is one row. This starts
  lda #%11111011                ;   at $fb40, meaning row 45 ($bf0 / 64) and
  sta dst_h                     ;   column 0 ($b40 % 64).

  jsr DrawFloor                 ; Draw the floor at the given destination

  jsr DrawDude                  ; Draw the dude

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

  ; Check if player is pushing a direction
  ; If so,
  ;   Adjust the dude position

  UP:
    lda INPUT
    and #%00010000
    beq NoUP
    lda dude_y
    beq NoUP
    dec dude_y
  NoUP:

  DN:
    lda INPUT
    and #%00100000
    beq NoDN
    lda dude_y
    cmp dude_bound_y
    beq NoDN
    inc dude_y
  NoDN:

  LT:
    lda INPUT
    and #%01000000
    beq NoLT
    lda dude_x
    beq NoRT
    dec dude_x
  NoLT:

  RT:
    lda INPUT
    and #%10000000
    beq NoRT
    lda dude_x
    cmp dude_bound_x
    beq NoRT
    inc dude_x
  NoRT:

  Other:
    lda INPUT
    and #%00001111
    beq NoOther
    lda #64-36
    sta dude_x
    sta dude_y
  NoOther:

Timer:
  lda counter
  cmp #6                        ; If 'counter' !== 6,
  bne +                         ;   Go to nearest '+' label
  inc fcount                    ; Else, increment 'fcount'
  lda fcount
  cmp #6                        ; If 'fcount' !== 6,
  bne ++                        ;   Go to nearest '++' label
  lda #0                        ; Else,
  sta fcount                    ;   Set 'fcount' to 0
++
  lda #0
  sta counter                   ; Set 'counter' to 0
+
  inc counter                   ; Increment counter

  rti                           ; Return from interrupt


DrawFloor:
  ; X: Current position of sprite column pointer
  ; Y: Current position of sprite row pointer

    ldx #$00
  -
    ldy #$00
  --
    lda (flr_l),y               ; Load current floor pixel into the accumulator
    sta (dst_l),y               ; Store current pixel in destination pixel

    iny                         ; Increment column pointer
    cpy flr_wide                ; If column pointer matches the width of
                                ;     the floor sprite,
    bne --                      ;   Draw the next column
                                ; Else,
    _addwb flr_l,flr_wide,flr_l ; Add `flr_wide` to the floor sprite's pointer
                                ;   (go to the next 'row' within the sprite)
    _addwi dst_l,64,dst_l       ; Move destination pointer to next row on screen

    inx                         ; Increment current height
    cpx flr_high                ; If current height is not desired height,
    bne -                       ;   Draw next row of sprite
  rts

DrawDude:
    lda #0
    sta dst_l
    sta dst_h
    sta temp                    ; Clear variables

    ; Set dude position and store in dst_l and dst_h
    clc
    lda dude_y
    ror                         ; Set dst_h, by multiplying dude_y by 64 (to
                                ;   match row) Rotate dude_y to the right by
                                ;   two. ((256 / 2) / 2 === 64)
    ror temp                    ; Put the numbers rotated out above into a temp
                                ;   to eventually be used added to dst_l
    ror
    ror temp

    ora #$f0                    ; Put on the VIDEO page ($f0)
    sta dst_h                   ; Store the number as the high pointer

    clc
    lda temp
    adc dude_x                  ; Add the temp numbers above to dude_x value
    sta dst_l                   ; Store the number as the low pointer

    ldx #0
  --
    ldy #0
  -
    lda (dude_l),y
    cmp #$ff
    beq +
    sta (dst_l),y               ; Store the sprite's pixel at the dst pointer.
  +
    iny
    cpy dude_wide
    bne -

    _addwb dude_l,dude_wide,dude_l ; Add `dude_wide` to the dude sprite pointer
                                ;   (go to the next 'row' within the sprite)
    _addwi dst_l,64,dst_l       ; Move destination pointer to next row on screen

    inx
    cpx dude_high
    bne --

  rts


Clear:
  ; Clear whole video page to prepare for redraw

    lda #0
    ldx #0
  -
    sta $f000,x                 ; Paint the sky color at top of screen
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
    dex
    bne -

    lda #3
    ldx #$40
  -
    sta $fb00,x                 ; Paint the line right above the floor
    dex
    cpx #$ff
    bne -
  rts


  org $0500

palette:
  ;   Env Colors: 00-03
  ;   Black  lGreen Green  dGreen
  hex 000000 00A619 008013 004800
  ;   Ship colors: 04-09
  ;   Gray   dGray  Orange Yellow lBlue  lGray
  hex b0b0b0 666666 ff6c00 ffdb00 92cdff 404040

  FloorSpriteLoPtr:
  db <(floor0)
  db <(floor1)
  db <(floor2)
  db <(floor3)
  db <(floor4)
  db <(floor5)

  FloorSpriteHiPtr:
  db >(floor0)
  db >(floor1)
  db >(floor2)
  db >(floor3)
  db >(floor4)
  db >(floor5)

  DudeMSprLoPtr:
  db <(dude_mid)

  DudeMSprHiPtr:
  db >(dude_mid)

  DudeLSprLoPtr:
  db <(dude_low)

  DudeLSprHiPtr:
  db >(dude_low)

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
dude_mid:
  incbin "roms/3dworld/ship11.raw"
dude_low:
  incbin "roms/3dworld/ship21.raw"
