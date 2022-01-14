include "64cube.inc" 			; Include the helper functions

ENUM $0  				; Enumerate values starting from $0000
	counter rBYTE 1 		; Set 'counter' as 1 byte
ENDE 					; End enumeration

	org $200 			; Set program origin to $0200
	sei 				; Set interrupt disable flag
	ldx #$ff 			; Load value $ff into X
	txs 				; Transfer value of X into the stack
					;   pointer

	; This will set the video buffer page in the $f000 page in memory
	lda #$f				; Load high byte of page into 'a'
					;   register
	sta VIDEO			; Store value in A at 'VIDEO'

	; This will set the colors buffer page in the $5000 page in memory
	lda #$5 			; Load highbyte of page into 'a'
					;   register
	sta COLORS 			; Store value in A at 'COLORS'

	_setw IRQ, VBLANK_IRQ 		; Set value of VBLANK_IRQ to address of
					;   'IRQ' label
	cli 				; Clear interrupt disable flag

Infinite: 				; Set 'Infinite' label
	jmp Infinite 			; Create infinite loop


IRQ: 					; Set 'IRQ' label
	inc counter 			; Increment 'counter'
	rti 				; Return from interrupt

Palette: 				; Set 'Palette' label
	; This will set the palette of colors available in the COLORS page,
	;   enumerating from $00 upward within that page.
	org $0500 			; Set program origin to $0500
	hex 000000 ffffff ; Set these colors in colors page

Image:
    org $f000
    include "roms/3dworld/floor_0.raw"
    org $f555
    include "roms/3dworld/floor_1.raw"
    org $faaa
    include "roms/3dworld/floor_0.raw"
