; Game Boy test
; © 2000 Doug Lanford
; opus@dnai.com

;---------------------------------------------------------------------
; this version of the test does the following:
;		1) sets up the gameboy
;		2) runs a scrolling background
;		3) drives a directional spaceship sprite from the joypad
;			- sprite tile and flip changes so ship is pointing in the direction of the joypad
;		4) defines and uses a bullet sprite system, which the ship can fire
;---------------------------------------------------------------------

include "hardware.inc"

; GAMEBOY SYSTEM CONSTANTS
; the hardware registers for the Game Boy begin at address $FF00
; All the 8 bit register addresses below are offsets relative to $FF00
PAD_OUTPUT_MASK			equ		PADF_A | PADF_B | PADF_SELECT | PADF_START	; mask for the output buttons

;-------------------------------------------------------------------------
; start of the game rom (address 0000)
;-------------------------------------------------------------------------
SECTION	"ROM_Start",ROM0[$0000]

; NOTE: the hardware requires the interrupt jumps to be at these addresses

SECTION	"VBlank_IRQ_Jump",ROM0[$0040]
; Vertical Blanking interrupt
	jp VBlankFunc

SECTION	"LCDC_IRQ_Jump",ROM0[$0048]
; LCDC Status interrupt (can be set for H-Blanking interrupt)
	reti

SECTION	"Timer_Overflow_IRQ_Jump",ROM0[$0050]
; Main Timer Overflow interrupt
	reti

SECTION	"Serial_IRQ_Jump",ROM0[$0058]
; Serial Transfer Completion interrupt
	reti

SECTION	"Joypad_IRQ_Jump",ROM0[$0060]
; Joypad Button Interrupt?????
	reti




SECTION	"GameBoy_Header_Start",ROM0[$0100]
; begining of Game Boy game header
	nop
	jp 		$150         ; goto beginning of game code

; Game Boy standard header... DO NOT CHANGE!
db $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
db $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
db $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

db "Opus Test       "	; game name (must be 16 bytes)
db $00,$00,$00			; unused
db $00					; cart type
db $00					; ROM Size (32 k)
db $00					; RAM Size (0 k)
db $00,$00				; maker ID
db $01					; Version     =1
db $DA					; Complement check (Important)
db $ff,$ff				; Cheksum, needs to be calculated!


include "sprite.inc"
include "memory.asm"
include "dma.inc"

SECTION	"Game_Code_Start",ROM0[$0150]
; begining of game code
start::
	; init the stack pointer
	ld		sp, $FFFF

	dma_Copy2HRAM
	
	; enable only vblank interrupts
	ld		a, IEF_VBLANK			; set vblank interrupt bit
	ld		[rIE], a	; load it to the hardware register

	; standard inits
	sub		a	;	a = 0
	ld		[rSTAT], a	; init status
	ld		[rLCDC], a	; init LCD to everything off
	ld		a, 16
	ld		[rSCX], a	; background map will start at 16,16
	ld		[rSCY], a

	ld		a, 0
	ld		[vblank_flag], a

	; load the tiles
	ld		bc, TileData
	call	LoadTiles

	; load the background map
	ld		bc, BkgMapData
	call	LoadMapToBkg

	; init the palettes
	call	InitPalettes

	; clear the sprite data
	call	InitSprites

	; init  my spaceship sprite
	PutSpriteXAddr	spaceship1, $50
	PutSpriteYAddr	spaceship1, $80
	sprite_PutTile spaceship1, 8
	sprite_PutTile spaceship2, 9
	sprite_PutTile spaceship3, 6
	sprite_PutTile spaceship4, 7
	sprite_PutTile spaceship5, 4
	sprite_PutTile spaceship6, 5
	sprite_PutFlags	spaceship1, 0

	; init  my spaceship sprite
	PutSpriteXAddr	bat1, $80
	PutSpriteYAddr	bat1, $30
	sprite_PutTile	bat1, 2
	sprite_PutFlags	bat1, 0
	PutSpriteXAddr	bat2, $88
	PutSpriteYAddr	bat2, $30
	sprite_PutTile	bat2, 3
	sprite_PutFlags	bat2, 0

	; set display to on, background on, window off, sprites on, sprite size 8x8
	;	tiles at $8000, background map at $9800, window map at $9C00
	ld		a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_BG8000 | LCDCF_WIN9C00
	ld		[rLCDC],a


	ld a, $00
	ld [WaveTimer], a
	ld [SpriteFrame], a

	; allow interrupts to start occuring
	ei


; main game loop
Game_Loop::
	; don't do a frame update unless we have had a vblank
	ld		a, [vblank_flag]
	cp		0
	jp		z, .end_game_loop

	; get this frame's joypad info
	call	ReadJoypad

	; update any active bullets
	; do this before MoveSpaceship, since we want the bullet to display at
	; its launch position for one frame
	call	UpdateBulletPositions

	; adjust sprite due to d-pad presses
	call	MoveSpaceship

	; reset vblank flag
	ld		a, 0
	ld		[vblank_flag], a

.end_game_loop
; time to loop!
	jp		Game_Loop




;-----------------------------------------------------------------------
; copy a block of data
;
; in:	de - destination ptr
;		hl - source ptr
;		b - number of bytes to copy
;-----------------------------------------------------------------------
CopyBlock::
	push	af

.copy_block_loop
	ld      a, [hli]
	ld		[de], a
	inc		de
	dec		b
	jr		nz, .copy_block_loop

	pop		af
	ret




;------------------------------------------
; init the local copy of the sprites
;------------------------------------------
InitSprites::
	ld		hl, $c000	; my sprites are at $c000
	ld		b, 40*4		; 40 sprites, 4 bytes per sprite
	ld		a, $ff
.init_sprites_loop
	ld		[hli], a
	dec		b
	jr		nz, .init_sprites_loop

; init my bullet sprites
	ld		hl, bullet_data
	ld		b, 16		; 16 bullets in table
.init_bullets_loop
	ld		a, $ff		; signal for an unused bullet slot
	ld		[hli], a
	inc		hl			; 2 bytes per bullet

	dec		b
	jr		nz, .init_bullets_loop

	ret


;----------------------------------------------------
; load the tiles from ROM into the tile video memory
;
; IN:	bc = address of tile data to load
;----------------------------------------------------
LoadTiles::
	ld		hl, _VRAM8000	; load the tiles to tiles bank 1

	ld		de, 4 * 16
	ld		d, $10  ; 16 bytes per tile
	ld		e, $05  ; number of tiles to load

.load_tiles_loop
	; only write during
	ld		a, [rSTAT]	; get the status
	and		STATF_OAM			; don't write during sprite and transfer modes
	jr		nz, .load_tiles_loop

	ld		a, [bc]		; get the next value from the source
	ld		[hli], a	; load the value to the destination, incrementing dest. ptr
	inc		bc			; increment the source ptr

	; now loop de times
	dec		d
	jp		nz, .load_tiles_loop
	dec		e
	jp		nz, .load_tiles_loop

	ret



;----------------------------------------------------
; load the tile map to the background
;
; IN:	bc = address of map to load
;----------------------------------------------------
LoadMapToBkg::
	ld		hl, _SCRN0	; load the map to map bank 0

	ld		d, $00	; 256 bytes per "block"
	ld		e, $04	; 4 blocks (32x32 tiles, 1024 bytes)

.load_map_loop
	; only write during
	ld		a, [rSTAT]	; get the status
	and		STATF_OAM			; don't write during sprite and transfer modes
	jr		nz, .load_map_loop

	ld		a, [bc]		; get the next value from the source
	ld		[hli], a	; load the value to the destination, incrementing dest. ptr
	inc		bc			; increment the source ptr

	; now loop de times
	dec		d
	jp		nz, .load_map_loop
	dec		e
	jp		nz, .load_map_loop

	ret




;----------------------------------------------------
; init the palettes to basic
;----------------------------------------------------
InitPalettes::
	ld		a, %11100100	; set palette colors

	; load it to all the palettes
	ld		[rBGP], a
	ld		[rOBP0], a
	ld		[rOBP1], a

	ret



;-----------------------------------------------------------------------
; read the joypad
;
; output:
; 		This loads two variables:
;			joypad_held		- what buttons are currently held
;			joypad_down		- what buttons went down since last joypad read
;-----------------------------------------------------------------------
ReadJoypad::
	; get the d-pad buttons
	ld		a, P1F_GET_DPAD		; select d-pad
	ld		[rP1], a	; send it to the joypad
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]	; get the result back (takes a few cycles)
	cpl			; bit-flip the result
;  ld	b, a
	and		PAD_OUTPUT_MASK		; mask out the output bits
	swap	a					; put the d-pad button results to top nibble
	ld		b, a				; and store it

	; get A / B / SELECT / START buttons
	ld		a, P1F_GET_BTN		; select buttons
	ld		[rP1], a	; send it to the joypad
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]
	ld		a, [rP1]	; get the result back (takes even more cycles?)
	cpl			; bit-flip the result
	and		PAD_OUTPUT_MASK		; mask out the output bits
	or		b					; add it to the other button bits
	ld		b, a			; put it back in c

	; calculate the buttons that went down since last joypad read
	ld		a, [joypad_held]	; grab last button bits
	cpl							; invert them
	and		b					; combine the bits with current bits
	ld		[joypad_down], a	; store just-went-down button bits

	ld		a, b
	ld      [joypad_held], a	; store the held down button bits

	ld		a, $30       ; reset joypad
    ld		[rP1],A

	ret			; done




;---------------------------------------------------
; my vblank routine - do all graphical changes here
; while the display is not drawing
;---------------------------------------------------
VBlankFunc::	
	di		; disable interrupts
	push	af

	; increment my little timer
	ld		a, [ScrollTimer]			; get the scroll timer
	inc		a					; increment it
	ld		[ScrollTimer], a

	; is it time to scroll yet?
	and		%00000001
	jr		nz, .vblank_sprite_DMA	; only scroll ever other vblank

; load the sprite attrib table to OAM memory
.vblank_sprite_DMA

	; set the vblank occured flag
	ld		a, 1
	ld		[vblank_flag], a

	call	UpdateBulletTimers

	pop af
	ei		; enable interrupts
	
	jp DMA_ROUTINE
	
	reti	; and done



;-------------------------------------------------------------
; adjust my spaceship sprite based on d-pad presses.  This
; both moves the sprite and chooses the sprite attributes to
; make the sprite face the correct direction
;-------------------------------------------------------------
MoveSpaceship::
	push	af

	jp .done_checking_dpad
	; check buttons for d-pad presses
.check_for_up
	ld		a, [joypad_held]
	bit		PADB_UP, a
	jp		z, .check_for_down	; if button not pressed then done

	; up was held down
.check_for_upright
	; is right also held?
	ld		a, [joypad_held]
	bit		PADB_RIGHT, a
	jp		z, .check_for_upleft

	; up + right held, so sprite needs to be diagonal
	sprite_PutTile spaceship1, 1
	sprite_PutFlags spaceship1, 0

	jp		.adjust_up_pos

.check_for_upleft
	; is left also held?
	ld		a, [joypad_held]
	bit		PADB_LEFT, a
	jp		z, .set_up_only

	; up + left held, so sprite needs to be diagonal
	sprite_PutTile spaceship1, 1
	sprite_PutFlags spaceship1, OAMF_XFLIP

	jp		.adjust_up_pos

.set_up_only
	; only up was held, so sprite needs to be up
	sprite_PutTile spaceship1, 0
	sprite_PutFlags spaceship1, 0

.adjust_up_pos
	; adjust the sprite's position
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, .check_for_left

	; move sprite up a pixel
	MoveUp spaceship1, 1


	; don't check down, since up + down should never occur
	jp		.check_for_left

.check_for_down
	ld		a, [joypad_held]
	bit		PADB_DOWN, a
	jp		z, .check_for_left	; if button not pressed then done

	; down was held down
.check_for_downright
	; is right also held?
	ld		a, [joypad_held]
	bit		PADB_RIGHT, a
	jp		z, .check_for_downleft

	; down + right held, so sprite needs to be diagonal
	sprite_PutTile spaceship1, 1
	sprite_PutFlags spaceship1, OAMF_YFLIP

	jp		.adjust_down_pos

.check_for_downleft
	; is left also held?
	ld		a, [joypad_held]
	bit		PADB_LEFT, a
	jp		z, .set_down_only

	; down + left held, so sprite needs to be diagonal
	sprite_PutTile spaceship1, 1
	sprite_PutFlags spaceship1, OAMF_XFLIP + OAMF_YFLIP

	jp		.adjust_down_pos

.set_down_only
	; only down was held, so sprite needs to be down
	sprite_PutTile spaceship1, 0
	sprite_PutFlags spaceship1, OAMF_YFLIP
	
.adjust_down_pos
	; adjust the sprite's position
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, .check_for_left

	; move sprite up a pixel
	MoveDown spaceship1, 1

.check_for_left
	ld		a, [joypad_held]
	bit		PADB_LEFT, a
	jp		z, .check_for_right	; if button not pressed then done

	; left was pressed
.check_left_andUpOrDown
	ld		a, [joypad_held]
	and		PADF_UP + PADF_DOWN
	jp		nz, .adjust_left_pos	; if up or down was pressed, then we already set the sprite attribs

	; sprite needs to be horizontal
	sprite_PutTile spaceship1, 2
	sprite_PutFlags spaceship1, OAMF_XFLIP

.adjust_left_pos
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, .done_checking_dpad

	; move sprite left one pixel
	MoveLeft spaceship1, 1

	jp		.done_checking_dpad	; if left was pressed, don't check right

.check_for_right
	ld		a, [joypad_held]
	bit		PADB_RIGHT, a
	jp		z, .done_checking_dpad	; if button not pressed then done

	; right was pressed
.check_right_andUpOrDown
	ld		a, [joypad_held]
	and		PADF_UP + PADF_DOWN
	jp		nz, .adjust_right_pos	; if up or down was pressed, then we already set the sprite attribs

	; sprite needs to be horizontal
	sprite_PutTile spaceship1, 2
	sprite_PutFlags spaceship1, 0

.adjust_right_pos
	ld		a, [ScrollTimer]	; only move sprite every 2nd vblank
	and		%00000001
	jr		nz, .done_checking_dpad

	; move sprite left one pixel
	MoveRight spaceship1, 1

	jp		.done_checking_dpad	; if left was pressed, don't check right

.done_checking_dpad
	ld		a, [joypad_down]
	bit		PADB_A, a
	jr		z, .check_b_button

	ld a, [SpriteFrame]
	xor 1
	ld [SpriteFrame], a


.check_b_button

.done_move_ship




	GetSpriteXAddr spaceship1
	PutSpriteXAddr spaceship3, a
	PutSpriteXAddr spaceship5, a
	add 	8
	PutSpriteXAddr spaceship2, a
	PutSpriteXAddr spaceship4, a
	PutSpriteXAddr spaceship6, a

	GetSpriteYAddr spaceship1
	PutSpriteYAddr spaceship2, a
	sub 	8
	PutSpriteYAddr spaceship3, a
	PutSpriteYAddr spaceship4, a
	sub 	8
	PutSpriteYAddr spaceship5, a
	PutSpriteYAddr spaceship6, a
	
	sprite_GetTile spaceship1
	sprite_PutTile spaceship1, 8
	sprite_PutTile spaceship2, 9
	sprite_PutTile spaceship3, 6
	sprite_PutTile spaceship4, 7
	sprite_PutTile spaceship5, 4

	ld a, [SpriteFrame]
	and 1
	jr nz, .secondSpriteFrameSet
	sprite_PutTile spaceship6, 5
	jp .doneSpriteFrameCheck
.secondSpriteFrameSet
	sprite_PutTile spaceship6, 10
.doneSpriteFrameCheck

	sprite_GetFlags spaceship1
	sprite_PutFlags spaceship1, 0
	sprite_PutFlags spaceship2, 0
	sprite_PutFlags spaceship3, 0
	sprite_PutFlags spaceship4, 0
	sprite_PutFlags spaceship5, 0
	sprite_PutFlags spaceship6, 0

	pop		af
	ret



;------------------------------------------------------------
; return the direction to fire a bullet in from the
; spaceship's sprite attribs
;
; OUT:		a = direction ship is facing (0..7)
;           b = x start location
;			c = y start location
;------------------------------------------------------------
GetBulletDirection::
	sprite_GetTile spaceship1
	cp		1						; diagonal ship tile?
	jr		z, .ship_is_diag

	; ship is either vertical or horizontal
	cp		0		; vertical ship tile?
	jr		nz, .ship_is_horiz

	; vertical ship tile
	sprite_GetFlags spaceship1
	and     OAMF_YFLIP	; is the ship pointing down?
	jp		nz,	.ship_faces_down

.ship_faces_up
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	add     a, 2
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	sub		3
	ld		c, a
	; direction is up
	ld		a, 0
	ret

.ship_faces_down
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	add     a, 2
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	add		a, 8
	ld		c, a
	; direction is down
	ld		a, 4
	ret

.ship_is_horiz
	; horizontal ship tile
	sprite_GetFlags spaceship1
	and     OAMF_XFLIP	; is the ship pointing left?
	jp		nz,	.ship_faces_left

.ship_faces_right
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	add     a, 8
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	add		a, 2
	ld		c, a
	; direction is right
	ld		a, 2
	ret

.ship_faces_left
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	sub     3
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	add		a, 2
	ld		c, a
	; direction is left
	ld		a, 6
	ret

.ship_is_diag
	sprite_GetFlags spaceship1
	and		OAMF_XFLIP	; x flipped?
	jp		nz, .ship_is_x_flipped

.ship_is_not_x_flipped
	sprite_GetFlags spaceship1
	and		OAMF_YFLIP	; y flipped?
	jp		nz, .ship_faces_downright

.ship_faces_upright
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	add     a, 7
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	sub		2
	ld		c, a
	; direction is up-right
	ld		a, 1
	ret

.ship_faces_downright
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	add     a, 7
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	add		a, 7
	ld		c, a
	; direction is down-right
	ld		a, 3
	ret

.ship_is_x_flipped
	sprite_GetFlags spaceship1
	and		OAMF_YFLIP	; y flipped?
	jp		nz, .ship_faces_downleft

.ship_faces_upleft
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	sub     2
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	sub		2
	ld		c, a
	; direction is up-left
	ld		a, 7
	ret

.ship_faces_downleft
	; calc bullet x launch pos
	GetSpriteXAddr spaceship1
	sub     2
	ld		b, a
	; calc bullet y launch pos
	GetSpriteYAddr spaceship1
	add		a, 7
	ld		c, a
	; direction is down-left
	ld		a, 5
	ret


;------------------------------------------------------------
; launch a bullet
;------------------------------------------------------------
LaunchBullet::
	push	af
	push	bc
	push	de

	; find an empty bullet
	ld		hl, bullet_data		; get the addr of the 1st bullet
	ld		d, 16				; 16 bullet slots to check
.find_empty_bullet_loop
	ld		a, [hl]
	cp		$ff			; is this bullet unused
	jr		z, .found_empty_bullet

	inc		hl	; skip 2 bytes, to top of next bullet
	inc		hl

	dec		d
	jr		nz, .find_empty_bullet_loop

	; no slots left... exit
	pop		de
	pop		bc
	pop		af
	ret

.found_empty_bullet
	call	GetBulletDirection	; get the direction and offset to fire the bullet at
	; a = orientation
	; b = x pos
	; c = y pos
	; hl = bullet data to launch
	; index into bullet array = 16 - d

	ld		[hli], a	; store the orientation
	ld		[hl], 60	; bullet lasts 1 second (60 vblanks)

	ld		a, 16
	sub		d		; a = index into bullet array

	ld		hl, bullet_sprites	; get top of bullet sprites

	sla		a
	sla		a		; multiply index by 4 (4 bytes per sprite)
	ld		e, a	; store it in de
	ld		d, 0

	add		hl, de	; I should be pointing at the correct sprite addr

	; load the sprite info
	ld		[hl], c
	inc		hl
	ld		[hl], b
	inc		hl
	ld		[hl], 5	; bullets use tile 5
	inc		hl
	ld		[hl], 0

	pop		de
	pop		bc
	pop		af
	ret


;-----------------------------------------------------------------
; update the bullet timing ever vblank
;-----------------------------------------------------------------
UpdateBulletTimers::
	push	af
	push	bc
	push	hl

	ld		hl, bullet_data
	ld		b, 16		; 16 bullets to update
.update_bullets_loop
	ld		a, [hli]
	cp		$ff
	jr		z, .update_bullets_loop_end

	; this is an active bullet
	dec		[hl]	; decrement the timer
	jr		nz, .update_bullets_loop_end

	; this bullet's timer ran out
	push	hl		; save where we were
	push	bc

	dec		hl		; go back a byte
	ld		a, $ff
	ld		[hl], a	; this sprite is no longer active

	; calc this bullet's sprite location
	ld		a, 16	; calc index (16 - b)
	sub		b
	ld		e, a	; store index in de
	sla		e
	sla		e		; 4 bytes per sprite attrib
	ld		d, 0
	ld		hl, bullet_sprites
	add		hl, de

	ld		a, $00
	ld		[hli], a
	ld		[hl], a		; turn of the sprite in the attrib table
	pop		bc
	pop		hl

.update_bullets_loop_end
	inc		hl
	dec		b
	jr		nz, .update_bullets_loop

	pop		hl
	pop		bc
	pop		af
	ret


;------------------------------------------------------
; update bullet positions
;------------------------------------------------------
UpdateBulletPositions::
	push	af
	push	bc

	ld		hl, bullet_data
	ld		b, 16		; 16 bullets to update
.update_bullets_pos_loop
	ld		a, [hl]
	cp		$ff
	jp		z, .update_bullets_pos_loop_end

	; this is an active bullet
	; get its sprite addr
	push	hl
	ld		a, 16	; calc index (16 - b)
	sub		b
	ld		e, a	; store index in de
	sla		e
	sla		e		; 4 bytes per sprite attrib
	ld		d, 0
	ld		hl, bullet_sprites
	add		hl, de
	ld		d, h
	ld		e, l	; store the address in de
	pop		hl

.check_bullet_fly_up
	ld		a, [hl]		; get the orientation info
	cp		0		; flying up?
	jr		nz, .check_bullet_fly_upright

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	ld		a, [hl]
	sub		3
	ld		[hl], a
	pop		hl

	jp		.update_bullets_pos_loop_end

.check_bullet_fly_upright
	ld		a, [hl]		; get the orientation info
	cp		1		; flying up-right?
	jr		nz, .check_bullet_fly_right

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	ld		a, [hl]
	sub		2
	ld		[hli], a
	ld		a, [hl]
	add		a, 2
	ld		[hl], a
	pop		hl

	jp		.update_bullets_pos_loop_end

.check_bullet_fly_right
	ld		a, [hl]		; get the orientation info
	cp		2		; flying right?
	jr		nz, .check_bullet_fly_downright

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	inc		hl
	ld		a, [hl]
	add		a, 2
	ld		[hl], a
	pop		hl

	jp		.update_bullets_pos_loop_end

.check_bullet_fly_downright
	ld		a, [hl]		; get the orientation info
	cp		3		; flying down-right?
	jr		nz, .check_bullet_fly_down

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	ld		a, [hl]
	add		a, 2
	ld		[hli], a
	ld		a, [hl]
	add		a, 2
	ld		[hl], a
	pop		hl

	jp		.update_bullets_pos_loop_end

.check_bullet_fly_down
	ld		a, [hl]		; get the orientation info
	cp		4		; flying down?
	jr		nz, .check_bullet_fly_downleft

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	ld		a, [hl]
	add		a, 3
	ld		[hl], a
	pop		hl

	jr		.update_bullets_pos_loop_end

.check_bullet_fly_downleft
	ld		a, [hl]		; get the orientation info
	cp		5		; flying down-left?
	jr		nz, .check_bullet_fly_left

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	ld		a, [hl]
	add		a, 2
	ld		[hli], a
	ld		a, [hl]
	sub		2
	ld		[hl], a
	pop		hl

	jp		.update_bullets_pos_loop_end

.check_bullet_fly_left
	ld		a, [hl]		; get the orientation info
	cp		6		; flying left?
	jr		nz, .check_bullet_fly_upleft

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	inc		hl
	ld		a, [hl]
	sub		3
	ld		[hl], a
	pop		hl

	jp		.update_bullets_pos_loop_end

.check_bullet_fly_upleft
	ld		a, [hl]		; get the orientation info
	cp		7		; flying up-left?
	jr		nz, .update_bullets_pos_loop_end

	; update this sprite's position
	push	hl
	ld		h, d
	ld		l, e	; grab the sprite address
	ld		a, [hl]
	sub		2
	ld		[hli], a
	ld		a, [hl]
	sub		2
	ld		[hl], a
	pop		hl

.update_bullets_pos_loop_end
	inc		hl
	inc		hl
	dec		b
	jp		nz, .update_bullets_pos_loop

	pop		bc
	pop		af
	ret






;-------------------------------------------------------------
; scroll the screen up a few pixels, adding new tiles to the
; bottom of the screen map as necessary
;
; in:	c = # pixels to scroll
;-------------------------------------------------------------
ScrollScreenUp::
	push	af
	push	bc
	push	de
	push	hl

	; get the number of pixels we have scrolled since the last tile load
	ld		a, [y_scrl_accum]
	add		a, c
	cp		8	; has the accum gone past 7?
	jr		c, .scroll_screen_up_done	; no, we don't need to add any tiles

	; yes, we need to add some tiles to the bottom of the screen
.scroll_screen_up_tiles_loop
	; calc the line we need to add tiles to
	push	af		; save the loop variable
	ld		a, [rSCY]	;get the current y screen position
	srl		a
	srl		a
	srl		a	; divide by 8 (get tile pos from pixel pos)
	add		a, 20	; screen is 18 tiles hi, so add tiles 20 lines down from screen pos
	cp		32		; did we go past the end of the bkg map?
	jr		c, .scroll_screen_up_add_tiles	; no, we have the tile line we need

	; yes, we need to wrap
	sub		32

.scroll_screen_up_add_tiles
	ld      b, a	; store line number for input to routine
	ld		a, [world_y]	; get our world screen position (lo byte)
	ld		d, a
	ld		a, [world_y+1]	; hi byte
	srl		a
	rr		d	; left shift the hi and low bytes
	srl		a
	rr		d	; left shift the hi and low bytes
	srl		a
	rr		d	; left shift the hi and low bytes - div by 8!
	ld		a, d	; the low byte hold the interesting data after ths shift
	add		a, 20	; go down 20 tiles to tile we want to add
	cp		64	; world is 64 tiles tall
	jr		c, .scroll_screen_up_wrap_world

	sub		64	; wrap

.scroll_screen_up_wrap_world
	ld		e, a
	ld		d, 0
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit - multiply world_y by 32

	ld		hl, WorldMap	; get the top of the world map
	add		hl, de	; set address of line to load

	call	AddWorldTilesLine	; add the line of tiles!

	pop		af		; restore the loop variable

	sub		8	; did one tile line (8 pixels)
	cp		7	; still greater than 7?
	jr		nc, .scroll_screen_up_tiles_loop	; yes, do another line of tiles!

.scroll_screen_up_done
	; update the y scroll accumulator and the world pos
	ld		[y_scrl_accum], a

	ld		a, [world_y+1]	; get world y pos hi byte
	ld		b, a
	ld		a, [world_y]	; lo byte
	add		a, c	; adjust world pos
	jr		nc, .scroll_screen_up_nocarry

	inc		b	; carry flag... increase hi byte

.scroll_screen_up_nocarry
	ld		d, a
	ld		a, b	; get hi byte of world y
	cp      $02		; less than hi byte of world height (64 tiles * 8 pixels/byte = 512)?
	jr      c, .scroll_screen_up_worldwrap
	ld		a, d
	cp		$00		; lo byte of world x < lo byte of 512?
	jr		c, .scroll_screen_up_worldwrap

	; world y is greater than world map y... we need to wrap
	ld		b, 0	; clear hi byte, leave lo byte alone (now $20x becomes $00x)

.scroll_screen_up_worldwrap
	ld		a, d
	ld		[world_y], a
	ld		a, b
	ld		[world_y+1], a

	; update the background scroll plane scroll
	ld		a, [rSCY]
	add		a, c
	ld		[rSCY], a

	pop		hl
	pop     de
	pop		bc
	pop		af
	ret


;-------------------------------------------------------------
; scroll the screen down a few pixels, adding new tiles to the
; top of the screen map as necessary
;
; in:	c = # pixels to scroll
;-------------------------------------------------------------
ScrollScreenDown::
	push	af
	push	bc
	push	de
	push	hl

	; get the number of pixels we have scrolled since the last tile load
	ld		a, [y_scrl_accum]
	sub		c
	cp		8	; has the accum gone below 0?
	jr		c, .scroll_screen_down_done	; no, we don't need to add any tiles

	; yes, we need to add some tiles to the top of the screen
.scroll_screen_down_tiles_loop
	; calc the line we need to add tiles to
	push	af		; save the loop variable
	ld		a, [rSCY]	;get the current y screen position
	srl		a
	srl		a
	srl		a	; divide by 8 (get tile pos from pixel pos)
	sub		2	; add tiles to line two tiles off the top of the screen
	cp		32		; did we go past the end of the bkg map?
	jr		c, .scroll_screen_down_add_tiles	; no, we have the tile line we need

	; yes, we need to wrap
	add		a, 32

.scroll_screen_down_add_tiles
	ld      b, a	; store line number for input to routine
	ld		a, [world_y]	; get our world screen position (lo byte)
	ld		d, a
	ld		a, [world_y+1]	; hi byte
	srl		a
	rr		d	; left shift the hi and low bytes
	srl		a
	rr		d	; left shift the hi and low bytes
	srl		a
	rr		d	; left shift the hi and low bytes - div by 8!
	ld		a, d	; the low byte hold the interesting data after ths shift
	sub		2	; go up two tiles to tile we want to add
	cp		64	; world is 64 tiles tall
	jr		c, .scroll_screen_down_wrap_world

	add		a, 64	; wrap

.scroll_screen_down_wrap_world
	ld		e, a
	ld		d, 0
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit - multiply world_y by 32

	ld		hl, WorldMap	; get the top of the world map
	add		hl, de	; set address of line to load

	call	AddWorldTilesLine	; add the line of tiles!

	pop		af		; restore the loop variable

	add		a, 8	; did one tile line (8 pixels)
	cp		8	; still less than zero (greater than 8)?
	jr		nc, .scroll_screen_down_tiles_loop	; yes, do another line of tiles!

.scroll_screen_down_done:
	; update the y scroll accumulator and the world pos
	ld		[y_scrl_accum], a

	ld		a, [world_y+1]	; get world y pos hi byte
	ld		b, a
	ld		a, [world_y]	; lo byte
	sub		c	; adjust world pos
	jr		nc, .scroll_screen_down_nocarry

	dec		b	; carry flag... increase hi byte

.scroll_screen_down_nocarry
	ld		d, a
	ld		a, b	; get hi byte of world y
	cp      $02		; less than hi byte of world height (64 tiles * 8 pixels/byte = 512)?
	jr      c, .scroll_screen_down_worldwrap
	ld		a, d
	cp		$00		; lo byte of world x < lo byte of 512?
	jr		c, .scroll_screen_down_worldwrap

	; world y is greater than world map y... we need to wrap
	ld		b, 1	; set hi byte to 1 (now $fffx becomes $01fx)

.scroll_screen_down_worldwrap
	ld		a, d
	ld		[world_y], a
	ld		a, b
	ld		[world_y+1], a

	; update the background scroll plane scroll
	ld		a, [rSCY]
	sub		c
	ld		[rSCY], a

	pop		hl
	pop     de
	pop		bc
	pop		af
	ret


;-----------------------------------------------------
; add a line of tiles to the background map
;
; in:	b = bkg map line number to add tiles to
;		hl = ptr to 1st tile to add
;-----------------------------------------------------
AddWorldTilesLine::
	push	af
	push	bc
	push	de

	ld		a, [rSCX]	; get the horiz bkg map scroll value
	srl		a
	srl		a
	srl		a	; divide by 8 (convert pixel value to tile value)
	sub		2	; go left two more tiles
	cp		32	; did we go past 0?
	jr		c, .add_tiles_line_get_vram_addr	; no

	; yes, wrap us back
	add		a, 32

.add_tiles_line_get_vram_addr
	ld		c, a	; store the x tile number
	ld		e, b	; get the bkg line number
	ld		d, 0
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit
	sla		e
	rl		d	; shift de up one bit - multiply de by 32
	ld		a, e
	add		a, c
	ld		e, a	; add the xpos to de
	push	hl
	ld		hl, _SCRN0	; load to map 0
	add		hl, de	; hl now points at map memory to load to
	ld		d, h
	ld		e, l	; store it in de
	pop		hl		; and restore hl = world tile ptr

	ld		b, 24	; load 24 tiles
.add_tiles_line_loop
	; only write during
;	ld		a, [rSTAT]	; get the status
;	and		STATF_OAM			; don't write during sprite and transfer modes
;	jr		nz, add_tiles_line_loop

	ld		a, [hli]
	ld		[de], a		; load a tile ref
	inc		c		; inc the x pos
	ld		a, c
	cp		32
	jr		c, .add_tiles_line_loop_end

	sub		32
	ld		c, a	; wrap back

	ld		a, e
	sub		32
	ld		e, a
	cp		32
	jr		c, .add_tiles_line_loop_end

	dec		d		; subtract 32 from de

.add_tiles_line_loop_end
	ld		a, e	; increment de
	inc		e
	jr		nz, .add_tiles_line_incDE
	inc		d	; e became $00, so we need to carry the add to top byte
.add_tiles_line_incDE
	dec		b
	jp		nz, .add_tiles_line_loop

	pop		de
	pop		bc
	pop		af
	ret










; tiles are here
TileData:
INCLUDE "draculaTiles.z80"

; map is here
WorldMap:
BkgMapData:
INCLUDE "draculaMap.z80"



;-------------------------------------------------------------------------
; Internal RAM... store dynamic data here
;-------------------------------------------------------------------------
SECTION	"RAM_Start_Sprites",WRAM0[$C000]


	SpriteAttr	spaceship1
	SpriteAttr	spaceship2
	SpriteAttr	spaceship3
	SpriteAttr	spaceship4
	SpriteAttr	spaceship5
	SpriteAttr	spaceship6
	SpriteAttr	bat1
	SpriteAttr	bat2

; bullet sprites start here (16 of them)
bullet_sprites:
ds		1





SECTION	"RAM_Other_Variables",WRAM0[$C0A0]
; other variables

; joypad values
joypad_held:
ds		1		; what buttons are currently held
joypad_down:
ds		1		; what buttons went down since last joypad read

; scroll values
world_x:
ds		2
world_y:
ds		2

x_scrl_accum:
ds		1
y_scrl_accum:
ds		1

; bullets (16 of them, 2 bytes for each)
;	1st byte = orientation (3 bits) - if $ff, this bullet is unused
;	2nd byte = time left to live (in vblanks)
bullet_data:
ds		32

; frame timing
vblank_flag:
ds		1		; set if a vblank occured since last pass through game loop

; scroll direction flag
scrl_dir_flag:
ds		1

; temp variables
ScrollTimer:
ds		1		; temp variable for slowing down scroll speed

WaveTimer:
ds		1	

SpriteFrame:
ds		1	

