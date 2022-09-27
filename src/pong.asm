;****************************************************************************************************************************************************
;*	Pong Source File
;*
;*  This is the whole project here. It's kinda organized.

;****************************************************************************************************************************************************
;*	Includes
;****************************************************************************************************************************************************
; system includes
INCLUDE	"Hardware.inc"

;;;This is the list of all the old EQUates. These are now exported labels further down. Try Unioning them later if ram gets tight.

;wBallX			EQU $C00D
;wBallY			EQU $C00C
;these show the direction of the ball
;wBallFlags		EQU $C100
;this is the decimal speed of the ball
;wBallSpeed		EQU $C101
;stores the last ping and the status of serial updates
;wLastComm		EQU $C102
;holds the current wScene (title, menu, etc)
;wScene			EQU $C103
;flag that shows we are connected to another GB
;wIsConnected	EQU	$C104
;flag showing whether we have wPriority advantage in game
;wPriority		EQU	$C105
;counts vblanks since startup. only used to determine wPriority and should overflow
;occured to me that rDIV is literally just this, so reuse this I guess?
;depending on how we do resyncs, this could be useful, though (rDIV clocks much quicker time than this ever would, so having two reference frames is nice) so I'll keep it for now.
;actually it just stores rDIV right now, updated every VBlank, so it literally is rDIV. maybe fix that?
;also note that resyncs will not be necessary since emu link cable auto-pauses (at least BGB does I think) and packets won't be lost -- however emu link cable is ass anyways so ignore it
;wVBlankCt		EQU	$C106
;indicates the last interrupt was a vblank
;wVBlankFlag		EQU	$C107
;holds the desired wTransition to another wScene (fade is most common, but there are some weird ones)
;wTransition		EQU	$C108
;flags for pending data updates (like clock or joypad) that the other GB is waiting on
;wUpdateFlags	EQU	$C109
;shows what serial mode we are in: 0 is control, 1 is wait, and 2 is data. The mode we are in is controlled by the handler, but you can read it to see if something happened.
;wSerialFlag		EQU $C10A
;save the state of our joypad when grabbed so we don't use two different joypad states in a loop
;wJoypadSav0		EQU	$C10B
;also save the opponent joypad here, we'll need it several times
;wJoypadSav1		EQU	$C10C
;flag that is set if only 1 player is playing the game (for testing purposes now, but will be a part of the game later)
;wOnePlayer		EQU	$C10D
;store the health of each target on the screen
;wTargetState0	EQU $C10E
;wTargetState1	EQU	$C10F
;and this flags which targets need to be updated (makes UpdateTargets less redundant)
;wTargetFlags	EQU $C110
;This tells us who the winner was. If it's 1, we lost. 2 means we won, and 3 means a draw. If it's 0 yet, the game isn't over.
;wPongState		EQU	$C111
;This keeps track of the number of tile updates we are going to do, as well as the offset to append tiles to in the queue.
;wTileCount		EQU	$C112
;This is temp storage for sp. It's 16-bit, so skip the next address.
;wSaveSp			EQU	$C113
;This stores the option we are pointing at. For convenience right now and since we don't have more than one option pane at a time, we'll just call this OptionNo
;wOptionNo		EQU $C115
;And this is for clocking frames for timers on inputs (so the ARR isn't too low)
;wInputClock		EQU	$C116

;UpdateFlags format:
;7: joypad
;6: sync lcd
;5: clock (wPriority)
;4-0: unused
;if it's set, an update is pending, otherwise it's not and you can start the request yourself

;TargetState0/1 FORMAT:
;7-6: target2 (bottom) hits
;5-4: target1 (middle) hits
;3-2: target0 (top) hits
;1-0: unused
;hits starts at 00 and increments up to 11 (broken)
;If this register ever is %111111XX, that side has lost
;this only stores the target's state graphically. Refer to another register (coming soon) for the actual hit points each target has.

;TargetFlags FORMAT:
;7-5: Opponent updates (bottom, middle, top)
;3-1: Player updates (bottom, middle, top)
;4&0: unused
;if the bit is set, this tells UpdateTargets to reload those tiles based on TargetState0/1

;this is the location in tile memory of the first target tile for both player and opponent
TARGET_TILE_START	EQU	$34
OPP_TAR_TILE_START	EQU $40
;and these are the locations of the targets in the tilemap. Get the next part of the target by adding 32.
vTarget00	EQU $9841
vTarget10	EQU $9852
vTarget01	EQU vTarget00 + $20*$5
vTarget11	EQU vTarget10 + $20*$5
vTarget02	EQU vTarget01 + $20*$5
vTarget12	EQU vTarget11 + $20*$5

;BallFlags BITFLAGS:
;7: x-direction	0-Right 1-Left
;6: y-direction	0-Down 1-Up
;5-4: unused
;3-0: binary speed (including alternator and controller bits)

;Control bytes so far:
;$00 - nothing
;$01 - update joypads
;$10 - initial connection byte
;$11 - lcd start sync
;$12 - send clocks for wPriority setting
;$FF - not ready for comms / no comms

;****************************************************************************************************************************************************
;*	cartridge header
;****************************************************************************************************************************************************

	SECTION	"Org $00",ROM0[$00]
RST_00:	
	jp	$100

	SECTION	"Org $08",ROM0[$08]
RST_08:	
	jp	$100

	SECTION	"Org $10",ROM0[$10]
RST_10:
	jp	$100

	SECTION	"Org $18",ROM0[$18]
RST_18:
	jp	$100

	SECTION	"Org $20",ROM0[$20]
RST_20:
	jp	$100

	SECTION	"Org $28",ROM0[$28]
RST_28:
	jp	$100

	SECTION	"Org $30",ROM0[$30]
RST_30:
	jp	$100

	SECTION	"Org $38",ROM0[$38]
RST_38:
	jp	$100

	SECTION	"V-Blank IRQ Vector",ROM0[$40]
VBL_VECT:
	call HandleVBlank
	reti
	
	SECTION	"LCD IRQ Vector",ROM0[$48]
LCD_VECT:
	reti

	SECTION	"Timer IRQ Vector",ROM0[$50]
TIMER_VECT:
	reti

	SECTION	"Serial IRQ Vector",ROM0[$58]
SERIAL_VECT:
	call HandlePing	
	reti

	SECTION	"Joypad IRQ Vector",ROM0[$60]
JOYPAD_VECT:
	reti
	
	SECTION	"Start",ROM0[$100]
	nop
	jp	Start

	; $0104-$0133 (Nintendo logo - do _not_ modify the logo data here or the GB will not run the program)
	DB	$CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
	DB	$00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
	DB	$BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

	; $0134-$013E (Game title - up to 11 upper case ASCII characters; pad with $00)
	DB	"PONG",0,0,0,0,0,0,0
		;0123456789A

	; $013F-$0142 (Product code - 4 ASCII characters, assigned by Nintendo, just leave blank)
	DB	"    "
		;0123

	; $0143 (Color GameBoy compatibility code)
	DB	$00	; $00 - DMG 
			; $80 - DMG/GBC
			; $C0 - GBC Only cartridge

	; $0144 (High-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0145 (Low-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0146 (GameBoy/Super GameBoy indicator)
	DB	$00	; $00 - GameBoy

	; $0147 (Cartridge type - all Color GameBoy cartridges are at least $19)
	DB	$00	; $19 - ROM + MBC5

	; $0148 (ROM size)
	DB	$00	; $01 - 512Kbit = 64Kbyte = 4 banks

	; $0149 (RAM size)
	DB	$00	; $00 - None

	; $014A (Destination code)
	DB	$01	; $01 - All others
			; $00 - Japan

	; $014B (Licensee code - this _must_ be $33)
	DB	$33	; $33 - Check $0144/$0145 for Licensee code.

	; $014C (Mask ROM version - handled by RGBFIX)
	DB	$00

	; $014D (Complement check - handled by RGBFIX)
	DB	$00

	; $014E-$014F (Cartridge checksum - handled by RGBFIX)
	DW	$00


;****************************************************************************************************************************************************
;*	Program Start
;****************************************************************************************************************************************************

SECTION "Program Start",ROM0

Start::
	di						;don't need interrupts quite yet
	ld a,%00001001
	ld [rIE],a				;we only want some interrupts here (and by some I mean only serial and vblank)
	ld a,$FF
	ld [rSB],a				;We are NOT ready to start communications, so load the $FF message byte in.
	ld [wLastComm],a		;This is a copy of the last message sent. Right now we would send a NO_COMM request, which is $FF.
	ld	sp,$FFFE			;set the stack to right up at the top of HRAM
	call WaitVBlank			;wait for v-blank

	ld	a,0		
	ldh	[rLCDC],a			;turn off LCD 
	ld [wScene],a			;Also set the wScene to 0 (title screen)
	ld [wIsConnected],a		;Also we aren't connected yet
	ld [wPriority],a		;Also we start without host advantage
	ld [wTransition],a		;Also the first wTransition is a fade
	ld [wSerialFlag],a		;Also we are in control mode
	ld [wUpdateFlags],a		;Also no transfers are pending
	ld [wJoypadSav0],a
	ld [wJoypadSav1],a		;zero out the paddle saves too
	ld [wTargetState0],a	;no targets have taken damage yet either
	ld [wTargetState1],a
	ld [wTargetFlags],a		;and none are pending update yet
	ld [wPongState],a		;also nobody wins yet
	ld [wTileCount],a		;also no tile updates are pending
	ld [wOptionNo],a		;may as well zero this right now while we can (start on first option)
	ld [wOnePlayer],a		;we start assuming two-player by zeroing this byte

	call LoadTiles			;gotta get the tiles in VRAM first
	
	ld hl,TitleMap
	call LoadMap			;load up the title map
	

	ld	a,%11100100			;load a normal palette up
	ldh	[rBGP],a	
	ldh [rOBP0],a
	ldh [rOBP1],a	
	
	ld	a,%10010001	
	ldh	[rLCDC],a			;turn on the LCD, BG, etc
	
	call ClearSpriteRam
	call CopyDMA
	call WaitVBlank
	call hDMARoutine
	
TitleHandle::
	
	call WaitFrame			;Give it a wait
	call GetJoypad			;It's start,select,b,a,down,up,left,right, from bits 7-0 of a
	and %11110000			;Only interested in the buttons, thanks
	jr z, TitleHandle		;If a button is pressed, lets move off the title screen

	ld a,4
	ld [wScene],a			;Set the wScene to the main menu screen

MainLoop:: 
	
	;First we look at the wTransition handler
.wTransition
	ld a,[wTransition]
.fade
	cp 0
	jr nz, .sp1
	call FadeTrans
	jp .sceneHandler
.sp1
	cp 1
	jr nz,.sp2		
	call TransSP1
	jp .sceneHandler
.sp2
	cp 2
	jr nz,.sp3		
	call TransSP2
	jp .sceneHandler
.sp3
	cp 3
	jr nz,.sp4		
	call TransSP3
	jp .sceneHandler
.sp4
	cp 4
	jr nz,.sceneHandler		;hanging if
	call TransSP4
	jp .sceneHandler
	
.sceneHandler
	;Now we pass it on to the wScene handler
	ld a,[wScene]
.connect
	cp 1
	jr nz,.pong
	call ConnectToGameBoy
	jp MainLoop
.pong
	cp 2
	jr nz,.results
	call PlayPong
	jp MainLoop
.results
	cp 3
	jr nz,.mainMenu
	call ShowResults
	jp MainLoop
.mainMenu
	cp 4
	jr nz,.lobby
	call MainMenu
	jp MainLoop
.lobby
	cp 5
	jr nz,.options
	call Lobby
	jp MainLoop
.options
	cp 6
	jr nz,.now_what
	call OptionsScreen
	jp MainLoop
.now_what
	;add more stuff here as more scenes are added
	;for now just gonna make the gb reset if we get here
	jp Start
	
FadeTrans::
	;Transition stuff first
	ld c,3
.transitionOut
	call WaitVBlank			;wait for VBlank
	ld a,[rBGP] 			;grab the colors
	sla a
	sla a					;fade everything down
	ld [rBGP],a				;pop it back in
	ld b,15					;we're waiting 15 v-blanks (roughly half a second)
.transitionOutLoop
	call WaitFrame			;chill for a bit
	dec b
	jr nz,.transitionOutLoop
	dec c
	jr nz,.transitionOut
	
	;then load the new map
	call GetNextMap
	call WaitVBlank
	ld	a,0		
	ldh	[rLCDC],a			;turn off LCD 
	call LoadMap
	ld	a,%10010001	
	ldh	[rLCDC],a			;turn on the LCD, BG, etc
	
	;then wTransition back in
	ld c,3
.transitionIn
	call WaitVBlank
	ld a,c
	cp 3
	jr nz,.next
	ld a,%01000000
	jr .out
.next
	cp 2
	jr nz,.next2
	ld a,%10010000
	jr .out
.next2
	ld a,%11100100
.out
	ld [rBGP],a
	ld b,15					;we're waiting 15 v-blanks (roughly half a second)
.transitionInLoop
	call WaitFrame			;chill for a bit
	dec b
	jr nz,.transitionInLoop
	dec c
	jr nz,.transitionIn
	ret
	
;This transitions from connect to the lobby (but right now is used to go straight ingame, as there is no lobby)
TransSP1::
	call GetNextMap			;load the next map in
	call WaitVBlank
	ld a,0
	ldh [rLCDC],a
	call LoadMap
	;also make sure the lcd restart is synced across both GBs. Not sure if this is really necessary, but we're doing it.
	ld a,[wOnePlayer]
	cp 1
	jr z,.transferFinish	;if we're in sp, just skip this and turn it on
	ld a,[wUpdateFlags]
	bit 6,a					;is a sync pending yet?
	jr z,.request
	call LinkRespond		;if so, respond
	jr .turnItOnBaby
.request
	ld a,$11
	ld [rSB],a				;otherwise, send the request!
	call LinkRequest	
.turnItOnBaby
	call LinkSwap			;since we only want to sync, we don't care what we swap -- we'll ignore it anyways. We just do this to put the handler back in control mode and also to REALLY sync things up
.transferFinish
	ld a,%10010001
	ldh [rLCDC],a			;now TURN IT ON
	ret						;guess we're done then
	
;this is the Transition from game to results screen. This'll have a lot of moving parts as well, but it also sucks for now.
TransSP2::
	call GetNextMap			;very basic routine for now -- just turns off the screen, copies the map, and turns it back on
	call WaitVBlank
	xor a
	ldh [rLCDC],a
	call LoadMap
	ld a,%10010001
	ldh [rLCDC],a
	ret
	
;this is the Transition from menu to lobby (solo). This'll have a lot of moving parts as well, but it also sucks for now.
TransSP3::
	call GetNextMap			;very basic routine for now
	call WaitVBlank
	xor a
	ldh [rLCDC],a
	call LoadMap
	ld a,%10010001
	ldh [rLCDC],a
	ret
	
;this is the Transition from menu to connect screen (duel). This'll have a lot of moving parts as well, but it also sucks for now.
TransSP4::
	call GetNextMap			;very basic routine for now
	call WaitVBlank
	xor a
	ldh [rLCDC],a
	call LoadMap
	ld a,%10010001
	ldh [rLCDC],a
	ret
	
MainMenu::
	ld hl,wCursorOne		;initialize cursor sprites
	ld a,64
	ldi [hl],a
	ld a,56
	ldi [hl],a
	ld a,51
	ldi [hl],a
	xor a
	ldi [hl],a
	ld a,64					;cursor 2
	ldi [hl],a
	ld a,112
	ldi [hl],a
	ld a,51
	ldi [hl],a
	xor a
	ldi [hl],a				
	
	call WaitVBlank
	
	ld	a,%10010011	
	ldh	[rLCDC],a			;enable sprites
	
	call hDMARoutine		;load the new OAM
	
	xor a
	ld [wInputClock],a		;zero the input clock
	
.loop
	call GetJoypad			;For quick reference: It's start,select,b,a,down,up,left,right, from bits 7-0 of a
	
	;process button press first
	ld b,a
	and %10010000			;check for A or START
	jr z,.checkChange		;if zero, then we can try dpad
	
	ld a,[wOptionNo]		;since we're selecting something, let's see what we're selecting
	or a
	jr nz,.out0
.solo
	ld a,2
	ld [wScene],a			;the lobby is next for solo, but we're hijacking it to go straight in-game for now
	ld a,1
	ld [wTransition],a		;and this is the route we take there -- this should 'sync' the gameboys but we're in SP so it should skip that part
	ld [wOnePlayer],a		;also set the single player flag
	ret
.out0
	cp 1
	jr nz,.options
.duel
	ld a,1
	ld [wScene],a			;connection is next for duel
	ld a,4
	ld [wTransition],a		;and this is the route we take there
	ret
.options
	ld a,6
	ld [wScene],a			;next up here is the options screen
	ld a,0
	ld [wTransition],a		;we'll make that a simple fade for now
	ret
.checkChange
	;then we check for dpad inputs (honestly order doesn't matter here, it's a menu selection, nobody cares if their frame-synced option change is discarded for their confirm input)
	ld a,[wInputClock]
	or a					;skip this if the clock hasn't run out yet
	jr nz,.out
	bit 3,b					;they press down?
	jr z,.upCheck
.down
	ld a,[wOptionNo]
	cp 2
	jr z,.out				;we can't go any higher than 2
	inc a
	ld [wOptionNo],a		;otherwise increment and continue
	ld a,[wCursorOne]
	add 16
	ld [wCursorOne],a		;also move the option cursors down
	ld a,[wCursorTwo]
	add 16
	ld [wCursorTwo],a
	jr .setClock
.upCheck
	bit 2,b
	jr z,.out
.up
	ld a,[wOptionNo]
	or a
	jr z,.out				;we can't go any lower than 0
	dec a
	ld [wOptionNo],a		;otherwise decrement and continue
	ld a,[wCursorOne]
	sub 16
	ld [wCursorOne],a		;also move the option cursors up
	ld a,[wCursorTwo]
	sub 16
	ld [wCursorTwo],a
.setClock
	ld a,7					;also set the clock (this number felt good)
	ld [wInputClock],a
.out
	call WaitVBlank
	call hDMARoutine		;update graphics
	ld a,[wInputClock]
	or a
	jp z,.loop
	dec a
	ld [wInputClock],a
	jp .loop				;and loop
	
OptionsScreen::
	jp Start				;placeholder screen, just reset for now
	ret
	
Lobby::
	jp Start				;again, placeholder
	ret

PlayPong::
	;initialize sprites
	ld hl,PaddleOne			;paddle1
	ld a,$20
	ld [hl+],a
	ld [hl+],a
	ld a,2
	ld [hl+],a
	ld a,%00000000
	ld [hl+],a
	ld a,$28				;paddle2
	ld [hl+],a
	ld a,$20
	ld [hl+],a
	ld a,2
	ld [hl+],a
	ld a,%00000000
	ld [hl+],a
	ld a,$30				;paddle3
	ld [hl+],a
	ld a,$20
	ld [hl+],a
	ld a,2
	ld [hl+],a
	ld a,%00000000
	ld [hl+],a
	ld a,$60				;ball
	ld [hl+],a
	ld a,$4E+$8		
	ld [hl+],a
	ld a,1
	ld [hl+],a
	ld a,%00000000
	ld [hl+],a
	ld a,$20				;opp1
	ld [hl+],a
	ld a,$88
	ld [hl+],a
	ld a,2
	ld [hl+],a
	ld a,%00100000
	ld [hl+],a
	ld a,$28				;opp2
	ld [hl+],a
	ld a,$88
	ld [hl+],a
	ld a,2
	ld [hl+],a
	ld a,%00100000
	ld [hl+],a
	ld a,$30				;opp3
	ld [hl+],a
	ld a,$88
	ld [hl+],a
	ld a,2
	ld [hl+],a
	ld a,%00100000
	ld [hl+],a
	;Remember that the speed settings (bits 0-3) are as follows:
	;0000 - horizontal (speed 0)
	;1000 - between 0 and 1 (speed +/-1)
	;0001 - moving diagonally (speed +/-2)
	;1001 - between 1 and 2 (speed +/-3)
	;0010 - moving 2 pix up/down per frame (speed +/-4)
	;1010 - between 2 and 3 (speed +/-5)
	;0011 - moving 3 pix per frame (speed +/-6)
	;Don't set it to anything else -- It can cause game-breaking errors
	ld hl,wBallFlags	
	ld a,%10000001
	ld [hl],a
	ld a,1					;This is the ball's speed variable
	ld [wBallSpeed],a
	
	;Before we start, we need to decide 'host' advantage
	;this is done by comparing CLOCK values -- the larger one is 'host'
	ld a,[wOnePlayer]
	cp 1
	jr z,.noHostAdv			;if we're in sp, just skip this without host adv
	ld a,[wUpdateFlags]
	bit 5,a					;check to see if the other GB is waiting for clock already
	jr z,.sendReq			;if not, we better send it
	call LinkRespond		;otherwise, signal that we are ready!
	jr .prep
.sendReq
	ld a,$12
	ld [rSB],a
	call LinkRequest		;request the transfer! By time we return, we can start swapping
.prep
	ld a,[wVBlankCt]
	ld [rSB],a
	call LinkSwap			;and send it
	ld b,a
	ld a,[wVBlankCt]
	cp b					;is our clock bigger or smaller?
	jr c,.noHostAdv
	ld a,1
	ld [wPriority],a		;we got advantage yay
	ld a,[wBallFlags]		;host's ball starts going right
	res 7,a
	ld [wBallFlags],a
	ld a,[wBallX]			;also move it a little bit so both balls are mirrored
	inc a
	ld [wBallX],a
.noHostAdv

	call WaitVBlank
	
	ld	a,%10010011	
	ldh	[rLCDC],a			;enable sprites
	
	call hDMARoutine
.loop
	;check win conditions here
	ld b,0
	ld a,[wTargetState0]
	cp %11111100			;all the targets busted yet?
	jr nz,.checkOpp
	inc b					;could still be a draw, but that's one side down
	ld a,1
	ld [wPongState],a		;signal that we lost
.checkOpp
	ld a,[wTargetState1]
	cp %11111100			;how about the opp targets?
	jr nz,.decide
	inc b					;if b was inc-ed twice, we have the rare but possible draw!
	ld a,2
	ld [wPongState],a		;signal that we won
.decide
	ld a,[wPongState]
	or a					;check for zero
	jr z,.continuePlay		;no winners? keep playing
	ld a,b	
	cp 2					;both lost? guess it's a draw
	jr nz,.prepExit
	inc a
	ld [wPongState],a		;WINNER should reflect the end of the game now, we can return after setting some flags
.prepExit
	ld a,2
	ld [wTransition],a		;we use TransSP2 for the next wTransition
	ld a,3
	ld [wScene],a			;and move to the RESULTS wScene
	ret						;that's all for pong!
	
.continuePlay

	;Do calculations, get input, etc.
	call GetJoypad
	ld [wJoypadSav0],a		;save it for later
	
	ld a,[wOnePlayer]
	cp 1
	jr z,.spCopy			;if we're in sp, just skip this without updating the opp paddle
	ld a,[wUpdateFlags]
	bit 7,a					;joypad update pending?
	jr z,.reqPad			;if not, its time to request
	call LinkRespond
	jr .sendPad
.reqPad
	ld a,$01
	ld [rSB],a
	call LinkRequest		;otherwise send the request
.sendPad
	ld a,[wJoypadSav0]
	ld [rSB],a
	call LinkSwap			;send our paddles
	ld [wJoypadSav1],a		;and save the opp pad for later
	jr .padTransFinish
.spCopy
	ld a,[wJoypadSav0]		;copy out paddle to the opponent's paddle in sp (so it's mirror mode)
	ld [wJoypadSav1],a
.padTransFinish
	
	call UpdatePaddle0		;now update the paddle
	call UpdatePaddle1		;now we can update the other paddle
	
	call UpdateBall			;and then the ball
	
	call UpdateTargets		;see if target tile updates are in order and schedule them if so
	
	call WaitVBlank
	;ld b,b					;breakpoint!!!!!!!
	
	call hDMARoutine
	
	ld a,[wTileCount]
	or a
	call nz,VBlankLoad		;now load the tiles that are pending (if any, otherwise skip this step -- more time for main loop next frame!)
	
	jp .loop
	
ShowResults::
	jp Start				;Just a reset placeholder
	ret
	
;This never gets called from here, just copied into HRAM
DMA::						;Address $FF80
	ld a,$C0				;$3EC0
	ldh [$FF46],a			;$EA46FF (?)
	ld a,$28				;$3E28
.wait						;Address $FF87
	dec a					;$3D
	jr nz,.wait				;$20FD (?)
	ret						;$C9
DMAEnd::

;***************************************************************
;* Subroutines
;***************************************************************

SECTION "Graphics Routines",ROM0

; Loads tiles from the TileQueue into VRAM. Designed to fit in a VBlank after OAM DMA.
; Note: Due to time limitations, the maximum number of tiles we can transfer right now is: ~55
; ACCESSES VRAM -- ONLY RUN DURING VBLANK. If you plan to load the entire screen at once, I recommend
; turning off the LCD and running LoadMap instead. This is intended for updates on-the-fly.
;;; REFACTOR -- If possible, getting the cycles/tile down would help a ton (every cycle counts here)
VBlankLoad::
	; first some overhead
	;ld b,b
	ld a,[wTileCount]		; (4)
	add a					; (1)
	add $47					; (2)
	ld h,$C2				; (2)
	ld l,a					; (1)
	ld [hl],0				; this whole thing puts a cap on the dst queue so we can exit (3)
	ld [wSaveSp],sp			; save sp (5)
	ld sp,wDstQueue			; and move it to the dst queue (3)
	ld hl,wTileQueue		; hl will just be our src holder (3)
.loadLoop		
	ldi a,[hl]				; grab the next tile data and increment (2)
	ld b,a					; save for later (1)
	pop de					; grab the next dst addr (3)
	ld a,d					; quick check for exit (1)
	or a					; if d == 0, we're done (1)
	jr z,.cleanup			; if we're done, leave (3/2)
	ld a,b					; grab the tile from earlier (1)
	ld [de],a				; and load the tile data there (2)
	jr .loadLoop			; do it again (3)
.cleanup
	ld sp,wSaveSp			; restore sp (3)
	pop hl					; (3)
	ld sp,hl				; (2)
	ld hl,wTileCount		; and reset the tile count (3)
	ld [hl],0				; (3)
	ret
	; total of 49 + 16*Tiles cycles (59 cycle overhead including the call/ret)
	; this limits us to 55 tiles updated per frame (not a lot)

	
; Adds the tile in a and the dest in hl to the tile queue for loading.
; @param b -- the tile index to add
; @param de -- the destination address in VRAM to put it
; appends to wTileQueue and wDstQueue
; modifies wTileCount (increment)
; @destroys a,hl
AddTile::
	;ld b,b
	ld a,[wTileCount]		; get the tilecount (4)
	ld h,$C2				; get hl with the address to load to -- this is always $C2XX (2)
	ld l,a					; add the offset to l and load it back -- since the start is $C200, just loading the offset will do (1)
	ld [hl],b				; and write the tile data there (2)
	add a					; multiply the offset by two (since DstQueue is twice the size) (1)
	add $46					; add this to the start of the DstQueue (this is $C246) (2)
	ld l,a					; and load it back -- now hl has the address to put LSB in (1)
	ld a,e					; (1)
	ldi [hl],a				; get LSB in DstQueue (2)
	ld [hl],d				; and then MSB (2)
	ld hl,wTileCount		; get tilecount (3)
	inc [hl]				; and increment it (3)
	ret
	; total of 24 cycles (34 including the call and ret)


SECTION "Support Routines",ROM0

;Updates this needs: If the ball is in the way of where the paddle is moving, move the ball one unit like the paddle and make the ball's y-speed maximum and in the correct direction
;also same for opp paddle
UpdatePaddle0::
	ld a,[wJoypadSav0]		;This gets the joypad data I saved before this call
	bit 3,a
	jr nz,.down
	bit 2,a
	jr nz,.up
	ret
.down
	ld hl,wPaddle0Y2
	ld a,[hl]
	cp $88
	ret z
	ld hl,wPaddle0Y0
	ld d,0
	ld e,4
	ld b,3
.dLoop
	ld a,[hl]
	inc a
	ld [hl],a
	add hl,de
	dec b
	jr nz,.dLoop
	ret
.up
	ld hl,wPaddle0Y0
	ld a,[hl]
	cp $20
	ret z
	ld d,0
	ld e,4
	ld b,3
.uLoop
	ld a,[hl]
	dec a
	ld [hl],a
	add hl,de
	dec b
	jr nz,.uLoop
	ret
	
UpdatePaddle1::
	ld a,[wJoypadSav1]		;This gets the joypad data from the other gb
	bit 3,a
	jr nz,.down
	bit 2,a
	jr nz,.up
	ret
.down
	ld hl,wPaddle1Y2
	ld a,[hl]
	cp $88
	ret z
	ld hl,wPaddle1Y0
	ld d,0
	ld e,4
	ld b,3
.dLoop
	ld a,[hl]
	inc a
	ld [hl],a
	add hl,de
	dec b
	jr nz,.dLoop
	ret
.up
	ld hl,wPaddle1Y0
	ld a,[hl]
	cp $20
	ret z
	ld d,0
	ld e,4
	ld b,3
.uLoop
	ld a,[hl]
	dec a
	ld [hl],a
	add hl,de
	dec b
	jr nz,.uLoop
	ret

;updateball needs some major overhauls to make good. Maybe a generic MoveProjectile function is in order?
UpdateBallX::
	ld hl,wBallX
	ld a,[hl]		;Get Ball x-pos
	cp $95			;If it's hit the right wall
	jp nz,.out		
	ld hl,wBallFlags	;load up the ball flags
	ld a,[hl]		
	or %10000000	;and set the first bit
	ld [hl],a		
	;also need to see if a target was struck and change hp accordingly
	;no collision for y values $80-$85 and $A8-$AD
	ld a,[wBallY]
	cp $40
	jr c,.hit0		;first target hits here
	cp $46
	jp c,.move		;this is the grace spot between tar0 and tar1
	cp $68
	jr c,.hit1		;second target is here
	cp $6E
	jr nc,.hit2		;if that wasn't a carry, then tar2 is the only option
	jp .move		
.hit0
	ld a,[wTargetState1]
	and %00001100
	cp %00001100	;is it already destroyed?
	jp z,.move		;if so, forget about it
	ld a,[wTargetState1]
	add %00000100	;otherwise increment and load back
	ld [wTargetState1],a	
	ld a,[wTargetFlags]
	set 5,a			;and signal an update
	ld [wTargetFlags],a
	jp .move
.hit1
	ld a,[wTargetState1]
	and %00110000
	cp %00110000	;is it already destroyed?
	jp z,.move		;if so, forget about it
	ld a,[wTargetState1]
	add %00010000	;otherwise increment and load back
	ld [wTargetState1],a	
	ld a,[wTargetFlags]
	set 6,a			;and signal an update
	ld [wTargetFlags],a
	jp .move
.hit2
	ld a,[wTargetState1]
	and %11000000
	cp %11000000	;is it already destroyed?
	jp z,.move		;if so, forget about it
	ld a,[wTargetState1]
	add %01000000	;otherwise increment and load back
	ld [wTargetState1],a	
	ld a,[wTargetFlags]
	set 7,a			;and signal an update
	ld [wTargetFlags],a
	jp .move
.out
	cp $18
	jr nz,.out1
	ld a,[wBallFlags]
	and %01111111	;reset the first bit
	ld [wBallFlags],a
	;also need to see if a target was struck and change hp accordingly
	;no collision for y values $80-$85 and $A8-$AD
	ld a,[wBallY]
	cp $40
	jr c,.thit0		;first target hits here
	cp $46
	jp c,.move		;this is the grace spot between tar0 and tar1
	cp $68
	jr c,.thit1		;second target is here
	cp $6E
	jr nc,.thit2	;if that wasn't a carry, then tar2 is the only option
	jp .move		
.thit0
	ld a,[wTargetState0]
	and %00001100
	cp %00001100	;is it already destroyed?
	jp z,.move		;if so, forget about it
	ld a,[wTargetState0]
	add %00000100	;otherwise increment and load back
	ld [wTargetState0],a	
	ld a,[wTargetFlags]
	set 1,a			;and signal an update
	ld [wTargetFlags],a
	jp .move
.thit1
	ld a,[wTargetState0]
	and %00110000
	cp %00110000	;is it already destroyed?
	jp z,.move		;if so, forget about it
	ld a,[wTargetState0]
	add %00010000	;otherwise increment and load back
	ld [wTargetState0],a	
	ld a,[wTargetFlags]
	set 2,a			;and signal an update
	ld [wTargetFlags],a
	jp .move
.thit2
	ld a,[wTargetState0]
	and %11000000
	cp %11000000	;is it already destroyed?
	jp z,.move		;if so, forget about it
	ld a,[wTargetState0]
	add %01000000	;otherwise increment and load back
	ld [wTargetState0],a	
	ld a,[wTargetFlags]
	set 3,a			;and signal an update
	ld [wTargetFlags],a
	jp .move
.out1				
	cp $24			;Otherwise, if it's hit $24 (the paddle)
	jr nz,.out2			
	ld hl,wPaddle0Y0	;load hl with the appropriate paddle register
	jr .calcPaddle		;and calculate
.out2
	cp $89			;or if it hit $89 (the opponent paddle)
	jp nz,.move
	ld hl,wPaddle1Y0		;load hl with the OPP paddle register
.calcPaddle	;Check for paddle collision
	ld b,-7
	ld a,[wBallY]
	add 3							
	sub [hl]				;This string of arithmetic loads into a the distance the ball is from the top right of the collision zone.
	jp c,.move				;If there was a carry after that subtraction (ie a is negative), then the ball doesn't collide and we can .move now
	;Subsequent subtractions are designed to cause carries if the ball is in that zone, meaning the c flag being set after a subtraction means the ball is there.
	sub 1					;mirror_up
	jp c,.mUp
	inc b					;-6 zone
	sub 1
	jr c,.updateSpeed
	inc b					;-5 zone
	sub 1
	jr c,.updateSpeed
	inc b					;-4 zone
	sub 2
	jr c,.updateSpeed
	inc b					;-3 zone
	sub 2
	jr c,.updateSpeed
	inc b					;-2 zone
	sub 2
	jr c,.updateSpeed
	inc b					;-1 zone
	sub 2
	jr c,.updateSpeed
	inc b					;0 zone
	sub 6
	jr c,.updateSpeed
	inc b					;+1 zone
	sub 2
	jr c,.updateSpeed
	inc b					;+2 zone
	sub 2
	jr c,.updateSpeed
	inc b					;+3 zone
	sub 2
	jr c,.updateSpeed
	inc b					;+4 zone
	sub 2
	jr c,.updateSpeed
	inc b					;+5 zone
	sub 1
	jr c,.updateSpeed
	inc b					;+6 zone
	sub 1
	jr c,.updateSpeed
	sub 1					;mirror_down
	jp nc,.move
.mDown
	ld a,[wBallSpeed]			;grab the speed
	add 6					;see if it's negative
	cp 6
	jp nc,.move				;if not, just keep the ball moving
	call NegateBallSpeed		;if so, then negate that speed
	jr .updateFlags		;and also bounce it off of the paddle
.mUp
	ld a,[wBallSpeed]			;read above, it's pretty much the same
	add 6
	cp 7					;cp 7 just ensures that a 0 jumps to .move
	jp c,.move
	call NegateBallSpeed
	jr .updateFlags
.updateSpeed
	ld a,[wBallSpeed]		;load the old speed
	add b				;add the speed modifier from the zone on the paddle the ball landed
	add 12				;adjust these possible speeds from the range -12 -- +12 to 0 -- +24
	cp 6				;See if this new value is below 6, or if the old value was below -6
	jr nc,.checkUpper
	ld a,6				;Set this low value to 6 (will become -6)
	jr .loadFlags
.checkUpper
	cp 19				;See if this new value is above 18 (18 and below will carry after a cp 19)
	jr c,.loadFlags
	ld a,18				;Set this high value to 18 (will become 6)
.loadFlags
	sub 12				;Transform the speed back down to the range -6 to 6
	ld [wBallSpeed],a		;And reload it into ball_s
.updateFlags
	ld a,[wBallSpeed]		;Get the speed 
	add 6				;make it positive 
	sla a
	sla a				;and multiply by 4 (each of the cases below is 4 bytes, so we need to space out the jumps by 4
	ld hl,.switch		
	ld c,a
	ld b,0
	add hl,bc
	jp hl				;now, jump forward by _a_ bytes
.switch
	ld b,%01000011		;Set b to the appropriate controller value	-6
	jr .setNewSpeed	;and jump out
	ld b,%01001010		;-5
	jr .setNewSpeed	
	ld b,%01000010		;-4
	jr .setNewSpeed	
	ld b,%01001001		;-3
	jr .setNewSpeed	
	ld b,%01000001		;-2
	jr .setNewSpeed	
	ld b,%01001000		;-1
	jr .setNewSpeed	
	ld b,%00000000		;0
	jr .setNewSpeed	
	ld b,%00001000		;+1
	jr .setNewSpeed	
	ld b,%00000001		;+2
	jr .setNewSpeed	
	ld b,%00001001		;+3
	jr .setNewSpeed	
	ld b,%00000010		;+4
	jr .setNewSpeed	
	ld b,%00001010		;+5
	jr .setNewSpeed	
	ld b,%00000011		;+6
	jr .setNewSpeed	
.setNewSpeed
	ld a,[wBallFlags]		;Now get the flags
	and %10110000		;and clear the relevant bits that are about to be updated
	bit 7,a				;still need to update the left/right flag though, which should invert
	jr nz,.makeZero
	set 7,a
	jr .finishUp
.makeZero
	res 7,a
.finishUp
	or b				;and then update them to whatever b is
	ld [wBallFlags],a		;Load this back into the register
.move
	ld hl,wBallFlags
	ld a,[hl]
	bit 7,a
	ld hl,wBallX
	ld a,[hl]
	jr nz,.left
	inc a
	ld [hl],a
	ret
.left
	dec a
	ld [hl],a
	ret
	
UpdateBallY::
	ld hl,wBallY
	ld a,[hl]		;Get Ball y-pos
	cp $8D			;If it's hit 140
	jr nz,.out	
	ld hl,wBallFlags	;load up the ball flags
	ld a,[hl]		
	or %01000000	;and set the second bit
	ld [hl],a	
	call NegateBallSpeed	;also negate the speed
	jr .move
.out
	cp $20			;Otherwise, if it's hit 20
	jr nz,.move
	ld hl,wBallFlags	;load up the ball flags
	ld a,[hl]		
	and %10111111	;and reset the second bit
	ld [hl],a	
	call NegateBallSpeed	;also negate the speed
.move
	ld hl,wBallFlags
	ld a,[hl]
	bit 6,a
	ld hl,wBallY
	ld a,[hl]
	jr nz,.down
	inc a
	jr .out2
.down
	dec a
.out2
	ld [hl],a
	ret

UpdateBall::
	call UpdateBallX ;Update the x pos first
	ld hl,wBallFlags
	ld a,[hl]
	and %00001111	;Get the last 4 bits of the ball flags (these show the ball's y speed)
	ld b,a			;Save this into b for now
	bit 3,b			;Test bit 3 of b (this shows whether the alternator is on or off)
	jr z,.ready		;If it's off, then just prepare the other flags and start updating the y value
	bit 2,b			;If it's on, we need to see whether it needs to increase or decrease. Bit 2 tells us this.
	jr z,.up		;If bit 2 is reset, we increment
	dec b			;If bit 2 is set, we decrement. Since the least significant bits of b are the ones we want to change, a simple decrement is all that is needed.
	res 2,b			;Now we reset bit 2 to indicate that next time, we will increment b. This is the alternation.
	jr .mask		;And prepare to mask
.up
	inc b			;If bit 2 is reset, we increment.
	set 2,b			;Then set bit 2 to indicate a decrement next time.
.mask
	ld a,[hl]		;hl still holds the address of wBallFlags, so we are loading the flags back into a right now
	and %11110000	;And we grab the first 4 bits; the ones that should stay the same.
	or b			;Or-ing b then places the updated last 4 bits into a, fully updating the flags.
	ld [hl],a		;We then reload these into the wBallFlags register.
.ready
	and %00000011	;Now we grab the last 2 bits. These are going to be used to count how many updates we need.
	ld b,a			;And store this number into b, since the UpdateBallY method needs to use a, but doesn't need b.
	inc b			;This is a little trick to reset the zero flag -- that's all that matters here. We need the flag to reflect the current value of b, 
	dec b			;not whatever garbage it held from the updates above.
.yLoop
	ret z			;If b is zero, we're done updating y.
	call UpdateBallY	;Otherwise, update the y value
	dec b			;then decrement
	jr .yLoop		;then loop back
	
;sees what the damage is on each target and loads the appropriate tiles into vram if needed
;note that target tiles start at $33 and are in groups of three: top, middle, bottom
;$33 is the start for our targets, $3F is the start for opp targets (so glad I can't just mirror them without making them sprites, DMG)
;as for locations in map, use the equates
;REFACTOR -- This badly needs optimization for speed AND to work with the new Vblank load system in place (it works, just could be faster probably)
UpdateTargets::
	ld a,[wTargetFlags]	;first load up the update flags
	bit 7,a
	jr z, .next0
	res 7,a
	ld [wTargetFlags],a	;reset the flag
	ld de,vTarget12		;get the address of the tiles
	ld a,[wTargetState1]		;get the hp now
	and %11000000		;grab the right part of the hp
	srl a
	srl a
	srl a
	srl a				;move it to the right spot
	srl a
	srl a
	ld b,a
	add b
	add b				;multiply by 3
	add OPP_TAR_TILE_START	;that's the offset we need for getting tiles
	ld b,a					;and put it in c
	jp .update
.next0
	bit 6,a
	jr z, .next1
	res 6,a
	ld [wTargetFlags],a	;reset the flag
	ld de,vTarget11
	ld a,[wTargetState1]		;get the hp now
	and %00110000		;grab the right part of the hp
	srl a
	srl a
	srl a
	srl a				;move it to the right spot
	ld b,a
	add b
	add b				;multiply by 3
	add OPP_TAR_TILE_START	;that's the offset we need for getting tiles
	ld b,a					;and put it in c
	jp .update
.next1
	bit 5,a
	jr z, .next2
	res 5,a
	ld [wTargetFlags],a	;reset the flag
	ld de,vTarget10
	ld a,[wTargetState1]		;get the hp now
	and %00001100		;grab the right part of the hp
	srl a
	srl a				;move it to the right spot
	ld b,a
	add b
	add b				;multiply by 3
	add OPP_TAR_TILE_START	;that's the offset we need for getting tiles
	ld b,a					;and put it in c
	jp .update
.next2
	bit 3,a
	jr z, .next3
	res 3,a
	ld [wTargetFlags],a	;reset the flag
	ld de,vTarget02
	ld a,[wTargetState0]		;get the hp now
	and %11000000		;grab the right part of the hp
	srl a
	srl a
	srl a
	srl a				;move it to the right spot
	srl a
	srl a
	ld b,a
	add b
	add b				;multiply by 3
	add TARGET_TILE_START	;that's the offset we need for getting tiles
	ld b,a					;and put it in c
	jr .update
.next3
	bit 2,a
	jr z, .next4
	res 2,a
	ld [wTargetFlags],a	;reset the flag
	ld de,vTarget01
	ld a,[wTargetState0]		;get the hp now
	and %00110000		;grab the right part of the hp
	srl a
	srl a
	srl a
	srl a				;move it to the right spot
	ld b,a
	add b
	add b				;multiply by 3
	add TARGET_TILE_START	;that's the offset we need for getting tiles
	ld b,a					;and put it in c
	jr .update
.next4
	bit 1,a
	ret z				;no more updates! let's return
	res 1,a
	ld [wTargetFlags],a	;reset the flag
	ld de,vTarget00
	ld a,[wTargetState0]		;get the hp now
	and %00001100		;grab the right part of the hp
	srl a
	srl a				;move it to the right spot
	ld b,a
	add b
	add b				;multiply by 3
	add TARGET_TILE_START	;that's the offset we need for getting tiles
	ld b,a					;and put it in c
.update
	ld c,3				;set the loop count
.loop
	call AddTile		;add the tile to the queue
	;add de, 32			;do this (but this instruction doesn't exist so we do this instead)
	ld a,e				;get the lower half of de
	add 32				;and add 32 to it
	ld e,a				;note: for some reason this is one cycle faster than adc so we use it
	jr nc,.check		;no carry? cool then we done with that
	inc d				;otherwise we need to inc d (the carry should spill over to it)
.check
	dec c
	ld a,c
	cp $FF
	jp z,UpdateTargets	;check it again lol
	cp 1
	jr z,.loop			;if b is one, we don't need to change the tile
	inc b				;otherwise move to the next tile
	jr .loop
	
NegateBallSpeed::		;replaces ball_s with its 2's complement
	ld a,[wBallSpeed]
	cpl
	inc a
	ld [wBallSpeed],a
	ret
	
CopyDMA::
	ld de,hDMARoutine
	ld hl,DMA
	ld c,DMAEnd - DMA
.loop
	ldi a,[hl]
	ld [de],a
	inc de
	dec c
	jr nz,.loop
	ret
	
;right now this redundantly checks for vblank twice -- once thru the vflag register and again thru LY's value. We can remove the LY check later when refactoring.
WaitVBlank::
	ei
	ld a,1
	ld [wVBlankFlag],a	;We're waiting for a vblank
.haltLoop
	halt
	nop
	ld a,[wVBlankFlag]
	cp 1			;Did one happen?
	jr z,.haltLoop
	di
	ldh	a,[rLY]		;get current scanline
	cp	$91			;Are we in v-blank yet?
	jr	nc,WaitVBlank	;if A-91 != 0 then loop
	ld a,[rDIV]			;grab a random number and throw it in the clock now too, why not
	ld [wVBlankCt],a
	ld a,0
	ld [wUpdateFlags],a	;also reset the pending data transfers now (maybe better in another spot? idk)
	ret				;done
	
WaitFrame::		;Not exactly a frame but whatever, precision isn't important here. It always returns at the start of a frame though
	call WaitVBlank	;first wait for v-blank
.loop
	ldh a,[rLY]			;then ret after we leave v-blank
	or a
	jr nz, .loop
	ret

LoadTiles::
	ld	hl,PongTiles
	ld	de,_VRAM
	ld	bc,76*16	;we have 76 tiles and each tile takes 16 bytes
.loop
	ld	a,[hl+]	;get a byte from our tiles, and increment.
	ld	[de],a	;put that byte in VRAM and
	inc	de		;increment.
	dec	bc		;bc=bc-1.
	ld	a,b		;if b or c != 0,
	or	c		;
	jr	nz,.loop	;then loop.
	ret			;done

;Note: make sure you load the map you want to load into hl before you call this. A simple ld hl,[map label] will do
;Most of the time you can just call GetNextMap immediately before this and you'll be fine
LoadMap::
	ld	de,_SCRN0	;where our map goes
	ld	bc,32*32
.loop
	ld	a,[hl+]	;get a byte of the map and inc hl
	ld	[de],a	;put the byte at de
	inc	de		
	dec	bc		;decrement our counter
	ld a,b
	or c
	jr	nz,.loop	;and if the counter != 0 then loop
	ret			;done
	
GetNextMap::		;loads hl with the appropriate address of the new map
	ld a,[wScene]
.connection
	cp 1	
	jr nz, .pong
	ld hl,ConnectMap
	ret
.pong
	cp 2
	jr nz,.mainMenu
	ld hl,PongMap
	ret
.mainMenu
	cp 4
	ret nz
	ld hl,MenuMap
	ret

;For quick reference: It's start,select,b,a,down,up,left,right, from bits 7-0 of a
GetJoypad::				;mutates a, b, hl
	ld hl,rP1
	ld a,%11010000			;grabs the buttons
	ld [hl],a
	ld a,[hl]				;accounts for pin bounce, hopefully
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]				;just use the last one
	and %00001111			;get the last 4, since that's what we want
	sla a					;move these over to the left nibble
	sla a
	sla a
	sla a
	ld b,a					;and pop that in b for now
	ld a,%11100000			;grabs the dpad
	ld [hl],a
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	ld a,[hl]
	and %00001111			;now the lower nibble is dpad
	or b					;grab that upper nibble from b again for the buttons
	cpl						;not a (P1 shows a pressed state with a 0 and a not-pressed with a 1, so flip that to make things easier)
	ret
	
	
ClearSpriteRam::
	ld hl,PaddleOne			;Load the address of sprite data in WRAM0
	ld b,40					;40 sprites in ram, so load 40
.loop
	ld a,$FF				;
	ld [hl+],a				;Put 255 as y-value for the sprite (no wPriority conflicts)
	ld a,0					;
	ld [hl+],a				;Put 0 in x-coord (y is already 255 so it shouldn't really matter)
	ld [hl+],a				;Put 0 in tile # (should be blank, but again, it isn't shown)
	ld [hl+],a				;Put 0 in attribute byte
	dec b					;That's one less sprite to nullify
	jr nz,.loop				;If we've not done it 40 times, do it again.
	ret						;
	
;	So this is my idea for communications:
;		-First prompt with a d/c screen until the gameboy receives a ping from the other gb or their ping goes through
;		-then show "connected" or something and fade out the graphics
;		-then IMMEDIATELY TURN OFF THE LCD AND TURN IT BACK ON AGAIN -- this should sync the v-blank period on both gbs (maybe)
;		-fade the graphics back in -- now you're in the lobby and connection should be synced.
;
;	From here we need to follow a strict communications and operations protocol:
;		-After a v-blank and DMA transfer, we have to get the player inputs (see below)
;		-Once all of that is done, we update the objects in their wPriority order.
;		-After the this, wait until v-blank and repeat.
;
;	To prevent desync, both player inputs are grabbed RIGHT AFTER V-BLANK and BEFORE UPDATING ANYTHING. This is done synchonously:
;		-Each gb loads the dpad and buttons states (accounting for pin bounce) and formats the data into one byte.
;		-One gb will be running faster than the other, most likely (it actually doesn't matter, but pretend one is faster). This gb is called tx, and the other one is rx.
;		-tx sends a ping thru SB to rx indicating that a transfer is ready.
;		-rx receives this ping, finishes setting up the byte to send, and then sends a response ping to tx indicating the transfer will commence.
;		-At this point, both SBs are loaded with the formatted player input byte and a transfer swaps them.
;		-Both gbs then use this input data (as well as their own input data) to handle any player input before updating other things.
;
;	This means we need a wPriority system in place for updating objects. Here's the simple version:
;		-First, update the player paddles (it shouldn't matter the order, since they never interact with each other)
;		-Then, spawn in any new projectiles on either end (again, order shouldn't matter here)
;		-After this, update each type of projectile according to a wPriority order (this is set based on what type of projectile we're looking at and is mostly arbitrary).
;		-Within each type, projectiles are updated in order of creation -- first spawned, first updated. This is easy to do since each one will likely be stored in ram in this order already.
;		-Since the ball's direction is influenced by pretty much every projectile, update it last.
;		-It's likely that each update will already check for the necessary collisions, so now it's just simple game logic (did someone win yet, did targets take damage, etc).
;
;	Communications need a code as well, for determining where in the process we are and what to do next after a ping:
;		-$00 is a null message meaning the other gb isn't doing anything yet -- MAKE SURE TO RESET SB TO 0 AFTER A TRANSFER SO THIS WORKS PROPERLY
;		-$10 is sent to request a connection at the beginning -- it should always be the first message sent.
;		-After receiving a $10, a gb should send a response $10 -- this means both gbs are connected.
;		-After a connection, both gbs should fade graphics and send a $11 once the display is off.
;		-Once both gbs have received a $11, immediately turn on the displays to sync v-blank. Graphics can then fade back in and both gbs can process for a bit.
;		-Not sure what data will be shared in the lobby yet, but we'll reserve $12-$1F for those sorts of requests. One of these will be syncing game start.
;		-Once in the game, we move to $01 for sending player inputs. $02-$0F are reserved for ingame use only, which include terminating the game.
;		-We then move to $2X for postgame/prelobby comms.
;		-Also, set SB to $FF if the gb isn't ready to initiate comms, just to let the other gb know that comms are a no-go right now.
;		-Make sure SB is shadowed somewhere so you can crossreference the message received with the message you sent.
;
;	Here is the exact communication protocol a gameboy should operate under:
;		For tx:
;		-Start by loading a message byte into SB and then calling PING.
;		-PING should initiate a transfer of data and wait for the interrupt to go through.
;		-The interrupt will send us to the Serial Interrupt Vector. This then calls HandlePing.
;		-HandlePing usually won't do anything at this point for tx, but it still could. See below.
;		For rx:
;		-rx will be doing its own thing when suddenly it gets thrown to the Serial Interrupt Vector. This calls HandlePing.
;		-HandlePing reads the message byte and calls the appropriate handler function depending on the message. If it's $00, it does nothing.
;		-Before calling but after determining what to call, HandlePing sets SB to $00 just in case another transfer is initiated, although this shouldn't happen.
;
;	The harder part is determining when the gbs are connected and testing that connection. Here's how to do that:
;		-Once multiplayer is selected in the menu and the d/c screen pops up, set SB to $10 and call CONNECT. DO NOT CALL PING.
;		-CONNECT works differently in that it doesn't expect to get a response. It starts by initiating a transfer.
;		-Then, it waits for a short period of time. If no interrupt comes during that time, it aborts the connection and returns.
;		-If CONNECT didn't work, check for user input to exit multiplayer, then wait a short while. Call CONNECT again until the lobby is left or it returns positive.
;
;	Additionally, a connection could be terminated prematurely, either by battery dying, the cable getting wiggled, or other malicious interference.
;	We need to be on alert for these sorts of timeouts. Maybe this means we make PING handle timeouts like CONNECT does? idk.
;	EDIT: Easy fix for that is to d/c if the v-blank interrupt ever goes off while waiting for a ping. We've lost a frame at that point anyway, which is unacceptable.
;	ALTHOUGH, realize we are playing with netcode most likely -- lost packets are pretty possible over the internet. Guess we're also interpolating :(
;	On real hardware, connection shouldn't be an issue, but on emulator we have to roll with it
;	Since both GBs update their own state, the real issue is desyncs. If the player paddle can't be sent for a frame, this will likely cause both states to differ.
;	Inconsistencies only add up over time, so the states will have to be synced somehow. Again, this only has to be done if we LOSE a frame (meaning paddle state
;	isn't received before vblank), otherwise we're fine. Also, this SHOULD only be a problem on emu.
;	It's possible that BGB handles this, actually -- maybe it pauses if it doesn't get a response on the line? -- but we don't really know yet. We can fix it
;	when the time comes if necessary.
;	Either way, I'd say a 1-5 second timeout on comms should indicate the connection is over, regardless of hardware or emu. We can just do 255 VBlanks if we want, that's simple.
;
;	(Note: That last paragraph hasn't been implemented yet, just an idea)
;
;	Also, here's a quick explanation of the link system in place right now:
;	First, realize that the other GB may have already requested data -- check UP_FLAGS to see if that's the case.
;	If it is, call LinkRespond. This syncs both GBs for transfer. 
;	If not, there's an appropriate "ping code" that indicates to the other GB that you want to swap those values. Load this into SB and then call LinkRequest.
;	By the time either LinkRequest or LinkRespond return, both GBs will be synced and ready to transfer. Load the data you want to send into SB and call LinkSwap.
;	Once that returns, the other GB's data will be waiting for you in SB! yay! also disable interrupts at this point if you want.
;	NOTE: Needs to be tested to see if it actually works
	
;	ALSO SINCE IT ISN'T IN THE PAN DOCS -- If a GB is in internal clock mode, it REFUSES to ping another GB in internal clock mode
;	the other GB MUST INDICATE IT IS USING EXTERNAL CLOCK or you will get $FF back instead
;	However, if a transfer is already in progress and the other GB switches to internal clock mode, nothing will happen (the transfer will finish normally)
;	KEEP THIS IN MIND!!!!

;sends a ping byte to request an upcoming transfer and waits for a signal from the other GB before returning. 
;--be sure to load the ping request into SB before calling--
LinkRequest::		
	;first swap ping bytes and wait for interrupt
	ld a,[rSB]
	ld [wLastComm],a	;store the last ping for reference
	ld a,$81
	ld [rSC],a	;request a transfer
	ei
	nop			;ei doesn't take effect for one instruction
.listen
	halt	;and chill
	nop
	ld a,[wSerialFlag]	;did the right interrupt go through?
	cp 0
	jr z,.listen		;if not, we wait again
	;when the interrupt handler sees a ping byte, it should wait until that information is ready and then send a response back
	;if we get back a $00, we wait for another interrupt signaling the other gb is ready for transfer
	;after that, we can return -- the next byte expeceted by both gbs is the requested byte by the ping
	ld a,$80
	ld [rSC],a			;we are waiting for a signal now, not sending one
.ready
	ld a,[wSerialFlag]
	cp 2				;is the other GB ready?
	ret z				;if we're in data mode, then we are ready -- return!
	halt				;let's wait some more then
	nop				;we can jump back after a wake-up
	jr .ready
	
;signals to the other GB that we are finally ready to respond. The actual ping byte we send doesn't matter (it gets thrown out by
;the handler if it's in wait mode anyways) so send whatever a is right then.
LinkRespond::
	ld [rSB],a
	ld a,$81
	ld [rSC],a
	ei
	nop
.wait
	halt
	nop
	ld a,[wSerialFlag]		;we get the interrupt yet?
	cp 0
	ret z
	jr .wait
	
;initiates a safe transfer of SB and returns once complete. Assumed to be run *roughly* simultaneously on the other GB.
;this should be run after a LinkRequest returns and the appropriate value has been loaded into SB.
;Big concern here is if we accidentally double swap -- not sure if that will be a problem yet but it very well could be
;The final data is stored in a, not SB -- SB gets cleared back to $00 so you don't have to :)
LinkSwap::			
	;send a swap request
	ld a,[rSB]
	ld [wLastComm],a	;store the last ping for reference -- shouldn't affect anything in the handler since we SHOULD be in data mode anyways
	nop
	nop
	nop
	nop
	nop
	nop					;This is just for security's sake to make sure SB is properly loaded on both GBs before the transfer (and that COMM_BYTE is proper as well)
	nop
	nop
	nop
	nop
	ld a,$81
	ld [rSC],a			;swap it baby
	ei					;This MIGHT double swap, but I don't think it will -- it takes some time for the bits to swap in and out of SB, so if they're off by a couple cycles it shouldn't matter
	nop					;NOTE: It looks like it takes quite a bit of time honestly, so don't worry about it?
.listen
	halt				;chilll
	nop
	ld a,[wSerialFlag]
	cp 0
	jr nz,.listen		;we back in control mode? if so, it went great. Otherwise, go back
	di
	ld a,[rSB]			;grab the comm and save it in b for now
	ld b,a
	xor a
	ld [rSB],a			;then clear the comms
	ld [wLastComm],a
	ld a,b				;and grab the data again
	ret					;guess we're done then?
	
ConnectToGameBoy::
	ld a,[wOnePlayer]
	cp 1
	jr z,.connectFinish	;if we're in sp, just skip this wScene
	ld a,$10		;Load the initial ping byte into SB
	ld [rSB],a
	ld [wLastComm],a
	ld a,$81		;And request a transfer
	ld [rSC],a
	ei				;def enable these here
	call WaitVBlank	;now wait
	ld a,[wIsConnected]
	and a
	jp z,ConnectToGameBoy	;if a=0, we didn't get the interrupt -- try again!
	di
	;otherwise we're good and connected!
	;for now we'll just load into pong from here
	xor a
	ld [rSB],a
	ld [wLastComm],a		;reset comms
.connectFinish
	ld a,2						;this isn't the lobby yet -- right now it jumps right to the game
	ld [wScene],a
	ld a,1
	ld [wTransition],a			;we use a fancy transition into the lobby
	ret
	
HandleConnect::
	ld a,$00
	ld [rSB],a	;reset SB
	ld [wLastComm],a
	ld a,1		;also put a 1 in CONNECTED to show the connection was successful
	ld [wIsConnected],a
	ret			
	
; this is the serial interrupt handler -- every serial interrupt starts here
; note -- I honestly forgot how this works and even the documentation only kinda helps. (in fact it probably doesn't really work.) read at your own risk
HandlePing::
	ld a,[wLastComm]
	cp $FF						;Were we even ready to communicate?
	jr nz,.modeCheck1			;If not, ignore the comm
	ld [rSB],a					;also we are still not ready for comms then
	ret
.modeCheck1
	ld a,[wSerialFlag]
	cp 2			;Are we in data mode? if so, this byte has no meaning and we can return after updating some stuff
	jr nz,.modeCheck
	xor a
	ld [wSerialFlag],a	;put us back in control mode
	ret				;and leave
.modeCheck
	cp 1			;What about wait mode? if so, the fact we got a ping at all means we're good to enter data mode
	jr nz,.control
	inc a
	ld [wSerialFlag],a	;switch to data mode
	ld [wLastComm],a	;also throw a nonzero value in COMM_BYTE so LinkRequest knows we're good
	ret
.control
	ld b,1				; time for some shenanigans -- we load 1 (which means wait mode) into b, as that's what we default into after a control byte
	ld a,[rSB]
	ld hl,wLastComm	;is SB the same as what we sent?
	cp [hl]
	jr nz,.checkStuff	;if not, don't worry about it
	ld b,2				; if so, we can override that load 1 into a load 2, meaning we go straight to data mode. After all, the other gb is ready, huh?
.checkStuff
	cp $00	
	jr nz,.next
	ld a,[wLastComm]
	cp $10
	ret z				;if we sent a $10, then this whole transfer was pointless, just return
	ld a,1				;otherwise we sent a control byte ($FF would've jumped out by now and there's no way we sent a $00 if we just got one and weren't connected) so enter wait mode
	ld [wSerialFlag],a	;put us in wait mode then
	ld a,[wLastComm]
	ld [rSB],a			;also prep us with the ping byte again just in case the other GB sends a request so it goes thru
	ret
.next
	cp $01
	jr nz,.next0
	ld a,[wUpdateFlags]
	set 7,a
	ld [wUpdateFlags],a
	ld a,b
	ld [wSerialFlag],a
	ret
.next0
	cp $10
	jr nz,.next1
	ld a,[wIsConnected]
	cp 1					;were we already connected?
	jp nz,HandleConnect	;if not, we're on the line! yay
	xor a
	ld [rSB],a
	jp .checkStuff			;if so, pretend we just got a $00 and recheck everything (super hacky but whatever)
.next1
	cp $11
	jr nz,.next2
	ld a,[wUpdateFlags]
	set 6,a				;The other GB is waiting to sync for LCD turnon now
	ld [wUpdateFlags],a
	ld a,b
	ld [wSerialFlag],a	;set the mode to whatever we determined earlier
	ret
.next2
	cp $12
	jr nz,.next3
	ld a,[wUpdateFlags]
	set 5,a				;signal that the other GB is waiting for our clock!
	ld [wUpdateFlags],a
	ld a,b
	ld [wSerialFlag],a	;set the mode to whatever we determined earlier
	ret
.next3
	cp $FF				;They weren't ready, so ignore it
	ret z
	ret
	
HandleVBlank::
	ld a,0				;yep, a vblank happened
	ld [wVBlankFlag],a
	ret					;and return


; There are descriptions for each of these addresses in the comments at the top. These used to be hard-coded addresses before I refactored, but I never moved the documentation
SECTION "Sprite-Data", WRAM0[$C000]

UNION

PaddleOne::
wPaddle0Y0::
DS 4
PaddleTwo::
wPaddle0Y1::
DS 4
PaddleThree::
wPaddle0Y2::
DS 4
Ball::
wBallY::
DS 1
wBallX::
DS 3
OppOne::
wPaddle1Y0::
DS 4
OppTwo::
wPaddle1Y1::
DS 4
OppThree::
wPaddle1Y2::
DS 4
Others::
DS 132

NEXTU

wCursorOne::
DS 4
wCursorTwo::
DS 4

ENDU

; Again, documentation is at the top of the file
SECTION "General-Variables", WRAM0

wBallFlags::
DS 1
wBallSpeed::
DS 1
wLastComm::
DS 1
wScene::
DS 1
wIsConnected::
DS 1
wPriority::
DS 1
wVBlankCt::
DS 1
wVBlankFlag::
DS 1
wTransition::
DS 1
wUpdateFlags::
DS 1
wSerialFlag::
DS 1
wJoypadSav0::
DS 1
wJoypadSav1::
DS 1
wOnePlayer::
DS 1
wTargetState0::
DS 1
wTargetState1::
DS 1
wTargetFlags::
DS 1
wPongState::
DS 1
wTileCount::
DS 1
wSaveSp::
DS 2
wOptionNo::
DS 1
wInputClock::
DS 1
	
	
; Some magic numbers assume this address starts at $C200
SECTION "Tile-Update-Queue", WRAM0[$C200]

wTileQueue::
DS 70
wDstQueue::
DS 140
	
	
SECTION "DMA-Routine", HRAM[$FF80]

hDMARoutine::
DS 10
	

; TILE DATA AND TILEMAPS
;********************************************************************
; This section was generated by GBTD v2.2

 SECTION "Tiles", ROM0

; Start of tile array.
PongTiles::
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $E0,$E0,$E0,$E0,$E0,$E0,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0
DB $F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0
DB $55,$00,$AA,$00,$55,$00,$AA,$00
DB $55,$00,$AA,$00,$55,$00,$AA,$00
DB $00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$00,$00,$00,$FF,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$00,$00
DB $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E
DB $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E
DB $01,$01,$03,$03,$07,$07,$0F,$0F
DB $0F,$0F,$1F,$1F,$1F,$1F,$1F,$1F
DB $00,$00,$00,$00,$00,$00,$07,$07
DB $1F,$1F,$3F,$3F,$7F,$7F,$FF,$FF
DB $FE,$FE,$F8,$F8,$F0,$F0,$E0,$E0
DB $C0,$C0,$80,$80,$80,$80,$00,$00
DB $00,$00,$FF,$FF,$FF,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$00,$00,$00,$00
DB $7C,$7C,$7C,$7C,$7C,$7C,$7C,$7C
DB $7C,$7C,$7C,$7C,$7C,$7C,$7C,$7C
DB $80,$80,$C0,$C0,$E0,$E0,$F0,$F0
DB $F0,$F0,$F8,$F8,$F8,$F8,$F8,$F8
DB $1F,$1F,$1F,$1F,$1F,$1F,$0F,$0F
DB $0F,$0F,$07,$07,$03,$03,$01,$01
DB $F8,$F8,$F8,$F8,$F8,$F8,$F0,$F0
DB $F0,$F0,$E0,$E0,$C0,$C0,$80,$80
DB $00,$00,$00,$00,$00,$00,$E0,$E0
DB $F8,$F8,$FC,$FC,$FE,$FE,$FF,$FF
DB $FF,$FF,$7F,$7F,$3F,$3F,$1F,$1F
DB $07,$07,$00,$00,$00,$00,$00,$00
DB $FF,$FF,$FE,$FE,$FC,$FC,$F8,$F8
DB $E0,$E0,$00,$00,$00,$00,$00,$00
DB $7F,$7F,$1F,$1F,$0F,$0F,$07,$07
DB $03,$03,$01,$01,$01,$01,$00,$00
DB $00,$00,$80,$80,$80,$80,$C0,$C0
DB $E0,$E0,$F0,$F0,$F8,$F8,$FE,$FE
DB $00,$00,$01,$01,$01,$01,$03,$03
DB $07,$07,$0F,$0F,$1F,$1F,$7F,$7F
DB $7C,$7C,$C6,$C6,$C6,$C6,$FE,$FE
DB $C6,$C6,$C6,$C6,$C6,$C6,$00,$00
DB $FC,$FC,$C6,$C6,$C6,$C6,$FC,$FC
DB $C6,$C6,$C6,$C6,$FC,$FC,$00,$00
DB $7C,$7C,$C6,$C6,$C6,$C6,$C0,$C0
DB $C6,$C6,$C6,$C6,$7C,$7C,$00,$00
DB $F8,$F8,$CC,$CC,$C6,$C6,$C6,$C6
DB $C6,$C6,$CC,$CC,$F8,$F8,$00,$00
DB $FE,$FE,$C0,$C0,$C0,$C0,$FC,$FC
DB $C0,$C0,$C0,$C0,$FE,$FE,$00,$00
DB $FE,$FE,$C0,$C0,$C0,$C0,$FC,$FC
DB $C0,$C0,$C0,$C0,$C0,$C0,$00,$00
DB $7C,$7C,$C6,$C6,$C6,$C6,$C0,$C0
DB $CE,$CE,$C6,$C6,$7C,$7C,$00,$00
DB $C6,$C6,$C6,$C6,$C6,$C6,$FE,$FE
DB $C6,$C6,$C6,$C6,$C6,$C6,$00,$00
DB $7E,$7E,$18,$18,$18,$18,$18,$18
DB $18,$18,$18,$18,$7E,$7E,$00,$00
DB $7E,$7E,$0C,$0C,$0C,$0C,$0C,$0C
DB $CC,$CC,$CC,$CC,$78,$78,$00,$00
DB $CE,$CE,$D8,$D8,$F0,$F0,$E0,$E0
DB $F0,$F0,$D8,$D8,$CE,$CE,$00,$00
DB $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0
DB $C0,$C0,$C6,$C6,$FE,$FE,$00,$00
DB $C6,$C6,$EE,$EE,$FE,$FE,$D6,$D6
DB $C6,$C6,$C6,$C6,$C6,$C6,$00,$00
DB $C6,$C6,$E6,$E6,$F6,$F6,$DE,$DE
DB $CE,$CE,$C6,$C6,$C6,$C6,$00,$00
DB $7C,$7C,$C6,$C6,$C6,$C6,$C6,$C6
DB $C6,$C6,$C6,$C6,$7C,$7C,$00,$00
DB $FC,$FC,$C6,$C6,$C6,$C6,$C6,$C6
DB $FC,$FC,$C0,$C0,$C0,$C0,$00,$00
DB $7C,$7C,$C4,$C4,$C6,$C6,$C6,$C6
DB $C6,$C6,$CC,$CC,$7E,$7E,$00,$00
DB $FC,$FC,$C6,$C6,$C6,$C6,$FC,$FC
DB $F0,$F0,$D8,$D8,$CE,$CE,$00,$00
DB $FE,$FE,$C0,$C0,$C0,$C0,$FE,$FE
DB $06,$06,$06,$06,$FE,$FE,$00,$00
DB $FE,$FE,$30,$30,$30,$30,$30,$30
DB $30,$30,$30,$30,$30,$30,$00,$00
DB $C6,$C6,$C6,$C6,$C6,$C6,$C6,$C6
DB $C6,$C6,$C6,$C6,$7C,$7C,$00,$00
DB $C6,$C6,$C6,$C6,$C6,$C6,$C6,$C6
DB $C6,$C6,$6C,$6C,$38,$38,$00,$00
DB $C6,$C6,$C6,$C6,$C6,$C6,$D6,$D6
DB $FE,$FE,$EE,$EE,$C6,$C6,$00,$00
DB $C6,$C6,$C6,$C6,$C6,$C6,$7C,$7C
DB $C6,$C6,$C6,$C6,$C6,$C6,$00,$00
DB $C6,$C6,$C6,$C6,$C6,$C6,$7C,$7C
DB $18,$18,$18,$18,$18,$18,$00,$00
DB $FE,$FE,$0C,$0C,$18,$18,$30,$30
DB $60,$60,$C0,$C0,$FE,$FE,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$60,$60,$60,$60,$00,$00
DB $60,$60,$60,$60,$60,$60,$60,$60
DB $00,$00,$60,$60,$60,$60,$00,$00
DB $F0,$F0,$10,$10,$10,$10,$70,$70
DB $00,$00,$60,$60,$60,$60,$00,$00
DB $06,$06,$0E,$0E,$1C,$1C,$38,$38
DB $70,$70,$E0,$E0,$C0,$C0,$00,$00
DB $00,$00,$00,$00,$3C,$3C,$7E,$7E
DB $3C,$3C,$00,$00,$00,$00,$00,$00
DB $07,$FF,$09,$F9,$09,$F9,$0B,$F9
DB $09,$F9,$09,$F9,$09,$F9,$09,$FB
DB $09,$F9,$09,$F9,$0B,$FB,$09,$F9
DB $09,$F9,$0B,$FB,$09,$F9,$09,$F9
DB $09,$FB,$09,$F9,$09,$F9,$09,$F9
DB $0B,$F9,$09,$F9,$09,$F9,$07,$FF
DB $07,$FF,$0F,$F9,$0F,$F9,$0F,$F9
DB $0F,$F9,$0F,$F9,$0F,$F9,$0D,$FB
DB $0F,$F9,$0F,$F9,$0F,$FB,$0F,$F9
DB $0F,$F9,$0F,$FB,$0F,$F9,$0F,$F9
DB $0D,$FB,$0F,$F9,$0F,$F9,$0F,$F9
DB $0F,$F9,$0F,$F9,$0F,$F9,$07,$FF
DB $07,$FF,$09,$FF,$09,$FF,$09,$FF
DB $09,$FF,$09,$FF,$09,$FF,$09,$FF
DB $09,$FF,$09,$FF,$0B,$FF,$09,$FF
DB $09,$FF,$0B,$FF,$09,$FF,$09,$FF
DB $09,$FF,$09,$FF,$09,$FF,$09,$FF
DB $09,$FF,$09,$FF,$09,$FF,$07,$FF
DB $07,$FF,$0F,$FF,$0F,$FF,$0F,$FF
DB $0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF
DB $0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF
DB $0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF
DB $0F,$FF,$0F,$FF,$0F,$FF,$0F,$FF
DB $0F,$FF,$0F,$FF,$0F,$FF,$07,$FF
DB $E0,$FF,$90,$9F,$90,$9F,$D0,$9F
DB $90,$9F,$90,$9F,$90,$9F,$90,$DF
DB $90,$9F,$90,$9F,$D0,$DF,$90,$9F
DB $90,$9F,$D0,$DF,$90,$9F,$90,$9F
DB $90,$DF,$90,$9F,$90,$9F,$90,$9F
DB $D0,$9F,$90,$9F,$90,$9F,$E0,$FF
DB $E0,$FF,$F0,$9F,$F0,$9F,$F0,$9F
DB $F0,$9F,$F0,$9F,$F0,$9F,$B0,$DF
DB $F0,$9F,$F0,$9F,$F0,$DF,$F0,$9F
DB $F0,$9F,$F0,$DF,$F0,$9F,$F0,$9F
DB $B0,$DF,$F0,$9F,$F0,$9F,$F0,$9F
DB $F0,$9F,$F0,$9F,$F0,$9F,$E0,$FF
DB $E0,$FF,$90,$FF,$90,$FF,$90,$FF
DB $90,$FF,$90,$FF,$90,$FF,$90,$FF
DB $90,$FF,$90,$FF,$D0,$FF,$90,$FF
DB $90,$FF,$D0,$FF,$90,$FF,$90,$FF
DB $90,$FF,$90,$FF,$90,$FF,$90,$FF
DB $90,$FF,$90,$FF,$90,$FF,$E0,$FF
DB $E0,$FF,$F0,$FF,$F0,$FF,$F0,$FF
DB $F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF
DB $F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF
DB $F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF
DB $F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF
DB $F0,$FF,$F0,$FF,$F0,$FF,$E0,$FF


;************************************************************
;* tile map

SECTION "PongMap", ROM0

PongMap::
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$34,$00,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$00,$40,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$35,$00,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$00,$41,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$35
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$00,$41,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$36,$00,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$00,$42,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$00,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$00
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$34,$00,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$00,$40,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$35,$00,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$00,$41,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$35
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$00,$41,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$36,$00,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$00,$42,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$00,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$00
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$34,$00,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$00,$40,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$35,$00,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$00,$41,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$35
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$00,$41,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$36,$00,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$00,$42,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04

SECTION "ConnectMap", ROM0

ConnectMap::
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$00,$00,$08,$05,$05,$05,$05
DB $05,$05,$05,$05,$05,$05,$05,$05,$0F,$00
DB $00,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$00,$07,$09,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $12,$0C,$00,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$00,$06,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$0B,$00,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$00
DB $06,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$0B,$00,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$00,$06,$00,$00,$00,$00,$00,$33,$23
DB $1F,$33,$00,$00,$00,$00,$00,$0B,$00,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$00,$06,$00,$00,$00,$00,$00
DB $18,$32,$17,$30,$00,$00,$00,$00,$00,$0B
DB $00,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$00,$06,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$0B,$00,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$00,$06,$00
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$00,$00,$0B,$00,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$00
DB $06,$00,$00,$02,$01,$03,$03,$03,$03,$03
DB $03,$03,$02,$00,$00,$0B,$00,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$00,$06,$00,$00,$02,$03,$03,$03,$03
DB $03,$03,$03,$03,$02,$00,$00,$0B,$00,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$00,$06,$00,$00,$02,$03,$03
DB $03,$03,$03,$03,$03,$03,$02,$00,$00,$0B
DB $00,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$00,$06,$00,$00,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$00
DB $00,$0B,$00,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$00,$06,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$0B,$00,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$00
DB $06,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$0B,$00,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$00,$0D,$13,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$14,$0E,$00,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$00,$00,$10,$0A,$0A,$0A,$0A
DB $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$11,$00
DB $00,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03

SECTION "TitleMap", ROM0

TitleMap::
DB $04,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$00,$00,$00,$00,$33
DB $00,$28,$1D,$28,$20,$19,$31,$00,$33,$00
DB $00,$00,$00,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03

SECTION "MenuMap", ROM0

MenuMap::
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$04,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$00,$00,$00,$00,$00,$00
DB $27,$23,$20,$23,$00,$00,$00,$00,$00,$00
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$00,$00
DB $00,$00,$00,$00,$18,$29,$19,$20,$00,$00
DB $00,$00,$00,$00,$04,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$00,$00,$00,$00,$00,$23,$24,$28
DB $1D,$23,$22,$00,$00,$00,$00,$00,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$04,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $04,$04,$04,$04,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$04,$04,$04,$04,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$04,$04,$04,$04,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$04,$04,$04,$04
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$03,$03
DB $03,$03,$03,$03,$03,$03,$03,$03,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
DB $04,$04,$04,$04

;*** End Of File ***