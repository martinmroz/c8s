; ---- minimal chip8 game ---

; V0-2  scrap
; V3  player x
; V4  player y
; V5  enemy x
; V6  enemy y
; VF    collision detection

; keys:
; #2 = up
; #4 = left
; #5 = space
; #6 = right
; #8 = down
; #A = Ctrl
; #B = shift
; #C = Z
; #F = Space

; HIGH   ; Uncomment this for Hires Superchip mode (128x64)

.org $200

GAMEINIT:

  cls             ; clear screen

  call INITPLAYER ; init player 
  call INITENEMY  ; init enemy 

  call DRAWPLAYER ; draw player on startpos
  call DRAWENEMY  ; draw enemy on startpos

; ================== game loop ================
GAMELOOP: 
  
  call DRAWPLAYER   ; draw xor player (clear last pos)
  call HANDLEINPUT  ; move player 
  call DRAWPLAYER   ; draw player on new pos

  call DRAWENEMY
  call MOVEENEMY
  call DRAWENEMY  

  ; check collision, init player upon collision
  sne vf, 1
  jp GAMEINIT

  ld v0,1
  ld dt,v0
  call WAITDELAY

  jp GAMELOOP

; ================== functions ================
INITPLAYER:

  ld v3,32
  ld v4,25
  ret

; drawplayer  (uses: v3,v4,I)
DRAWPLAYER:
  ld i,SPR_DUDE    ;Set I to SPR_DUDE adress
  drw v3,v4,6
  ret

; handle input (uses v0)
HANDLEINPUT:

  ; check down
  ld v0,8       ;Set V0 to Key
  skp v0        ;Check Key (skip next instruction if pressed)
  jp checkup    ;Skip
  add v4,1      ;y++

  checkup:
  ld v0,2       ;Set V0 to Key
  skp v0        ;Check Key
  jp checkleft  ;skip to next check
  add v4,$FF    ;y--

  checkleft:
  ld v0,4       ;Set V0 to Key
  skp v0        ;Check Key
  jp checkright ;skip to next check
  add v3,$FF    ;x--

  checkright:
  ld v0,6       ;Set V0 to Key
  skp v0        ;Check Key
  jp checkdone  ;skip to next check
  add v3,1      ;x++

  checkdone:
  ret

INITENEMY:
  ld v5,0
  rnd v6,15
  ret

DRAWENEMY:
  ld i,SPR_ENEMY     ;Set I to SPR_ENEMY adress
  drw v5,v6,5
  ret

MOVEENEMY:
  add v5,1

  ; compare V1>60
  ld v0,v5  
  ld v1,60
  sub v0,v1
  sne vf,1 
  call INITENEMY
  ret

WAITDELAY:
  ld v0,dt     ;Set V0 to DelayTimer
  se v0,0       ;Check zero
  jp WAITDELAY     
  ret

; ================== graphics ================
SPR_DUDE:
  .db $3c ;   ++++
  .db $18 ;    ++
  .db $ff ; ++++++++
  .db $18 ;    ++
  .db $24 ;   +  +
  .db $e7 ; +++  +++

SPR_ENEMY:
  .db $7e ;  ++++++
  .db $ff ; ++++++++
  .db $99 ; +  ++  +
  .db $e7 ; +++  +++
  .db $3c ;   ++++
