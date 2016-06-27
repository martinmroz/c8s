
.org $000
start_of_memory:
.org $100
in_reserved_area:
.org $200

  NOP                     ; 0000
  CLS                     ; 00E0
  RET                     ; 00EE
  TRAPRET                 ; 00F9
  TRAP                    ; 00FA

; SYS $address            ; 0nnn
  
  JP $000                 ; 1000
  JP start_of_memory      ; 1000
  JP #100                 ; 1100
  JP in_reserved_area     ; 1100
  JP 4095                 ; 1FFF

  CALL $000               ; 2000
  CALL start_of_memory    ; 2000
  CALL #100               ; 2100
  CALL in_reserved_area   ; 2100
  CALL 4095               ; 2FFF

  SE V0, 4                ; 3004
  SE V7, #F               ; 370F
  SE VF, 255              ; 3FFF

  SNE V0, 4               ; 4004
  SNE V7, #F              ; 470F
  SNE VF, $FF             ; 4FFF

  SE V0, V7               ; 5070
  SE V7, VF               ; 57F0
  SE VF, VF               ; 5FF0

  LD V0, 0                ; 6000
  LD V1, $7               ; 6107
  LD VF, #FF              ; 6FFF

  ADD V0, 0               ; 7000
  ADD V1, $7              ; 7107
  ADD VF, #FF             ; 7FFF

  LD V0, V0               ; 8000
  LD V1, V7               ; 8170
  LD VF, VF               ; 8FF0

  OR V0, V0               ; 8001
  OR V1, V7               ; 8171
  OR VF, VF               ; 8FF1

  AND V0, V0              ; 8002
  AND V1, V7              ; 8172
  AND VF, VF              ; 8FF2

  XOR V0, V0              ; 8003
  XOR V1, V7              ; 8173
  XOR VF, VF              ; 8FF3

  ADD V0, V0              ; 8004
  ADD V1, V7              ; 8174
  ADD VF, VF              ; 8FF4

  SUB V0, V0              ; 8005
  SUB V1, V7              ; 8175
  SUB VF, VF              ; 8FF5

  SHR V0                  ; 8006
  SHR V1                  ; 8106
  SHR VF                  ; 8F06

  SUBN V0, V0             ; 8007
  SUBN V1, V7             ; 8177
  SUBN VF, VF             ; 8FF7

  SHL V0                  ; 800E
  SHL V1                  ; 810E
  SHL VF                  ; 8F0E

  SNE V0, V7              ; 9070
  SNE V7, VF              ; 97F0
  SNE VF, VF              ; 9FF0

  LD I, 0                 ; A000
  LD I, #FF               ; A0FF
  LD I, $567              ; A567
  LD I, start_of_memory   ; A000
  LD I, in_reserved_area  ; A100

  JP V0, 0                ; B000
  JP V0, #FF              ; B0FF
  JP V0, $567             ; B567
  JP V0, start_of_memory  ; B000
  JP V0, in_reserved_area ; B100

  RND V0, 4               ; C004
  RND V7, #F              ; C70F
  RND VF, 255             ; CFFF

  DRW V0, V0, $0          ; D000
  DRW V1, V1, #1          ; D111
  DRW V2, VF, 1           ; D2F1
  DRW VF, V2, 15          ; DF2F

  SKP V0                  ; E09E
  SKP V1                  ; E19E
  SKP VF                  ; EF9E

  SKNP V0                 ; E0A1
  SKNP V1                 ; E1A1
  SKNP VF                 ; EFA1

  LD V0, DT               ; F007
  LD V1, DT               ; F107
  LD VF, DT               ; FF07

  LD V0, K                ; F00A
  LD V1, K                ; F10A
  LD VF, K                ; FF0A

  LD DT, V0               ; F015
  LD DT, V1               ; F115
  LD DT, VF               ; FF15

  LD ST, V0               ; F018
  LD ST, V1               ; F118
  LD ST, VF               ; FF18

  ADD I, V0               ; F01E
  ADD I, V1               ; F11E
  ADD I, VF               ; FF1E

  SPRITE I, V0            ; F029 or LD F, V0
  SPRITE I, V1            ; F129 or LD F, V1
  SPRITE I, VF            ; FF29 or LD F, VF

  BCD [I], V0             ; F033 or LD B, V0
  BCD [I], V1             ; F133 or LD B, V0
  BCD [I], VF             ; FF33 or LD B, V0

  SAVE [I], V0            ; F055 or LD [I], V0
  SAVE [I], V1            ; F155 or LD [I], V1
  SAVE [I], VF            ; FF55 or LD [I], VF

  RESTORE V0, [I]         ; F065 or LD V0, [I]
  RESTORE V0, [I]         ; F165 or LD V1, [I]
  RESTORE V0, [I]         ; FF65 or LD VF, [I]
