
.org $000
CONST_12B:  .equ    $FFF 
CONST_08B:  .equ    255
CONST_04B:  .equ    15
ZERO:       .equ    0

test:
  se    v0, CONST_08B
  se    v0, 255
  sne   v0, CONST_04B
  sne   v0, 15
  ld    v0, CONST_08B
  ld    v0, 255
  add   v0, CONST_04B
  add   v0, 15
  rnd   v0, CONST_08B
  rnd   v0, 255
  drw   v0, v1, CONST_04B
  drw   v0, v1, 15
  ld    v0, test
  ld    v0, 0
