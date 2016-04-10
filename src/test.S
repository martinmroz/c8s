.org $000

label:
  .db $00
  .db "Hello, World!"
main:
  ld v1, $Q
  trap
  call $1FF
