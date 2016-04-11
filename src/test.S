.org $000

label:
  .db $00
  .db "Hello, World!"
main:
  ld v1, $01
  trap
  call $1FF
