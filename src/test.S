.org $000

label:
  .db $00
  .db "Hello, World!"
main:
  trap
  call $1FF
