.org $000

label:
  .db "Hello, World!", $00
main:
  ld v1, $01
  trap
  call $1FF
