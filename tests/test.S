
.org $000
message:
  .db "Hello, World!", $00

.org $200
main:
  ; Load the Index register with the address of message.
  ld i, message
  trap
