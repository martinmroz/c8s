;
; Copyright 2016 The c8s Developers. See the COPYRIGHT
; file at the top-level directory of this distribution.
;
; Licensed under the MIT license <LICENSE-MIT or http://opensource.org/licenses/MIT>.
; All files in the project carrying such notice may not be copied, modified, or 
; distributed except according to those terms.
;

.org $000
message:
  .db "Hello, World!", $00

.org $200
main:
  ; Load the Index register with the address of message.
  ld i, message
  trap
