; See https://cc65.github.io/doc/customizing.html for more info.

.export _STARTUP : absolute = 1

.export   _init
.import   _main, copydata, zerobss, initlib

.import __RAM_START__, __RAM_SIZE__, __STACKSIZE__

.include "zeropage.inc"

.segment "VECTORS"
  .word 0, _init, 0   ; NMI, Reset, IRQ


.segment "CODE"
_init:
  SEI
  CLD

  ; Initialize stack to 0x01FF.
  LDX #$FF
  TXS

  ; Initialize software-stack for C runtime.
  LDA #<(__RAM_START__ + __RAM_SIZE__)
  STA sp
  LDA #>(__RAM_START__ + __RAM_SIZE__)
  STA sp+1

  JSR zerobss
  JSR copydata
  JSR initlib

  ; Jump to C runtime.
  JSR _main

_loop:
  JMP _loop
