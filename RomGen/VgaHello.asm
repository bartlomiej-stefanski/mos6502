  .org $0x8000    ; Memory ROM start

Reset:
  LDX #$FF
  TXS

Start:
  LDA #$41        ; 'A'
  STA $A000       ; Wpisz do VGA

Loop:
  JMP Loop

  ; --- Wektory (na końcu pliku 8KB) ---
  .org $FFFC
  .word Reset     ; Wektor Resetu
  .word $0000     ; Wektor IRQ (nieużywany)
