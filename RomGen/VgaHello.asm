  .org $E000      ; Adres startowy ROMu w Twoim systemie

Reset:
  SEI             ; Wyłącz przerwania
  CLD             ; Wyłącz tryb dziesiętny
  LDX #$FF
  TXS             ; Ustaw stos

Start:
  LDA #$41        ; 'A'
  STA $A000       ; Wpisz do VGA
Loop:
  JMP Loop

  ; --- Wektory (na końcu pliku 8KB) ---
  .org $FFFC
  .word Reset     ; Wektor Resetu
  .word $0000     ; Wektor IRQ (nieużywany)
