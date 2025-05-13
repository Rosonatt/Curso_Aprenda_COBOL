       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILA-EXEMPLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILA.
           05  FILA-ITENS OCCURS 10 TIMES PIC X(20).
           05  FILA-FRENTE PIC 9(2) VALUE 1.
           05  FILA-TRAS  PIC 9(2) VALUE 1.
           05  FILA-TAMANHO PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN.
           PERFORM ENFILEIRAR "Primeiro"
           PERFORM ENFILEIRAR "Segundo"
           PERFORM ENFILEIRAR "Terceiro"
           
           PERFORM DESENFILEIRAR
           PERFORM DESENFILEIRAR
           PERFORM DESENFILEIRAR
           
           STOP RUN.
       
       ENFILEIRAR.
           IF FILA-TAMANHO = 10
               DISPLAY "Fila cheia!"
           ELSE
               MOVE FUNCTION UPPER-CASE(IN-ITEM) TO FILA-ITENS(FILA-TRAS)
               DISPLAY "Item enfileirado: " FILA-ITENS(FILA-TRAS)
               ADD 1 TO FILA-TRAS
               IF FILA-TRAS > 10
                   MOVE 1 TO FILA-TRAS
               END-IF
               ADD 1 TO FILA-TAMANHO
           END-IF.
       
       DESENFILEIRAR.
           IF FILA-TAMANHO = 0
               DISPLAY "Fila vazia!"
           ELSE
               DISPLAY "Item desenfileirado: " FILA-ITENS(FILA-FRENTE)
               ADD 1 TO FILA-FRENTE
               IF FILA-FRENTE > 10
                   MOVE 1 TO FILA-FRENTE
               END-IF
               SUBTRACT 1 FROM FILA-TAMANHO
           END-IF.