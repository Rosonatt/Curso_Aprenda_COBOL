       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEQUE-EXEMPLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DEQUE.
           05  DEQUE-ITENS OCCURS 10 TIMES PIC X(20).
           05  DEQUE-FRENTE PIC 9(2) VALUE 1.
           05  DEQUE-TRAS   PIC 9(2) VALUE 1.
           05  DEQUE-TAMANHO PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN.
           PERFORM INSERIR-FRENTE "Front1"
           PERFORM INSERIR-TRAS "Back1"
           PERFORM INSERIR-FRENTE "Front2"
           PERFORM INSERIR-TRAS "Back2"
           
           PERFORM REMOVER-FRENTE
           PERFORM REMOVER-TRAS
           PERFORM REMOVER-FRENTE
           PERFORM REMOVER-TRAS
           
           STOP RUN.
       
       INSERIR-FRENTE.
           IF DEQUE-TAMANHO = 10
               DISPLAY "Deque cheio!"
           ELSE
               SUBTRACT 1 FROM DEQUE-FRENTE
               IF DEQUE-FRENTE < 1
                   MOVE 10 TO DEQUE-FRENTE
               END-IF
               MOVE FUNCTION UPPER-CASE(IN-ITEM) TO DEQUE-ITENS(DEQUE-FRENTE)
               DISPLAY "Inserido na frente: " DEQUE-ITENS(DEQUE-FRENTE)
               ADD 1 TO DEQUE-TAMANHO
           END-IF.
       
       INSERIR-TRAS.
           IF DEQUE-TAMANHO = 10
               DISPLAY "Deque cheio!"
           ELSE
               MOVE FUNCTION UPPER-CASE(IN-ITEM) TO DEQUE-ITENS(DEQUE-TRAS)
               DISPLAY "Inserido atrás: " DEQUE-ITENS(DEQUE-TRAS)
               ADD 1 TO DEQUE-TRAS
               IF DEQUE-TRAS > 10
                   MOVE 1 TO DEQUE-TRAS
               END-IF
               ADD 1 TO DEQUE-TAMANHO
           END-IF.
       
       REMOVER-FRENTE.
           IF DEQUE-TAMANHO = 0
               DISPLAY "Deque vazio!"
           ELSE
               DISPLAY "Removido da frente: " DEQUE-ITENS(DEQUE-FRENTE)
               ADD 1 TO DEQUE-FRENTE
               IF DEQUE-FRENTE > 10
                   MOVE 1 TO DEQUE-FRENTE
               END-IF
               SUBTRACT 1 FROM DEQUE-TAMANHO
           END-IF.
       
       REMOVER-TRAS.
           IF DEQUE-TAMANHO = 0
               DISPLAY "Deque vazio!"
           ELSE
               SUBTRACT 1 FROM DEQUE-TRAS
               IF DEQUE-TRAS < 1
                   MOVE 10 TO DEQUE-TRAS
               END-IF
               DISPLAY "Removido de trás: " DEQUE-ITENS(DEQUE-TRAS)
               SUBTRACT 1 FROM DEQUE-TAMANHO
           END-IF.