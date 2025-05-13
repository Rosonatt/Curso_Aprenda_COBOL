       IDENTIFICATION DIVISION.
       PROGRAM-ID. PILHA-EXEMPLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PILHA.
           05  PILHA-ITENS OCCURS 10 TIMES PIC X(20).
           05  PILHA-TOPO   PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN.
           PERFORM PUSH "Primeiro"
           PERFORM PUSH "Segundo"
           PERFORM PUSH "Terceiro"
           
           PERFORM POP
           PERFORM POP
           PERFORM POP
           
           STOP RUN.
       
       PUSH.
           ADD 1 TO PILHA-TOPO
           MOVE FUNCTION UPPER-CASE(IN-ITEM) TO PILHA-ITENS(PILHA-TOPO)
           DISPLAY "Item empilhado: " PILHA-ITENS(PILHA-TOPO).
       
       POP.
           IF PILHA-TOPO = 0
               DISPLAY "Pilha vazia!"
           ELSE
               DISPLAY "Item desempilhado: " PILHA-ITENS(PILHA-TOPO)
               SUBTRACT 1 FROM PILHA-TOPO
           END-IF.