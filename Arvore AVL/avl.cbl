       IDENTIFICATION DIVISION.
       PROGRAM-ID. AVL-TREE-COMPLETE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NODE.
           05  WS-DATA           PIC 9(04).
           05  WS-LEFT           USAGE POINTER.
           05  WS-RIGHT          USAGE POINTER.
           05  WS-HEIGHT         PIC 9(02).
       
       01  ROOT-NODE            USAGE POINTER VALUE NULL.
       01  NEW-NODE             USAGE POINTER.
       01  TEMP-NODE            USAGE POINTER.
       01  TEMP-NODE2           USAGE POINTER.
       
       01  INPUT-VALUE          PIC 9(04).
       01  CHOICE               PIC 9(01).
       01  FOUND-FLAG           PIC X(01) VALUE 'N'.
           88  FOUND            VALUE 'Y'.
           88  NOT-FOUND        VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "AVL Tree Implementation in COBOL (with deletion)".
           
           PERFORM UNTIL CHOICE = 0
               DISPLAY " "
               DISPLAY "1. Insert Node"
               DISPLAY "2. Delete Node"
               DISPLAY "3. Search Node"
               DISPLAY "4. Display Tree (Inorder)"
               DISPLAY "5. Display Tree (Preorder)"
               DISPLAY "6. Display Tree (Postorder)"
               DISPLAY "0. Exit"
               DISPLAY "Enter your choice: " WITH NO ADVANCING
               ACCEPT CHOICE
               
               EVALUATE CHOICE
                   WHEN 1
                       PERFORM INSERT-NODE
                   WHEN 2
                       PERFORM DELETE-NODE
                   WHEN 3
                       PERFORM SEARCH-NODE
                   WHEN 4
                       PERFORM DISPLAY-INORDER
                   WHEN 5
                       PERFORM DISPLAY-PREORDER
                   WHEN 6
                       PERFORM DISPLAY-POSTORDER
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM.
           
           STOP RUN.

       INSERT-NODE.
           DISPLAY "Enter value to insert: " WITH NO ADVANCING
           ACCEPT INPUT-VALUE
           CALL "INSERT" USING ROOT-NODE, INPUT-VALUE.

       DELETE-NODE.
           DISPLAY "Enter value to delete: " WITH NO ADVANCING
           ACCEPT INPUT-VALUE
           CALL "DELETE" USING ROOT-NODE, INPUT-VALUE.

       SEARCH-NODE.
           DISPLAY "Enter value to search: " WITH NO ADVANCING
           ACCEPT INPUT-VALUE
           SET NOT-FOUND TO TRUE
           CALL "SEARCH" USING ROOT-NODE, INPUT-VALUE, FOUND-FLAG
           IF FOUND
               DISPLAY "Value " INPUT-VALUE " found in tree."
           ELSE
               DISPLAY "Value " INPUT-VALUE " not found in tree."
           END-IF.

       DISPLAY-INORDER.
           IF ROOT-NODE = NULL
               DISPLAY "Tree is empty"
           ELSE
               DISPLAY "Inorder Traversal:"
               CALL "INORDER" USING ROOT-NODE
           END-IF.

       DISPLAY-PREORDER.
           IF ROOT-NODE = NULL
               DISPLAY "Tree is empty"
           ELSE
               DISPLAY "Preorder Traversal:"
               CALL "PREORDER" USING ROOT-NODE
           END-IF.

       DISPLAY-POSTORDER.
           IF ROOT-NODE = NULL
               DISPLAY "Tree is empty"
           ELSE
               DISPLAY "Postorder Traversal:"
               CALL "POSTORDER" USING ROOT-NODE
           END-IF.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT.
       DATA DIVISION.
       LINKAGE SECTION.
       01  NODE-PTR             USAGE POINTER.
       01  VALUE-TO-INSERT      PIC 9(04).
       
       PROCEDURE DIVISION USING NODE-PTR, VALUE-TO-INSERT.
           IF NODE-PTR = NULL
               ALLOCATE WS-NODE
               SET ADDRESS OF NEW-NODE TO NODE-PTR
               MOVE VALUE-TO-INSERT TO WS-DATA OF NEW-NODE
               SET WS-LEFT OF NEW-NODE TO NULL
               SET WS-RIGHT OF NEW-NODE TO NULL
               MOVE 1 TO WS-HEIGHT OF NEW-NODE
           ELSE
               SET ADDRESS OF TEMP-NODE TO NODE-PTR
               IF VALUE-TO-INSERT < WS-DATA OF TEMP-NODE
                   CALL "INSERT" USING WS-LEFT OF TEMP-NODE, VALUE-TO-INSERT
               ELSE
                   IF VALUE-TO-INSERT > WS-DATA OF TEMP-NODE
                       CALL "INSERT" USING WS-RIGHT OF TEMP-NODE, VALUE-TO-INSERT
                   ELSE
                       DISPLAY "Value already exists in tree."
                   END-IF
               END-IF
               
               *> Update height and balance the tree
               CALL "UPDATE-HEIGHT" USING NODE-PTR
               CALL "BALANCE-TREE" USING NODE-PTR
           END-IF.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELETE.
       DATA DIVISION.
       LINKAGE SECTION.
       01  NODE-PTR             USAGE POINTER.
       01  VALUE-TO-DELETE      PIC 9(04).
       
       PROCEDURE DIVISION USING NODE-PTR, VALUE-TO-DELETE.
           IF NODE-PTR = NULL
               DISPLAY "Value not found in tree."
               EXIT PROGRAM
           END-IF
           
           SET ADDRESS OF TEMP-NODE TO NODE-PTR
           
           *> Search for the node to delete
           IF VALUE-TO-DELETE < WS-DATA OF TEMP-NODE
               CALL "DELETE" USING WS-LEFT OF TEMP-NODE, VALUE-TO-DELETE
           ELSE
               IF VALUE-TO-DELETE > WS-DATA OF TEMP-NODE
                   CALL "DELETE" USING WS-RIGHT OF TEMP-NODE, VALUE-TO-DELETE
               ELSE
                   *> Node found - perform deletion
                   IF WS-LEFT OF TEMP-NODE = NULL OR WS-RIGHT OF TEMP-NODE = NULL
                       *> Node with 0 or 1 child
                       IF WS-LEFT OF TEMP-NODE = NULL
                           SET TEMP-NODE2 TO WS-RIGHT OF TEMP-NODE
                       ELSE
                           SET TEMP-NODE2 TO WS-LEFT OF TEMP-NODE
                       END-IF
                       
                       *> No child case
                       IF TEMP-NODE2 = NULL
                           SET TEMP-NODE2 TO TEMP-NODE
                           SET NODE-PTR TO NULL
                       ELSE
                           *> One child case
                           SET ADDRESS OF TEMP-NODE2 TO TEMP-NODE2
                           MOVE WS-DATA OF TEMP-NODE2 TO WS-DATA OF TEMP-NODE
                           SET WS-LEFT OF TEMP-NODE TO WS-LEFT OF TEMP-NODE2
                           SET WS-RIGHT OF TEMP-NODE TO WS-RIGHT OF TEMP-NODE2
                           SET WS-HEIGHT OF TEMP-NODE TO WS-HEIGHT OF TEMP-NODE2
                       END-IF
                       
                       FREE TEMP-NODE2
                   ELSE
                       *> Node with 2 children - get inorder successor
                       SET TEMP-NODE2 TO WS-RIGHT OF TEMP-NODE
                       PERFORM UNTIL WS-LEFT OF TEMP-NODE2 = NULL
                           SET ADDRESS OF TEMP-NODE2 TO WS-LEFT OF TEMP-NODE2
                       END-PERFORM
                       
                       *> Copy successor data to current node
                       MOVE WS-DATA OF TEMP-NODE2 TO WS-DATA OF TEMP-NODE
                       
                       *> Delete the successor
                       CALL "DELETE" USING WS-RIGHT OF TEMP-NODE, WS-DATA OF TEMP-NODE2
                   END-IF
               END-IF
           END-IF
           
           *> Update height and balance the tree if node was deleted
           IF NODE-PTR NOT = NULL
               CALL "UPDATE-HEIGHT" USING NODE-PTR
               CALL "BALANCE-TREE" USING NODE-PTR
           END-IF.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH.
       DATA DIVISION.
       LINKAGE SECTION.
       01  NODE-PTR             USAGE POINTER.
       01  VALUE-TO-SEARCH      PIC 9(04).
       01  FOUND-FLAG           PIC X(01).
       
       PROCEDURE DIVISION USING NODE-PTR, VALUE-TO-SEARCH, FOUND-FLAG.
           IF NODE-PTR = NULL
               EXIT PROGRAM
           END-IF
           
           SET ADDRESS OF TEMP-NODE TO NODE-PTR
           IF VALUE-TO-SEARCH = WS-DATA OF TEMP-NODE
               SET FOUND TO TRUE
           ELSE
               IF VALUE-TO-SEARCH < WS-DATA OF TEMP-NODE
                   CALL "SEARCH" USING WS-LEFT OF TEMP-NODE, VALUE-TO-SEARCH, FOUND-FLAG
               ELSE
                   CALL "SEARCH" USING WS-RIGHT OF TEMP-NODE, VALUE-TO-SEARCH, FOUND-FLAG
               END-IF
           END-IF.
           EXIT PROGRAM.

       *> (Include here the UPDATE-HEIGHT, GET-HEIGHT, BALANCE-TREE, 
       *> ROTATE-RIGHT, ROTATE-LEFT procedures from the previous example)
       *> They are exactly the same as in the previous code

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INORDER.
       DATA DIVISION.
       LINKAGE SECTION.
       01  NODE-PTR             USAGE POINTER.
       
       PROCEDURE DIVISION USING NODE-PTR.
           IF NODE-PTR NOT = NULL
               SET ADDRESS OF TEMP-NODE TO NODE-PTR
               CALL "INORDER" USING WS-LEFT OF TEMP-NODE
               DISPLAY WS-DATA OF TEMP-NODE
               CALL "INORDER" USING WS-RIGHT OF TEMP-NODE
           END-IF.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PREORDER.
       DATA DIVISION.
       LINKAGE SECTION.
       01  NODE-PTR             USAGE POINTER.
       
       PROCEDURE DIVISION USING NODE-PTR.
           IF NODE-PTR NOT = NULL
               SET ADDRESS OF TEMP-NODE TO NODE-PTR
               DISPLAY WS-DATA OF TEMP-NODE
               CALL "PREORDER" USING WS-LEFT OF TEMP-NODE
               CALL "PREORDER" USING WS-RIGHT OF TEMP-NODE
           END-IF.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTORDER.
       DATA DIVISION.
       LINKAGE SECTION.
       01  NODE-PTR             USAGE POINTER.
       
       PROCEDURE DIVISION USING NODE-PTR.
           IF NODE-PTR NOT = NULL
               SET ADDRESS OF TEMP-NODE TO NODE-PTR
               CALL "POSTORDER" USING WS-LEFT OF TEMP-NODE
               CALL "POSTORDER" USING WS-RIGHT OF TEMP-NODE
               DISPLAY WS-DATA OF TEMP-NODE
           END-IF.
           EXIT PROGRAM.