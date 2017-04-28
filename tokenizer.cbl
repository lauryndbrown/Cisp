      ******************************************************************
      * Author: lauryn brown
      * Date:
      * Purpose: tokenize lisp input file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOKENIZER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LISP-FILE ASSIGN TO DYNAMIC WS-LISP-NAME
               ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD LISP-FILE.
           01 IN-LISP-RECORD PIC X(200).
       WORKING-STORAGE SECTION.
       01 WS-LISP-NAME PIC X(100).
       01 WS-IN-LISP-RECORD PIC X(200).
       01 WS-LISP-EOF PIC X.
       78 WS-MAX-LISP-LENGTH VALUE 200.
       01 WS-LISP-LENGTH PIC 9(10).
       01 WS-CALC-LENGTH-STR PIC X(200).
       01 WS-IS-COMMENT PIC X.
           88 WS-IS-COMMENT-YES VALUE "Y", FALSE 'N'.
       01 WS-FORMAT-LISP.
           02 WS-NUM-LENGTH-ADD PIC 9(10).
           02 WS-PAREN-RIGHT PIC X.
               88 WS-PAREN-RIGHT-YES VALUE "Y", FALSE "N".
           02 WS-PAREN-LEFT PIC X.
               88 WS-PAREN-LEFT-YES VALUE "Y", FALSE "N".
           02 WS-PAREN-TEMP-STR PIC X(2000).
           02 WS-PAREN-TEMP-NUM PIC 9(10).
           02 WS-WHICH-PAREN PIC X.
       01 WS-FORMAT-STR-INDEX PIC 9(10).
       01 WS-COUNT PIC 9(10).
       01 STRING-PTR PIC 9(10).
       01 WS-TEMP-NUM PIC 9(10).
       01 WS-FLAG PIC A(1).
           88 WS-FLAG-YES VALUE 'Y', FALSE 'N'.
       01 WS-SYMBOL-FLAGS.
           02 WS-OPEN-PAREN PIC X.
               88 WS-OPEN-PAREN-YES VALUE 'Y', FALSE 'N'.
           02 WS-CLOSE-PAREN PIC X.
               88 WS-CLOSE-PAREN-YES VALUE 'Y', FALSE 'N'.
       01 WS-PARSE-STR.
           02 WS-PARSE-STR-INDEX PIC 9(5).
           02 WS-PARSE-STR-END PIC X.
               88 WS-PARSE-HAS-ENDED VALUE 'Y', FALSE 'N'.
           02 WS-PARSE-STR-CHAR PIC X.
           02 WS-PARSE-EXPRESSION-START PIC 9(5).
           02 WS-PARSE-EXPRESSION-END PIC 9(5).
           02 WS-PARSE-EXPRESSION-LEN PIC 9(5).
      *****************************************
      *    WS Shared with LOGGER SubRoutine
      *****************************************
       01 WS-LOG-OPERATION-FLAG PIC X(5).
       01 WS-LOG-RECORD.
           02 WS-LOG-RECORD-FUNCTION-NAME PIC X(40).
           02 WS-LOG-RECORD-MESSAGE PIC X(100).
       LINKAGE SECTION.
      ********* Size of table must equal size specified in CISP
       01 LS-LISP-FILE-NAME PIC X(100).
       01 LS-SYMBOL-LENGTH PIC 9(4).
       01 LS-LISP-SYMBOLS.
           02 LS-SYMBOL-TABLE-SIZE PIC 9(4).
           02 LS-SYMBOL PIC X(50) OCCURS 100 TIMES.
           02 LS-SYMBOL-LEN PIC 9(2) OCCURS 100 TIMES.
       PROCEDURE DIVISION USING LS-LISP-FILE-NAME,
             LS-SYMBOL-LENGTH, LS-LISP-SYMBOLS.
       MAIN-PROCEDURE.

      ******** Open and read in the lisp file
           PERFORM FILE-HANDLING-PROCEDURE.
      D    DISPLAY "AFTER FILE-HANDLING-PROCEDURE:" WS-IN-LISP-RECORD.
      ******* tokenize lisp and store in symbol table
           PERFORM TOKENIZE-LISP-PROCEDURE.
           PERFORM CAL-LENGTH-ALL-SYMBOLS.
      D    PERFORM PRINT-SYMBOL-TABLE.
           GOBACK.
       CAL-LENGTH-ALL-SYMBOLS.
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT = 100
               PERFORM CALC-LENGTH-SYMBOL
               MOVE WS-PARSE-EXPRESSION-LEN TO LS-SYMBOL-LEN(WS-COUNT)
           END-PERFORM.
       CALC-LENGTH-SYMBOL.
           SET WS-PARSE-HAS-ENDED TO FALSE.
           MOVE 0 TO WS-PARSE-EXPRESSION-LEN.
           PERFORM VARYING WS-PARSE-STR-INDEX FROM 1 BY 1 UNTIL
           WS-PARSE-HAS-ENDED OR WS-PARSE-STR-INDEX > 100
               IF LS-SYMBOL(WS-COUNT)(WS-PARSE-STR-INDEX:1) = " " THEN
                   SET WS-PARSE-HAS-ENDED TO TRUE
               ELSE
                   ADD 1 TO WS-PARSE-EXPRESSION-LEN
               END-IF
           END-PERFORM.
       APPEND-LISP-PROCEDURE.
      D    DISPLAY IN-LISP-RECORD.
      **********CALC IN-LISP-RECORD LENGTH
           MOVE IN-LISP-RECORD TO WS-CALC-LENGTH-STR
           PERFORM CALC-LISP-LENGTH
           IF NOT WS-IS-COMMENT-YES THEN
               IF WS-TEMP-NUM = 0 THEN
                   MOVE IN-LISP-RECORD TO WS-IN-LISP-RECORD
               ELSE
                   ADD 1 TO WS-TEMP-NUM
                   STRING WS-IN-LISP-RECORD(1:WS-TEMP-NUM)
                   DELIMITED BY SIZE
                   IN-LISP-RECORD(1:WS-LISP-LENGTH) DELIMITED BY SIZE
                   INTO WS-IN-LISP-RECORD
                   SUBTRACT 1 FROM WS-TEMP-NUM
               END-IF
               ADD WS-LISP-LENGTH TO WS-TEMP-NUM
           END-IF.
       FILE-HANDLING-PROCEDURE.
      ***** Opens LISP-FILE for reading ****************************
           MOVE LS-LISP-FILE-NAME TO WS-LISP-NAME
           OPEN INPUT LISP-FILE.
           READ LISP-FILE
               AT END MOVE "Y" TO WS-LISP-EOF
               NOT AT END
                   MOVE IN-LISP-RECORD TO WS-CALC-LENGTH-STR
                   PERFORM CALC-LISP-LENGTH
                   IF NOT WS-IS-COMMENT-YES THEN
                       MOVE IN-LISP-RECORD TO WS-IN-LISP-RECORD
                       MOVE WS-LISP-LENGTH TO WS-TEMP-NUM
                   END-IF
           END-READ.
           PERFORM UNTIL WS-LISP-EOF="Y"
               READ LISP-FILE
                   AT END MOVE "Y" TO WS-LISP-EOF
                   NOT AT END PERFORM APPEND-LISP-PROCEDURE
               END-READ
           END-PERFORM.
           CLOSE LISP-FILE.
      ******LOG File Handling
           MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
           MOVE "TOKENIZER:FILE-HANDLING-PROCEDURE" TO
             WS-LOG-RECORD-FUNCTION-NAME.
           MOVE "COMPLETED reading LISP-FILE" TO WS-LOG-RECORD-MESSAGE.
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       TOKENIZE-LISP-PROCEDURE.
      ******** Tokenizes the lisp file and stores it in the WS-SYMBOL Table
           PERFORM FORMAT-LISP-PROCEDURE.
      D     DISPLAY "After FORMAT-LISP-PROCEDURE".
      D     DISPLAY "TOKENIZE-LISP-PROCEDURE:" WS-IN-LISP-RECORD.
           MOVE 1 TO STRING-PTR.
           MOVE 0 TO LS-SYMBOL-TABLE-SIZE.
           SET WS-FLAG-YES TO FALSE.
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL
             WS-COUNT = 100 OR WS-FLAG
               UNSTRING WS-IN-LISP-RECORD DELIMITED BY ALL ' ' INTO
               LS-SYMBOL(WS-COUNT) WITH POINTER STRING-PTR
               IF LS-SYMBOL(WS-COUNT) = SPACES THEN
                   SET WS-FLAG-YES TO TRUE
               ELSE
                   ADD 1 TO LS-SYMBOL-TABLE-SIZE
               END-IF
           END-PERFORM.
      *****LOG File Handling
           MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
           MOVE "TOKENIZER:TOKENIZE-LISP-PROCEDURE" TO
             WS-LOG-RECORD-FUNCTION-NAME.
           MOVE "COMPLETED tokenizing lisp" TO WS-LOG-RECORD-MESSAGE.
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       PRINT-SYMBOL-TABLE.
      ******* Prints Tokenized lisp stored in WS-SYMBOL Table
           MOVE 1 TO WS-COUNT.
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL
           WS-COUNT GREATER THAN LS-SYMBOL-TABLE-SIZE
               DISPLAY WS-COUNT
               DISPLAY LS-SYMBOL(WS-COUNT)
               DISPLAY LS-SYMBOL-LEN(WS-COUNT)
           END-PERFORM.
       FORMAT-LISP-PROCEDURE.
      ***** Calculates the length of the lisp program.
      ***** Adding additional spaces between parenthesis
      ***** for easier parsing.
      D    DISPLAY "FORMAT-LISP-PROCEDURE:" WS-IN-LISP-RECORD.
           MOVE WS-IN-LISP-RECORD TO WS-CALC-LENGTH-STR.
           PERFORM CALC-LISP-LENGTH.
           MOVE 1 TO WS-FORMAT-STR-INDEX.
           IF WS-IN-LISP-RECORD(1:1)="("
               AND NOT WS-IN-LISP-RECORD(2:1) EQUAL " " THEN
               MOVE WS-IN-LISP-RECORD TO WS-PAREN-TEMP-STR
               STRING "( " DELIMITED BY SIZE
               WS-PAREN-TEMP-STR(2:WS-LISP-LENGTH) DELIMITED BY
               SIZE INTO WS-IN-LISP-RECORD
               ADD 3 TO WS-FORMAT-STR-INDEX
               ADD 1 TO WS-LISP-LENGTH
           END-IF.
           PERFORM VARYING WS-FORMAT-STR-INDEX FROM WS-FORMAT-STR-INDEX
           BY 1 UNTIL WS-FORMAT-STR-INDEX > WS-LISP-LENGTH
               SET WS-PAREN-LEFT-YES TO FALSE
               SET WS-PAREN-RIGHT-YES TO FALSE
               MOVE WS-IN-LISP-RECORD TO WS-PAREN-TEMP-STR
               EVALUATE WS-IN-LISP-RECORD(WS-FORMAT-STR-INDEX:1)
               WHEN "("
                   PERFORM FORMAT-PAREN-SPACE-PROCEDURE
               WHEN ")"
                   PERFORM FORMAT-PAREN-SPACE-PROCEDURE
      *         WHEN ";"

               END-EVALUATE
      D         DISPLAY WS-IN-LISP-RECORD(WS-FORMAT-STR-INDEX:1)
      D         " left:" WS-PAREN-RIGHT " right:" WS-PAREN-LEFT
           END-PERFORM.
      ****** Log FORMAT-LISP-PROCEDURE Complete
           MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
           MOVE "TOKENIZER:FORMAT-LISP-PROCEDURE" TO
             WS-LOG-RECORD-FUNCTION-NAME.
           MOVE "COMPLETED formatting lisp string for parsing" TO
             WS-LOG-RECORD-MESSAGE.
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       CALC-LISP-LENGTH.
      *****Calculate the acutal length of the lisp
           MOVE 0 TO WS-LISP-LENGTH.
           MOVE 0 TO WS-NUM-LENGTH-ADD.
           SET WS-IS-COMMENT-YES TO FALSE.
           PERFORM VARYING WS-FORMAT-STR-INDEX FROM 1 BY 1 UNTIL
           WS-FORMAT-STR-INDEX = WS-MAX-LISP-LENGTH
               IF WS-CALC-LENGTH-STR(WS-FORMAT-STR-INDEX:1)
               EQUAL ";" THEN
                   SET WS-IS-COMMENT-YES TO TRUE
               ELSE IF NOT WS-CALC-LENGTH-STR(WS-FORMAT-STR-INDEX:1)
               EQUALS " " THEN
                   ADD 1 TO WS-LISP-LENGTH
                   ADD WS-NUM-LENGTH-ADD TO WS-LISP-LENGTH
                   MOVE 0 TO WS-NUM-LENGTH-ADD
               ELSE
                   ADD 1 TO WS-NUM-LENGTH-ADD
               END-IF
           END-PERFORM.
       RESET-PARSE-FLAGS-PROCEDURE.
           SET WS-OPEN-PAREN-YES TO FALSE.
           SET WS-CLOSE-PAREN-YES TO FALSE.
           MOVE 0 TO WS-PARSE-EXPRESSION-START.
           MOVE 0 TO WS-PARSE-EXPRESSION-END.
           MOVE 0 TO WS-PARSE-EXPRESSION-LEN.
       PRINT-PARSE-FLAGS-PROCEDURE.
           DISPLAY 'Open Paren:' WS-OPEN-PAREN.
           DISPLAY 'Close Paren:' WS-CLOSE-PAREN.
           DISPLAY 'Expression Start:' WS-PARSE-EXPRESSION-START.
           DISPLAY 'Expression END:' WS-PARSE-EXPRESSION-END.
           DISPLAY 'Expression Length:' WS-PARSE-EXPRESSION-LEN.
       FORMAT-CHECK-PAREN-PROCEDURE.
      *    ----Check left side of paren
           SUBTRACT 1 FROM WS-FORMAT-STR-INDEX.
           IF NOT WS-IN-LISP-RECORD(WS-FORMAT-STR-INDEX:1)EQUAL " " THEN
               SET WS-PAREN-LEFT-YES TO TRUE
           END-IF.
      *    ----Check right side of paren
           ADD 2 TO WS-FORMAT-STR-INDEX.
           IF NOT WS-IN-LISP-RECORD(WS-FORMAT-STR-INDEX:1)EQUAL " " THEN
               SET WS-PAREN-RIGHT-YES TO TRUE
           END-IF.
      *    ----Reset the Index to it's original position
           SUBTRACT 1 FROM WS-FORMAT-STR-INDEX.

       FORMAT-ADD-LEFT-SPACE.
           MOVE WS-FORMAT-STR-INDEX TO WS-PAREN-TEMP-NUM.
           SUBTRACT 1 FROM WS-PAREN-TEMP-NUM.
           STRING WS-PAREN-TEMP-STR(1:WS-PAREN-TEMP-NUM)
            DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            WS-PAREN-TEMP-STR(WS-FORMAT-STR-INDEX:WS-LISP-LENGTH)
            DELIMITED BY SIZE INTO WS-IN-LISP-RECORD.
           ADD 1 TO WS-FORMAT-STR-INDEX.
           ADD 1 TO WS-LISP-LENGTH.
       FORMAT-ADD-RIGHT-SPACE.
           MOVE WS-FORMAT-STR-INDEX TO WS-PAREN-TEMP-NUM.
           ADD 1 TO WS-PAREN-TEMP-NUM.
           STRING WS-PAREN-TEMP-STR(1:WS-FORMAT-STR-INDEX)
            DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            WS-PAREN-TEMP-STR(WS-PAREN-TEMP-NUM:WS-LISP-LENGTH)
            DELIMITED BY SIZE INTO WS-IN-LISP-RECORD.
           ADD 1 TO WS-FORMAT-STR-INDEX.
           ADD 1 TO WS-LISP-LENGTH.
       FORMAT-ADD-BOTH-SPACES.
           MOVE WS-FORMAT-STR-INDEX TO WS-PAREN-TEMP-NUM.
           SUBTRACT 1 FROM WS-PAREN-TEMP-NUM.
           MOVE WS-PAREN-TEMP-STR(WS-FORMAT-STR-INDEX:1)
            TO WS-WHICH-PAREN.
           ADD 1 TO WS-FORMAT-STR-INDEX.
           STRING WS-PAREN-TEMP-STR(1:WS-PAREN-TEMP-NUM)
            DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            WS-WHICH-PAREN DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            WS-PAREN-TEMP-STR(WS-FORMAT-STR-INDEX:WS-LISP-LENGTH)
            INTO WS-IN-LISP-RECORD.
           ADD 1 TO WS-FORMAT-STR-INDEX.
           ADD 2 TO WS-LISP-LENGTH.
       FORMAT-PAREN-SPACE-PROCEDURE.
           PERFORM FORMAT-CHECK-PAREN-PROCEDURE.
           IF WS-PAREN-RIGHT-YES AND WS-PAREN-LEFT-YES THEN
               PERFORM FORMAT-ADD-BOTH-SPACES
           ELSE IF WS-PAREN-RIGHT-YES THEN
               PERFORM FORMAT-ADD-RIGHT-SPACE
           ELSE IF WS-PAREN-LEFT-YES THEN
               PERFORM FORMAT-ADD-LEFT-SPACE
           END-IF.
       END PROGRAM TOKENIZER.
