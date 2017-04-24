      ******************************************************************
      * Author: Lauryn Brown
      * Date: 2017
      * Purpose: COBOL Common Lisp Interpreter
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CISP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT TESTS-FILE ASSIGN TO "..\test\tests-lists.txt"
          ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD TESTS-FILE.
       01 LISP-TEST-FILE-NAME PIC X(30).
       WORKING-STORAGE SECTION.
       01 WS-LISP-TESTS-FILE-EOF PIC X.
      *     88 WS-LISP-TESTS-FILE-EOF-YES VALUE'Y', FALSE 'N'.
      *****************************************
      *    WS Shared with LOGGER SubRoutine
      *****************************************
           01 WS-LOG-OPERATION-FLAG PIC X(5).
           01 WS-LOG-RECORD.
               02 WS-LOG-RECORD-FUNCTION-NAME PIC X(40).
               02 WS-LOG-RECORD-MESSAGE PIC X(100).
      *****************************************
      *    WS Shared with TOKENIZER, LISP SubRoutine
      *****************************************
      *****IF WS-SYMBOL-LENGTH CHANGED HERE PLEASE CHANGE IN TOKENIZER, LISP
       01 WS-LISP-FILE-NAME PIC X(30).
       78 WS-SYMBOL-LENGTH VALUE 100.
       01 WS-LISP-SYMBOLS.
           02 WS-SYMBOL-TABLE-SIZE PIC 9(4).
           02 WS-SYMBOL PIC X(50) OCCURS WS-SYMBOL-LENGTH TIMES.
           02 WS-SYMBOL-LEN PIC 9(2) OCCURS WS-SYMBOL-LENGTH TIMES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT-LOGGER-PROCEDURE.
           OPEN INPUT TESTS-FILE.
           MOVE 'N' TO WS-LISP-TESTS-FILE-EOF.
           PERFORM UNTIL WS-LISP-TESTS-FILE-EOF='Y'
               READ TESTS-FILE
                   AT END MOVE 'Y' TO WS-LISP-TESTS-FILE-EOF
                   NOT AT END PERFORM LISP-PROCEDURE
               END-READ
           END-PERFORM.
           CLOSE TESTS-FILE.
           PERFORM CLOSE-LOGGER-PROCEDURE.
           GOBACK.
       LISP-PROCEDURE.
           MOVE LISP-TEST-FILE-NAME TO WS-LISP-FILE-NAME.
           DISPLAY "FILE:" WS-LISP-FILE-NAME.
           PERFORM TOKENIZE-LISP-PROCEDURE.
           PERFORM EVALUTE-LISP-PROCEDURE.
       TOKENIZE-LISP-PROCEDURE.
      ********* Tokenize the Lisp string
            MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
            MOVE "TOKENIZER" TO WS-LOG-RECORD-FUNCTION-NAME.
            MOVE "Starting Tokenizer" TO WS-LOG-RECORD-MESSAGE.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
            CALL "TOKENIZER" USING WS-LISP-FILE-NAME,
                 WS-SYMBOL-LENGTH, WS-LISP-SYMBOLS.
       EVALUTE-LISP-PROCEDURE.
      ********* Evalute lisp
            MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
            MOVE "LISP" TO WS-LOG-RECORD-FUNCTION-NAME.
            MOVE "Starting Lisp Evalutation" TO WS-LOG-RECORD-MESSAGE.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
            CALL "LISP" USING WS-LISP-SYMBOLS.
       INIT-LOGGER-PROCEDURE.
            MOVE "OPEN" TO WS-LOG-OPERATION-FLAG.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       CLOSE-LOGGER-PROCEDURE.
            MOVE "CLOSE" TO WS-LOG-OPERATION-FLAG.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.

       END PROGRAM CISP.
