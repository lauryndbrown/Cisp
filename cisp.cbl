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

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

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
       78 WS-SYMBOL-LENGTH VALUE 40.
       01 WS-LISP-SYMBOLS.
           02 WS-SYMBOL-TABLE-SIZE PIC 9.
           02 WS-SYMBOL PIC X(50) OCCURS WS-SYMBOL-LENGTH TIMES.
           02 WS-SYMBOL-LEN PIC 9(2) OCCURS 40 TIMES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE "OPEN" TO WS-LOG-OPERATION-FLAG.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
      ********* Tokenize the Lisp string
            MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
            MOVE "TOKENIZER" TO WS-LOG-RECORD-FUNCTION-NAME.
            MOVE "Starting Tokenizer" TO WS-LOG-RECORD-MESSAGE.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
            CALL "TOKENIZER" USING WS-SYMBOL-LENGTH, WS-LISP-SYMBOLS.
      ********* Evalute lisp
            MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
            MOVE "LISP" TO WS-LOG-RECORD-FUNCTION-NAME.
            MOVE "Starting Lisp Evalutation" TO WS-LOG-RECORD-MESSAGE.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
            CALL "LISP" USING WS-LISP-SYMBOLS.
            MOVE "CLOSE" TO WS-LOG-OPERATION-FLAG.
            CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
           GOBACK.
       END PROGRAM CISP.
