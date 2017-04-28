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
       01 LISP-TEST-FILE-NAME PIC X(100).
       WORKING-STORAGE SECTION.
       01 WS-CMD-LINE.
           02 WS-CMD-LINE-VAL PIC X(100).
           02 WS-CMD-LINE-NUM-AGRS PIC 9(4).
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
       01 WS-LISP-FILE-NAME PIC X(100).
       78 WS-SYMBOL-LENGTH VALUE 100.
       01 WS-LISP-SYMBOLS.
           02 WS-SYMBOL-TABLE-SIZE PIC 9(4).
           02 WS-SYMBOL PIC X(50) OCCURS WS-SYMBOL-LENGTH TIMES.
           02 WS-SYMBOL-LEN PIC 9(2) OCCURS WS-SYMBOL-LENGTH TIMES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT-LOGGER-PROCEDURE.
           PERFORM READ-CMD-LINE-PROCEDURE.
           PERFORM TOKENIZE-LISP-PROCEDURE.
           PERFORM EVALUTE-LISP-PROCEDURE.
           PERFORM CLOSE-LOGGER-PROCEDURE.
           GOBACK.
       READ-CMD-LINE-PROCEDURE.
      ********* Read the lisp file name and save to working storage
           ACCEPT WS-CMD-LINE-NUM-AGRS FROM ARGUMENT-NUMBER.
           ACCEPT WS-CMD-LINE-VAL FROM ARGUMENT-VALUE.
           MOVE WS-CMD-LINE-VAL TO WS-LISP-FILE-NAME.
           MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
           MOVE "CISP:READ-CMD-LINE-PROCEDURE"
           TO WS-LOG-RECORD-FUNCTION-NAME.
           MOVE "Reading commandline argument" TO WS-LOG-RECORD-MESSAGE.
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
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
