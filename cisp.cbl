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
           01 WS-SHARED-DATA PIC X(10).
           01 WS-LOG-RECORD.
               02 WS-LOG-RECORD-ID PIC 9(10).
               02 WS-LOG-RECORD-FUNCTION-NAME PIC X(40).
               02 WS-LOG-RECORD-MESSAGE PIC X(100).
      *****************************************
      *    WS Shared with TOKENIZER SubRoutine
      *****************************************

      *****************************************
      *    WS Shared with LISP SubRoutine
      *****************************************

      *****************************************
      *    WS Shared with RECURSION SubRoutine
      *****************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "CISP".
            CALL 'LOGGER' USING WS-LOG-RECORD.
            DISPLAY WS-LOG-RECORD.
      *     PERFORM LOG-INIT-PROCEDURE.
      *     PERFORM FILE-HANDLING-PROCEDURE.
      *     MOVE "MAIN-PROCEDURE" TO WS-LOG-RECORD-FUNCTION-NAME.
      *     MOVE "COMPLETED FILE-HANDLING-PROCEDURE"
      *       TO WS-LOG-RECORD-MESSAGE.
      *     PERFORM LOG-WRITE-TO-PROCEDURE.
      *     PERFORM LISP-PROCEDURE.
      *     MOVE "MAIN-PROCEDURE" TO WS-LOG-RECORD-FUNCTION-NAME.
      *     MOVE "COMPLETED LISP-PROCEDURE"
      *       TO WS-LOG-RECORD-MESSAGE.
      *     PERFORM LOG-WRITE-TO-PROCEDURE.
           STOP RUN.
       GET-FILE-NAME-PROCEDURE.
      *     ACCEPT LISP-NAME.
      *     IF LISP-NAME EQUALS SPACES THEN
      *         MOVE "..\test\arithmetic.lisp" TO LISP-NAME
      *     END-IF.
       FILE-HANDLING-PROCEDURE.
           PERFORM GET-FILE-NAME-PROCEDURE.
      *     OPEN INPUT LISP-FILE.
      *     READ LISP-FILE.
      *     CLOSE LISP-FILE.
       LISP-PROCEDURE.
      *     PERFORM UNSTRING-LISP-PROCEDURE.
      *******log completion
      *     MOVE "LISP-PROCEDURE" TO WS-LOG-RECORD-FUNCTION-NAME.
      *     MOVE "COMPLETED UNSTRING-LISP-PROCEDURE"
      *       TO WS-LOG-RECORD-MESSAGE.
      *     PERFORM LOG-WRITE-TO-PROCEDURE.
      ******
      *     PERFORM EVALUATE-LISP-PRCEDURE.
       END PROGRAM CISP.
