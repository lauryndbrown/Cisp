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
           SELECT LISP-FILE ASSIGN TO DYNAMIC LISP-NAME
               ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD LISP-FILE.
           01 IN-LISP-RECORD PIC X(2000).
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM FILE-HANDLING-PROCEDURE.
           STOP RUN.
       GET-FILE-NAME-PROCEDURE.
           ACCEPT LISP-NAME.
           IF LISP-NAME EQUALS SPACES THEN
               MOVE "..\test\arithmetic.lisp" TO LISP-NAME
           END-IF.
       FILE-HANDLING-PROCEDURE.
           PERFORM GET-FILE-NAME-PROCEDURE.
           OPEN INPUT LISP-FILE.
           READ LISP-FILE.
           DISPLAY IN-LISP-RECORD.
           CLOSE LISP-FILE.
       END PROGRAM CISP.
