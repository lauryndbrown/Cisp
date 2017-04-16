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
       01 WS-SYMBOL-LENGTH PIC 9(10) VALUE 5.
       01 WS-LISP-SYMBOLS.
           02 WS-SYMBOL PIC X(100) OCCURS 5 TIMES.
       01 WS-COUNT PIC 9(10).
       01 STRING-PTR PIC 9(10).
      *     02 WS-SYMBOL-SUBSCRIPT PIC S9(3) COMP-3.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM FILE-HANDLING-PROCEDURE.
           PERFORM LISP-PROCEDURE.
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
       LISP-PROCEDURE.
      *     UNSTRING IN-LISP-RECORD DELIMITED BY '(' OR ')' OR ' '
      *     INSPECT IN-LISP-RECORD TALLYING WS-NUM-SYMBOLS FOR ALL ' '
      *         BEFORE INITIAL SPACES.
           MOVE 1 TO STRING-PTR.
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL
             WS-COUNT > WS-SYMBOL-LENGTH
               UNSTRING IN-LISP-RECORD DELIMITED BY ALL ' ' INTO
               WS-SYMBOL(WS-COUNT) WITH POINTER STRING-PTR
           END-PERFORM.
      *     UNSTRING IN-LISP-RECORD DELIMITED BY ALL '('
      *         INTO WS-SYMBOL(1), WS-SYMBOL(2)
      *     END-UNSTRING.
           DISPLAY "LISP PROCEDURE".
      *     DISPLAY IN-LISP-RECORD.
           PERFORM PRINT-SYMBOL-TABLE.
       PRINT-SYMBOL-TABLE.
           MOVE 1 TO WS-COUNT.
           PERFORM UNTIL WS-COUNT GREATER THAN WS-SYMBOL-LENGTH
               DISPLAY WS-COUNT
               DISPLAY WS-SYMBOL(WS-COUNT)
               ADD 1 TO WS-COUNT
           END-PERFORM.
       END PROGRAM CISP.
