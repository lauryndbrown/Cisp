      ******************************************************************
      * Author: Lauryn Brown
      * Date:
      * Purpose: Evalute tokenized lisp
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISP.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-SYMBOL-TABLE-INDEX PIC 9(4).
       01 WS-CURR-COMMAND PIC X(20).
       01 WS-CURRENT-VALUE PIC X(20).
       01 WS-CURRENT-VALUE-NUMERIC
       REDEFINES WS-CURRENT-VALUE PIC 9(20).
       01 WS-INIT-COMMAND PIC X.
           88 WS-INIT-COMMAND-YES VALUE "Y", FALSE 'N'.
      *****************************************
      *    WS Shared with CISP-ERROR SubRoutine
      *****************************************
       01 WS-CISP-ERROR-FLAG PIC X(30).
       01 WS-ERROR.
          02 WS-ERROR-NAME PIC X(40).
          02 WS-ERROR-FATAL PIC X.
           88 WS-ERROR-FATAL-YES VALUE 'Y', FALSE 'N'.
          02 WS-ERROR-MESSAGE PIC X(100).
      *****************************************
      *    WS Shared with LOGGER SubRoutine
      *****************************************
           01 WS-LOG-OPERATION-FLAG PIC X(5).
           01 WS-LOG-RECORD.
               02 WS-LOG-RECORD-FUNCTION-NAME PIC X(40).
               02 WS-LOG-RECORD-MESSAGE PIC X(100).
      *****************************************
      *    WS Shared with RECUSRION SubRoutine
      *****************************************
       01 WS-RECURSION-FLAG PIC X(30).
       01 WS-RECURSION-OBJECT.
          02 WS-COMMAND-NAME PIC X(20).
          02 WS-COMMAND-RESULT PIC X(20).
          02 WS-COMMAND-RESULT-NUMERIC
          REDEFINES WS-COMMAND-RESULT PIC 9(20).
          02 WS-COMMAND-RETURNS-RESULT PIC X.
             88 WS-COMMAND-RETURNS-RESULT-YES VALUE 'Y', FALSE 'N'.
       LINKAGE SECTION.
       01 LS-LISP-SYMBOLS.
           02 LS-SYMBOL-TABLE-SIZE PIC 9(4).
           02 LS-SYMBOL PIC X(50) OCCURS 100 TIMES.
           02 LS-SYMBOL-LEN PIC 9(2) OCCURS 100 TIMES.
       PROCEDURE DIVISION USING LS-LISP-SYMBOLS.
       MAIN-PROCEDURE.
           PERFORM INIT-CALL-STACK-PROCEDURE.
      ********* EVALUTE LISP
           PERFORM VARYING WS-SYMBOL-TABLE-INDEX FROM 1 BY 1 UNTIL
           WS-SYMBOL-TABLE-INDEX > LS-SYMBOL-TABLE-SIZE
               EVALUATE LS-SYMBOL(WS-SYMBOL-TABLE-INDEX)
               WHEN "("
                   SET WS-INIT-COMMAND-YES TO TRUE
               WHEN ")"
                   PERFORM LOG-COMMAND-EVALUTATION
                   PERFORM RETURN-PROCEDURE
               WHEN OTHER
                   MOVE LS-SYMBOL(WS-SYMBOL-TABLE-INDEX)
                    TO WS-CURR-COMMAND
                   PERFORM LOG-CURRENT-COMMAND-PROCEDURE
      D             PERFORM DEBUG-LISP
                   IF WS-INIT-COMMAND-YES THEN
                       PERFORM INIT-RECURSION-OBJECT-PROCEDURE
                   ELSE
                       PERFORM EVALUATE-CURRENT-COMMAND
                   END-IF
                END-EVALUATE
           END-PERFORM.
           PERFORM CLOSE-CALL-STACK-PROCEDURE.
           GOBACK.
       INIT-CALL-STACK-PROCEDURE.
      *********Initialize Call stack for Recursion
       MOVE "INIT" TO WS-RECURSION-FLAG.
       CALL "RECURSION" USING WS-RECURSION-FLAG.
       INIT-RECURSION-OBJECT-PROCEDURE.

           IF WS-COMMAND-NAME = SPACES THEN
               MOVE WS-CURR-COMMAND TO WS-COMMAND-NAME
           ELSE
      *****Recursion detected saving current state to the stack
               MOVE "ADD-TO-CALL-STACK" TO WS-RECURSION-FLAG
               CALL "RECURSION" USING WS-RECURSION-FLAG,
               WS-RECURSION-OBJECT
      D        DISPLAY "INIT-CALL-STACK-PROCEDURE: " WS-RECURSION-OBJECT
      ******Add the next command to the recursion OBJECT
               MOVE WS-CURR-COMMAND TO WS-COMMAND-NAME
               MOVE SPACES TO WS-COMMAND-RESULT
      D         DISPLAY "New saved temp OBJECT:" WS-COMMAND-NAME
      D         Display " "
           END-IF.
           SET WS-INIT-COMMAND-YES TO FALSE.
       RETURN-PROCEDURE.
           MOVE "IS-EMPTY" TO WS-RECURSION-FLAG.
           CALL "RECURSION" USING WS-RECURSION-FLAG.
      D     DISPLAY "RETURN-PROCEDURE: " WS-RECURSION-FLAG.
           IF NOT WS-RECURSION-FLAG = "STACK-EMPTY" THEN
               MOVE WS-COMMAND-RESULT TO WS-CURRENT-VALUE
      D         display WS-RECURSION-OBJECT
               PERFORM POP-CALL-STACK
      D         DISPLAY "After POP-CALL-STACK:" WS-RECURSION-OBJECT
               MOVE WS-COMMAND-NAME TO WS-CURR-COMMAND
                PERFORM EVALUATE-CURRENT-COMMAND
           ELSE
               MOVE SPACES TO WS-COMMAND-NAME
           END-IF.
      D     display " ".
       PRINT-CALL-STACK.
           MOVE "PRINT-CALL-STACK" TO WS-RECURSION-FLAG.
           CALL "RECURSION" USING WS-RECURSION-FLAG.
       DEBUG-LISP.
           DISPLAY "WS-CURR-COMMAND:" WS-CURR-COMMAND.
           DISPLAY " WS-CURRENT-VALUE:" WS-CURRENT-VALUE.
           DISPLAY " WS-COMMAND-NAME:" WS-COMMAND-NAME.
           DISPLAY " WS-COMMAND-RESULT:" WS-COMMAND-RESULT.
           DISPLAY " ".
       POP-CALL-STACK.
      D     DISPLAY "POP-CALL-STACK:" WS-RECURSION-OBJECT.
           MOVE "POP-CALL-STACK" TO WS-RECURSION-FLAG.
           CALL "RECURSION" USING WS-RECURSION-FLAG,
           WS-RECURSION-OBJECT.
       CLOSE-CALL-STACK-PROCEDURE.
           MOVE "CLOSE" TO WS-RECURSION-FLAG.
           CALL "RECURSION" USING WS-RECURSION-FLAG.
       LOG-CURRENT-COMMAND-PROCEDURE.
      ******log Current Command To be Executed
           MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
           MOVE "LISP" TO
                WS-LOG-RECORD-FUNCTION-NAME.
           STRING 'Command:' DELIMITED BY SIZE
             WS-CURR-COMMAND DELIMITED BY SIZE
             INTO WS-LOG-RECORD-MESSAGE
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       EVALUATE-CURRENT-COMMAND.
           EVALUATE WS-CURR-COMMAND
           WHEN "print"
      D         DISPLAY "print"
               PERFORM LISP-PRINT-PROCEDURE
           WHEN "+"
      D         DISPLAY "add"
               PERFORM LISP-ADD-PROCEDURE
           WHEN OTHER
               PERFORM EVALUATE-CURRENT-VALUES.
       EVALUATE-CURRENT-VALUES.
      ************ Evalute values
           IF WS-CURR-COMMAND(1:LS-SYMBOL-LEN(WS-SYMBOL-TABLE-INDEX))
           IS NUMERIC THEN
               MOVE WS-CURR-COMMAND TO WS-CURRENT-VALUE-NUMERIC
           ELSE IF WS-CURR-COMMAND(1:1) = '"'
           AND WS-CURR-COMMAND(LS-SYMBOL-LEN(WS-SYMBOL-TABLE-INDEX):1)
           EQUALS '"' THEN
               MOVE WS-CURR-COMMAND TO WS-CURRENT-VALUE
           ELSE
      *****Command or value not interpreted.
      *****Throw an error and stop run
               MOVE "THROW-ERROR" TO WS-CISP-ERROR-FLAG
               MOVE "LISP FORMAT ERROR:" TO WS-ERROR-NAME
               STRING WS-CURR-COMMAND DELIMITED BY SPACE
                 " COULD NOT BE INTERPRETED." DELIMITED BY SIZE
                 INTO WS-ERROR-MESSAGE
               SET WS-ERROR-FATAL-YES TO TRUE
               CALL "CISP-ERROR" USING WS-CISP-ERROR-FLAG, WS-ERROR
           END-IF.
           PERFORM APPLY-VALUE-TO-EXPRESSION.
       APPLY-VALUE-TO-EXPRESSION.
           MOVE WS-COMMAND-NAME TO WS-CURR-COMMAND.
           PERFORM EVALUATE-CURRENT-COMMAND.
      D     DISPLAY "APPLY-VALUE-TO-EXPRESSION".
       LISP-PRINT-PROCEDURE.
      D    DISPLAY "LISP-PRINT-PROCEDURE"
           MOVE WS-CURRENT-VALUE TO WS-COMMAND-RESULT.
           DISPLAY WS-COMMAND-RESULT.
       LISP-ADD-PROCEDURE.
           IF WS-COMMAND-RESULT-NUMERIC EQUALS SPACES THEN
               MOVE 0 TO WS-COMMAND-RESULT-NUMERIC
           END-IF.
           ADD WS-CURRENT-VALUE-NUMERIC TO WS-COMMAND-RESULT-NUMERIC.
       LOG-COMMAND-EVALUTATION.
           MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
           STRING "LISP:" DELIMITED BY SIZE
            WS-CURR-COMMAND INTO
                WS-LOG-RECORD-FUNCTION-NAME.
           STRING 'Result:' DELIMITED BY SIZE
             WS-COMMAND-RESULT DELIMITED BY SIZE
             INTO WS-LOG-RECORD-MESSAGE
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       END PROGRAM LISP.
