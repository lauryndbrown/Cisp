      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISP.
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
       LINKAGE SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "LISP"
            GOBACK.
       LISP-WRITE-PROCEDURE.
      *     PERFORM RECURSION-PROCEDURE.
       LISP-ADD-PROCEDURE.
      *     IF COMMAND-RESULT-NUMERIC EQUALS SPACES THEN
      *         MOVE 0 TO COMMAND-RESULT-NUMERIC
      *     END-IF.
      *     ADD COMMAND-RESULT-NUMERIC TO WS-COMMAND-RESULT-NUMERIC.
      *     IF WS-OPEN-PAREN-YES THEN
      *         PERFORM RECURSION-PROCEDURE
      *     END-IF.
       PRINT-SYMBOL-TABLE.
      *     MOVE 1 TO WS-COUNT.
      *     PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL
      *     WS-COUNT GREATER THAN WS-SYMBOL-LENGTH
      *         DISPLAY WS-COUNT
      *         DISPLAY WS-SYMBOL(WS-COUNT)
      *     END-PERFORM.
       EVALUATE-LISP-PRCEDURE.
      *     PERFORM INIT-CALL-STACK-PROCEDURE.
      *     PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL
      *     WS-COUNT = WS-SYMBOL-TABLE-SIZE
      *         PERFORM PARSE-STRING-PROCEDURE
      *         DISPLAY "AFTER PARSE-STRING-PROCEDURE IN"
      *         " EVALUATE-LISP-PRCEDURE."
      *         DISPLAY "COUNT:" WS-COUNT
      *         " SYMBOL:" WS-SYMBOL(WS-COUNT) "$"
      *         " START:" WS-PARSE-EXPRESSION-START
      *         " LEN:" WS-PARSE-EXPRESSION-LEN
      *         " END:" WS-PARSE-EXPRESSION-END
      *         MOVE WS-SYMBOL(WS-COUNT)
      *         (WS-PARSE-EXPRESSION-START:WS-PARSE-EXPRESSION-LEN)
      *         TO WS-CURR-COMMAND
      ******log Current Command To be Executed
               MOVE 'EVALUATE-LISP-PRCEDURE'
               TO WS-LOG-RECORD-FUNCTION-NAME
      *         STRING 'Command:' DELIMITED BY SIZE
      *         WS-CURR-COMMAND DELIMITED BY SIZE
      *         INTO WS-LOG-RECORD-MESSAGE
      *         PERFORM LOG-WRITE-TO-PROCEDURE
      ************
      *         PERFORM EVALUATE-CURRENT-COMMAND
      *     END-PERFORM.
      ********** Logging Completed Executing lisp
           MOVE 'EVALUATE-LISP-PRCEDURE' TO WS-LOG-RECORD-FUNCTION-NAME
           MOVE "COMPLETED: EVALUATING LISP" TO WS-LOG-RECORD-MESSAGE
      *     PERFORM LOG-WRITE-TO-PROCEDURE
      ***************
      *     PERFORM CLOSE-CALL-STACK-PROCEDURE.
      * EVALUATE-CURRENT-COMMAND.
      ********** Logging Completed Executing lisp
           MOVE 'EVALUATE-CURRENT-COMMAND'
            TO WS-LOG-RECORD-FUNCTION-NAME
      *     MOVE WS-CURR-COMMAND
      *      TO WS-LOG-RECORD-MESSAGE
      *     PERFORM LOG-WRITE-TO-PROCEDURE
      ***************
      *     EVALUATE WS-CURR-COMMAND
      *     WHEN "write"
      *         PERFORM LISP-WRITE-PROCEDURE
      *     WHEN "+"
      *         IF WS-OPEN-PAREN-YES THEN
      *             MOVE 0 TO WS-COMMAND-RESULT-NUMERIC
      *         END-IF
      *         PERFORM LISP-ADD-PROCEDURE
      *     WHEN OTHER
      *         IF WS-CURR-COMMAND(1:WS-PARSE-EXPRESSION-LEN) IS NUMERIC THEN

      *             MOVE WS-CURR-COMMAND TO WS-COMMAND-RESULT-NUMERIC
      *             PERFORM LISP-EVAL-LAST-EXPRESSION
      *         ELSE
      *             Display "OTHER"
      *         END-IF
           .
      *     IF WS-CLOSE-PAREN-YES THEN
      **************Logging Closed Paren found
      *         MOVE 'EVALUATE-CURRENT-COMMAND'
      *           TO WS-LOG-RECORD-FUNCTION-NAME
      *         MOVE 'Found Closed Paren' TO WS-LOG-RECORD-MESSAGE
      *         PERFORM LOG-WRITE-TO-PROCEDURE
      ****************
      *         PERFORM CALL-STACK-DELETE-PROCEDURE
      *     END-IF.
       END PROGRAM LISP.
