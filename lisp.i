#line 1 "lisp.cbl"






 IDENTIFICATION DIVISION.
 PROGRAM-ID. LISP.
 DATA DIVISION.
 FILE SECTION.
 WORKING-STORAGE SECTION.
 01 WS-SYMBOL-TABLE-INDEX PIC 9(4).
 01 WS-CURR-COMMAND PIC X(100).



 01 WS-LOG-OPERATION-FLAG PIC X(5).
 01 WS-LOG-RECORD.
 02 WS-LOG-RECORD-FUNCTION-NAME PIC X(40).
 02 WS-LOG-RECORD-MESSAGE PIC X(100).



 01 WS-RECURSION-FLAG PIC X(30).
 01 WS-RECURSION-OBJECT.
 02 WS-COMMAND-NAME PIC X(20).
 02 WS-COMMAND-RESULT PIC X(20).
 02 WS-COMMAND-RESULT-NUMERIC
 REDEFINES WS-COMMAND-RESULT PIC 9(20).
 02 WS-COMMAND-RETURNS-RESULT PIC X.
 88 WS-COMMAND-RETURNS-RESULT-YES VALUE 'Y' FALSE 'N'.
 LINKAGE SECTION.
 01 LS-LISP-SYMBOLS.
 02 LS-SYMBOL-TABLE-SIZE PIC 9.
 02 LS-SYMBOL PIC X(50) OCCURS 40 TIMES.
 02 LS-SYMBOL-LEN PIC 9(2) OCCURS 40 TIMES.
 PROCEDURE DIVISION USING LS-LISP-SYMBOLS.
 MAIN-PROCEDURE.
 DISPLAY "LISP".
 PERFORM INIT-CALL-STACK-PROCEDURE.

 PERFORM VARYING WS-SYMBOL-TABLE-INDEX FROM 1 BY 1 UNTIL
 WS-SYMBOL-TABLE-INDEX > LS-SYMBOL-TABLE-SIZE
 EVALUATE LS-SYMBOL(WS-SYMBOL-TABLE-INDEX)
 WHEN "(" 
 DISPLAY "Open paren" 
 WHEN ")" 
 DISPLAY "closed paren" 
 MOVE "REMOVE-FROM-CALL-STACK" TO WS-RECURSION-FLAG

 WHEN OTHER
 MOVE LS-SYMBOL(WS-SYMBOL-TABLE-INDEX)
 TO WS-CURR-COMMAND
 PERFORM LOG-CURRENT-COMMAND-PROCEDURE
 PERFORM EVALUATE-CURRENT-COMMAND
 END-PERFORM.
 MOVE "PRINT" TO WS-RECURSION-FLAG.
 CALL "RECURSION" USING WS-RECURSION-FLAG.
 PERFORM CLOSE-CALL-STACK-PROCEDURE.
 GOBACK.
 INIT-CALL-STACK-PROCEDURE.

 MOVE "INIT" TO WS-RECURSION-FLAG.
 CALL "RECURSION" USING WS-RECURSION-FLAG.
 INIT-RECURSION-OBJECT-PROCEDURE.
 IF WS-COMMAND-NAME = SPACES THEN
 DISPLAY "NO PREVIOUS OBJECT" 
 MOVE WS-CURR-COMMAND TO WS-COMMAND-NAME
 ELSE

 DISPLAY "PREVIOUS OBJECT" 
 MOVE "ADD-TO-CALL-STACK" TO WS-RECURSION-FLAG
 CALL "RECURSION" USING WS-RECURSION-FLAG
 WS-RECURSION-OBJECT

 MOVE WS-CURR-COMMAND TO WS-COMMAND-NAME
 END-IF.
 CLOSE-CALL-STACK-PROCEDURE.
 MOVE "CLOSE" TO WS-RECURSION-FLAG.
 CALL "RECURSION" USING WS-RECURSION-FLAG.
 LOG-CURRENT-COMMAND-PROCEDURE.

 MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
 MOVE "LISP" TO
 WS-LOG-RECORD-FUNCTION-NAME.
 STRING 'Command:' DELIMITED BY SIZE
 WS-CURR-COMMAND DELIMITED BY SIZE
 INTO WS-LOG-RECORD-MESSAGE
 CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG WS-LOG-RECORD.
 EVALUATE-CURRENT-COMMAND.
 EVALUATE WS-CURR-COMMAND
 WHEN "write" 
 DISPLAY "write" 
 PERFORM INIT-RECURSION-OBJECT-PROCEDURE

 WHEN "+" 
 DISPLAY "+" 
 PERFORM INIT-RECURSION-OBJECT-PROCEDURE





 WHEN OTHER
 DISPLAY "other" 


 IF WS-CURR-COMMAND(1:LS-SYMBOL-LEN(WS-SYMBOL-TABLE-INDEX))
 IS NUMERIC THEN

 MOVE WS-CURR-COMMAND TO WS-COMMAND-RESULT-NUMERIC
 DISPLAY "NUMERIC" 
 DISPLAY WS-COMMAND-RESULT-NUMERIC

 ELSE
 Display "OTHER" 
 END-IF
 .


 OTHER-PROCEDURES.
































 MOVE 'EVALUATE-CURRENT-COMMAND' 
 TO WS-LOG-RECORD-FUNCTION-NAME




















 .









 END PROGRAM LISP.
