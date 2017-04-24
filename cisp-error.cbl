      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CISP-ERROR.
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
      *    WS Shared with RECUSRION SubRoutine
      *****************************************
       01 WS-RECURSION-FLAG PIC X(30).
       LINKAGE SECTION.
       01 LS-CISP-ERROR-FLAG PIC X(30).
       01 LS-ERROR.
          02 LS-ERROR-NAME PIC X(40).
          02 LS-ERROR-FATAL PIC X.
           88 LS-ERROR-FATAL-YES VALUE 'Y', FALSE 'N'.
          02 LS-ERROR-MESSAGE PIC X(100).
       PROCEDURE DIVISION USING LS-CISP-ERROR-FLAG, LS-ERROR.
       MAIN-PROCEDURE.
           EVALUATE LS-CISP-ERROR-FLAG
           WHEN "THROW-ERROR"
           PERFORM THROW-ERROR-PROCEDURE.
       THROW-ERROR-PROCEDURE.
           DISPLAY LS-ERROR-NAME.
           DISPLAY LS-ERROR-MESSAGE.
           IF LS-ERROR-FATAL-YES THEN
               PERFORM END-CISP-PROCEDURE
           END-IF.
       END-CISP-PROCEDURE.
           PERFORM LOG-ERROR-PROCEDURE.
           PERFORM CLOSE-OPEN-FILES-PROCEDURE.
           STOP RUN.
       LOG-ERROR-PROCEDURE.
      ******log error
           MOVE "ADD" TO WS-LOG-OPERATION-FLAG.
           MOVE "CISP-ERROR" TO
                WS-LOG-RECORD-FUNCTION-NAME.
           STRING LS-ERROR-NAME DELIMITED BY SIZE
             ":" DELIMITED BY SIZE
             LS-ERROR-MESSAGE DELIMITED BY SIZE
             "Fatal:" DELIMITED BY SIZE
             LS-ERROR-FATAL DELIMITED BY SIZE
             INTO WS-LOG-RECORD-MESSAGE.
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       CLOSE-OPEN-FILES-PROCEDURE.
           MOVE "STACK-FILE-STATUS" TO WS-RECURSION-FLAG.
           CALL "RECURSION" USING WS-RECURSION-FLAG.
           DISPLAY "RECURSION:" WS-RECURSION-FLAG.
           IF WS-RECURSION-FLAG = "Y" THEN
               MOVE "CLOSE" TO WS-RECURSION-FLAG
               CALL "RECURSION" USING WS-RECURSION-FLAG
           END-IF.
           MOVE "CLOSE" TO WS-LOG-OPERATION-FLAG.
           CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD.
       END PROGRAM CISP-ERROR.
