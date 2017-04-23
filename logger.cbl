      ******************************************************************
      * Author: Lauryn Brown
      * Date:
      * Purpose: log activity done by other programs
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGGER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL LOG-FILE ASSIGN TO DYNAMIC WS-LOG-FILE-NAME
               ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD LOG-FILE.
           01 LOG-RECORD.
               02 LOG-RECORD-ID PIC 9(10).
               02 LOG-RECORD-FUNCTION-NAME PIC X(40).
               02 LOG-RECORD-MESSAGE PIC X(100).
       WORKING-STORAGE SECTION.
           01 WS-LOG-FILE-NAME PIC X(20).
       LINKAGE SECTION.
           01 LS-LOG-OPERATION-FLAG PIC X(5).
           01 LS-LOG-RECORD.
               02 LS-LOG-RECORD-FUNCTION-NAME PIC X(40).
               02 LS-LOG-RECORD-MESSAGE PIC X(100).
       PROCEDURE DIVISION USING LS-LOG-OPERATION-FLAG, LS-LOG-RECORD.
       MAIN-PROCEDURE.
           EVALUATE LS-LOG-OPERATION-FLAG
           WHEN "OPEN"
               PERFORM LOG-INIT-PROCEDURE
           WHEN "CLOSE"
               PERFORM LOG-CLOSE-PROCEDURE
           WHEN "ADD"
               PERFORM LOG-WRITE-TO-PROCEDURE
           WHEN OTHER
               PERFORM LOG-FLAG-ERROR-PROCEDURE.
           GOBACK.
       LOG-INIT-PROCEDURE.
           MOVE '..\logs\log.data' TO WS-LOG-FILE-NAME.
           OPEN OUTPUT LOG-FILE.
           MOVE 1 TO LOG-RECORD-ID.
           MOVE "LOG-INIT-PROCEDURE" TO LOG-RECORD-FUNCTION-NAME.
           MOVE "Starting Program!" TO LOG-RECORD-MESSAGE.
           WRITE LOG-RECORD.
       LOG-WRITE-TO-PROCEDURE.
           ADD 1 TO LOG-RECORD-ID.
           MOVE LS-LOG-RECORD-FUNCTION-NAME TO LOG-RECORD-FUNCTION-NAME.
           MOVE LS-LOG-RECORD-MESSAGE TO LOG-RECORD-MESSAGE.
           WRITE LOG-RECORD.
       LOG-FLAG-ERROR-PROCEDURE.
           DISPLAY "READ FLAG ERROR".
       LOG-CLOSE-PROCEDURE.
           ADD 1 TO LOG-RECORD-ID.
           MOVE "LOGGER:LOG-CLOSE-PROCEDURE"
             TO LOG-RECORD-FUNCTION-NAME.
           MOVE "Closed logging file" TO LOG-RECORD-MESSAGE.
           WRITE LOG-RECORD.
           CLOSE LOG-FILE.
       END PROGRAM LOGGER.
