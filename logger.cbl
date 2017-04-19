      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGGER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL LOG-FILE ASSIGN TO DYNAMIC LOG-FILE-NAME
               ORGANISATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.
       FD LOG-FILE.
           01 LOG-RECORD.
               02 LOG-RECORD-ID PIC 9(10).
               02 LOG-RECORD-FUNCTION-NAME PIC X(40).
               02 LOG-RECORD-MESSAGE PIC X(100).
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
           01 LS-LOG-RECORD.
               02 LS-LOG-RECORD-ID PIC 9(10).
               02 LS-LOG-RECORD-FUNCTION-NAME PIC X(40).
               02 LS-LOG-RECORD-MESSAGE PIC X(100).
       PROCEDURE DIVISION USING LS-LOG-RECORD.
       MAIN-PROCEDURE.
           DISPLAY "LOGGER".
           MOVE "HELLO" TO LS-LOG-RECORD-MESSAGE.
           EXIT PROGRAM.
       LOG-INIT-PROCEDURE.
           MOVE '..\logs\log.data' TO LOG-FILE-NAME.
           OPEN OUTPUT LOG-FILE.
           MOVE 1 TO LOG-RECORD-ID.
           MOVE "LOG-INIT-PROCEDURE" TO LOG-RECORD-FUNCTION-NAME.
           MOVE "Starting Program!" TO LOG-RECORD-MESSAGE.
           WRITE LOG-RECORD.
       LOG-WRITE-TO-PROCEDURE.
           ADD 1 TO LOG-RECORD-ID.
      *     MOVE WS-LOG-RECORD-FUNCTION-NAME TO LOG-RECORD-FUNCTION-NAME.
      *     MOVE WS-LOG-RECORD-MESSAGE TO LOG-RECORD-MESSAGE.
           WRITE LOG-RECORD.
       END PROGRAM LOGGER.
