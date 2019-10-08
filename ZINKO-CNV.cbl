      ******************************************************************
      *  opencobol SAMPLE
      *
      *  Copyright 2019 Tokyo System House Co., Ltd.
      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 ZINKO-CNV.
       AUTHOR.                     TSH.
       DATE-WRITTEN.               2019-10-10.
      ******************************************************************
       ENVIRONMENT                 DIVISION.
      ******************************************************************
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT ZINKO-S-FILE     ASSIGN TO "ZINKO-SEQ"
                                   ORGANIZATION   IS   LINE SEQUENTIAL
                                   FILE   STATUS  IS   F-STATUS.
           SELECT ZINKO-I-FILE     ASSIGN TO "ZINKO-IDX"
                                   ORGANIZATION   IS   INDEXED
                                   ACCESS MODE    IS   DYNAMIC
                                   RECORD KEY     IS   Z-I-CODE
                                   FILE   STATUS  IS   F-STATUS.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       FILE                        SECTION.
       FD  ZINKO-S-FILE.
       01  ZINKO-S-REC.
           03   Z-S-CODE           PIC  X(07).
           03   Z-S-NAME           PIC  N(04).
           03   Z-S-ZINKO          PIC  9(08).
       FD  ZINKO-I-FILE.
       01  ZINKO-I-REC.
           03   Z-I-CODE           PIC  X(07).
           03   Z-I-NAME           PIC  N(04).
           03   Z-I-ZINKO          PIC  9(08).
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01  F-STATUS                PIC  XX.
      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           OPEN INPUT  ZINKO-S-FILE.
           OPEN OUTPUT ZINKO-I-FILE.
       READ-RTN.
           MOVE "00" TO F-STATUS.
           PERFORM UNTIL F-STATUS <> "00"
              READ ZINKO-S-FILE NEXT
              IF F-STATUS = "10" THEN
                 NEXT SENTENCE
              ELSE IF F-STATUS <> "00" THEN
                 DISPLAY "READ ERROR:" F-STATUS
                 MOVE -1 TO RETURN-CODE
                 GOBACK
              END-IF
              END-IF
              MOVE ZINKO-S-REC TO ZINKO-I-REC
              WRITE ZINKO-I-REC
           END-PERFORM.
       READ-EXT.
           CLOSE ZINKO-S-FILE.
           CLOSE ZINKO-I-FILE.
       MAIN-EXT.
           GOBACK.
