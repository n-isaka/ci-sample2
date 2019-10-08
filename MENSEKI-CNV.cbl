      ******************************************************************
      *  opencobol SAMPLE
      *
      *  Copyright 2019 Tokyo System House Co., Ltd.
      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 MENSEKI-CNV.
       AUTHOR.                     TSH.
       DATE-WRITTEN.               2019-10-10.
      ******************************************************************
       ENVIRONMENT                 DIVISION.
      ******************************************************************
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT MENSEKI-S-FILE   ASSIGN TO "MENSEKI-SEQ"
                                   ORGANIZATION   IS   LINE SEQUENTIAL
                                   FILE   STATUS  IS   F-STATUS.
           SELECT MENSEKI-I-FILE   ASSIGN TO "MENSEKI-IDX"
                                   ORGANIZATION   IS   INDEXED
                                   ACCESS MODE    IS   DYNAMIC
                                   RECORD KEY     IS   M-I-CODE
                                   FILE   STATUS  IS   F-STATUS.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       FILE                        SECTION.
       FD  MENSEKI-S-FILE.
       01  MENSEKI-S-REC.
           03   M-S-CODE           PIC  X(07).
           03   M-S-NAME           PIC  N(04).
           03   M-S-MENSEKI        PIC  9(05).
       FD  MENSEKI-I-FILE.
       01  MENSEKI-I-REC.
           03   M-I-CODE           PIC  X(07).
           03   M-I-NAME           PIC  N(04).
           03   M-I-MENSEKI        PIC  9(05).
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01  F-STATUS                PIC  XX.
      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           OPEN INPUT  MENSEKI-S-FILE.
           OPEN OUTPUT MENSEKI-I-FILE.
       READ-RTN.
           MOVE "00" TO F-STATUS.
           PERFORM UNTIL F-STATUS <> "00"
              READ MENSEKI-S-FILE NEXT
              IF F-STATUS = "10" THEN
                 NEXT SENTENCE
              ELSE IF F-STATUS <> "00" THEN
                 DISPLAY "READ ERROR:" F-STATUS
                 MOVE -1 TO RETURN-CODE
                 GOBACK
              END-IF
              END-IF
              MOVE MENSEKI-S-REC TO MENSEKI-I-REC
              WRITE MENSEKI-I-REC
           END-PERFORM.
       READ-EXT.
           CLOSE MENSEKI-S-FILE.
           CLOSE MENSEKI-I-FILE.
       MAIN-EXT.
           GOBACK.
