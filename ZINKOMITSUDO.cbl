      ******************************************************************
      *  opencobol SAMPLE
      *
      *  Copyright 2019 Tokyo System House Co., Ltd.
      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 ZINKOMITSUDO.
       AUTHOR.                     TSH.
       DATE-WRITTEN.               2019-10-10.
      ******************************************************************
       ENVIRONMENT                 DIVISION.
      ******************************************************************
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT ZINKO-I-FILE     ASSIGN TO "ZINKO-IDX"
                                   ORGANIZATION   IS   INDEXED
                                   ACCESS MODE    IS   DYNAMIC
                                   RECORD KEY     IS   Z-I-CODE
                                   FILE   STATUS  IS   F-STATUS.
           SELECT MENSEKI-I-FILE   ASSIGN TO "MENSEKI-IDX"
                                   ORGANIZATION   IS   INDEXED
                                   ACCESS MODE    IS   DYNAMIC
                                   RECORD KEY     IS   M-I-CODE
                                   FILE   STATUS  IS   F-STATUS.
           SELECT SORT-TMP-FILE    ASSIGN TO "sort.dat".
           SELECT MITSUDO-FILE     ASSIGN TO "mitsudo.csv"
                                   ORGANIZATION   IS   LINE SEQUENTIAL
                                   FILE   STATUS  IS   F-STATUS.
           SELECT JOUI-FILE        ASSIGN TO "TOP5.txt"
                                   ORGANIZATION   IS   LINE SEQUENTIAL
                                   FILE   STATUS  IS   F-STATUS.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       FILE                        SECTION.
       FD  ZINKO-I-FILE.
       01  ZINKO-I-REC.
           03   Z-I-CODE           PIC  X(07).
           03   Z-I-NAME           PIC  N(04).
           03   Z-I-ZINKO          PIC  9(08).
       FD  MENSEKI-I-FILE.
       01  MENSEKI-I-REC.
           03   M-I-CODE           PIC  X(07).
           03   M-I-NAME           PIC  N(04).
           03   M-I-MENSEKI        PIC  9(05).
       SD  SORT-TMP-FILE.
       01  SORT-TMP-REC.
           03   ST-CODE            PIC  X(07).
           03   ST-NAME            PIC  X(08).
           03   ST-MITSUDO         PIC  9(08)V9(03).
       FD  MITSUDO-FILE.
       01  MITSUDO-REC.
           03   MITUDO-DATA        PIC  X(29).
       FD  JOUI-FILE.
       01  JOUI-REC.
           03   JOUI-NAME          PIC  N(04).
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01  F-STATUS                PIC  XX.
       01  S-STATUS                PIC  XX.
       01  ZIKOMITSUDO-DATA        PIC  9(08)V9(03).
       01  CNT                     PIC  9.
      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           SORT SORT-TMP-FILE ON DESCENDING ST-MITSUDO
              INPUT  PROCEDURE SORTIN-RTN  THRU SORTIN-EXT
              OUTPUT PROCEDURE SORTOUT-RNT THRU SORTOUT-EXT.
       MAIN-EXT.
           GOBACK.

       SORTIN-RTN.
           OPEN INPUT  ZINKO-I-FILE.
           OPEN INPUT  MENSEKI-I-FILE.


           MOVE "00" TO F-STATUS.
           MOVE SPACE TO Z-I-CODE.

           START ZINKO-I-FILE KEY IS > Z-I-CODE.

           PERFORM UNTIL F-STATUS <> "00"
              READ ZINKO-I-FILE NEXT
              IF F-STATUS = "10" THEN
                 NEXT SENTENCE
              ELSE IF F-STATUS <> "00" THEN
                 DISPLAY "READ ERROR:" F-STATUS
                 MOVE -1 TO RETURN-CODE
                 GOBACK
              END-IF
              END-IF
      *       DISPLAY ZINKO-I-REC
              MOVE Z-I-CODE TO M-I-CODE
              READ MENSEKI-I-FILE KEY IS M-I-CODE
      *       DISPLAY MENSEKI-I-REC
              COMPUTE ZIKOMITSUDO-DATA = Z-I-ZINKO / M-I-MENSEKI
      *       DISPLAY Z-I-CODE "," Z-I-NAME "," ZIKOMITSUDO-DATA
              INITIALIZE MITSUDO-REC
              MOVE Z-I-CODE TO ST-CODE
              MOVE Z-I-NAME TO ST-NAME
              MOVE ZIKOMITSUDO-DATA TO ST-MITSUDO
              RELEASE SORT-TMP-REC
           END-PERFORM.
           
           CLOSE ZINKO-I-FILE.
           CLOSE MENSEKI-I-FILE.
       SORTIN-EXT.

       SORTOUT-RNT.
           OPEN OUTPUT MITSUDO-FILE.
           OPEN OUTPUT JOUI-FILE.

           MOVE '"code","name","mitsudo"' TO MITUDO-DATA.
           WRITE MITSUDO-REC.

           MOVE "00" TO S-STATUS.
           MOVE 0 TO CNT.
           PERFORM UNTIL S-STATUS <> "00"
              RETURN SORT-TMP-FILE
              AT END
                 MOVE "10" TO S-STATUS
                 NEXT SENTENCE
              END-RETURN

              STRING     ST-CODE     DELIMITED BY SIZE
                         ","         DELIMITED BY SIZE
                         ST-NAME     DELIMITED BY SIZE
                         ","         DELIMITED BY SIZE
                         ST-MITSUDO(1:8)  DELIMITED BY SIZE
                         "."         DELIMITED BY SIZE
                         ST-MITSUDO(9:3)  DELIMITED BY SIZE
                         INTO        MITUDO-DATA
              WRITE MITSUDO-REC
              IF CNT < 4 THEN
                 MOVE ST-NAME TO JOUI-NAME
                 WRITE JOUI-REC
                 ADD 1 TO CNT
              END-IF
           END-PERFORM.

           CLOSE MITSUDO-FILE.
           CLOSE JOUI-FILE.
       SORTOUT-EXT.

