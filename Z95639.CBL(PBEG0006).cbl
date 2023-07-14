       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PBEG0006.
       AUTHOR.        FURKAN TUNCER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE ASSIGN OUTFILE
                           STATUS OUT-ST.
           SELECT INP-FILE ASSIGN INPFILE
                           STATUS INP-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05 OUT-ID            PIC 9(05).
           05 OUT-DVZ           PIC 9(03).
           05 OUT-ISLEM-TIPI    PIC X(01).
           05 OUT-RETURN-CODE   PIC 9(02).
           05 OUT-ACIKLAMA      PIC X(30).
           05 OUT-FNAME-FROM    PIC X(15).
           05 OUT-FNAME-TO      PIC X(15).
           05 OUT-LNAME-FROM    PIC X(15).
           05 OUT-LNAME-TO      PIC X(15).
      *
       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           05 INP-ISLEM-TIPI    PIC X(01).
           05 INP-ID            PIC X(05).
           05 INP-DVZ           PIC X(03).
      *
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 WS-PBEGIDX        PIC X(08)   VALUE 'PBEGIDX'.
           05 OUT-ST            PIC 9(02).
              88 OUT-SUCCESS                VALUE 00 97.
           05 INP-ST            PIC 9(02).
              88 INP-EOF                    VALUE 10.
              88 INP-SUCCESS                VALUE 00 97.
           05 WS-ISLEM-TIPI     PIC X(01).
              88 WS-ISLEM-TIPI-VALID        VALUE 'R' 'W' 'U' 'D'.
           05 WS-SUB-AREA.
              07 WS-SUB-FUNC    PIC X(01).
                 88 WS-FUNC-READ            VALUE 'R'.
                 88 WS-FUNC-UPDATE          VALUE 'U'.
                 88 WS-FUNC-WRITE           VALUE 'W'.
                 88 WS-FUNC-DELETE          VALUE 'D'.
              07 WS-SUB-ID      PIC 9(05).
              07 WS-SUB-DVZ     PIC 9(03).
              07 WS-SUB-RC      PIC 9(02).
              07 WS-SUB-DATA    PIC X(84).
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS UNTIL INP-EOF.
           PERFORM H999-PROGRAM-EXIT.
       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           IF NOT INP-SUCCESS
              DISPLAY 'UNABLE TO OPEN FILE: ' INP-ST
              MOVE INP-ST TO RETURN-CODE
              PERFORM H999-PROGRAM-EXIT
           END-IF.
           OPEN OUTPUT OUT-FILE.
           IF NOT OUT-SUCCESS
              DISPLAY 'UNABLE TO OPEN FILE: ' OUT-ST
              MOVE OUT-ST TO RETURN-CODE
              PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ INP-FILE.
           IF NOT INP-SUCCESS
              DISPLAY 'UNABLE TO READ FILE: ' INP-ST
              MOVE INP-ST TO RETURN-CODE
              PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.
      *EVALUATE AND SET TO TRUE FOR CALL OPERATION
       H200-PROCESS.
           MOVE INP-ISLEM-TIPI TO WS-ISLEM-TIPI
           IF WS-ISLEM-TIPI-VALID
              EVALUATE WS-ISLEM-TIPI
                 WHEN 'R'
                    SET WS-FUNC-READ   TO TRUE
                 WHEN 'U'
                    SET WS-FUNC-UPDATE TO TRUE
                 WHEN 'W'
                    SET WS-FUNC-WRITE  TO TRUE
                 WHEN 'D'
                    SET WS-FUNC-DELETE TO TRUE
               END-EVALUATE
               MOVE INP-ID             TO WS-SUB-ID
               MOVE INP-DVZ            TO WS-SUB-DVZ
               MOVE ZEROS              TO WS-SUB-RC
               MOVE SPACES             TO WS-SUB-DATA
               CALL WS-PBEGIDX         USING WS-SUB-AREA
           ELSE
               MOVE INP-ID             TO WS-SUB-ID
               MOVE INP-DVZ            TO WS-SUB-DVZ
               MOVE ZEROS              TO WS-SUB-RC
               MOVE 'INVALID OPERATE'  TO WS-SUB-DATA
           END-IF.
           PERFORM H300-PREP-OUTPUT.
           READ    INP-FILE.
       H200-END. EXIT.
      *DISPLAY THE OUTPUT WITH THE INCOMING DATA
       H300-PREP-OUTPUT.
           STRING WS-SUB-ID
                  WS-SUB-DVZ '-'
                  WS-ISLEM-TIPI '-' 'RC:'
                  WS-SUB-RC '-'
                  WS-SUB-DATA
                  DELIMITED BY SIZE INTO OUT-REC
           END-STRING.
           WRITE OUT-REC
           END-WRITE.
       H300-END. EXIT.
      *
       H999-PROGRAM-EXIT.
           CLOSE OUT-FILE
                 INP-FILE.
           STOP RUN.
       H999-END. EXIT.
