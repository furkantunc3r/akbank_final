       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PBEGIDX.
       AUTHOR.        FURKAN TUNCER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-REC ASSIGN        ACCTREC
                           ORGANIZATION  INDEXED
                           ACCESS        RANDOM
                           RECORD        ACCT-KEY
                           STATUS        ACCT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-REC.
       01  ACCT-FIELDS.
           03 ACCT-KEY.
              05 ACCT-ID        PIC S9(05) COMP-3.
              05 ACCT-DVZ       PIC S9(03) COMP.
           03 ACCT-NAME         PIC X(15).
           03 ACCT-SURNAME      PIC X(15).
           03 ACCT-DATE         PIC S9(7)  COMP-3.
           03 FILLER            PIC X(8)   VALUE SPACES.
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 ACCT-ST           PIC 9(02).
              88 ACCT-EOF                   VALUE 10.
              88 ACCT-SUCCESS               VALUE 00 97.
           05 UPDATE-NAME       PIC X(15).
           05 I                 PIC 9(2).
           05 INSERT-POS        PIC 9(2)    VALUE 1.
           05 YYYYDDD           PIC 9(7).
           05 MMDDYYYY          PIC 9(8).
           05 OUT-DATE          PIC X(8).
           05 OLD-NAME          PIC X(15).
           05 OLD-SURNAME       PIC X(15).
       LINKAGE SECTION.
       01  LS-WORK-AREA.
           05 LS-SUB-FUNC    PIC X(01).
              88 LS-FUNC-READ               VALUE 'R'.
              88 LS-FUNC-UPDATE             VALUE 'U'.
              88 LS-FUNC-WRITE              VALUE 'W'.
              88 LS-FUNC-DELETE             VALUE 'D'.
           05 LS-SUB-ID      PIC 9(05).
           05 LS-SUB-DVZ     PIC 9(03).
           05 LS-SUB-RC      PIC 9(02).
           05 LS-SUB-DATA    PIC X(84).
       PROCEDURE DIVISION USING LS-WORK-AREA.
      *CALL THE REQUIRED STATEMENT ACCORDING TO THE 88 VARS
       000-MAIN.
           EVALUATE TRUE
              WHEN LS-FUNC-READ
                 PERFORM H200-READ
              WHEN LS-FUNC-UPDATE
                 PERFORM H300-UPDATE
              WHEN LS-FUNC-WRITE
                 PERFORM H400-WRITE
              WHEN LS-FUNC-DELETE
                 PERFORM H500-DELETE
           END-EVALUATE.
           PERFORM H999-PROGRAM-EXIT.
      *
       H100-OPEN-FILES.
           OPEN I-O ACCT-REC.
           IF NOT ACCT-SUCCESS
              MOVE 'UNABLE TO OPEN FILE: ' TO LS-SUB-DATA
              MOVE ACCT-ST                 TO LS-SUB-RC
              PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.
      *GENERAL READ
       H150-READ-FILE.
           MOVE LS-SUB-ID  TO ACCT-ID.
           MOVE LS-SUB-DVZ TO ACCT-DVZ.
           READ ACCT-REC KEY IS ACCT-KEY
              INVALID KEY
                 MOVE 'NO SUCH RECORD' TO LS-SUB-DATA
                 MOVE ACCT-ST          TO LS-SUB-RC
                 PERFORM H999-PROGRAM-EXIT.
           IF NOT ACCT-SUCCESS
                 MOVE 'UNABLE TO READ FILE: ' TO LS-SUB-DATA
                 MOVE ACCT-ST                 TO LS-SUB-RC
                 PERFORM H999-PROGRAM-EXIT
           END-IF.
       H150-END. EXIT.
      *READ OPERATION
       H200-READ.
           PERFORM H100-OPEN-FILES.
           PERFORM H150-READ-FILE.
           PERFORM H600-PREP-DATE.
           STRING 'RECORD READ-'
                   ACCT-NAME ' '
                   ACCT-SURNAME ' '
                   OUT-DATE
                   DELIMITED BY SIZE INTO LS-SUB-DATA
           END-STRING.
       H200-END. EXIT.
      *CHECK FOR NON-SPACE CHARACTERS AND WRITE THEM INTO A VARIABLE
      *CHANGE THE LETTERS WITH THE INSPECT COMMAND
      *THEN CHANGE THE NEW VALUE WITH THE READ VARIABLE
      *THEN REWRITE THE VSAM FILE
       H300-UPDATE.
           PERFORM H100-OPEN-FILES.
           PERFORM H150-READ-FILE.
           PERFORM H600-PREP-DATE.
           MOVE SPACES TO UPDATE-NAME.
           MOVE 1 TO INSERT-POS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF ACCT-NAME
              IF ACCT-NAME(I:1) NOT = ' '
                 MOVE ACCT-NAME(I:1) TO UPDATE-NAME(INSERT-POS:1)
                 COMPUTE INSERT-POS = INSERT-POS + 1
              END-IF
           END-PERFORM.
           MOVE ACCT-NAME    TO OLD-NAME.
           MOVE ACCT-SURNAME TO OLD-SURNAME.
           MOVE UPDATE-NAME  TO ACCT-NAME.
           INSPECT ACCT-SURNAME REPLACING
              ALL 'E' BY 'I'.
           INSPECT ACCT-SURNAME REPLACING
              ALL 'A' BY 'E'.
           REWRITE ACCT-FIELDS
           END-REWRITE.
           STRING 'RECORD UPDATED-'
                  OLD-NAME ' '
                  OLD-SURNAME ' TO '
                  ACCT-NAME ' '
                  ACCT-SURNAME ' '
                  DELIMITED BY SIZE INTO LS-SUB-DATA
           END-STRING.
       H300-END. EXIT.
      *ADD A NEW RECORD TO THE VSAM FILE
      *IF KEY ALREADY EXISTS RETURN DUPLICATE-22
       H400-WRITE.
           PERFORM H100-OPEN-FILES.
           MOVE LS-SUB-ID  TO ACCT-ID.
           MOVE LS-SUB-DVZ TO ACCT-DVZ.
           MOVE 'FURKAN'   TO ACCT-NAME.
           MOVE 'TUNCER'   TO ACCT-SURNAME.
           MOVE '19980823' TO ACCT-DATE.
           MOVE '19980823' TO OUT-DATE.
           WRITE ACCT-FIELDS
              INVALID KEY
                 MOVE 'DUPLICATE KEY' TO LS-SUB-DATA
                 MOVE ACCT-ST         TO LS-SUB-RC
                 PERFORM H999-PROGRAM-EXIT
           END-WRITE.
           STRING 'RECORD ADDED-'
                  ACCT-NAME ' '
                  ACCT-SURNAME
                  OUT-DATE
                  DELIMITED BY SIZE INTO LS-SUB-DATA
           END-STRING.
       H400-END. EXIT.
      *FIND THE RECORD AND DELETE
       H500-DELETE.
           PERFORM H100-OPEN-FILES.
           PERFORM H150-READ-FILE.
           PERFORM H600-PREP-DATE.
           DELETE ACCT-REC RECORD
           END-DELETE.
           STRING 'RECORD DELETED-'
                  ACCT-NAME ' '
                  ACCT-SURNAME ' '
                  OUT-DATE
                  DELIMITED BY SIZE INTO LS-SUB-DATA
           END-STRING.
       H500-END. EXIT.
      *CHANGE TO FORMAT OF DATE TO BE DISPLAYABLE
       H600-PREP-DATE.
           COMPUTE YYYYDDD = FUNCTION INTEGER-OF-DAY(ACCT-DATE).
           COMPUTE MMDDYYYY = FUNCTION DATE-OF-INTEGER(YYYYDDD).
           MOVE MMDDYYYY TO OUT-DATE.
       H600-END. EXIT.
      *CLOSE THE VSAM FILE AND GIVE CONTROL BACK TO THE MAIN PROG.
       H999-PROGRAM-EXIT.
           CLOSE ACCT-REC.
           GOBACK.
       H999-END. EXIT.
