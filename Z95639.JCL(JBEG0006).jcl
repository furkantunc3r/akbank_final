//JBEG0006 JOB 1,NOTIFY=&SYSUID
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(PBEGIDX),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(PBEGIDX),DISP=SHR
// IF RC < 5 THEN
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(PBEG0006),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(PBEG0006),DISP=SHR
//LKED.SYSLIB  DD DSN=&SYSUID..LOAD(PBEGIDX),DISP=SHR
//DELET100 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
   DELETE Z95639.QSAM.OUTFILE NONVSAM
   IF LASTCC LE 08 THEN SET MAXCC = 00
// IF RC < 5 THEN
//RUN     EXEC PGM=PBEG0006
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTREC   DD DSN=&SYSUID..VSAM.AA,DISP=SHR
//INPFILE   DD DSN=&SYSUID..QSAM.INP,DISP=SHR
//OUTFILE   DD DSN=&SYSUID..QSAM.OUTFILE,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(5,5),RLSE),
//             DCB=(RECFM=FB,LRECL=101,BLKSIZE=0)
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
// ELSE
// ENDIF
// ELSE
// ENDIF
