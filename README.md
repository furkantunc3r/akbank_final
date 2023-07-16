# akbank_final
Final project of COBOL bootcamp

The aim of this project is to create one sub,
and one main program to do operations on a vsam file
which we created.

-> PBEG0006 is the main program and resposible for I/O operations and calling the sub program

-> PBEGIDX is the sub program and resposible for all the VSAM file related operations

## How it works

1. Create a QSAM file with desired entries from SORTEG02

2. Create the VSAM file with DELDEF01

3. Create the input file in which we will provide the desired operations from SORTEG03

4. Finally run the JBEG0006 and the operations will be done
