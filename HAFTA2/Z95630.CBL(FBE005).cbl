       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FBEG005.
       AUTHOR.        Kadir Kaan Goc.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN  TO  PRTLINE
                             STATUS      PRT-ST.
           SELECT ACCT-REC   ASSIGN  TO  ACCTREC
                             STATUS      ACCT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05  PRINT-SEQ           PIC X(04).
           05  PRINT-AD            PIC X(15).
           05  PRINT-SOYAD         PIC X(15).
           05  P-BIRTDAY           PIC X(11).
           05  PRINT-DTAR          PIC 9(08).
           05  P-TODAY             PIC X(09).
           05  PRINT-TODAY         PIC 9(08).
           05  P-FARK              PIC X(08).
           05  PRINT-FARK          PIC 9(05).
      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-SEQ            PIC X(04).
           05  ACCT-AD             PIC X(15).
           05  ACCT-SOYAD          PIC X(15).
           05  ACCT-DTAR           PIC 9(08).
           05  ACCT-TODAY          PIC 9(08).
      *
       WORKING-STORAGE SECTION.
       01 WS-WORK-AREA.
           05  PRT-ST              PIC 9(02).
               88  PRT-SUCCESS                VALUE 00 97.
           05  ACCT-ST             PIC 9(02).
               88  ACCT-EOF                   VALUE 10.
               88  ACCT-SUCCESS               VALUE 00 97.
           05  WS-INT-D            PIC 9(07).
           05  WS-INT-T            PIC 9(07).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS UNTIL ACCT-EOF.
           PERFORM H999-PROGRAM-EXIT.
       H100-OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
           READ ACCT-REC.
       H100-END. EXIT.

       H200-PROCESS.
           COMPUTE WS-INT-D = FUNCTION INTEGER-OF-DATE(ACCT-DTAR)
           COMPUTE WS-INT-T = FUNCTION INTEGER-OF-DATE(ACCT-TODAY)
           INITIALIZE PRINT-REC
           MOVE ACCT-SEQ      TO   PRINT-SEQ
           MOVE ACCT-AD       TO   PRINT-AD
           MOVE ACCT-SOYAD    TO   PRINT-SOYAD
           MOVE ACCT-DTAR     TO   PRINT-DTAR
           MOVE ACCT-TODAY    TO   PRINT-TODAY
           MOVE "BIRTDAY: "   TO   P-BIRTDAY
           MOVE " TODAY: "    TO   P-TODAY
           MOVE " FARK: "     TO   P-FARK
           COMPUTE PRINT-FARK = WS-INT-T - WS-INT-D
           WRITE PRINT-REC.
           READ ACCT-REC.
       H200-END. EXIT.
       H300-CLOSE-FILES.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
       H300-END. EXIT.
       H999-PROGRAM-EXIT.
           PERFORM H300-CLOSE-FILES.
           STOP RUN.

