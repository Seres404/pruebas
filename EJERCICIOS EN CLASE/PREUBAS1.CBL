      ******************************************************************
      * FECHA       : 27/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34017                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE ES UN PROGRAMA QUE GENERA VALIDA Y GENERA UN*
      *             : REPORTE DE VENTAS PARA UN NEGOCIO, ESTE TIENE DOS*
      *             : SALIDAS, UNA DE ELLAS ES UN REPORTE DETALLADO DE *
      *             : LAS VENTAS REALIZADAS Y OTRO MÁS SENCILLO EN EL  *
      *             : QUE SE ORDENAN DE FORMA ASCENDENTE LOS REGISTROS *
      *             : DE CADA VENTA REALIZADA                          *
      * ARCHIVOS    : EDUC.EDU34017.ENTRADA.UNO                        *
      * ARCHIVOS    : EDUC.EDU34017.SALIDA.UNO                         *
      * ARCHIVOS    : EDUC.EDU34017.SALIDA.DOS                         *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU34016.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE           ASSIGN TO SALES
                                       FILE STATUS IS FS-SALES.
           SELECT SUMMARY-REPORT       ASSIGN TO SUMREPO
                                       FILE STATUS IS FS-REPORT.
           SELECT SORTED-FILE          ASSIGN TO SORTED
                                       FILE STATUS IS FS-SORTED.
           SELECT WORK-FILE            ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
       FD SALES-FILE.
       01 SALES-REC.
          88 END-OF-SALES-FILE                        VALUE HIGH-VALUES.
          02 SF-CUST-ID             PIC X(5).
          02 SF-CUST-NAME           PIC X(20).
          02 SF-OIL-ID.
             03 FILLER              PIC X.
                88 ESSENTIAL-OIL                       VALUE "E".
             03 SF-OIL-NAME         PIC 99.
          02 SF-UNIT-SIZE           PIC 99.
          02 SF-UNITS-SOLD          PIC 999.
       SD WORK-FILE.
       01 WORK-REC.
          88 END-OF-WORK-FILE                         VALUE HIGH-VALUES.
          02 WF-CUST-ID             PIC X(5).
          02 WF-CUST-NAME           PIC X(20).
          02 WF-OIL-ID.
             03 FILLER              PIC X.
             03 WF-OIL-NUM          PIC 99.
          02 WF-UNIT-SIZE           PIC 99.
          02 WF-UNITS-SOLD          PIC 999.
       FD SUMMARY-REPORT.
       01 PRINT-LINE                PIC X(64).
       FD SORTED-FILE.
       01 SORTED-REC                PIC X(33).
       WORKING-STORAGE SECTION.
       01 WKS-OILS-TABLE.
          02 WKS-OIL-COST-VALUES.
             03 FILLER              PIC X(40)
                                                       VALUE
                             "0041003200450050002910250055003900650075".
             03 FILLER              PIC X(40)
                                                       VALUE
                             "0080004400500063006500550085004812500065".
             03 FILLER              PIC X(40)
                                                       VALUE
                             "0060005500670072006501250085006511150105".
          02 FILLER REDEFINES WKS-OIL-COST-VALUES.
             03 WKS-OIL-COST        PIC 99V99 OCCURS 30 TIMES.
       01 WKS-REPORT-HEADING-LINE   PIC X(44)
                                                       VALUE
                         "              RESUMEN DE VENTAS DE AROMAMORA".

       01 WKS-REPORT-HEADING-UNDERLINE.
          02 FILLER                 PIC X(13)          VALUE SPACES.
          02 FILLER                 PIC X(32)          VALUE ALL "-".

       01 WKS-TOPIC-HEADING-LINE.
          02 FILLER                 PIC BX(13)
                                                       VALUE
                                                        "   CLIENTE   ".
          02 FILLER                 PIC X(08)          VALUE SPACES.
          02 FILLER                 PIC X(10)          VALUE
                                                           "CUST-ID   ".
          02 FILLER                 PIC X(8)           VALUE "VENTAS  ".
          02 FILLER                 PIC X(11)          VALUE
                                                          "CANTIDAD   ".
          02 FILLER                 PIC X(11)          VALUE
                                                          "TOTAL VENTA".
       01 WKS-CUST-SALES-LINE.
          02 WKS-PRN-CUST-NAME      PIC X(20).
          02 WKS-PRN-CUST-ID        PIC BBB9(5).
          02 WKS-PRN-CUST-SALES     PIC BBBBBZZ9.
          02 WKS-PRN-QTY-SOLD       PIC BBBBBZZ,ZZ9.
          02 WKS-PRN-SALES-VALUE    PIC BBBBQQQ,QQ9.99.

       01 WKS-TOTAL-SALES-LINE.
          02 FILLER                 PIC X(33)          VALUE SPACES.
          02 FILLER                 PIC X(19)          VALUE
                "TOTAL DE VENTAS   :".
          02 WKS-PRN-TOTAL-SALES    PIC BBBBBBZZ,ZZ9.


       01 WKS-TOTAL-QTY-SOLD-LINE.
          02 FILLER                 PIC X(33)          VALUE SPACES.
          02 FILLER                 PIC X(19)          VALUE
                "UNIDADES VENDIDAS :".
          2 WKS-PRN-TOTAL-QTY-SOLD  PIC BBBBBZZZ,ZZ9.

       01 WKS-TOTAL-SALES-VALUE-LINE.
          02 FILLER                 PIC X(33)          VALUE SPACES.
          02 FILLER                 PIC X(19)          VALUE
                "VALOR TOTAL VENTAS:".
          02 WKS-PRN-TOTAL-SALES-VALUE  PIC BQQQQ,QQ9.99.

       01 WKS-CUST-TOTALS.
          02 WKS-CUST-SALES         PIC 999.
          02 WKS-CUST-QTY-SOLD      PIC 9(5).
          02 WKS-CUST-SALES-VALUE   PIC 9(5)V99.

       01 WKS-FINAL-TOTALS.
          02 WKS-TOTAL-SALES        PIC 9(5)           VALUE ZEROS.
          02 WKS-TOTAL-QTY-SOLD     PIC 9(6)           VALUE ZEROS.
          02 WKS-TOTAL-SALES-VALUE  PIC 9(6)V99        VALUE ZEROS.

       01 WKS-TEMP-VARIABLES.
          02 WKS-SALE-QTY-SOLD      PIC 99999.
          02 WKS-VALUE-OF-SALE      PIC 999999V99.
          02 WKS-PREV-CUST-ID       PIC X(5).
       01 FS-SALES                  PIC 9(02)          VALUE ZEROS.
       01 FS-REPORT                 PIC 9(02)          VALUE ZEROS.
       01 FS-SORTED                 PIC 9(02)          VALUE ZEROS.
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATASET
           PERFORM 120-SORT-DATA
           PERFORM 150-CLOSE-DATASET
           STOP RUN.
       100-MAIN-E.            EXIT.

      *-----<  SECCION DE APARTURA DE ARCHIVOS
       110-OPEN-DATASET SECTION.
           OPEN INPUT SALES-FILE
           OPEN OUTPUT SUMMARY-REPORT
           OPEN OUTPUT SORTED-FILE
           IF (FS-SALES = 97) AND (FS-REPORT = 97) AND (FS-SORTED = 97)
              MOVE ZEROS TO FS-SALES
              MOVE ZEROS TO FS-REPORT
              MOVE ZEROS TO FS-SORTED
           END-IF

           EVALUATE TRUE
           WHEN FS-SALES NOT = 0
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY
                   "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY " FILE ESTATUS DEL ARCHIVO ES  : (" FS-SALES ")"
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                PERFORM 150-CLOSE-DATASET
                MOVE 91 TO RETURN-CODE
                STOP RUN
           WHEN FS-REPORT NOT = 0
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY
                   "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY " FILE ESTATUS DEL ARCHIVO ES  : (" FS-REPORT
                   ")"
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                PERFORM 150-CLOSE-DATASET
                MOVE 91 TO RETURN-CODE
                STOP RUN
           WHEN FS-SORTED NOT = 0
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY
                   "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY " FILE ESTATUS DEL ARCHIVO ES  : (" FS-SORTED
                   ")"
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                PERFORM 150-CLOSE-DATASET
                MOVE 91 TO RETURN-CODE
                STOP RUN
           WHEN OTHER
                DISPLAY
                   "********** APERTURA DE ARCHIVOS EXITOSA ********"
                   UPON CONSOLE
           END-EVALUATE.
       110-OPEN-DATASET-E.         EXIT.

      *------> SECCIÓN PARA EL ORDENAMIENTO Y LOGICA PRINCIPAL
       120-SORT-DATA SECTION.
           SORT WORK-FILE     ON ASCENDING WF-CUST-NAME
           INPUT PROCEDURE    IS  130-ESSENTIAL-OILS
           OUTPUT PROCEDURE   IS  140-SUMARY-REPORT.
       120-SORT-DATA-E.            EXIT.

      *------->
       130-ESSENTIAL-OILS SECTION.
           READ SALES-FILE
                AT END  SET END-OF-SALES-FILE TO TRUE
           END-READ

           PERFORM UNTIL END-OF-SALES-FILE
                   IF ESSENTIAL-OIL
                      RELEASE WORK-REC FROM SALES-REC
                   END-IF
                   READ SALES-FILE
                      AT END SET END-OF-SALES-FILE TO TRUE
                   END-READ
           END-PERFORM.
       130-ESSENTIAL-OILS-E. EXIT.

      *------->
       140-SUMARY-REPORT  SECTION.
           WRITE PRINT-LINE FROM WKS-REPORT-HEADING-LINE
           WRITE PRINT-LINE FROM WKS-REPORT-HEADING-UNDERLINE
           WRITE PRINT-LINE FROM WKS-TOPIC-HEADING-LINE

           RETURN WORK-FILE
           AT END
              SET END-OF-WORK-FILE TO TRUE
           END-RETURN.

           PERFORM 145-PRINT-CUSTOMER-LINES UNTIL END-OF-WORK-FILE
           MOVE  WKS-TOTAL-SALES      TO   WKS-PRN-TOTAL-SALES
           WRITE PRINT-LINE           FROM WKS-TOTAL-SALES-LINE
           MOVE WKS-TOTAL-QTY-SOLD    TO WKS-PRN-TOTAL-QTY-SOLD
           WRITE PRINT-LINE           FROM WKS-TOTAL-QTY-SOLD-LINE
           MOVE WKS-TOTAL-SALES-VALUE TO WKS-PRN-TOTAL-SALES-VALUE
           WRITE PRINT-LINE           FROM WKS-TOTAL-SALES-VALUE-LINE.
       140-SUMARY-REPORT-E.  EXIT.

       145-PRINT-CUSTOMER-LINES SECTION.
           MOVE ZEROS            TO WKS-CUST-TOTALS.
           MOVE WF-CUST-ID       TO WKS-PRN-CUST-ID, WKS-PREV-CUST-ID.
           MOVE WF-CUST-NAME     TO WKS-PRN-CUST-NAME.

           PERFORM UNTIL WF-CUST-ID NOT = WKS-PREV-CUST-ID
                   WRITE SORTED-REC FROM WORK-REC
                   ADD 1 TO WKS-CUST-SALES, WKS-TOTAL-SALES

                   COMPUTE
                   WKS-SALE-QTY-SOLD = WF-UNIT-SIZE * WF-UNITS-SOLD

                   ADD WKS-SALE-QTY-SOLD
                         TO WKS-CUST-QTY-SOLD, WKS-TOTAL-QTY-SOLD

                   COMPUTE WKS-VALUE-OF-SALE
                         = WKS-SALE-QTY-SOLD * WKS-OIL-COST(WF-OIL-NUM)

                   ADD WKS-VALUE-OF-SALE
                         TO WKS-CUST-SALES-VALUE, WKS-TOTAL-SALES-VALUE
                   RETURN WORK-FILE
                       AT END SET END-OF-WORK-FILE TO TRUE
                   END-RETURN
           END-PERFORM.

           MOVE WKS-CUST-SALES       TO WKS-PRN-CUST-SALES.
           MOVE WKS-CUST-QTY-SOLD    TO WKS-PRN-QTY-SOLD.
           MOVE WKS-CUST-SALES-VALUE TO WKS-PRN-SALES-VALUE.
           WRITE PRINT-LINE          FROM WKS-CUST-SALES-LINE.
       145-PRINT-CUSTOMER-LINES-E. EXIT.
       150-CLOSE-DATASET SECTION.
           CLOSE SALES-FILE
           CLOSE SUMMARY-REPORT
           CLOSE SORTED-FILE.
       150-CLOSE-DATASET-E.            EXIT.

           DISPLAY
           "  ID |                NOMBRE COMPLETO                   |"
           " EDAD | CUMPLEAÑOS |  GENERO |"
            ID  |                NOMBRE COMPLETO                   | EDAD | CUMPLEAÑOS |  GENERO |