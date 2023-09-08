      ******************************************************************
      * FECHA       : 20/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33017                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA LEE LOS DATOS DE SYSIN LUEGO LOS   *
      *             : ORDENA POR TIPO DE CUENTA, CANAL Y VALOR VERIFICA*
      *             : SU ESTRUCTURA, LUEGO CON LOS DATOS VALIDADOS GE- *
      *             : NERA UN REPORTE DETALLANDO LOS TOTALES POR CANAL *
      * ARCHIVOS    : REPORTE (PS SALIDA REPORTE)                      *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     EDU33018.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT REPORTE  ASSIGN TO SYSOO7
                            FILE STATUS IS FS-REPORTE.
            SELECT WORKFILE ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
       FD REPORTE
       REPORT IS TRASACCION-CUENTAS.
       SD WORKFILE.
       01 WORK-REG.
          02 REGISTRO-SORT.
             04 TIPO-REGISTRO          PIC 9(02).
                88 ACUMULABLE                     VALUE 02.
                88 FIN-DE-ARCHIVO                 VALUE 09.
             04                        PIC X(01).
             04 TIPO-CUENTA            PIC 9(03).
             04                        PIC X(01).
             04 TIPO-CANAL             PIC 9(03).
             04                        PIC X(01).
             04 COMENTARIO-REGISTRO    PIC X(17).
             04                        PIC X(01).
             04 VALOR                  PIC 9(06)V99.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *        VARIABLES DE OPERACION, CONTADORES, DISPARADORES        *
      ******************************************************************
       01 WKS-CAMPOS-DE-TRABAJO.
          02 FS-REPORTE                PIC 9(02) VALUE ZEROS.
          02 WKS-REGISTROS-LEIDOS      PIC 9(04) VALUE ZEROS.
          02 WKS-REG-ERROR             PIC 9(04) VALUE ZEROS.
          02 WKS-FECHA.
              03 WKS-DIA               PIC 9(02) VALUE ZEROS.
              03 FILLER1               PIC X(01) VALUE '/'.
              03 WKS-MES               PIC 9(02) VALUE ZEROS.
              03 FILLER2               PIC X(01) VALUE '/'.
              03 WKS-ANIO              PIC 9(04) VALUE ZEROS.
          02 WKS-FIN-ARCHIVO           PIC 9(01).
              88 FIN-WORKFILE                    VALUE 1.
      ******************************************************************
      *                  MAQUETACION REPORTE DE SALIDA                 *
      ******************************************************************
       REPORT SECTION.
       RD  TRASACCION-CUENTAS
           CONTROLS ARE TIPO-CUENTA
           LINE LIMIT IS 85
           PAGE LIMIT IS 50
           HEADING 1
           FIRST DETAIL 7
           LAST DETAIL 42
           FOOTING 46.
      ******************************************************************
      *                     MAQUETACION PAGE HEADER                    *
      ******************************************************************
       01  TYPE IS PH.
           02 LINE 1.
              03 COLUMN   1            PIC X(22) VALUE
                 'BANCO INDUSTRIAL, S.A.'.
              03 COLUMN  28            PIC X(36) VALUE
                 'REPORTE TRANSACCIONES TIPO DE CUENTA'.
              03 COLUMN  68            PIC X(17) VALUE
                 'S E M I L L E R O'.
           02 LINE 2.
              03 COLUMN   1            PIC X(25) VALUE
                 'EDU33018    00.27042023.R'.
              03 COLUMN  31            PIC X(20) VALUE
                 'FECHA DEL MOVIMIENTO'.
              03 COLUMN  52            PIC X(10) SOURCE WKS-FECHA.
              03 COLUMN  73            PIC X(06) VALUE 'PAGINA'.
              03 COLUMN  82            PIC Z(04) SOURCE PAGE-COUNTER
                                                 IN TRASACCION-CUENTAS.
           02 LINE 3.
              03 COLUMN  01            PIC X(85) VALUE ALL '='.
           02 LINE 4.
              03 COLUMN  01            PIC X(33) VALUE
              "*  TIPO CUENTA    CANAL OPERACION".
              03 COLUMN  37            PIC X(49) VALUE
              "COMENTARIO TRANSACCION       VALOR  TRANSACCION *".
           02 LINE 5.
              03 COLUMN  01            PIC X(85) VALUE ALL '='.
      ******************************************************************
      *                     MAQUETACION LINEA DETALLE                  *
      ******************************************************************
       01  DETAILLINE TYPE IS DE.
           02 LINE +1.
              03 COLUMN   8            PIC 9(03) SOURCE TIPO-CUENTA.
              03 COLUMN  24            PIC 9(03) SOURCE TIPO-CANAL.
              03 COLUMN  37            PIC 9(17)
                                             SOURCE COMENTARIO-REGISTRO.
              03 COLUMN  71            PIC QQQ,QQ9.99 SOURCE  VALOR.
       01 DETALLE1    TYPE IS DE.
           02 LINE +0.
              03                       PIC ZZZ,ZZ9.99 SOURCE VALOR.
       01 DETALLE2    TYPE IS DE.
           02 LINE +0.
              03                       PIC ZZZ,ZZ9.99 SOURCE VALOR.
       01 DETALLE3    TYPE IS DE.
           02 LINE +0.
              03                       PIC ZZZ,ZZ9.99 SOURCE VALOR.
      ******************************************************************
      *                    MAQUETACION CONTROL HEADING                 *
      ******************************************************************
       01  CUENTAS  TYPE IS CH.
           02 LINE IS PLUS 1.
              03 COLUMN 24             PIC X(40) VALUE
              "========== CUENTAS MONETARIAS =========="
              PRESENT WHEN TIPO-CUENTA = 001.
           02 LINE IS PLUS 1.
              03 COLUMN 24             PIC X(43) VALUE
              "========== CUENTAS DE AHORROS =========="
              PRESENT WHEN TIPO-CUENTA = 002.
           02 LINE IS PLUS 1.
              03 COLUMN 24             PIC X(43) VALUE
              "========== TARJETA DE CREDITO =========="
              PRESENT WHEN TIPO-CUENTA = 003.
      ******************************************************************
      *                   MAQUETACION CONTROL FOOTING                  *
      ******************************************************************
        01 TCUENTA TYPE IS CF TIPO-CUENTA.
           02 LINE PLUS 1.
              03 COLUMN 67             PIC X(19) VALUE ALL "-".
           02 LINE PLUS 1.
              03 COLUMN 25             PIC X(19) VALUE
              'TOTAL TRANSACCIONES'.
              03 COLUMN 45             PIC X(18) VALUE
              "CUENTAS MONETARIAS" PRESENT WHEN TIPO-CUENTA = 001.
              03 COLUMN 45             PIC X(18) VALUE
              "CUENTAS DE AHORROS" PRESENT WHEN TIPO-CUENTA = 002.
              03 COLUMN 45             PIC X(18) VALUE
              "TARJETA DE CREDITO" PRESENT WHEN TIPO-CUENTA = 003.
              03 COLUMN 65             PIC X(01) VALUE "=".
              03 TA COLUMN 67          PIC QQQ,QQQ,QQ9.99
                                              SUM VALOR UPON DETAILLINE.
           02 LINE PLUS 1.
              03 COLUMN 25             PIC X(41) VALUE
              'TOTAL TRANSACCIONES DESDE CAJA          ='.
              03 TB COLUMN 67          PIC QQQ,QQQ,QQ9.99
                                                SUM VALOR UPON DETALLE1.
           02 LINE PLUS 1.
              03 COLUMN 25             PIC X(41) VALUE
              'TOTAL TRANSACCIONES DESDE INTERNET      ='.
              03 TC COLUMN 67          PIC QQQ,QQQ,QQ9.99
                                                SUM VALOR UPON DETALLE2.
           02 LINE PLUS 1.
              03 COLUMN 25             PIC X(41) VALUE
              'TOTAL TRANSACCIONES DESDE CAJEROS ATM   ='.
              03 TD COLUMN 67          PIC QQQ,QQQ,QQ9.99
                                                SUM VALOR UPON DETALLE3.
           02 LINE PLUS 1.
              03 COLUMN 01             PIC X(85) VALUE ALL SPACES.
            02 LINE PLUS 1.
              03 COLUMN 01             PIC X(85) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION PAGE FOOTING                   *
      ******************************************************************
       01  TYPE IS PF.
           02  LINE PLUS 0.
              03 COLUMN 1              PIC X(25) VALUE
              'FECHA Y HORA DE OPERACION'.
              03 COLUMN 27             PIC <99/<99/9999  FUNC MDATE.
              03 COLUMN 40             PIC 99,99,99      FUNC TIME.
              03 COLUMN 50             PIC X(16) VALUE
              'DATA-CENTRO S.A.'.
              03 COLUMN 73             PIC X(06)  VALUE 'PAGINA'.
              03 COLUMN 82             PIC Z(04) SOURCE PAGE-COUNTER IN
                                                 TRASACCION-CUENTAS.
           02 LINE PLUS 1.
              03 COLUMN 01             PIC X(85) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION REPORT FINAL                   *
      ******************************************************************
       01  TYPE IS RF.
           02 LINE IS PLUS 1.
              03 COLUMN 26             PIC X(35) VALUE
              "========== R E S U M E N ==========".
           02 LINE PLUS 1.
               03 COLUMN 17            PIC X(37) VALUE
               'TOTAL GENERAL TRANSACCIONES         ='.
               03 COLUMN 55            PIC QQQ,QQQ,QQ9.99 SUM TA.
           02 LINE PLUS 1.
               03 COLUMN 17            PIC X(37) VALUE
               "TOTAL REGISTROS PROCESADOS SYSIN    =".
               03 COLUMN 63            PIC ZZ,ZZZ COUNT OF DETAILLINE.
           02  LINE PLUS 1.
               03 COLUMN 17            PIC X(17) VALUE
                "DETALLES:".
               03 COLUMN 54            PIC X(16) VALUE ALL "-".

           02 LINE PLUS 1.
               03 COLUMN 17            PIC X(33) VALUE
               "TOTAL CAPTADO CANAL 1 CAJA       ".
               03 COLUMN 55            PIC QQQ,QQQ,QQ9.99 SUM TB.
           02 LINE PLUS 1.
               03 COLUMN 17            PIC X(33) VALUE
               "TOTAL CAPTADO CANAL 2 INTERNET   ".
               03 COLUMN 55            PIC QQQ,QQQ,QQ9.99 SUM TC.
           02 LINE PLUS 1.
               03 COLUMN 17            PIC X(33) VALUE
               "TOTAL CAPTADO CANAL 3 CAJEROS ATM".
               03 COLUMN 55            PIC QQQ,QQQ,QQ9.99 SUM TD.
           02 LINE PLUS 1.
              03 COLUMN 01             PIC X(85) VALUE ALL SPACES.
           02  LINE PLUS 1.
               03 COLUMN 01            PIC X(22) VALUE
               'ESTA ES LA ULTIMA HOJA'.
               03 COLUMN 51            PIC X(22) VALUE
               'ESTE REPORTE CONSTA DE'.
               03 COLUMN 73            PIC Z,ZZZ SOURCE PAGE-COUNTER IN
                                                 TRASACCION-CUENTAS.
               03 COLUMN 79            PIC X(07) VALUE 'PAGINAS'.
           02 LINE PLUS 1.
              03 COLUMN 01             PIC X(85) VALUE ALL SPACES.
       PROCEDURE DIVISION.
       100-PRINCIPAL     SECTION.
           PERFORM  1OO-ABRE-DATASET
           PERFORM  100-ORDENAR-GENERAR-REP
           PERFORM  300-ESTADISTICAS
           PERFORM  XXX-CLOSE-DATASET
           STOP RUN.
       100-PRINCIPAL-E.  EXIT.

       1OO-ABRE-DATASET     SECTION.
           OPEN OUTPUT REPORTE
           INITIATE TRASACCION-CUENTAS
           ACCEPT WKS-FECHA FROM SYSIN
           IF FS-REPORTE = 97
              MOVE ZEROS TO FS-REPORTE
           END-IF
           IF FS-REPORTE NOT = 0
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO ES  : (" FS-REPORTE ")"
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              PERFORM XXX-CLOSE-DATASET
              MOVE 91 TO RETURN-CODE
              STOP RUN
           ELSE
              DISPLAY
              "********** APERTURA DE ARCHIVOS EXITOSA ********"
              UPON CONSOLE
           END-IF.
       1OO-ABRE-DATASET-E.  EXIT.

       100-ORDENAR-GENERAR-REP SECTION.
           SORT WORKFILE ON ASCENDING KEY TIPO-CUENTA OF WORK-REG
                            ASCENDING KEY TIPO-CANAL  OF WORK-REG
                            ASCENDING KEY VALOR       OF WORK-REG
              INPUT  PROCEDURE   IS 210-VERIFICACION-SYSIN
              OUTPUT PROCEDURE   IS 220-OUTPUT-DATOS-FORMATEADOS.
       100-ORDENAR-GENERAR-REP-E. EXIT.

       210-VERIFICACION-SYSIN SECTION.
           ACCEPT REGISTRO-SORT FROM SYSIN
           PERFORM UNTIL FIN-DE-ARCHIVO
                ADD 1 TO WKS-REGISTROS-LEIDOS
                IF  REGISTRO-SORT NOT EQUAL SPACES  AND
                    TIPO-CUENTA   IS NUMERIC        AND
                    TIPO-CANAL    IS NUMERIC        AND
                    TIPO-CUENTA   NOT EQUAL ZEROS   AND
                    TIPO-CANAL    NOT EQUAL ZEROS
                    RELEASE WORK-REG
                ELSE
                    ADD 1 TO WKS-REG-ERROR
                    PERFORM 230-REGISTROS-NO-VALIDOS
                END-IF
                ACCEPT REGISTRO-SORT FROM SYSIN
           END-PERFORM.
       210-VERIFICACION-SYSIN-E. EXIT.

       220-OUTPUT-DATOS-FORMATEADOS SECTION.
           RETURN WORKFILE
                 AT END SET FIN-WORKFILE  TO TRUE
           END-RETURN

           PERFORM UNTIL FIN-WORKFILE
                GENERATE DETAILLINE
                EVALUATE TIPO-CANAL
                   WHEN 001
                        GENERATE DETALLE1
                   WHEN 002
                        GENERATE DETALLE2
                   WHEN 003
                        GENERATE DETALLE3
                END-EVALUATE
                RETURN WORKFILE
                     AT END SET FIN-WORKFILE TO TRUE
                END-RETURN
           END-PERFORM.
       220-OUTPUT-DATOS-FORMATEADOS-E. EXIT.

       300-ESTADISTICAS SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                   ESTADISTICAS"
           DISPLAY
           "**********************************************************"
           DISPLAY
           " TOTAL DE REGISTROS LEIDOS     : " WKS-REGISTROS-LEIDOS
           DISPLAY
           " REGISTROS CON ERROR           : " WKS-REG-ERROR
           DISPLAY
           "**********************************************************".
       300-ESTADISTICAS-E. EXIT.

       230-REGISTROS-NO-VALIDOS SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*       ERROR, SE HA INGRESADO UN REGISTRO INVALIDO      *"
           DISPLAY
           "**********************************************************"
           DISPLAY
           " ==> ES EL REGISTRO NO. (" WKS-REGISTROS-LEIDOS ") EN SYSIN"
           DISPLAY
           "          =====  POSIBLES CAUSAS DEL ERROR  ====="
           DISPLAY
           " 1.- EL REGISTRO ESTABA VACIO (ESPACIOS)"
           DISPLAY
           " 2.- EL TIPO DE CUENTA O TIPO CANAL SON IGUAL A CEROS"
           DISPLAY
           " NOTA: EL REGISTRO FUE IGNORADO, VERIFIQUE SU ENTRADA"
            DISPLAY
           " VISTA PREVIA DEL REGISTRO ES: (EN CASO NO SER VACIO)"
           DISPLAY
           " ==> " REGISTRO-SORT
           DISPLAY
           "**********************************************************"
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       230-REGISTROS-NO-VALIDOS-E. EXIT.

       XXX-CLOSE-DATASET SECTION.
           TERMINATE TRASACCION-CUENTAS
           CLOSE REPORTE.
       XXX-CLOSE-DATASET-E. EXIT.