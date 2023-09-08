      ******************************************************************
      * FECHA       : 24/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34022                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : PROGRAMA QUE REGENERA UN REPORTE DE VENTAS POR   *
      *             : DISTRITO Y POR EMPLEADO, LUEGO DE ELLO BRINDA EL *
      *             : TOTAL DE VENTAS REALIZADAS POR  LOS DISTRITOS    *
      * ARCHIVOS    : VENTADAT (PS DATOS DE ENTRADA VENTA)             *
      *             : REPORTE  (PS DATOS DE SALIDA REPORTE)            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EDU34022.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTADAT ASSIGN TO VENTADAT
                           FILE STATUS IS FS-VENTADAT.
           SELECT REPORTE  ASSIGN TO SYS010
                           FILE STATUS IS FS-REPORTE.
       DATA DIVISION.
       FILE SECTION.
       FD  VENTADAT.
       01  REGISTRO-VENTA.
           88 ENDOFFILE  VALUE HIGH-VALUES.
           02 CODIGO-CIUDAD         PIC 9(01).
           02 CODIGO-EMPLEADO       PIC 9(01).
           02 VALOR-VENTA           PIC 9(4)V99.

       FD  REPORTE
           REPORT IS REPORTE-VENTAS.

       WORKING-STORAGE SECTION.
      * ---------> VARIABLES DE CONTROL, EDITADAS Y CONTADORES.
       01 FS-VENTADAT                      PIC 9(02) VALUE ZEROS.
       01 FS-REPORTE                       PIC 9(02) VALUE ZEROS.
       01 WKS-FECHA.
          02 WKS-DIA                       PIC 9(02) VALUE ZEROS.
          02 FILLER1                       PIC X(01) VALUE '/'.
          02 WKS-MES                       PIC 9(02) VALUE ZEROS.
          02 FILLER2                       PIC X(01) VALUE '/'.
          02 WKS-ANIO                      PIC 9(04) VALUE ZEROS.

      * -----------------> TABLA DE CIUDADES
       01  WKS-TABLA-DATOS.
           02 WKS-CIUDADES-TABLA.
              03 FILLER        PIC X(18) VALUE "DUBLIN   BELFAST  ".
              03 FILLER        PIC X(18) VALUE "CORK     GALWAY   ".
              03 FILLER        PIC X(18) VALUE "SLIGO    WATERFORD".
              03 FILLER        PIC X(9)  VALUE "LIMERICK".
           02 FILLER REDEFINES WKS-CIUDADES-TABLA.
              03 WKS-NOMBRE-CIUDAD     PIC X(9) OCCURS 7 TIMES.
       REPORT SECTION.
       RD  REPORTE-VENTAS
           CONTROLS ARE CODIGO-EMPLEADO
           PAGE LIMIT IS 66
           HEADING 1
           FIRST DETAIL 7
           LAST DETAIL 42
           FOOTING 52.
       01  TYPE IS PH.
           02 LINE 1.
              03 COLUMN   1 PIC X(22) VALUE 'BANCO INDUSTRIAL, S.A.'.
              03 COLUMN  38 PIC X(50) VALUE
                  '            REPORTE DE VENTAS POR CIUDAD     '.
              03 COLUMN 106 PIC X(24) VALUE '      S E M I L L E R O'.
           02 LINE 2.
              03 COLUMN   1 PIC X(25) VALUE 'EDU34022    00.23042023.R'.
              03 COLUMN  49 PIC X(20) VALUE 'FECHA DEL MOVIMIENTO'.
              03 COLUMN  70 PIC X(10) SOURCE WKS-FECHA.
              03 COLUMN 120 PIC X(06) VALUE 'PAGINA'.
              03 COLUMN 128 PIC Z(05) SOURCE
                                      PAGE-COUNTER IN REPORTE-VENTAS.
           02 LINE 3.
              03 COLUMN  01 PIC X(132) VALUE ALL '='.
           02 LINE 4.
              03 COLUMN 16     PIC X(06) VALUE "CIUDAD".
              03 COLUMN 54     PIC X(18) VALUE "CODIGO DE EMPLEADO".
              03 COLUMN 95     PIC X(12) VALUE "VALOR VENTAS".
           02 LINE 5.
              03 COLUMN  01 PIC X(132) VALUE ALL '='.
       01  DETAILLINE TYPE IS DETAIL.
           02 LINE IS PLUS 1.
              03 COLUMN 16      PIC X(09)
                               SOURCE WKS-NOMBRE-CIUDAD(CODIGO-CIUDAD).
              03 COLUMN 54     PIC 9(01)
                               SOURCE CODIGO-EMPLEADO.
              03 COLUMN 95     PIC QQ,QQQ.99 SOURCE VALOR-VENTA.
       01  SALESPERSONGRP
           TYPE IS CONTROL FOOTING CODIGO-EMPLEADO  NEXT GROUP PLUS 2.
           02 LINE IS PLUS 1.
              03 COLUMN 92     PIC X(18) VALUE
                               "------------------".
           02 LINE IS PLUS 1.
              03 COLUMN 54     PIC X(28) VALUE
                              "TOTAL VENTAS CODIGO EMPLEADO".
              03 COLUMN 83     PIC 9 SOURCE CODIGO-EMPLEADO.
              03 COLUMN 85     PIC X VALUE "=".
              03 SMS COLUMN 92 PIC QQ,QQQ,QQ9.99 SUM VALOR-VENTA.
       01  TYPE IS PF.
           02  LINE PLUS 0.
              03 COLUMN 1   PIC X(25) VALUE 'FECHA Y HORA DE OPERACION'.
              03 COLUMN 30  PIC <99/<99/9999  FUNC MDATE.
              03 COLUMN 42  PIC 99,99,99      FUNC TIME.
              03 COLUMN 52  PIC X(16) VALUE 'DATA-CENTRO S.A.'.
              03 COLUMN 113 PIC X(6)  VALUE 'PAGINA'.
              03 COLUMN 124 PIC ZZ,ZZ9 SOURCE PAGE-COUNTER IN
                                              REPORTE-VENTAS.
       01  TYPE IS RF.
           02  LINE PLUS 3.
               03 COLUMN 50 PIC X(22) VALUE 'ESTA ES LA ULTIMA HOJA'.
           02  LINE PLUS 1.
               03 COLUMN 50 PIC X(23) VALUE 'ESTE REPORTE CONSTA DE '.
               03 COLUMN 73 PIC Z,ZZZ SOURCE PAGE-COUNTER IN
                                              REPORTE-VENTAS.
               03 COLUMN 80 PIC X(07) VALUE 'PAGINAS'.
       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 100-ABRE-INICIA-REPORTE
           PERFORM 200-GENERAR-REPORTE
           PERFORM 300-TERMINA-CIERRA-REPORTE
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

       100-ABRE-INICIA-REPORTE SECTION.
           ACCEPT WKS-FECHA FROM SYSIPT
           OPEN INPUT VENTADAT
           OPEN OUTPUT REPORTE

           IF (FS-VENTADAT EQUAL 97) AND (FS-REPORTE EQUAL 97)
              MOVE ZEROS TO FS-VENTADAT
                            FS-REPORTE
           END-IF
           IF (FS-VENTADAT NOT EQUAL 0) AND (FS-REPORTE NOT EQUAL 0)
              PERFORM 111-ERROR-ABRIR-ARCHIVOS
           ELSE
              INITIATE REPORTE-VENTAS
           END-IF.
       100-ABRE-INICIA-REPORTE-E.

       111-ERROR-ABRIR-ARCHIVOS SECTION.
           DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY
                   "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY " FILE ESTATUS DEL ARCHIVO VENTAS  : ("
                                                        FS-VENTADAT ")"
                   UPON CONSOLE
                DISPLAY " FILE ESTATUS DEL ARCHIVO REPORTE : ("
                                                          FS-REPORTE ")"
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                CLOSE VENTADAT
                CLOSE REPORTE
                MOVE 91 TO RETURN-CODE
                STOP RUN.
       111-ERROR-ABRIR-ARCHIVOS-E. EXIT.

       200-GENERAR-REPORTE SECTION.
           READ VENTADAT
                AT END SET ENDOFFILE TO TRUE
           END-READ
           PERFORM UNTIL ENDOFFILE
              GENERATE DETAILLINE
              READ VENTADAT
                 AT END SET ENDOFFILE TO TRUE
              END-READ
           END-PERFORM.
       200-GENERAR-REPORTE-E. EXIT.

       300-TERMINA-CIERRA-REPORTE SECTION.
           TERMINATE REPORTE-VENTAS
           CLOSE VENTADAT
           CLOSE REPORTE.
       300-TERMINA-CIERRA-REPORTE-E. EXIT.