      ******************************************************************
      * FECHA       : 25/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33016                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA VERIFICA EL ARCHIVO DE TRANSACCION *
      *             : LEE SECUENCIALMENTE, LOS REGISTROS QUE CUMPLEN   *
      *             : SUS CONDICIONES SON ENVIADOS A UN REPORTE Y LUEGO*
      *             : BORRADOS DEL ARCHIVO MAESTRO.                    *
      * ARCHIVOS    : STFRJO, CFTGEN, REPORTE(PS SALIDA GDG)           *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU33016.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT STFRJO  ASSIGN TO STFRJO
                           ORGANIZATION  IS INDEXED
                           ACCESS        IS SEQUENTIAL
                           RECORD KEY    IS STFR-LLAVE IN REG-STFRJO
                           FILE STATUS   IS FS-STFRJO
                                            FSE-STFRJO.
            SELECT CFTGEN  ASSIGN TO CFTGEN
                           ORGANIZATION  IS INDEXED
                           ACCESS MODE   IS DYNAMIC
                           RECORD KEY    IS CFTB-LLAVE
                           FILE STATUS   IS FS-CFTGEN
                                            FSE-CFTGEN.
           SELECT REPORTE  ASSIGN TO SYS010
                           FILE STATUS IS FS-REPORTE.

           SELECT WORKFILE ASSIGN TO SORTWK1.

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *                DEFINICIONES DE ARCHIVOS A UTILIZAR             *
      ******************************************************************
       FD STFRJO.
          COPY STFRJO.
       FD CFTGEN.
          COPY CFTGEN  REPLACING CFTGEN BY REG-CFTGEN.

       FD REPORTE
          REPORT IS REPORTE-SAT.
       SD WORKFILE.
       01 WORK-REG.
      *---> LLAVE
           05 STFR-LLAVE.
              10 STFR-NUMERO-FORMULARIO      PIC 9(11).
      *---> ENVIA NOTIFICACION PAGO SAT
           05 ENVIA-NOTIFICACION-PAGO.
              10  STFR-CODIGO-AGENCIA        PIC  9(06).
              10  STFR-MONTO-COBRADO-2       PIC  9(16)V99.
           05 STFR-HORA-RECIBO-PAGO-390      PIC  S9(07).
           05 NOMBRE-AGENCIA                 PIC X(30).
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *                CONTADORES, OPERADORES Y CONTROLADORES          *
      ******************************************************************
       01 WKS-ARCHIVO-ANALIZAR      PIC 9(01)      VALUE ZEROS.
       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-PROGRAMA           PIC X(08)      VALUE "EDU33021".
          02 WKS-LECTURAS-TOTALES   PIC 9(06)      VALUE ZEROS.
          02 WKS-REG-ESCRITOS       PIC 9(06)      VALUE ZEROS.
          02 WKS-REG-DELETE         PIC 9(06)      VALUE ZEROS.
          02 WKS-REG-NO-CUMPLEN     PIC 9(06)      VALUE ZEROS.
          02 WKS-TABLA1-OCCURS      PIC 9(04)      VALUE ZEROS.
          02 WKS-FIN-WORKFILE       PIC 9(01)      VALUE 0.
          02 WKS-FIN-CARGA-AGE      PIC 9(01)      VALUE 0.
          02 WKS-CICLO-LECTURA      PIC X(02)      VALUE "SI".
          02 WKS-TOTAL-EDIT         PIC QQQ,QQQ,QQ9.99 VALUE ZEROS.
       01 WKS-FECHA.
          02 WKS-DIA                PIC 9(02)      VALUE ZEROS.
          02 FILLER1                PIC X(01)      VALUE '/'.
          02 WKS-MES                PIC 9(02)      VALUE ZEROS.
          02 FILLER2                PIC X(01)      VALUE '/'.
          02 WKS-ANIO               PIC 9(04)      VALUE ZEROS.
      ******************************************************************
      *                      VARIABLES PARA FILESTATUS                 *
      ******************************************************************
      * ---> VARIABLES DE FILE STATUS NORMAL
       01 FS-CFTGEN                    PIC 9(02)     VALUE ZEROS.
       01 FS-REPORTE                   PIC 9(02)     VALUE ZEROS.
       01 FS-STFRJO                    PIC 9(02)     VALUE ZEROS.
      * ---> VARIABLES DE FILE STATUS EXTENDED
       01 FSE-STFRJO.
          02 FSE-RETURN                PIC S9(04) COMP-5
                                                     VALUE ZEROS.
          02 FSE-FUNCTION              PIC S9(04) COMP-5
                                                     VALUE ZEROS.
          02 FSE-FEEDBACK              PIC S9(04) COMP-5
                                                     VALUE ZEROS.
       01 FSE-CFTGEN.
          02 FSE-RETURN                PIC S9(04) COMP-5
                                                     VALUE ZEROS.
          02 FSE-FUNCTION              PIC S9(04) COMP-5
                                                     VALUE ZEROS.
          02 FSE-FEEDBACK              PIC S9(04) COMP-5
                                                     VALUE ZEROS.
      * ---> VARIABLES DE RUTINA PARA CONTROL DE FILE STATUS EXTENDIDO
       01 PROGRAMA                     PIC X(08)     VALUE SPACES.
       01 ARCHIVO                      PIC X(08)     VALUE SPACES.
       01 ACCION                       PIC X(10)     VALUE SPACES.
       01 LLAVE                        PIC X(32)     VALUE SPACES.
      ******************************************************************
      *         ============== TABLA DE AGENCIAS ============          *
      ******************************************************************
       01 WKS-TABLAS.
      *--> TABLA AGENCIAS CFT-006
          02 WKS-TABLA-AGE OCCURS 1 TO 9999
                DEPENDING ON WKS-TABLA1-OCCURS
                ASCENDING KEY WKS-COD-AGE
                INDEXED BY IDX-AGE.
             03 WKS-COD-AGE            PIC 9(06).
             03 WKS-NOMBRE-AGE         PIC X(30).
      ******************************************************************
      *                  MAQUETACION REPORTE DE SALIDA                 *
      ******************************************************************
       REPORT SECTION.
       RD  REPORTE-SAT
           CONTROLS ARE FINAL
           STFR-CODIGO-AGENCIA IN WORK-REG
           LINE LIMIT IS 113
           PAGE LIMIT IS 66
           HEADING 1
           FIRST DETAIL 7
           LAST DETAIL 42
           FOOTING 52.
      ******************************************************************
      *                     MAQUETACION PAGE HEADER                    *
      ******************************************************************
       01  TYPE IS PH.
           02 LINE 1.
              03 COLUMN   1            PIC X(22) VALUE
                 'BANCO INDUSTRIAL, S.A.'.
              03 COLUMN  35            PIC X(45) VALUE
                 'REPORTE FORMULARIOS PRESENTADOS DESDE AGENCIA'.
              03 COLUMN  97            PIC X(17) VALUE
                 'S E M I L L E R O'.
           02 LINE 2.
              03 COLUMN   1            PIC X(25) VALUE
                 'EDU33021    01.03052023.R'.
              03 COLUMN  40            PIC X(20) VALUE
                 'FECHA DEL MOVIMIENTO'.
              03 COLUMN  61            PIC X(10) SOURCE WKS-FECHA.
              03 COLUMN 101            PIC X(06) VALUE 'PAGINA'.
              03 COLUMN 109            PIC Z(05) SOURCE PAGE-COUNTER IN
                                                 REPORTE-SAT.
           02 LINE 3.
              03 COLUMN  01 PIC X(113) VALUE ALL '='.
      ******************************************************************
      *                     MAQUETACION LINEA DETALLE                  *
      ******************************************************************
       01  DETAILLINE TYPE IS DETAIL.
           02 LINE IS PLUS 1.
              03 COLUMN 12             PIC Z(06) SOURCE
                 STFR-CODIGO-AGENCIA    IN WORK-REG.
              03 COLUMN 35             PIC 9(11) SOURCE
                 STFR-NUMERO-FORMULARIO IN WORK-REG.
              03 COLUMN 59             PIC Q,QQQ,QQQ,QQQ,QQQ,QQ9.99
                 SOURCE STFR-MONTO-COBRADO-2 IN WORK-REG.
              03 COLUMN 93             PIC 99'H.'99'M.'99'S'
                 SOURCE STFR-HORA-RECIBO-PAGO-390 IN WORK-REG.
       01  DETAILNONE TYPE IS DETAIL.
           02 LINE IS PLUS 1.
              03 COLUMN 30             PIC X(41) VALUE
              "NO HAY MOVIMIENTOS QUE CUMPLAN PARAMETROS".
      ******************************************************************
      *                 MAQUETACION CONTROL BREAK HEADING              *
      ******************************************************************
       01  AGENCIAS  TYPE IS CH STFR-CODIGO-AGENCIA IN WORK-REG.
           02 LINE IS PLUS 1.
              03 COLUMN 39             PIC X(09) VALUE "AGENCIA :".
              03 COLUMN 49             PIC X(30) SOURCE NOMBRE-AGENCIA.

           02 LINE IS PLUS 1.
              03 COLUMN 8              PIC X(43) VALUE
                 "CODIGO DE AGENCIA      NUMERO DE FORMULARIO".
              03 COLUMN 61             PIC X(44) VALUE
                 "MONTO DEL FORMULARIO            HORA DE PAGO".
           02 LINE IS PLUS 1.
              03 COLUMN 5              PIC X(105) VALUE ALL "=".
      ******************************************************************
      *                   MAQUETACION CONTROL FOOTING                  *
      ******************************************************************
       01  FINAGENCIA TYPE IS CF
           STFR-CODIGO-AGENCIA IN WORK-REG NEXT GROUP PLUS 2.
           02 LINE IS PLUS 1.
              03 COLUMN 53             PIC X(30) VALUE
              "SUMA  ------------------------".
           02 LINE IS PLUS 1.
              03 COLUMN  6             PIC X(15) VALUE
              "TOTAL AGENCIA: ".
              03 COLUMN 37             PIC Q,QQQ,QQQ,QQQ,QQQ,QQ9.99
              SUM STFR-MONTO-COBRADO-2 IN WORK-REG RESET ON
                   STFR-CODIGO-AGENCIA IN WORK-REG.
           02 LINE IS PLUS 1.
              03 COLUMN 3              PIC X(100) VALUE ALL SPACES.
           02 LINE IS PLUS 1.
              03 COLUMN 3              PIC X(100) VALUE ALL SPACES.
      ******************************************************************
      *                  MAQUETACION CONTROL FOOTING FINAL             *
      ******************************************************************
       01 TYPE IS CF FINAL.
           02 LINE IS PLUS 1.
              03 COLUMN 43             PIC X(35) VALUE
              "========== R E S U M E N ==========".
           02 LINE PLUS 1.
              03 COLUMN 26             PIC X(48)  VALUE
              'TOTAL FORMULARIOS RECIBIDOS EN AGENCIA        = '.
              03 COLUMN 89             PIC ZZZ,ZZZ,ZZ9
               COUNT OF DETAILLINE.
           02 LINE PLUS 1.
              03 COLUMN 26             PIC X(48)  VALUE
              'VALOR TOTAL FORMULARIOS                       ='.
              03 COLUMN 80             PIC Q,QQQ,QQQ,QQQ,QQQ,QQ9.99
               SUM STFR-MONTO-COBRADO-2 IN WORK-REG.
           02 LINE PLUS 1.
              03 COLUMN 01             PIC X(100) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION PAGE FOOTING                   *
      ******************************************************************
       01  TYPE IS PF.
           02  LINE PLUS 0.
              03 COLUMN 1              PIC X(25) VALUE
              'FECHA Y HORA DE OPERACION'.
              03 COLUMN 28             PIC 99/99/9999 FUNC DATE.
              03 COLUMN 42             PIC 99'H.'99'M.'99'S' FUNC TIME.
              03 COLUMN 56             PIC X(16) VALUE
              'DATA-CENTRO S.A.'.
              03 COLUMN 101            PIC X(6)  VALUE 'PAGINA'.
              03 COLUMN 108            PIC ZZZZ SOURCE PAGE-COUNTER IN
                                                REPORTE-SAT.
           02 LINE PLUS 1.
              03 COLUMN 01             PIC X(100) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION REPORT FINAL                   *
      ******************************************************************
       01  TYPE IS RF.
           02 LINE PLUS 3.
             03 COLUMN  26             PIC X(48) VALUE
             "ESTA ES LA ULTIMA HOJA DE ESTE REPORTE CONSTA DE".
             03 COLUMN  75             PIC Z,ZZZ SOURCE PAGE-COUNTER IN
                                                 REPORTE-SAT.
             03 COLUMN 81             PIC X(07)  VALUE 'PAGINAS'.

      ******************************************************************

       PROCEDURE DIVISION.
       100-SECCION-PRINCIPAL SECTION.
           PERFORM 100-ABRIR-VALIDAR-DATASETS
           PERFORM 110-VERIFICAR-INTEGRIDAD-FS
           PERFORM 200-CARGAR-TABLAS-EN-MEMORIA
           PERFORM 300-SORT-DATOS-CORRECTOS
           PERFORM 800-ESTADISTICAS
           PERFORM XXX-CIERRA-ARCHIVOS
           STOP RUN.
       100-SECCION-PRINCIPAL-E.            EXIT.

       100-ABRIR-VALIDAR-DATASETS SECTION.
           MOVE "EDU33021"   TO PROGRAMA
           ACCEPT WKS-FECHA  FROM SYSIN
           INITIATE REPORTE-SAT
           OPEN I-O          STFRJO
           OPEN INPUT        CFTGEN
           OPEN OUTPUT       REPORTE.
       100-ABRIR-VALIDAR-DATASETS-E.            EXIT.

       110-VERIFICAR-INTEGRIDAD-FS SECTION.
           IF (FS-STFRJO = 97) AND (FS-CFTGEN  = 97) AND
              (FS-REPORTE = 97)

              DISPLAY
              "********************************************************"
              UPON CONSOLE
              DISPLAY
              "*      >>>   APERTURA DE ARCHIVOS E/S EXITOSA   <<<    *"
              UPON CONSOLE
              DISPLAY
              "********************************************************"
              UPON CONSOLE
              MOVE ZEROS TO FS-STFRJO
              MOVE ZEROS TO FS-CFTGEN
              MOVE ZEROS TO FS-REPORTE
           END-IF
           IF FS-STFRJO NOT EQUAL 0
              MOVE 'OPEN'    TO ACCION
              MOVE SPACES    TO LLAVE
              MOVE 'STFRJO' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                    LLAVE, FS-STFRJO, FSE-STFRJO
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO STFRJO  <<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-CFTGEN NOT EQUAL 0
              MOVE 'OPEN'    TO ACCION
              MOVE SPACES    TO LLAVE
              MOVE 'CFTGEN ' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                    LLAVE, FS-CFTGEN, FSE-CFTGEN
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO CFTGEN <<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF.
        110-VERIFICAR-INTEGRIDAD-FS-E. EXIT.

       200-CARGAR-TABLAS-EN-MEMORIA         SECTION.
           MOVE "CFT0060000000000" TO CFTB-LLAVE
           START CFTGEN
              KEY IS >= CFTB-LLAVE
           END-START
           MOVE 2 TO WKS-ARCHIVO-ANALIZAR
           PERFORM 700-ERRORES-LEC-SECUENCIAL

           READ CFTGEN NEXT RECORD
           PERFORM UNTIL WKS-FIN-CARGA-AGE = 1
                   EVALUATE CFTB-CODIGO-TABLA
                   WHEN 006
                        ADD 1 TO WKS-TABLA1-OCCURS
                        MOVE CFTB-CORRELATIVO TO WKS-COD-AGE(IDX-AGE)
                        MOVE CFTB-RESTO TO WKS-NOMBRE-AGE(IDX-AGE)
                        SET IDX-AGE UP BY 1
                   WHEN OTHER
                        MOVE 1 TO WKS-FIN-CARGA-AGE
                   END-EVALUATE
                   READ CFTGEN NEXT RECORD
                   MOVE 2 TO WKS-ARCHIVO-ANALIZAR
                   PERFORM 700-ERRORES-LEC-SECUENCIAL
           END-PERFORM.
       200-CARGAR-TABLAS-EN-MEMORIA-E.      EXIT.

       300-SORT-DATOS-CORRECTOS SECTION.
           SORT WORKFILE ON
              ASCENDING KEY STFR-CODIGO-AGENCIA OF WORK-REG
              ASCENDING KEY STFR-LLAVE          OF WORK-REG
                 INPUT  PROCEDURE IS 300-VERIFI-DATOS-SORT
                 OUTPUT PROCEDURE IS 300-GENERAR-REPORTE.
       300-SORT-DATOS-CORRECTOS-E. EXIT.

       300-VERIFI-DATOS-SORT SECTION.
           READ STFRJO
                AT END MOVE "SI" TO WKS-CICLO-LECTURA
           END-READ
           EVALUATE FS-STFRJO
              WHEN 0
                   PERFORM 300-LECTURA-CICLOS
              WHEN 10
                   MOVE "NO" TO WKS-CICLO-LECTURA
                   IF WKS-REG-DELETE = 0
                       MOVE ALL "*" TO WORK-REG
                       GENERATE DETAILNONE
                   END-IF
              WHEN OTHER
                   MOVE 2 TO WKS-ARCHIVO-ANALIZAR
                   PERFORM 700-ERRORES-LEC-SECUENCIAL
           END-EVALUATE.
       300-VERIFI-DATOS-SORT-E. EXIT.

       300-LECTURA-CICLOS    SECTION.
           PERFORM UNTIL WKS-CICLO-LECTURA = "NO"
               ADD 1 TO WKS-LECTURAS-TOTALES

               PERFORM 300-CONDICIONALES-SORT
               READ STFRJO
                    AT END MOVE "SI" TO WKS-CICLO-LECTURA
               END-READ
               EVALUATE FS-STFRJO
                  WHEN 10
                      MOVE "NO" TO WKS-CICLO-LECTURA
                      PERFORM 300-LECTURA-CICLOS
                      IF WKS-REG-DELETE = 0
                          MOVE ALL "*" TO WORK-REG
                          GENERATE DETAILNONE
                      END-IF
                  WHEN OTHER
                       MOVE 2 TO WKS-ARCHIVO-ANALIZAR
                       PERFORM 700-ERRORES-LEC-SECUENCIAL
               END-EVALUATE
           END-PERFORM.
       300-LECTURA-CICLOS-E. EXIT.

       300-CONDICIONALES-SORT SECTION.
           IF STFR-PROCEDENCIA-TRANSACION = 1 AND STFR-FLAG-PAGO = 1
              IF STFR-MONTO-COBRADO-2 IN REG-STFRJO >= 3000 AND
                 STFR-MONTO-COBRADO-2 IN REG-STFRJO <= 6500
                 MOVE CORRESPONDING REG-STFRJO TO WORK-REG
                 SEARCH ALL WKS-TABLA-AGE
                    AT END MOVE SPACES TO  NOMBRE-AGENCIA
                    WHEN WKS-COD-AGE(IDX-AGE) =
                    STFR-CODIGO-AGENCIA IN WORK-REG
                    MOVE WKS-NOMBRE-AGE(IDX-AGE) TO NOMBRE-AGENCIA
                 END-SEARCH
                 RELEASE WORK-REG
                 DELETE STFRJO
                 MOVE 2 TO WKS-ARCHIVO-ANALIZAR
                 PERFORM 700-ERRORES-LEC-SECUENCIAL
                 ADD 1 TO WKS-REG-DELETE
           ELSE
             ADD 1 TO  WKS-REG-NO-CUMPLEN
           END-IF.
       300-CONDICIONALES-SORT-E. EXIT.

       300-GENERAR-REPORTE SECTION.
           RETURN WORKFILE
                AT END MOVE 1 TO WKS-FIN-WORKFILE
           END-RETURN.
           PERFORM UNTIL WKS-FIN-WORKFILE = 1
                 GENERATE DETAILLINE
              ADD 1 TO WKS-REG-ESCRITOS
              RETURN WORKFILE
                 AT END MOVE 1 TO WKS-FIN-WORKFILE
              END-RETURN
           END-PERFORM.
       300-GENERAR-REPORTE-E. EXIT.


       700-ERRORES-LEC-SECUENCIAL       SECTION.
           EVALUATE WKS-ARCHIVO-ANALIZAR
           WHEN 1
               IF FS-STFRJO NOT EQUAL 0
                  MOVE 'OPEN'    TO ACCION
                  MOVE SPACES    TO LLAVE
                  MOVE 'STFRJO' TO ARCHIVO
                  CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                        LLAVE, FS-STFRJO, FSE-STFRJO
                  MOVE  91  TO RETURN-CODE
                  DISPLAY
                   "   >>> ALGO SALIO MAL AL LEER EL ARCHIVO STFRJO <<<"
                  UPON CONSOLE
                  DISPLAY
                  "            !!! VERIFICAR DETALLES EN SPOOL !!!"
                  UPON CONSOLE
                  PERFORM XXX-CIERRA-ARCHIVOS
                  STOP RUN
               END-IF
           WHEN 2
                IF FS-CFTGEN NOT EQUAL 0
                   MOVE 'READ' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'CFTGEN ' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                         LLAVE, FS-CFTGEN, FSE-CFTGEN
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                   "   >>> ALGO SALIO MAL AL LEER EL ARCHIVO CFTGEN <<<"
                      UPON CONSOLE
                   DISPLAY
                      "          !!! VERIFICAR DETALLES EN SPOOL !!!"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           END-EVALUATE
           INITIALIZE WKS-ARCHIVO-ANALIZAR.
       700-ERRORES-LEC-SECUENCIAL-E.          EXIT.

       800-ESTADISTICAS SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                   E S T A D I S T I C A S              *"
           DISPLAY
           "**********************************************************"
           DISPLAY
           " > REGISTROS LEIDOS EN TOTAL            : "
           WKS-LECTURAS-TOTALES
           DISPLAY
           " > REGISTROS ESCRITOS REPORTE           : " WKS-REG-ESCRITOS
           DISPLAY
           " > REGISTROS ELIMINADOS                 : " WKS-REG-DELETE
           DISPLAY
           " > REGISTROS QUE NO CUMPLEN CONDICIONES : "
           WKS-REG-NO-CUMPLEN
           DISPLAY
           "**********************************************************".
       800-ESTADISTICAS-E. EXIT.


        XXX-CIERRA-ARCHIVOS SECTION.
           TERMINATE REPORTE-SAT
           CLOSE   STFRJO
           CLOSE   CFTGEN
           CLOSE   REPORTE.
        XXX-CIERRA-ARCHIVOS-E. EXIT.