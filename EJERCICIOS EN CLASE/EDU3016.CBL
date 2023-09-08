      ******************************************************************
      * FECHA       : 25/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33016                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA LEE UN ARCHIVO MAESTRO DE CUENTAS  *
      *             : PDI Y PFP, DE LAS CUALES RECUPERA LAS CUENTAS EN-*
      *             : LACE QUE CUMPLAN CON CIERTOS PARAMETROS, LAS CUA-*
      *             : LES SERAN ORDENADAS POR AGENCIA Y ENVIADAS A UN  *
      *             : REPORTE                                          *
      * ARCHIVOS    : FPMCTAN, CFTGEN, REPORTE(PS SALIDA),             *
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

           SELECT FPMCTAN  ASSIGN TO FPMCTAN
                                   ORGANIZATION IS INDEXED
                                   ACCESS       IS SEQUENTIAL
                                   RECORD KEY   IS FPMC-LLAVE
                                   FILE STATUS  IS FS-FPMCTAN
                                                   FSE-FPMCTAN.
           SELECT FPMCTA2  ASSIGN TO FPMCTA2
                                   ORGANIZATION IS INDEXED
                                   ACCESS       IS RANDOM
                                   RECORD KEY   IS FPM2-LLAVE
                                   FILE STATUS  IS FS-FPMCTA2
                                                   FSE-FPMCTA2.
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
       FD FPMCTAN.
          COPY FPMCTAN.
       FD FPMCTA2.
          COPY FPMCTAN REPLACING REG-FPMCTA BY REG-FPMCT2
                          LEADING FPMC BY FPM2.
       FD CFTGEN.
          COPY CFTGEN  REPLACING CFTGEN BY REG-CFTGEN.

       FD REPORTE
          REPORT IS REPORTE-PDI-PFP.
       SD WORKFILE.
       01 WORKAREA.
          88 ENDOFFILE                 VALUE HIGH-VALUES.
          02 CUENTA-PRINCIPAL          PIC 9(16).
          02 CUENTA-SECUNDARIA         PIC 9(16).
          02 APORTACION-PACTADA        PIC 9(09)V99.
          02 CODIGO-AGENCIA            PIC 9(04).
          02 NOMBRE-AGENCIA            PIC X(30).
          02 PLAZO-FONDO               PIC 9(03).
          02 TASA-INTERESES            PIC ZZ9.9999.
          02 NUMERO-EJECUTIVO          PIC 9(04).
          02 CONTACTO-GERENCIAL        PIC X(25).
          02 CARGO-CONTACTO-GERENCIAL  PIC X(15).


       WORKING-STORAGE SECTION.
      ******************************************************************
      *                CONTADORES, OPERADORES Y CONTROLADORES          *
      ******************************************************************
       01 WKS-ARCHIVO-ANALIZAR         PIC X(08) VALUE SPACES.
       01 WKS-CONTADORES.
          02 WKS-LECTURAS-TOTALES      PIC 9(06) VALUE ZEROS.
          02 WKS-CTA-PRINCIPAL         PIC 9(06) VALUE ZEROS.
          02 WKS-CTA-ENLACE            PIC 9(06) VALUE ZEROS.
          02 WKS-CTA-NOT-FOUND         PIC 9(06) VALUE ZEROS.
          02 WKS-CTA-ENLACE-VERIFI     PIC 9(06) VALUE ZEROS.
          02 WKS-CTA-APORTE-MENOR      PIC 9(06) VALUE ZEROS.
          02 WKS-TOTALES-EDIT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
          02 WKS-CONTADOR-AGENCIAS     PIC 9(04) VALUE ZEROS.
          02 WKS-TABLA1-OCCURS         PIC 9(04) VALUE ZEROS.
       01 WKS-DISPARADORES.
          88 WKS-FIN-CARGA-AGE                   VALUE HIGH-VALUES.
          02 WKS-CICLOS                PIC X(02) VALUE "NO".
       01 WKS-FECHA.
          02 WKS-DIA              PIC 9(02) VALUE ZEROS.
          02 FILLER1              PIC X(01) VALUE '/'.
          02 WKS-MES              PIC 9(02) VALUE ZEROS.
          02 FILLER2              PIC X(01) VALUE '/'.
          02 WKS-ANIO             PIC 9(04) VALUE ZEROS.
      ******************************************************************
      *                      VARIABLES PARA FILESTATUS                 *
      ******************************************************************
      * ---> VARIABLES DE FILE STATUS NORMAL
       01  FS-FPMCTAN                  PIC 9(02) VALUE ZEROS.
       01  FS-FPMCTA2                  PIC 9(02) VALUE ZEROS.
       01  FS-CFTGEN                   PIC 9(02) VALUE ZEROS.
       01  FS-REPORTE                  PIC 9(02) VALUE ZEROS.
      * ---> VARIABLES DE FILE STATUS EXTENDED
       01  FSE-FPMCTAN.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
       01  FSE-FPMCTA2.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
       01  FSE-CFTGEN.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
      * ---> VARIABLES DE RUTINA PARA CONTROL DE FILE STATUS EXTENDIDO
       01  PROGRAMA                    PIC X(08) VALUE SPACES.
       01  ARCHIVO                     PIC X(08) VALUE SPACES.
       01  ACCION                      PIC X(10) VALUE SPACES.
       01  LLAVE                       PIC X(32) VALUE SPACES.
      ******************************************************************
      *         ============== TABLA DE AGENCIAS ============          *
      ******************************************************************
       01 WKS-TABLAS.
      *--> TABLA AGENCIAS CFT-006
          02 WKS-TABLA-AGE OCCURS 1 TO 9999
                           DEPENDING ON  WKS-TABLA1-OCCURS
                           ASCENDING KEY WKS-COD-AGE
                           INDEXED   BY  IDX-AGE.
             03 WKS-COD-AGE             PIC 9(04).
             03 WKS-NOMBRE-AGE          PIC X(30).
      ******************************************************************
      *                  MAQUETACION REPORTE DE SALIDA                 *
      ******************************************************************
       REPORT SECTION.
       RD  REPORTE-PDI-PFP
           CONTROLS ARE CODIGO-AGENCIA
           PAGE LIMIT IS 66
           HEADING 1
           FIRST DETAIL 5
           LAST DETAIL 42
           FOOTING 52.
      ******************************************************************
      *                     MAQUETACION PAGE HEADER                    *
      ******************************************************************
       01  TYPE IS PH.
           02 LINE 1.
              03 COLUMN   1 PIC X(22) VALUE 'BANCO INDUSTRIAL, S.A.'.
              03 COLUMN  38 PIC X(50) VALUE
                  '      REPORTE PRODUCTOS FINANCIERA INDUSTRIAL'.
              03 COLUMN 106 PIC X(24) VALUE '      S E M I L L E R O'.
           02 LINE 2.
              03 COLUMN   1 PIC X(25) VALUE 'EDU33022    00.24042023.R'.
              03 COLUMN  49 PIC X(20) VALUE 'FECHA DEL MOVIMIENTO'.
              03 COLUMN  70 PIC X(10) SOURCE WKS-FECHA.
              03 COLUMN 120 PIC X(06) VALUE 'PAGINA'.
              03 COLUMN 128 PIC Z(05) SOURCE
                                      PAGE-COUNTER IN REPORTE-PDI-PFP.
           02 LINE 3.
              03 COLUMN  01 PIC X(132) VALUE ALL '='.
      ******************************************************************
      *                     MAQUETACION LINEA DETALLE                  *
      ******************************************************************
       01  DETAILLINE TYPE IS DETAIL.
           02 LINE IS PLUS 1.
              03 COLUMN 3    PIC X(16) SOURCE CUENTA-PRINCIPAL.
              03 COLUMN 20   PIC X(16) SOURCE CUENTA-SECUNDARIA.
              03 COLUMN 37   PIC QQQ,QQQ,QQ9.99
                                       SOURCE APORTACION-PACTADA.
              03 COLUMN 57   PIC 9(03) SOURCE PLAZO-FONDO.

              03 COLUMN 67   PIC ZZ9.9999
                                       SOURCE TASA-INTERESES.
              03 COLUMN 76   PIC X(01) VALUE "%".
              03 COLUMN 81   PIC 9(04) SOURCE NUMERO-EJECUTIVO.
              03 COLUMN 89   PIC X(25) SOURCE CONTACTO-GERENCIAL.
              03 COLUMN 115  PIC X(15) SOURCE CARGO-CONTACTO-GERENCIAL.
      ******************************************************************
      *                 MAQUETACION CONTROL BREAK HEADING              *
      ******************************************************************
       01  AGENCIAS  TYPE IS CH.
           02 LINE IS PLUS 1.
              03 COLUMN 3    PIC X(05) VALUE "---->".
              03 COLUMN 21   PIC X(19) VALUE "CODIGO DE AGENCIA :".
              03 COLUMN 40   PIC 9(04) SOURCE CODIGO-AGENCIA.
              03 COLUMN 59   PIC X(21) VALUE "NOMBRE DE LA AGENCIA:".
              03 COLUMN 81   PIC X(30) SOURCE NOMBRE-AGENCIA.
              03 COLUMN 125  PIC X(05) VALUE "<----".

           02 LINE IS PLUS 1.
              03 COLUMN 3    PIC X(16) VALUE "CUENTA PRINCIPAL".
              03 COLUMN 21   PIC X(13) VALUE "CUENTA ENLACE".
              03 COLUMN 37   PIC X(14) VALUE "APORTE PACTADO".
              03 COLUMN 53   PIC X(11) VALUE "PLAZO FONDO".
              03 COLUMN 65   PIC X(12) VALUE "TASA INTERES".
              03 COLUMN 78   PIC X(09) VALUE "EJECUTIVO".
              03 COLUMN 93   PIC X(17) VALUE "CONTACTO GERENCIA".
              03 COLUMN 115  PIC X(14) VALUE "CARGO CONTACTO".
           02 LINE IS PLUS 1.
              03 COLUMN 3    PIC X(127) VALUE ALL "-".
      ******************************************************************
      *                   MAQUETACION CONTROL FOOTING                  *
      ******************************************************************
       01  FINAGENCIA
           TYPE IS CONTROL FOOTING CODIGO-AGENCIA  NEXT GROUP PLUS 2.
           02 LINE IS PLUS 1.
              03 COLUMN 37        PIC X(14) VALUE "--------------".
           02 LINE IS PLUS 1.
              03 COLUMN 37        PIC QQQ,QQQ,QQ9.99
                                            SUM  APORTACION-PACTADA.
              03 COLUMN 52        PIC X(15) VALUE "= TOTAL AGENCIA".
           02 LINE IS PLUS 1.
              03 COLUMN 3    PIC X(127) VALUE ALL SPACES.
           02 LINE IS PLUS 1.
              03 COLUMN 3    PIC X(127) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION PAGE FOOTING                   *
      ******************************************************************
       01  TYPE IS PF.
           02  LINE PLUS 0.
              03 COLUMN 1         PIC X(25)
                                  VALUE 'FECHA Y HORA DE OPERACION'.
              03 COLUMN 30        PIC <99/<99/9999  FUNC MDATE.
              03 COLUMN 42        PIC 99,99,99      FUNC TIME.
              03 COLUMN 52        PIC X(16) VALUE 'DATA-CENTRO S.A.'.
              03 COLUMN 120       PIC X(6)  VALUE 'PAGINA'.
              03 COLUMN 127       PIC ZZ,ZZ9 SOURCE PAGE-COUNTER IN
                                              REPORTE-PDI-PFP.
       01  TYPE IS RF.
           02 LINE IS PLUS 1.
              03 COLUMN 37        PIC X(14) VALUE "--------------".
           02 LINE IS PLUS 1.
              03 COLUMN 37        PIC QQQ,QQQ,QQ9.99
                                            SUM  APORTACION-PACTADA.
              03 COLUMN 52        PIC X(16) VALUE "= TOTAL AGENCIAS".
           02 LINE IS PLUS 1.
              03 COLUMN 37        PIC ZZZ,ZZZ,ZZZ
                                            COUNT OF DETAILLINE.
              03 COLUMN 52        PIC X(55) VALUE
              "= TOTAL CUENTAS ENLACE CON APORTES SUPERIORES A Q500.00".
           02  LINE PLUS 3.
               03 COLUMN 50       PIC X(22)
                                  VALUE 'ESTA ES LA ULTIMA HOJA'.
           02  LINE PLUS 1.
               03 COLUMN 50       PIC X(23)
                                  VALUE 'ESTE REPORTE CONSTA DE '.
               03 COLUMN 73       PIC Z,ZZZ SOURCE PAGE-COUNTER IN
                                              REPORTE-PDI-PFP.
               03 COLUMN 80       PIC X(07) VALUE 'PAGINAS'.
       PROCEDURE DIVISION.
       100-SECCION-PRINCIPAL SECTION.
           PERFORM 100-ABRIR-VALIDAR-DATASETS
           PERFORM 110-VERIFICAR-INTEGRIDAD-FS
           PERFORM 200-CARGAR-TABLAS-EN-MEMORIA
           PERFORM 300-SORT-DATOS-CORRECTOS
           TERMINATE REPORTE-PDI-PFP
           PERFORM 800-ESTADISTICAS
           PERFORM XXX-CIERRA-ARCHIVOS
           STOP RUN.
       100-SECCION-PRINCIPAL-E. EXIT.

       100-ABRIR-VALIDAR-DATASETS           SECTION.
           MOVE "EDU33016" TO PROGRAMA
           ACCEPT WKS-FECHA FROM SYSIN
           OPEN INPUT         FPMCTAN
           OPEN INPUT         FPMCTA2
           OPEN INPUT         CFTGEN
           OPEN OUTPUT        REPORTE.
       100-ABRIR-VALIDAR-DATASETS-E.        EXIT.

       110-VERIFICAR-INTEGRIDAD-FS          SECTION.
           IF (FS-FPMCTAN = 97) AND (FS-FPMCTA2 = 97) AND
              (FS-CFTGEN  = 97) AND (FS-REPORTE = 97)
              DISPLAY
              "********************************************************"
              UPON CONSOLE
              DISPLAY
              "*      >>>   APERTURA DE ARCHIVOS E/S EXITOSA   <<<    *"
              UPON CONSOLE
              DISPLAY
              "********************************************************"
              UPON CONSOLE
              MOVE ZEROS TO FS-FPMCTAN
              MOVE ZEROS TO FS-FPMCTA2
              MOVE ZEROS TO FS-CFTGEN
              MOVE ZEROS TO FS-REPORTE
              INITIATE REPORTE-PDI-PFP
           END-IF

           IF FS-FPMCTAN NOT EQUAL 0
              MOVE 'OPEN'    TO ACCION
              MOVE SPACES    TO LLAVE
              MOVE 'FPMCTAN' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                               LLAVE, FS-FPMCTAN, FSE-FPMCTAN
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "     >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO FPMCTAN <<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-FPMCTA2 NOT EQUAL 0
              MOVE 'OPEN'    TO ACCION
              MOVE SPACES    TO LLAVE
              MOVE 'FPMCTA2' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                    LLAVE, FS-FPMCTA2, FSE-FPMCTA2
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO FPMCTA2 <<<"
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
       110-VERIFICAR-INTEGRIDAD-FS-E.       EXIT.

       200-CARGAR-TABLAS-EN-MEMORIA         SECTION.
           MOVE "CFT0060000000000" TO CFTB-LLAVE
           START CFTGEN
              KEY IS >= CFTB-LLAVE
           END-START
           MOVE "CFTGEN  " TO WKS-ARCHIVO-ANALIZAR
           PERFORM 700-ERRORES-LEC-SECUENCIAL

           READ CFTGEN NEXT RECORD
           PERFORM UNTIL WKS-FIN-CARGA-AGE
                   EVALUATE CFTB-CODIGO-TABLA
                   WHEN 006
                        ADD 1 TO WKS-TABLA1-OCCURS
                        ADD 1 TO WKS-CONTADOR-AGENCIAS
                        MOVE CFTB-CORRELATIVO TO WKS-COD-AGE(IDX-AGE)
                        MOVE CFTB-RESTO TO WKS-NOMBRE-AGE(IDX-AGE)
                        SET IDX-AGE UP BY 1
                   WHEN OTHER
                        SET WKS-FIN-CARGA-AGE TO TRUE
                   END-EVALUATE
                   READ CFTGEN NEXT RECORD
                   MOVE "CFTGEN  " TO WKS-ARCHIVO-ANALIZAR
                   PERFORM 700-ERRORES-LEC-SECUENCIAL
           END-PERFORM.
       200-CARGAR-TABLAS-EN-MEMORIA-E.      EXIT.

       300-SORT-DATOS-CORRECTOS SECTION.
           SORT WORKFILE ON ASCENDING KEY CODIGO-AGENCIA
              INPUT  PROCEDURE IS 300-BUSCAR-CUENTAS-LLAVE
              OUTPUT PROCEDURE IS 3OO-GENERAR-REPORTE-PDI.
       300-SORT-DATOS-CORRECTOS-E. EXIT.

       300-BUSCAR-CUENTAS-LLAVE             SECTION.
           READ FPMCTAN
                AT END MOVE "SI" TO WKS-CICLOS
           END-READ

           MOVE "FPMCTAN" TO WKS-ARCHIVO-ANALIZAR
                 PERFORM 700-ERRORES-LEC-SECUENCIAL

           PERFORM UNTIL WKS-CICLOS = "SI"
               ADD 1 TO WKS-LECTURAS-TOTALES
               IF   FPMC-CUENTA-ENLACE        >  ZEROS AND
                    FPMC-SITUACION-CUENTA     =  32    AND
                    FPMC-SITUACION-JURIDICA   =  1     AND
                    FPMC-APORTACION-PACTADA   =  0
                    ADD 1 TO WKS-CTA-PRINCIPAL
                    PERFORM 310-BUSQUEDA-RANDOM
               END-IF
               READ FPMCTAN
                   AT END MOVE "SI" TO WKS-CICLOS
               END-READ
               IF FS-FPMCTAN NOT EQUAL 0 AND 10
                  MOVE "FPMCTAN" TO WKS-ARCHIVO-ANALIZAR
                  PERFORM 700-ERRORES-LEC-SECUENCIAL
               END-IF
           END-PERFORM.
       300-BUSCAR-CUENTAS-LLAVE-E.          EXIT.

       310-BUSQUEDA-RANDOM                  SECTION.
           MOVE FPMC-CUENTA-ENLACE   TO FPM2-LLAVE
           READ FPMCTA2
                KEY IS FPM2-LLAVE
           END-READ
           IF FS-FPMCTA2 NOT EQUAL 0 AND 23
              MOVE "FPMCTA2" TO WKS-ARCHIVO-ANALIZAR
              PERFORM 700-ERRORES-LEC-SECUENCIAL
           END-IF

           EVALUATE FS-FPMCTA2
              WHEN 0
                   ADD 1 TO WKS-CTA-ENLACE
                   PERFORM 320-BUSQUEDA-DATOS-CUENTA
              WHEN 23
                   ADD 1 TO WKS-CTA-NOT-FOUND
              WHEN OTHER
                   MOVE "FPMCTA2" TO WKS-ARCHIVO-ANALIZAR
                   PERFORM 700-ERRORES-LEC-SECUENCIAL
           END-EVALUATE
           INITIALIZE REG-FPMCT2.
       310-BUSQUEDA-RANDOM-E.               EXIT.

       320-BUSQUEDA-DATOS-CUENTA            SECTION.
           IF FPM2-APORTACION-PACTADA > 500
              ADD 1 TO WKS-CTA-ENLACE-VERIFI
              MOVE FPMC-LLAVE              TO  CUENTA-PRINCIPAL
              MOVE FPM2-LLAVE              TO  CUENTA-SECUNDARIA
              MOVE FPM2-APORTACION-PACTADA TO  APORTACION-PACTADA
              MOVE FPM2-PLAZO-DEL-FONDO    TO  PLAZO-FONDO
              MOVE FPM2-TASA-INTERES       TO  TASA-INTERESES
              MOVE FPM2-NUMERO-EJECUTIVO   TO  NUMERO-EJECUTIVO
              MOVE FPM2-CONTACTO-GERENCIAL TO  CONTACTO-GERENCIAL
              MOVE FPM2-CARGO-CONTACTO-GERENCIAL
                                           TO  CARGO-CONTACTO-GERENCIAL
              MOVE FPM2-AGENCIA            TO  CODIGO-AGENCIA
              SEARCH ALL WKS-TABLA-AGE
                AT END MOVE SPACES TO  NOMBRE-AGENCIA

                WHEN WKS-COD-AGE(IDX-AGE) = FPM2-AGENCIA
                MOVE WKS-NOMBRE-AGE(IDX-AGE) TO   NOMBRE-AGENCIA
              END-SEARCH
              RELEASE WORKAREA
           ELSE
              ADD 1 TO WKS-CTA-APORTE-MENOR
           END-IF.
       320-BUSQUEDA-DATOS-CUENTA-E. EXIT.

       3OO-GENERAR-REPORTE-PDI              SECTION.
      *    PRIMER LECTURA
           RETURN WORKFILE
                AT END SET ENDOFFILE TO TRUE
           END-RETURN.
           DISPLAY WORKAREA
           PERFORM UNTIL ENDOFFILE
              GENERATE DETAILLINE
              RETURN WORKFILE
                 AT END SET ENDOFFILE TO TRUE
              END-RETURN
              DISPLAY "DATOS QUE SE TREAEN DEL RETURN " WORKAREA
           END-PERFORM.
       3OO-GENERAR-REPORTE-PDI-E.           EXIT.

       700-ERRORES-LEC-SECUENCIAL       SECTION.
           EVALUATE WKS-ARCHIVO-ANALIZAR
           WHEN "FPMCTAN"
                IF FS-FPMCTAN NOT EQUAL 0
                   MOVE 'READ' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'FPMCTAN' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-FPMCTAN, FSE-FPMCTAN
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                  "   >>> ALGO SALIO MAL AL LEER EL ARCHIVO FPMCTAN <<<"
                      UPON CONSOLE
                   DISPLAY
                      "          !!! VERIFICAR DETALLES EN SPOOL !!!"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           WHEN "FPMCTA2"
                IF FS-FPMCTA2 NOT EQUAL 0
                   MOVE 'READ' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'FPMCTA2' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-FPMCTA2, FSE-FPMCTA2
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                  "   >>> ALGO SALIO MAL AL LEER EL ARCHIVO FPMCTA2 <<<"
                      UPON CONSOLE
                   DISPLAY
                      "          !!! VERIFICAR DETALLES EN SPOOL !!!"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           WHEN "CFTGEN  "
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
       800-ESTADISTICAS                     SECTION.
           DISPLAY
           "**********************************************************"
           UPON CONSOLE
           DISPLAY
           "*                       ESTADISTICAS                     *"
           UPON CONSOLE
           DISPLAY
           "**********************************************************"
           UPON CONSOLE
           MOVE WKS-CONTADOR-AGENCIAS TO WKS-TOTALES-EDIT
           DISPLAY
           "  TOTAL AGENCIAS CARGADAS EN MEMORIA    : " WKS-TOTALES-EDIT
           UPON CONSOLE

           INITIALIZE WKS-TOTALES-EDIT
           MOVE  WKS-LECTURAS-TOTALES TO WKS-TOTALES-EDIT
           DISPLAY
           "  CUENTAS LEIDAS EN TOTAL  FPMCTA       : " WKS-TOTALES-EDIT
           UPON CONSOLE

           INITIALIZE WKS-TOTALES-EDIT
           MOVE  WKS-CTA-PRINCIPAL TO WKS-TOTALES-EDIT
           DISPLAY
           "  CUENTAS PRINCIPALES ENCONTRADAS       : " WKS-TOTALES-EDIT
           UPON CONSOLE

           INITIALIZE WKS-TOTALES-EDIT
           MOVE WKS-CTA-ENLACE TO WKS-TOTALES-EDIT
           DISPLAY
           "  CUENTAS ENLAZADAS ENCONTRADAS         : " WKS-TOTALES-EDIT
           UPON CONSOLE

           INITIALIZE WKS-TOTALES-EDIT
           MOVE WKS-CTA-NOT-FOUND TO WKS-TOTALES-EDIT
           DISPLAY
           "  CUENTAS ENLAZADAS NO ENCONTRADAS      : " WKS-TOTALES-EDIT
           UPON CONSOLE

           INITIALIZE WKS-TOTALES-EDIT
           MOVE WKS-CTA-APORTE-MENOR TO WKS-TOTALES-EDIT
           DISPLAY
           "  CUENTAS ENLACE APORTACION < Q500      : " WKS-TOTALES-EDIT
           UPON CONSOLE

           INITIALIZE WKS-TOTALES-EDIT
           MOVE WKS-CTA-ENLACE-VERIFI TO WKS-TOTALES-EDIT
           DISPLAY
           "  CUENTAS ENLACE QUE CUMPLEN REQUISITOS : " WKS-TOTALES-EDIT
           UPON CONSOLE

           DISPLAY
           "**********************************************************"
           UPON CONSOLE.
       800-ESTADISTICAS-E. EXIT.
       XXX-CIERRA-ARCHIVOS                  SECTION.
           CLOSE  FPMCTAN
           CLOSE  FPMCTA2
           CLOSE  CFTGEN
           CLOSE  REPORTE.
       XXX-CIERRA-ARCHIVOS-E.               EXIT.