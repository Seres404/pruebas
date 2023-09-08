      ******************************************************************
      * FECHA       : 28/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33019                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA LEE SECUENCIALMENTE EL ARCHIVO MA- *
      *             : ESTRO MOMAES POR MEDIO DE SU PATH, RECUPERA LAS  *
      *             : CUENTAS DE CLIENTE NATURAL Y GENERA UN REPORTE   *
      *             : DE ESTAS, NOMBRE DEL CLIENTE Y ESTADO DE LA CUENT*
      * ARCHIVOS    : MOMAES, MOTGEN, CFCNAT, REPORTE                  *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      * PROGRAMA(S) : RUTINA CFGD2QL2, EDIT DE NOMBRES                 *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     EDU33019.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT MOMAES ASSIGN TO MOMAES
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS DYNAMIC
                   RECORD KEY    IS MOMA-LLAVE
                   ALTERNATE RECORD KEY IS MOMA-CODIGO-UNIVERSAL
                                    WITH DUPLICATES
                   FILE STATUS   IS  FS-MOMAES
                                     FSE-MOMAES.
            SELECT CFCNAT ASSIGN TO CFCNAT
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS RANDOM
                   RECORD KEY    IS CFCN-CODIGO-CLIENTE
                   FILE STATUS   IS FS-CFCNAT
                                    FSE-CFCNAT.
            SELECT MOTGEN  ASSIGN TO MOTGEN
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS DYNAMIC
                   RECORD KEY    IS MOTC-LLAVE
                   FILE STATUS   IS FS-MOTGEN
                                    FSE-MOTGEN.
            SELECT REPORTE  ASSIGN TO SYSOO7
                   FILE STATUS IS FS-REPORTE.
       DATA DIVISION.
       FILE SECTION.
       FD  MOMAES.
           COPY MOMAES.
       FD  CFCNAT.
           COPY CFCNAT REPLACING CFCNAT BY REG-CFCNAT.
       FD  MOTGEN.
           COPY MOTGEN.
       FD  REPORTE
           REPORT IS CUENTAS-REPORTE.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *         VARIABLES PARA EVALUAR INTEGRIDAD DE ARCHIVOS          *
      ******************************************************************
       01  FS-MOMAES                   PIC 9(02) VALUE ZEROS.
       01  FS-CFCNAT                   PIC 9(02) VALUE ZEROS.
       01  FS-MOTGEN                   PIC 9(02) VALUE ZEROS.
       01  FS-REPORTE                  PIC 9(02) VALUE ZEROS.
      *                VARIABLES DE FILE STATUS EXTENDED               *
       01  FSE-MOMAES.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
       01  FSE-CFCNAT.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
       01  FSE-MOTGEN.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
      *Variables de Rutina para control de File Status Extendido
       01  PROGRAMA                    PIC X(08) VALUE SPACES.
       01  ARCHIVO                     PIC X(08) VALUE SPACES.
       01  ACCION                      PIC X(10) VALUE SPACES.
       01  LLAVE                       PIC X(32) VALUE SPACES.

      ******************************************************************
      *        VARIABLES DE OPERACION, CONTADORES, DISPARADORES        *
      ******************************************************************
       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-PROGRAMA              PIC X(08) VALUE "EDU33019".
          02 WKS-NOMBRE-EDITADO        PIC X(40) VALUE SPACES.
          02 WKS-SITUACION-CUENTA      PIC X(15) VALUE SPACES.
          02 WKS-ARCHIVO-ANALIZAR      PIC X(08) VALUE SPACES.
          02 WKS-TABLA1-SIT            PIC 9(02) VALUE ZEROS.
          02 WKS-NOT-FOUNF-CIF         PIC 9(03) VALUE ZEROS.
          02 WKS-CLIENTES-NAT          PIC 9(04) VALUE ZEROS.
          02 WKS-CLIENTES-JUR          PIC 9(04) VALUE ZEROS.
          02 WKS-REG-LEIDOS            PIC 9(04) VALUE ZEROS.
          02 WKS-FECHA.
              03 WKS-DIA               PIC 9(02) VALUE ZEROS.
              03 FILLER1               PIC X(01) VALUE '/'.
              03 WKS-MES               PIC 9(02) VALUE ZEROS.
              03 FILLER2               PIC X(01) VALUE '/'.
              03 WKS-ANIO              PIC 9(04) VALUE ZEROS.
          02 WKS-DISPARADORES          PIC 9(01).
             88 WKS-FIN-MOTGEN                   VALUE 1.
             88 WKS-FIN-MOMAES                   VALUE 2.
           COPY CFNM.
      ******************************************************************
      *        ============== TABLA DE SITUACION ============          *
      ******************************************************************
       01 WKS-TABLAS.
      *--> TABLA SITUACIONES MOT-003
          02 WKS-TABLA-SIT OCCURS 1 TO 99
                           DEPENDING ON  WKS-TABLA1-SIT
                           ASCENDING KEY WKS-COD-SIT
                           INDEXED   BY  IDX-SIT.
             03 WKS-COD-SIT             PIC 9(02).
             03 WKS-DETALLE-SIT         PIC X(15).
      ******************************************************************
      *                  MAQUETACION REPORTE DE SALIDA                 *
      ******************************************************************
       REPORT SECTION.
       RD  CUENTAS-REPORTE
           CONTROL ARE FINAL
           PAGE LIMIT IS 51 LINES
           HEADING 1
           FIRST DETAIL 7
           LAST DETAIL 47
           FOOTING 49.
      ******************************************************************
      *                     MAQUETACION PAGE HEADER                    *
      ******************************************************************
       01  TYPE IS PH.
           02 LINE 1.
              03 COLUMN   1             PIC X(22) VALUE
                 'BANCO INDUSTRIAL, S.A.'.
              03 COLUMN  49             PIC X(37) VALUE
                 'REPORTE DE CUENTAS CLIENTES NATURALES'.
              03 COLUMN 115             PIC X(17) VALUE
                 'S E M I L L E R O'.
           02 LINE 2.
              03 COLUMN   1             PIC X(25) VALUE
                 'EDU33019    00.23042023.R'.
              03 COLUMN  53             PIC X(20) VALUE
                 'FECHA DEL MOVIMIENTO'.
              03 COLUMN  74             PIC X(10) SOURCE WKS-FECHA.
              03 COLUMN 120             PIC X(06) VALUE 'PAGINA'.
              03 COLUMN 129             PIC Z(04) SOURCE PAGE-COUNTER IN
                                                  CUENTAS-REPORTE.
           02 LINE 3.
              03 COLUMN  01             PIC X(132) VALUE ALL '='.
           02 LINE 4.
              03 COLUMN  01             PIC X(50) VALUE
                 "* NUMERO DE CUENTA  CIF CLIENTE  FECHA DE APERTURA".
              03 COLUMN  64             PIC X(18) VALUE
                 "NOMBRE DEL CLIENTE".
              03 COLUMN  99             PIC X(34) VALUE
                 "CODIGO SIT.  ESTADO DE LA CUENTA *".
           02 LINE 5.
              03 COLUMN  01             PIC X(132) VALUE ALL '='.
      ******************************************************************
      *                     MAQUETACION LINEA DETALLE                  *
      ******************************************************************
       01  DETAILLINE TYPE IS DETAIL.
           02 LINE IS PLUS 1.
              03 COLUMN  05             PIC 9(10) SOURCE
                 MOMA-NUMERO-CUENTA.
              03 COLUMN  22             PIC 9(08) SOURCE
                 MOMA-CODIGO-UNIVERSAL.
              03 COLUMN  37             PIC 99/99/9999 SOURCE
                 MOMA-FECHA-APERTURA.
              03 COLUMN  54             PIC X(40) SOURCE
                 WKS-NOMBRE-EDITADO.
              03 COLUMN 104             PIC X(01) SOURCE
                 MOMA-SITUACION-CUENTA.
              03 COLUMN 117             PIC X(10) SOURCE
                 WKS-SITUACION-CUENTA.
      ******************************************************************
      *                     MAQUETACION PAGE FOOTING                   *
      ******************************************************************
       01 TYPE IS CF FINAL.
           02 LINE IS PLUS 1.
              03 COLUMN 46         PIC X(35) VALUE
              "========== R E S U M E N ==========".
           02 LINE PLUS 1.
              03 COLUMN 43         PIC X(35) VALUE
              'TOTAL CUENTAS CLIENTES NATURALES  ='.
              03 COLUMN 80         PIC ZZZZ  COUNT OF DETAILLINE.
           02 LINE PLUS 1.
              03 COLUMN 01         PIC X(132) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION PAGE FOOTING                   *
      ******************************************************************
       01  TYPE IS PF.
           02  LINE PLUS 0.
              03 COLUMN 1         PIC X(25)
                                  VALUE 'FECHA Y HORA DE OPERACION'.
              03 COLUMN 28        PIC 99/99/9999
                                  FUNC DATE.
              03 COLUMN 42        PIC 99'H.'99'M.'99'S'
                                  FUNC TIME.
              03 COLUMN 56        PIC X(16) VALUE 'DATA-CENTRO S.A.'.
              03 COLUMN 120       PIC X(6)  VALUE 'PAGINA'.
              03 COLUMN 129       PIC ZZZZ SOURCE PAGE-COUNTER IN
                                           CUENTAS-REPORTE.
           02 LINE PLUS 1.
              03 COLUMN 01        PIC X(132) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION REPORT FINAL                   *
      ******************************************************************
       01  TYPE IS RF.
           02 LINE PLUS 3.
             03 COLUMN  01        PIC X(22) VALUE
             "ESTA ES LA ULTIMA HOJA".
             03 COLUMN  97        PIC X(22) VALUE
             "ESTE REPORTE CONSTA DE".
             03 COLUMN 119        PIC Z,ZZZ SOURCE PAGE-COUNTER IN
                                           CUENTAS-REPORTE.
             03 COLUMN 126        PIC X(07)  VALUE 'PAGINAS'.
       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 110-APERTURA-DATASET
           INITIATE    CUENTAS-REPORTE
           PERFORM 200-CARGA-TABLAS-MEMORIA
           PERFORM 300-BARRIDO-MOMAES
           TERMINATE   CUENTAS-REPORTE
           PERFORM 800-ESTADISTICAS
           PERFORM XXX-CIERRA-ARCHIVOS
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

       110-APERTURA-DATASET SECTION.
           OPEN INPUT  MOMAES
           OPEN INPUT  CFCNAT
           OPEN INPUT  MOTGEN
           OPEN OUTPUT REPORTE
           ACCEPT WKS-FECHA FROM SYSIN
           PERFORM 120-EVALUA-FS-INTEGRIDAD.
       110-APERTURA-DATASET-E. EXIT.

       120-EVALUA-FS-INTEGRIDAD SECTION.
           MOVE 'EDU34019' TO PROGRAMA
           IF (FS-REPORTE = 97) AND (FS-MOMAES = 97) AND
              (FS-CFCNAT = 97) AND (FS-MOTGEN = 97)
              MOVE ZEROS TO FS-REPORTE
              MOVE ZEROS TO FS-MOMAES
              MOVE ZEROS TO FS-CFCNAT
              MOVE ZEROS TO FS-MOTGEN
           END-IF
           IF FS-MOMAES NOT EQUAL 0
              MOVE 'OPEN'    TO ACCION
              MOVE SPACES    TO LLAVE
              MOVE 'MOMAES'  TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                    LLAVE, FS-MOMAES, FSE-MOMAES
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO MOMAES <<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-CFCNAT NOT EQUAL 0
              MOVE 'OPEN'    TO ACCION
              MOVE SPACES    TO LLAVE
              MOVE 'CFCNAT'  TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                    LLAVE, FS-CFCNAT, FSE-CFCNAT
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO CFCNAT <<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-MOTGEN NOT EQUAL 0
              MOVE 'OPEN'    TO ACCION
              MOVE SPACES    TO LLAVE
              MOVE 'MOTGEN ' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                    LLAVE, FS-CFCNAT, FSE-CFCNAT
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO MOTGEN <<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           PERFORM 700-ERRORES-LEC-SECUENCIAL.
       120-EVALUA-FS-INTEGRIDAD-E. EXIT.

       200-CARGA-TABLAS-MEMORIA SECTION.
           MOVE "MOT0030000000000" TO MOTC-LLAVE
           START MOTGEN
              KEY IS >= MOTC-LLAVE
           END-START
           MOVE "MOTGEN" TO WKS-ARCHIVO-ANALIZAR
           PERFORM 700-ERRORES-LEC-SECUENCIAL

           READ MOTGEN NEXT RECORD
           PERFORM UNTIL WKS-FIN-MOTGEN
                   EVALUATE MOTC-CODIGO-TABLA
                   WHEN 003
                       ADD 1 TO WKS-TABLA1-SIT
                       MOVE MOTC-CORRELATIVO TO WKS-COD-SIT(IDX-SIT)
                       MOVE MOTC-RESTO       TO WKS-DETALLE-SIT(IDX-SIT)
                       SET IDX-SIT UP BY 1
                   WHEN OTHER
                       SET WKS-FIN-MOTGEN TO TRUE
                   END-EVALUATE
                   READ MOTGEN   NEXT RECORD
                   MOVE "MOTGEN" TO WKS-ARCHIVO-ANALIZAR
                   PERFORM 700-ERRORES-LEC-SECUENCIAL
           END-PERFORM.
       200-CARGA-TABLAS-MEMORIA-E. EXIT.

       300-BARRIDO-MOMAES SECTION.
            MOVE ZEROS TO MOMA-CODIGO-UNIVERSAL
            START MOMAES
                  KEY IS > MOMA-CODIGO-UNIVERSAL
            END-START
            MOVE "MOMAES" TO WKS-ARCHIVO-ANALIZAR
            PERFORM 700-ERRORES-LEC-SECUENCIAL

            READ MOMAES NEXT RECORD
            IF FS-MOMAES = 10
                 SET WKS-FIN-MOMAES TO TRUE
            END-IF
            IF FS-MOMAES NOT EQUAL 10 AND FS-MOMAES NOT EQUAL 0
                 MOVE "MOMAES" TO WKS-ARCHIVO-ANALIZAR
                 PERFORM 700-ERRORES-LEC-SECUENCIAL
            END-IF

            PERFORM UNTIL WKS-FIN-MOMAES
                   ADD 1 TO  WKS-REG-LEIDOS
                   IF  MOMA-TIPO-CLIENTE  = 1
                      ADD 1 TO WKS-CLIENTES-NAT
                      PERFORM 310-BUSQUEDA-DATOS-CLIENTE
                      PERFORM 310-SITUACION-CUENTA
                      GENERATE DETAILLINE
                   ELSE
                      ADD 1 TO WKS-CLIENTES-JUR
                   END-IF
                   READ MOMAES NEXT RECORD
                   IF FS-MOMAES = 10
                      SET WKS-FIN-MOMAES TO TRUE
                   END-IF
                   IF FS-MOMAES NOT EQUAL 10 AND FS-MOMAES NOT EQUAL 0
                      MOVE "MOMAES" TO WKS-ARCHIVO-ANALIZAR
                      PERFORM 700-ERRORES-LEC-SECUENCIAL
                   END-IF
           END-PERFORM.
        300-BARRIDO-MOMAES-E. EXIT.

       310-BUSQUEDA-DATOS-CLIENTE SECTION.
           MOVE MOMA-CODIGO-UNIVERSAL TO CFCN-CODIGO-CLIENTE
           READ CFCNAT
               KEY IS CFCN-CODIGO-CLIENTE
           END-READ
           EVALUATE FS-CFCNAT
              WHEN 0
                  PERFORM 310-NOMBRES-FORMATEADOS
              WHEN 23
                  INITIALIZE CFNM-CAMPO-RETORNO
                  INITIALIZE WKS-NOMBRE-EDITADO
                  ADD 1 TO WKS-NOT-FOUNF-CIF
              WHEN OTHER
                  MOVE "MOMAES" TO WKS-ARCHIVO-ANALIZAR
                  PERFORM 700-ERRORES-LEC-SECUENCIAL
           END-EVALUATE.
       310-BUSQUEDA-DATOS-CLIENTE-E. EXIT.

       310-SITUACION-CUENTA SECTION.
           INITIALIZE WKS-SITUACION-CUENTA
           SEARCH ALL WKS-TABLA-SIT
              AT END MOVE SPACES TO WKS-SITUACION-CUENTA
              WHEN  WKS-COD-SIT(IDX-SIT) = MOMA-SITUACION-CUENTA
                    MOVE WKS-DETALLE-SIT(IDX-SIT) TO
                    WKS-SITUACION-CUENTA
           END-SEARCH.
       310-SITUACION-CUENTA-E. EXIT.

       310-NOMBRES-FORMATEADOS SECTION.
            INITIALIZE CFNM-CAMPO-RETORNO
            INITIALIZE WKS-NOMBRE-EDITADO
            MOVE 'N'                 TO CFNM-ORDENAMIENTO
            MOVE 40                  TO CFNM-RANGO-LONGITUD
            MOVE 3                   TO CFNM-RANGO-APELLIDOS
            MOVE CFCN-NOMBRE-CLIENTE TO CFNM-NOMBRE-COMPLETO
            CALL 'CFGD2QL2' USING CFNM
            MOVE CFNM-CAMPO-RETORNO  TO WKS-NOMBRE-EDITADO.
       310-NOMBRES-FORMATEADOS-E. EXIT.

       700-ERRORES-LEC-SECUENCIAL SECTION.
           EVALUATE WKS-ARCHIVO-ANALIZAR
           WHEN "MOMAES"
                IF FS-MOMAES NOT EQUAL 0
                   MOVE 'OPEN' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'MOMAES' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-MOMAES, FSE-MOMAES
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO MOMAES <<<"
                      UPON CONSOLE
                   DISPLAY
                      "            !!! VERIFICAR DETALLES EN SPOOL !!!"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           WHEN "CFCNAT"
                IF FS-CFCNAT NOT EQUAL 0
                   MOVE 'OPEN' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'CFCNAT' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-CFCNAT, FSE-CFCNAT
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO CFCNAT <<<"
                      UPON CONSOLE
                   DISPLAY
                      "            !!! VERIFICAR DETALLES EN SPOOL !!!"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           WHEN "MOTGEN"
                IF FS-MOTGEN NOT EQUAL 0
                   MOVE 'OPEN' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'MOTGEN ' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-CFCNAT, FSE-CFCNAT
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                "    >>> ALGO SALIO MAL AL ABRIR EL ARCHIVO MOTGEN <<<"
                      UPON CONSOLE
                   DISPLAY
                      "            !!! VERIFICAR DETALLES EN SPOOL !!!"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           END-EVALUATE
           INITIALIZE WKS-ARCHIVO-ANALIZAR.
       700-ERRORES-LEC-SECUENCIAL-E.                     EXIT.

       800-ESTADISTICAS SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                      ESTADISTICAS                      *"
           DISPLAY
           "**********************************************************"
           DISPLAY
           " REGISTROS LEIDOS DESDE MOMAES : " WKS-REG-LEIDOS
           DISPLAY
           " CLIENTES NATURALES            : " WKS-CLIENTES-NAT
           DISPLAY
           " CLIENTES JURIDICOS            : " WKS-CLIENTES-JUR
           DISPLAY
           "**********************************************************".
       800-ESTADISTICAS-E. EXIT.

       XXX-CIERRA-ARCHIVOS SECTION.
           CLOSE REPORTE
           CLOSE MOMAES
           CLOSE CFCNAT
           CLOSE MOTGEN.
       XXX-CIERRA-ARCHIVOS-E.            EXIT.