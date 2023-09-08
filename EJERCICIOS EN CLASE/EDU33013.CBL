      ******************************************************************
      * FECHA       : 17/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33013                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA LEE LOS DATOS DE UN PS, ESTOS SON  *
      *             : EL NUMERO DE CUENTA DE CLIENTE, EN BASE A ESTO   *
      *             : EL PROGRAMA GENERA UN REPORTE EN SYSOUT CON LOS  *
      *             : DETALLES DE LA CUENTA, TAMBIEN LOS DETALLES DEL  *
      *             : CLIENTE COMO SU NOMBRE Y DPI                     *
      * ARCHIVOS    : MOMAES (VSAM KSDS)                               *
      *             : CFCNAT                      (VSAM KSDS)          *
      *             : MOTGEN                      (VSAM KSDS)          *
      *             : CFTGEN                      (VSAM KSDS)          *
      *             : ENTRADA                     (PS ORDENADO)        *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      * PROGRAMA(S) : RUTINA EDU3301R, VERIFICACION DE DPI             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     EDU33013.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT ENTRADA ASSIGN TO ENTRADA
                                    FS-ENTRADA.
            SELECT MOMAES  ASSIGN TO MOMAES
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS RANDOM
                   RECORD KEY    IS MOMA-LLAVE
                   FILE STATUS   IS FS-MOMAES
                                    FSE-MOMAES.
            SELECT CFCNAT ASSIGN TO CFCNAT
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS RANDOM
                   RECORD KEY    IS CFCN-CODIGO-CLIENTE
                   FILE STATUS   IS FS-CFCNAT
                                    FSE-CFCNAT.
            SELECT MOTGEN  ASSIGN TO MOTGEN
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS RANDOM
                   RECORD KEY    IS MOTC-LLAVE
                   FILE STATUS   IS FS-MOTGEN
                                    FSE-MOTGEN.
            SELECT CFTGEN ASSIGN TO CFTGEN
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS RANDOM
                   RECORD KEY    IS CFTB-LLAVE
                   FILE STATUS   IS FS-CFTGEN
                                    FSE-CFTGEN.
       DATA DIVISION.
       FILE SECTION.
       FD ENTRADA.
       01 REGISTRO-LLAVE.
          88 FIN-DATOS-ENTRADA         VALUE HIGH-VALUES.
          88 CUENTA-EXISTE             VALUE HIGH-VALUES.
          02 REGISTRO-ENTRADA          PIC X(10).
       FD  MOMAES.
           COPY MOMAES.
       FD  CFCNAT.
           COPY CFCNAT REPLACING CFCNAT BY REG-CFCNAT.
       FD  MOTGEN.
           COPY MOTGEN.
       FD  CFTGEN.
           COPY CFTGEN REPLACING CFTGEN BY REG-CFTGEN.
       WORKING-STORAGE SECTION.
      *VARIABLES DE FILE STATUS NORMAL
       01  FS-ENTRADA                  PIC 9(02) VALUE ZEROS.
       01  FS-MOMAES                   PIC 9(02) VALUE ZEROS.
       01  FS-CFCNAT                   PIC 9(02) VALUE ZEROS.
       01  FS-MOTGEN                   PIC 9(02) VALUE ZEROS.
       01  FS-CFTGEN                   PIC 9(02) VALUE ZEROS.
      *VARIABLES DE FILE STATUS EXTENDED
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
       01  FSE-CFTGEN.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
      *Variables de Rutina para control de File Status Extendido
       01  PROGRAMA                    PIC X(08) VALUE SPACES.
       01  ARCHIVO                     PIC X(08) VALUE SPACES.
       01  ACCION                      PIC X(10) VALUE SPACES.
       01  LLAVE                       PIC X(32) VALUE SPACES.
      *VARIABLES RUTINA PARA VERIFICAR DPI
       01  WKS-DPI-EDITADO.
           02 WKS-DPI-CORREL           PIC  9(08) VALUE ZEROS.
           02 FILLER                   PIC  X(01) VALUE "-".
           02 WKS-DPI-VERIFICADOR      PIC  9(01) VALUE ZEROS.
           02 FILLER                   PIC  X(01) VALUE "-".
           02 WKS-DPI-DEPARTAMENTO     PIC  9(02) VALUE ZEROS.
           02 WKS-DPI-MUNICIPIO        PIC  9(02) VALUE ZEROS.
       01  WKS-CODIGO-RETORNO          PIC  9(02) VALUE ZEROS.
       01  WKS-DESCRIPCION             PIC  X(50) VALUE SPACES.
      *ContadoresWKS-ARCHIVO-ANALIZAR y Controladores
       01  WKS-CIF-ESTADO              PIC X(25) VALUE SPACES.
       01  WKS-ARCHIVO-ANALIZAR        PIC X(06) VALUE SPACES.
       01  WKS-FIN-ENTRADA             PIC 9(02) VALUE ZEROS.
       01  WKS-REG-LEIDOS-ENTRADA      PIC 9(02) VALUE ZEROS.
       01  WKS-REG-ERROR404            PIC 9(02) VALUE ZEROS.
       01  WKS-REG-FOUND               PIC 9(02) VALUE ZEROS.
       01  WKS-REG-CON-ERROR           PIC 9(02) VALUE ZEROS.
       01  WKS-GUIONES                 PIC X(60) VALUE ALL "=".
       01  WKS-FECHA-APERTURA          PIC B99/99/9999 VALUE ZEROS.
       01  WKS-SITUACION-CUENTA        PIC x(10) VALUE SPACES.
       01  WKS-AGENCIA-APERTURA        PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 200-ABRE-DATASETS
           PERFORM 300-PROCESA-ARCHIVOS
           PERFORM 510-ESTADISTICAS
           PERFORM XXX-CIERRA-ARCHIVOS
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

      *-----------> SERIE 200 ENFOCADA EN LA APERTURA DE ARCHIVOS
       200-ABRE-DATASETS SECTION.
           MOVE 'EDU33009'   TO PROGRAMA
           OPEN INPUT ENTRADA
           OPEN INPUT MOMAES
           OPEN INPUT CFCNAT
           OPEN INPUT MOTGEN
           OPEN INPUT CFTGEN
           PERFORM 210-EVALUA-FS-INTEGRIDAD.
       200-ABRE-DATASETS-E. EXIT.

       210-EVALUA-FS-INTEGRIDAD SECTION.
           IF (FS-ENTRADA = 97) AND (FS-MOMAES = 97) AND
              (FS-CFCNAT = 97) AND (FS-MOTGEN = 97) AND (FS-CFTGEN = 97)
              MOVE ZEROS TO FS-ENTRADA
              MOVE ZEROS TO FS-MOMAES
              MOVE ZEROS TO FS-CFCNAT
              MOVE ZEROS TO FS-MOTGEN
              MOVE ZEROS TO FS-CFTGEN
           END-IF
           IF FS-MOMAES NOT EQUAL 0
              MOVE 'OPEN'   TO ACCION
              MOVE SPACES   TO LLAVE
              MOVE 'MOMAES' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                               LLAVE, FS-MOMAES, FSE-MOMAES
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "     >>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO <<<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-CFCNAT NOT EQUAL 0
              MOVE 'OPEN'   TO ACCION
              MOVE SPACES   TO LLAVE
              MOVE 'CFCNAT' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                             LLAVE, FS-CFCNAT, FSE-CFCNAT
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "     >>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO <<<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-MOTGEN NOT EQUAL 0
              MOVE 'OPEN'   TO ACCION
              MOVE SPACES   TO LLAVE
              MOVE 'MOTGEN' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                             LLAVE, FS-MOTGEN, FSE-MOTGEN
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "     >>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO <<<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-CFTGEN NOT EQUAL 0
              MOVE 'OPEN'   TO ACCION
              MOVE SPACES   TO LLAVE
              MOVE 'CFTGEN' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                             LLAVE, FS-CFTGEN, FSE-CFTGEN
              MOVE  91  TO RETURN-CODE
              DISPLAY
              "     >>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO <<<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           END-IF.
       210-EVALUA-FS-INTEGRIDAD-E. EXIT.

       300-PROCESA-ARCHIVOS SECTION.
           READ ENTRADA
                AT END SET FIN-DATOS-ENTRADA TO TRUE
           END-READ
      *--->CICLO DE LECTURAS AL PS
           PERFORM UNTIL FIN-DATOS-ENTRADA
              ADD 1 TO WKS-REG-LEIDOS-ENTRADA
              PERFORM 310-VALIDA-ESTRUCTURA-KEY
              READ ENTRADA
                   AT END SET FIN-DATOS-ENTRADA TO TRUE
              END-READ
           END-PERFORM.
       300-PROCESA-ARCHIVOS-E. EXIT.

       310-VALIDA-ESTRUCTURA-KEY SECTION.
           IF REGISTRO-ENTRADA IS NUMERIC
              IF REGISTRO-ENTRADA NOT EQUAL ZEROS
                 PERFORM 320-BUSQUEDA-CUENTA
              ELSE
                 PERFORM 403-ERROR-ESTRUCTURA-KEY
              END-IF
           ELSE
              PERFORM 403-ERROR-ESTRUCTURA-KEY
           END-IF.
       310-VALIDA-ESTRUCTURA-KEY-E. EXIT.

       320-BUSQUEDA-CUENTA SECTION.
           MOVE REGISTRO-ENTRADA TO MOMA-LLAVE
           READ MOMAES
                KEY IS MOMA-LLAVE
           END-READ
           EVALUATE FS-MOMAES
              WHEN 0
                   ADD 1 TO WKS-REG-FOUND
                   MOVE MOMA-FECHA-APERTURA TO WKS-FECHA-APERTURA
                   PERFORM 330-BUSQUEDA-DATOS-CLIENTE
                   PERFORM 340-BUSQUEDA-DATOS-CUENTA
                   PERFORM 350-BUSQUEDA-SITUACION-C
                   PERFORM 500-IMPRESION-REPORTE
              WHEN 23
                   ADD 1 TO WKS-REG-ERROR404
                   PERFORM  404-ERROR-404
              WHEN OTHER
                   MOVE "MOMAES" TO WKS-ARCHIVO-ANALIZAR
                   PERFORM 400-ERRORES-LECTURA-VSAM
           END-EVALUATE.
       320-BUSQUEDA-CUENTA-E. EXIT.

       330-BUSQUEDA-DATOS-CLIENTE SECTION.
           INITIALIZE WKS-CIF-ESTADO
           INITIALIZE WKS-DPI-EDITADO, WKS-CODIGO-RETORNO,
                      WKS-DESCRIPCION
           MOVE MOMA-CODIGO-UNIVERSAL TO CFCN-CODIGO-CLIENTE
           READ CFCNAT
                KEY IS CFCN-CODIGO-CLIENTE
           END-READ

           EVALUATE FS-CFCNAT
              WHEN 0
                   MOVE "CIF CORRECTO "       TO WKS-CIF-ESTADO
                   MOVE CFCN-DPI-CORREL       TO WKS-DPI-CORREL
                   MOVE CFCN-DPI-VERIFICADOR  TO WKS-DPI-VERIFICADOR
                   MOVE CFCN-DPI-DEPARTAMENTO TO WKS-DPI-DEPARTAMENTO
                   MOVE CFCN-DPI-MUNICIPIO    TO WKS-DPI-MUNICIPIO

                   CALL "EDU3301R" USING WKS-DPI-EDITADO,
                                     WKS-CODIGO-RETORNO, WKS-DESCRIPCION
              WHEN 23
                   ADD 1 TO WKS-REG-ERROR404
                   MOVE "CIF NO EXISTE, VERIFICAR" TO WKS-CIF-ESTADO
                   MOVE SPACES    TO CFCN-NOMBRES
                   MOVE SPACES    TO CFCN-PRIMER-APELLIDO
                   MOVE SPACES    TO CFCN-SEGUNDO-APELLIDO
                   MOVE SPACES    TO CFCN-APELLIDO-CASADA
              WHEN OTHER
                   MOVE "CFCNAT" TO WKS-ARCHIVO-ANALIZAR
                   PERFORM 400-ERRORES-LECTURA-VSAM
           END-EVALUATE.
       330-BUSQUEDA-DATOS-CLIENTE-E. EXIT.

       340-BUSQUEDA-DATOS-CUENTA SECTION.
           MOVE "CFT" TO CFTB-TABLA
           MOVE 006 TO CFTB-CODIGO-TABLA
           MOVE MOMA-AGENCIA TO CFTB-CORRELATIVO
           READ CFTGEN
              KEY IS CFTB-LLAVE
           END-READ
           EVALUATE FS-CFCNAT
           WHEN 0
                MOVE CFTB-RESTO(1:40) TO WKS-AGENCIA-APERTURA
           WHEN 23
                MOVE "AGENCIA NO ENCONTRADA" TO WKS-AGENCIA-APERTURA
           WHEN OTHER
                MOVE "CFTGEN" TO WKS-ARCHIVO-ANALIZAR
                PERFORM 400-ERRORES-LECTURA-VSAM
           END-EVALUATE.
       340-BUSQUEDA-DATOS-CUENTA-E. EXIT.

       350-BUSQUEDA-SITUACION-C SECTION.
           MOVE "MOT"                  TO MOTC-TABLA
           MOVE 003                    TO MOTC-CODIGO-TABLA
           MOVE MOMA-SITUACION-CUENTA  TO MOTC-CORRELATIVO
           READ MOTGEN
              KEY IS MOTC-LLAVE
           END-READ
           EVALUATE FS-MOTGEN
           WHEN 0
                MOVE MOTC-DESCRIPCION(1:10) TO WKS-SITUACION-CUENTA
           WHEN 23
                MOVE " SITUACION INVALIDA" TO WKS-SITUACION-CUENTA
           WHEN OTHER
                MOVE "MOTGEN" TO WKS-ARCHIVO-ANALIZAR
                PERFORM 400-ERRORES-LECTURA-VSAM
           END-EVALUATE.
       350-BUSQUEDA-SITUACION-C-E. EXIT.

      *--------> SERIE 400 ENFOCADA EN VALIDAR ERRORES DE LECTURA
       400-ERRORES-LECTURA-VSAM SECTION.
           EVALUATE WKS-ARCHIVO-ANALIZAR
           WHEN "MOMAES"
                IF FS-MOMAES NOT EQUAL 0 AND 23
                   MOVE 'READ' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'MOMAES' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-MOMAES, FSE-MOMAES
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                      ">>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO"
                      " MOMAES"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           WHEN "CFCNAT"
                IF FS-CFCNAT NOT EQUAL 0 AND 23
                   MOVE 'READ' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'CFCNAT' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-CFCNAT, FSE-CFCNAT
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                      ">>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO"
                      " CFTCNAT"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           WHEN "MOTGEN"
                IF FS-MOTGEN NOT EQUAL 0 AND 23
                   MOVE 'READ' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'MOTGEN' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-MOTGEN, FSE-MOTGEN
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                      ">>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO"
                      " MOTGEN"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           WHEN "CFTGEN"
                IF FS-CFTGEN NOT EQUAL 0 AND 23
                   MOVE 'READ' TO ACCION
                   MOVE SPACES TO LLAVE
                   MOVE 'CFTGEN' TO ARCHIVO
                   CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                      LLAVE, FS-CFTGEN, FSE-CFTGEN
                   MOVE 91 TO RETURN-CODE
                   DISPLAY
                      ">>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO"
                      " CFTGEN"
                      UPON CONSOLE
                   PERFORM XXX-CIERRA-ARCHIVOS
                   STOP RUN
                END-IF
           END-EVALUATE
           INITIALIZE WKS-ARCHIVO-ANALIZAR.
       400-ERRORES-LECTURA-VSAM-E. EXIT.

       403-ERROR-ESTRUCTURA-KEY SECTION.
           ADD 1 TO WKS-REG-CON-ERROR
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "          OH NO HUBO UN ERROR AL VALIDAR TU CUENTA"
           DISPLAY
           "            <<<<<<< ERRORES MAS COMUNES >>>>>>>"
           DISPLAY WKS-GUIONES
           DISPLAY
           " 1. LA CUENTA TIENE LETRAS, ESPACIOS O GUIONES"
           DISPLAY
           " 2. EL NUMERO DE CUENTA ES MENOR A 10 DIGITOS"
           DISPLAY
           " ==> CUENTA A BUSCAR : " REGISTRO-ENTRADA
           DISPLAY WKS-GUIONES
           DISPLAY
           "    >>>VERIFICA LA CUENTA Y VUELVE A INTENTARLO<<<"
           DISPLAY WKS-GUIONES
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       403-ERROR-ESTRUCTURA-KEY-E. EXIT.

       404-ERROR-404 SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "          OH LA CUENTA QUE BUSCAS NO EXISTE"
           DISPLAY
           "          <<<<<<< INTENTA DE NUEVO >>>>>>>"
           DISPLAY
           " ==> CUENTA A BUSCAR : " REGISTRO-ENTRADA
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       404-ERROR-404-E. EXIT.

      *------- SERIES 500 ENFOCADAS A LA IMPRESION DEL REPORTE EN SYSOUT
       500-IMPRESION-REPORTE SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "*                                                         "
           " *"
           DISPLAY
           "*      DETALLE CUENTAS MONETARIAS BANCO INDUSTRIAL S,A"
           "     *"
           DISPLAY
           "*                                                         "
           " *"
           DISPLAY WKS-GUIONES
           DISPLAY
           "* NUMERO DE CUENTA    : " MOMA-NUMERO-CUENTA
           DISPLAY
           "* FECHA APERTURA      :" WKS-FECHA-APERTURA
           DISPLAY
           "* CODIGO SITUACION    : " MOMA-SITUACION-CUENTA
           DISPLAY
           "* SITUACION DETALLE   : " WKS-SITUACION-CUENTA
           DISPLAY
           "* CODIGO AGENCIA      : " MOMA-AGENCIA
           DISPLAY
           "* NOMBRE DE AGENCIA   : " WKS-AGENCIA-APERTURA
           DISPLAY WKS-GUIONES
           DISPLAY
           "*                     DATOS DEL CLIENTE               "
           "     *"
           DISPLAY WKS-GUIONES
           DISPLAY
           "* CODIGO DE CLIENTE   : " CFCN-CODIGO-CLIENTE
           DISPLAY
           "* ESTADO CIF          : " WKS-CIF-ESTADO
           DISPLAY
           "* NOMBRES             : " CFCN-NOMBRES
           DISPLAY
           "* PRIMER  APELLIDO    : " CFCN-PRIMER-APELLIDO
           DISPLAY
           "* SEGUNDO APELLIDO    : " CFCN-SEGUNDO-APELLIDO
           DISPLAY
           "* APELLIDO DE CASADA  : " CFCN-APELLIDO-CASADA
           DISPLAY WKS-GUIONES
           DISPLAY
           "*                 VERIFICACION DPI GUARDADO           "
           "     *"
           DISPLAY WKS-GUIONES
           DISPLAY
           "* DPI                 : " WKS-DPI-EDITADO
           DISPLAY
           "* CODIGO DE RETORNO   : " WKS-CODIGO-RETORNO
           DISPLAY
           "* DESCRIPCION CODIGO  : " WKS-DESCRIPCION
           DISPLAY WKS-GUIONES
           DISPLAY " "
           DISPLAY " ".
       500-IMPRESION-REPORTE-E. EXIT.

       510-ESTADISTICAS SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "             <<<<<<< ESTADISTICAS >>>>>>>"
           DISPLAY WKS-GUIONES
           DISPLAY
           "==> REGISTROS LEIDOS EN TOTAL : " WKS-REG-LEIDOS-ENTRADA
           DISPLAY
           "==> REGISTROS NO ENCONTRADOS  : " WKS-REG-ERROR404
           DISPLAY
           "==> REGISTROS CON ERRORES     : " WKS-REG-CON-ERROR
           DISPLAY
           "==> REGISTROS ENCONTRADOS     : " WKS-REG-FOUND
           DISPLAY
           "             <<<<<<< ESTADISTICAS >>>>>>>"
           DISPLAY WKS-GUIONES.
       510-ESTADISTICAS-E. EXIT.

       XXX-CIERRA-ARCHIVOS SECTION.
           CLOSE ENTRADA
           CLOSE MOMAES
           CLOSE CFCNAT
           CLOSE MOTGEN
           CLOSE CFTGEN.
       XXX-CIERRA-ARCHIVOS-E. EXIT.