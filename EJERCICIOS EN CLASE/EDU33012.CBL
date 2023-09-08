      ******************************************************************
      * FECHA       : 12/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33012                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : PROGRAMA PROGRAMA CARGA EN MEMORIA LAS TABLAS DE *
      *             : DEPARTAMENTOS Y MUNICIPIOS DE UN ARCHIVO,        *
      *             : LUEGO DE ESTO EL PROGRAMA RECIBE POR SYSIN UNA   *
      *             : PETICION DE BUSQUEDA EN EL ARREGLO QUE HEMOS CREA*
      *             : ADO EN MEMORIA CON LAS TABLAS                    *
      * ARCHIVOS    : EDUC.SEM.CIF.MODGRAL.TABLAS (KSDS)               *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDU33012.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT CFTGEN ASSIGN TO CFTGEN
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS DYNAMIC
                   RECORD KEY    IS CFTB-LLAVE
                   FILE STATUS   IS FS-CFTGEN
                                    FSE-CFTGEN.
       DATA DIVISION.
       FILE SECTION.
       FD  CFTGEN.
           COPY CFTGEN REPLACING CFTGEN BY REG-CFTGEN.
       WORKING-STORAGE SECTION.
      *--> VARIABLES DE FILE STATUS NORMAL
       01 FS-CFTGEN                     PIC 9(02)  VALUE ZEROS.
      *--> VARIABLES DE FILE STATUS EXTENDED
       01 FSE-CFTGEN.
          02 FSE-RETURN                 PIC S9(04) COMP-5
                                                   VALUE ZEROS.
          02 FSE-FUNCTION               PIC S9(04) COMP-5
                                                   VALUE ZEROS.
          02 FSE-FEEDBACK               PIC S9(04) COMP-5
                                                   VALUE ZEROS.
      *--> VARIABLES DE RUTINA PARA CONTROL DE FILE STATUS EXTENDIDO
       01 PROGRAMA                      PIC X(08)  VALUE SPACES.
       01 ARCHIVO                       PIC X(08)  VALUE SPACES.
       01 ACCION                        PIC X(10)  VALUE SPACES.
       01 LLAVE                         PIC X(32)  VALUE SPACES.
      *--> VARIABLES DE OPERACION Y CONTADORES
       01 WKS-TABLA1-OCCURS             PIC 9(02)  VALUE ZEROS.
       01 WKS-TABLA2-OCCURS             PIC 9(03)  VALUE ZEROS.
       01 WKS-LECTURAS-TOTAL            PIC 9(03)  VALUE ZEROS.
       01 WKS-DEPTOS-TOTAL              PIC 9(02)  VALUE ZEROS.
       01 WKS-MUNICP-TOTAL              PIC 9(03)  VALUE ZEROS.
       01 WKS-TIMER-INICIO.
          02 WKS-DIA                    PIC 9(02)  VALUE ZEROS.
          02 WKS-HORA                   PIC 9(02)  VALUE ZEROS.
          02 WKS-MINUTO                 PIC 9(02)  VALUE ZEROS.
          02 WKS-SEGUNDO                PIC 9(02)  VALUE ZEROS.
       01 WKS-TIMER-EDIT-I.
          02 WKS-DIA-IE                 PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(03)  VALUE "D :".
          02 WKS-HORA-IE                PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(03)  VALUE "H :".
          02 WKS-MINUTO-IE              PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(03)  VALUE "M :".
          02 WKS-SEGUNDO-IE             PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(01)  VALUE "S".
       01 WKS-TIMER-FIN                 PIC 9(08)  VALUE ZEROS.
       01 WKS-TIMER-EDIT-F.
          02 WKS-DIA-FE                 PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(03)  VALUE "D :".
          02 WKS-HORA-FE                PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(03)  VALUE "H :".
          02 WKS-MINUTO-FE              PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(03)  VALUE "M :".
          02 WKS-SEGUNDO-FE             PIC 9(02)  VALUE ZEROS.
          02 FILLER                     PIC X(01)  VALUE "S".
       01 WKS-CONTROLADOR-LECTURA       PIC X(03).
          88 WKS-DETENER                           VALUE "005".
       01 WKS-BUSQUEDA.
          02 WKS-DEPARTAMENTO           PIC X(02)  VALUE ZEROS.
          02 WKS-MUNICIPIOS             PIC X(03)  VALUE ZEROS.

      *------------------->   REDEFINICIONES DE ENTRADAS
       01 WKS-END-PROGRAM REDEFINES WKS-BUSQUEDA.
          02 FILLER                     PIC X(03).
             88 WKS-END                     VALUE "FIN".
          02 FILLER                     PIC X(02).
      *============== TABLA DE MUNICIPIOS Y DEPARTAMENTOS ============
       01 WKS-TABLAS.
      *--> TABLA DEPARTAMENTOS CFT-003
          02 WKS-TABLA-DEP OCCURS 1 TO 99
                           DEPENDING ON  WKS-TABLA1-OCCURS
                           ASCENDING KEY WKS-LLAVE-DEP
                           INDEXED   BY  IDX-DEP.
             03 WKS-LLAVE-DEP           PIC 9(02).
             03 WKS-NOMBRE-DEP          PIC X(20).
      *--> TABLA MUNICIPIOS   CFT-004
          02 WKS-TABLA-MUNI OCCURS 1 TO 999
                            DEPENDING ON  WKS-TABLA2-OCCURS
                            ASCENDING KEY WKS-LLAVE-MUN
                            INDEXED   BY  IDX-MUN.
             03 WKS-LLAVE-MUN           PIC 9(05).
             03 WKS-NOMBRE-MUN          PIC X(45).
       PROCEDURE DIVISION.
       000-PRINCIPAL SECTION.
           PERFORM 100-ABRIR-ARCHIVOS
           PERFORM 110-VERIFICAR-FS
           PERFORM 200-CARGAR-TABLAS-EN-MEMORIA
           PERFORM 300-ACEPTAR-DATOS-Y-BUSQUEDA
           PERFORM 600-ESTADISTICAS
           PERFORM XXX-CIERRA-ARCHIVOS
           STOP RUN.
       OO0-PRINCIPAL-E. EXIT.

       100-ABRIR-ARCHIVOS SECTION.
           OPEN INPUT CFTGEN.
       100-ABRIR-ARCHIVOS-E.

       110-VERIFICAR-FS SECTION.
           IF FS-CFTGEN = 97
              MOVE ZEROS TO FS-CFTGEN
           END-IF
           IF FS-CFTGEN NOT EQUAL 0
              MOVE 'EDU33012' TO PROGRAMA
              MOVE 'OPEN'     TO ACCION
              MOVE SPACES     TO LLAVE
              MOVE 'CFTGEN'   TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                 LLAVE, FS-CFTGEN, FSE-CFTGEN
              MOVE 91 TO RETURN-CODE
              DISPLAY
              "     >>>> OH NO, ALGO SALIO MAL AL ABRIR EL ARCHIVO <<<<"
              UPON CONSOLE
              DISPLAY
              "               !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           ELSE
              DISPLAY
              "     >>>>>>>>>>APERTURA DE ARCHIVOS EXITOSA<<<<<<<<<<<<"
              UPON CONSOLE
           END-IF.
       110-VERIFICAR-FS-E. EXIT.

       200-CARGAR-TABLAS-EN-MEMORIA SECTION.
           MOVE "CFT0030000000000" TO CFTB-LLAVE
           START CFTGEN
                 KEY IS >= CFTB-LLAVE
           END-START

           READ CFTGEN NEXT RECORD
           PERFORM UNTIL WKS-DETENER
               EVALUATE CFTB-CODIGO-TABLA
               WHEN 003
                    ADD 1                 TO WKS-TABLA1-OCCURS
                    ADD 1                 TO WKS-DEPTOS-TOTAL
                    MOVE CFTB-CORRELATIVO TO WKS-LLAVE-DEP(IDX-DEP)
                    MOVE CFTB-RESTO       TO WKS-NOMBRE-DEP(IDX-DEP)
                    SET IDX-DEP UP BY 1
               WHEN 004
                    ADD 1                 TO WKS-TABLA2-OCCURS
                    ADD 1                 TO WKS-MUNICP-TOTAL
                    MOVE CFTB-CORRELATIVO TO WKS-LLAVE-MUN(IDX-MUN)
                    MOVE CFTB-RESTO       TO WKS-NOMBRE-MUN(IDX-MUN)
                    SET IDX-MUN UP BY 1
               WHEN OTHER
                    SET WKS-DETENER TO TRUE
               END-EVALUATE
               READ CFTGEN NEXT RECORD
               PERFORM 700-ERORES-LECTURA-VSAM
           END-PERFORM.
       200-CARGAR-TABLAS-EN-MEMORIA-E.

       300-ACEPTAR-DATOS-Y-BUSQUEDA SECTION.
           ACCEPT WKS-BUSQUEDA FROM SYSIN
           PERFORM UNTIL WKS-END
              IF (WKS-BUSQUEDA IS NUMERIC) AND
                 (WKS-DEPARTAMENTO <= WKS-TABLA1-OCCURS) AND
                 (WKS-DEPARTAMENTO > ZEROS)
                  PERFORM 310-BUSQUEDA-DEPTO-INFO
              ELSE
                  PERFORM 400-ERRORES-ESCTRURA-ENTRADA
              END-IF
              ACCEPT WKS-BUSQUEDA FROM SYSIN
           END-PERFORM.
       300-ACEPTAR-DATOS-Y-BUSQUEDA-E. EXIT.

       310-BUSQUEDA-DEPTO-INFO SECTION.
           ACCEPT WKS-TIMER-INICIO FROM TIME
           PERFORM 601-TIMER-INICIO
           SET IDX-DEP TO 1
           SEARCH WKS-TABLA-DEP
                AT END PERFORM 510-NOT-FOUD-DEPTO
                WHEN WKS-LLAVE-DEP(IDX-DEP) = WKS-DEPARTAMENTO
                     PERFORM 500-DISPLAY-DEPTO-DETALLE
                     PERFORM 310-BUSQUEDA-MUNI-INFO
           END-SEARCH.
       310-BUSQUEDA-DEPTO-INFO-E. EXIT.

       310-BUSQUEDA-MUNI-INFO SECTION.
           ACCEPT WKS-TIMER-INICIO FROM TIME
           PERFORM 602-TIMER-FIN
           SEARCH ALL WKS-TABLA-MUNI
                AT END  PERFORM 510-NOT-FOUD-MUNICIP
                WHEN WKS-LLAVE-MUN(IDX-MUN) = WKS-BUSQUEDA
                     PERFORM 500-DISPLAY-MUNICP-DETALLE
           END-SEARCH.
       310-BUSQUEDA-MUNI-INFO-E. EXIT.

       400-ERRORES-ESCTRURA-ENTRADA SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY
           "========================================================="
           DISPLAY
           "*          DEPARTAMENTOS Y MUNICIPIOS DE GUATEMALA      *"
            DISPLAY
           "========================================================="
           DISPLAY
           "=> HAS INGRESADO LA BUSQUEDA      : " WKS-BUSQUEDA
           DISPLAY
           ">>>>>>> LA ENTRADA NO ES VALIDA, POSIBLES RAZONES: <<<<<<"
           DISPLAY
           "  1. HAS INTRODUCIDO CARACTERES NO NUMERICOS"
           DISPLAY
           "  2. LA CADENA NO PUEDE SER VACIA (ESPACIOS)"
           DISPLAY
           "  3. LA CADENA NO PUEDE SER CEROS "
           DISPLAY
           "=========================================================".
       400-ERRORES-ESCTRURA-ENTRADA-E. EXIT.

       500-DISPLAY-DEPTO-DETALLE SECTION.
           ACCEPT WKS-TIMER-FIN FROM TIME
           MOVE   WKS-TIMER-FIN TO WKS-TIMER-EDIT-F
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY
           "========================================================="
           DISPLAY
           "*          DEPARTAMENTOS Y MUNICIPIOS DE GUATEMALA      *"
            DISPLAY
           "========================================================="
           DISPLAY
           "=> HAS INGRESADO LA BUSQUEDA      : " WKS-BUSQUEDA
           DISPLAY
           "========================================================="
           DISPLAY
           "*                   DEPARTAMENTO DETALLE                *"
           DISPLAY
           "========================================================="
           DISPLAY
           "=> CODIGO DEPARTAMENTO            : " WKS-DEPARTAMENTO
           DISPLAY
           "=> CORRESPONDE AL DEPARTAMENTO DE : "
                                                WKS-NOMBRE-DEP(IDX-DEP)
            DISPLAY
           "========================================================="
           DISPLAY
           "* ESTADISTICAS TIEMPOS DE BUSQUEDA PARA DEPARTAMENTOS"
           DISPLAY
           "* SE USO UN SEARCH NORMAL"
           DISPLAY
           "* SE INICIO  BUSQUEDA A LAS : " WKS-TIMER-EDIT-I
           DISPLAY
           "* SE TERMINO BUSQUEDA A LAS : " WKS-TIMER-EDIT-F.
       500-DISPLAY-DEPTO-DETALLE-E. EXIT.

       500-DISPLAY-MUNICP-DETALLE SECTION.
           ACCEPT WKS-TIMER-FIN FROM TIME
           MOVE   WKS-TIMER-FIN TO WKS-TIMER-EDIT-F
           DISPLAY
           "========================================================="
           DISPLAY
           "*                     MUNICIPIO DETALLE                 *"
           DISPLAY
           "========================================================="
           DISPLAY
           "=> CORRELATIVO MUNICIPIO          : " WKS-MUNICIPIOS
           DISPLAY
           "=> CORRESPONDE AL MUNICIPIO    DE : "
                                                WKS-NOMBRE-MUN(IDX-MUN)
           DISPLAY
           "========================================================="
           DISPLAY
           "* ESTADISTICAS TIEMPOS DE BUSQUEDA PARA MUNICIPIOS"
           DISPLAY
           "* SE USO UN SEARCH NORMAL ALL"
           DISPLAY
           "* SE INICIO  BUSQUEDA A LAS : " WKS-TIMER-EDIT-I
           DISPLAY
           "* SE TERMINO BUSQUEDA A LAS : " WKS-TIMER-EDIT-F
           DISPLAY
           "=========================================================".
       500-DISPLAY-MUNICP-DETALLE-E. EXIT.

       510-NOT-FOUD-DEPTO SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY
           "========================================================="
           DISPLAY
           "*          DEPARTAMENTOS Y MUNICIPIOS DE GUATEMALA      *"
            DISPLAY
           "========================================================="
           DISPLAY
           "=> HAS INGRESADO LA BUSQUEDA      : " WKS-BUSQUEDA
           DISPLAY
           "=> LA CUAL NO HA SIDO ENCONTRADA, VERIFICA TU ENTRADA"
           DISPLAY
           "=========================================================".
       510-NOT-FOUD-DEPTO-E. EXIT.

       510-NOT-FOUD-MUNICIP SECTION.
           DISPLAY
           "========================================================="
           DISPLAY
           "=> CORRELATIVO MUNICIPIO               : " WKS-MUNICIPIOS
           DISPLAY
           ">>>> LO SENTIMOS NO PUDIMOS ENCONTRAR ESTE RESULTADO <<<<"
           DISPLAY
           "      VERIFICA NUEVAMENTE EL CODIGO DE DEPARTAMENTO"
           DISPLAY
           "=========================================================".
       510-NOT-FOUD-MUNICIP-E. EXIT.

       600-ESTADISTICAS SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY
           "========================================================="
           DISPLAY
           "**                  >>>ESTADISTICAS<<<                 **"
           DISPLAY
           "========================================================="
           DISPLAY
           "*  LECTURAS REALIZADAS AL ARCHIVO CFTGEN : "
                                                  WKS-LECTURAS-TOTAL
           DISPLAY
           "*  TOTAL DE DEPARTAMENTOS CARGADOS       : "
                                                  WKS-DEPTOS-TOTAL
           DISPLAY
           "*  TOTAL DE MUNICIPIOS CARGADOS          : "
                                                  WKS-MUNICP-TOTAL
           DISPLAY
           "=========================================================".
       600-ESTADISTICAS-E. EXIT.

       601-TIMER-INICIO SECTION.
           MOVE WKS-DIA           TO  WKS-DIA-IE
           MOVE WKS-HORA          TO  WKS-HORA-IE
           MOVE WKS-MINUTO        TO  WKS-MINUTO-IE
           MOVE WKS-SEGUNDO       TO  WKS-SEGUNDO-IE.
       601-TIMER-INICIO-E. EXIT.

       602-TIMER-FIN    SECTION.
           MOVE WKS-DIA           TO  WKS-DIA-FE
           MOVE WKS-HORA          TO  WKS-HORA-FE
           MOVE WKS-MINUTO        TO  WKS-MINUTO-FE
           MOVE WKS-SEGUNDO       TO  WKS-SEGUNDO-FE.
       602-TIMER-FIN-E. EXIT.

       700-ERORES-LECTURA-VSAM SECTION.
           IF FS-CFTGEN NOT EQUAL 0
              MOVE 'EDU33012' TO PROGRAMA
              MOVE 'READ'     TO ACCION
              MOVE CFTB-LLAVE TO LLAVE
              MOVE 'CFTGEN'   TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                 LLAVE, FS-CFTGEN, FSE-CFTGEN
              MOVE 91 TO RETURN-CODE
              DISPLAY
              "     >>>> OH NO, ALGO SALIO MAL AL LEER EL ARCHIVO <<<<"
              UPON CONSOLE
              DISPLAY
              "            !!! VERIFICAR DETALLES EN SPOOL !!!"
              UPON CONSOLE
              PERFORM XXX-CIERRA-ARCHIVOS
              STOP RUN
           ELSE
              ADD 1 TO WKS-LECTURAS-TOTAL
           END-IF.
       700-ERORES-LECTURA-VSAM-E. EXIT.

       XXX-CIERRA-ARCHIVOS SECTION.
           CLOSE CFTGEN.
       XXX-CIERRA-ARCHIVOS-E. EXIT.