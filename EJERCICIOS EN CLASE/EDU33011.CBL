      ******************************************************************
      * FECHA       : 12/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33011                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : PROGRAMA GENERA UN ARCHIVO ORDENADO PARA PODER   *
      *             : TRANSFERIRLO POR MEDIO DE FTP DEL Z A UNA PC     *
      *             : PROGRMA LEE UN ESDS PARA LUEGO INGRESAR UN       *
      *             : ENCABEZADO Y LOS DATOS EN SECUENCIA, USANDO EL   *
      *             : SEPARADOR "|" COMO INDICADOR.                    *
      * ARCHIVOS    : EDUC.SEM.MOVIM.CONSOLI.DADO (ESDS)               *
      *             : EDUC.EDU33011.SALIDA.UNO    (PS FTP)             *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU33011.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT MOMDCO  ASSIGN TO AS-MOMDCO
                   ORGANIZATION   IS SEQUENTIAL
                   ACCESS MODE    IS SEQUENTIAL
                   FILE STATUS    IS FS-MOMDCO
                                     FSE-MOMDCO.
            SELECT FTPREP ASSIGN TO FTPREP
                   FILE STATUS    IS FS-FTPREP.
       DATA DIVISION.
       FILE SECTION.
       FD  MOMDCO.
           COPY MOMDCO.
       FD  FTPREP
           RECORDING MODE IS F.
       01 PRINT-LINE                    PIC X(258).
       WORKING-STORAGE SECTION.
      *------->          VARIABLES DE FILE STATUS NORMAL
       01 FS-MOMDCO                       PIC 9(02)  VALUE ZEROS.
       01 FS-FTPREP                       PIC 9(02)  VALUE ZEROS.
      *------->         VARIABLES DE FILE STATUS EXTENDED
       01 FSE-MOMDCO.
          02 FSE-RETURN                   PIC S9(04) COMP-5
                                                     VALUE ZEROS.
          02 FSE-FUNCTION                 PIC S9(04) COMP-5
                                                     VALUE ZEROS.
          02 FSE-FEEDBACK                 PIC S9(04) COMP-5
                                                     VALUE ZEROS.
      *--->  VARIABLES DE RUTINA PARA CONTROL DE FILE STATUS EXTENDIDO
       01 PROGRAMA                        PIC X(08)  VALUE SPACES.
       01 ARCHIVO                         PIC X(08)  VALUE SPACES.
       01 ACCION                          PIC X(10)  VALUE SPACES.
       01 LLAVE                           PIC X(32)  VALUE SPACES.
      *------->             CONTROLADOR DE LECTURAS
       01 WKS-FIN-ARCHIVOS                PIC X(01).
          88 WKS-FIN-MOMDCO                          VALUE HIGH-VALUES.
      *------->            PRINT LINE VARIABLES ARRAY
       01 HEADER1.
          05                              PIC X(11)   VALUE
                                                         'TRANSACCION'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(03)   VALUE 'DIA'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(03)   VALUE 'MES'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(12)   VALUE
                                                         'CLASE CUENTA'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(10)   VALUE
                                                           'NO. CUENTA'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(09)   VALUE 'DOCUMENTO'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(07)   VALUE 'ACENCIA'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(05)   VALUE 'VALOR'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(09)   VALUE 'RESERVA 1'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(15)   VALUE
                                                      'VALOR RESERVA 1'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(09)   VALUE 'RESERVA 2'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(15)   VALUE
                                                      'VALOR RESERVA 2'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(06)   VALUE 'LEGAJO'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(19)   VALUE
                                                  'TERMINAL FINANCIERA'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(13)   VALUE
                                                        'NUM.SECUENCIA'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(15)   VALUE
                                                      'TIPO MOVIMIENTO'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(13)   VALUE
                                                        'SIT. REGISTRO'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(12)   VALUE
                                                       'GRAB. VERIF.'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(18)   VALUE
                                                   'ORIGEN TRANSACCION'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(06)   VALUE 'CAJERO'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(13)   VALUE
                                                      'CODIGO MONEDA'.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05                              PIC X(13)   VALUE
                                                      'CODIGO MOTIVO'.
          05 FILLER                       PIC X(01)   VALUE '|'.
      * ------->           VARIABLES OPERACIONES MOMDCO
       01 WKS-REG-MOMDCO.
          05 WKS-MOMC-CODIGO-TRANSACCION  PIC 9(02)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-DIA                 PIC 9(02)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-MES                 PIC 9(02)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-CLASE-CUENTA        PIC 9(01)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-NUMERO-CUENTA       PIC 9(10)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-DOCUMENTO           PIC 9(08)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-AGENCIA-ORIGEN      PIC 9(03)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-VALOR               PIC  +QQQ,QQQ,QQQ.QQ.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-TIPO-RESERVA1       PIC 9(01)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-VALOR-RESERVA       PIC  +QQQ,QQQ,QQQ.QQ.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-TIPO-RESERVA2       PIC 9(01)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-VALOR-RESERVA2      PIC  +QQQ,QQQ,QQQ.QQ.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-LEGAJO              PIC 9(09)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-TERMINAL-FINANCIERA PIC X(04)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-NUM-SECUENCIA       PIC 9(04)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-TIPO-MOVIMIENTO     PIC 9(01)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-SIT-REGISTRO        PIC 9(01)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-GRABA-VERIF         PIC X(08)   VALUE SPACES.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-ORIGEN-TRANSACCION  PIC 9(02)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-CAJERO              PIC X(04)   VALUE SPACES.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-CODIGO-MONEDA       PIC 9(01)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
          05 WKS-MOMC-CODIGO-MOTIVO       PIC 9(05)   VALUE ZEROS.
          05 FILLER                       PIC X(01)   VALUE '|'.
       01 WKS-ESTADISTICAS.
          05 WKS-REGISTROS-TOTAL          PIC 9(03)   VALUE ZEROS.
          05 WKS-REGISTROS-ESCRITOS       PIC 9(03)   VALUE ZEROS.
          05 WKS-REGISTROS-N-ESCRITOS     PIC 9(03)   VALUE ZEROS.
          PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 200-APERTURA-ARCHIVOS
           PERFORM 210-VERIFICAR-FS-INTEGRIDAD
           PERFORM 300-LECTURA-Y-ESCRITURA
      *            400-SERIES VALIDA FSE
      *            500-SERIES VALIDA INTEGRIDAD ESCRITURA
           PERFORM 600-ESTADISTICAS
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

      *-----> SERIES 200 ENCARGADAS DE VALIDAR LA APERTURA DE DATASETS
       200-APERTURA-ARCHIVOS SECTION.
           OPEN INPUT  MOMDCO
           OPEN OUTPUT FTPREP.
       200-APERTURA-ARCHIVOS-E. EXIT.

       210-VERIFICAR-FS-INTEGRIDAD SECTION.
           IF (FS-MOMDCO = 97) AND (FS-FTPREP = 97)
              MOVE ZEROS TO FS-MOMDCO
              MOVE ZEROS TO FS-FTPREP
           END-IF
           IF FS-MOMDCO NOT EQUAL 0
              MOVE 'EDU33011' TO PROGRAMA
              MOVE 'OPEN'     TO ACCION
              MOVE SPACES     TO LLAVE
              MOVE 'MOMDCO'   TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                    LLAVE, FS-MOMDCO, FSE-MOMDCO
              MOVE 91 TO RETURN-CODE
              PERFORM 410-ERRORES-APERTURA
              PERFORM 700-CIERRA-ARCHIVOS
              STOP RUN
           END-IF.
       210-VERIFICAR-FS-INTEGRIDAD-E. EXIT.

      *---------> SERIES 300 ENCARGADAS DE LEER Y ESCRIBIR DATOS
       300-LECTURA-Y-ESCRITURA SECTION.
           MOVE HEADER1 TO PRINT-LINE
           WRITE PRINT-LINE
           READ MOMDCO
                   AT END SET  WKS-FIN-MOMDCO TO TRUE
           END-READ
           PERFORM UNTIL  WKS-FIN-MOMDCO
                   ADD 1 TO WKS-REGISTROS-TOTAL
                   PERFORM 310-ESCRITURAS-AND-MOVE
           END-PERFORM.
       300-LECTURA-Y-ESCRITURA-E. EXIT.

       310-ESCRITURAS-AND-MOVE SECTION.
           INITIALIZE  WKS-REG-MOMDCO
           MOVE MOMC-CODIGO-TRANSACCION TO WKS-MOMC-CODIGO-TRANSACCION
           MOVE MOMC-DIA                TO WKS-MOMC-DIA
           MOVE MOMC-MES                TO WKS-MOMC-MES
           MOVE MOMC-CLASE-CUENTA       TO WKS-MOMC-CLASE-CUENTA
           MOVE MOMC-NUMERO-CUENTA      TO WKS-MOMC-NUMERO-CUENTA
           MOVE MOMC-DOCUMENTO          TO WKS-MOMC-DOCUMENTO
           MOVE MOMC-AGENCIA-ORIGEN     TO WKS-MOMC-AGENCIA-ORIGEN
           MOVE MOMC-VALOR              TO WKS-MOMC-VALOR
           MOVE MOMC-TIPO-RESERVA1      TO WKS-MOMC-TIPO-RESERVA1
           MOVE MOMC-VALOR-RESERVA1     TO WKS-MOMC-VALOR-RESERVA
           MOVE MOMC-TIPO-RESERVA2      TO WKS-MOMC-TIPO-RESERVA2
           MOVE MOMC-VALOR-RESERVA2     TO WKS-MOMC-VALOR-RESERVA2
           MOVE MOMC-LEGAJO             TO WKS-MOMC-LEGAJO
           MOVE MOMC-TERMINAL-FINANCIERA
                                        TO WKS-MOMC-TERMINAL-FINANCIERA
           MOVE MOMC-NUM-SECUENCIA      TO WKS-MOMC-NUM-SECUENCIA
           MOVE MOMC-TIPO-MOVIMIENTO    TO WKS-MOMC-TIPO-MOVIMIENTO
           MOVE MOMC-SIT-REGISTRO       TO WKS-MOMC-SIT-REGISTRO
           MOVE MOMC-GRABA-VERIF        TO WKS-MOMC-GRABA-VERIF
           MOVE MOMC-ORIGEN-TRANSACCION TO WKS-MOMC-ORIGEN-TRANSACCION
           MOVE MOMC-CAJERO             TO WKS-MOMC-CAJERO
           MOVE MOMC-CODIGO-MONEDA      TO WKS-MOMC-CODIGO-MONEDA
           MOVE MOMC-CODIGO-MOTIVO      TO WKS-MOMC-CODIGO-MOTIVO
           MOVE WKS-REG-MOMDCO          TO PRINT-LINE
           WRITE PRINT-LINE
           PERFORM 500-ERROES-ESCRITURA-PS
           READ MOMDCO
              AT END SET WKS-FIN-MOMDCO TO TRUE
           END-READ.
       310-ESCRITURAS-AND-MOVE-E. EXIT.

      *-----------> SERIES 400 ENCARGADAS DE VALIDAR FSE
       410-ERRORES-APERTURA SECTION.
           DISPLAY
           "================================================="
           UPON CONSOLE
           DISPLAY
           "* HUBO UN ERROR AL ABRIR ALGUNO DE LOS ARCHIVOS *"
           UPON CONSOLE
           DISPLAY
           "================================================="
           UPON CONSOLE
           DISPLAY
           "==> FILE STATUS DEL ARCHIVO MOMDCO :" FS-MOMDCO
           UPON CONSOLE
           DISPLAY
           "==> FILE STATUS DEL ARCHIVO FTPREP :" FS-FTPREP
           UPON CONSOLE
           DISPLAY
           "==> VERIFICAR DETALLE AL FINAL DEL SPOOL"
           UPON CONSOLE
           DISPLAY
           "================================================="
           UPON CONSOLE.
       410-ERRORES-APERTURA-E. EXIT.

      *--------> SERIES 500 VALIDA INTEGRIDAD DE ESCRITURA DE UN PS
       500-ERROES-ESCRITURA-PS SECTION.
           IF FS-FTPREP NOT = 0
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY
                   "       HUBO UN ERROR AL ESCRIBIR UN REGISTRO     "
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                DISPLAY
                   " FILE ESTATUS DEL ARCHIVO ES  : (" FS-FTPREP ")"
                   UPON CONSOLE
                DISPLAY
                   "  >>>>>EL PROGRAMA FINALIZO POR SEGURIDAD<<<<<  "
                   UPON CONSOLE
                DISPLAY
                   "================================================"
                   UPON CONSOLE
                ADD 1 TO WKS-REGISTROS-N-ESCRITOS
                PERFORM 700-CIERRA-ARCHIVOS
                MOVE 91 TO RETURN-CODE
                STOP RUN
           ELSE
                ADD 1 TO WKS-REGISTROS-ESCRITOS
           END-IF.
       500-ERROES-ESCRITURA-PS-E. EXIT.

       600-ESTADISTICAS SECTION.
           DISPLAY
           "================================================="
           DISPLAY
           "*                    ESTADISTICAS               *"
           DISPLAY
           "================================================="
           DISPLAY
           "==> REGISTROS PROCESADOS   TOTAL : " WKS-REGISTROS-TOTAL
           DISPLAY
           "==> REGISTROS ESCRITOS     TOTAL : " WKS-REGISTROS-ESCRITOS
           DISPLAY
           "==> REGISTROS NO ESCRITOS  TOTAL : "
                                                WKS-REGISTROS-N-ESCRITOS
           DISPLAY
           "=================================================".
       600-ESTADISTICAS-E. EXIT.

       700-CIERRA-ARCHIVOS SECTION.
           CLOSE  MOMDCO
           CLOSE  FTPREP.
       700-CIERRA-ARCHIVOS-E. EXIT.