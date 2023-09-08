      ******************************************************************
      * FECHA       : 19/12/2022                                       *
      * PROGRAMADOR : ERICK RAMIREZ (PEDR)                             *
      * APLICACION  : TARJETA CREDITO/INSTITUCIONAL                    *
      * PROGRAMA    : TLCU1C04                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : HACE LA MIGRACION DE DATOS DE TARJETA DE CREDITO *
      *             : /INSTITUCIONAL LAS CUALES PRENSENTAN VALORES EN  *
      *             : INTERES O MORA(TOMANDO EL VALOR MAYOR ENTRE ESTOS*
      *             : DOS), PARA PODER CALCULAR LOS DIAS QUE LLEVAN DE *
      *             : MORA                                             *
      * ARCHIVOS    : TLMATH=C,TIMATH=C,TLPRCT=C , TIAXL1=C, TLTGEN=C  *
      *             : TLAXLQ=C,TLAXLD=C, TLIMOR=A                      *
      * ACCION (ES) : A=ACTUALIZA, R=REPORTE, C=CONSULTAR, K=ARCHIVO   *
      * INSTALADO   : DD/MM/AAAA                                       *
      * BPM/RATIONAL: 228866                                           *
      * NOMBRE      : MANTENIMIENTO CONTEO DIAS MORA                   *
      * DESCRIPCION : MANTENIMIENTO                                    *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. CIERRES1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TLMATH ASSIGN TO TLMATH
                  ORGANIZATION  IS INDEXED
                  ACCESS        IS SEQUENTIAL
                  RECORD KEY    IS TLMT-LLAVE
                  FILE STATUS   IS FS-TLMATH
                                   FSE-TLMATH.

           SELECT TIMATH ASSIGN TO TIMATH
                  ORGANIZATION  IS INDEXED
                  ACCESS        IS SEQUENTIAL
                  RECORD KEY    IS TIMT-LLAVE
                  FILE STATUS   IS FS-TIMATH
                                   FSE-TIMATH.

           SELECT TLPRCT ASSIGN TO TLPRCT
                  ORGANIZATION  IS INDEXED
                  ACCESS        IS RANDOM
                  RECORD KEY    IS TLPC-LLAVE
                  FILE STATUS   IS FS-TLPRCT
                                   FSE-TLPRCT.

           SELECT TLAXLQ ASSIGN TO TLAXLQ
                  ORGANIZATION  IS INDEXED
                  ACCESS        IS RANDOM
                  RECORD KEY    IS TLAL-LLAVE
                  FILE STATUS   IS FS-TLAXLQ
                                   FSE-TLAXLQ.

           SELECT TLAXLD ASSIGN TO TLAXLD
                  ORGANIZATION  IS INDEXED
                  ACCESS        IS RANDOM
                  RECORD KEY    IS TLAE-LLAVE
                  FILE STATUS   IS FS-TLAXLD
                                   FSE-TLAXLD.

           SELECT TIAXL1 ASSIGN TO TIAXL1
                  ORGANIZATION  IS INDEXED
                  ACCESS        IS RANDOM
                  RECORD KEY    IS TIAL-LLAVE
                  FILE STATUS   IS FS-TIAXL1
                                   FSE-TIAXL1.

           SELECT TLIMOR ASSIGN TO TLIMOR
                  ORGANIZATION  IS INDEXED
                  ACCESS        IS DYNAMIC
                  RECORD KEY    IS TLIM-LLAVE
                  FILE STATUS   IS FS-TLIMOR
                                   FSE-TLIMOR.

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *               DEFINICION DE ESTRUCTURA DE ARCHIVOS             *
      ******************************************************************
      *   MAESTRO DE INFORMACION GENERAL DE TH.
      *   MAESTRO DE INFORMACION GENERAL DE TARJETA INST.
      *   MAESTRO DE PARAMATROS POR CUENTA
      *   MAESTRO ANEXO MAESTRO DE SALDOS MONEDA LOCAL
      *   MAESTRO ANEXO MAESTRO DE SALDOS MONEDA DOLARES
      *   MAESTRO ANEXO MAESTRO DE SALDOS MONEDA LOCAL
      *   HISTORICO DE MORAS DIAS  PERDIDA ESPERADA
       FD TLMATH.
          COPY TLMATH4.
       FD TIMATH.
          COPY TIMATH2.
       FD TLPRCT.
          COPY TLPRCT.
       FD TLAXLQ.
          COPY TLAXLQ.
       FD TLAXLD.
          COPY TLAXLD.
       FD TIAXL1.
          COPY TIAXL1.
       FD TLIMOR.
          COPY TLIMOR.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *           RECURSOS RUTINAS FSE Y VALIDACION FILE-STATUS        *
      ******************************************************************
       01 WKS-FS-STATUS.
          02 WKS-STATUS.
      *      MAESTRO DE INFORMACION GENERAL DE TH.
             04 FS-TLMATH              PIC 9(02) VALUE ZEROES.
             04 FSE-TLMATH.
                08 FSE-RETURN          PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FUNCTION        PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FEEDBACK        PIC S9(4) COMP-5 VALUE 0.
      *      MAESTRO DE INFORMACION GENERAL DE TARJETA INST.
             04 FS-TIMATH              PIC 9(02) VALUE ZEROES.
             04 FSE-TIMATH.
                08 FSE-RETURN          PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FUNCTION        PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FEEDBACK        PIC S9(4) COMP-5 VALUE 0.
      *      MAESTRO DE PARAMATROS POR CUENTA
             04 FS-TLPRCT              PIC 9(02) VALUE ZEROES.
             04 FSE-TLPRCT.
                08 FSE-RETURN          PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FUNCTION        PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FEEDBACK        PIC S9(4) COMP-5 VALUE 0.
      *      MAESTRO ANEXO MAESTRO DE SALDOS MONEDA LOCAL
             04 FS-TLAXLQ              PIC 9(02) VALUE ZEROES.
             04 FSE-TLAXLQ.
                08 FSE-RETURN          PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FUNCTION        PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FEEDBACK        PIC S9(4) COMP-5 VALUE 0.
      *      MAESTRO ANEXO MAESTRO DE SALDOS MONEDA DOLARES
             04 FS-TLAXLD              PIC 9(02) VALUE ZEROES.
             04 FSE-TLAXLD.
                08 FSE-RETURN          PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FUNCTION        PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FEEDBACK        PIC S9(4) COMP-5 VALUE 0.
      *      MAESTRO ANEXO MAESTRO DE SALDOS MONEDA LOCAL
             04 FS-TIAXL1              PIC 9(02) VALUE ZEROES.
             04 FSE-TIAXL1.
                08 FSE-RETURN          PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FUNCTION        PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FEEDBACK        PIC S9(4) COMP-5 VALUE 0.
      *      HISTORICO DE MORAS DIAS  PERDIDA ESPERADA
             04 FS-TLIMOR              PIC 9(02) VALUE ZEROES.
             04 FSE-TLIMOR.
                08 FSE-RETURN          PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FUNCTION        PIC S9(4) COMP-5 VALUE 0.
                08 FSE-FEEDBACK        PIC S9(4) COMP-5 VALUE 0.
      *      VARIABLES RUTINA DE FSE
             04 PROGRAMA               PIC X(08) VALUE SPACES.
             04 ARCHIVO                PIC X(08) VALUE SPACES.
             04 ACCION                 PIC X(10) VALUE SPACES.
             04 LLAVE                  PIC X(32) VALUE SPACES.
      ******************************************************************
      *              RECURSOS DE TRABAJO Y VARIABLES AUXILIARES        *
      ******************************************************************
       01 WKS-VARIABLES-TRABAJO.
          02 WKS-CICLOS-INTERES                PIC 9(02) VALUE ZEROES.
          02 WKS-CICLOS-MORAS                  PIC 9(02) VALUE ZEROES.
          02 WKS-CICLO-MAYOR                   PIC 9(02) VALUE ZEROES.
          02 WKS-I                             PIC 9(02) VALUE ZEROES.
          02 WKS-X                             PIC 9(02) VALUE ZEROES.
          02 WKS-Y                             PIC 9(02) VALUE ZEROES.
          02 ESCRITOS-TLIMOR                   PIC 9(07) VALUE ZEROES.
          02 WKS-CON-MORA-Q                    PIC 9(07) VALUE ZEROES.
          02 WKS-CON-MORA-D                    PIC 9(07) VALUE ZEROES.
          02 WKS-CON-MORA-TI                   PIC 9(07) VALUE ZEROES.
          02 WKS-MASCARA                   PIC Z,ZZZ,ZZ9 VALUE ZEROES.
          02 FECHAS.
             04 FECHA-INICIO                   PIC 9(08).
             04 FILLER                         PIC X(01).
             04 FECHA-FIN                      PIC 9(08).
          02 WKS-IMPORTANTES.
             04 WKS-MORAS                      PIC 9(02) VALUE ZEROES.
             04 WKS-SITUACION-CUENTA           PIC 9(02) VALUE ZEROES.
             04 WKS-DIA-CORTE                  PIC 9(02) VALUE ZEROES.
             04 WKS-DIFERENCIAL                PIC S9(2) VALUE ZEROES.
             04 WKS-DIA-GRACIA                 PIC 9(02) VALUE ZEROES.
             04 WKS-NO-DIAS                    PIC S9(3) VALUE ZEROES.
             04 WKS-TOTAL-MORAS                PIC 9(02) VALUE ZEROES.
             04 WKS-I-MES                      PIC 9(02) VALUE ZEROES.
      *--> AQUI SE COLOCA LA FECHA FIN QUE VIENE DE SYSIPT
          02 WKS-FECHA                         PIC 9(08) VALUE ZEROES.
          02 WKS-FECHA-R REDEFINES WKS-FECHA.
             04 WKS-ANI                        PIC 9(04).
             04 WKS-MES                        PIC 9(02).
             04 WKS-DIA                        PIC 9(02).
      *--> FECHA DE TRASLADO ES LA FECHA DE INICIO EN TLMORH
          02 WKS-FECHA-TRASLADO                PIC 9(08) VALUE ZEROES.
          02 WKS-FECHA-TRASLADO-R REDEFINES WKS-FECHA-TRASLADO.
             04 WKS-ANI-TRASLADO               PIC 9(04).
             04 WKS-MES-TRASLADO               PIC 9(02).
             04 WKS-DIA-TRASLADO               PIC 9(02).
      *--> FECHA FIN ES LA FECHA FIN EN TLMORH
          02 WKS-FECHA-FIN                     PIC 9(08) VALUE ZEROES.
          02 WKS-FECHA-FIN-R      REDEFINES WKS-FECHA-FIN.
             04 WKS-ANI-F                      PIC 9(04).
             04 WKS-MES-F                      PIC 9(02).
             04 WKS-DIA-F                      PIC 9(02).
          02 WKS-FECHA-AUX.
             04 WKS-ANI-AUX                    PIC 9(04).
             04 WKS-MES-AUX                    PIC 9(02).
             04 WKS-DIA-AUX                    PIC 9(02).
          02 WKS-FLAGS.
             04 WKS-FIN-MES                    PIC 9(01) VALUE ZEROES.
                88 SI-FIN-MES                            VALUE 1.
             04 WKS-BINES-ONLINE               PIC 9(06) VALUE ZEROES.
                88 BI-ONLINE                             VALUE 423247,
                                               428594, 422616, 402799.
             04 WKS-BIN-LOC                    PIC 9(06) VALUE ZEROES.
                88 BIN-LOC                               VALUE 050830,
                                               050834, 050836, 050840,
                                               050846, 050850, 050851,
                                               423615, 423616, 429132,
                                               430335, 430336, 432175,
                                               450834, 486311, 512314.
             04 WKS-BIN-INSTITU                PIC 9(06).
                88 BIN-INSTITU                           VALUE 050837.
             04 WKS-FIN-TLMATH                 PIC 9(01) VALUE ZEROES.
                88 FIN-TLMATH                            VALUE 1.
             04 WKS-FIN-TIMATH                 PIC 9(01) VALUE ZEROES.
                88 FIN-TIMATH                            VALUE 1.
             04 WKS-CUENTA-OK-TLMATH           PIC 9(01) VALUE ZEROES.
                88 CUENTA-OK                             VALUE 0.

       01  TABLA-DIAS.
           02 FILLER        PIC X(24) VALUE '312831303130313130313031'.
       01  F REDEFINES TABLA-DIAS.
           02 DIA-FIN-MES       PIC 99 OCCURS 12 TIMES.
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      *               S E C C I O N    P R I N C I P A L
      ******************************************************************
       000-MAIN SECTION.
           PERFORM APERTURA-ARCHIVOS
           PERFORM VALIDACION-FIN-DE-MES
           PERFORM BUSCAR-MORAS-TCS        UNTIL FIN-TLMATH
           PERFORM CIERRA-Y-ABRE-IO-TLIMOR
           PERFORM BUSCAR-MORAS-TC-INST    UNTIL FIN-TIMATH
           PERFORM ESTADISTICAS
           PERFORM CIERRA-ARCHIVOS
           STOP RUN.
       000-MAIN-E. EXIT.

       APERTURA-ARCHIVOS SECTION.
           ACCEPT FECHAS      FROM SYSIN
           MOVE   FECHA-FIN   TO   WKS-FECHA
           MOVE   'CIERRES1'  TO   PROGRAMA
           OPEN INPUT  TLMATH TIMATH TLAXLQ
                       TLAXLD TIAXL1 TLPRCT
                OUTPUT TLIMOR
           IF FS-TLMATH NOT EQUAL 0 AND 97
              MOVE 'OPEN'     TO    ACCION
              MOVE SPACES     TO    LLAVE
              MOVE 'TLMATH'   TO    ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
                                    FS-TLMATH, FSE-TLMATH
              PERFORM CIERRA-ARCHIVOS
              DISPLAY ">>> ALGO SALIO MAL AL ABRIR ARCHIVO TLMATH<<<"
                      UPON CONSOLE
              DISPLAY "       >>> VERIFICAR DETALLES EN SPOOL <<<"
                      UPON CONSOLE
              STOP RUN
           END-IF

           IF FS-TIMATH NOT EQUAL 0 AND 97
              MOVE 'OPEN'     TO    ACCION
              MOVE SPACES     TO    LLAVE
              MOVE 'TIMATH'   TO    ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
                                    FS-TIMATH, FSE-TIMATH
              PERFORM CIERRA-ARCHIVOS
              MOVE  91        TO RETURN-CODE
              DISPLAY ">>> ALGO SALIO MAL AL ABRIR ARCHIVO TIMATH<<<"
                      UPON CONSOLE
              DISPLAY "       >>> VERIFICAR DETALLES EN SPOOL <<<"
                      UPON CONSOLE
              STOP RUN
           END-IF

           IF FS-TLPRCT NOT EQUAL 0 AND 97
              MOVE 'OPEN'     TO    ACCION
              MOVE SPACES     TO    LLAVE
              MOVE 'TLPRCT'   TO    ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
                                    FS-TLPRCT, FSE-TLPRCT
              PERFORM CIERRA-ARCHIVOS
              MOVE  91        TO RETURN-CODE
              DISPLAY ">>> ALGO SALIO MAL AL ABRIR ARCHIVO TLPRCT<<<"
                      UPON CONSOLE
              DISPLAY "       >>> VERIFICAR DETALLES EN SPOOL <<<"
                      UPON CONSOLE
              STOP RUN
           END-IF

           IF FS-TLAXLQ NOT EQUAL 0 AND 97
              MOVE 'OPEN'     TO    ACCION
              MOVE SPACES     TO    LLAVE
              MOVE 'TLAXLQ'   TO    ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
                                    FS-TLAXLQ, FSE-TLAXLQ
              PERFORM CIERRA-ARCHIVOS
              MOVE  91        TO RETURN-CODE
              DISPLAY ">>> ALGO SALIO MAL AL ABRIR ARCHIVO TLAXLQ<<<"
                      UPON CONSOLE
              DISPLAY "       >>> VERIFICAR DETALLES EN SPOOL <<<"
                      UPON CONSOLE
              STOP RUN
           END-IF

           IF FS-TLAXLD NOT EQUAL 0 AND 97
              MOVE 'OPEN'     TO    ACCION
              MOVE SPACES     TO    LLAVE
              MOVE 'TLAXLD'   TO    ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
                                    FS-TLAXLD, FSE-TLAXLD
              PERFORM CIERRA-ARCHIVOS
              MOVE  91        TO RETURN-CODE
              DISPLAY ">>> ALGO SALIO MAL AL ABRIR ARCHIVO TLAXLD<<<"
                      UPON CONSOLE
              DISPLAY "       >>> VERIFICAR DETALLES EN SPOOL <<<"
                      UPON CONSOLE
              STOP RUN
           END-IF

           IF FS-TIAXL1 NOT EQUAL 0 AND 97
              MOVE 'OPEN'     TO    ACCION
              MOVE SPACES     TO    LLAVE
              MOVE 'TIAXL1'   TO    ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
                                    FS-TIAXL1, FSE-TIAXL1
              PERFORM CIERRA-ARCHIVOS
              MOVE  91        TO RETURN-CODE
              DISPLAY ">>> ALGO SALIO MAL AL ABRIR ARCHIVO TIAXL1<<<"
                      UPON CONSOLE
              DISPLAY "       >>> VERIFICAR DETALLES EN SPOOL <<<"
                      UPON CONSOLE
              STOP RUN
           END-IF

           IF FS-TLIMOR NOT EQUAL 0 AND 97 AND 35
              MOVE 'OPEN'     TO    ACCION
              MOVE SPACES     TO    LLAVE
              MOVE 'TLIMOR'   TO    ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE,
                                    FS-TLIMOR, FSE-TLIMOR
              PERFORM CIERRA-ARCHIVOS
              MOVE  91        TO RETURN-CODE
              DISPLAY ">>> ALGO SALIO MAL AL ABRIR ARCHIVO TLIMOR<<<"
                      UPON CONSOLE
              DISPLAY "       >>> VERIFICAR DETALLES EN SPOOL <<<"
                      UPON CONSOLE
              STOP RUN
           END-IF.
       APERTURA-ARCHIVOS-E. EXIT.

       VALIDACION-FIN-DE-MES SECTION.
           IF DIA-FIN-MES (WKS-MES) =  WKS-DIA
              MOVE 1 TO WKS-FIN-MES
           ELSE
              MOVE 0 TO WKS-FIN-MES
           END-IF.
       VALIDACION-FIN-DE-MES-E. EXIT.

      ******************************************************************
      *                    B U S C A   M O R A S   T C                 *
      ******************************************************************
       BUSCAR-MORAS-TCS SECTION.
      ******************************************************************
      *                       CONDICIONALES PARA                       *
      *         VALIDAR SI NO ES UNA BI CREDIT ONLINE O UNA LOCAL      *
      ******************************************************************
           PERFORM LEE-TLMATH
           MOVE TLMT-BIN TO WKS-BINES-ONLINE WKS-BIN-LOC

           IF NOT BI-ONLINE
              IF (TLMT-SITUACION-CUENTA = 3 OR 15)
                 MOVE 1 TO WKS-CUENTA-OK-TLMATH
              ELSE
                 IF (TLMT-SITUACION-CUENTA = 4 OR 7) AND
                    (TLMT-CUENTA-NUEVA > 0)
                    MOVE 1 TO WKS-CUENTA-OK-TLMATH
                 ELSE
                    MOVE 0 TO WKS-CUENTA-OK-TLMATH
                 END-IF
              END-IF
           ELSE
              MOVE 1 TO WKS-CUENTA-OK-TLMATH
           END-IF

      ******************************************************************
      *          B U S C A   M O R A S   E N   Q U E T Z A L E S  TC   *
      ******************************************************************
           IF CUENTA-OK
              PERFORM VALIDA-CICLOS-MAYOR-GTQ
              IF FS-TLAXLQ = 0       AND WKS-CICLO-MAYOR > 0
                 MOVE LOW-VALUES      TO WKS-IMPORTANTES
                 INITIALIZE              REG-TLIMOR
                 MOVE TLMT-LLAVE      TO TLPC-LLAVE TLIM-CUENTA
                 MOVE 1               TO TLIM-MONEDA
                 MOVE WKS-CICLO-MAYOR TO WKS-MORAS WKS-TOTAL-MORAS
                 PERFORM LEE-TLPRCT
                 IF FS-TLPRCT = 0
                    ADD     1 TO WKS-CON-MORA-Q
                    PERFORM MUEVE-DATOS
                    PERFORM AGREGA-CUENTA-NUEVA-MORA VARYING WKS-I
                            FROM 1 BY 1 UNTIL WKS-I > WKS-MORAS
                    PERFORM ESCRIBE-TLIMOR
                 END-IF
              END-IF

      ******************************************************************
      *            B U S C A   M O R A S   E N   D O L A R E S      TC *
      ******************************************************************
              IF NOT BIN-LOC
                 PERFORM VALIDA-CICLOS-MAYOR-USD
                 IF FS-TLAXLD = 0 AND WKS-CICLO-MAYOR > 0
                    MOVE LOW-VALUES TO WKS-IMPORTANTES
                    INITIALIZE         REG-TLIMOR
                    MOVE TLMT-LLAVE TO TLPC-LLAVE TLIM-CUENTA
                    MOVE 2          TO TLIM-MONEDA
                    MOVE WKS-CICLO-MAYOR TO WKS-MORAS WKS-TOTAL-MORAS
                    PERFORM LEE-TLPRCT
                    IF FS-TLPRCT = 0
                       ADD     1 TO WKS-CON-MORA-D
                       PERFORM MUEVE-DATOS
                       PERFORM AGREGA-CUENTA-NUEVA-MORA VARYING WKS-I
                               FROM 1 BY 1 UNTIL WKS-I > WKS-MORAS
                       PERFORM ESCRIBE-TLIMOR
                    END-IF
                 END-IF
              END-IF

           END-IF.
       BUSCAR-MORAS-TCS-E. EXIT.

      ******************************************************************
      *     B U S C A   M O R A S   T C   I N S T I T U C I O N A L    *
      ******************************************************************
       BUSCAR-MORAS-TC-INST  SECTION.
           PERFORM LEE-TIMATH
           PERFORM VALIDA-CICLOS-MAYOR-TCI
           IF FS-TIAXL1  = 0 AND WKS-CICLO-MAYOR > 0
              MOVE LOW-VALUES TO WKS-IMPORTANTES
              INITIALIZE         REG-TLIMOR
              MOVE TIMT-LLAVE TO TLIM-CUENTA
              MOVE 01         TO TLIM-MONEDA
              MOVE WKS-CICLO-MAYOR TO WKS-MORAS WKS-TOTAL-MORAS
              ADD  1          TO WKS-CON-MORA-TI
              PERFORM MUEVE-DATOS-TI
              PERFORM AGREGA-CUENTA-NUEVA-MORA-TI VARYING WKS-I FROM 1
                      BY 1 UNTIL WKS-I > WKS-MORAS
              PERFORM BUSCA-TLIMOR
           END-IF.
       BUSCAR-MORAS-TC-INST-E. EXIT.

       VALIDA-CICLOS-MAYOR-GTQ SECTION.
           MOVE    TLMT-LLAVE TO TLAL-LLAVE
           PERFORM LEE-TLAXLQ
           IF FS-TLAXLQ = 0 THEN
      *--> UNICAMENTE SE VALIDAN LOS CICLOS DE INTERES (1) Y MORA (3)
              MOVE 0 TO WKS-CICLOS-INTERES
              MOVE 0 TO WKS-CICLOS-MORAS
              MOVE 0 TO WKS-CICLO-MAYOR

              PERFORM VARYING WKS-X FROM 1 BY 2 UNTIL WKS-X > 3
                MOVE    0  TO WKS-Y
                PERFORM 18 TIMES
                   ADD  1  TO WKS-Y
                   IF (WKS-X = 1) AND TLAL-CICLOS (WKS-X WKS-Y) > 0
                      ADD 1 TO WKS-CICLOS-INTERES
                   END-IF
                   IF (WKS-X = 3) AND TLAL-CICLOS (WKS-X WKS-Y) > 0
                      ADD 1 TO WKS-CICLOS-MORAS
                   END-IF
                END-PERFORM
              END-PERFORM
              IF WKS-CICLOS-INTERES >= WKS-CICLOS-MORAS
                 MOVE WKS-CICLOS-INTERES TO WKS-CICLO-MAYOR
              ELSE
                 MOVE WKS-CICLOS-MORAS   TO WKS-CICLO-MAYOR
              END-IF
           END-IF.
       VALIDA-CICLOS-MAYOR-GTQ-E. EXIT.

       VALIDA-CICLOS-MAYOR-USD SECTION.
           MOVE    TLMT-LLAVE TO TLAE-LLAVE
           PERFORM LEE-TLAXLD
           IF FS-TLAXLD = 0 THEN
      *--> UNICAMENTE SE VALIDAN LOS CICLOS DE INTERES (1) Y MORA (3)
              MOVE 0 TO WKS-CICLOS-INTERES
              MOVE 0 TO WKS-CICLOS-MORAS
              MOVE 0 TO WKS-CICLO-MAYOR

              PERFORM VARYING WKS-X FROM 1 BY 2 UNTIL WKS-X > 3
                MOVE    0  TO WKS-Y
                PERFORM 18 TIMES
                   ADD  1  TO WKS-Y
                   IF (WKS-X = 1) AND TLAE-CICLOS (WKS-X WKS-Y) > 0
                      ADD 1 TO WKS-CICLOS-INTERES
                   END-IF
                   IF (WKS-X = 3) AND TLAE-CICLOS (WKS-X WKS-Y) > 0
                      ADD 1 TO WKS-CICLOS-MORAS
                   END-IF
                END-PERFORM
              END-PERFORM
              IF WKS-CICLOS-INTERES >= WKS-CICLOS-MORAS
                 MOVE WKS-CICLOS-INTERES TO WKS-CICLO-MAYOR
              ELSE
                 MOVE WKS-CICLOS-MORAS   TO WKS-CICLO-MAYOR
              END-IF
           END-IF.
       VALIDA-CICLOS-MAYOR-USD-E. EXIT.

       VALIDA-CICLOS-MAYOR-TCI SECTION.
           MOVE    TIMT-LLAVE TO TIAL-LLAVE
           PERFORM LEE-TIAXL1
           IF FS-TIAXL1 = 0 THEN
      *--> UNICAMENTE SE VALIDAN LOS CICLOS DE INTERES (1) Y MORA (3)
              MOVE 0 TO WKS-CICLOS-INTERES
              MOVE 0 TO WKS-CICLOS-MORAS
              MOVE 0 TO WKS-CICLO-MAYOR

              PERFORM VARYING WKS-X FROM 1 BY 2 UNTIL WKS-X > 3
                MOVE    0  TO WKS-Y
                PERFORM 18 TIMES
                   ADD  1  TO WKS-Y
                   IF (WKS-X = 1) AND TIAL-CICLOS (WKS-X WKS-Y) > 0
                      ADD 1 TO WKS-CICLOS-INTERES
                   END-IF
                   IF (WKS-X = 3) AND TIAL-CICLOS (WKS-X WKS-Y) > 0
                      ADD 1 TO WKS-CICLOS-MORAS
                   END-IF
                END-PERFORM
              END-PERFORM
              IF WKS-CICLOS-INTERES >= WKS-CICLOS-MORAS
                 MOVE WKS-CICLOS-INTERES TO WKS-CICLO-MAYOR
              ELSE
                 MOVE WKS-CICLOS-MORAS   TO WKS-CICLO-MAYOR
              END-IF
           END-IF.
       VALIDA-CICLOS-MAYOR-TCI-E. EXIT.
      ******************************************************************
      *              ACTUALIZACION DE CICLOS Y MORAS
      ******************************************************************
       MUEVE-DATOS SECTION.
           MOVE TLMT-SITUACION-CUENTA         TO WKS-SITUACION-CUENTA
           MOVE TLMT-DIA-CORTE                TO WKS-DIA-CORTE
           MOVE TLPC-VALOR-PARAMETRO-E (7)    TO WKS-DIA-GRACIA
           MOVE TLPC-VALOR-PARAMETRO-E (8)    TO WKS-DIFERENCIAL.
       MUEVE-DATOS-E. EXIT.

       MUEVE-DATOS-TI SECTION.
           MOVE TIMT-SITUACION-CUENTA         TO WKS-SITUACION-CUENTA
           MOVE TIMT-DIA-CORTE                TO WKS-DIA-CORTE
           MOVE 0                             TO WKS-DIA-GRACIA
           MOVE 0                             TO WKS-DIFERENCIAL.
       MUEVE-DATOS-TI-E. EXIT.

       AGREGA-CUENTA-NUEVA-MORA SECTION.
           IF WKS-I = 1 THEN
              IF SI-FIN-MES THEN
                 COMPUTE WKS-MES-TRASLADO = WKS-MES
              ELSE
                 COMPUTE WKS-MES-TRASLADO = WKS-MES - 1
              END-IF
              IF WKS-MES-TRASLADO = 0
                  COMPUTE WKS-MES-TRASLADO = WKS-MES-TRASLADO + 12
                  COMPUTE WKS-MES-F        = 01
                  COMPUTE WKS-ANI-TRASLADO = WKS-ANI - 1
                  COMPUTE WKS-ANI-F        = WKS-ANI
              ELSE
                  COMPUTE WKS-MES-F        = WKS-MES
                  COMPUTE WKS-ANI-F        = WKS-ANI
                  COMPUTE WKS-ANI-TRASLADO = WKS-ANI
              END-IF
              COMPUTE WKS-DIA-F        = WKS-DIA
              COMPUTE WKS-DIA-TRASLADO = WKS-DIA-CORTE + WKS-DIFERENCIAL
              IF WKS-DIA-TRASLADO      = 0
                 COMPUTE WKS-DIA-TRASLADO = WKS-DIA-TRASLADO +
                                            WKS-DIA-GRACIA   + 2
              ELSE
                 COMPUTE WKS-DIA-TRASLADO = WKS-DIA-TRASLADO +
                                            WKS-DIA-GRACIA   + 1
              END-IF
              MOVE WKS-FECHA-TRASLADO TO WKS-FECHA-AUX
              SUBTRACT 1 FROM WKS-DIA-AUX
              ADD      1 TO   WKS-MES-AUX
              IF WKS-MES-AUX > 12 THEN
                 MOVE 01 TO   WKS-MES-AUX
                 ADD   1 TO   WKS-ANI-AUX
              END-IF
      *--> CONDICION VERDADERA SI YA PASO SU FECHA DE FIN DE CICLO
              IF WKS-FECHA-AUX <= WKS-FECHA
                 ADD 1 TO WKS-MES-TRASLADO
              END-IF
              MOVE WKS-MES-TRASLADO TO WKS-I-MES
           ELSE
              COMPUTE WKS-MES-TRASLADO = WKS-MES-TRASLADO  - 1
              COMPUTE WKS-MES-F        = WKS-MES-TRASLADO  + 1
              COMPUTE WKS-DIA-TRASLADO = WKS-DIA-CORTE + WKS-DIFERENCIAL
              IF WKS-DIA-TRASLADO      = 0
                 COMPUTE WKS-DIA-TRASLADO = WKS-DIA-TRASLADO +
                                            WKS-DIA-GRACIA   + 2

                 COMPUTE WKS-DIA-F        = WKS-DIA-CORTE    +
                                            WKS-DIFERENCIAL  +
                                            WKS-DIA-GRACIA   + 1
              ELSE
                 COMPUTE WKS-DIA-TRASLADO = WKS-DIA-TRASLADO +
                                            WKS-DIA-GRACIA   + 1

                 COMPUTE WKS-DIA-F        = WKS-DIA-CORTE    +
                                            WKS-DIFERENCIAL  +
                                            WKS-DIA-GRACIA
              END-IF

              COMPUTE WKS-ANI-TRASLADO = WKS-ANI
              COMPUTE WKS-ANI-F        = WKS-ANI
              IF WKS-MES-TRASLADO = 0
                  COMPUTE WKS-MES-TRASLADO = WKS-MES-TRASLADO + 12
                  COMPUTE WKS-MES-F        = 1
                  COMPUTE WKS-ANI-TRASLADO = WKS-ANI - 1
                  COMPUTE WKS-ANI-F        = WKS-ANI
              ELSE
              IF WKS-I > WKS-I-MES
                     COMPUTE WKS-ANI-TRASLADO = WKS-ANI - 1
                     COMPUTE WKS-ANI-F        = WKS-ANI - 1
                 END-IF
              END-IF
           END-IF
           COMPUTE WKS-NO-DIAS     = FUNCTION INTEGER-OF-DATE
                   (WKS-FECHA-FIN) - FUNCTION INTEGER-OF-DATE
                   (WKS-FECHA-TRASLADO)

           IF WKS-NO-DIAS < 0 THEN
              MOVE ZEROES TO WKS-NO-DIAS
           ELSE
              COMPUTE WKS-NO-DIAS     = WKS-NO-DIAS + 1
           END-IF

      *--> MUEVE LOS DATOS A VARIABLES DEL TLMORH
           MOVE WKS-I                TO TLIM-CICLO-VENCIDO (WKS-I)
           MOVE WKS-FECHA-TRASLADO   TO TLIM-FECHA-INICIO  (WKS-I)
           MOVE WKS-FECHA-FIN        TO TLIM-FECHA-FIN     (WKS-I)
           MOVE WKS-NO-DIAS          TO TLIM-DIAS-MORA     (WKS-I)
           MOVE WKS-DIA-GRACIA       TO TLIM-DIA-GRACIA    (WKS-I)
           MOVE WKS-DIFERENCIAL      TO TLIM-DIFERENCIAL   (WKS-I)
           MOVE WKS-DIA-CORTE        TO TLIM-DIA-CORTE     (WKS-I)
           MOVE WKS-SITUACION-CUENTA TO TLIM-SITUACION-CTA (WKS-I)
           MOVE WKS-TOTAL-MORAS      TO TLIM-MORAS         (WKS-I).
       AGREGA-CUENTA-NUEVA-MORA-E. EXIT.


       AGREGA-CUENTA-NUEVA-MORA-TI  SECTION.
           IF WKS-I = 1
              MOVE WKS-FECHA TO WKS-FECHA-FIN  WKS-FECHA-TRASLADO

              IF SI-FIN-MES THEN
                 ADD 1 TO WKS-MES-TRASLADO
                 IF WKS-MES-TRASLADO > 12 THEN
                    MOVE 01 TO WKS-MES-TRASLADO
                    ADD   1 TO WKS-ANI-TRASLADO
                 END-IF
              END-IF
              COMPUTE WKS-DIA-TRASLADO = 01
           ELSE
              COMPUTE WKS-MES-TRASLADO = WKS-MES-TRASLADO - 1
              COMPUTE WKS-MES-F        = WKS-MES-TRASLADO
              IF WKS-MES-TRASLADO = 0
                  MOVE 12 TO WKS-MES-TRASLADO WKS-MES-F
                  COMPUTE WKS-ANI-TRASLADO = WKS-ANI - 1
                  COMPUTE WKS-ANI-F        = WKS-ANI - 1
              END-IF
              COMPUTE WKS-DIA-TRASLADO = 01
              COMPUTE WKS-DIA-F        = DIA-FIN-MES (WKS-MES-F)
           END-IF
           COMPUTE WKS-NO-DIAS = FUNCTION INTEGER-OF-DATE
                                                         (WKS-FECHA-FIN)
                               - FUNCTION INTEGER-OF-DATE
                                                    (WKS-FECHA-TRASLADO)
           IF WKS-NO-DIAS < 0 THEN
              MOVE ZEROES TO WKS-NO-DIAS
           ELSE
             COMPUTE WKS-NO-DIAS = WKS-NO-DIAS + 1
           END-IF

      *--> MUEVE LOS DATOS A VARIABLES DEL TLMORH
           MOVE WKS-I                TO TLIM-CICLO-VENCIDO (WKS-I)
           MOVE WKS-FECHA-TRASLADO   TO TLIM-FECHA-INICIO  (WKS-I)
           MOVE WKS-FECHA-FIN        TO TLIM-FECHA-FIN     (WKS-I)
           MOVE WKS-NO-DIAS          TO TLIM-DIAS-MORA     (WKS-I)
           MOVE WKS-DIA-GRACIA       TO TLIM-DIA-GRACIA    (WKS-I)
           MOVE WKS-DIFERENCIAL      TO TLIM-DIFERENCIAL   (WKS-I)
           MOVE WKS-DIA-CORTE        TO TLIM-DIA-CORTE     (WKS-I)
           MOVE WKS-SITUACION-CUENTA TO TLIM-SITUACION-CTA (WKS-I)
           MOVE WKS-TOTAL-MORAS      TO TLIM-MORAS         (WKS-I).
       AGREGA-CUENTA-NUEVA-MORA-TI-E. EXIT.

       BUSCA-TLIMOR SECTION.
           MOVE TIMT-LLAVE TO TLIM-CUENTA
           MOVE 1          TO TLIM-MONEDA
           READ TLIMOR KEY IS TLIM-LLAVE
             INVALID KEY
                PERFORM ESCRIBE-TLIMOR
             NOT INVALID KEY
                DISPLAY 'TLIMOR, ERROR REGISTRO YA AGREGADO: '
                        TLIM-CUENTA
           END-READ.
       BUSCA-TLIMOR-E. EXIT.

       ESCRIBE-TLIMOR SECTION.
           WRITE REG-TLIMOR
           IF FS-TLIMOR NOT = 0 THEN
                DISPLAY 'ERROR AL GRABAR TLIMOR, STATUS: ' FS-TLIMOR
                        'CUENTA: ' TLIM-CUENTA 'MONEDA: '  TLIM-MONEDA
           ELSE
                ADD 1 TO ESCRITOS-TLIMOR
           END-IF.
       ESCRIBE-TLIMOR-E. EXIT.

       CIERRA-Y-ABRE-IO-TLIMOR SECTION.
           CLOSE TLIMOR
           OPEN  I-O TLIMOR.
       CIERRA-Y-ABRE-IO-TLIMOR-E. EXIT.

      ******************************************************************
      *              L E C T U R A S   A   M A E S T R O S             *
      ******************************************************************
       LEE-TLMATH SECTION.
           READ TLMATH NEXT RECORD
             AT END
                MOVE 1 TO WKS-FIN-TLMATH
           END-READ.
       LEE-TLMATH-E. EXIT.

       LEE-TLAXLQ SECTION.
           READ TLAXLQ KEY IS TLAL-LLAVE
             INVALID KEY
               DISPLAY 'TLAXLQ, ERROR NO ENCONTRO CUENTA: ' TLAL-LLAVE
           END-READ.
        LEE-TLAXLQ-E. EXIT.

       LEE-TLAXLD SECTION.
           READ TLAXLD KEY IS TLAE-LLAVE
             INVALID KEY
               DISPLAY 'TLAXLD, ERROR NO ENCONTRO CUENTA: ' TLAE-LLAVE
           END-READ.
       LEE-TLAXLD-E. EXIT.

       LEE-TLPRCT SECTION.
           READ TLPRCT KEY IS TLPC-LLAVE
             INVALID KEY
               DISPLAY 'TLPRCT, ERROR NO ENCONTRO CUENTA: ' TLPC-LLAVE
           END-READ.
       LEE-TLPRCT-E. EXIT.

       LEE-TIMATH SECTION.
           READ TIMATH NEXT RECORD
             AT END
                MOVE 1 TO WKS-FIN-TIMATH
           END-READ.

       LEE-TIAXL1 SECTION.
           READ TIAXL1 KEY IS TIAL-LLAVE
             INVALID KEY
               DISPLAY 'TIAXL1, ERROR NO ENCONTRO CUENTA: ' TIAL-LLAVE
           END-READ.
       LEE-TIAXL1-E. EXIT.

       ESTADISTICAS SECTION.
           DISPLAY '******************************************'
           MOVE    WKS-CON-MORA-Q    TO   WKS-MASCARA
           DISPLAY 'REGISTROS CON MORA EN Q:     ' WKS-MASCARA
           MOVE    WKS-CON-MORA-D    TO   WKS-MASCARA
           DISPLAY 'REGISTROS CON MORA EN D:     ' WKS-MASCARA
           MOVE    WKS-CON-MORA-TI   TO   WKS-MASCARA
           DISPLAY 'REGISTROS CON MORA TI Q:     '  WKS-MASCARA
           MOVE    ESCRITOS-TLIMOR   TO   WKS-MASCARA
           DISPLAY 'TOTAL DE REGISTROS ESCRITOS: ' WKS-MASCARA
           DISPLAY '******************************************'.
       ESTADISTICAS-E. EXIT.

       CIERRA-ARCHIVOS SECTION.
           CLOSE TLMATH  TIMATH
                 TLPRCT  TLAXLQ
                 TLAXLD  TIAXL1
                 TLIMOR.
       CIERRA-ARCHIVOS-E. EXIT.