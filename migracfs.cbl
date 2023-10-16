       IDENTIFICATION DIVISION.
       PROGRAM-ID.                    MIGRACFS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ******************************************************************
      *              A R C H I V O S   D E   E N T R A D A
      ******************************************************************
            SELECT TLMATH  ASSIGN   TO TLMATH
                   ORGANIZATION     IS INDEXED
                   ACCESS MODE      IS SEQUENTIAL
                   RECORD KEY       IS TLMT-LLAVE
                   FILE STATUS      IS FS-TLMATH
                                       FSE-TLMATH.
            SELECT TIMATH  ASSIGN   TO TIMATH
                   ORGANIZATION     IS INDEXED
                   ACCESS MODE      IS SEQUENTIAL
                   RECORD KEY       IS TIMT-LLAVE
                   FILE STATUS      IS FS-TIMATH
                                       FSE-TIMATH.

            SELECT TLMACO  ASSIGN   TO TLMACO
                   ORGANIZATION     IS INDEXED
                   ACCESS MODE      IS RANDOM
                   RECORD KEY       IS TLMO-LLAVE
                   FILE STATUS      IS FS-TLMACO
                                       FSE-TLMACO.
            SELECT TLTGEN  ASSIGN   TO TLTGEN
                   ORGANIZATION     IS INDEXED
                   ACCESS           IS DYNAMIC
                   RECORD KEY       IS TLTG-LLAVE
                   FILE STATUS      IS FS-TLTGEN
                                       FSE-TLTGEN.
            SELECT TLENBL  ASSIGN   TO TLENBL
                   ORGANIZATION     IS INDEXED
                   ACCESS           IS DYNAMIC
                   RECORD KEY       IS TLBL-LLAVE
                   FILE STATUS      IS FS-TLENBL
                                       FSE-TLENBL.
            SELECT MASTERDB ASSIGN  TO MASTERDB
                   ORGANIZATION     IS SEQUENTIAL
                   FILE STATUS      IS FS-MCDB.
            SELECT VISARDB ASSIGN   TO VISARDB
                   ORGANIZATION     IS SEQUENTIAL
                   FILE STATUS      IS FS-VSDB.
      ******************************************************************
      *              A R C H I V O S   D E   S A L I D A
      ******************************************************************
            SELECT CFSIBA  ASSIGN TO CFSIBA
                   ORGANIZATION   IS INDEXED
                   ACCESS MODE    IS DYNAMIC
                   RECORD KEY     IS CFSI-LLAVE
                   FILE STATUS    IS FS-CFSIBA
                                     FSE-CFSIBA.
            SELECT FTPREP ASSIGN  TO FTPREP
                   FILE STATUS    IS FS-FTPREP.

       DATA DIVISION.
       FILE SECTION.
      *1 -->MAESTRO DE INFORMACION GENERAL DE TH
       FD TLMATH.
          COPY TLMATH4.

      *2 -->MAESTRO DE INFORMACION GENERAL DE TARJETA INST
       FD TIMATH.
          COPY TIMATH.

      *3 -->COMPLEMENTO DEL MAESTRO DE TH
       FD TLMACO.
          COPY TLMACO2.

      *4 -->MAESTRO DE TABLAS CODIGO TARJETA EMPRESARIAL
       FD TLTGEN.
          COPY TLTGEN.

      *5 -->ARCHIVO INIDCE ALTERNO MANUAL DE TLENIV (PROG BIN COMPLEJO)
       FD TLENBL.
          COPY TLENBL.

      *6 -->APENDICE PARA E83 TARJETAS CRE. Y MONETARIOS
       FD CFSIBA.
          COPY CFSIBA.

      *7 -->REPORTE DE ERRORES EN LA EJECUCION
       FD FTPREP.
       01 REPORT-LINE                   PIC X(76).
      *8 -->BASE DE DATOS MASTER CARD
       FD MASTERDB.
       01 REG-MASTERDB.
          02 MCDB-LLAVE                 PIC 9(16).
          02 FILLER                     PIC X(01).
          02 MCDB-SEGMENTO              PIC 9(01).
          02 FILLER                     PIC X(01).
          02 MCDB-SUBSEGMENTO           PIC 9(02).
          02 FILLER                     PIC X(01).
          02 MCDB-DESTINO               PIC 9(08).
          02 FILLER                     PIC X(226).
      *9 -->BASE DE DATOS MASTER CARD
       FD VISARDB.
       01 REG-VISARDB.
          02 VSDB-LLAVE                 PIC 9(16).
          02 FILLER                     PIC X(01).
          02 VSDB-SEGMENTO              PIC 9(01).
          02 FILLER                     PIC X(01).
          02 VSDB-SUBSEGMENTO           PIC 9(02).
          02 FILLER                     PIC X(01).
          02 VSDB-DESTINO               PIC 9(08).
          02 FILLER                     PIC X(226).

       WORKING-STORAGE SECTION.
      ******************************************************************
      *               C A M P O S    D E    T R A B A J O              *
      ******************************************************************
       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-PROGRAMA               PIC X(08)         VALUE
                                                          "CUADRETC".
          02 WKS-TAB-LONG               PIC 9(03)         VALUE ZEROS.
      *   FLAGS FIN DE ARCHIVO Y ARCHIVO A PROCESAR
          02 WKS-FIN-ARCHIVOS           PIC 9(01).
             88 WKS-END-TLTGEN                            VALUE 1.
             88 WKS-END-TLMATH                            VALUE 2.
             88 WKS-END-TIMATH                            VALUE 3.
             88 WKS-END-MASTERDB                          VALUE 4.
             88 WKS-END-VISARDB                           VALUE 5.
          02 WKS-VALIDACIONES-OK        PIC 9(01)         VALUE ZEROS.
      *   BIN Y DIVISAS TEMPORALES
          02 WKS-BIN-TEMP               PIC 9(06)         VALUE ZEROS.
          02 WKS-DIVISAS-TEMP.
             03 WKS-DOLLAR              PIC 9(01)         VALUE ZEROS.
      ******************************************************************
      *        C O N T A D O R E S   E S T A D I S T I C A S           *
      ******************************************************************
          02 WKS-IGNORADOS-TLMT         PIC 9(10)         VALUE ZEROS.
          02 WKS-IGNORADOS-TIMT         PIC 9(10)         VALUE ZEROS.
          02 WKS-AGREGADO-CFSIBA        PIC 9(10)         VALUE ZEROS.
          02 WKS-DUPKEY-CFSIBA          PIC 9(10)         VALUE ZEROS.
          02 WKS-REG-TABLAS-NF          PIC 9(03)         VALUE ZEROS.
          02 WKS-COMPLEJO-NF            PIC 9(10)         VALUE ZEROS.
          02 WKS-TLMACO-NF              PIC 9(10)         VALUE ZEROS.
          02 WKS-CONTADOR-ERRORES       PIC 9(10)         VALUE ZEROS.
      *
          02 WKS-UPDATE-CFSIBA          PIC 9(10)         VALUE ZEROS.
          02 WKS-NOTUPT-CFSIBA          PIC 9(10)         VALUE ZEROS.
          02 WKS-LEIDOS-BBDD-MC         PIC 9(10)         VALUE ZEROS.
          02 WKS-LEIDOS-BBDD-VS         PIC 9(10)         VALUE ZEROS.
          02 WKS-MASK                   PIC Z,ZZZ,ZZZ,ZZ9.

      *   FECHA RECIBIDA DESDE EL SYSIN
          02 WKS-SYSIN-FECHA.
             04 WKS-DD                  PIC 9(02).
             04 FILLER                  PIC X(01).
             04 WKS-MM                  PIC 9(02).
             04 FILLER                  PIC X(01).
             04 WKS-AA                  PIC 9(04).
          02 WKS-FECHA-COMPARE          PIC 9(06).

      *   FECHA A VALIDAR DE ARCHIVOS FORMATO DD/MM/AAAA
      *   FECHA DE ARCHIVO ES DE 9 CHARS
          02 WKS-FECHA-FILE             PIC S9(09).
          02 WKS-FILE-MMAA              PIC 9(06).
      ******************************************************************
      *              TABLA  TLGEN   004  BINES DE TARJETA              *
      ******************************************************************
          02 WKS-TABLA-004-TLTGEN.
             03 WKS-DATOS-TABLA-004     OCCURS  0 TO  999
                                        DEPENDING ON  WKS-TAB-LONG
                                        ASCENDING KEY WKS-BIN-004
                                        INDEXED   BY  WKS-I.
                04 WKS-BIN-004          PIC 9(06).
                04 WKS-TIPO-BIN-004     PIC X(03).

      ******************************************************************
      *           ESTRUCTURA REPORTE DE ERRORES CON TARJETAS           *
      ******************************************************************
       01 WKS-REPORTE-ERRORES.
          02 WKS-CTA-TLMATH             PIC X(16).
          02 FILLER                     PIC X(01)         VALUE "|".
          02 WKS-CTA-COMPLEJA           PIC X(16).
          02 FILLER                     PIC X(01)         VALUE "|".
          02 WKS-FILE                   PIC 9(01).
          02 FILLER                     PIC X(01)         VALUE "|".
          02 WKS-MENSAJE-ERROR          PIC X(40).

      ******************************************************************
      *         C O N T A D O R E S   E S T A D I S T I C A S          *
      ******************************************************************
      *         VARIABLES PARA EVALUAR INTEGRIDAD DE ARCHIVOS          *
      ******************************************************************
       01 FS-TLMATH                     PIC 9(02)         VALUE ZEROS.
       01 FS-TIMATH                     PIC 9(02)         VALUE ZEROS.
       01 FS-TLMACO                     PIC 9(02)         VALUE ZEROS.
       01 FS-TLTGEN                     PIC 9(02)         VALUE ZEROS.
       01 FS-TLENBL                     PIC 9(02)         VALUE ZEROS.
       01 FS-CFSIBA                     PIC 9(02)         VALUE ZEROS.
       01 FS-FTPREP                     PIC 9(02)         VALUE ZEROS.
       01 FS-MCDB                       PIC 9(02)         VALUE ZEROS.
       01 FS-VSDB                       PIC 9(02)         VALUE ZEROS.
       01 FS-CICLO                      PIC 9(02)         VALUE ZEROS.
      *                VARIABLES DE FILE STATUS EXTENDED               *
       01 FSE-TLMATH.
          02 FSE-RETURN                 PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FUNCTION               PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FEEDBACK               PIC S9(04) COMP-5 VALUE ZEROS.
       01 FSE-TIMATH.
          02 FSE-RETURN                 PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FUNCTION               PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FEEDBACK               PIC S9(04) COMP-5 VALUE ZEROS.
       01 FSE-CFSIBA.
          02 FSE-RETURN                 PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FUNCTION               PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FEEDBACK               PIC S9(04) COMP-5 VALUE ZEROS.
       01 FSE-TLMACO.
          02 FSE-RETURN                 PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FUNCTION               PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FEEDBACK               PIC S9(04) COMP-5 VALUE ZEROS.
       01 FSE-TLTGEN.
          02 FSE-RETURN                 PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FUNCTION               PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FEEDBACK               PIC S9(04) COMP-5 VALUE ZEROS.
       01 FSE-TLENBL.
          02 FSE-RETURN                 PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FUNCTION               PIC S9(04) COMP-5 VALUE ZEROS.
          02 FSE-FEEDBACK               PIC S9(04) COMP-5 VALUE ZEROS.

      * Variables de Rutina para control de File Status Extendido
       01 PROGRAMA                      PIC X(08)         VALUE SPACES.
       01 ARCHIVO                       PIC X(08)         VALUE SPACES.
       01 ACCION                        PIC X(10)         VALUE SPACES.
       01 LLAVE                         PIC X(32)         VALUE SPACES.
      ******************************************************************
       PROCEDURE DIVISION.
       000-MAIN SECTION.
           PERFORM PROCESOS-FECHA
           PERFORM ABRIR-ARCHIVOS
           PERFORM CARDA-TABLA-TLT004
           PERFORM PROCESAR-TC-EMPRESARIAL   UNTIL WKS-END-TLMATH
           PERFORM PROCESAR-TC-INSTITUCIONAL UNTIL WKS-END-TIMATH
           PERFORM PROCESAR-MC-DATABASE      UNTIL WKS-END-MASTERDB
           PERFORM PROCESAR-VS-DATABASE      UNTIL WKS-END-VISARDB
           PERFORM ESTADISTICAS
           PERFORM CERRAR-ARCHIVOS
           STOP RUN.
       000-MAIN-E. EXIT.

      * ACEPTAMOS FECHA DESDE FECH03 EN FORMATO DD/MM/AAAA PARA
      * EXTRATER FECHA DE COMPARACION EN FORMATO MMAAAA
       PROCESOS-FECHA SECTION.
           ACCEPT WKS-SYSIN-FECHA FROM SYSIN
           MOVE WKS-MM TO WKS-FECHA-COMPARE(1:2)
           MOVE WKS-AA TO WKS-FECHA-COMPARE(3:4).
       PROCESOS-FECHA-E. EXIT.

      *APERTURA Y VALIDACION FSE DE ARCHIVOS
       ABRIR-ARCHIVOS SECTION.
           OPEN INPUT  TLMATH, TIMATH, TLMACO, TLTGEN, TLENBL, MASTERDB,
                       VISARDB
           OPEN OUTPUT FTPREP
           OPEN I-O    CFSIBA

           IF FS-FTPREP NOT = 0 OR FS-MCDB NOT = 0 OR FS-VSDB NOT = 0
              DISPLAY "***********************************************"
              DISPLAY "*      ERROR AL ABRIR ARCHIVOS PLANOS         *"
              DISPLAY "***********************************************"
              DISPLAY "* FILE STATUS DEL ARCHIVO FTPREP   : " FS-FTPREP
              DISPLAY "* FILE STATUS DEL ARCHIVO MASTERDB : " FS-MCDB
              DISPLAY "* FILE STATUS DEL ARCHIVO VISARDB  : " FS-VSDB
              DISPLAY "***********************************************"
              MOVE  91        TO RETURN-CODE
              PERFORM CERRAR-ARCHIVOS
              STOP RUN
           END-IF

           IF (FS-TLMATH = 97) AND (FS-TIMATH = 97) AND
              (FS-CFSIBA = 97) AND (FS-TLMACO = 97) AND
              (FS-TLTGEN = 97) AND (FS-TLENBL = 97) AND
              (FS-FTPREP = 97)
                 MOVE ZEROS TO FS-TLMATH
                 MOVE ZEROS TO FS-TIMATH
                 MOVE ZEROS TO FS-CFSIBA
                 MOVE ZEROS TO FS-TLMACO
                 MOVE ZEROS TO FS-TLTGEN
                 MOVE ZEROS TO FS-TLENBL
                 MOVE ZEROS TO FS-FTPREP
           END-IF

           MOVE ZEROS    TO FS-CICLO
           MOVE 'OPEN'   TO ACCION
           MOVE SPACES   TO LLAVE

           PERFORM VARYING FS-CICLO FROM 1 BY 1 UNTIL FS-CICLO > 6
               PERFORM FILE-STATUS-EXTENDED
           END-PERFORM

           MOVE   1   TO WKS-FILE
           MOVE ZEROS TO FS-CICLO WKS-FIN-ARCHIVOS
                         WKS-TAB-LONG WKS-BIN-TEMP WKS-DIVISAS-TEMP.
       ABRIR-ARCHIVOS-E. EXIT.

       CARDA-TABLA-TLT004 SECTION.
           MOVE 'TLT'     TO TLTG-CODIGO
           MOVE  004      TO TLTG-CORRELATIVO-TABLA
           MOVE ZEROS     TO TLTG-CORRELATIVO-REGISTRO

           START TLTGEN
             KEY GREATER TLTG-LLAVE
           END-START

           IF FS-TLTGEN NOT = 0
              MOVE 4          TO FS-CICLO
              MOVE 'READ'     TO ACCION
              MOVE TLTG-LLAVE TO LLAVE
              MOVE 91         TO RETURN-CODE
              PERFORM FILE-STATUS-EXTENDED
              PERFORM CERRAR-ARCHIVOS
              STOP RUN
           END-IF

           PERFORM VARYING WKS-I FROM 1 BY 1 UNTIL
                   TLTG-CORRELATIVO-TABLA > 004 OR WKS-END-TLTGEN

             READ TLTGEN
                  NEXT RECORD
             END-READ

             EVALUATE  FS-TLTGEN
                WHEN 0
                  IF TLTG-CORRELATIVO-TABLA = 004
                     ADD        1                        TO WKS-TAB-LONG
                     MOVE TLTG-CORRELATIVO-REGISTRO(5:6) TO
                          WKS-BIN-004     (WKS-I)
                     MOVE TLTG-DESCRIPCION(4:3)          TO
                          WKS-TIPO-BIN-004(WKS-I)
                  END-IF
                WHEN 10
                  MOVE 1          TO WKS-FIN-ARCHIVOS
                WHEN OTHER
                  MOVE 4          TO FS-CICLO
                  MOVE 'READ'     TO ACCION
                  MOVE TLTG-LLAVE TO LLAVE
                  MOVE 91         TO RETURN-CODE
                  PERFORM FILE-STATUS-EXTENDED
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
             END-EVALUATE
           END-PERFORM.
       CARDA-TABLA-TLT004-E. EXIT.

       PROCESAR-TC-EMPRESARIAL SECTION.
           INITIALIZE  REG-CFSIBA,        WKS-FECHA-FILE,
                       WKS-CTA-TLMATH,    WKS-CTA-COMPLEJA,
                       WKS-MENSAJE-ERROR, WKS-VALIDACIONES-OK

           READ TLMATH END-READ

           EVALUATE FS-TLMATH
              WHEN 0
                   MOVE  57                        TO CFSI-PRODUCTO
                   MOVE TLMT-FECHA-ULT-CAMB-SITUAC TO WKS-FECHA-FILE
                   MOVE WKS-FECHA-FILE(4:6)        TO WKS-FILE-MMAA
                   MOVE TLMT-LLAVE                 TO WKS-CTA-TLMATH
                                                      TLMO-LLAVE
                   IF TLMT-CODIGO-CLIENTE(8:1) = 1
                      MOVE "Y200"                  TO CFSI-DESTINO
                   END-IF
                   EVALUATE TRUE
                      WHEN TLMT-SITUACION-CUENTA = 3 OR 15

                           IF WKS-FILE-MMAA = WKS-FECHA-COMPARE
                              PERFORM VALIDACIONES-DE-TC-EMP
                           ELSE
                              ADD 1 TO WKS-IGNORADOS-TLMT
                           END-IF

                      WHEN (TLMT-SITUACION-CUENTA = 4 OR 7) AND
                           (TLMT-CUENTA-NUEVA > 0)

                           IF WKS-FILE-MMAA = WKS-FECHA-COMPARE
                              PERFORM VALIDACIONES-DE-TC-EMP
                           ELSE
                              ADD 1 TO WKS-IGNORADOS-TLMT
                           END-IF
                      WHEN (TLMT-SITUACION-CUENTA = 4 OR 7) AND
                           (TLMT-CUENTA-NUEVA <= 0)

                           ADD 1 TO WKS-IGNORADOS-TLMT
                      WHEN OTHER

                           PERFORM VALIDACIONES-DE-TC-EMP

                   END-EVALUATE
              WHEN 10
                  MOVE 2           TO WKS-FIN-ARCHIVOS WKS-FILE
              WHEN OTHER
                  MOVE 1           TO FS-CICLO
                  MOVE 'READ'      TO ACCION
                  MOVE TLMT-CUENTA TO LLAVE
                  MOVE 91          TO RETURN-CODE
                  PERFORM FILE-STATUS-EXTENDED
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
           END-EVALUATE.
       PROCESAR-TC-EMPRESARIAL-E. EXIT.

       PROCESAR-TC-INSTITUCIONAL SECTION.
           INITIALIZE  REG-CFSIBA,        WKS-FECHA-FILE,
                       WKS-CTA-TLMATH,    WKS-CTA-COMPLEJA,
                       WKS-MENSAJE-ERROR, WKS-VALIDACIONES-OK

           READ TIMATH END-READ

           EVALUATE FS-TIMATH
              WHEN 0
                   MOVE  45                        TO CFSI-PRODUCTO
                   MOVE TIMT-FECHA-ULT-CAMB-SITUAC TO WKS-FECHA-FILE
                   MOVE TIMT-LLAVE                 TO WKS-CTA-TLMATH
                                                      TLMO-LLAVE
                                                      CFSI-CUENTA
                   IF TIMT-CODIGO-CLIENTE(8:1) = 1
                      MOVE "Y200"                  TO CFSI-DESTINO
                   END-IF
                   EVALUATE TRUE
                      WHEN TIMT-SITUACION-CUENTA = 3 OR 15

                           IF WKS-FILE-MMAA = WKS-FECHA-COMPARE
                              PERFORM VALIDACIONES-DE-TC-INS
                           ELSE
                              ADD 1 TO WKS-IGNORADOS-TIMT
                           END-IF

                      WHEN (TIMT-SITUACION-CUENTA = 4 OR 7) AND
                           (TIMT-CUENTA-NUEVA > 0)

                           IF WKS-FILE-MMAA = WKS-FECHA-COMPARE
                              PERFORM VALIDACIONES-DE-TC-INS
                           ELSE
                              ADD 1 TO WKS-IGNORADOS-TIMT
                           END-IF
                      WHEN (TIMT-SITUACION-CUENTA = 4 OR 7) AND
                           (TIMT-CUENTA-NUEVA <= 0)

                           ADD 1 TO WKS-IGNORADOS-TIMT

                      WHEN OTHER

                           PERFORM VALIDACIONES-DE-TC-INS

                   END-EVALUATE
              WHEN 10
                   MOVE 3 TO WKS-FIN-ARCHIVOS

              WHEN OTHER
                  MOVE 2           TO FS-CICLO
                  MOVE 'READ'      TO ACCION
                  MOVE TLMT-CUENTA TO LLAVE
                  MOVE 91          TO RETURN-CODE
                  PERFORM FILE-STATUS-EXTENDED
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
           END-EVALUATE.
       PROCESAR-TC-INSTITUCIONAL-E. EXIT.

       VALIDACIONES-DE-TC-EMP SECTION.
      * VALIDACIONES A REALIZAR
      * 1. TIPO DE BIN: INTERNACIONAL O LOCAL
      * 2. PROCEDIMIENTO BIN COMPLEJO
      * 3. DATOS DE TLMACO
           PERFORM 001-PROCEDIMIENTO-BINES

           IF WKS-VALIDACIONES-OK = 0
              PERFORM 002-RECUPERAR-CTA-COMPLEJA
           END-IF

           IF WKS-VALIDACIONES-OK = 0
              PERFORM 003-LEE-DATOS-TLMACO
           END-IF

           IF WKS-VALIDACIONES-OK = 0
              PERFORM 004-ESCRIBE-CFSIBA
           END-IF.
       VALIDACIONES-DE-TC-EMP-E. EXIT.

       VALIDACIONES-DE-TC-INS SECTION.
           MOVE 1            TO CFSI-ACT-GENERADOR-DIVISAS-Q
           MOVE 00           TO CFSI-ACT-GENERADOR-DIVISAS-D
           PERFORM 003-LEE-DATOS-TLMACO
           IF WKS-VALIDACIONES-OK = 0
              PERFORM 004-ESCRIBE-CFSIBA
           END-IF.
       VALIDACIONES-DE-TC-INS-E. EXIT.

       001-PROCEDIMIENTO-BINES SECTION.
      *    SE GUARDA BIN LEIDO EN TEMP PARA EVITAR SEARCH INECESARIOS
           IF TLMT-BIN NOT = WKS-BIN-TEMP
              SET WKS-I         TO 1
              SEARCH ALL WKS-DATOS-TABLA-004
                AT END
                     MOVE 1     TO WKS-VALIDACIONES-OK
                     ADD  1     TO WKS-REG-TABLAS-NF
                     MOVE WKS-BIN-TEMP
                                TO WKS-CTA-COMPLEJA
                     MOVE "BIN NO ENCONTRADO EN TABLA TLGEN 004"
                                TO WKS-MENSAJE-ERROR
                     PERFORM DOCUMENTACION-ERRORES
                WHEN WKS-BIN-004(WKS-I) = TLMT-BIN
                     IF WKS-TIPO-BIN-004(WKS-I) = "INT"
                        MOVE 1  TO CFSI-ACT-GENERADOR-DIVISAS-Q
                        MOVE 3  TO CFSI-ACT-GENERADOR-DIVISAS-D
                     ELSE
                        MOVE 1  TO CFSI-ACT-GENERADOR-DIVISAS-Q
                        MOVE 00 TO CFSI-ACT-GENERADOR-DIVISAS-D
                     END-IF
                     MOVE 0                   TO WKS-VALIDACIONES-OK
                     MOVE WKS-BIN-004(WKS-I)  TO WKS-BIN-TEMP
                     MOVE CFSI-ACT-GENERADOR-DIVISAS-D
                                              TO WKS-DOLLAR

              END-SEARCH

           ELSE
              MOVE 1           TO CFSI-ACT-GENERADOR-DIVISAS-Q
              MOVE WKS-DOLLAR  TO CFSI-ACT-GENERADOR-DIVISAS-D
              MOVE 0           TO WKS-VALIDACIONES-OK
           END-IF.
       001-PROCEDIMIENTO-BINES-E. EXIT.

       002-RECUPERAR-CTA-COMPLEJA SECTION.
      * 2. VALIDAMOS SI EL BIN ERA COMPLEJO PARA RECUPERAR LLAVE
           IF TLMT-BIN(1:1) = 6
              MOVE TLMT-CUENTA TO TLBL-LLAVE
              READ TLENBL
                  KEY IS TLBL-LLAVE
              END-READ
              EVALUATE FS-TLENBL
                   WHEN 0
                        MOVE TLBL-CUENTA-COMPLEJA TO CFSI-CUENTA
                                                     WKS-CTA-COMPLEJA
                        MOVE 0                    TO WKS-VALIDACIONES-OK
                   WHEN 23
      *                 DOCUMENTAR ERROR
                        ADD  1          TO WKS-COMPLEJO-NF
                        MOVE 2          TO WKS-VALIDACIONES-OK
                        MOVE CFSI-CUENTA
                                        TO WKS-CTA-COMPLEJA
                        MOVE "CTA COMPLEJA NO ENCONTRADA EN TLENBL"
                                        TO WKS-MENSAJE-ERROR
                        PERFORM DOCUMENTACION-ERRORES
                   WHEN OTHER
                        MOVE 5          TO FS-CICLO
                        MOVE 'READ'     TO ACCION
                        MOVE TLTG-LLAVE TO LLAVE
                        MOVE 91         TO RETURN-CODE
                        PERFORM FILE-STATUS-EXTENDED
                        PERFORM CERRAR-ARCHIVOS
                        STOP RUN
              END-EVALUATE
           ELSE
              MOVE TLMT-CUENTA          TO CFSI-CUENTA
              MOVE 0                    TO WKS-VALIDACIONES-OK
           END-IF.
       002-RECUPERAR-CTA-COMPLEJA-E. EXIT.

       003-LEE-DATOS-TLMACO SECTION.
      *                 EQUIVALENCIAS TLMACO A CFSIBA
      *       ------------------------------------------------
      *     1. EMPRESARIAL MAYOR    ==   1. EMPRESARIALES
      *     2. EMPRESARIAL MENOR    ==   1. EMPRESARIALES
      *     3. CONSUMO              ==   4. CONSUMO
      *     4. MICROCREDITO         ==   2. PRODUCTIVOS
      *     5. HIPOTECARIO VIVIENDA ==   3. HIPOTECARIO PARA VIVIENDA

           READ TLMACO
              KEY IS TLMO-LLAVE
           END-READ

           EVALUATE FS-TLMACO
              WHEN 0
                   MOVE TLMO-CODIGO-ORIGEN-CREDITO TO
                        CFSI-CODIGO-ORIGEN-CREDITO
                   MOVE ZEROS                      TO
                        WKS-VALIDACIONES-OK
                   EVALUATE TLMO-CLASE-JM141
                      WHEN 1
                           MOVE 1   TO CFSI-SEGMENTO-DEUDOR
                      WHEN 2
                           MOVE 1   TO CFSI-SEGMENTO-DEUDOR
                      WHEN 3
                           MOVE 4   TO CFSI-SEGMENTO-DEUDOR
                           MOVE 41  TO CFSI-SUBSEGMENTO-DEUDOR
                      WHEN 4
                           MOVE 2   TO CFSI-SEGMENTO-DEUDOR
                      WHEN OTHER
                           MOVE 3   TO CFSI-SEGMENTO-DEUDOR
                   END-EVALUATE
              WHEN 23
                   ADD  1           TO WKS-TLMACO-NF
                   MOVE 3           TO WKS-VALIDACIONES-OK
                   MOVE CFSI-CUENTA TO WKS-CTA-COMPLEJA
                   MOVE "DATOS DE CTA NO ENCONTRADOS EN TLMACO"
                                    TO WKS-MENSAJE-ERROR
                   PERFORM DOCUMENTACION-ERRORES
              WHEN OTHER
                  MOVE 3            TO FS-CICLO
                  MOVE 'READ'       TO ACCION
                  MOVE TLMT-CUENTA  TO LLAVE
                  MOVE 91           TO RETURN-CODE
                  PERFORM FILE-STATUS-EXTENDED
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
           END-EVALUATE.
       003-LEE-DATOS-TLMACO-E. EXIT.

       004-ESCRIBE-CFSIBA SECTION.
           WRITE
              REG-CFSIBA
           END-WRITE

           EVALUATE FS-CFSIBA
              WHEN 0
                  ADD 1            TO WKS-AGREGADO-CFSIBA
              WHEN 22
                  ADD 1            TO WKS-DUPKEY-CFSIBA
                  MOVE CFSI-CUENTA TO WKS-CTA-COMPLEJA
                  MOVE "REGISTRO YA EXISTENTE EN CFSIBA, DUPKEY"
                                   TO WKS-MENSAJE-ERROR
                  PERFORM DOCUMENTACION-ERRORES

              WHEN OTHER
                  MOVE 6          TO FS-CICLO
                  MOVE 'WRITE'    TO ACCION
                  MOVE CFSI-LLAVE TO LLAVE
                  MOVE 91         TO RETURN-CODE
                  PERFORM FILE-STATUS-EXTENDED
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
           END-EVALUATE.
       004-ESCRIBE-CFSIBA-E. EXIT.

       PROCESAR-MC-DATABASE SECTION.
           INITIALIZE REG-CFSIBA, WKS-CTA-TLMATH,   WKS-CTA-COMPLEJA
                      WKS-FILE,   WKS-MENSAJE-ERROR
           READ MASTERDB END-READ

           EVALUATE FS-MCDB
              WHEN 0
                  MOVE  57              TO CFSI-PRODUCTO
                  MOVE MCDB-LLAVE       TO CFSI-CUENTA
                  MOVE MCDB-SEGMENTO    TO CFSI-SEGMENTO-DEUDOR
                  MOVE MCDB-SUBSEGMENTO TO CFSI-SUBSEGMENTO-DEUDOR
                  MOVE MCDB-DESTINO     TO CFSI-DESTINO
                  PERFORM ACTUALIZA_CFSIBA
              WHEN 10
                  MOVE  4         TO WKS-FIN-ARCHIVOS
              WHEN OTHER
                  DISPLAY "*******************************************"
                  DISPLAY "*   ERROR AL LEER BASE DE DATOS MC        *"
                  DISPLAY "*******************************************"
                  DISPLAY "* FILE STATUS DEL ARCHIVO : " FS-MCDB
                  DISPLAY "*******************************************"
                  MOVE  91        TO RETURN-CODE
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
           END-EVALUATE.
       PROCESAR-MC-DATABASE-E.

       PROCESAR-VS-DATABASE SECTION.
           INITIALIZE REG-CFSIBA, WKS-CTA-TLMATH,   WKS-CTA-COMPLEJA
                      WKS-FILE,   WKS-MENSAJE-ERROR
           READ VISARDB END-READ

           EVALUATE FS-VSDB
              WHEN 0
                  IF CFSI-BIN = 050837
                     MOVE  45           TO CFSI-PRODUCTO
                  ELSE
                     MOVE  57           TO CFSI-PRODUCTO
                  END-IF
                  MOVE VSDB-LLAVE       TO CFSI-CUENTA
                  MOVE VSDB-SEGMENTO    TO CFSI-SEGMENTO-DEUDOR
                  MOVE VSDB-SUBSEGMENTO TO CFSI-SUBSEGMENTO-DEUDOR
                  MOVE VSDB-DESTINO     TO CFSI-DESTINO
                  PERFORM ACTUALIZA_CFSIBA
              WHEN 10
                  MOVE  4         TO WKS-FIN-ARCHIVOS
              WHEN OTHER
                  DISPLAY "*******************************************"
                  DISPLAY "*   ERROR AL LEER BASE DE DATOS VS        *"
                  DISPLAY "*******************************************"
                  DISPLAY "* FILE STATUS DEL ARCHIVO : " FS-VSDB
                  DISPLAY "*******************************************"
                  MOVE  91        TO RETURN-CODE
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
           END-EVALUATE.
       PROCESAR-VS-DATABASE-E. EXIT.

       ACTUALIZA_CFSIBA SECTION.
           READ CFSIBA
               KEY IS CFSI-LLAVE
           END-READ
           EVALUATE FS-CFSIBA
              WHEN 0
                  REWRITE
                       REG-CFSIBA
                  END-REWRITE
                  IF FS-CFSIBA = 0
                      ADD 1            TO WKS-UPDATE-CFSIBA
                  ELSE
                      ADD 1            TO WKS-NOTUPT-CFSIBA
                      MOVE CFSI-CUENTA TO WKS-CTA-TLMATH
                      MOVE FS-CFSIBA   TO WKS-CTA-COMPLEJA
                      MOVE 3           TO WKS-FILE
                      MOVE
                      "NO SE PUDO ACTUALIZAR CFSIBA PARAMETROS BBDD"
                                       TO WKS-MENSAJE-ERROR
                      PERFORM DOCUMENTACION-ERRORES
                  END-IF
              WHEN 23
                  MOVE CFSI-CUENTA TO WKS-CTA-TLMATH
                  MOVE 3           TO WKS-FILE
                  MOVE "CUENTA DE BBDD NO ENCONTRADA EN CFSIBA"
                                   TO WKS-MENSAJE-ERROR
                  PERFORM DOCUMENTACION-ERRORES

              WHEN OTHER
                  MOVE 6          TO FS-CICLO
                  MOVE 'REWRITE'  TO ACCION
                  MOVE CFSI-LLAVE TO LLAVE
                  MOVE 91         TO RETURN-CODE
                  PERFORM FILE-STATUS-EXTENDED
                  PERFORM CERRAR-ARCHIVOS
                  STOP RUN
           END-EVALUATE.
       ACTUALIZA_CFSIBA-E. EXIT.


       FILE-STATUS-EXTENDED SECTION.
           EVALUATE FS-CICLO
              WHEN 1
                  IF FS-TLMATH NOT EQUAL 0
                     MOVE 'TLMATH'   TO ARCHIVO
                     CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-TLMATH, FSE-TLMATH
                     MOVE  91        TO RETURN-CODE
                     PERFORM CERRAR-ARCHIVOS
                     STOP RUN
                  END-IF
              WHEN 2
                  IF FS-TIMATH NOT EQUAL 0
                     MOVE 'TIMATH'   TO ARCHIVO
                     CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-TIMATH, FSE-TIMATH
                     MOVE  91        TO RETURN-CODE
                     PERFORM CERRAR-ARCHIVOS
                     STOP RUN
                  END-IF
              WHEN 3
                  IF FS-TLMACO NOT EQUAL 0
                     MOVE 'TLMACO'   TO ARCHIVO
                     CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-TLMACO, FSE-TLMACO
                     MOVE  91        TO RETURN-CODE
                     PERFORM CERRAR-ARCHIVOS
                     STOP RUN
                  END-IF
              WHEN 4
                  IF FS-TLTGEN NOT EQUAL 0
                     MOVE 'TLTGEN'   TO ARCHIVO
                     CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-TLTGEN, FSE-TLTGEN
                     MOVE  91        TO RETURN-CODE
                     PERFORM CERRAR-ARCHIVOS
                     STOP RUN
                  END-IF
              WHEN 5
                  IF FS-TLENBL NOT EQUAL 0
                     MOVE 'TLENBL'   TO ARCHIVO
                     CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-TLENBL, FSE-TLENBL
                     MOVE  91        TO RETURN-CODE
                     PERFORM CERRAR-ARCHIVOS
                     STOP RUN
                  END-IF
              WHEN OTHER
                  IF FS-CFSIBA NOT EQUAL 0
                     MOVE 'CFSIBA'   TO ARCHIVO
                     CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-CFSIBA, FSE-CFSIBA
                     MOVE  91        TO RETURN-CODE
                     PERFORM CERRAR-ARCHIVOS
                     STOP RUN
                  END-IF
           END-EVALUATE.
       FILE-STATUS-EXTENDED-E. EXIT.

       DOCUMENTACION-ERRORES SECTION.
           WRITE
              REPORT-LINE FROM WKS-REPORTE-ERRORES
           END-WRITE
           IF FS-FTPREP = 0
              ADD 1 TO WKS-CONTADOR-ERRORES
           ELSE
              DISPLAY "ERROR AL GRABAR BITACORA : " TLMT-LLAVE
              DISPLAY "FS ARCHIVO FTPREP        : " FS-FTPREP
              MOVE  91  TO RETURN-CODE
              PERFORM CERRAR-ARCHIVOS
              STOP RUN
           END-IF.
       DOCUMENTACION-ERRORES-E. EXIT.


       ESTADISTICAS SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                  E S T A D I S T I C A S               *"
           DISPLAY
           "**********************************************************"

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-AGREGADO-CFSIBA TO  WKS-MASK
           DISPLAY
           "TOTAL REGISTROS ESCRITOS EN CFSIBA       :" WKS-MASK
           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-UPDATE-CFSIBA   TO  WKS-MASK
           DISPLAY
           "TOTAL REGISTROS ACTUALIZADOS POR BBDD    :" WKS-MASK

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-LEIDOS-BBDD-MC  TO  WKS-MASK
           DISPLAY
           "TOTAL REGISTROS LEIDOS BBDD MASTER       :" WKS-MASK
           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-LEIDOS-BBDD-VS  TO  WKS-MASK
           DISPLAY
           "TOTAL REGISTROS LEIDOS BBDD VISA         :" WKS-MASK

           DISPLAY
           "**********************************************************"
           DISPLAY
           "*           I N F O R M E   D E    E R R O R E S         *"
           DISPLAY
           "**********************************************************"
           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-IGNORADOS-TLMT  TO WKS-MASK
           DISPLAY
           "TOTAL TC EMP IGNORADOS SIT 3, 15 , 4 Y 7 : " WKS-MASK

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-IGNORADOS-TIMT  TO WKS-MASK
           DISPLAY
           "TOTAL TC INS IGNORADOS SIT 3, 15 , 4 Y 7 : " WKS-MASK
           MOVE ZEROS                TO WKS-MASK
           MOVE WKS-CONTADOR-ERRORES TO WKS-MASK
           DISPLAY
           "> TOTAL ERRORES DOCUMENTADOS REPORTE     : " WKS-MASK

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-DUPKEY-CFSIBA   TO WKS-MASK
           DISPLAY
           "TOTAL DUPLICADOS EN CFSIBA NO ESCRITOS   : " WKS-MASK

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-COMPLEJO-NF     TO WKS-MASK
           DISPLAY
           "TOTAL BIN COMPLEJO NO ENCONTRADOS TLENBL : " WKS-MASK

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-TLMACO-NF       TO WKS-MASK
           DISPLAY
           "TOTAL CTAS NO ENCONTRADAS EN TLMACO      : " WKS-MASK

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-REG-TABLAS-NF   TO WKS-MASK
           DISPLAY
           "TOTAL BINES NO ENCONTRADOS TABLA TLT004  : " WKS-MASK

           MOVE ZEROS               TO WKS-MASK
           MOVE WKS-NOTUPT-CFSIBA   TO WKS-MASK
           DISPLAY
           "REGISTROS NO ACTUALIZADOS EN CFSIBA BBDD : " WKS-MASK

           DISPLAY
           "**********************************************************".
       ESTADISTICAS-E. EXIT.

       CERRAR-ARCHIVOS SECTION.
           CLOSE TLMATH, TIMATH, TLMACO, TLTGEN, TLENBL, CFSIBA, FTPREP.
       CERRAR-ARCHIVOS-E. EXIT.
