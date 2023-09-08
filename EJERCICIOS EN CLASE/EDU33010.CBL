      ******************************************************************
      * FECHA       : 10/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33010                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : PROGRAMA QUE LEE TRES ARCHIVOS SIMULTANEOS PARA  *
      *             : VERIFICAR LA INTEGRIDAD DE LOS REGISTROS EN CADA *
      *             : ARCHIVO, LUEGO ENVIA AL SPOOL EL DETALLE DE REGI-*
      *             : TROS PROCESADOS EN TOTAL, LOS QUE TUVIERON ERROR *
      *             : Y LOS QUE FUERON ENCONTRADOS INTEGROS            *
      * ARCHIVOS    : EDUC.EEDR.MAESTRO.MONE                           *
      *             : EDUC.EEDR.MATRIZ.SALDOS                          *
      *             : EDUC.EEDR.SALDOS.LINEA                           *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU33010.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT MOMAES  ASSIGN TO MOMAES
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS SEQUENTIAL
                   RECORD KEY    IS MOMA-LLAVE
                   FILE STATUS   IS FS-MOMAES
                                    FSE-MOMAES.
            SELECT MOMATS  ASSIGN TO MOMATS
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS SEQUENTIAL
                   RECORD KEY    IS MOMS-LLAVE
                   FILE STATUS   IS FS-MOMATS
                                    FSE-MOMATS.
            SELECT MOSLIN  ASSIGN TO MOSLIN
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS SEQUENTIAL
                   RECORD KEY    IS MOSL-LLAVE
                   FILE STATUS   IS FS-MOSLIN
                                    FSE-MOSLIN.
       DATA DIVISION.
       FILE SECTION.
       FD  MOMAES.
           COPY MOMAES.
       FD  MOMATS.
           COPY MOMATS.
       FD  MOSLIN.
           COPY MOSLIN.
       WORKING-STORAGE SECTION.
      *------->          VARIABLES DE FILE STATUS NORMAL
       01  FS-MOMAES                   PIC 9(02) VALUE ZEROS.
       01  FS-MOMATS                   PIC 9(02) VALUE ZEROS.
       01  FS-MOSLIN                   PIC 9(02) VALUE ZEROS.
      *------->         VARIABLES DE FILE STATUS EXTENDED
       01  FSE-MOMAES.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
       01  FSE-MOMATS.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
       01  FSE-MOSLIN.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
      *Variables de Rutina para control de File Status Extendido
       01  PROGRAMA                    PIC X(08) VALUE SPACES.
       01  ARCHIVO                     PIC X(08) VALUE SPACES.
       01  ACCION                      PIC X(10) VALUE SPACES.
       01  LLAVE                       PIC X(32) VALUE SPACES.
      *CONTROLADOR DE LECTURAS
       01 WKS-FIN-ARCHIVOS             PIC X(01).
          88 WKS-FIN-MOMAES                      VALUE HIGH-VALUES.
          88 WKS-FIN-MOMATS                      VALUE HIGH-VALUES.
          88 WKS-FIN-MOSLIN                      VALUE HIGH-VALUES.

      *CONTADOR DE REGISTROS
       01 WKS-REGISTROS-TOTAL          PIC 9(03) VALUE ZEROS.
       01 WKS-REGISTROS-INTEGROS       PIC 9(03) VALUE ZEROS.
       01 WKS-REGISTROS-NO-INTGR       PIC 9(03) VALUE ZEROS.
       01 WKS-REGISTROS-AJUSTADOS      PIC 9(03) VALUE ZEROS.
       01 WKS-REGISTROS-NO-AJ          PIC 9(03) VALUE ZEROS.
       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 200-APERTURA-DE-ARCHIVOS
           PERFORM 300-LECTURA-VERIFICA-INTGR
      *--->        400 SERIE ENCARGADAS DE HACER DISPLAY DE LOS ERRORES
      *--->        500 SERIE ENCARGADAS DE HACER DISPLAY DE INTEGRIDAD
           PERFORM 600-ESTADISTICAS
           PERFORM 700-CIERRA-ARCHIVOS
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

      *--->> SERIE 200 EVALUA LA APERTURA DE ARCHIVOS Y CONFLICTOS
      *      ASOCIADOS A LAS APERTURAS
       200-APERTURA-DE-ARCHIVOS SECTION.
           MOVE 'EDU33011' TO PROGRAMA
           OPEN INPUT MOMAES
           OPEN INPUT MOMATS
           OPEN INPUT MOSLIN
           PERFORM 210-EVALUA-FS-INTEGRIDAD.
       200-APERTURA-DE-ARCHIVOS-E.            EXIT.

       210-EVALUA-FS-INTEGRIDAD SECTION.
           IF (FS-MOMAES = 97) AND (FS-MOMATS = 97) AND (FS-MOSLIN = 97)
              MOVE ZEROS TO FS-MOMAES
              MOVE ZEROS TO FS-MOMATS
              MOVE ZEROS TO FS-MOSLIN
           END-IF
           IF FS-MOMAES NOT EQUAL 0
              MOVE 'OPEN' TO ACCION
              MOVE SPACES TO LLAVE
              MOVE 'MOMAES' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                 LLAVE, FS-MOMAES, FSE-MOMAES
              MOVE 91 TO RETURN-CODE
              PERFORM 410-ERRORES-APERTURA
              PERFORM 700-CIERRA-ARCHIVOS
              STOP RUN
           END-IF
           IF FS-MOMATS NOT EQUAL 0
              MOVE 'OPEN' TO ACCION
              MOVE SPACES TO LLAVE
              MOVE 'MOMATS' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                 LLAVE, FS-MOMAES, FSE-MOMAES
              MOVE 91 TO RETURN-CODE
              PERFORM 410-ERRORES-APERTURA
              PERFORM 700-CIERRA-ARCHIVOS
              STOP RUN
           END-IF

           IF FS-MOSLIN NOT EQUAL 0
              MOVE 'OPEN' TO ACCION
              MOVE SPACES TO LLAVE
              MOVE 'MOSLIN' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                 LLAVE, FS-MOMAES, FSE-MOMAES
              MOVE 91 TO RETURN-CODE
              PERFORM 410-ERRORES-APERTURA
              PERFORM 700-CIERRA-ARCHIVOS
              STOP RUN
           END-IF.
       210-EVALUA-FS-INTEGRIDAD-E.            EXIT.

      *---->SERIE 300 ENCARGADA DE LA LECTURA Y VALIDACIÓN DE INTEGRIDAD
       300-LECTURA-VERIFICA-INTGR SECTION.
           PERFORM 2 TIMES
                   READ MOMATS
           END-PERFORM.
           PERFORM 320-LECTURAS-PAREJAS
           PERFORM UNTIL WKS-FIN-MOMAES AND WKS-FIN-MOMATS
              AND WKS-FIN-MOSLIN
                   PERFORM 310-VERIFICAR-INTEGRIDAD
                   PERFORM 320-LECTURAS-PAREJAS
                   ADD 1 TO WKS-REGISTROS-TOTAL
           END-PERFORM.
       300-LECTURA-VERIFICA-INTGR-E.          EXIT.

       310-VERIFICAR-INTEGRIDAD SECTION.
           IF MOMA-LLAVE =(MOMS-LLAVE AND MOSL-LLAVE)
              ADD 1 TO WKS-REGISTROS-INTEGROS
              PERFORM 500-INTEGRIDAD-DISPLAY
           ELSE
              ADD 1 TO WKS-REGISTROS-NO-INTGR
              PERFORM 310-CASOS-NO-INTEGRIDAD
           END-IF.
       310-VERIFICAR-INTEGRIDAD-E.            EXIT.

       310-CASOS-NO-INTEGRIDAD SECTION.
           EVALUATE TRUE
      *             SI MOMAES - SI MOMATS - NO MOSLIN
           WHEN (MOMS-LLAVE > MOSL-LLAVE) AND
                (MOMA-LLAVE > MOSL-LLAVE)

                PERFORM 400-ERROR-INTEGRIDAD-DATOS

                PERFORM UNTIL MOSL-LLAVE =
                   (MOMS-LLAVE AND MOMA-LLAVE) OR WKS-FIN-MOSLIN
                        READ MOSLIN
                        AT END
                           SET WKS-FIN-MOSLIN TO TRUE
                        END-READ
                END-PERFORM
                IF MOMA-LLAVE =(MOMS-LLAVE AND MOSL-LLAVE)
                   ADD 1 TO WKS-REGISTROS-AJUSTADOS
                   DISPLAY
                   "            SE HIZO UN AJUSTE EN MOSLIN"
                   PERFORM 500-INTEGRIDAD-DISPLAY
                ELSE
                   ADD 1 TO WKS-REGISTROS-NO-AJ
                   DISPLAY
                   "    SE TRATO DE UBICAR LA LLAVE Y NO SE ENCONTRO"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMAES"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMATS"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOSLIN !!!"
                   PERFORM 400-ERROR-INTEGRIDAD-DATOS
                END-IF
      *             SI MOMAES - NO MOMATS - SI MOSLIN
           WHEN (MOMA-LLAVE = MOSL-LLAVE) AND
                (MOMA-LLAVE > MOMS-LLAVE)

                PERFORM 400-ERROR-INTEGRIDAD-DATOS

                PERFORM UNTIL MOMS-LLAVE =
                   (MOSL-LLAVE AND MOMA-LLAVE) OR WKS-FIN-MOMATS
                        READ MOMATS
                        AT END
                           SET WKS-FIN-MOMATS TO TRUE
                        END-READ
                END-PERFORM

                IF MOMA-LLAVE =(MOMS-LLAVE AND MOSL-LLAVE)
                   ADD 1 TO WKS-REGISTROS-AJUSTADOS
                   DISPLAY
                   "       SE HIZO UN AJUSTE DE LECTURA EN MOMATS"
                   DISPLAY
                   " SE LOGRO UBICAR LAS LLAVES EN REGISTROS SIGUIENTES"
                   PERFORM 500-INTEGRIDAD-DISPLAY
                ELSE
                   ADD 1 TO WKS-REGISTROS-NO-AJ
                   DISPLAY
                   "    SE TRATO DE UBICAR LA LLAVE Y NO SE ENCONTRO"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMAES"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOSLIN"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOMATS !!!"
                   PERFORM 400-ERROR-INTEGRIDAD-DATOS
                END-IF

      *             SI MOSLIN - SI MOMATS - NO MOMATS
           WHEN (MOSL-LLAVE = MOMS-LLAVE) AND
                (MOMS-LLAVE > MOMA-LLAVE)

                PERFORM 400-ERROR-INTEGRIDAD-DATOS

                PERFORM UNTIL MOMS-LLAVE =
                   (MOMA-LLAVE AND MOSL-LLAVE) OR WKS-FIN-MOMATS
                        READ MOMATS
                        AT END
                           SET WKS-FIN-MOMATS TO TRUE
                        END-READ
                END-PERFORM

                IF MOMS-LLAVE =(MOMA-LLAVE AND MOSL-LLAVE)
                   ADD 1 TO WKS-REGISTROS-AJUSTADOS
                   DISPLAY
                   "     SE HIZO UN AJUSTE DE LECTURA EN MOMATS "
                   DISPLAY
                   " SE LOGRO UBICAR LAS LLAVES EN REGISTROS SIGUIENTES"
                   PERFORM 500-INTEGRIDAD-DISPLAY
                ELSE
                   ADD 1 TO WKS-REGISTROS-NO-AJ
                   DISPLAY
                   "    SE TRATO DE UBICAR LA LLAVE Y NO SE ENCONTRO"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMATS"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOSLIN"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOMAES !!!"
                   PERFORM 400-ERROR-INTEGRIDAD-DATOS
                END-IF
      *             SI MOMAES > - NO MOMATS >- NO MOSLIN
           WHEN (MOMA-LLAVE > MOSL-LLAVE) AND
                (MOMA-LLAVE > MOMS-LLAVE)
                PERFORM 400-ERROR-INTEGRIDAD-DATOS

                PERFORM UNTIL MOMA-LLAVE =
                   (MOSL-LLAVE AND MOMS-LLAVE)
                   OR WKS-FIN-MOMATS OR WKS-FIN-MOSLIN

                        READ MOMATS
                        AT END
                           SET WKS-FIN-MOMATS TO TRUE
                        END-READ

                        READ MOSLIN
                        AT END
                           SET WKS-FIN-MOSLIN TO TRUE
                        END-READ
                END-PERFORM

                IF MOMA-LLAVE =(MOMS-LLAVE AND MOSL-LLAVE)
                   ADD 1 TO WKS-REGISTROS-AJUSTADOS
                   DISPLAY
                   "    SE HIZO UN AJUSTE DE LECTURA EN MOMATS Y MOSLIN"
                   DISPLAY
                   " SE LOGRO UBICAR LAS LLAVES EN REGISTROS SIGUIENTES"
                   PERFORM 500-INTEGRIDAD-DISPLAY
                ELSE
                   ADD 1 TO WKS-REGISTROS-NO-AJ
                   DISPLAY
                   "    SE TRATO DE UBICAR LA LLAVE Y NO SE ENCONTRO"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMAES"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOMATS !!!"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOSLIN !!!"
                   PERFORM 400-ERROR-INTEGRIDAD-DATOS
                END-IF
      *             SI MOMATS > - NO MOMAES >- NO MOSLIN
           WHEN (MOMS-LLAVE > MOSL-LLAVE) AND
                (MOMS-LLAVE > MOMA-LLAVE)
                PERFORM 400-ERROR-INTEGRIDAD-DATOS

                PERFORM UNTIL MOMS-LLAVE =
                   (MOSL-LLAVE AND MOMA-LLAVE)
                   OR WKS-FIN-MOMAES OR WKS-FIN-MOSLIN

                        READ MOMAES
                        AT END
                           SET WKS-FIN-MOMAES TO TRUE
                        END-READ

                        READ MOSLIN
                        AT END
                           SET WKS-FIN-MOSLIN TO TRUE
                        END-READ
                END-PERFORM

                IF MOMS-LLAVE =(MOMA-LLAVE AND MOSL-LLAVE)
                   ADD 1 TO WKS-REGISTROS-AJUSTADOS
                   DISPLAY
                   "   SE HIZO UN AJUSTE DE LECTURA EN MOMAES Y MOSLIN"
                   DISPLAY
                   " SE LOGRO UBICAR LAS LLAVES EN REGISTROS SIGUIENTES"
                   PERFORM 500-INTEGRIDAD-DISPLAY
                ELSE
                   ADD 1 TO WKS-REGISTROS-NO-AJ
                   DISPLAY
                   "     SE TRATO DE UBICAR LA LLAVE Y NO SE ENCONTRO"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMATS"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOMAES !!!"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOSLIN !!!"
                   PERFORM 400-ERROR-INTEGRIDAD-DATOS
                END-IF
      *    SI MOMAES SI MOMATS, NO MOSLIN AND MOSLIN > A LOS DEMAS
           WHEN (MOMS-LLAVE < MOSL-LLAVE) AND
                (MOMA-LLAVE < MOSL-LLAVE)

                PERFORM 400-ERROR-INTEGRIDAD-DATOS

                PERFORM UNTIL MOSL-LLAVE =
                   (MOMS-LLAVE AND MOMA-LLAVE)
                   OR WKS-FIN-MOMAES OR WKS-FIN-MOMATS

                        READ MOMAES
                        AT END
                           SET WKS-FIN-MOMAES TO TRUE
                        END-READ

                        READ MOMATS
                        AT END
                           SET WKS-FIN-MOMATS TO TRUE
                        END-READ
                END-PERFORM
                IF MOMA-LLAVE =(MOMS-LLAVE AND MOSL-LLAVE)
                   ADD 1 TO WKS-REGISTROS-AJUSTADOS
                   DISPLAY
                   "          SE HIZO UN AJUSTE EN MOMATS Y MOMATS"
                   PERFORM 500-INTEGRIDAD-DISPLAY
                ELSE
                   ADD 1 TO WKS-REGISTROS-NO-AJ
                   DISPLAY
                   "    SE TRATO DE UBICAR LA LLAVE Y NO SE ENCONTRO"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMAES"
                   DISPLAY
                   ">           LLAVE SI ESTABA EN MOMATS"
                   DISPLAY
                   ">       !!! LLAVE NO ESTABA EN MOSLIN !!!"
                   PERFORM 400-ERROR-INTEGRIDAD-DATOS
                END-IF
           END-EVALUATE.
       310-CASOS-NO-INTEGRIDAD-E.             EXIT.

       320-LECTURAS-PAREJAS SECTION.
           READ MOMAES
                AT END SET WKS-FIN-MOMAES TO TRUE
           END-READ
           READ MOMATS
                   AT END SET WKS-FIN-MOMATS TO TRUE
           END-READ
           READ MOSLIN
                AT END SET WKS-FIN-MOSLIN TO TRUE
           END-READ.
       320-LECTURAS-PAREJAS-E.                EXIT.

      *-----> SERIES 400 ENCARGADAS DE HACER DISPLAY DE LOS ERRORES
       400-ERROR-INTEGRIDAD-DATOS SECTION.
           DISPLAY
           "================================================="
           DISPLAY
           "*          LLAVES SIN INTEGRIDAD DETALLE        *"
           DISPLAY
           "================================================="
           DISPLAY
           "==> LLAVE MOMAES : " MOMA-LLAVE
           DISPLAY
           "==> LLAVE MOMATS : " MOMS-LLAVE
           DISPLAY
           "==> LLAVE MOSLIN : " MOSL-LLAVE
           DISPLAY
           "***         LLAVES DISTINTAS EN ARCHIVOS"
           DISPLAY
           "================================================="
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       400-ERROR-INTEGRIDAD-DATOS-E.          EXIT.

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
           "==> FILE STATUS DEL ARCHIVO MOMAES :" FS-MOMAES
           UPON CONSOLE
           DISPLAY
           "==> FILE STATUS DEL ARCHIVO MOMATS :" FS-MOMATS
           UPON CONSOLE
           DISPLAY
           "==> FILE STATUS DEL ARCHIVO MOSLIN :" FS-MOSLIN
           UPON CONSOLE
           DISPLAY
           "==> VERIFICAR DETALLE AL FINAL DEL SPOOL"
           UPON CONSOLE
           DISPLAY
           "================================================="
           UPON CONSOLE.
       410-ERRORES-APERTURA-E.                 EXIT.

      *-----> SERIE 500 ENCARGADA DE DISPLAY DE REPORTES
       500-INTEGRIDAD-DISPLAY SECTION.
           DISPLAY
           "================================================="
           DISPLAY
           "*       INTEGRIDAD DE LLAVES COMPROBADA         *"
           DISPLAY
           "================================================="
           DISPLAY
           "==> LLAVE MOMAES : " MOMA-LLAVE
           DISPLAY
           "==> LLAVE MOMATS : " MOMS-LLAVE
           DISPLAY
           "==> LLAVE MOSLIN : " MOSL-LLAVE
           DISPLAY
           "*** LLAVES IGUALES IGUALES EN TODOS LOS ARCHIVOS"
           DISPLAY
           "================================================="
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       500-INTEGRIDAD-DISPLAY-E.              EXIT.

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
           "==> REGISTROS INTEGROS     TOTAL : " WKS-REGISTROS-INTEGROS
           DISPLAY
           "==> REGISTROS NO INTEGROS  TOTAL : " WKS-REGISTROS-NO-INTGR
           DISPLAY
           "==> REGISTROS AJUSTADOS    TOTAL : " WKS-REGISTROS-AJUSTADOS
           DISPLAY
           "==> REGISTROS NO AJUSTADOS TOTAL : " WKS-REGISTROS-NO-AJ
           DISPLAY
           "=================================================".
       600-ESTADISTICAS-E.                    EXIT.

       700-CIERRA-ARCHIVOS SECTION.
           CLOSE MOMAES
           CLOSE MOMATS
           CLOSE MOSLIN.
       700-CIERRA-ARCHIVOS-E.                 EXIT.