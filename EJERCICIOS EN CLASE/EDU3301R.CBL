      ******************************************************************
      * FECHA       : 17/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU3301R                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : RUTINA PARA VALIDAR LA ESTRUCTURA DE UN DPI Y SI *
      *             : SU DIGITO VERIFICADOR COINCIDE CON ESTE.         *
      * ARCHIVOS    : NO APLICA                                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU3301R.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      * ---->PARA VALIDAR SI LOS DATOS INGRESADOS SON CORRECTOS CREAMOS
      *      NUESTRAS PROPIAS CLASES DE ESTA FORMA NO VAMOS A EVALUAR
           CLASS DPI IS
             '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' '-' ' '.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *------------------->   VARIABLES NUMERICAS PARA DPI
       01 WKS-REG-NUMERICO-DPI.
          02 WKS-NUM-DPI OCCURS 13 TIMES INDEXED BY DI9.
             03 WKS-DPI-REG     PIC 9(01) VALUE ZEROS.
      *------------------->   VARIABLES PARA PROCESOS
       01 WKS-INDEX             PIC 9(02) VALUE ZEROS.
       01 WKS-CONTADOR-LONG     PIC 9(02) VALUE ZEROS.
       01 WKS-VAR-VALIDA        PIC 9(02) VALUE ZEROS.
       01 WKS-SUM-TOTAL         PIC 9(03) VALUE ZEROS.
       01 WKS-CONTADOR-CICL     PIC 9(02) VALUE ZEROS.
       01 WKS-VAR-CICLO         PIC 9(02) VALUE 9.
       01 WKS-CEROS-FALTANTES   PIC 9(02) VALUE ZEROS.
       LINKAGE SECTION.
       01 VALIDACION-DPI.
          02 DPI-X OCCURS 15 TIMES INDEXED BY DIX.
             03 DPI-DIGITOS     PIC X(01).
       01 CODIGO-RESPUESTA      PIC 9(02).
       01 DESC-RESPUESTA        PIC X(50).
       PROCEDURE DIVISION USING VALIDACION-DPI, CODIGO-RESPUESTA,
                                                  DESC-RESPUESTA.
       100-PRINCIPAL SECTION.
           PERFORM 100-INICIALIZAR
           PERFORM 200-VERIFICA-DATOS
           PERFORM 200-DICCIONARIO-RC
           GOBACK.
       100-PRINCIPAL-E.            EXIT.

       100-INICIALIZAR SECTION.
           INITIALIZE WKS-REG-NUMERICO-DPI, WKS-INDEX, WKS-CONTADOR-LONG
                      WKS-VAR-VALIDA, WKS-SUM-TOTAL, WKS-CONTADOR-CICL,
                      WKS-VAR-CICLO
           INITIALIZE CODIGO-RESPUESTA, DESC-RESPUESTA.
       100-INICIALIZAR-E. EXIT.

       200-VERIFICA-DATOS SECTION.
           IF VALIDACION-DPI IS DPI
                 MOVE 9 TO WKS-VAR-CICLO
                 MOVE ZEROS TO WKS-CONTADOR-CICL
                 PERFORM 200-VERIFICA-ESTRUCTURA
           ELSE
              MOVE 10 TO CODIGO-RESPUESTA
           END-IF.
       200-VERIFICA-DATOS-E.            EXIT.

       200-VERIFICA-ESTRUCTURA SECTION.
           PERFORM VARYING DIX FROM 1 BY 1 UNTIL DIX > 15
                   IF DPI-DIGITOS(DIX) NUMERIC
                      ADD 1 TO WKS-INDEX
                      MOVE DPI-DIGITOS(DIX) TO WKS-DPI-REG(WKS-INDEX)
                   END-IF
           END-PERFORM

           IF WKS-REG-NUMERICO-DPI EQUAL ZEROS
              MOVE 11 TO CODIGO-RESPUESTA
           ELSE
              DISPLAY " EL REGISTRO TRAE :" WKS-REG-NUMERICO-DPI
              IF WKS-INDEX < 13
                 MOVE 13 TO CODIGO-RESPUESTA
              END-IF

              IF WKS-INDEX = 13
                 PERFORM VARYING DI9 FROM 1 BY 1 UNTIL WKS-VAR-CICLO = 1
                         ADD 1 TO WKS-CONTADOR-CICL
                         IF WKS-CONTADOR-CICL <= 8
                            COMPUTE WKS-VAR-VALIDA =
                               (WKS-DPI-REG(DI9) * WKS-VAR-CICLO)
                         END-IF
                         SUBTRACT 1 FROM WKS-VAR-CICLO
                         COMPUTE WKS-SUM-TOTAL = WKS-SUM-TOTAL +
                            WKS-VAR-VALIDA
                 END-PERFORM
                 COMPUTE WKS-SUM-TOTAL = FUNCTION REM(WKS-SUM-TOTAL 11)
                 COMPUTE WKS-SUM-TOTAL = 11 - WKS-SUM-TOTAL

                 IF WKS-SUM-TOTAL = WKS-DPI-REG(9)
                    MOVE 00 TO CODIGO-RESPUESTA
                 ELSE
                    MOVE 12 TO CODIGO-RESPUESTA
                 END-IF
              END-IF
           END-IF.
       200-VERIFICA-ESTRUCTURA-E.            EXIT.

       200-DICCIONARIO-RC SECTION.
           EVALUATE CODIGO-RESPUESTA
           WHEN 00
                MOVE "EL DPI ES VALIDO" TO DESC-RESPUESTA
           WHEN 10
                MOVE "EL DPI NO ES NUMERICO" TO DESC-RESPUESTA

           WHEN 11
                MOVE "EL DPI ES INVALIDO, DEBE SER MAYOR A CEROS"
                   TO DESC-RESPUESTA
           WHEN 12
                MOVE
                   "EL DPI ES INVALIDO, NO COINCIDE DIGITO VERIFICADOR"
                   TO DESC-RESPUESTA
           WHEN 13
                MOVE
                   "EL DPI NO ES VALIDO, LOGITUD MENOR A 13 DIGITOS"
                   TO DESC-RESPUESTA
           END-EVALUATE.
       200-DICCIONARIO-RC-E.            EXIT.