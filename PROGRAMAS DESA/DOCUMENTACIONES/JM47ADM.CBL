      ******************************************************************
      * FECHA       : 28/08/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS (PEDR)                *
      * APLICACION  : TARJETA EMPRESARIAL                              *
      * PROGRAMA    : TMBD1CLF                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : CONSULTA EL ARCHIVO TLIMOR DETALLANDO CADA       *
      *             : CICLO Y MOSTRANDO EL TOTAL DE DIAS MORA POR      *
      *             : PERDIDA ESPERADA SEGUN LA MONEDA.                *
      * ARCHIVOS    : TLIMOR                                           *
      * ACCION (ES) : C=CONSULTAR                                      *
      * PROGRAMA(S) : XCTL                                             *
      * CANAL       : PLATAFORMA                                       *
      * INSTALADO   : 09/01/2023                                       *
      * BPM/RATIONAL: 228866                                           *
      * NOMBRE      : MANTENIMIENTO CONTEO DIAS MORA                   *
      * DESCRIPCION : PROYECTO                                         *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JM47ADM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01 WKS-VARIABLES.
          77  FECHA                    PIC X(10).
          77  HORA                     PIC X(8).
          77  TIEMPO            COMP-3 PIC S9(15).
       01  SWITCHES.
           02  SEND-MAPA               PIC X.
               88  SEND-MAPONLY                   VALUE '1'.
               88  SEND-MAPONLY-ERASE             VALUE '2'.
               88  SEND-DATAONLY                  VALUE '3'.
               88  SEND-DATAONLY-ALARM            VALUE '4'.
       01  DATOS-COMMAREA.
           02 DATOS-CFQO.
              03 WKS-PROGRAMA-1        PIC X(8)   VALUE SPACES.
              03 WKS-COD-CLIENTE       PIC X(8)   VALUE SPACES.
              03 WKS-CTA-CORPO.
                 04  WKS-CTA1          PIC 9(4)   VALUE ZEROES.
                 04  WKS-CTA2          PIC 9(4)   VALUE ZEROES.
                 04  WKS-CTA3          PIC 9(4)   VALUE ZEROES.
                 04  WKS-CTA4          PIC 9(4)   VALUE ZEROES.
                 04  FILLER            PIC 9(1)   VALUE ZEROES.
           02  WKS-VALIDA              PIC 9(1)   VALUE ZEROES.
           02  WKS-MAPA                PIC 9(1)   VALUE ZEROES.
           02  WKS-NOMBRE-CLI          PIC X(30)  VALUE SPACES.
           02  WKS-PROGRAMA-2          PIC X(8)   VALUE SPACES.
           02  WKS-PAGO-QUETZ-O-DOLAR  PIC 9(1)   VALUE ZEROES.
           02  WKS-RETMON              PIC X      VALUE ZEROES.
           02  WKS-SWMAP               PIC 9      VALUE ZEROES.

       01 WKS-DATOS.
          02 CONTROL-HORA.
             04 HORA1                PIC 9(02) VALUE ZEROES.
             04                      PIC X(01).
             04 MINUTOS              PIC 9(02) VALUE ZEROES.
             04                      PIC X(01).
             04 SEGUNDOS             PIC 9(02) VALUE ZEROES.
          02 CONTROL-FECHA.
             04 DIA                  PIC 9(02) VALUE ZEROES.
             04                      PIC X(01).
             04 MES                  PIC 9(02) VALUE ZEROES.
             04                      PIC X(01).
             04 ANIO                 PIC 9(04) VALUE ZEROES.
       01 TOTAL-DIAS-MORA            PIC 9(03) VALUE ZEROES.
       01  I                         PIC 9(02) VALUE ZEROES.
       01  J                         PIC 9(02) VALUE ZEROES.
       01 FECHA1                     PIC 9(08) VALUE ZEROES.
       01 FECHA-MASCARA.
          02 DIA-M                   PIC 9(02) VALUE ZEROES.
          02 FILLER                  PIC X(01) VALUE '/'.
          02 MES-M                   PIC 9(02) VALUE ZEROES.
          02 FILLER                  PIC X(01) VALUE '/'.
          02 ANIO-M                  PIC 9(04) VALUE ZEROES.
       01 MENSAJES.
          02 CLIENTE-NO-EXISTE       PIC X(40) VALUE
                               'CLIENTE NO EXISTE EN TLIMOR'.
          02 ERROR-TLIMOR            PIC X(40) VALUE
                               'ERROR CON ARCHIVO TLIMOR'.
          02 ERROR-MONEDA            PIC X(40) VALUE
                               'INGRESE UNA MONEDA VALIDA 1 Ó 2'.
          02 ERROR-CUENTA            PIC X(40) VALUE
                               'ES NECESARIO INGRESAR UNA CUENTA'.
          02 TLIMOR-CERRADO          PIC X(40) VALUE
                               'ARCHIVO TLIMOR ESTA CERRADO     '.
          02 TECLA-SALIDA            PIC X(40) VALUE
                               '**** PRESIONE F10 PARA SALIR ****'.

       COPY TLIMOR.
       COPY ERRWORK2.
       COPY TLFL1.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY COMTLLL3.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         02 FILLER PIC X  OCCURS 0 TO 4096 DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA     TO   DATOS-COMMAREA.
           IF WKS-PROGRAMA-2 EQUAL SPACES
              IF WKS-PROGRAMA-1 NOT EQUAL 'TL5D1QL8'
                 MOVE WKS-PROGRAMA-1    TO WKS-PROGRAMA-2
                 MOVE 'TL5D1QL8'        TO WKS-PROGRAMA-1
              END-IF
           END-IF
           PERFORM FECHA-HORA        THRU FECHA-HORA-E.

           IF (EIBTRNID = 'TLQ8')
              SET SEND-MAPONLY-ERASE TO TRUE
              PERFORM SEND-MAP       THRU SEND-MAP-E

              SET SEND-MAPONLY       TO TRUE
              PERFORM SEND-MAP       THRU SEND-MAP-E

              MOVE DFHCOMMAREA       TO DATOS-COMMAREA
              PERFORM LIMPIA-MAPA    THRU LIMPIA-MAPA-E

               IF (WKS-CTA-CORPO > 0) AND (WKS-RETMON = 1 OR 2)
                 PERFORM BUSCA-TLIMOR      THRU BUSCA-TLIMOR-E
                 PERFORM PROC-BIN-COMPLEJO THRU PROC-BIN-COMPLEJO-E
                 MOVE -1                   TO TLFL1-MONEDAL
               ELSE
                 MOVE -1              TO TLFL1-MONEDAL
                 MOVE 'CUENTA O MONEDA NO RECIBIDA' TO TLFL1-MENSAJEO
               END-IF

              SET SEND-DATAONLY TO TRUE
              PERFORM SEND-MAP THRU SEND-MAP-E
           ELSE
      *----     EVALUAR PFS.
             IF (EIBTRNID = 'TMCF')
              EVALUATE EIBAID
               WHEN DFHENTER
                  PERFORM RECIBE-DATOS   THRU RECIBE-DATOS-E
                  PERFORM LIMPIA-MAPA    THRU LIMPIA-MAPA-E

                  IF (WKS-CTA-CORPO > 0) AND (WKS-RETMON = 1 OR 2)
                    PERFORM BUSCA-TLIMOR      THRU BUSCA-TLIMOR-E
                    PERFORM PROC-BIN-COMPLEJO THRU PROC-BIN-COMPLEJO-E
                    MOVE -1              TO TLFL1-MONEDAL
                  ELSE
                    MOVE -1              TO TLFL1-MONEDAL
                    MOVE 'CUENTA O MONEDA INVALIDA   ' TO TLFL1-MENSAJEO
                  END-IF

                  SET SEND-DATAONLY TO TRUE
                  PERFORM SEND-MAP  THRU SEND-MAP-E
               WHEN DFHPF10
               WHEN DFHCLEAR
                  IF WKS-PROGRAMA-1 NOT EQUAL SPACES AND
                                             LOW-VALUES
                        EXEC CICS
                             XCTL PROGRAM(WKS-PROGRAMA-1)
                                  COMMAREA(DATOS-COMMAREA)
                                  LENGTH  (LENGTH OF DATOS-COMMAREA)
                                  NOHANDLE
                        END-EXEC
                  ELSE
                        EXEC CICS XCTL PROGRAM('SYSCIMEN') END-EXEC
                  END-IF
               WHEN OTHER
                  MOVE 'TECLA INVALIDA' TO TLFL1-MENSAJEO
                  MOVE -1               TO TLFL1-MONEDAL
                  SET SEND-DATAONLY     TO TRUE
                  PERFORM SEND-MAP      THRU SEND-MAP-E
               END-EVALUATE
             ELSE
                SET SEND-MAPONLY-ERASE TO TRUE
                PERFORM SEND-MAP THRU SEND-MAP-E
                SET SEND-MAPONLY TO TRUE
                PERFORM SEND-MAP THRU SEND-MAP-E
             END-IF
           END-IF

           EXEC CICS RETURN TRANSID('TMCF')
                            COMMAREA(DATOS-COMMAREA)
           END-EXEC.
      *     GOBACK.
       BUSCA-TLIMOR SECTION.
           PERFORM LIMPIA-MAPA THRU LIMPIA-MAPA-E.
           MOVE WKS-CTA-CORPO(1:6)  TO TLIM-BIN
           MOVE WKS-CTA-CORPO(7:5)  TO TLIM-NUMERO-CORRELATIVO
           MOVE WKS-CTA-CORPO(12:2) TO TLIM-CNTROL-ADICIONALES
           MOVE WKS-CTA-CORPO(14:1) TO TLIM-CLASE-TARJETA
           MOVE WKS-CTA-CORPO(15:1) TO TLIM-TIPO-TARJETA
           MOVE WKS-CTA-CORPO(16:1) TO TLIM-DIGITO-VERIFICADOR
           MOVE WKS-RETMON          TO TLIM-MONEDA
           EXEC CICS READ
                     DATASET('TLIMOR')
                     INTO   (REG-TLIMOR)
                     RIDFLD (TLIM-LLAVE)
                     NOHANDLE
           END-EXEC.
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                    PERFORM MUEVE-VALORES-MORA
                                       THRU MUEVE-VALORES-MORA-E
                    MOVE TECLA-SALIDA        TO TLFL1-MENSAJEO
               WHEN DFHRESP(NOTFND)
                    MOVE CLIENTE-NO-EXISTE   TO TLFL1-MENSAJEO
               WHEN DFHRESP(NOTOPEN)
                    MOVE TLIMOR-CERRADO      TO TLFL1-MENSAJEO
               WHEN OTHER
                   MOVE ERROR-TLIMOR         TO TLFL1-MENSAJEO
           END-EVALUATE.
       BUSCA-TLIMOR-E. EXIT.

       MUEVE-VALORES-MORA.
            MOVE WKS-RETMON     TO TLFL1-MONEDAO
            MOVE WKS-NOMBRE-CLI TO TLFL1-NOMBREO
            PERFORM VARYING I  FROM 1 BY 1 UNTIL I GREATER 18
               MOVE I                    TO TLFL1-CICLOO(I)
               IF TLIM-CICLO-VENCIDO(I) > ZEROES
                  MOVE TLIM-DIAS-MORA(I)    TO TLFL1-DIAS-MORAO(I)
                  ADD  TLIM-DIAS-MORA(I)    TO TOTAL-DIAS-MORA
      *--> CAMBIA FORMATO DE FECHA INICIO A DD/MM/AAAA
                  MOVE TLIM-FECHA-INICIO(I) TO FECHA1
                  MOVE FECHA1(1:4)          TO ANIO-M
                  MOVE FECHA1(5:2)          TO MES-M
                  MOVE FECHA1(7:2)          TO DIA-M
                  MOVE FECHA-MASCARA        TO TLFL1-FECHA-INICIOO(I)
      *--> CAMBIA FORMATO DE FECHA CIERRE A DD/MM/AAAA
                  MOVE TLIM-FECHA-FIN(I)    TO FECHA1
                  MOVE FECHA1(1:4)          TO ANIO-M
                  MOVE FECHA1(5:2)          TO MES-M
                  MOVE FECHA1(7:2)          TO DIA-M
                  MOVE FECHA-MASCARA        TO TLFL1-FECHA-CIERREO(I)
               ELSE
                   MOVE ' 0 '               TO TLFL1-DIAS-MORAO(I)
                   MOVE SPACES              TO TLFL1-FECHA-INICIOO(I)
                   MOVE SPACES              TO TLFL1-FECHA-CIERREO(I)
               END-IF
            END-PERFORM
                   MOVE TOTAL-DIAS-MORA      TO TLFL1-TOTAL-DIASO.
       MUEVE-VALORES-MORA-E. EXIT.

       FECHA-HORA.
           EXEC CICS
                ASSIGN OPID(WKS-USER-ID)
                       NOHANDLE
           END-EXEC.
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
               PERFORM PAR-ERROR THRU PAR-ERROR-E
           END-IF.
           EXEC CICS
                IGNORE CONDITION MAPFAIL
                       NOHANDLE
           END-EXEC.
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
               PERFORM PAR-ERROR THRU PAR-ERROR-E
           END-IF.
           EXEC CICS
                ASKTIME ABSTIME(TIEMPO)
                        NOHANDLE
           END-EXEC.
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
               PERFORM PAR-ERROR THRU PAR-ERROR-E
           END-IF.
           EXEC CICS FORMATTIME
                     ABSTIME(TIEMPO)
                     NOHANDLE
                     DDMMYYYY(FECHA)
                     DATESEP('/')
                     TIME(HORA)
                     TIMESEP(':')
           END-EXEC.
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
               PERFORM PAR-ERROR THRU PAR-ERROR-E
           END-IF.
           MOVE WKS-USER-ID TO TLFL1-TERMIDO.
           MOVE FECHA       TO TLFL1-FECHAO.
           MOVE HORA        TO TLFL1-HORAO.
       FECHA-HORA-E.  EXIT.

       SEND-MAP.
           IF SEND-MAPONLY
               EXEC CICS
                    SEND MAP('TLFL1')
                         MAPSET('TLFL1')
                         FREEKB
               END-EXEC
           END-IF
           IF SEND-MAPONLY-ERASE
               EXEC CICS
                    SEND MAP('TLFL1')
                         MAPSET('TLFL1')
                         ERASE
                         FREEKB
               END-EXEC
           END-IF.
           MOVE FECHA                      TO TLFL1-FECHAO
           MOVE HORA                       TO TLFL1-HORAO
           MOVE EIBTRMID                   TO TLFL1-TERMIDO
           IF SEND-DATAONLY
               EXEC CICS
                    SEND MAP('TLFL1')
                         MAPSET('TLFL1')
                         DATAONLY
                         CURSOR
                         FREEKB
               END-EXEC
           END-IF.
           IF SEND-DATAONLY-ALARM
               EXEC CICS
                    SEND MAP('TLFL1')
                         MAPSET('TLFL1')
                         DATAONLY
                         CURSOR
                         ALARM
                         FREEKB
               END-EXEC
           END-IF.
       SEND-MAP-E.  EXIT.

       PAR-ERROR.
           MOVE WKS-USER-ID  TO  ERR-OPID.
           MOVE EIBFN        TO  ERR-FN,  ERR-COMMAND.
           MOVE EIBRCODE     TO  ERR-RCODE.
           MOVE EIBRESP      TO  ERR-RESP.
           MOVE EIBTRNID     TO  ERR-TRAN.
           MOVE EIBTRMID     TO  ERR-TERM.
           MOVE EIBDS        TO  ERR-ARCH.
           EXEC CICS XCTL
                PROGRAM('TESTTRAN')
                COMMAREA(COMMAREA-ERR)
                LENGTH(50)
                NOHANDLE
           END-EXEC.
       PAR-ERROR-E.  EXIT.

       RECIBE-DATOS.
           EXEC CICS
                RECEIVE MAP('TLFL1')
                        MAPSET('TLFL1')
                        INTO(TLFL1I)
                        NOHANDLE
           END-EXEC.
            EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     MOVE TLFL1-MONEDAI   TO WKS-RETMON
                WHEN DFHRESP(MAPFAIL)
                     MOVE 'INGRESE DATOS' TO TLFL1-MENSAJEO
                WHEN OTHER
                     MOVE 'OTRO ERROR'    TO TLFL1-MENSAJEO
                END-EVALUATE.
       RECIBE-DATOS-E. EXIT.

       PROC-BIN-COMPLEJO SECTION.
           MOVE WKS-CTA-CORPO         TO  TLL3-CUENTA-COMPLEJA
           MOVE ZEROES                TO  TLL3-CUENTA-LOCAL
           MOVE ZEROES                TO  TLL3-STATUS.
           EXEC CICS LINK PROGRAM('TL5D1LL3')
                COMMAREA(COMTLLL3)
                LENGTH(LENGTH OF COMTLLL3)
           END-EXEC.
           IF TLL3-STATUS EQUAL ZEROES
              MOVE TLL3-CUENTA-COMPLEJA TO TLFL1-CUENTAO
           ELSE
              MOVE 'ERROR CON RUTINA DE BIN COMPLEJO' TO TLFL1-MENSAJEO
           END-IF.
       PROC-BIN-COMPLEJO-E. EXIT.

       LIMPIA-MAPA.
            MOVE SPACES         TO TLFL1-CUENTAO
            MOVE SPACES         TO TLFL1-NOMBREO
            MOVE SPACES         TO TLFL1-MONEDAO
            MOVE SPACES         TO TLFL1-MENSAJEO
            MOVE SPACES         TO TLFL1-TOTAL-DIASO
            PERFORM VARYING I  FROM 1 BY 1 UNTIL I GREATER 18
                  MOVE I                    TO TLFL1-CICLOO(I)
                  MOVE SPACES               TO TLFL1-DIAS-MORAO(I)
                  MOVE SPACES               TO TLFL1-FECHA-INICIOO(I)
                  MOVE SPACES               TO TLFL1-FECHA-CIERREO(I)
            END-PERFORM.
       LIMPIA-MAPA-E. EXIT.