      ******************************************************************
      * FECHA       : 12/06/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS (EEDR)                *
      * APLICACION  : EDUCACION SEMILLERO                              *
      * PROGRAMA    : ED3D1QL3                                         *
      * TIPO        : LINEA CON MAPA                                   *
      * DESCRIPCION : ADMINISTRADOR CUENTAS DE AHORRO, BANCO INDUSTRIAL*
      *             : ESTA DEPENDENCIA DEL ADMINISTRADOR UBICA CUENTAS *
      *             : POR MEDIO DEL INDICE ALTERNO, SU ÚNICA FUNCIÓN ES*
      *             : HACER UN BROWSE AL PATH                          *
      * ARCHIVOS    : EDP1AH                      (VSAM KSDS)          *
      * ACCION (ES) : MENU PRINCIPAL                                   *
      * PROGRAMA(S) : XCTRL ED3D1LLQ                                   *
      * CANAL       : PANTALLA ADMINISTRATIVA                          *
      * INSTALADO   : DD/MM/AAAA                                       *
      ******************************************************************
      *        A D M I N I S T R A D O R   D E   C U E N T A S
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     ED3D1CLD.
       AUTHOR.                         DANIEL RAMIREZ.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SPECIAL-NAMES.
      *     CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *               C A M P O S     D E     T R A B A J O            *
      ******************************************************************
       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-NOMBRE-PROGRAMA     PIC X(08)  VALUE "ED3D1QL3".
          02 WKS-INDEX.
             03 INDEX-1              PIC 9(02)  VALUE ZEROS.
             03 INDEX-2              PIC 9(02)  VALUE ZEROS.
             03 INDEX-3              PIC 9(02)  VALUE ZEROS.
          02 SEND-FLAGS              PIC X(01)  VALUE SPACES.
             88 SEND-ERASE                      VALUE '1'.
             88 SEND-DATAONLY                   VALUE '2'.
             88 SEND-DATAONLY-ALARM             VALUE '3'.
             88 SEND-COMPLETO                   VALUE '4'.
             88 SEND-ALTERNATE                  VALUE '5'.
          02 WKS-FECHAS-HORAS.
             03 WKS-TIEMPO           PIC S9(15) COMP-3 VALUE +0.
             03 FECHA-SYS            PIC 9(08)  VALUE ZEROS.
             03 WKS-HORA.
                04 HORA              PIC 9(02).
                04 MINUTO            PIC 9(02).
                04 SEGUNDO           PIC 9(02).
             03 WKS-HORA-EDIT.
                04 HORA              PIC 9(02).
                04 FILLER            PIC X(02)  VALUE "H:".
                04 MINUTO            PIC 9(02).
                04 FILLER            PIC X(02)  VALUE "M:".
                04 SEGUNDO           PIC 9(02).
                04 FILLER            PIC X(01)  VALUE "S".
           02 WKS-FIN                PIC X(26)  VALUE
                'GOODBYE SEE YOU LATER EDQ3'.
      ******************************************************************
      *      DEFINICION DE COMMAREA PARA COMUNICACION ENTRE PROGRAMAS  *
      ******************************************************************
       01 WKS-COMMAREAS.
          02 WKS-CIF-PRIMERO         PIC 9(08)  VALUE ZEROS.
          02 WKS-LLAVE-CP            PIC 9(10)  VALUE ZEROS.
          02 WKS-CIF-ULTIMO          PIC 9(08)  VALUE ZEROS.
          02 WKS-LLAVE-CU            PIC 9(10)  VALUE ZEROS.
      *                  C O N T R O L A D O R E S
          02 WKS-FIN-ARCH            PIC 9(01)  VALUE ZEROS.
          02 WKS-FIN-DATOS           PIC 9(01)  VALUE ZEROS.
          02 WKS-FIN-DATOS-ATRAS     PIC 9(01)  VALUE ZEROS.
          02 WKS-FIN-ARCH-ATRAS      PIC 9(01)  VALUE ZEROS.
          02 ATRAS                   PIC 9(01)  VALUE ZEROS.
          02 WKS-PAG-CONT            PIC 9(02)  VALUE ZEROS.
          02 WKS-UNICA               PIC 9(01)  VALUE ZEROS.
          02 WKS-NEXTS               PIC 9(01)  VALUE ZEROS.
          02 MENU-CTRL               PIC 9(01)  VALUE ZEROS.

      ******************************************************************
      *                S E C C I O N    D E    C O P Y S               *
      ******************************************************************
      * --- MAPAS Y CARACTERISTICAS
       COPY EDCLD.
       COPY EDMAAH.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY ATTR.
      ******************************************************************
      *         S E C C I O N    D E    C O M U N I C A C I O N E S    *
      ******************************************************************
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           02 FL    PIC X OCCURS 0 TO 9999 DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
       000-PRINCIPAL SECTION.
           INITIALIZE WKS-COMMAREAS
           MOVE    DFHCOMMAREA    TO WKS-COMMAREAS
           PERFORM 100-VALIDA-LLAMADO
           PERFORM REGRESA-CONTROL-A-TRANS.
       000-PRINCIPAL-E. EXIT.

       100-VALIDA-LLAMADO SECTION.
           IF EIBTRNID NOT = "EDCD"
              MOVE    1               TO MENU-CTRL
              MOVE   -1               TO EDCLD-DETALLESL(1)
              SET     SEND-ALTERNATE  TO TRUE
              PERFORM ENVIA-MAPA
              MOVE    1               TO EDMA-CODIGO-CLIENTE
              PERFORM START-INICIO
              PERFORM ENVIA-MAPA
           ELSE
              PERFORM 101-MENU-INICIO
           END-IF.
       100-VALIDA-LLAMADO-E. EXIT.

       101-MENU-INICIO SECTION.
           EVALUATE TRUE
           WHEN EIBCALEN = ZERO
                MOVE    -1             TO EDCLD-DETALLESL(1)
                SET     SEND-ALTERNATE TO TRUE
                PERFORM ENVIA-MAPA
                MOVE     1             TO EDMA-CODIGO-CLIENTE
                PERFORM START-INICIO
                PERFORM ENVIA-MAPA

           WHEN EIBAID = DFHENTER
                MOVE     1             TO EDMA-CODIGO-CLIENTE
                SET     SEND-DATAONLY  TO TRUE
                PERFORM START-INICIO
                PERFORM ENVIA-MAPA

           WHEN EIBAID = DFHPF5
                IF WKS-FIN-ARCH NOT = 1
                   PERFORM READ-SIGUIENTES
                ELSE
                   MOVE 'SIN MAS DATOS QUE MOSTRAR'
                                       TO EDCLD-MENSAJEO
                END-IF
                SET SEND-DATAONLY TO TRUE
                PERFORM ENVIA-MAPA

           WHEN EIBAID = DFHPF6
                MOVE     1             TO ATRAS
                PERFORM READ-ANTERIORES
                SET  SEND-DATAONLY      TO TRUE
                PERFORM ENVIA-MAPA

           WHEN EIBAID = DFHPF10
                  IF MENU-CTRL = 1
                     EXEC CICS
                         XCTL PROGRAM('ED3D1LLQ')
                     END-EXEC
                  ELSE
                     EXEC CICS SEND TEXT FROM(WKS-FIN) ERASE END-EXEC
                     EXEC CICS RETURN END-EXEC
                  END-IF

           WHEN EIBAID = DFHCLEAR
                MOVE         -1               TO EDCLD-DETALLESL(1)
                SET      SEND-ERASE           TO TRUE
                PERFORM  ENVIA-MAPA

           WHEN OTHER
                MOVE 'OPCION EN DESARROLLO / TECLA INVALIDA'
                                              TO EDCLD-MENSAJEO
                MOVE         -1               TO EDCLD-DETALLESL(1)
                SET  SEND-DATAONLY-ALARM      TO TRUE
                PERFORM ENVIA-MAPA
           END-EVALUATE.
       101-MENU-INICIO-E. EXIT.

      *****************************************************************
      *                     CARGA DE DATOS INICIAL
      *****************************************************************
      * -----> CODIGO FUENTE EN LIBRERIA   =   EEDR.SOURCE.RTC(CARGAINT)
       START-INICIO SECTION.
           PERFORM LIMPIA-ARREGLO
           EXEC CICS STARTBR
                DATASET ('EDP1AH')
                RIDFLD  (EDMA-CODIGO-CLIENTE)
                GTEQ
                NOHANDLE
           END-EXEC

           EVALUATE EIBRESP
           WHEN DFHRESP(NORMAL)
                PERFORM CARGA-DATOS-INICIO-ARRAY UNTIL INDEX-1 >= 18 OR
                                                 WKS-FIN-ARCH   = 01

                EXEC CICS ENDBR DATASET('EDP1AH') END-EXEC
                SET SEND-DATAONLY        TO TRUE
           WHEN DFHRESP(NOTFND)
                MOVE 'ARCHIVO MAESTRO EDM1AH DE CUENTAS VACIO'
                                         TO EDCLD-MENSAJEO
           WHEN DFHRESP(NOTOPEN)
                MOVE 'ARCHIVO MAESTRO DE CUENTAS EDM1AH CERRADO'
                                         TO  EDCLD-MENSAJEO
           WHEN OTHER
                MOVE 'OCURRIO UN ERROR DESCONOCIDO EN ARHIVO EDM1AH'
                                         TO EDCLD-MENSAJEO
                SET SEND-DATAONLY-ALARM  TO TRUE
                PERFORM ENVIA-MAPA
                PERFORM REGRESA-CONTROL-A-TRANS
           END-EVALUATE.
       START-INICIO-E. EXIT.

       CARGA-DATOS-INICIO-ARRAY SECTION.
           ADD  1  TO INDEX-1
           EXEC CICS READNEXT
                     DATASET  ('EDP1AH')
                     INTO     (REG-EDMAAH)
                     RIDFLD   (EDMA-CODIGO-CLIENTE)
                     KEYLENGTH(LENGTH OF EDMA-CODIGO-CLIENTE)
                     NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     PERFORM LLENA-ARREGLO-DISPLAY
                WHEN DFHRESP(DUPKEY)
                     PERFORM LLENA-ARREGLO-DISPLAY
                WHEN DFHRESP(NOTFND)
                     MOVE 'LA LLAVE SIGUIENTE NO SE ENCONTRO'
                                             TO EDCLD-MENSAJEO
                     MOVE    -1              TO EDCLD-DETALLESL(1)
                     MOVE     1              TO WKS-FIN-ARCH
                     SET SEND-DATAONLY-ALARM TO TRUE
                WHEN DFHRESP(ENDFILE)
                     MOVE    -1              TO EDCLD-DETALLESL(1)
                     MOVE     1              TO WKS-FIN-ARCH
                     SET SEND-DATAONLY-ALARM TO TRUE
           END-EVALUATE.

       CARGA-DATOS-INICIO-ARRAY-E. EXIT.
      ****************************************************************
      *     START DEL REGISTRO ANTES DE LEER /CONDICIONES             *
      ****************************************************************
       READ-SIGUIENTES SECTION.
           MOVE WKS-CIF-PRIMERO    TO EDMA-CODIGO-CLIENTE
           MOVE       ZEROS        TO WKS-FIN-ARCH-ATRAS
           MOVE       ZEROS        TO WKS-FIN-DATOS-ATRAS
           MOVE         -1         TO EDCLD-DETALLESL(1)
           MOVE       ZEROS        TO ATRAS
           PERFORM START-READ-NEXT.
       READ-SIGUIENTES-E. EXIT.

       START-READ-NEXT SECTION.
           PERFORM LIMPIA-ARREGLO
           EXEC CICS STARTBR
                DATASET ('EDP1AH')
                RIDFLD  (EDMA-CODIGO-CLIENTE)
                GTEQ
                NOHANDLE
           END-EXEC

           EVALUATE EIBRESP
           WHEN DFHRESP(NORMAL)
                PERFORM 111-READ-NEXT UNTIL INDEX-1 >= 18 OR
                                      WKS-FIN-ARCH   = 01
                EXEC CICS ENDBR DATASET('EDP1AH') END-EXEC
                SET SEND-DATAONLY        TO TRUE
           WHEN DFHRESP(NOTFND)
                MOVE 'ARCHIVO MAESTRO EDM1AH DE CUENTAS VACIO'
                                         TO EDCLD-MENSAJEO
           WHEN DFHRESP(NOTOPEN)
                MOVE 'ARCHIVO MAESTRO DE CUENTAS EDM1AH CERRADO'
                                         TO  EDCLD-MENSAJEO
           WHEN OTHER
                MOVE 'OCURRIO UN ERROR DESCONOCIDO EN ARHIVO EDM1AH'
                                         TO EDCLD-MENSAJEO
                SET SEND-DATAONLY-ALARM  TO TRUE
                PERFORM ENVIA-MAPA
                PERFORM REGRESA-CONTROL-A-TRANS
           END-EVALUATE.
       START-READ-NEXT-E. EXIT.

       111-READ-NEXT SECTION.
           ADD  1  TO INDEX-1
           EXEC CICS READNEXT
                     DATASET  ('EDP1AH')
                     INTO     (REG-EDMAAH)
                     RIDFLD   (EDMA-CODIGO-CLIENTE)
                     NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     PERFORM LLENA-ARREGLO-DISPLAY
                WHEN DFHRESP(DUPKEY)
                        IF INDEX-1 = 1 AND WKS-LLAVE-CP > ZEROS
                           AND EDMA-NUMERO-CUENTA NOT = WKS-LLAVE-CP

                            SUBTRACT 1 FROM INDEX-1
                        ELSE
                           PERFORM LLENA-ARREGLO-DISPLAY
                        END-IF
                WHEN DFHRESP(NOTFND)
                     MOVE 'LA LLAVE SIGUIENTE NO SE ENCONTRO'
                                             TO EDCLD-MENSAJEO
                     MOVE    -1              TO EDCLD-DETALLESL(1)
                     MOVE     1              TO WKS-FIN-ARCH
                     SET SEND-DATAONLY-ALARM TO TRUE
                WHEN DFHRESP(ENDFILE)
                     MOVE    -1              TO EDCLD-DETALLESL(1)
                     MOVE     1              TO WKS-FIN-ARCH
                     SET SEND-DATAONLY-ALARM TO TRUE
           END-EVALUATE.
       111-READ-NEXT-E. EXIT.



      ****************************************************************
      *          START READ PREV Y READ-PREV VALIDACIONES            *
      ****************************************************************

       READ-ANTERIORES SECTION.
           MOVE WKS-CIF-ULTIMO     TO EDMA-CODIGO-CLIENTE
           MOVE       ZEROS        TO WKS-FIN-ARCH-ATRAS
           MOVE       ZEROS        TO WKS-FIN-ARCH
           MOVE       ZEROS        TO WKS-FIN-DATOS-ATRAS
           MOVE         -1         TO EDCLD-DETALLESL(1)
           MOVE          1         TO ATRAS
           PERFORM START-READ-NEXT.
       READ-ANTERIORES-E. EXIT.

      * 112-READ-PREV SECTION.
      *     MOVE 'YA CASI TERMINAMOS EL F6 ....'
      *                                   TO EDCLD-MENSAJEO.
      * 112-READ-PREV-E. EXIT.

      ******************************************************************
      *     FUNCIONES O CODIGO QUE SE REPITE SEGUIDAMENTE EN EL PROG   *
      ******************************************************************
       ENVIA-MAPA SECTION.
           PERFORM HORA-FECHA-SISTEMA
           EVALUATE TRUE
               WHEN SEND-ALTERNATE
                    EXEC CICS
                         SEND MAP('EDCLD')
                         MAPSET('EDCLD')
                         FROM (EDCLDO)
                         ALTERNATE
                         FREEKB
                         ERASE
                         CURSOR
                    END-EXEC
               WHEN SEND-COMPLETO
                    EXEC CICS
                         SEND MAP('EDCLD')
                         MAPSET('EDCLD')
                         FROM (EDCLDO)
                         FREEKB
                         CURSOR
                    END-EXEC
               WHEN SEND-ERASE
                    EXEC CICS
                         SEND MAP('EDCLD')
                         MAPSET('EDCLD')
                         FROM(EDCLDO)
                         ERASE
                         FREEKB
                         CURSOR
                    END-EXEC
               WHEN SEND-DATAONLY
                    EXEC CICS
                         SEND MAP('EDCLD')
                         MAPSET('EDCLD')
                         DATAONLY
                         FREEKB
                         CURSOR
                    END-EXEC
               WHEN SEND-DATAONLY-ALARM
                    EXEC CICS
                         SEND MAP('EDCLD')
                         MAPSET('EDCLD')
                         FROM(EDCLDO)
                         DATAONLY
                         FREEKB
                         ALARM
                         CURSOR
                    END-EXEC
           END-EVALUATE.
       ENVIA-MAPA-E. EXIT.

       HORA-FECHA-SISTEMA SECTION.
           EXEC CICS ASKTIME ABSTIME(WKS-TIEMPO) END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WKS-TIEMPO)
                     DDMMYYYY(FECHA-SYS)
                     TIME(WKS-HORA)
           END-EXEC
           MOVE EIBTRMID                TO EDCLD-TERMINALO
           MOVE FECHA-SYS               TO EDCLD-FECHA-SO
           MOVE CORRESPONDING WKS-HORA  TO WKS-HORA-EDIT
           MOVE WKS-HORA-EDIT           TO EDCLD-HORA-SO.
       HORA-FECHA-SISTEMA-E. EXIT.

       LIMPIA-ARREGLO SECTION.
           PERFORM VARYING INDEX-1 FROM 1 BY 1 UNTIL INDEX-1 > 18
                   MOVE SPACES       TO EDCLD-DETALLESO(INDEX-1)
                   MOVE ATTR-NEUTRAL TO EDCLD-DETALLESO(INDEX-1)
           END-PERFORM
           MOVE ZEROS                TO INDEX-1
           MOVE SPACES               TO EDCLD-MENSAJEO.
       LIMPIA-ARREGLO-E. EXIT.

       LLENA-ARREGLO-DISPLAY SECTION.
           IF EDMA-NUMERO-CUENTA NOT = 0
      * --------->     MOVIMIENTO DE DATOS AL ARREGLO(INDEX)
               MOVE EDMA-NUMERO-CUENTA   TO EDCLD-NUM-CTAO   (INDEX-1)
               MOVE EDMA-CODIGO-CLIENTE  TO EDCLD-LLAVESO    (INDEX-1)
               MOVE EDMA-FECHA-APERTURA-CUENTA
                                         TO EDCLD-FECH-APERTO(INDEX-1)
               MOVE EDMA-SALDO-CUENTA    TO EDCLD-SALDOSO    (INDEX-1)
               MOVE EDMA-FECHA-ULTIMO-MOVMNTO
                                         TO EDCLD-FECH-MVNTO (INDEX-1)

      *<--------------- VALIDACION CUENTAS ACTIVAS/ELIMINADAS -------->
               IF EDMA-MARCA-ELIMINADO = "D" OR "d"
                  MOVE ATTR-RED          TO EDCLD-DETALLESC  (INDEX-1)
                  MOVE "CTA INACTIVA"    TO EDCLD-ESTADO-CTAO(INDEX-1)
               ELSE
                  MOVE ATTR-NEUTRAL      TO EDCLD-DETALLESC  (INDEX-1)
                  MOVE "CTA ACTIVA"      TO EDCLD-ESTADO-CTAO(INDEX-1)
               END-IF
      * <---------------  DATOS QUE LLEVAMOS A LA COMMAREA ----------->
               IF INDEX-1 = 1
                   MOVE EDMA-CODIGO-CLIENTE
                                         TO WKS-CIF-ULTIMO
                   MOVE EDMA-NUMERO-CUENTA
                                         TO WKS-LLAVE-CU
               ELSE
                   MOVE EDMA-CODIGO-CLIENTE
                                         TO WKS-CIF-PRIMERO
                   MOVE EDMA-NUMERO-CUENTA
                                         TO WKS-LLAVE-CP
                   MOVE EDMA-NUMERO-CUENTA
                                         TO EDCLD-MENSAJEO
               END-IF
      * <------------------------------------------------------------->
           ELSE
               SUBTRACT 1 FROM INDEX-1
           END-IF.
       LLENA-ARREGLO-DISPLAY-E. EXIT.

       REGRESA-CONTROL-A-TRANS SECTION.
           EXEC CICS
                     RETURN TRANSID('EDCD')
                     COMMAREA(WKS-COMMAREAS)
                     LENGTH(LENGTH OF WKS-COMMAREAS)
                     NOHANDLE
           END-EXEC.
       REGRESA-CONTROL-A-TRANS-E. EXIT.