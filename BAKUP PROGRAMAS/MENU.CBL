      ******************************************************************
      * FECHA       : 12/06/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS (EEDR)                *
      * APLICACION  : EDUCACION SEMILLERO                              *
      * PROGRAMA    : ED3D1ML3                                         *
      * TIPO        : LINEA CON MAPA                                   *
      * DESCRIPCION : PROGRAMA ENCARGADO DE LA APERTURA DE CUENTAS DE  *
      *             : AHORRO DE FORMA AUTOMATICA                       *
      * ARCHIVOS    : EDM1CL                      (VSAM KSDS)          *
      * ACCION (ES) : DETALLE CLIENTE DE CUENTAS DE AHORRO             *
      * PROGRAMA(S) : XCTRL ED3D1QL3                                   *
      * CANAL       : PANTALLA ADMINISTRATIVA                          *
      * INSTALADO   : DD/MM/AAAA                                       *
      ******************************************************************
      *               D E T A L L E   D E   C U E N T A S
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     ED3D1QLQ.
       AUTHOR.                         DANIEL RAMIREZ.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *               C A M P O S     D E     T R A B A J O            *
      ******************************************************************
       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-NOMBRE-PROGRAMA     PIC X(08)  VALUE "ED3D1QLQ".
          02 WKS-TEL-EDIT.
             03 WKS-PRIMER           PIC 9(04)  VALUE ZEROS.
             03 FILLER               PIC X(01)  VALUE "-".
             03 WKS-SEGUNDO          PIC 9(04)  VALUE ZEROS.
          02 WKS-FIN                 PIC X(60)  VALUE
                'TRANSACCION NO SOPORTADA INVIDUALMENTE,'.
          02 WKS-INDEX.
             03 INDEX-1              PIC 9(02)  VALUE ZEROS.
             03 INDEX-2              PIC 9(02)  VALUE ZEROS.
      ******************************************************************
      *      DEFINICION DE COMMAREA PARA COMUNICACION ENTRE PROGRAMAS  *
      ******************************************************************
       01 WKS-COMMAREAS.
          02 WKS-CUENTA-CONSULTA     PIC 9(10)  VALUE ZEROS.
          02 WKS-COD-CLIENTE         PIC 9(08)  VALUE ZEROS.
          02 WKS-ESTADO-CTA          PIC X(02)  VALUE ZEROS.
      *                  C O N T R O L A D O R E S
          02 WKS-TIPO-BUSQUEDA       PIC 9(01)  VALUE ZEROS.
          02 WKS-FIN-ARCH            PIC 9(01)  VALUE ZEROS.
          02 WKS-FIN-DATOS           PIC 9(01)  VALUE ZEROS.
          02 WKS-FIN-DATOS-ATRAS     PIC 9(01)  VALUE ZEROS.
          02 WKS-FIN-ARCH-ATRAS      PIC 9(01)  VALUE ZEROS.
          02 ATRAS                   PIC 9(01)  VALUE ZEROS.
      *  C O N T A D O R E S    Y    L L A V E S    T E M P O R A L E S
          02 WKS-LLAVE-PRIMERA       PIC 9(10)  VALUE ZEROS.
          02 WKS-LLAVE-ULTIMA        PIC 9(10)  VALUE ZEROS.
          02 WKS-PAG-CONT            PIC 9(02)  VALUE ZEROS.
          02 WKS-PUNTERO             PIC 9(02)  VALUE ZEROS.
          02 MENU-CTRL               PIC 9(01)  VALUE ZEROS.
      *       D A T O S   P R O G R A M A   D E   A D I C I O N
          02 WKS-FECHA-GEN           PIC 9(08)  VALUE ZEROS.
          02 WKS-DATOS-NUEVOS-OK     PIC 9(01)  VALUE ZEROS.
          02 WKS-CUENTA-NUEVA        PIC 9(10)  VALUE ZEROS.
          02 WKS-CIF-NUEVO           PIC 9(08)  VALUE ZEROS.
          02 WKS-APORTE              PIC 9(11)V99
                                                VALUE ZEROS.
      *     D A T O S   P R O G R A M A   D E   E L I M I N A C I O N
          02 WKS-DELETE-OK           PIC 9(01)  VALUE ZEROS.
      *    D A T O S   P R O G R A M A   D E   A C TU A L I Z A C I O N
      *    FECHAS EN FORMATO DDMMYYYY
          02 WKS-GRUPO-MOV.
             03 DIA                  PIC 9(02)  VALUE ZEROS.
             03 MES                  PIC 9(02)  VALUE ZEROS.
             03 ANIO                 PIC 9(04)  VALUE ZEROS.
          02 WKS-FECH-NUM REDEFINES WKS-GRUPO-MOV.
             03 WKS-FECH-MVNTO       PIC 9(08).

          02 WKS-GRUP-APERT.
             03 DIA                  PIC 9(02)  VALUE ZEROS.
             03 MES                  PIC 9(02)  VALUE ZEROS.
             03 ANIO                 PIC 9(04)  VALUE ZEROS.
          02 WKS-APERT-NUM REDEFINES WKS-GRUP-APERT.
             03 WKS-FECH-APRTA       PIC 9(08).
      *   FECHAS REFORMATEADAS YYYYMMDD
          02 WKS-FECHAS-INTEGER.
             03 WKS-MOVIMIENTO       PIC 9(08)  VALUE ZEROS.
             03 WKS-APERTURAS        PIC 9(08)  VALUE ZEROS.
          02 WKS-SWITCHES.
             03 SWITCH-CIF           PIC 9(01)  VALUE ZEROS.
             03 SWITCH-OK            PIC 9(01)  VALUE ZEROS.
             03 SWITCH-0             PIC 9(01)  VALUE ZEROS.
             03 SWITCH-1             PIC 9(01)  VALUE ZEROS.
             03 SWITCH-2             PIC 9(01)  VALUE ZEROS.
             03 SWITCH-3             PIC 9(01)  VALUE ZEROS.
             03 SWITCH-4             PIC 9(01)  VALUE ZEROS.
             03 SWITCH-5             PIC 9(01)  VALUE ZEROS.
          02 WKS-FECHA-OK            PIC S9(06) VALUE ZEROS.
          02 WKS-SALDOS-UPDATE       PIC S9(09)V99
                                                VALUE ZEROS.
      ******************************************************************
      *                S E C C I O N    D E    C O P Y S               *
      ******************************************************************
      * --- MAPAS Y CARACTERISTICAS
       COPY EDQLQ.
       COPY EDMACL.
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
           MOVE    DFHCOMMAREA    TO WKS-COMMAREAS
           PERFORM 100-VALIDA-LLAMADO
           PERFORM REGRESA-CONTROL-A-TRANS.
       000-PRINCIPAL-E. EXIT.

       100-VALIDA-LLAMADO SECTION.
           IF EIBTRNID NOT = "EDQQ"
              PERFORM 120-PROCESA-SOLICITUD
              PERFORM ENVIA-MAPA

           ELSE
              PERFORM 110-VALIDA-TECLAS
              PERFORM ENVIA-MAPA
           END-IF.
       100-VALIDA-LLAMADO-E. EXIT.


       110-VALIDA-TECLAS SECTION.
           EVALUATE TRUE
           WHEN EIBCALEN = ZERO
                EXEC CICS SEND TEXT FROM(WKS-FIN) ERASE END-EXEC
                     EXEC CICS RETURN END-EXEC
           WHEN EIBAID = DFHPF10

                EXEC CICS
                     XCTL PROGRAM('ED3D1QL3')
                     COMMAREA(WKS-COMMAREAS)
                     LENGTH(LENGTH OF WKS-COMMAREAS)
                END-EXEC

           WHEN OTHER
                MOVE ' TECLA INVALIDA, PARA SALIR USE F10 '
                                              TO EDQLQ-MENSAJE-OPCIONAO
                MOVE         -1               TO EDQLQ-CTA-LINKEDL
                MOVE          1               TO INDEX-2
           END-EVALUATE.
       110-VALIDA-TECLAS-E. EXIT.

       120-PROCESA-SOLICITUD SECTION.
           MOVE WKS-COD-CLIENTE TO EDMC-LLAVE
           EXEC CICS READ
                DATASET ('EDM1CL')
                INTO    (REG-EDMACL)
                RIDFLD  (EDMC-LLAVE)
                NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     IF EDMC-MARCA-ELIMINADO = "D" OR "d"
                        MOVE ATTR-RED          TO EDQLQ-ESTADO-CIFC
                        MOVE "INACTIVO"        TO EDQLQ-ESTADO-CIFO
                     ELSE
                        MOVE "ACTIVO"          TO EDQLQ-ESTADO-CIFO
                     END-IF
                     IF WKS-ESTADO-CTA NOT EQUAL SPACES
                        MOVE ATTR-RED          TO EDQLQ-ESTADO-CTAC
                        MOVE "ELIMINADA"       TO EDQLQ-ESTADO-CTAO
                     ELSE
                        MOVE "VIGENTE"         TO EDQLQ-ESTADO-CTAO
                     END-IF
                     MOVE WKS-CUENTA-CONSULTA  TO EDQLQ-CTA-LINKEDO
                     MOVE EDMC-CODIGO-CLIENTE  TO EDQLQ-CIF-LINKEDO
                     MOVE EDMC-NOMBRE-CLIENTE  TO EDQLQ-NOMBRESO
                     MOVE EDMC-FECHA-NAC-O-CONSTITUC
                                               TO EDQLQ-FECHA-NACO
                     MOVE EDMC-NUMERO-TELEFONO TO EDQLQ-CELULARO
                     MOVE EDMC-FECHA-CREACION  TO EDQLQ-FECHA-INGRESOO

                WHEN DFHRESP(NOTFND)
                     MOVE 'CIF NO ENCONTRADO EN MAESTRO DE CLIENTES'
                                               TO EDQLQ-MENSAJE-OPCIONAO
                WHEN DFHRESP(NOTOPEN)
                     MOVE 'MAESTRO DE CLIENTES ESTA CERRADO EDM1CL'
                                               TO EDQLQ-MENSAJE-OPCIONAO
                WHEN OTHER
                     MOVE 'ERROR INESPERADO EN ARCHIVO MAESTRO EDM1CL'
                                               TO EDQLQ-MENSAJE-OPCIONAO

           END-EVALUATE.
       120-PROCESA-SOLICITUD-E. EXIT.


      ******************************************************************
      *     FUNCIONES O CODIGO QUE SE REPITE SEGUIDAMENTE EN EL PROG   *
      ******************************************************************
       ENVIA-MAPA SECTION.
           IF INDEX-2 NOT EQUAL 1
              PERFORM CAPA-UNO
              MOVE -1 TO EDQLQ-CTA-LINKEDL
              EXEC CICS
                   SEND MAP('EDQLQ')
                   MAPSET('EDQLQ')
                   FROM (EDQLQO)
                   FREEKB
                   CURSOR
              END-EXEC
           ELSE
              EXEC CICS
                   SEND MAP('EDQLQ')
                   MAPSET('EDQLQ')
                   FROM (EDQLQO)
                   FREEKB
                   CURSOR
              END-EXEC
           END-IF.
       ENVIA-MAPA-E. EXIT.

       CAPA-UNO SECTION.
           PERFORM VARYING INDEX-1 FROM 1 BY 1 UNTIL INDEX-1 > 15
                  MOVE SPACES  TO EDQLQ-FONDOO(INDEX-1)
           END-PERFORM
           MOVE SPACES TO EDQLQ-FONDO2O
           EXEC CICS
                SEND MAP('EDQLQA')
                MAPSET('EDQLQ')
                FROM (EDQLQAO)
                DATAONLY
                FREEKB
                CURSOR
           END-EXEC.
       CAPA-UNO-E. EXIT.

       REGRESA-CONTROL-A-TRANS SECTION.
           EXEC CICS
                     RETURN TRANSID('EDQQ')
                     COMMAREA(WKS-COMMAREAS)
                     LENGTH(LENGTH OF WKS-COMMAREAS)
                     NOHANDLE
           END-EXEC.
       REGRESA-CONTROL-A-TRANS-E. EXIT.