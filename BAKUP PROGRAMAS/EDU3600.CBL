      ******************************************************************
      * FECHA       : 18/05/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS (EEDR)                *
      * APLICACION  : EDUCACION SEMILLERO                              *
      * PROGRAMA    : ED3D1NL3                                         *
      * TIPO        : LINEA CON MAPA                                   *
      * DESCRIPCION : VERIFICA SI UNA CUENTA EXISTE DENTRO DE LOS      *
      *             : REGISTROS EL BANCO, VALIDA SU ESTRUCTURA         *
      * ARCHIVOS    : MOMAES                      (VSAM KSDS)          *
      *             : CFCNAT                      (VSAM KSDS)          *
      *             : MOTGEN                      (VSAM KSDS)          *
      *             : CFTGEN                      (VSAM KSDS)          *
      *             : EDBIDA                      (VSAM ESDS)          *
      * CANAL       : PANTALLA ADMINISTRATIVA                          *
      * INSTALADO   : DD/MM/AAAA                                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     ED3D1NL3.
       AUTHOR.                         DANIEL RAMIREZ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-NOMBRE-PROGRAMA      PIC X(08) VALUE "ED3D1NL3".
          02 WKS-FECHAS-EDIT          PIC X(10) VALUE SPACES.
          02 RESPONSE-CODE            PIC S9(8) COMP.
          02 WKS-CONTADOR-LONG        PIC 9(02) VALUE ZEROS.
          02 SEND-FLAG                PIC X(01) VALUE SPACES.
             88 SEND-ERASE                      VALUE '1'.
             88 SEND-DATAONLY                   VALUE '2'.
             88 SEND-DATAONLY-ALARM             VALUE '3'.
          02 WKS-MSG-BITACORA.
             03 MOMAES-OK             PIC 9(01) VALUE ZEROS.
             03 CFTGEN-OK             PIC 9(01) VALUE ZEROS.
             03 MOTGEN-OK             PIC 9(01) VALUE ZEROS.
             03 CFCNAT-OK             PIC 9(01) VALUE ZEROS.
          02 WKS-COMMAREAS.
             03 WKS-COMA-CUENTA-MONE  PIC X(10)    VALUE SPACES.
          02 WKS-FECHA-FORMATEADA.
             03 WKS-DIA               PIC 9(02) VALUE ZEROS.
             03 WKS-MES               PIC 9(02) VALUE ZEROS.
             03 WKS-ANIO              PIC 9(04) VALUE ZEROS.
          02 WKS-FECHA-FORMATEADA2.
             03 WKS-DIA-2             PIC 9(02) VALUE ZEROS.
             03 WKS-MES-2             PIC 9(02) VALUE ZEROS.
             03 WKS-ANIO-2            PIC 9(04) VALUE ZEROS.
          02 WKS-RBA-EDBIDA           PIC S9(04) COMP VALUE +000.
          02 WKS-FIN                  PIC X(24) VALUE
                'GOODBYE SEE YOU LATER :)'.
      ******************************************************************
      *                S E C C I O N    D E    C O P Y S               *
      ******************************************************************
      * --- MAPAS Y CARACTERISTICAS
       COPY EDNL3.
       COPY CFNM.
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY ATTR.
      * --- ARCHIVOS Y RECURSOS
       COPY MOMAES.
       COPY CFCNAT.
       COPY MOTGEN.
       COPY CFTGEN.
       COPY EDBIDA.
       PROCEDURE DIVISION.
       000-MAIN SECTION.
           PERFORM 100-MENU-INICIO
           PERFORM REGRESA-CONTROL-A-TRANS.
       000-MAIN-E. EXIT.

       100-MENU-INICIO SECTION.
           EVALUATE TRUE
                  WHEN EIBCALEN = ZERO
                       SET SEND-ERASE TO TRUE
                       PERFORM ENVIA-MAPA
                  WHEN EIBAID = DFHENTER
                       PERFORM 200-PROCESA-SOLICITUD
                  WHEN EIBAID = DFHPF3
                       SET SEND-ERASE TO TRUE
                       PERFORM ENVIA-MAPA
                  WHEN EIBAID =  DFHPF10
                       PERFORM REGRESA-CONTROL-A-CICS
                  WHEN OTHER
                       MOVE 'TECLA INVALIDA' TO EDNL3-MENSAJE-DYSO
                       SET SEND-DATAONLY TO TRUE
                       PERFORM ENVIA-MAPA
           END-EVALUATE.
       100-MENU-INICIO-E. EXIT.

       200-PROCESA-SOLICITUD SECTION.
           PERFORM 210-RECIBE-MAPA
           PERFORM 220-VALIDA-DATOS-ENTRADA.
       200-PROCESA-SOLICITUD-E. EXIT.

       210-RECIBE-MAPA SECTION.
           MOVE EDNL3-CUENTA-LLAVE-DYSI TO WKS-COMMAREAS
           EXEC CICS
                RECEIVE MAP('EDNL3')
                MAPSET('EDNL3')
                INTO(EDNL3I)
                NOHANDLE
           END-EXEC.
       210-RECIBE-MAPA-E. EXIT.

       220-VALIDA-DATOS-ENTRADA SECTION.
           IF EDNL3-CUENTA-LLAVE-DYSI IS NUMERIC
               IF EDNL3-CUENTA-LLAVE-DYSI IS NOT EQUAL ZEROS
                  PERFORM 300-BUSCA-DATOS-CONSULTA
                  PERFORM ESCRIBE-BITACORA-TRANS
                  SET SEND-DATAONLY TO TRUE
                  PERFORM ENVIA-MAPA
               ELSE
                  SET SEND-ERASE TO TRUE
                  PERFORM ENVIA-MAPA
                  MOVE
                  "LA CUENTA TIENE QUE SER DISTINTA A CEROS" TO
                  EDNL3-MENSAJE-DYSO
                  PERFORM ESCRIBE-BITACORA-TRANS
                  SET SEND-DATAONLY-ALARM TO TRUE
                  PERFORM ENVIA-MAPA

               END-IF
           ELSE
              SET SEND-ERASE TO TRUE
              PERFORM ENVIA-MAPA
              MOVE
              "LA CUENTA TIENE QUE SER NUMERICA DE 10 DIGITOS" TO
              EDNL3-MENSAJE-DYSO
              PERFORM ESCRIBE-BITACORA-TRANS
              SET SEND-DATAONLY-ALARM TO TRUE
              PERFORM ENVIA-MAPA
           END-IF.
       220-VALIDA-DATOS-ENTRADA-E. EXIT.

       300-BUSCA-DATOS-CONSULTA SECTION.
      * ----------> PRIMERO BUSCAMOS LOS DATOS DE LA CUENTA EN MOMAES
      * ----------> CON ESTO ARMAMOS LAS DEMÁS LLAVES.
           MOVE EDNL3-CUENTA-LLAVE-DYSI TO MOMA-LLAVE
           EXEC CICS READ
                DATASET ('MOMAES')
                INTO    (REG-MOMAES)
                RIDFLD  (MOMA-LLAVE)
                NOHANDLE
           END-EXEC

           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     MOVE SPACES                TO EDNL3-MENSAJE-DYSO
                     MOVE MOMA-FECHA-APERTURA   TO EDNL3-FECHA-APO
                     MOVE 1                     TO MOMAES-OK
                     PERFORM 320-BUSCA-DATOS-CUENTA
                     PERFORM 320-BUSCA-DATOS-CLIENTE
                WHEN DFHRESP(NOTFND)
                     SET SEND-ERASE TO TRUE
                     PERFORM ENVIA-MAPA
                     MOVE "LA CUENTA NO FUE ENCONTRADA/NO EXISTE" TO
                     EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE
                     PERFORM ENVIA-MAPA
                     PERFORM ESCRIBE-BITACORA-TRANS
                     PERFORM REGRESA-CONTROL-A-TRANS
                WHEN DFHRESP(NOTOPEN)
                     SET SEND-ERASE TO TRUE
                     PERFORM ENVIA-MAPA
                     MOVE "EL ARCHIVO MOMAES ESTA CERRADO" TO
                           EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE
                     PERFORM ENVIA-MAPA
                     PERFORM ESCRIBE-BITACORA-TRANS
                     PERFORM REGRESA-CONTROL-A-TRANS
                WHEN OTHER
                     SET SEND-ERASE TO TRUE
                     PERFORM ENVIA-MAPA
                     MOVE
                     "OCURRIO UN ERROR DESCONOCIDO AL CONSULTAR CTA" TO
                     EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE
                     PERFORM ENVIA-MAPA
                     PERFORM ESCRIBE-BITACORA-TRANS
                     PERFORM REGRESA-CONTROL-A-TRANS
           END-EVALUATE.
       300-BUSCA-DATOS-CONSULTA-E. EXIT.

       320-BUSCA-DATOS-CUENTA SECTION.
      ***------------------------
           MOVE "CFT"                  TO CFTB-TABLA
           MOVE 006                    TO CFTB-CODIGO-TABLA
           MOVE MOMA-AGENCIA           TO CFTB-CORRELATIVO
      *---- NOMBRE AGENCIA
           EXEC CICS READ
                DATASET   ('CFTGEN')
                INTO      (CFTGEN)
                RIDFLD    (CFTB-LLAVE)
                NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     MOVE CFTB-DESCRIPCION(1:47) TO
                          EDNL3-AGE-APERT-DYSO
                     MOVE 1                     TO CFTGEN-OK
                WHEN DFHRESP(NOTFND)
                     MOVE "- AGENCIA NO ENCONTRADA -" TO
                     EDNL3-AGE-APERT-DYSO
                WHEN DFHRESP(NOTOPEN)
                     MOVE "EL ARCHIVO CFTGEN ESTA CERRADO" TO
                     EDNL3-AGE-APERT-DYSO
                WHEN OTHER
                     MOVE
                     "OCURRIO UN ERROR DESCONOCIDO ARCHIVO CFTGEN" TO
                     EDNL3-AGE-APERT-DYSO
           END-EVALUATE
      ***------------------------
           MOVE "MOT"                  TO MOTC-TABLA
           MOVE 003                    TO MOTC-CODIGO-TABLA
           MOVE MOMA-SITUACION-CUENTA  TO MOTC-CORRELATIVO
      *---- SITUACION CUENTA
           EXEC CICS READ
                DATASET ('MOTGEN')
                INTO    (REG-MOTGEN)
                RIDFLD  (MOTC-LLAVE)
                NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     MOVE MOMA-SITUACION-CUENTA  TO EDNL3-COD-SITUACINOO
                     MOVE MOTC-DESCRIPCION(1:11) TO
                          EDNL3-SIT-CUENTA-DYSO
                     MOVE 1                      TO MOTGEN-OK
                WHEN DFHRESP(NOTFND)
                     MOVE "-DESCONOCIDA-" TO EDNL3-SIT-CUENTA-DYSO
                WHEN DFHRESP(NOTOPEN)
                     MOVE "EL ARCHIVO CFTGEN ESTA CERRADO" TO
                           EDNL3-SIT-CUENTA-DYSO
                WHEN OTHER
                     MOVE
                     "OCURRIO UN ERROR DESCONOCIDO ARCHIVO MOTGEN" TO
                     EDNL3-SIT-CUENTA-DYSO
           END-EVALUATE.
       320-BUSCA-DATOS-CUENTA-E. EXIT.

       320-BUSCA-DATOS-CLIENTE SECTION.
           MOVE MOMA-CODIGO-UNIVERSAL TO CFCN-CODIGO-CLIENTE
           EXEC CICS READ
                DATASET ('CFCNAT')
                INTO    (CFCNAT)
                RIDFLD  (CFCN-CODIGO-CLIENTE)
                NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     MOVE CFCN-CODIGO-CLIENTE  TO EDNL3-COD-CLIENTE-DYSO
                     MOVE CFCN-DPI-CORREL       TO EDNL3-DPI-CORRELO
                     MOVE CFCN-DPI-VERIFICADOR  TO
                          EDNL3-DPI-VERIFICADORO
                     MOVE CFCN-DPI-DEPARTAMENTO TO
                          EDNL3-DPI-DEPARTAMENTOO
                     MOVE CFCN-DPI-MUNICIPIO    TO EDNL3-DPI-MUNICIPIOO
                     MOVE CFCN-FECHA-NACIMIENTO TO EDNL3-FECHA-NACO
                     PERFORM 330-NOMBRES-FORMATEADOS
                     MOVE 1                     TO CFCNAT-OK
                WHEN DFHRESP(NOTFND)
                     MOVE "CIF NO ENCONTRADO EN EL ARCHIVO CFCNAT" TO
                           EDNL3-NOMBRE-EDITADOO
                     SET SEND-DATAONLY-ALARM TO TRUE
                     PERFORM ENVIA-MAPA
                     PERFORM ESCRIBE-BITACORA-TRANS
                     PERFORM REGRESA-CONTROL-A-TRANS
                WHEN DFHRESP(NOTOPEN)
                     MOVE "EL ARCHIVO CFCNAT ESTA CERRADO" TO
                          EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE
                     PERFORM ENVIA-MAPA
                     PERFORM ESCRIBE-BITACORA-TRANS
                     PERFORM REGRESA-CONTROL-A-TRANS
                WHEN OTHER
                     MOVE
                     "OCURRIO UN ERROR DESCONOCIDO AL CONSULTAR CIF" TO
                     EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE
                     PERFORM ENVIA-MAPA
                     PERFORM ESCRIBE-BITACORA-TRANS
                     PERFORM REGRESA-CONTROL-A-TRANS
           END-EVALUATE.
       320-BUSCA-DATOS-CLIENTE-E. EXIT.

       330-NOMBRES-FORMATEADOS SECTION.
            MOVE SPACES              TO CFNM-CAMPO-RETORNO
            MOVE 'N'                 TO CFNM-ORDENAMIENTO
            MOVE 50                  TO CFNM-RANGO-LONGITUD
            MOVE 3                   TO CFNM-RANGO-APELLIDOS
            MOVE CFCN-NOMBRE-CLIENTE TO CFNM-NOMBRE-COMPLETO
            CALL 'RGGD2QL6' USING DFHEIBLK CFNM
            MOVE CFNM-CAMPO-RETORNO  TO EDNL3-NOMBRE-EDITADOO.
       330-NOMBRES-FORMATEADOS-E. EXIT.

       ENVIA-MAPA SECTION.
           MOVE -1 TO EDNL3-CUENTA-LLAVE-DYSL
           EVALUATE TRUE
               WHEN SEND-ERASE
                    EXEC CICS
                         SEND MAP('EDNL3')
                         MAPSET('EDNL3')
                         FROM(EDNL3O)
                         ERASE
                         CURSOR
                    END-EXEC
               WHEN SEND-DATAONLY
                    EXEC CICS
                         SEND MAP('EDNL3')
                         MAPSET('EDNL3')
                         DATAONLY
                         CURSOR
                    END-EXEC
               WHEN SEND-DATAONLY-ALARM
                    EXEC CICS
                         SEND MAP('EDNL3')
                         MAPSET('EDNL3')
                         FROM(EDNL3O)
                         DATAONLY
                         FREEKB
                         ALARM
                         CURSOR
                    END-EXEC
           END-EVALUATE.
       ENVIA-MAPA-E. EXIT.

       ESCRIBE-BITACORA-TRANS  SECTION.
           INITIALIZE REG-EDBIDA
           MOVE EDNL3-CUENTA-LLAVE-DYSI TO EDBD-CUENTA-BUSCADA
           MOVE EIBTRMID                TO EDBD-TERMINAL-USUARIO
           MOVE EIBTIME                 TO EDBD-HORA-BUSQUEDA
           MOVE EIBDATE                 TO EDBD-FECHA-BUSQUEDA

           IF (MOMAES-OK = 1) AND (CFTGEN-OK = 1) AND
              (MOTGEN-OK = 1) AND (CFCNAT-OK = 1)
              MOVE "BUSQUEDA EXITOSA DATOS ENCONTRADOS" TO
              EDBD-RESULTADO-BUSQ
           ELSE
              MOVE "BUSQUEDA CON ERRORES PARA CUENTA SOLICITADA" TO
              EDBD-RESULTADO-BUSQ
           END-IF

           EXEC CICS WRITE
                DATASET('EDBIDA')
                FROM(REG-EDBIDA)
                RIDFLD(WKS-RBA-EDBIDA)
                RBA
                NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)
                     CONTINUE
                WHEN DFHRESP(NOTOPEN)
                     MOVE
                     "EL ARCHIVO DE TRANSACCION EDBIDA ESTA CERRADO "
                     TO EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE
                WHEN DFHRESP(DISABLED)
                     MOVE
                     "EL ARCHIVO EDBIDA NO ESTA DISPONIBLE NOTIFICAR"
                     TO EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE

                WHEN OTHER
                     MOVE
                     "OCURRIO UN ERROR DESCONOCIDO ARCHIVO EDBIDA" TO
                     EDNL3-MENSAJE-DYSO
                     SET SEND-DATAONLY-ALARM TO TRUE
                     PERFORM ENVIA-MAPA
                     PERFORM REGRESA-CONTROL-A-TRANS
           END-EVALUATE.
       ESCRIBE-BITACORA-TRANS-E. EXIT.

       REGRESA-CONTROL-A-TRANS SECTION.
           EXEC CICS
                RETURN TRANSID('EDN3')
                COMMAREA(WKS-COMMAREAS)
                LENGTH(LENGTH OF WKS-COMMAREAS)
                NOHANDLE
           END-EXEC.
       REGRESA-CONTROL-A-TRANS-E. EXIT.

       REGRESA-CONTROL-A-CICS SECTION.
               EXEC CICS SEND TEXT FROM(WKS-FIN) ERASE END-EXEC
               EXEC CICS RETURN END-EXEC.
       REGRESA-CONTROL-A-CICS-E. EXIT.

      * ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
      * NOMBRE         : EDBIDA,'EDUC.SEMI.BITACORA.CONSULTA'          *
      * DESCRIPCION    : TABLAS GENERALES                              *
      * TIPO, LONGITUD : ESDS, 100 BYTES                               *
      * FECHA CREACION : 22_MAY_2023                                  *
      * ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
       01 REG-EDBIDA.
          02 EDBD-CUENTA-BUSCADA    PIC 9(10).
          02 EDBD-TERMINAL-USUARIO  PIC X(04).
          02 EDBD-HORA-BUSQUEDA     PIC X(06).
          02 EDBD-FECHA-BUSQUEDA    PIC X(08).
          02 EDBD-RESULTADO-BUSQ    PIC X(45).
          02 EDBD-FILLER            PIC X(27).
      * ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *