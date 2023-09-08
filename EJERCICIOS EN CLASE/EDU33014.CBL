      ******************************************************************
      * FECHA       : 18/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33014                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : PROGRAMA RECIBE NOMBRES EN SYSIN CON FORMATO POCO*
      *             : LEGIBLE, PARA GENERAR UNA PLANTILLA DE CHEQUE CON*
      *             : EL NOMBRE FORMATEADO, TOMA LA FECHA DEL SISTEMA  *
      *             : PARA FECHAR LA PLANTILLA DE CHEQUE               *
      * ARCHIVOS    : NO APLICA                                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     EDU33014.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * <---------     VARIABLES DE ENTRADA DE DATOS    --------->
       01 WKS-NOMBRE-ENTRADA.
          02 WKS-PRIMER-APELLIDO   PIC X(15)    VALUE SPACES.
          02 WKS-SEGUNDO-APELLIDO  PIC X(15)    VALUE SPACES.
          02 WKS-CASADA-APELLIDO   PIC X(15)    VALUE SPACES.
          02 WKS-PRIMER-NOMBRE     PIC X(15)    VALUE SPACES.
          02 WKS-SEGUNDO-NOMBRE    PIC X(15)    VALUE SPACES.
       01    REDEFINES WKS-NOMBRE-ENTRADA.
          02 WKS-STOP              PIC X(03).
             88 WKS-STOP-PROGRAM                VALUE "FIN".
          02 FILLER                PIC X(72).
       01 WKS-GUIONES              PIC X(75)    VALUE ALL "=".
      * <--------- VARIABLES DE TRABAJO Y CONTROLADORES --------->
       01 WKS-NOMBRES-EDITADOS     PIC X(30)    VALUE SPACES.
       01 WKS-APELLIDOS-EDIT       PIC X(30)    VALUE SPACES.
       01 WKS-NOMBRE-FORMATEADO    PIC X(75)    VALUE SPACES.
       01 WKS-FECHA-EDIT           PIC 99/99/99 VALUE ZEROS.
       01 WKS-CONTADOR-REG         PIC 9(02)    VALUE ZEROS.
       01 WKS-CONTADOR-CHEQ        PIC 9(02)    VALUE ZEROS.
       01 WKS-CONTADOR-ERROR       PIC 9(02)    VALUE ZEROS.

       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 200-RECIBE-DATOS-SYSIN
           PERFORM 400-ESTADISTICAS
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

       200-RECIBE-DATOS-SYSIN SECTION.
           ACCEPT WKS-NOMBRE-ENTRADA FROM SYSIN
           PERFORM UNTIL WKS-STOP-PROGRAM
                   ADD 1 TO WKS-CONTADOR-REG
                   IF WKS-NOMBRE-ENTRADA NOT EQUAL SPACES
                      IF WKS-PRIMER-APELLIDO NOT EQUAL SPACES AND
                         WKS-PRIMER-NOMBRE NOT EQUAL SPACES
                         IF WKS-CASADA-APELLIDO EQUAL SPACES
                            PERFORM 300-PROCESA-STRING
                         ELSE
                            PERFORM 300-PROCESA-STRING-CASADA
                         END-IF
                         ADD 1 TO WKS-CONTADOR-CHEQ
                      ELSE
                         DISPLAY " "
                         DISPLAY " "
                         DISPLAY WKS-GUIONES
                         DISPLAY
                            " EL PRIMER NOMBRE Y/O PRIMER APELLIDO "
                            "NO PUEDEN SER VACIOS"
                         DISPLAY
                            " REGISTRO EN CONFLICTO NO. "
                            WKS-CONTADOR-REG
                         DISPLAY
                            " REGISTRO DE ENTRADA     : "
                         DISPLAY
                            "==> " WKS-NOMBRE-ENTRADA
                         DISPLAY WKS-GUIONES
                         ADD 1 TO WKS-CONTADOR-ERROR
                      END-IF
                   ELSE
                      DISPLAY " "
                      DISPLAY " "
                      DISPLAY WKS-GUIONES
                      DISPLAY " NOMBRE NO PUEDE SER VACIO O ESPACIOS"
                      DISPLAY
                         " REGISTRO EN CONFLICTO NO. " WKS-CONTADOR-REG
                      DISPLAY WKS-GUIONES
                      ADD 1 TO WKS-CONTADOR-ERROR
                   END-IF
                   INITIALIZE WKS-NOMBRE-ENTRADA
                   ACCEPT WKS-NOMBRE-ENTRADA FROM SYSIN
           END-PERFORM.
       200-RECIBE-DATOS-SYSIN-E.            EXIT.

       300-PROCESA-STRING SECTION.
           INITIALIZE WKS-NOMBRE-FORMATEADO, WKS-NOMBRES-EDITADOS,
                      WKS-APELLIDOS-EDIT

           STRING WKS-PRIMER-NOMBRE    DELIMITED BY "  "
                  " "                  DELIMITED BY SIZE
                  WKS-SEGUNDO-NOMBRE   DELIMITED BY "  "
                  INTO WKS-NOMBRES-EDITADOS
           END-STRING

           STRING WKS-PRIMER-APELLIDO  DELIMITED BY "  "
                  " "                  DELIMITED BY SIZE
                  WKS-SEGUNDO-APELLIDO DELIMITED BY "  "
                  INTO WKS-APELLIDOS-EDIT
           END-STRING

           STRING WKS-NOMBRES-EDITADOS DELIMITED BY "  "
                  " "                  DELIMITED BY SIZE
                  WKS-APELLIDOS-EDIT   DELIMITED BY "  "
                  INTO WKS-NOMBRE-FORMATEADO
           END-STRING
           PERFORM 300-CHEQUE.
       300-PROCESA-STRING-E. EXIT.

       300-PROCESA-STRING-CASADA SECTION.
           INITIALIZE WKS-NOMBRE-FORMATEADO, WKS-NOMBRES-EDITADOS,
                      WKS-APELLIDOS-EDIT

           STRING WKS-PRIMER-NOMBRE    DELIMITED BY "  "
                  " "                  DELIMITED BY SIZE
                  WKS-SEGUNDO-NOMBRE   DELIMITED BY "  "
                  INTO WKS-NOMBRES-EDITADOS
           END-STRING

           STRING WKS-PRIMER-APELLIDO  DELIMITED BY "  "
                  " "                  DELIMITED BY SIZE
                  WKS-SEGUNDO-APELLIDO DELIMITED BY "  "
                  INTO WKS-APELLIDOS-EDIT
           END-STRING

           STRING WKS-NOMBRES-EDITADOS DELIMITED BY "  "
                  " "                  DELIMITED BY SIZE
                  WKS-APELLIDOS-EDIT   DELIMITED BY "  "
                  " DE "                DELIMITED BY SIZE
                  WKS-CASADA-APELLIDO  DELIMITED BY "  "
                  INTO WKS-NOMBRE-FORMATEADO
           END-STRING
           PERFORM 300-CHEQUE.
       300-PROCESA-STRING-CASADA-E. EXIT.

       300-CHEQUE SECTION.
           ACCEPT WKS-FECHA-EDIT FROM DATE
           DISPLAY WKS-GUIONES
           DISPLAY
           "*    BANCO INDUSTRIAL S,A.                    GUATEMALA, "
            WKS-FECHA-EDIT "         *"
           DISPLAY
           "*    CUENTA MONETARIA  NO. _______________    "
           "VALOR Q:__________________  *"
           DISPLAY
           "*                                              "
           "                           *"
           DISPLAY "*    PAGUESE A NOMBRE DE : " WKS-NOMBRE-FORMATEADO
           DISPLAY
           "*    LA SUMA DE:_______________________________"
           "_________________________  *"
           DISPLAY
           "*                                              "
           "                           *"
           DISPLAY
           "*                      FIRMA: _________________"
           "_________________________  *"
           DISPLAY
           "*                                              "
           "                           *"
           DISPLAY WKS-GUIONES
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       300-CHEQUE-E. EXIT.

       400-ESTADISTICAS SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY
           "=========================================================="
           DISPLAY
           "*                        ESTADISTICAS                    *"
           DISPLAY
           "=========================================================="
           DISPLAY
           "* TOTAL DE NOMBRES INGRESADOS                  : "
           WKS-CONTADOR-REG "      *"
           DISPLAY
           "* TOTAL DE CHEQUES GENERADOS                   : "
           WKS-CONTADOR-CHEQ "      *"
           DISPLAY
           "* TOTAL NOMBES INVALIDOS / CHEQUES NO GENERADS : "
           WKS-CONTADOR-ERROR "      *"
           DISPLAY
           "==========================================================".
       400-ESTADISTICAS-E. EXIT.
