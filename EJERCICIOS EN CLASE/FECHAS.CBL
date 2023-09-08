      ******************************************************************
      * FECHA       : 21/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33XX5                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA TOMA LOS DATOS DE UN ARCHIVO PLANO *
      *             : (PS) LUEGO VALIDA LAS FECHAS DE NACIMIENTO       *
      *             : PARA ENVIAR A LA COLA DE IMPRESION ESTADISTICAS  *
      *             : SIRVE PARA PRACTICAR LAS CADENAS DE TEXTO        *
      * ARCHIVOS    : EDUC.EDU34014.ENTRADA.UNO                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU33XX5.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            UPSI-0 IS WS-0 ON  STATUS IS ENGLISH
                           OFF STATUS IS ENGLISH-OFF
            UPSI-1 IS WS-1 ON  STATUS IS SPANISH
                           OFF STATUS IS SPANISH-OFF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *------------------> VARIABLES INGRESO DE DATOS
       01 WKS-DATE-ENTRY.
           02 WKS-DATE-SYSIN.
              03 WKS-YEAR-E        PIC 9(04) VALUE ZEROS.
              03 WKS-MONTH-E       PIC 9(02) VALUE ZEROS.
              03 WKS-DAY-E         PIC 9(02) VALUE ZEROS.
           02 WKS-DATE-NUM  REDEFINES WKS-DATE-SYSIN
                                   PIC 9(08).
           02 FILLER               PIC X(12).
       01 REDEFINES WKS-DATE-ENTRY.
           02                      PIC X(03).
              88 WKS-END-DATE                    VALUE "FIN".
           02                      PIC X(17).
      *
       01 WKS-WORK-FIELDS.
           02 WKS-CUENTA-FECHAS    PIC 9(02) VALUE ZEROS.
           02 WKS-CUENTA-PROCESOS  PIC 9(02) VALUE ZEROS.
              88 WKS-LIMITE                  VALUE  11.
           02 WKS-FECHA-GUARDADA   PIC 9(08) OCCURS 10 TIMES.
      *-----------------------> CONSTANTES
       01 WKS-DATE-C-NUM.
           02 WKS-YEAR-NUM-C       PIC 9(08) VALUE 01011990.
           02 WKS-YEAR-NUM-1       PIC 9(08) VALUE ZEROS.
       01 WKS-MONTHS.
           02 FILLER               PIC X(08) VALUE "JANENE31".
           02 FILLER               PIC X(08) VALUE "FEBFEB28".
           02 FILLER               PIC X(08) VALUE "MARMAR31".
           02 FILLER               PIC X(08) VALUE "APRABR30".
           02 FILLER               PIC X(08) VALUE "MAYMAY31".
           02 FILLER               PIC X(08) VALUE "JUNJUN30".
           02 FILLER               PIC X(08) VALUE "JULJUL31".
           02 FILLER               PIC X(08) VALUE "AGTAGO31".
           02 FILLER               PIC X(08) VALUE "SEPSEP30".
           02 FILLER               PIC X(08) VALUE "NOVNOV30".
           02 FILLER               PIC X(08) VALUE "DECDIC31".
       01 WKS-MONTH-ARRAY     REDEFINES WKS-MONTHS.
           02 WKS-MONTH-DUAL  OCCURS    12 TIMES.
              03 WKS-MONTH-EN      PIC X(03).
              03 WKS-MONTH-ES      PIC X(03).
              03 WKS-LAST-DAY      PIC 9(02).
      *
       01 WKS-DAY-OF-WEEK.
           02 FILLER               PIC X(18) VALUE "MONDAY   LUNES    ".
           02 FILLER               PIC X(18) VALUE "TUESDAY  MARTES   ".
           02 FILLER               PIC X(18) VALUE "WEDNESDAYMIERCOLES".
           02 FILLER               PIC X(18) VALUE "THURSDAY JUEVES   ".
           02 FILLER               PIC X(18) VALUE "FRIDAY   VIERNES  ".
           02 FILLER               PIC X(18) VALUE "SATURDAY SABADO   ".
           02 FILLER               PIC X(18) VALUE "SUNDAY   DOMINGO  ".
       01 WKS-DAYOFWEEK-ARRAY.
           02 WKS-DAY-DUAL    OCCURS    07 TIMES.
              03 WKS-DAY-EN        PIC X(09).
              03 WKS-DAY-ES        PIC X(09).
      *
       01 WKS-INDEX                PIC 9(02) VALUE ZEROS.
       01 WKS-DAYS-PASSED          PIC 9(04) VALUE ZEROS.
      * <---------------------->  MAIN SECTION
        PROCEDURE DIVISION.
        100-MAIN SECTION.
           PERFORM 110-ACCEPT-DATA
           STOP RUN.
        100-MAIN-E. EXIT.
      * <------------> ACCEPT DATA FROM SYSIN AND VALIDATION
       110-ACCEPT-DATA SECTION.
           ACCEPT WKS-DATE-ENTRY FROM SYSIN
           MOVE   0              TO WKS-CUENTA-FECHAS
      *
           PERFORM UNTIL WKS-END-DATE
                 ADD 1 TO WKS-CUENTA-FECHAS
                 MOVE WKS-DATE-NUM
                      TO WKS-FECHA-GUARDADA(WKS-CUENTA-FECHAS)
                 ACCEPT WKS-DATE-ENTRY FROM SYSIN
           END-PERFORM
           PERFORM 120-DATE-CALCS-DISPLAY.
      *
       110-ACCEPT-DATA-E. EXIT.

       120-DATE-CALCS-DISPLAY SECTION.
           MOVE 1 TO WKS-CUENTA-PROCESOS
           PERFORM UNTIL
           WKS-CUENTA-PROCESOS > WKS-CUENTA-FECHAS OR WKS-LIMITE
      * <---------------> CALCULO DE DIAS TRANSCURRIDOS
              COMPUTE WKS-DAYS-PASSED =
              FUNCTION INTEGER-OF-DATE
              (WKS-FECHA-GUARDADA(WKS-CUENTA-PROCESOS)) -
              FUNCTION INTEGER-OF-DATE(19900101)
      * <--------------> INDICE PARA EL DÍA DE LA SEMANA
              COMPUTE WKS-INDEX = FUNCTION REM(WKS-DAYS-PASSED 7)
              ADD 1 TO WKS-INDEX
      * <--------------> VALIDAMOS QUE RUTINA VAMOS A LLAMAR.
              IF ENGLISH THEN
                PERFORM 130-ENGLISH-MODE
              END-IF
              IF SPANISH THEN
                PERFORM 140-SPANISH-MODE
              END-IF
              ADD 1 TO WKS-CUENTA-PROCESOS
           END-PERFORM.
       120-DATE-CALCS-DISPLAY-E. EXIT.
      * <---------------------- LANGUAJES SECTIONS -------------------->
       130-ENGLISH-MODE SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                    ENGLISH MODE IS ON                  *"
            DISPLAY
           "**********************************************************"
           DISPLAY
           "* " WKS-DAYS-PASSED " DAYS AGO WAS 1 OF JANUARY  OF 1990, "
           "AND TODAY IS " WKS-DAY-EN(WKS-INDEX) ", DAY OF "
           WKS-MONTH-EN(WKS-MONTH-E) " OF YEAR " WKS-YEAR-E
           DISPLAY
           "* AND THE LAST DAY OF THIS MONTH WILL BE "
           WKS-LAST-DAY(WKS-MONTH-E).
           DISPLAY
           "**********************************************************".
       130-ENGLISH-MODE-E. EXIT.

       140-SPANISH-MODE SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                    SPANISH MODE IS ON                  *"
            DISPLAY
           "**********************************************************"
           DISPLAY
           "* " WKS-DAYS-PASSED "DAYS AGO WAS 1 OF JANUARY  OF 1990, "
           "AND TODAY IS " WKS-DAY-EN(WKS-INDEX) ", DAY OF "
           WKS-MONTH-EN(WKS-MONTH-E) " OF YEAR " WKS-YEAR-E
           DISPLAY
           "* AND THE LAST DAY OF THIS MONTH WILL BE "
           WKS-LAST-DAY(WKS-MONTH-E).
           DISPLAY
           "**********************************************************".
       140-SPANISH-MODE-E. EXIT.