      ******************************************************************
      * FECHA       : 08/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EEDR3004, CORRECCION DEL ANTERIOR EDU34004       *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA TOMA DATOS DE UN DATASET DE ENTRADA*
      *             : ESTOS DATOS LOS VALIDA QUE ESTUDIANTES ESTAN     *
      *             : ACTIVOS PARA LUEGO ESCRIBIR ESTOS REGISTROS DEN- *
      *             : TRO DE UN (PS), LUEGO DE ELLO BRINDA ESTADISTICAS*
      *             : SOBRE LOS REGISTROS PROCESADOS                   *
      * ARCHIVOS    : EDUC.SEMI.ENTRADA                                *
      *             : EDUC.EDU33004.SALIDA.UNO                         *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EEDR3004.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO STUDENT
                          FILE STATUS IS FS-STUDENT.
           SELECT REPORTE ASSIGN TO REPORTE
                          FILE STATUS IS FS-REPORTE.
       DATA DIVISION.
       FILE SECTION.
      *                   INPUT BUFFER DEFINITION
       FD STUDENT.
       01 REG-ENTRY.
           88 ENDOFSTUDENTFILE                 VALUE HIGH-VALUES.
           02 STUDENT-DATA.
              03 ENTRY-STUDENT-ID              PIC X(04).
              03 ENTRY-REGISTRATION-DATE       PIC 9(08).
              03 ENTRY-STUDENT-NAME            PIC X(75).
              03 ENTRY-BIRTHDATE.
                 04 ENTRY-DAY                  PIC 9(02).
                 04 ENTRY-MONT                 PIC 9(02).
                 04 ENTRY-YEAR                 PIC 9(04).
              03 ENTRY-NOTE-UNIT-ONE           PIC 9(02).
              03 ENTRY-NOTE-UNIT-TWO           PIC 9(02).
              03 ENTRY-NOTE-UNIT-TRE           PIC 9(02).
              03 ENTRY-STATUS-ESTUDENT         PIC X(01).
                 88 WKS-ACTIVOS                          VALUE 'A'.
                 88 WKS-INACTIVOS                        VALUE 'I'.
      *                    OUTPUT BUFFER DEFINITION
       FD REPORTE.
       01 REG-OUPUT.
           02 OUPUT-DATA.
              03 OUPUT-STUDENT-ID              PIC 9(04).
              03 FILLER                        PIC X(01).
              03 OUPUT-STUDENT-NAME            PIC X(75).
              03 FILLER                        PIC X(01).
              03 OUPUT-BIRTHDATE               PIC 9(08).
              03 FILLER                        PIC X(01).
              03 OUPUT-STUDENT-AGE             PIC 9(03).
              03 FILLER                        PIC X(01).
              03 OUPUT-STUDENT-AVERAGE         PIC 9(02)V99.
       WORKING-STORAGE SECTION.
      *                     VARIABLES FILE STATUS
       01  FS-STUDENT                          PIC 9(02) VALUE ZEROS.
       01  FS-REPORTE                          PIC 9(02) VALUE ZEROS.
      *                      STADISTICS VARIABLES
       01  WKS-STUDENTS-ACTIVE                 PIC 9(02) VALUE ZEROS.
       01  WKS-STUD-INACTIVE                   PIC 9(02) VALUE ZEROS.
       01  WKS-REG-READ                        PIC 9(02) VALUE ZEROS.
       01  WKS-REG-WRITEN                      PIC 9(02) VALUE ZEROS.
       01  WKS-REG-NOT-WRITEN                  PIC 9(02) VALUE ZEROS.
      *                      VARIABLES CALCULO DE FECHAS
       01  WKS-CURRENT-DATE.
           02 WKS-CURRENT-YEAR                 PIC 9(04) VALUE ZEROS.
           02 WKS-CURRENT-MONT                 PIC 9(02) VALUE ZEROS.
           02 WKS-CURRENT-DAY                  PIC 9(02) VALUE ZEROS.
       01  WKS-EDAD                            PIC 9(03).
      *                     VARIABLES PARA PROMEDIOS
       01  WKS-AVERGE                          PIC 9(03)V99.
       01  WKS-AVERAGE-EDIT                    PIC 99.99.
      *                    ----- MAIN SECTION -----
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-APERTURA-ARCHIVOS
           PERFORM 200-PROCESA-ARCHIVOS
           PERFORM 140-STADISTICS
           PERFORM 150-CLOSE-DATA
           STOP RUN.
       100-MAIN-E. EXIT.
      *                ----- OPEN DATASETS SECTION -----
       110-APERTURA-ARCHIVOS SECTION.
           OPEN INPUT   STUDENT
           OPEN OUTPUT  REPORTE
           IF FS-STUDENT = 97
              MOVE ZEROS TO FS-STUDENT
           IF FS-REPORTE = 97
              MOVE ZEROS TO FS-REPORTE
           END-IF
           IF FS-STUDENT NOT = 0 OR FS-REPORTE NOT = 0
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO UNO : (" FS-STUDENT ")"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO DOS : (" FS-REPORTE ")"
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              PERFORM 150-CLOSE-DATA
              MOVE 91 TO RETURN-CODE
              STOP RUN
           ELSE
           DISPLAY "************ LECTURA DE DATOS EXITOSA *************"
                       UPON CONSOLE
           END-IF.
       110-APERTURA-ARCHIVOS-E. EXIT.

      *--------> SERIE 200 ENFOCADA AL PROCESAMIENTO DE ARCHIVOS Y
      *          LOGICA DEL PROGRAMA
       200-PROCESA-ARCHIVOS SECTION.
      *--------> CICLO DE LECTURAS SECUENCIALES
           READ STUDENT
                AT END SET ENDOFSTUDENTFILE TO TRUE
           END-READ

           PERFORM UNTIL ENDOFSTUDENTFILE
              ADD 1 TO WKS-REG-READ
              PERFORM 210-VERIFICA-ESTUDIANTES-A-I
              READ STUDENT
                   AT END SET ENDOFSTUDENTFILE TO TRUE
              END-READ
           END-PERFORM.
       200-PROCESA-ARCHIVOS-E. EXIT.

      *--------> VERIFICA CUANTOS ESTUDIANTES ESTAN ACTIVOS
       210-VERIFICA-ESTUDIANTES-A-I SECTION.
              EVALUATE TRUE
                WHEN WKS-ACTIVOS
                    ADD 1 TO WKS-STUDENTS-ACTIVE
                    PERFORM 220-CALCULO-EDAD-PROMEDIO
                    PERFORM 240-ESCRITURA-ACTIVOS-REPORT
                WHEN WKS-INACTIVOS
                    ADD 1 TO WKS-STUD-INACTIVE
                    ADD 1 TO WKS-REG-NOT-WRITEN
                WHEN OTHER
                    DISPLAY
                    "================================================"
                    DISPLAY
                    "         HAY UN REGISTRO EN CONFLICTO"
                    DISPLAY
                    "================================================"
                    DISPLAY
                    " ==> SU ESTADO NO ES ACTIVO O INACTIVO"
                    DISPLAY
                    " ==> EL REGISTRO ES : " REG-ENTRY
                    DISPLAY
                    " ==> EL REGISTRO FUE IGNORADO"
                    DISPLAY
                    "================================================"
                    MOVE 91 TO RETURN-CODE
              END-EVALUATE.
       210-VERIFICA-ESTUDIANTES-A-I-E. EXIT.

      *-------------> CALCULO DE EDAD Y PROMEDIOS DEL ESTUDIANTE
       220-CALCULO-EDAD-PROMEDIO SECTION.
      *------------->  CALCULO EDAD
           ACCEPT WKS-CURRENT-DATE FROM DATE YYYYMMDD
           COMPUTE WKS-EDAD = WKS-CURRENT-YEAR - ENTRY-YEAR
           IF (ENTRY-MONT > WKS-CURRENT-MONT)
                    SUBTRACT 1 FROM WKS-EDAD
           END-IF
           IF (ENTRY-MONT = WKS-CURRENT-MONT) AND
              (ENTRY-DAY > WKS-CURRENT-DAY)
                    SUBTRACT 1 FROM WKS-EDAD
           END-IF

      *------------->  PROMEDIOS
           COMPUTE WKS-AVERGE = (ENTRY-NOTE-UNIT-ONE +
                   ENTRY-NOTE-UNIT-TWO + ENTRY-NOTE-UNIT-TRE) / 3.
           MOVE    WKS-AVERGE TO WKS-AVERAGE-EDIT.
       220-CALCULO-EDAD-PROMEDIO-E. EXIT.

      *--------------->   SECCION DE ESCRITURA DE DATOS
       240-ESCRITURA-ACTIVOS-REPORT SECTION.
           MOVE ENTRY-STUDENT-ID   TO OUPUT-STUDENT-ID
           MOVE ENTRY-STUDENT-NAME TO OUPUT-STUDENT-NAME
           MOVE ENTRY-BIRTHDATE    TO OUPUT-BIRTHDATE
           MOVE WKS-EDAD           TO OUPUT-STUDENT-AGE
           MOVE WKS-AVERAGE-EDIT   TO OUPUT-STUDENT-AVERAGE

           WRITE REG-OUPUT
           IF FS-REPORTE = 0
                ADD 1 TO WKS-REG-WRITEN
           ELSE
                DISPLAY
                "================================================="
                UPON CONSOLE
                DISPLAY
                "HUBO UN PROBLEMA AL INTENTAR ESCRIBIR UN REGISTRO"
                UPON CONSOLE
                DISPLAY
                "================================================="
                UPON CONSOLE
                DISPLAY
                "==> EL REGISTRO EN CONFLICTO FUE :"
                UPON CONSOLE
                DISPLAY
                "   ==>" REG-OUPUT
                UPON CONSOLE
                DISPLAY
                "==> EL PROGRAMA TERMINO POR SEGURIDAD, VERIFICAR!!"
                UPON CONSOLE
                DISPLAY
                "================================================="
                UPON CONSOLE
                PERFORM 150-CLOSE-DATA
                MOVE 91 TO RETURN-CODE
                STOP RUN
           END-IF.
       240-ESCRITURA-ACTIVOS-REPORT-E. EXIT.
      *                 ----- SECITON TO STADISTICS -----
       140-STADISTICS SECTION.
           DISPLAY
           ">>>>>>>>>>>>>>>>>>>>>>> ESTADISTICAS <<<<<<<<<<<<<<<<<<<<<"
           DISPLAY
           "||  REGISTROS LEIDOS  EN TOTAL  : ("    WKS-REG-READ    ")"
           DISPLAY
           "||  REGISTROS ESCRITOS          : ("    WKS-REG-WRITEN   ")"
           DISPLAY
           "||  ESTUDIANTES ACTIVOS         : (" WKS-STUDENTS-ACTIVE ")"
           DISPLAY
           ">>>>>>>>>>>> ESTUDIANTES INACTIVOS ESTADISTICAS <<<<<<<<<<<"
           DISPLAY
           "||  ESTUDIANTES INACTIVOS       : (" WKS-STUD-INACTIVE   ")"
           DISPLAY
           "||  REGISTROS NO ESCRITOS       : (" WKS-REG-NOT-WRITEN  ")"
           DISPLAY
           ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<".
       140-STADISTICS-E. EXIT.
      *                  ----- SECTION TO CLOSE FILES -----
       150-CLOSE-DATA SECTION.
           CLOSE STUDENT
           CLOSE REPORTE.
       150-CLOSE-DATA-E. EXIT.