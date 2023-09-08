      ******************************************************************
      * FECHA       : 16/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34009                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA TOMA DATOS DE UNA ENTRADA SYSIN    *
      *             : ESTOS DATOS LOS VALIDA Y GUARDA DENTRO DE UN     *
      *             : ARCHIVO PLANO (PS)                               *
      *             : BRINDA UN CONTADOR DE LOS REGISTROS LEIDOS       *
      * ARCHIVOS    : EDUC.SEMI.ENTRADA                                *
      *             : EDUC.EDU33004.SALIDA.UNO                         *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. PROJECT1.
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
           88 EndOfStudentFile                 VALUE HIGH-VALUES.
           02 STUDENT-DATA.
              03 ENTRY-STUDENT-ID              PIC X(04).
              03 ENTRY-REGISTRATION-DATE       PIC 9(08).
              03 ENTRY-STUDENT-NAME            PIC X(75).
              03 ENTRY-BIRTHDATE.
                 04 ENTRY-YEAR                 PIC 9(04).
                 04 ENTRY-MONT                 PIC 9(02).
                 04 ENTRY-DAY                  PIC 9(02).
              03 ENTRY-NOTE-UNIT-ONE           PIC 9(02).
              03 ENTRY-NOTE-UNIT-TWO           PIC 9(02).
              03 ENTRY-NOTE-UNIT-TRE           PIC 9(02).
              03 ENTRY-STATUS-ESTUDENT         PIC X(01).
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
           02 WKS-CURRENT-MONTH                PIC 9(02) VALUE ZEROS.
           02 WKS-CURRENT-DAY                  PIC 9(02) VALUE ZEROS.
       01  WKS-ENTRY-DATE.
           02 WKS-ENTRY-YEAR                   PIC 9(04) VALUE ZEROS.
           02 WKS-ENTRY-MONTH                  PIC 9(02) VALUE ZEROS.
           02 WKS-ENTRY-DAY                    PIC 9(02) VALUE ZEROS.
       01  WKS-EDAD                            PIC 9(03).
      *                     VARIABLES PARA PROMEDIOS
       01  WKS-CURSE-ONE                       PIC 9(02) VALUE ZEROS.
       01  WKS-CURSE-TWO                       PIC 9(02) VALUE ZEROS.
       01  WKS-CURSE-TRE                       PIC 9(02) VALUE ZEROS.
       01  WKS-AVERGE                          PIC 9(02)V99.
       01  WKS-AVERAGE-EDIT                    PIC Z.ZZ9.
      *                    ----- MAIN SECTION ----- 
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATA
           PERFORM 120-READ
           PERFORM 130-WRITE-DATA
           PERFORM 140-STADISTICS
           PERFORM 150-CLOSE-DATA
           STOP RUN.
       100-MAIN-E. EXIT.
      *                ----- OPEN DATASETS SECTION -----
       110-OPEN-DATA SECTION.
           OPEN INPUT   STUDENT
           OPEN OUTPUT  REPORTE
           IF FS-STUDENT = 97
              MOVE ZEROS TO FS-STUDENT
           IF FS-REPORTE = 97
              MOVE ZEROS TO FS-REPORTE      
           END-IF
           IF FS-STUDENT NOT = 0 OR FS-REPORTE NOT = 0
              DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO UNO : (" FS-STUDENT ")"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO DOS : (" FS-REPORTE ")"
                       UPON CONSOLE
              DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       UPON CONSOLE
              PERFORM 150-CLOSE-DATA
              MOVE 91 TO RETURN-CODE
              STOP RUN
           ELSE
           DISPLAY "************ LECTURA DE DATOS EXITOSA *************"
                       UPON CONSOLE
           END-IF.
       110-OPEN-DATA-E. EXIT.
      *                    ----- DATA READING SECTION -----
       120-READ SECTION.
           READ STUDENT
                AT END SET EndOfStudentFile TO TRUE
           END-READ
      *
           PERFORM UNTIL EndOfStudentFile
              ADD 1 TO WKS-REG-READ
              IF ENTRY-STATUS-ESTUDENT = "A"
                 ADD 1 TO WKS-REG-WRITEN
                 ADD 1 TO WKS-STUDENTS-ACTIVE
              END-IF
              IF ENTRY-STATUS-ESTUDENT = "I"
                 ADD 1 TO WKS-STUD-INACTIVE
                 ADD 1 TO WKS-REG-NOT-WRITEN
              END-IF
      *
              READ STUDENT 
                   AT END SET EndOfStudentFile TO TRUE 
              END-READ 
           END-PERFORM.
       120-READ-E. EXIT.
      *                  ----- DATA WRITING SECTION -----
       130-WRITE-DATA SECTION.
      *    --------------->        CALCULO EDAD
           MOVE ENTRY-YEAR           TO WKS-ENTRY-YEAR
           MOVE ENTRY-MONT           TO WKS-ENTRY-MONTH
           MOVE ENTRY-DAY            TO WKS-ENTRY-DAY 
      *
           ACCEPT WKS-CURRENT-DATE FROM DATE YYYYMMDD
           COMPUTE WKS-EDAD = WKS-CURRENT-YEAR - WKS-ENTRY-YEAR 
           IF (WKS-ENTRY-MONTH > WKS-CURRENT-MONTH)              
                    SUBTRACT 1 FROM WKS-EDAD 
           END-IF
           IF (WKS-ENTRY-MONTH = WKS-CURRENT-MONTH) AND 
              (WKS-ENTRY-DAY > WKS-CURRENT-DAY)    
                    SUBTRACT 1 FROM WKS-EDAD 
           END-IF    
   
      *    --------------->        PROMEDIOS  
           MOVE ENTRY-NOTE-UNIT-ONE  TO WKS-CURSE-ONE
           MOVE ENTRY-NOTE-UNIT-TWO  TO WKS-CURSE-TWO
           MOVE ENTRY-NOTE-UNIT-TRE  TO WKS-CURSE-TRE
      *           
           COMPUTE WKS-AVERGE = (WKS-CURSE-ONE  + WKS-CURSE-TWO)
           COMPUTE WKS-AVERGE = (WKS-AVERGE + WKS-CURSE-TRE) / 3.
           MOVE WKS-AVERGE TO WKS-AVERAGE-EDIT 
      *
      *    ---------------> PASAMOS DE ENTRADA A SALIDA
           MOVE ENTRY-STUDENT-ID   TO OUPUT-STUDENT-ID
           MOVE ENTRY-STUDENT-NAME TO OUPUT-STUDENT-NAME
           MOVE ENTRY-BIRTHDATE    TO OUPUT-BIRTHDATE
           MOVE WKS-EDAD           TO OUPUT-STUDENT-AGE
           MOVE WKS-AVERAGE-EDIT   TO OUPUT-STUDENT-AVERAGE
      *    --------------->  ESCRITURA DE DATOS EN EL REPORTE.
           WRITE REG-OUPUT.
       130-WRITE-DATA-E. EXIT. 
      *                 ----- SECITON TO STADISTICS -----
       140-STADISTICS SECTION.
           DISPLAY 
           ">>>>>>>>>>>>>>>>>>>>>>> ESTADISTICAS <<<<<<<<<<<<<<<<<<<<<"
           DISPLAY
           "||  REGISTROS LEIDOS  EN TOTAL  : ( "    WKS-REG-READ    ")"
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