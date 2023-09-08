      ******************************************************************
      * FECHA       : 20/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34014                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA TOMA LOS DATOS DE UN ARCHIVO PLANO *
      *             : (PS) LUEGO VALIDA LAS FECHAS DE NACIMIENTO       *
      *             : PARA ENVIAR A LA COLA DE IMPRESION ESTADISTICAS  *
      *             : SIRVE PARA PRACTICAR LAS CADENAS DE TEXTO        *
      * ARCHIVOS    : EDUC.EDU34014.ENTRADA.UNO                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. EDU34014.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT STUDENT ASSIGN TO STUDENT
                          FILE STATUS IS FS-STUDENT.
       DATA DIVISION.
       FILE SECTION.
       FD STUDENT. 
       01  STUDENTDETAILS.
           88 ENDOFSTUDENTFILE             VALUE HIGH-VALUES.
           02 STUDENTID                    PIC 9(7).
           02 STUDENTNAME.
              03 SURNAME                   PIC X(8).
              03 INITIALS                  PIC X(2).
           02 DATEOFBIRTH.
              03 YOBIRTH                   PIC 9(4).
              03 MOBIRTH                   PIC 9(2).
              03 DOBIRTH                   PIC 9(2).
           02 COURSECODE                   PIC X(4).
           02 GENDER                       PIC X(1).
              88 MALE                      VALUE "M", "M".
       WORKING-STORAGE SECTION. 
       01  WKS-MONTH-TABLE.
           02 WKS-VALUES-OF-TABLE.
              03 FILLER                    PIC X(18) VALUE 
                                                  "ENERO     FEBRERO".
              03 FILLER                    PIC X(178) VALUE 
                                                  "MARZO     ABRIL".
              03 FILLER                    PIC X(18) VALUE
                                                  "MAYO      JUNIO".
              03 FILLER                    PIC X(18) VALUE 
                                                  "JULIO     AGOSTO".
              03 FILLER                    PIC X(18) VALUE 
                                                  "SEPTIEMBRE OCTUBRE".
              03 FILLER                    PIC X(18) VALUE 
                                                  "NOVIEMBREDICIEMBRE".
           02 WKS-FILLER2  REDEFINES WKS-VALUES-OF-TABLE.
              03 WKS-MONTH OCCURS 12 TIMES PIC X(10).
      *
       01  FS-STUDENT                      PIC 9(02) VALUE ZEROS.
       01  WKS-MONTH-COUNT OCCURS 12 TIMES PIC 9(02) VALUE ZEROS.
       01  WKS-MONT-INDX                   PIC 9(02) VALUE ZEROS.
       01  WKS-HeadingLine                 PIC X(20) VALUE   
                                                 " MES     ESTUDIANTES".
       01  WKS-DISPLAY-LINE.
           02 PRINT-MONTH                  PIC X(10).
           02 FILLER                       PIC X(04) VALUE " == ".
           02 PRINT-STUDENT-COUNT          PIC ZZ9.
      *
       PROCEDURE DIVISION.
      * <--------------------->    MAIN SECTION
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATASET
           PERFORM 120-READ-REGS
           PERFORM 140-CLOSE-DATASET.
           STOP RUN.
       100-MAIN-E. EXIT.
      
      * <------------------>  SECTION TO OPEN DATASET
       110-OPEN-DATASET SECTION.
           OPEN INPUT STUDENT
           IF FS-STUDENT = 97
              MOVE ZEROS TO FS-STUDENT
           END-IF
           IF FS-STUDENT NOT = 0
              DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO UNO : (" FS-STUDENT ")"
                       UPON CONSOLE
              DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       UPON CONSOLE
              PERFORM 140-CLOSE-DATASET
              MOVE 91 TO RETURN-CODE
              STOP RUN
           ELSE
           DISPLAY "************ LECTURA DE DATOS EXITOSA *************"
                       UPON CONSOLE
           END-IF.
       110-OPEN-DATASET-E. EXIT.

      * <-------------> SECTION TO READ ALL REGS IN THE DATASET 
       120-READ-REGS SECTION.
           READ STUDENT
                AT END SET ENDOFSTUDENTFILE TO TRUE
           END-READ
           PERFORM UNTIL ENDOFSTUDENTFILE
              ADD 1 TO WKS-MONTH-COUNT(MOBIRTH)
              READ STUDENT
                AT END SET ENDOFSTUDENTFILE TO TRUE
              END-READ
           END-PERFORM
           PERFORM 130-DISPLAY-STADISTICS.
       120-READ-REGS-E. EXIT.

      * <--------> SECTION TO DISPLAY STADISTICS IN THE PRINT SPOOL
       130-DISPLAY-STADISTICS SECTION.
           DISPLAY WKS-HeadingLine
           PERFORM VARYING WKS-MONT-INDX FROM 1 BY 1 
                                         UNTIL WKS-MONT-INDX > 12
              MOVE WKS-MONTH(18 4) TO PRINT-MONTH 
              MOVE WKS-MONTH-COUNT(WKS-MONT-INDX) TO PRINT-STUDENT-COUNT
              DISPLAY WKS-DISPLAY-LINE
           END-PERFORM.
       130-DISPLAY-STADISTICS-E. EXIT.
      
      * <-------------> SECTION TO CLOSE THE DATASET IN USE
       140-CLOSE-DATASET SECTION.
           CLOSE STUDENT.
       140-CLOSE-DATASET-E. EXIT.
      *                8712351SMITH   MS19671012LM51F