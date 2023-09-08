      ******************************************************************
      * FECHA       : 27/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34016                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA LEE LOS DATOS DE UN DATASET PS     *
      *             : Y VALIDA CUANTOS DE LOS ESTUDIANTES QUE SE TIENEN*
      *             : EN ESTE SON VARONES, ASÍ MISMO ORDENA TODOS LOS  *
      *             : REGISTROS POR NOMBRE DE FORMA ASCENDENTE LOS QUE *
      *             : COINCIDAN CON EL SEXO MASCULINO SON ENVIADOS A UN*
      *             : DATASET "REPORTE" Y MUESTRA LAS ESTADISTICAS     *
      * ARCHIVOS    : EDUC.EDU34016.ENTRADA.UNO                        *
      *             : EDUC.EDU34016.SALIDA.UNO                         *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU34016.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT STUDENT ASSIGN TO STUDENT
                           FILE STATUS IS FS-STUDENT.
            SELECT REPORTE     ASSIGN TO REPORTE.
            SELECT WORKFILE    ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
       FD STUDENT.
       01 StudentRec                   PIC X(30).
           88 EndOfFile                VALUE HIGH-VALUES.
       FD REPORTE.
       01 REGISTROSALIDA               PIC X(30).
       SD WorkFile.
       01 WorkRec.
           02 FILLER                   PIC 9(7).
           02 WStudentName             PIC X(10).
           02 FILLER                   PIC X(12).
           02 WGender                  PIC X.
                88 MaleStudent         VALUE "M".
                88 FEMALE              VALUE "F".
       WORKING-STORAGE SECTION.
       01  WKS-STADISTICAS.
           02  WKS-REG-TOTAL            PIC 9(03) VALUE ZEROS.
           02  WKS-MALES-READ          PIC 9(03) VALUE ZEROS.
           02  WKS-FEMALES-READ        PIC 9(03) VALUE ZEROS.
       01  FS-STUDENT                  PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATASET
           PERFORM 120-SORT-DATA
           PERFORM 140-STADISTICS-DATA
           PERFORM 150-CLOSE-DATASET
           STOP RUN.
       100-MAIN-E. EXIT.
       110-OPEN-DATASET SECTION.
           OPEN INPUT STUDENT
           IF FS-STUDENT = 97
              MOVE ZEROS TO FS-STUDENT
           END-IF
           IF FS-STUDENT NOT = 0
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO ES  : (" FS-STUDENT ")"
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              PERFORM 150-CLOSE-DATASET
              MOVE 91 TO RETURN-CODE
              STOP RUN
           ELSE
           DISPLAY    "********** APERTURA DE ARCHIVOS EXITOSA ********"
                       UPON CONSOLE
           END-IF.
       110-OPEN-DATASET-E. EXIT.

       120-SORT-DATA SECTION.
           SORT WorkFile ON ASCENDING KEY WStudentName
                INPUT PROCEDURE IS 130-READ-AND-VALIDATION
                GIVING REPORTE.
       120-SORT-DATA-E. EXIT.

       130-READ-AND-VALIDATION SECTION.
           READ STUDENT
                AT END SET ENDOFFILE TO TRUE
           END-READ
           PERFORM UNTIL ENDOFFILE
               ADD 1 TO WKS-REG-TOTAL
               MOVE STUDENTREC TO WORKREC
               EVALUATE TRUE
                   WHEN MALESTUDENT
                        ADD 1 TO WKS-MALES-READ
                        RELEASE WORKREC
                   WHEN FEMALE
                        ADD 1 TO WKS-FEMALES-READ
                   WHEN OTHER
                   DISPLAY
                   "=================================================="
                   DISPLAY
                   " HAY UN REGISTRO EN CONFLICTO, VERIFICAR GENERO   "
                   DISPLAY
                   "=================================================="
                   DISPLAY
                   " EL REGISTRO ES EL " STUDENTREC
                   DISPLAY
                   "=================================================="
               END-EVALUATE
               READ STUDENT
                    AT END SET ENDOFFILE TO TRUE
               END-READ
           END-PERFORM.
       130-READ-AND-VALIDATION-E. EXIT.
      * SECCION DE IMPRESION DE ESTADISTICAS EN SPOOL.
       140-STADISTICS-DATA SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                      ESTADISTICAS                      *"
           DISPLAY
           "**********************************************************"
           DISPLAY
           " REGISTROS LEIDOS EN TOTAL  : (" WKS-REG-TOTAL ")"
           DISPLAY
           " ESTUDIANTES SEXO MASCULINO : (" WKS-MALES-READ ")"
           DISPLAY
           " ESTUDIANTES SEXO FEMENINO  : (" WKS-FEMALES-READ
                                              ") NO ESCRITOS EN REPORTE"
           DISPLAY
           "**********************************************************".
       140-STADISTICS-DATA-E. EXIT.

      * <-------------> SECTION TO CLOSE THE DATASET IN USE
       150-CLOSE-DATASET SECTION.
           CLOSE STUDENT.
       150-CLOSE-DATASET-E. EXIT.