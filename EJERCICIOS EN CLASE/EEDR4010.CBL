      ******************************************************************
      * FECHA : 10/03/2023                                             *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION : EDUCACION                                         *
      * PROGRAMA : EDU34010                                            *
      * TIPO : BATCH                                                   *
      * DESCRIPCION : DESCRIPCION 1 (LO QUE HACE SU PROGRAMA)          *
      *             : DESCRIPCION 2                                    *
      *             : DESCRIPCION 3                                    *
      *             : DESCRIPCION 4                                    *
      ******************************************************************
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. EDU34010.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT STUDENTS ASSIGN TO STUDENTS
                  FILE STATUS IS FS-STUDENTS.
       DATA DIVISION.
       FILE SECTION.
       FD STUDENTS.
       01 StudentDetails.
           02  StudentId       PIC 9(7).
           02  StudentName.
              03 Surname       PIC X(8).
              03 Initials      PIC XX.
           02  DateOfBirth.
              03 YOBirth       PIC 9(4).
              03 MOBirth       PIC 9(2).
              03 DOBirth       PIC 9(2).
           02  CourseCode      PIC X(4).
           02  Gender          PIC X.
       WORKING-STORAGE SECTION.
       01  FS-STUDENTS             PIC 9(02) VALUE ZEROS.
       01  WKS-ESTADISTICS.
           02 WKS-REG-READ         PIC 9(02) VALUE ZEROS.
           02 WKS-REG-NOT-READ     PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 110-OPEN-DATA
           PERFORM 120-READ-AND-WRITE
           PERFORM 130-ESTADISTICS
           PERFORM 140-CLOSE-FILE
           STOP RUN.
       100-PRINCIPAL-E. EXIT.
      *                 SECCION UNO LEER CARGA INICIAL
       110-OPEN-DATA SECTION.
           OPEN INPUT STUDENTS
           IF FS-STUDENTS = 97
              MOVE ZEROS TO FS-STUDENTS
           END-IF
           IF FS-STUDENTS NOT = 0
              DISPLAY "¡¡¡¡ERROR AL ABRIR EL ARCHIVO!!!! :"
                       UPON CONSOLE
              PERFORM 140-CLOSE-FILE 
              MOVE 91 TO RETURN-CODE
              STOP RUN
           END-IF.
       110-OPEN-DATA-E. EXIT. 
      *                SECCION DE ITERACCION PARA LECTURA
       120-READ-AND-WRITE SECTION.
           READ STUDENTS
           AT END MOVE HIGH-VALUES TO StudentDetails
           END-READ
           IF STUDENTS = SPACES 
              ADD 1 TO WKS-REG-NOT-READ 
           END-IF
           PERFORM UNTIL StudentDetails = HIGH-VALUES
              ADD 1 TO WKS-REG-READ
              DISPLAY StudentId SPACE Surname " " Initials
                      SPACE CourseCode SPACE YOBirth
              READ STUDENTS
                 AT END MOVE HIGH-VALUES TO StudentDetails
              END-READ
           END-PERFORM.
       120-READ-AND-WRITE-E. EXIT.
      *                    SECCION DE ESTADISTICAS 
       130-ESTADISTICS SECTION.
           DISPLAY ">>>>>>>>>>>>>>>>>>>>RESUMEN<<<<<<<<<<<<<<<<<<<<<"
           DISPLAY "*                                                 *"
           DISPLAY "* REGISTROS LEIDOS:      ("WKS-REG-READ")         *"
           DISPLAY "* REGISTROS NO LEIDOS: ("WKS-REG-NOT-READ ")      *"
           DISPLAY ">>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<".
       130-ESTADISTICS-E. EXIT.
      *                  SECCIÓN CIERRE DE ARCHIVOS
       140-CLOSE-FILE SECTION.
           CLOSE STUDENTS.
       140-CLOSE-FILE-E. EXIT.
