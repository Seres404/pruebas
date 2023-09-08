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
           SELECT STUDEN ASSIGN TO STUDEN
                  FILE STATUS IS FS-STUDEN.
           SELECT STUDE2  ASSIGN TO STUDE2
                  FILE STATUS IS FS-STUDE2.
       DATA DIVISION.
       FILE SECTION.
       FD STUDEN.
       RECORDING MODE IS F.
       01 StudentDetails.
           02  StudentId           PIC 9(7).
           02  StudentName.
              03 Surname           PIC X(8).
              03 Initials          PIC XX.
           02  DateOfBirth.
              03 YOBirth           PIC 9(4).
              03 MOBirth           PIC 9(2).
              03 DOBirth           PIC 9(2).
           02  CourseCode          PIC X(4).
           02  Gender              PIC X.
       FD STUDE2.
       RECORDING MODE IS F.
       01 StudentDetails2          PIC X(30).
       WORKING-STORAGE SECTION.
       01  FS-STUDEN               PIC 9(02) VALUE ZEROS.
       01  FS-STUDE2               PIC 9(02) VALUE ZEROS.
       01  WKS-SALIDA.
           02 WKS-CONTROL          PIC 9(02) VALUE ZEROS.
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
           OPEN INPUT  STUDEN
           OPEN OUTPUT STUDE2
           IF FS-STUDEN = 97 AND FS-STUDE2 = 97
              MOVE ZEROS TO FS-STUDEN
              MOVE ZEROS TO FS-STUDE2
           END-IF
           IF FS-STUDEN NOT = 0 AND FS-STUDE2 = 97
              DISPLAY "=============================================="
                       UPON CONSOLE
              DISPLAY "*     HUBO UN ERROR AL ABRIR LOS ARCHIVOS    *"
                       UPON CONSOLE
              DISPLAY "* CODIGO DE ERROR ARCHIVO UNO :" FS-STUDEN 
                       UPON CONSOLE
              DISPLAY "* CODIGO DE ERROR ARCHIVO UNO :" FS-STUDE2 
                       UPON CONSOLE
              DISPLAY "=============================================="
                       UPON CONSOLE
              PERFORM 140-CLOSE-FILE 
              MOVE 91 TO RETURN-CODE
              STOP RUN
           END-IF.
       110-OPEN-DATA-E. EXIT. 
      *                SECCION DE ITERACCION PARA LECTURA
       120-READ-AND-WRITE SECTION.
           READ STUDEN
                AT END MOVE HIGH-VALUES TO StudentDetails
           END-READ
           PERFORM UNTIL StudentDetails = HIGH-VALUES
              ADD 1 TO WKS-CONTROL
              WRITE StudentDetails2 FROM StudentDetails
              READ STUDEN
                 AT END MOVE HIGH-VALUES TO StudentDetails
              END-READ
           END-PERFORM.
       120-READ-AND-WRITE-E. EXIT.
      *                    SECCION DE ESTADISTICAS 
       130-ESTADISTICS SECTION.
           DISPLAY ">>>>>>>>>>>>>>>>>>>>RESUMEN<<<<<<<<<<<<<<<<<<<<<"
           DISPLAY "*                                                 *"
           DISPLAY "* REGISTROS LEIDOS:      (" WKS-CONTROL ")        *"
           DISPLAY ">>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<".
       130-ESTADISTICS-E. EXIT.
      *                  SECCIÓN CIERRE DE ARCHIVOS
       140-CLOSE-FILE SECTION.
           CLOSE STUDEN
           CLOSE STUDE2.
       140-CLOSE-FILE-E. EXIT.