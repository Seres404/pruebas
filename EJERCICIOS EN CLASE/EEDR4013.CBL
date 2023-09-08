      ******************************************************************
      * FECHA       : 09/03/2023                                       *
      * PROGRAMADOR : CERICK DANIEL RAMIREZ DIVAS                      *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34013                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA TOMA LOS DATOS DE UN ARCHIVO PLANO *
      *             : PROCESA LOS DATOS Y LOS VALIDA, POSTERIOR A ESTO *
      *             : GENERA UN SEGUNDO ARCHIVO CON UN REPORTE A       *
      *             : DETALLE                                          *
      * ARCHIVOS    : EDUC.EDU34013.SALIDA.UNO, EDUC.EDU34013.ENTRADA. *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. EDU34013.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT STUDENT ASSIGN TO STUDENT
                          FILE STATUS IS FS-STUDENT.
           SELECT REPORTE ASSIGN TO REPORTE
                          FILE STATUS IS FS-REPORTE.
       DATA DIVISION.
       FILE SECTION. 
       FD  STUDENT.
       01  STUDENTDETAILS.
           88 EndOfStudentFile             VALUE HIGH-VALUES.
           02 STUDENTID                    PIC 9(7).
           02 StudentName.
              03 SURNAME                   PIC X(8).
              03 INITIALS                  PIC X(2).
           02 DATEOFBIRTH.
              03 YOBirth                   PIC 9(4).
              03 MOBirth                   PIC 9(2).
              03 DOBirth                   PIC 9(2).
           02 CourseCode                   PIC X(4).
           02 Gender                       PIC X(1).
              88 MALE                      VALUE "M", "m".
       FD  REPORTE.
       01  PRINTLINE                       PIC X(40).
       WORKING-STORAGE SECTION. 
      *                     VARIABLES FILE STATUS
       01  FS-STUDENT                      PIC 9(02) VALUE ZEROS.
       01  FS-REPORTE                      PIC 9(02) VALUE ZEROS.
       01  WKS-HEADINGLINE                 PIC X(33)
                   VALUE "* RECUENTO TOTAL DE ESTUDIANTES *".
      *
       01  WKS-StudentTotalLine.
           02 FILLER                       PIC X(24)
                   VALUE "* TOTAL DE ESTUDIANTES :".
           02 WKS-PrnStudentCount          PIC Z,ZZ9. 
      *      
       01  WKS-MALETOTALLINE.
           02 FILLER                       PIC X(24)
                   VALUE "* TOTAL DE VARONES     :".
           02 WKS-PrnMaleCount             PIC Z,ZZ9. 
      *
       01  WKS-FEMALETOTALLINE.
           02  FILLER                      PIC X(24)
                   VALUE "* TOTAL DE MUJERES     :".
           02 WKS-PrnFemaleCount           PIC Z,ZZ9.
      *
       01  WORKTOTAL.
           02  WKS-StudentCount            PIC 9(4) VALUE ZERO. 
           02  WKS-MaleCount               PIC 9(4) VALUE ZERO. 
           02  WKS-FemaleCount             PIC 9(4) VALUE ZERO.
      * 
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM  110-OPEN-DATA
           PERFORM  120-READ
           PERFORM  130-WRITE-DATA
           PERFORM  140-CLOSE-DATA
           STOP RUN.
       100-MAIN-E. EXIT.
      *                  SECCION UNO PARA ABRIR ARCHIVOS
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
              PERFORM 140-CLOSE-DATA
              MOVE 91 TO RETURN-CODE
              STOP RUN
           ELSE
           DISPLAY "************ LECTURA DE DATOS EXITOSA *************"
                       UPON CONSOLE
           END-IF.
       110-OPEN-DATA-E. EXIT.
      *                     SECCION PARA LEER DATOS
       120-READ    SECTION.
           READ STUDENT
                AT END SET EndOfStudentFile TO TRUE
           END-READ
      *     
           PERFORM UNTIL EndOfStudentFile 
            ADD 1 TO WKS-StudentCount 
            IF Male  
               ADD 1 TO WKS-MaleCount 
            ELSE   
               ADD 1 TO WKS-FemaleCount 
            END-IF 
            READ STUDENT 
                 AT END SET EndOfStudentFile TO TRUE 
            END-READ 
           END-PERFORM.
      *
       120-READ-E. EXIT.
      *                 SECCION DE ESCRITURA DE DATOS
       130-WRITE-DATA SECTION.
           MOVE WKS-StudentCount TO WKS-PrnStudentCount 
           MOVE WKS-MaleCount    TO WKS-PrnMaleCount 
           MOVE WKS-FemaleCount  TO WKS-PrnFemaleCount 
      *
           WRITE PrintLine FROM WKS-HeadingLine 
           WRITE PrintLine FROM WKS-StudentTotalLine
           WRITE PrintLine FROM WKS-MaleTotalLine  
           WRITE PrintLine FROM WKS-FemaleTotalLine.
       130-WRITE-DATA-E.
      *               SECCION PARA CERRAR ARCHIVOS
       140-CLOSE-DATA SECTION.
           CLOSE STUDENT
           CLOSE REPORTE.
       140-CLOSE-DATA-E. EXIT. 
      
