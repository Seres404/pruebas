      ******************************************************************
      * FECHA       : 09/03/2023                                       *
      * PROGRAMADOR : CERICK DANIEL RAMIREZ DIVAS                      *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34009                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA TOMA DATOS DE UNA ENTRADA SYSIN    *
      *             : ESTOS DATOS LOS VALIDA Y GUARDA DENTRO DE UN     *
      *             : ARCHIVO PLANO (PS)                               *
      *             : BRINDA UN CONTADOR DE LOS REGISTROS LEIDOS       *
      * ARCHIVOS    : EDUC.EDU34009.SALIDA.UNO                         *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. EDU34009.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT STUDENT ASSIGN TO STUDENT
                  FILE STATUS IS FS-STUDENT.
       DATA DIVISION. 
       FILE SECTION.
       FD STUDENT.
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
       WORKING-STORAGE SECTION. 
       01 FS-STUDENT               PIC 9(2) VALUE ZEROS.
       01 WKS-ESTADISTICS.
           02 WKS-REG-READ         PIC 9(2) VALUE ZEROS.
           02 WKS-REG-WRITEN       PIC 9(2) VALUE ZEROS.
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATA
           PERFORM 120-READ-AND-WRITE
           PERFORM 130-ESTADISTICS
           PERFORM 140-CLOSE-DATA
           STOP RUN.
       100-MAIN-E. EXIT.
      *    --------------- SECCION UNO APERTURA DE ARCHIVOS ------------
       110-OPEN-DATA SECTION.
           OPEN OUTPUT  STUDENT
           IF FS-STUDENT = 97
              MOVE ZEROS TO FS-STUDENT
           END-IF
           IF FS-STUDENT NOT = 0
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY "  ! OH NO ALGO SALIO MAL AL ABRIR EL ARCHIVO  ! "
                       UPON CONSOLE
              DISPLAY "  FILE STATUS CODE IS: " FS-STUDENT 
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              PERFORM 140-CLOSE-DATA
              MOVE 91 TO RETURN-CODE
              STOP RUN
           END-IF.
       110-OPEN-DATA-E. EXIT.
      *    ------------- SECCION DOS LECTURA Y ESCRITURA ---------------
       120-READ-AND-WRITE SECTION.
           DISPLAY "INGRESA LOS DATOS DEL ESTUDIANTE, PARA PARAR 
      -            " INGRESE CEROS EN EL ULTIMO REGISTRO "
           DISPLAY "---------------------------------------------------"
           PERFORM 150-FOOLPROOF
           PERFORM 099-DATA-ENTRY
           PERFORM UNTIL StudentDetails = ZEROS
                   ADD 1 TO WKS-REG-READ 
                   ADD 1 TO WKS-REG-WRITEN
                   WRITE StudentDetails
                   PERFORM 099-DATA-ENTRY
           END-PERFORM
           PERFORM 140-CLOSE-DATA.
       120-READ-AND-WRITE-E. EXIT.
      *    -------------- ANEXO PARA ACEPTAR DATOS EN SYSIN -----------"
       099-DATA-ENTRY SECTION.
           DISPLAY "INGRESE LOS DATOS DE LA SIGUIENTE FORMA :          "
           DISPLAY " CODIGO,APELLIDO,INICIALES,AAAAMMDD,CURSECODE, 
      -            " GENERO (TODO JUNTO SIN ESPACIOS)" .
       099-DATA-ENTRY-E. EXIT.
      *    ----------------- SECCION DE ESTADISTICAS ------------------"
       130-ESTADISTICS SECTION.
           DISPLAY ">>>>>>>>>>>>>>>>>>>ESTADISTICAS<<<<<<<<<<<<<<<<<<<"
           DISPLAY "* REGISTROS LEIDOS                 : (" 
                                                     WKS-REG-READ   " )"
           DISPLAY "* REGISTROS ESCRITOS               : (" 
                                                    WKS-REG-WRITEN " )".
       130-ESTADISTICS-E. EXIT.
      *    ----------------- SECCION DE CIERRE DE ARCHIVOS ------------"
       140-CLOSE-DATA SECTION.
           CLOSE STUDENT.
       140-CLOSE-DATA-E. EXIT.
      *    --------------- SECCIÓN PARA EVITAR UN LOOP --------------- "
       150-FOOLPROOF SECTION.
           MOVE ZEROS TO StudentDetails
           ACCEPT  StudentDetails FROM SYSIN.
       150-FOOLPROOF-E. EXIT.