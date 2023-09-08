      ******************************************************************
      * FECHA : 10/03/2023                                             *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION : EDUCACION                                         *
      * PROGRAMA : EDU34009                                            *
      * TIPO : BATCH                                                   *
      * DESCRIPCION : DESCRIPCION 1 (LO QUE HACE SU PROGRAMA)          *
      * : DESCRIPCION 2                                                *
      * : DESCRIPCION 3                                                *
      * : DESCRIPCION 4                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU34009.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT STUDENTS ASSIGN TO STUDENTS
                  FILE STATUS IS FS-STUDENTS.
       DATA DIVISION. 
       FILE SECTION.
       FD STUDENTS.
       01  STUDENTDETAILS.
           02  STUDENTID           PIC 9(7).
           02  STUDENTNAME.
               03 Surname          PIC X(8).
               03 Initials         PIC XX.
           02  DateOfBirth.
               03 YOBirth          PIC 9(4).
               03 MOBirth          PIC 9(2).
               03 DOBirth          PIC 9(2).
           02  CourseCode          PIC X(4).
           02  Gender              PIC X   .
           02  FILLER              PIC X(5).
       WORKING-STORAGE SECTION.
       01  FS-STUDENTS             PIC 9(02) VALUE ZEROS.
       01  WKS-ESTADISTICS.
           02 WKS-REG-READ         PIC 9(02) VALUE ZEROS.
           02 WKS-REG-WRITEN       PIC 9(02) VALUE ZEROS.
           02 WKS-REG-NOT-WRITE    PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       101-MAIN SECTION.
           PERFORM 110-OPEN-DAT
           PERFORM 120-READ-AND-WRITE
           PERFORM 130-ESTADISTICA
           PERFORM 140-CLOSE-DAT.
           STOP RUN.
       101-MAIN-E. EXIT.
      *    >>>>>>>>>>>>>>>>>>>>>>>>>SECCIONES<<<<<<<<<<<<<<<<<<<<<<<<<<
      *    11111                   SECCIÓN UNO                    11111
       110-OPEN-DAT SECTION.
           OPEN OUTPUT STUDENTS
           IF FS-STUDENTS = 97
              MOVE ZEROS TO FS-STUDENTS
           END-IF
           IF FS-STUDENTS NOT = 0
              DISPLAY "ERROR WHEN ATTEMPTING TO OPEN THE FILE!!!!!" " :"
                      FS-STUDENTS UPON CONSOLE
              PERFORM 140-CLOSE-DAT
              MOVE 91 TO RETURN-CODE
              STOP RUN
           END-IF.
       110-OPEN-DAT-E. EXIT.
      *    22222                   SECCIÓN DOS                     22222
       120-READ-AND-WRITE SECTION.
           DISPLAY "Enter student details using template below"
           DISPLAY "StudId,Surname,Initials,YOB,MOB,DOB,Course,Gender"
           DISPLAY "1234567AAAABBBBJV20000101CBOLM"
           DISPLAY ">>>>>>>>>>>>>>>>>>>>>>&&&<<<<<<<<<<<<<<<<<<<<<<<<"
           PERFORM 150-FOOLPROOF
      *    ^^^LA TEST FOR DUMMIES ESTA PENSADA POR SI EL USUARIO
      *    NO COLOCA ESPACIOS AL FINAL DEL SYSIN ^^^^
           PERFORM UNTIL StudentDetails = SPACES
               ADD 1 TO WKS-REG-READ
               WRITE StudentDetails
               IF FS-STUDENTS NOT = 0
                  ADD 1 TO  WKS-REG-NOT-WRITE
               ELSE
                  ADD 1 TO  WKS-REG-READ
               END-IF
                 PERFORM 150-FOOLPROOF
           END-PERFORM.
       120-READ-AND-WRITE-E. EXIT.
      *    33333                   SECCIÓN TRES                   33333
       130-ESTADISTICA SECTION.
           DISPLAY ">>>>>>>>>>>>>>>>>>>>RESUMEN<<<<<<<<<<<<<<<<<<<<<"
           DISPLAY "*                                                 *"
           DISPLAY "* REGISTROS LEIDOS:      ("WKS-REG-READ")         *"
           DISPLAY "* REGISTROS ESCRITOS:    ("WKS-REG-WRITEN")       *"
           DISPLAY "* REGISTROS NO ESCRITOS: ("WKS-REG-NOT-WRITE")    *"
           DISPLAY ">>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<".
       130-ESTADISTICA-E. EXIT.
      *    44444                  SECCIÓN CUATRO                   44444
       140-CLOSE-DAT SECTION.
           CLOSE STUDENTS.
       140-CLOSE-DAT-E. EXIT.
      *    55555                     FOOLPROOF                     55555
       150-FOOLPROOF SECTION.
           MOVE SPACES TO StudentDetails
           ACCEPT StudentDetails.
       150-FOOLPROOF-E. EXIT.
