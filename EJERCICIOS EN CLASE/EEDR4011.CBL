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
          88  EndOfStudentFile  VALUE HIGH-VALUES.
           02  StudentId       PIC 9(7).
           02  StudentName.
           03 Surname      PIC X(8).
           03 Initials     PIC XX.
           02  DateOfBirth.
           03 YOBirth      PIC 9(4).
           03 MOBirth      PIC 9(2).
           03 DOBirth      PIC 9(2).
           02  CourseCode      PIC X(4).
           02  Gender          PIC X.