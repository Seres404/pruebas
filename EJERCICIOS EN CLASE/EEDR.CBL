       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SeqWrite.
       AUTHOR.  Michael Coughlan.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT StudentFile ASSIGN TO "STUDENTS.DAT"
		                ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD StudentFile.
       01  WKS-StudentDetails.
           02  WKS-StudentId       PIC 9(7) VALUE ZEROS.
           02  WKS-StudentName.
               03 WKS-Surname      PIC X(8) VALUE ZEROS.
               03 WKS-Initials     PIC XX   VALUE SPACES.
           02  WKS-DateOfBirth.
               03 WKS-YOBirth      PIC 9(4) VALUE ZEROS.
               03 WKS-MOBirth      PIC 9(2) VALUE ZEROS.
               03 WKS-DOBirth      PIC 9(2) VALUE ZEROS.
           02  WKS-CourseCode      PIC X(4) VALUE SPACES.
           02  WKS-Gender          PIC X    VALUE SPACES.
           02  WKS-FILLER          PIC X(5) VALUE SPACES.
       PROCEDURE DIVISION.
       Begin.
           OPEN OUTPUT StudentFile
            DISPLAY "Enter student details using template below.  
      -        " Enter no data to end."
           PERFORM GetStudentDetails
           PERFORM UNTIL WKS-StudentDetails = SPACES
           WRITE WKS-StudentDetails
           PERFORM GetStudentDetails
           END-PERFORM
           CLOSE StudentFile
           STOP RUN.

       GetStudentDetails.
           DISPLAY "Enter - StudId, Surname, Initials, YOB, MOB, 
      -    "DOB, Course, Gender"
           DISPLAY "NNNNNNNSSSSSSSSIIYYYYMMDDCCCCG"
           ACCEPT  WKS-StudentDetails.  