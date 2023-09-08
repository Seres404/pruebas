       IDENTIFICATION DIVISION. 
       PROGRAM-ID. PRUEBAS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO STUDENT
                          FILE STATUS IS FS-STUDENT.
           SELECT UPDATING ASSIGN TO UPDATING
                          FILE STATUS IS FS-UPDATING.
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
              03 ENTRY-BIRTHDATE               PIC 9(08).        
              03 ENTRY-NOTE-UNIT-ONE           PIC 9(02).
              03 ENTRY-NOTE-UNIT-TWO           PIC 9(02).
              03 ENTRY-NOTE-UNIT-TRE           PIC 9(02).
              03 ENTRY-STATUS-ESTUDENT         PIC X(01).
      *                    OUTPUT BUFFER DEFINITION
       FD UPDATING.
       01 REG-ENTRY-OUPUT                      PIC X(102) VALUE SPACES.    
       WORKING-STORAGE SECTION. 
      *                     VARIABLES FILE STATUS
       01  FS-STUDENT                          PIC 9(02) VALUE ZEROS.
       01  FS-UPDATING                         PIC 9(02) VALUE ZEROS.  
      *                       UPDATING VARIABLES
       01  WKS-ACTUALIZADOR.                  
           02 WKS-STUDENTID                    PIC 9(04) VALUE ZEROS.
           02 FILLER                           PIC X(01) VALUE SPACES.
           02 WKS-STATUS-NEW                   PIC X(01) VALUE SPACES.
      *                          STADISTICS
       01  WKS-REG-UPDATING                    PIC 9(02) VALUE ZEROS.
      *                    ----- MAIN SECTION ----- 
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATA
           PERFORM 120-READ-AND-WRITE-DATA
           PERFORM 130-STADISTICS
           PERFORM 150-CLOSE-DATA
           STOP RUN.
       100-MAIN-E. EXIT.
      *                ----- OPEN DATASETS SECTION -----
       110-OPEN-DATA SECTION.
           OPEN INPUT   STUDENT
           OPEN OUTPUT  UPDATING 
           IF FS-STUDENT = 97
              MOVE ZEROS TO FS-STUDENT
           IF FS-UPDATING = 97
              MOVE ZEROS TO FS-UPDATING      
           END-IF
           IF FS-STUDENT NOT = 0 OR FS-UPDATING NOT = 0
              DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO UNO :(" FS-STUDENT ")"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO DOS :(" FS-UPDATING ")"
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
       120-READ-AND-WRITE-DATA SECTION.
           READ STUDENT
                AT END SET EndOfStudentFile TO TRUE
           END-READ
      *
           PERFORM UNTIL EndOfStudentFile
              ACCEPT WKS-ACTUALIZADOR FROM SYSIN 
      *      
              IF WKS-ACTUALIZADOR NOT EQUAL TO SPACES  
                 EVALUATE TRUE 
                    WHEN WKS-STATUS-NEW = "A"
                         IF WKS-STUDENTID = ENTRY-STUDENT-ID 
                            INSPECT ENTRY-STATUS-ESTUDENT 
                                  REPLACING ALL "I" BY "A"
                            ADD 1 TO WKS-REG-UPDATING
                         ELSE
                            DISPLAY 
                            "ERROR EN CODIGO DE ESTUDIANTE / NO EXISTE"    
                         END-IF 
                    WHEN WKS-STATUS-NEW  = "I"
                         IF WKS-STUDENTID = ENTRY-STUDENT-ID 
                            INSPECT ENTRY-STATUS-ESTUDENT 
                                    REPLACING ALL "A" BY "I"
                            ADD 1 TO WKS-REG-UPDATING
                         ELSE
                            DISPLAY 
                            "ERROR EN CODIGO DE ESTUDIANTE / NO EXISTE"    
                         END-IF 
                  END-EVALUATE
               END-IF 
               WRITE REG-ENTRY-OUPUT FROM REG-ENTRY
               READ STUDENT 
                   AT END SET EndOfStudentFile TO TRUE 
               END-READ 
           END-PERFORM.
       120-READ-AND-WRITE-DATA-E. EXIT.
      *                   ----- STADISTICS SECTION -----
       130-STADISTICS SECTION.
           DISPLAY 
           ">>>>>>>>>>>>>>>>>>>>>>> ESTADISTICAS <<<<<<<<<<<<<<<<<<<<<"
           DISPLAY
           "||  REGISTROS ACTUALIZADOS  EN TOTAL : " WKS-REG-UPDATING
           DISPLAY
           ">>>>>>>>>>>>>>>>>>>>>>> ESTADISTICAS <<<<<<<<<<<<<<<<<<<<<".
       130-STADISTICS-E. EXIT.
      *                  ----- SECTION TO CLOSE FILES -----
       150-CLOSE-DATA SECTION.
           CLOSE STUDENT
           CLOSE UPDATING.
       150-CLOSE-DATA-E. EXIT.