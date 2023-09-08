      ******************************************************************
      * FECHA : 10/03/2023                                             *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION : EDUCACION                                         *
      * PROGRAMA : EDU34012                                            *
      * TIPO : BATCH                                                   *
      * DESCRIPCION : DESCRIPCION 1 (LO QUE HACE SU PROGRAMA)          *
      *             : DESCRIPCION 2                                    *
      *             : DESCRIPCION 3                                    *
      *             : DESCRIPCION 4                                    *
      ******************************************************************
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. EDU34012.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDREC  ASSIGN STUDREC
                           FILE STATUS IS FS-ONE.
           SELECT TRANSREC ASSIGN TRANSREC
                           FILE STATUS IS FS-TWO.
           SELECT NEWSTREC ASSIGN NEWSTREC
                           FILE STATUS IS FS-NEW.
       DATA DIVISION.
       FILE SECTION.
       FD STUDREC.
       01 StudentRecord.
           88 EndOfStudentFile     VALUE HIGH-VALUES.
           02 StudentID            PIC X(7).
           02 FILLER               PIC X(23).
       FD TRANSREC.
       01 TransRecord.
           88 EndOfTransFile       VALUE HIGH-VALUES.
           02 TransStudentID       PIC X(7).
           02 FILLER               PIC X(23).
       FD NEWSTREC.
       01 NewStudentRecord         PIC X(30).
       WORKING-STORAGE SECTION.
       01  FS-ONE                  PIC 9(02) VALUE ZEROS.
       01  FS-TWO                  PIC 9(02) VALUE ZEROS.
       01  FS-NEW                  PIC 9(02) VALUE ZEROS.
       01  WKS-ESTADISTICS.
           02 WKS-REG-READ         PIC 9(02) VALUE ZEROS.
           02 WKS-REG-WRITEN       PIC 9(02) VALUE ZEROS.
           02 WKS-REG-NOT-WRITE    PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATA
           PERFORM 120-READ
           PERFORM 130-WRITE
           PERFORM 140-ESTADISTICS
           PERFORM 150-CLOSE-DATA
           STOP RUN.
       100-MAIN-E. EXIT.
       110-OPEN-DATA SECTION.
      * ********************APERTURA PRIMER  DATASET********************
           OPEN INPUT   STUDREC
           OPEN INPUT   TRANSREC
           OPEN OUTPUT  NEWSTREC
           IF FS-ONE = 97 AND  FS-TWO = 97 AND FS-NEW = 97
              MOVE ZEROS TO FS-ONE
              MOVE ZEROS TO FS-TWO
              MOVE ZEROS TO FS-NEW             
           END-IF
           IF FS-ONE NOT = 0 OR FS-TWO NOT = 0 OR FS-NEW NOT = 0
              DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO UNO : (" FS-ONE ")"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO DOS : (" FS-TWO ")"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO NEW : (" FS-NEW  ")"
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
       110-OPEN-DATA-E.
      *                  SECCION DOS LECTURA DE ARCHIVOS
       120-READ SECTION.
           READ STUDREC
                AT END SET EndOfStudentFile TO TRUE
           READ TRANSREC
                AT END SET EndOfTransFile TO TRUE.
       120-READ-E. EXIT.
      *              SECCIÓN TRES ESCRITURA DEL NUEVO ARCHIVO
       130-WRITE SECTION.
           PERFORM  UNTIL (EndOfStudentFile ) AND (EndOfTransFile)
               EVALUATE TRUE
               WHEN (StudentID  < TransStudentID )
                    ADD 1 to WKS-REG-READ 
                    ADD 1 to WKS-REG-WRITEN 
                    WRITE NewStudentRecord  FROM StudentRecord
                    READ STUDREC
                         AT END SET EndOfStudentFile TO TRUE 
                    END-READ
               WHEN (StudentID  > TransStudentID )
                    ADD 1 to WKS-REG-READ 
                    ADD 1 to WKS-REG-WRITEN 
                    WRITE NewStudentRecord  FROM TransRecord 
                    READ TRANSREC
                         AT END SET EndOfTransFile TO TRUE 
                    END-READ
               WHEN (StudentID  = TransStudentID )
                    ADD 1 TO WKS-REG-NOT-WRITE
                    DISPLAY "*************  ERROR  ******************"
                    DISPLAY "EL REGISTRO : " StudentID
                    DISPLAY "YA EXISTE EN EL REGISTRO ORIGINAL"
                    READ TRANSREC
                         AT END SET EndOfTransFile TO TRUE 
                    END-READ
              END-EVALUATE 
           END-PERFORM.
       130-WRITE-E. EXIT.
      *                  SECCION CUATRO ESTADISTICAS
       140-ESTADISTICS SECTION.
           DISPLAY ">>>>>>>>>>>>>>>>>>>ESTADISTICAS<<<<<<<<<<<<<<<<<<<"
           DISPLAY "* REGISTROS LEIDOS                 : (" 
                                                     WKS-REG-READ   " )"
           DISPLAY "* REGISTROS ESCRITOS               : (" 
                                                     WKS-REG-WRITEN " )"
           DISPLAY "* REGISTROS NO ESCRITOS/DUPLICADOS : (" 
                                                     WKS-REG-NOT-WRITE. 
       140-ESTADISTICS-E. EXIT.
      *                 SECCION CINCO CIERRE DE DATASETS
       150-CLOSE-DATA SECTION.
           CLOSE STUDREC 
           CLOSE TRANSREC 
           CLOSE NEWSTREC.
       150-CLOSE-DATA-E. EXIT.