      ******************************************************************
      * FECHA       : 30/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34018                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE ES UN PROGRAMA QUE LEE DATOS ORDENADOS      *
      *             : DE UN ARCHIVO DE ENTRADA PS, LUEGO LOS TRANSFIERE*
      *             : A UN DATASET KSDS ORDENADO POR LLAVE             *
      *             : LUEGO MUESTRA UNA ESTADISTICA DE LOS REGISTROS   *
      *             : LEIDOS, ESCRITOS E IGNORADOS POR SER DUPLICADOS  *
      *             : PARA LOS DUPLICADOS ENVIA UN MENSAJE A CONSOLA   *
      * ARCHIVOS    : EDUC.EDU34018.ENTRADA.UNO (ORDENADO PREV)        *
      *             : EDUC.EDU34018.KSDS01 (VSAM KSDS)                 *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU34018.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT PELIS ASSIGN TO PELIS
                   FILE STATUS  IS FS-PELIS.
            SELECT EDPELI ASSIGN TO EDPELI
                  ORGANIZATION  IS  INDEXED
                  ACCESS        IS  SEQUENTIAL
                  RECORD KEY    IS  EDPL-LLAVE
                  FILE STATUS   IS  FS-EDPELI
                                    FSE-EDPELI.
       DATA DIVISION.
       FILE SECTION.
       FD  EDPELI.
           COPY EDPELI.
       FD  PELIS.
       01 SEQVIDEORECORD.
          02 SEQVIDEOCODE          PIC 9(5).
          02 SEQVIDEOTITLE         PIC X(40).
          02 SEQVIDEOSUPPLIERCODE  PIC 99.
          02 FILLER                PIC X(13).
       WORKING-STORAGE SECTION.
      *Variables de File Status
       01  FS-PELIS                    PIC 9(02) VALUE ZEROS.
       01  FS-EDPELI                   PIC 9(02) VALUE ZEROS.
       01  FSE-EDPELI.
           02 FSE-RETURN               PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FUNCTION             PIC S9(04) COMP-5 VALUE ZEROS.
           02 FSE-FEEDBACK             PIC S9(04) COMP-5 VALUE ZEROS.
      *Variables de Rutina para control de File Status Extendido
       01  PROGRAMA                    PIC X(08) VALUE SPACES.
       01  ARCHIVO                     PIC X(08) VALUE SPACES.
       01  ACCION                      PIC X(10) VALUE SPACES.
       01  LLAVE                       PIC X(32) VALUE SPACES.
      *Contadores y Controladores
       01  WKS-FIN-PELIS               PIC 9(02) VALUE ZEROS.
       01  WKS-REG-LEIDOS-ENTRADA      PIC 9(02) VALUE ZEROS.
       01  WKS-REG-ESCRITOS-SALIDA     PIC 9(02) VALUE ZEROS.
       01  WKS-REG-DUPLICADOS          PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATASETS
           PERFORM 120-READING-DATA
           PERFORM 140-STADISTICS
           PERFORM XXX-CLOSE-DATASETS
           STOP RUN.
       100-MAIN-E. EXIT.

      *----->        SECCIÓN PARA LA APERTURA DE DATASETS
       110-OPEN-DATASETS SECTION.
           MOVE 'EDU34018'      TO PROGRAMA
           OPEN INPUT PELIS.
           OPEN OUTPUT EDPELI.
           IF FS-EDPELI EQUAL 97
              MOVE ZEROS TO FS-EDPELI
                            FSE-EDPELI
           END-IF
           IF FS-PELIS  EQUAL 97
              MOVE ZEROS TO FS-PELIS
           END-IF

           IF FS-EDPELI  NOT EQUAL 0
              MOVE 'OPEN'   TO ACCION
              MOVE SPACES   TO LLAVE
              MOVE 'EDPELI' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE
                                    FS-EDPELI, FSE-EDPELI
              MOVE  91  TO RETURN-CODE
              PERFORM XXX-CLOSE-DATASETS
              STOP  RUN
           END-IF
           IF FS-PELIS  NOT EQUAL 0
              DISPLAY '********************************* ' UPON CONSOLE
              DISPLAY 'ERROR ABRIR ARCHIVOS ' FS-PELIS     UPON CONSOLE
              DISPLAY '********************************* ' UPON CONSOLE
              MOVE  91  TO RETURN-CODE
              PERFORM XXX-CLOSE-DATASETS
              STOP  RUN
           END-IF.
       110-OPEN-DATASETS-E. EXIT.

      *--------> SECCION PARA LEER DATOS DE ENTRADA (PS)
       120-READING-DATA SECTION.
           READ PELIS NEXT RECORD
                AT END MOVE 1 TO WKS-FIN-PELIS
           END-READ
           PERFORM UNTIL WKS-FIN-PELIS = 1
             ADD 1 TO WKS-REG-LEIDOS-ENTRADA
             PERFORM 130-WRITE-DATA
             READ PELIS
                AT END MOVE 1 TO WKS-FIN-PELIS
             END-READ
           END-PERFORM.
       120-READING-DATA-E. EXIT.

      *--------> SECCION DE ESCRITURA DE DATOS Y VALIDACION
       130-WRITE-DATA SECTION.
           WRITE REG-EDPELI FROM SEQVIDEORECORD
           IF FS-EDPELI NOT EQUAL 0 AND 22
              MOVE 'WRITE'    TO ACCION
              MOVE EDPL-LLAVE TO LLAVE
              MOVE 'EDPELI'   TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE
                 FS-EDPELI, FSE-EDPELI
              MOVE 91 TO RETURN-CODE
              PERFORM XXX-CLOSE-DATASETS
              STOP RUN
           END-IF

           IF FS-EDPELI = 22
              ADD 1 TO WKS-REG-DUPLICADOS
              SUBTRACT 1 FROM WKS-REG-ESCRITOS-SALIDA
              DISPLAY '********************************* ' UPON CONSOLE
              DISPLAY '*         ---ALERTA---          * ' UPON CONSOLE
              DISPLAY '********************************* ' UPON CONSOLE
              DISPLAY
                 'HAY UN REGISTRO DUPLICADO CON LLAVE:' EDPL-LLAVE
                 UPON CONSOLE
              DISPLAY 'EL REGISTRO FUE IGNORADO' UPON CONSOLE
              DISPLAY '********************************* ' UPON CONSOLE
           END-IF
           ADD 1 TO WKS-REG-ESCRITOS-SALIDA.
       130-WRITE-DATA-E. EXIT.

      *------> SECCION DE ESTADISTICAS SOBRE REGISTROS PROCESADOS
       140-STADISTICS SECTION.
           DISPLAY
           "=========================================================="
           DISPLAY
           "*                      ESTADISTICAS                      *"
           DISPLAY
           "=========================================================="
           DISPLAY
           "| REGISTROS LEÍDOS EN TOTAL   : (" WKS-REG-LEIDOS-ENTRADA
                                               ")                     |"
           DISPLAY
           "| REGISTROS ESCRITOS EN TOTAL : (" WKS-REG-ESCRITOS-SALIDA
                                               ")                     |"

            DISPLAY
           "| DUPLICADOS / IGNORADOS      : (" WKS-REG-DUPLICADOS
                                               ")                     |"
           DISPLAY
           "==========================================================".
       140-STADISTICS-E. EXIT.
       XXX-CLOSE-DATASETS SECTION.
           CLOSE PELIS
           CLOSE EDPELI.
       XXX-CLOSE-DATASETS-E. EXIT.