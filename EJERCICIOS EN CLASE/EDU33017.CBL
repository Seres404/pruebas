      ******************************************************************
      * FECHA       : 20/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33017                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA LEE LOS DATOS DE SYSIN LUEGO LOS   *
      *             : ORDENA POR GENERO Y LUEGO POR CODIGO, ESTO LO    *
      *             : ESCRIBE EN UN REPORTE (PS) HACIENDO UNA SEPARACION*
      *             : POR GENEROS Y DANDO EL DETALLE DE PAGOS REALIZADO*
      * ARCHIVOS    : NO APLICA                                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU33017.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS "Q" WITH PICTURE SYMBOL "Q".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT STUDENT  ASSIGN TO STUDENT
                            FILE STATUS IS FS-STUDENT.
            SELECT WORKFILE ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
       FD STUDENT
       REPORT IS REPORTE-ESTUDIANTES.
       SD WORKFILE.
       01 WORK-REG.
          88 END-ENTRY-DATA            VALUE "FIN".
          02 DATOS-ESTUDIANTE-SORT.
             05 CODIGO-ESTUDIANTED     PIC 9(05).
             05                        PIC X(01).
             05 NOMBRE-ESTUDIANTED     PIC X(50).
             05                        PIC X(01).
             05 EDAD-ESTUDIANTED       PIC 9(02).
             05                        PIC X(01).
             05 FECHA-NACIMIENTO-ESTD  PIC 9(08).
             05                        PIC X(01).
             05 GENERO-ESTD            PIC X(01).
                88 MALESTUDENT                   VALUE "M".
                88 FEMALE                        VALUE "F".
             05                        PIC X(01).
             05 VALOR-PAGO             PIC 9(05)V99.
       WORKING-STORAGE SECTION.
       01 FS-STUDENT                   PIC 9(02) VALUE ZEROS.
      *     VARIABLES PARA OPERACION DE DATOS
       01 WKS-OPERACIONES.
          02 WKS-REG-TOTAL             PIC 9(04) VALUE ZEROS.
          02 WKS-MALES-READ            PIC 9(03) VALUE ZEROS.
          02 WKS-FEMALES-READ          PIC 9(03) VALUE ZEROS.
          02 WKS-REG-ERROR             PIC 9(03) VALUE ZEROS.
          02 WKS-GUIONES               PIC X(55) VALUE ALL "*".
       01 WKS-FECHA.
          02 WKS-DIA                   PIC 9(02) VALUE ZEROS.
          02 FILLER1                   PIC X(01) VALUE '/'.
          02 WKS-MES                   PIC 9(02) VALUE ZEROS.
          02 FILLER2                   PIC X(01) VALUE '/'.
          02 WKS-ANIO                  PIC 9(04) VALUE ZEROS.
      ******************************************************************
      *                  MAQUETACION REPORTE DE SALIDA                 *
      ******************************************************************
       REPORT SECTION.
       RD  REPORTE-ESTUDIANTES
           CONTROLS ARE GENERO-ESTD
           PAGE LIMIT IS 20
           HEADING 1
           FIRST DETAIL 7
           LAST DETAIL 16
           FOOTING 18.
      ******************************************************************
      *                     MAQUETACION PAGE HEADER                    *
      ******************************************************************
       01  TYPE IS PH.
           02 LINE 1.
              03 COLUMN   1 PIC X(22) VALUE 'BANCO INDUSTRIAL, S.A.'.
              03 COLUMN  38 PIC X(50) VALUE
                  '       REPORTE ESTUDIANTES DEL SEMILLERO 2023'.
              03 COLUMN 106 PIC X(24) VALUE '      S E M I L L E R O'.
           02 LINE 2.
              03 COLUMN   1 PIC X(25) VALUE 'EDU33017    00.26042023.R'.
              03 COLUMN  49 PIC X(20) VALUE 'FECHA DEL MOVIMIENTO'.
              03 COLUMN  70 PIC X(10) SOURCE WKS-FECHA.
              03 COLUMN 120 PIC X(06) VALUE 'PAGINA'.
              03 COLUMN 129 PIC Z(04) SOURCE PAGE-COUNTER
                                      IN REPORTE-ESTUDIANTES.
           02 LINE 3.
              03 COLUMN  03 PIC X(127) VALUE ALL '='.
           02 LINE 4.
              03 COLUMN  03 PIC X(22) VALUE  '*  CODIGO ESTUDIANTE |'.
              03 COLUMN  42 PIC X(15) VALUE  'NOMBRE COMPLETO'.
              03 COLUMN  76 PIC X(23) VALUE '| EDAD |  CUMPLEAÑOS  |'.
              03 COLUMN 102 PIC X(28) VALUE
                                         'GENERO   |     VALOR PAGO  *'.
           02 LINE 5.
              03 COLUMN  03 PIC X(127) VALUE ALL '='.
      ******************************************************************
      *                     MAQUETACION LINEA DETALLE                  *
      ******************************************************************
       01  DETAILLINE TYPE IS DETAIL.
           02 LINE IS PLUS 1.
              03 COLUMN 12  PIC X(05) SOURCE CODIGO-ESTUDIANTED.
              03 COLUMN 24  PIC X(01) VALUE "|".
              03 COLUMN 26  PIC X(50) SOURCE NOMBRE-ESTUDIANTED.
              03 COLUMN 76  PIC X(01) VALUE "|".
              03 COLUMN 79  PIC Z(02) SOURCE EDAD-ESTUDIANTED.
              03 COLUMN 83  PIC X(01) VALUE "|".
              03 COLUMN 85  PIC ZZ/ZZ/ZZZZ SOURCE FECHA-NACIMIENTO-ESTD.
              03 COLUMN 98  PIC X(01) VALUE "|".
              03 COLUMN 101 PIC X(09) VALUE "MASCULINO" PRESENT WHEN
                                        MALESTUDENT.
              03 COLUMN 101 PIC X(09) VALUE "FEMENINO " PRESENT WHEN
                                        FEMALE.
              03 COLUMN 111 PIC X(01) VALUE "|".
              03 COLUMN 118 PIC QQ,QQ9.99 SOURCE VALOR-PAGO.
      ******************************************************************
      *                 MAQUETACION CONTROL BREAK HEADING              *
      ******************************************************************
       01  GENEROS  TYPE IS CH.
           02 LINE IS PLUS 1.
              03 COLUMN 43  PIC X(43) VALUE
              "================= HOMBRES ================="
              PRESENT WHEN MALESTUDENT.
           02 LINE IS PLUS 1.
              03 COLUMN 43  PIC X(43) VALUE
              "================= MUJERES ================="
              PRESENT WHEN FEMALE.
      ******************************************************************
      *                   MAQUETACION CONTROL FOOTING                  *
      ******************************************************************
       01  FINGENERO
           TYPE IS CONTROL FOOTING GENERO-ESTD   NEXT GROUP PLUS 2.

           02 LINE IS PLUS 1.
              03 COLUMN 114 PIC X(17) VALUE "-----------------".
           02 LINE IS PLUS 1.
              03 COLUMN 11  PIC X(25) VALUE
                "TOTAL HOMBRES INSCRITOS =" PRESENT WHEN MALESTUDENT.
              03 COLUMN 11  PIC X(25) VALUE
                "TOTAL MUJERES INSCRITAS =" PRESENT WHEN FEMALE.
              03 COLUMN 39  PIC ZZZZ COUNT OF DETAILLINE.
              03 COLUMN 77  PIC X(35) VALUE
                                  "TOTAL DE PAGOS RECIBIDOS          =".
              03 COLUMN 115 PIC QQQ,QQ9.99 SUM  VALOR-PAGO.
           02 LINE IS PLUS 1
              03 COLUMN 3   PIC X(127) VALUE ALL SPACES.
           02 LINE IS PLUS 1.
              03 COLUMN 3   PIC X(127) VALUE ALL SPACES.
      ******************************************************************
      *                     MAQUETACION PAGE FOOTING                   *
      ******************************************************************
       01  TYPE IS PF.
           02  LINE PLUS 0.
              03 COLUMN 1   PIC X(25) VALUE 'FECHA Y HORA DE OPERACION'.
              03 COLUMN 30  PIC <99/<99/9999  FUNC MDATE.
              03 COLUMN 42  PIC 99,99,99      FUNC TIME.
              03 COLUMN 52  PIC X(16) VALUE 'DATA-CENTRO S.A.'.
              03 COLUMN 120 PIC X(6)  VALUE 'PAGINA'.
              03 COLUMN 129 PIC Z(04) SOURCE PAGE-COUNTER IN
                                             REPORTE-ESTUDIANTES.
       01  TYPE IS RF.
           02 LINE IS PLUS 1.
              03 COLUMN 114 PIC X(17) VALUE "-----------------".
           02 LINE IS PLUS 1
              03 COLUMN 11  PIC X(23) VALUE "TOTAL DE ESTUDIANTES ES".
              03 COLUMN 35  PIC Z,ZZZ COUNT OF DETAILLINE.
              03 COLUMN 77  PIC X(35) VALUE
                                  "TOTAL DE PAGOS RECIBIDOS          =".
              03 COLUMN 115 PIC Q,QQQ,QQ9.99 SUM VALOR-PAGO.
           02  LINE PLUS 3.
               03 COLUMN 50 PIC X(22) VALUE 'ESTA ES LA ULTIMA HOJA'.
           02  LINE PLUS 1.
               03 COLUMN 50 PIC X(23) VALUE 'ESTE REPORTE CONSTA DE '.
               03 COLUMN 73 PIC Z,ZZZ SOURCE PAGE-COUNTER IN
                                              REPORTE-ESTUDIANTES.
               03 COLUMN 80 PIC X(07) VALUE 'PAGINAS'.
       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 100-ABRE-ARCHIVOS
           PERFORM 200-SORT-Y-PROCESAMIENTO
           PERFORM 300-ESTADISTICAS
           PERFORM 999-CLOSE-DATASET
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

       100-ABRE-ARCHIVOS SECTION.
           OPEN OUTPUT STUDENT
           INITIATE REPORTE-ESTUDIANTES
           ACCEPT WKS-FECHA FROM SYSIN
           IF FS-STUDENT = 97
              MOVE ZEROS TO FS-STUDENT
           END-IF
           IF FS-STUDENT NOT = 0
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY "       HUBO UN ERROR AL ABRIR LOS ARCHIVOS      "
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              DISPLAY " FILE ESTATUS DEL ARCHIVO ES  : (" FS-STUDENT ")"
                       UPON CONSOLE
              DISPLAY "================================================"
                       UPON CONSOLE
              PERFORM 999-CLOSE-DATASET
              MOVE 91 TO RETURN-CODE
              STOP RUN
           ELSE
              DISPLAY
              "********** APERTURA DE ARCHIVOS EXITOSA ********"
              UPON CONSOLE
           END-IF.
       100-ABRE-ARCHIVOS-E. EXIT.

      * ------> SERIES 200 ENCARGADAS DE HACER ORDENAMIENTO DE ENTRADA
       200-SORT-Y-PROCESAMIENTO SECTION.
           SORT WORKFILE ON ASCENDING KEY GENERO-ESTD        OF WORK-REG
                            ASCENDING KEY CODIGO-ESTUDIANTED OF WORK-REG
                            ASCENDING KEY EDAD-ESTUDIANTED   OF WORK-REG
              INPUT  PROCEDURE   IS 210-VERIFICACION-SYSIN
              OUTPUT PROCEDURE   IS 220-OUTPUT-DATOS-FORMATEADOS.
       200-SORT-Y-PROCESAMIENTO-E. EXIT.
      *---------> SECCION PARA RECIBIR LOS DATOS DEL ESTUDIANTE
       210-VERIFICACION-SYSIN SECTION.
           ACCEPT DATOS-ESTUDIANTE-SORT FROM SYSIN
           PERFORM UNTIL END-ENTRY-DATA
                   ADD 1 TO WKS-REG-TOTAL
                   EVALUATE TRUE
                      WHEN  MALESTUDENT
                            ADD 1 TO WKS-MALES-READ
                            RELEASE WORK-REG
                      WHEN  FEMALE
                            ADD 1 TO WKS-FEMALES-READ
                            RELEASE WORK-REG
                      WHEN OTHER
                            PERFORM 230-REGISTROS-NO-VALIDOS
                   END-EVALUATE
                   ACCEPT DATOS-ESTUDIANTE-SORT FROM SYSIN
           END-PERFORM.
       210-VERIFICACION-SYSIN-E. EXIT.

       220-OUTPUT-DATOS-FORMATEADOS SECTION.
           RETURN WORKFILE
                 AT END SET END-ENTRY-DATA TO TRUE
           END-RETURN

           PERFORM UNTIL END-ENTRY-DATA
                GENERATE DETAILLINE
                RETURN WORKFILE
                     AT END SET END-ENTRY-DATA TO TRUE
                END-RETURN
           END-PERFORM.
       220-OUTPUT-DATOS-FORMATEADOS-E. EXIT.

       230-REGISTROS-NO-VALIDOS SECTION.
           ADD 1 TO WKS-REG-ERROR
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*       ERROR, SE HA INGRESADO UN REGISTRO INVALIDO      *"
           DISPLAY
           "**********************************************************"
           DISPLAY
           " ==> ES EL REGISTRO NO. (" WKS-REG-TOTAL ") DESDE SYSIN"
           DISPLAY
           "          =====  POSIBLES CAUSAS DEL ERROR  ====="
           DISPLAY
           " 1.- EL REGISTRO ESTABA VACIO"
           DISPLAY
           " 2.- EL GENERO ERA DISTINTO A (M) O (F)"
           DISPLAY
           " NOTA: EL REGISTRO FUE IGNORADO, VERIFIQUE SU ENTRADA"
           DISPLAY
           " VISTA PREVIA DEL REGISTRO ES: (EN CASO NO SER VACIO)"
           DISPLAY
           " ==> " DATOS-ESTUDIANTE-SORT
           DISPLAY
           "**********************************************************"
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       230-REGISTROS-NO-VALIDOS-E. EXIT.

       300-ESTADISTICAS SECTION.
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "*                   ESTADISTICAS"
           DISPLAY WKS-GUIONES
           DISPLAY
           " TOTAL DE REGISTROS LEIDOS : (" WKS-REG-TOTAL ")"
           DISPLAY
           " TOTAL DE HOMBRES          : (" WKS-MALES-READ ")"
           DISPLAY
           " TOTAL DE MUJERES          : (" WKS-FEMALES-READ ")"
           DISPLAY
           " REGISTROS CON ERROR       : (" WKS-REG-ERROR
           ") REVISE LA CONSOLA ! "
           DISPLAY WKS-GUIONES.
       300-ESTADISTICAS-E. EXIT.

       999-CLOSE-DATASET SECTION.
           TERMINATE REPORTE-ESTUDIANTES
           CLOSE STUDENT.
       999-CLOSE-DATASET-E. EXIT.