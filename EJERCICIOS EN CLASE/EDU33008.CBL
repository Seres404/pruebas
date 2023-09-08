      ******************************************************************
      * FECHA       : 27/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33008                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA LEE LOS DATOS DE SYSIN LUEGO LOS   *
      *             : ORDENA POR GENERO Y LUEGO POR EDAD, ESTO LO ENVIA*
      *             : A LA COLA DE IMPRESION, TAMBIÉN VALIDA SI EL     *
      *             : CAMPO DE GENERO TIENE UN CARACTER DISTINTO       *
      *             : F = FEMENINO O M = MASCULINO                     *
      * ARCHIVOS    : NO APLICA                                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU33008.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT WORKREC          ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
       SD WORKREC.
       01 WORKAREA.
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

       WORKING-STORAGE SECTION.
       01 WKS-STADISTICAS.
          02 WKS-REG-TOTAL          PIC 9(03) VALUE ZEROS.
          02 WKS-MALES-READ         PIC 9(03) VALUE ZEROS.
          02 WKS-FEMALES-READ       PIC 9(03) VALUE ZEROS.
          02 WKS-REG-ERROR          PIC 9(03) VALUE ZEROS.
          01 WKS-GUIONES            PIC X(89) VALUE ALL "=".
          01 WKS-GUINES-PEQ         PIC X(70) VALUE ALL "=".
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-SORTING-DATA-ENTRY
           PERFORM 150-ESTADISTICAS
           STOP RUN.
       100-MAIN-E. EXIT.

      *---------> SECCION PARA ORDENAR LOS DATOS EN FUNCION
      *           DE LA RUTINA DE ENTRADA DE DATOS
       110-SORTING-DATA-ENTRY SECTION.
           SORT WORKREC       ON ASCENDING KEY  GENERO-ESTD
                                 ASCENDING KEY  EDAD-ESTUDIANTED
           INPUT PROCEDURE    IS 110-RECEIVING-INPUT-DATA
           OUTPUT PROCEDURE   IS 130-OUTPUT-DISPLAY-DATA.
       110-SORTING-DATA-ENTRY-E. EXIT.

      *---------> SECCION PARA RECIBIR LOS DATOS DEL ESTUDIANTE
       110-RECEIVING-INPUT-DATA SECTION.
           ACCEPT DATOS-ESTUDIANTE-SORT FROM SYSIN
           PERFORM UNTIL END-ENTRY-DATA
                   RELEASE WORKAREA
                   ADD 1 TO WKS-REG-TOTAL
                   EVALUATE TRUE
                      WHEN  MALESTUDENT
                            ADD 1 TO WKS-MALES-READ
                      WHEN  FEMALE
                            ADD 1 TO WKS-FEMALES-READ
                      WHEN OTHER
                       ADD 1 TO WKS-REG-ERROR
                       DISPLAY WKS-GUINES-PEQ
                       UPON CONSOLE
                       DISPLAY
                       "                  HAY UN REGISTRO CON ERRORES "
                       UPON CONSOLE
                       DISPLAY WKS-GUINES-PEQ
                       UPON CONSOLE
                       DISPLAY "==> EL REGISTRO EN CONFLICTO ES : "
                       UPON CONSOLE
                       DISPLAY DATOS-ESTUDIANTE-SORT
                       UPON CONSOLE
                       DISPLAY WKS-GUINES-PEQ
                       UPON CONSOLE
                   END-EVALUATE
                   ACCEPT DATOS-ESTUDIANTE-SORT FROM SYSIN
           END-PERFORM.
       110-RECEIVING-INPUT-DATA-E. EXIT.

      *---------> SECCION PARA ORDENAR LA SALIDA AL SYSOUT
       130-OUTPUT-DISPLAY-DATA SECTION.
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "            "
           "     LISTADO DE ESTUDIANTES SEMILLERO 2023 DATACENTRO S,A"
           DISPLAY WKS-GUIONES
           DISPLAY
           "  ID  |                NOMBRE COMPLETO                   |"
           " EDAD | CUMPLEAÑOS |  GENERO |".
           RETURN WORKREC
                AT END SET END-ENTRY-DATA TO TRUE
           END-RETURN
           PERFORM UNTIL END-ENTRY-DATA
                PERFORM 140-DISPLAYS
           END-PERFORM
               DISPLAY WKS-GUIONES.
       130-OUTPUT-DISPLAY-DATA-E. EXIT.

      * -----> SECCIÓN PARA FORMATEAR LA SALIDA DEL REGISTRO ORDENADO
       140-DISPLAYS SECTION.
           IF (GENERO-ESTD = "M") OR (GENERO-ESTD = "F")
           DISPLAY
           " " CODIGO-ESTUDIANTED "|" NOMBRE-ESTUDIANTED "|  "
           EDAD-ESTUDIANTED "  |  " FECHA-NACIMIENTO-ESTD
            "  |    " GENERO-ESTD "    |"
           END-IF.
            RETURN WORKREC
                AT END SET END-ENTRY-DATA TO TRUE
           END-RETURN.
       140-DISPLAYS-E. EXIT.
       150-ESTADISTICAS SECTION.
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "*                                 ESTADISTICAS  "
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
       150-ESTADISTICAS-E. EXIT.