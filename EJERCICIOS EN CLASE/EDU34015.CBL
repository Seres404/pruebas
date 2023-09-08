      ******************************************************************
      * FECHA       : 24/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34015                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA RECIBE DATOS DESDE SYSIN PARA LUEGO*
      *             : ORDENARLOS DE FORMA ASCENDENTE POR FORMA DE LLAVE*
      *             : MISMA LLAVE ES EL CODIGO DE UN ESTUDIANTE        *
      *             : AL SER UN SORT SIMPLE NO ES NECESARIO ABRIR EL   *
      *             : DATASET DE SALIDA, YA QUE ESTO LO HACE SORT DE   *
      *             : FORMA AUTOMATICA                                 *
      * ARCHIVOS    : EDUC.EDU34015.SALIDA.UNO                         *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU34015.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT STUDENTFILE ASSIGN TO STUDENT.
            SELECT WORKFILE    ASSIGN TO SORTWK1.
       DATA DIVISION.
       FILE SECTION.
       FD STUDENTFILE.
       01 STUDENTDETAILS                PIC X(30).
       SD WORKFILE.
       01 WORKREC.
          02 WSTUDENTID                 PIC 9(7).
          02 FILLER                     PIC X(23).
       WORKING-STORAGE SECTION.
       01 WKS-REG-TOTAL                 PIC 9(03) VALUE ZEROS.
       PROCEDURE DIVISION.
       100-MAIN SECTION.
            PERFORM 110-SORT-REGS
            PERFORM 130-STADISTICS
            STOP RUN.
       100-MAIN-E. EXIT.

       110-SORT-REGS SECTION.
           SORT WORKFILE ON ASCENDING KEY WSTUDENTID
              INPUT PROCEDURE IS 120-GETSTUDENTDETAILS
              GIVING STUDENTFILE.
       110-SORT-REGS-E. EXIT.

       120-GetStudentDetails SECTION.
            DISPLAY
            "**********************************************************"
            UPON CONSOLE
            DISPLAY
            "* INGRESE LOS DATOS DEL ESTUDIANTE DE LA SIGUIENTE FORMA *"
            UPON CONSOLE
            DISPLAY
            "**********************************************************"
            UPON CONSOLE
            DISPLAY
            "*    ID-APELLIDO-INICIALES-FNACIMIENTO-IDCURSO-GENERO    *"
            UPON CONSOLE
            DISPLAY
            "*             1234567NNNNNNNNINFNFNFNFNIDIDM             *"
            UPON CONSOLE
            DISPLAY
            "**********************************************************"
            UPON CONSOLE
           ACCEPT WORKREC FROM SYSIN
           PERFORM UNTIL WORKREC = SPACES
                   ADD 1 TO  WKS-REG-TOTAL
                   RELEASE WORKREC
                   MOVE SPACES TO WORKREC
                   ACCEPT WORKREC
           END-PERFORM.
       120-GetStudentDetails-E. EXIT.

       130-STADISTICS SECTION.
           DISPLAY
           "**********************************************************"
           DISPLAY
           "*                      ESTADISTICAS                      *"
           DISPLAY
           "**********************************************************"
           DISPLAY
           " REGISTROS LEIDOS EN TOTAL  : (" WKS-REG-TOTAL ")"
           DISPLAY
           "**********************************************************".
       130-STADISTICS-E. EXIT.