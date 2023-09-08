      ******************************************************************
      * FECHA       : 07/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33003, CORRECCIÓN DEL ANTERIOR                *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA RECIBE UNA CADENA DE NUMEROS  EN   *
      *             : SYSIN PARA  HACER VARIAS OPERACIONES ENTRE SI    *
      *             : EL OBJETO DEL PROGRAMA ES MOSTRAR EL USO DE OPE- *
      *             : RACIONES MATATICAS EN COBOL                      *
      * ARCHIVOS    : NO APLICA                                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EEDR3003.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *               VARIABLES DE ENTRADA
       01 WKS-DATA-ENTRY.
                                             -9999.99  HASTA 9999.99
          05 WKS-NUMBER-A               PIC S9(04)v99 VALUE ZEROS.
          05 FILLER                     PIC X        VALUE SPACES.
          05 WKS-NUMBER-B               PIC 9(04)v99 VALUE ZEROS.
          05 FILLER                     PIC X        VALUE SPACES.
          05 WKS-NUMBER-C               PIC 9(04)v99 VALUE ZEROS.
          05 FILLER                     PIC X        VALUE SPACES.
          05 WKS-NUMBER-D               PIC 9(04)v99 VALUE ZEROS.
          05 FILLER                     PIC X        VALUE SPACES.
          05 WKS-NUMBER-E               PIC 9(04)v99 VALUE ZEROS.
          05 FILLER                     PIC X        VALUE SPACES.
       01 WKS-DATA-BACKUP               PIC X(34)    VALUE SPACES.
       01 WKS-RESULTADOS-EDITADOS.
          05 WKS-TOTAL                  PIC 9(09)v99 VALUE ZEROS.
          05 WKS-TOTAL-EDIT             PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS.
          05 WKS-EDITED-A               PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS.
          05 WKS-EDITED-B               PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS.
          05 WKS-EDITED-C               PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS.
          05 WKS-EDITED-D               PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS.
          05 WKS-EDITED-E               PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS.
       PROCEDURE DIVISION.
       100-PRINCIPAL SECTION.
           PERFORM 110-ACEPTA-REGISTRO-ENTRADA
           PERFORM 120-OPERACIONES-ARTIMETICAS
           STOP RUN.
       100-PRINCIPAL-E. EXIT.

       110-ACEPTA-REGISTRO-ENTRADA SECTION.
           ACCEPT WKS-DATA-ENTRY FROM SYSIN
           MOVE WKS-DATA-ENTRY TO WKS-DATA-BACKUP.
       110-ACEPTA-REGISTRO-ENTRADA-E. EXIT.

       120-OPERACIONES-ARTIMETICAS SECTION.
           PERFORM 121-SUMA
           PERFORM 122-RESTA
           PERFORM 123-MULTIPLICACION
           PERFORM 124-DIVISION.
       120-OPERACIONES-ARTIMETICAS-E. EXIT.

       121-SUMA SECTION.
           DISPLAY
           "************************* S U M A *************************"
           PERFORM 130-ENCABEZADO
           DISPLAY
           "==> D = C + ( A + B + 5 )"
           COMPUTE WKS-NUMBER-D =
                   WKS-NUMBER-C + (WKS-NUMBER-A + WKS-NUMBER-B + 5)
           MOVE    WKS-TOTAL TO WKS-TOTAL-EDIT
           DISPLAY
           "    EL RESULTADO DE LA SUMA ES : "  WKS-TOTAL-EDIT
           PERFORM 131-PIE-DE-PAGINA
           PERFORM BACKUP.
       121-SUMA-E. EXIT.

       122-RESTA SECTION.
           DISPLAY*
           "***********************  R E S T A  ***********************"
           PERFORM 130-ENCABEZADO
           DISPLAY
           "==> A = A - C"
           COMPUTE WKS-NUMBER-A = WKS-NUMBER-A - WKS-NUMBER-C
           MOVE    WKS-NUMBER-A TO WKS-TOTAL
           DISPLAY
           "  > VALOR DE A LUEGO DE LA RESTA : " WKS-TOTAL
           DISPLAY
           "==> B = B - C"
           COMPUTE WKS-NUMBER-B = WKS-NUMBER-B - WKS-NUMBER-C
           MOVE    WKS-NUMBER-B TO WKS-TOTAL
           DISPLAY
           "  > VALOR DE B LUEGO DE LA RESTA : " WKS-TOTAL
           PERFORM 131-PIE-DE-PAGINA
           PERFORM BACKUP.
       122-RESTA-E. EXIT.

       123-MULTIPLICACION SECTION.
           DISPLAY
           "********************  MULTIPLICACION  ********************"
           PERFORM 130-ENCABEZADO
           DISPLAY
           "==> E = A * B"
           COMPUTE WKS-NUMBER-E = WKS-NUMBER-A * WKS-NUMBER-B
           MOVE    WKS-NUMBER-E TO WKS-TOTAL
           DISPLAY
           "  > VALOR DE A LUEGO DE (E) LA OPERACION : " WKS-TOTAL
           DISPLAY
           "==> C = E * B"
           COMPUTE WKS-NUMBER-C = WKS-NUMBER-E * WKS-NUMBER-B
           MOVE    WKS-NUMBER-C TO WKS-TOTAL
           DISPLAY
           "  > VALOR DE A LUEGO DE (C) LA OPERACION : " WKS-TOTAL
           PERFORM 131-PIE-DE-PAGINA
           PERFORM BACKUP.
       123-MULTIPLICACION-E. EXIT.

       124-DIVISION SECTION.
           DISPLAY
           "***********************  DIVISION 1 ***********************"
           PERFORM 130-ENCABEZADO
           DISPLAY
           "==> D = C / A"
           COMPUTE WKS-NUMBER-D = WKS-NUMBER-C / WKS-NUMBER-A
           MOVE    WKS-NUMBER-D TO WKS-TOTAL
           DISPLAY
           "  > VALOR DE A LUEGO DE (D) LA OPERACION : " WKS-TOTAL
           PERFORM 131-PIE-DE-PAGINA
           PERFORM BACKUP
           DISPLAY
           "***********************  DIVISION 2 ***********************"
           PERFORM 130-ENCABEZADO
           DISPLAY
           "==> D = A / C Y EL RESIDUO LO GUARDAMOS EN (E)"
           COMPUTE WKS-NUMBER-E =
           FUNCTION REM(WKS-NUMBER-A  WKS-NUMBER-C)
           MOVE    WKS-NUMBER-E TO WKS-TOTAL
           DISPLAY
           "  > VALOR DE A LUEGO DE (E) LA OPERACION : " WKS-TOTAL
           PERFORM 131-PIE-DE-PAGINA
           PERFORM BACKUP.
       124-DIVISION-E. EXIT.

      * ------------> SECCIÓN DE DISPLAYS GENERICOS
       130-ENCABEZADO SECTION.
           PERFORM 132-MOVES-VARIABLES-EDIT
           DISPLAY
           "==> VALOR DE LOS DATOS DE ENTRADA : "
           DISPLAY
           '==> EL VALOR DE A ES :' WKS-EDITED-A
           DISPLAY
           '==> EL VALOR DE B ES :' WKS-EDITED-B
           DISPLAY
           '==> EL VALOR DE C ES :' WKS-EDITED-C
           DISPLAY
           '==> EL VALOR DE D ES :' WKS-EDITED-D
           DISPLAY
           '==> EL VALOR DE E ES :' WKS-EDITED-E
           DISPLAY
           "******************** OPERACIÓN A REALIZAR ****************".
       130-ENCABEZADO-E. EXIT.

       131-PIE-DE-PAGINA SECTION.
           PERFORM 132-MOVES-VARIABLES-EDIT
           DISPLAY
           "**********************************************************"
           DISPLAY
           "==> LUEGO DE LA OPERACION LOS VALORES SON LOS SIGUIENTES :"
           DISPLAY
           '==> EL VALOR DE A ES :' WKS-EDITED-A
           DISPLAY
           '==> EL VALOR DE B ES :' WKS-EDITED-B
           DISPLAY
           '==> EL VALOR DE C ES :' WKS-EDITED-C
           DISPLAY
           '==> EL VALOR DE D ES :' WKS-EDITED-D
           DISPLAY
           '==> EL VALOR DE E ES :' WKS-EDITED-E
           DISPLAY
           "**********************************************************"
           DISPLAY " "
           DISPLAY " ".
       131-PIE-DE-PAGINA-E. EXIT.

       132-MOVES-VARIABLES-EDIT SECTION.
           MOVE ZEROS          TO WKS-RESULTADOS-EDITADOS
           MOVE  WKS-NUMBER-A  TO WKS-EDITED-A
           MOVE  WKS-NUMBER-B  TO WKS-EDITED-B
           MOVE  WKS-NUMBER-C  TO WKS-EDITED-C
           MOVE  WKS-NUMBER-D  TO WKS-EDITED-D
           MOVE  WKS-NUMBER-E  TO WKS-EDITED-E.
       132-MOVES-VARIABLES-EDIT-E. EXIT.

       BACKUP SECTION.
           MOVE WKS-DATA-BACKUP TO WKS-DATA-ENTRY.
       BACKUP-E. EXIT.