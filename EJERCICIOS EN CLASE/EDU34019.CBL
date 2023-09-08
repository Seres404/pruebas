      ******************************************************************
      * FECHA       : 30/03/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU34019                                         *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE ES UN PROGRAMA QUE LEE EN SYSIN UN CODIGO DE*
      *             : BUSQUEDA Y UNA LLAVE ESTA PUEDE SER EL CODIGO DE *
      *             : UNA PELICULA O BIEN SU NOMBRE PARA UBICARLA EN   *
      *             : UN KSDS Y SU INDICE ALTERNO, LUEGO DE ESO BASADO *
      *             : EN QUE SI SE ENCUNTRA LA LLAVE MUESTRA EN SPOOL  *
      *             : EL DETALLE DE LA PELICULA
      * ARCHIVOS    : EDUC.EDU34018.ENTRADA.UNO (ORDENADO PREV)        *
      *             : EDUC.EDU34018.KSDS01 (VSAM KSDS)                 *
      * PROGRAMA(S) : RUTINA DEBD1R00, FILE STATUS EXTENDED            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDU34019.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT EDPELI ASSIGN TO EDPELI
                   ORGANIZATION  IS INDEXED
                   ACCESS MODE   IS DYNAMIC
                   RECORD KEY    IS EDPL-LLAVE
                   ALTERNATE RECORD KEY IS EDPL-VIDEOTITLE
                                    WITH DUPLICATES
                   FILE STATUS   IS  FS-EDPELI
                                     FSE-EDPELI.
       DATA DIVISION.
       FILE SECTION.
       FD  EDPELI.
           COPY EDPELI.
       WORKING-STORAGE SECTION.
       01 WKS-REG-ENTRADA.
           02 WKS-OPERACION            PIC 9(01).
              88 WKS-CODIGO                     VALUE 1.
              88 WKS-NOMBRE                     VALUE 2.
           02 FILLER                   PIC X(40) VALUE SPACES.
       01 WKS-REG-NUM REDEFINES WKS-REG-ENTRADA.
           02 FILLER                   PIC X(01).
           02 WKS-CODIGO-NUM           PIC 9(05).
           02 FILLER                   PIC X(35).
       01 WKS-REG-NOMBRE REDEFINES WKS-REG-ENTRADA.
           02 FILLER                   PIC X(01).
           02 WKS-NOMBRE-PELI          PIC X(40).
       01 WKS-TEXT REDEFINES WKS-REG-ENTRADA.
           02 FILLER                   PIC X(03).
              88 WKS-END                         VALUE "FIN".
           02 FILLER                   PIC X(37).
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
       01  WKS-REG-LEIDOS-ENTRADA      PIC 9(02) VALUE ZEROS.
       01  WKS-REG-ERROR404            PIC 9(02) VALUE ZEROS.
       01  WKS-GUIONES                 PIC X(58) VALUE ALL "=".
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 110-OPEN-DATASET
           PERFORM 120-SEARCH-DATA
           PERFORM 140-ESTADISTICAS
           PERFORM XXX-CLOSE-DATASETS
           STOP RUN.
       100-MAIN-E. EXIT.

       110-OPEN-DATASET SECTION.
           MOVE 'EDU34018'      TO PROGRAMA
           OPEN INPUT EDPELI
           IF FS-EDPELI EQUAL 97
              MOVE ZEROS TO FS-EDPELI
                            FSE-EDPELI
           END-IF
           IF FS-EDPELI  NOT EQUAL 0
              MOVE 'OPEN'   TO ACCION
              MOVE SPACES   TO LLAVE
              MOVE 'EDPELI' TO ARCHIVO
              CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION, LLAVE
                                    FS-EDPELI, FSE-EDPELI
              MOVE  91  TO RETURN-CODE
              PERFORM XXX-CLOSE-DATASETS
              STOP RUN
           END-IF.
       110-OPEN-DATASET-E. EXIT.

       120-SEARCH-DATA SECTION.
           ACCEPT WKS-REG-ENTRADA
           PERFORM UNTIL WKS-END
                ADD 1 TO WKS-REG-LEIDOS-ENTRADA
                DISPLAY WKS-GUIONES
                DISPLAY
                "                   INVENTARIO DE PELICULAS"
                DISPLAY WKS-GUIONES
                DISPLAY
                "==> TIPO DE BUSQUEDA : "
                DISPLAY
               "    1 PARA CODIGO DE PELICULA 2 PARA NOMBRE DE PELICULA"

                DISPLAY
                "==> HAS SELECCIONADO EL TIPO DE BUSQUEDA : ("
                                                       WKS-OPERACION ")"
                EVALUATE TRUE
                   WHEN WKS-CODIGO
                        PERFORM 000-CODIGO-PELICULA-SEARCH
                   WHEN WKS-NOMBRE
                        PERFORM 000-NOMBRE-PELICULA-SEARCH
                   WHEN OTHER
                        DISPLAY WKS-GUIONES
                        DISPLAY
                       "==>  OH NO HAS INGRESADO UNA OPCIÓN INVALIDA"
                        DISPLAY
                       "==>  RECUERDA QUE SOLO PUEDES SELECCIONAR 1 O 2"
                       DISPLAY WKS-GUIONES
                END-EVALUATE
                ACCEPT WKS-REG-ENTRADA
           END-PERFORM.
       120-SEARCH-DATA-E. EXIT.

       000-CODIGO-PELICULA-SEARCH SECTION.
            MOVE WKS-CODIGO-NUM TO EDPL-LLAVE
            DISPLAY
            "==> ESTAMOS BUSCANDO EL CONDIGO... : ( " EDPL-LLAVE ")"
            READ  EDPELI
                  KEY IS EDPL-LLAVE
            END-READ.
            EVALUATE TRUE
               WHEN FS-EDPELI = 0
                    PERFORM 130-DISPLAY-PELICULA-DETALLE
               WHEN FS-EDPELI = 23
                    ADD 1 TO WKS-REG-ERROR404
                    DISPLAY WKS-GUIONES
                    DISPLAY
                    "         OH NO LA PELI QUE BUSCAS NO LA TENEMOS"
                    DISPLAY
                    "               ===>POSIBLES CAUSAS<==="
                    DISPLAY
                    " 1)  ES POSIBLE QUE EL CODIGO ESTÉ MAL ESCRITO"
                    DISPLAY
                    " 2)  SOLO ACEPTAMOS 5 DIGITOS SIN ESPACIOS"
                    DISPLAY
                    " 3)  NO ES POSIBLE VALIDAR LETRAS "
                    DISPLAY
                    " SI TODO FUE ESCRITO EN ORDEN LA PELI NO ESTÁ "
                    DISPLAY
                    " EN EL STOCK, LO SENTIMOS MUCHO"
                    DISPLAY WKS-GUIONES
                    DISPLAY " "
                    DISPLAY " "
                    DISPLAY " "
               WHEN OTHER
                    MOVE 'EDU34019' TO PROGRAMA
                    IF FSE-EDPELI  NOT EQUAL 0 AND 23
                       MOVE 'READ'   TO ACCION
                       MOVE SPACES   TO LLAVE
                       MOVE 'EDPELI' TO ARCHIVO
                       CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-EDPELI, FSE-EDPELI
                       MOVE  91  TO RETURN-CODE
                       PERFORM XXX-CLOSE-DATASETS
                       STOP RUN
                    END-IF
            END-EVALUATE.
       000-CODIGO-PELICULA-SEARCH-E. EXIT.

       000-NOMBRE-PELICULA-SEARCH SECTION.
            MOVE WKS-NOMBRE-PELI  TO EDPL-VIDEOTITLE
            DISPLAY
            "==> ESTAMOS BUSCANDO LA PELICULA... :  " EDPL-VIDEOTITLE
            READ EDPELI KEY IS EDPL-VIDEOTITLE
            END-READ
            EVALUATE TRUE
               WHEN FS-EDPELI = 0
                    PERFORM 130-DISPLAY-PELICULA-DETALLE
               WHEN FS-EDPELI = 23
                    ADD 1 TO WKS-REG-ERROR404
                    DISPLAY WKS-GUIONES
                    DISPLAY
                    "        OH NO LA PELI QUE BUSCAS NO LA TENEMOS"
                    DISPLAY
                    "               ===>POSIBLES CAUSAS<==="
                    DISPLAY
                    " 1)  ES POSIBLE QUE EL NOMBRE ESTÉ MAL ESCRITO"
                    DISPLAY
                    " 2)  SOLO ACEPTAMOS HASTA 40 CARACTERES"
                    DISPLAY
                    " SI TODO FUE ESCRITO EN ORDEN LA PELI NO ESTÁ "
                    DISPLAY
                    " EN EL STOCK, LO SENTIMOS MUCHO"
                    DISPLAY WKS-GUIONES
                    DISPLAY " "
                    DISPLAY " "
                    DISPLAY " "
               WHEN OTHER
                    MOVE 'EDU34019' TO PROGRAMA
                    IF FSE-EDPELI  NOT EQUAL 0 AND 23
                       MOVE 'READ'   TO ACCION
                       MOVE SPACES   TO LLAVE
                       MOVE 'EDPELI' TO ARCHIVO
                       CALL 'DEBD1R00' USING PROGRAMA, ARCHIVO, ACCION,
                                           LLAVE, FS-EDPELI, FSE-EDPELI
                       MOVE  91  TO RETURN-CODE
                       PERFORM XXX-CLOSE-DATASETS
                       STOP RUN
                    END-IF
            END-EVALUATE.
       000-NOMBRE-PELICULA-SEARCH-E. EXIT.

       130-DISPLAY-PELICULA-DETALLE SECTION.
           DISPLAY WKS-GUIONES
           DISPLAY
           "*        EXCELENTE, TENEMOS ESTA PELICULA EN STOCK       *"
           DISPLAY WKS-GUIONES
           DISPLAY
           "==> NOMBRE DE PELICULA : " EDPL-VIDEOTITLE
           DISPLAY
           "==> CODIGO DE PELICULA : " EDPL-LLAVE
           DISPLAY
           "==> CODIGO PROVEEDOR   : " EDPL-VIDEOSUPPLIERCODE
           DISPLAY WKS-GUIONES
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       130-DISPLAY-PELICULA-DETALLE-E. EXIT.
       140-ESTADISTICAS SECTION.
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY WKS-GUIONES
           DISPLAY
           "                          ESTADISTICAS"
           DISPLAY WKS-GUIONES
           DISPLAY
           " TOTAL DE BUSQUEDAS REALIZADAS :" WKS-REG-LEIDOS-ENTRADA
           DISPLAY
           " TOTAL DE BUSQUEDAS CON ERROR  :" WKS-REG-ERROR404
           DISPLAY WKS-GUIONES.
       140-ESTADISTICAS-E. EXIT.
       XXX-CLOSE-DATASETS SECTION.
           CLOSE EDPELI.
       XXX-CLOSE-DATASETS-E. EXIT.