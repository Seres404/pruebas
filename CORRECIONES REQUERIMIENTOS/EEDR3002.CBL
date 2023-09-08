      ******************************************************************
      * FECHA       : 07/04/2023                                       *
      * PROGRAMADOR : ERICK DANIEL RAMIREZ DIVAS                       *
      * APLICACION  : EDUCACION                                        *
      * PROGRAMA    : EDU33002, CORRECCIÓN DEL ANTERIOR                *
      * TIPO        : BATCH                                            *
      * DESCRIPCION : ESTE PROGRAMA ANALIZA DOS CADENAS DE TEXTO, LAS  *
      *             : PARA VALIDAR QUE TIPO DE CARACTER SE ESTÁ INGRE- *
      *             : SANDO, PUEDE SER ALFABETICO, VOCAL, NUMERICO,PAR *
      *             : O IMPAR.                                         *
      * ARCHIVOS    : NO APLICA                                        *
      * PROGRAMA(S) : NO APLICA                                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EEDR3002.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * --------->          VARIABLES PARRAFO UNO
       01 WKS-REGISTRO1.
          02 WKS-CAMPO1          PIC X(01) VALUE SPACES.
          02 FILLER              PIC X(01) VALUE SPACES.
          02 WKS-CAMPO2          PIC X(01) VALUE SPACES.
          02 FILLER              PIC X(01) VALUE SPACES.
          02 WKS-CAMPO3          PIC X(01) VALUE SPACES.
          02 FILLER              PIC X(01) VALUE SPACES.
          02 WKS-CAMPO4          PIC X(01) VALUE SPACES.
          02 FILLER              PIC X(01) VALUE SPACES.
          02 WKS-CAMPO5          PIC X(01) VALUE SPACES.

      * --------->          VARIABLES PARRAFO DOS
       01 WKS-REGISTRO2.
          02 WKS-CAMPO           PIC X(01) OCCURS 5.
             88 WKS-LETRA2                 VALUE 'A' THRU 'I'
                   'J' THRU 'R'
                   'S' THRU 'Z'.
             88 WKS-VOCAL2                 VALUE 'A' 'E' 'I' 'O' 'U'.
             88 WKS-NUMERO2                VALUE '0' THRU '9'.
             88 WKS-PAR2                   VALUE '0' '2' '4' '6' '8'.
             88 WKS-IMPAR2                 VALUE '1' '3' '5' '7' '9'.

      * --------->            VARIABLES EVALUADOR
       01 WKS-GUIONES            PIC X(55) VALUE ALL "=".
       01 WKS-CAMPOS-DE-TRABAJO.
          02 WKS-EVALUADOR       PIC X(01).
             88 WKS-LETRA1                 VALUE 'A' THRU 'I'
                   'J' THRU 'R'
                   'S' THRU 'Z'.
             88 WKS-VOCAL1                 VALUE 'A' 'E' 'I' 'O' 'U'.
             88 WKS-NUMERO1                VALUE '0' THRU '9'.
             88 WKS-PAR1                   VALUE '0' '2' '4' '6' '8'.
             88 WKS-IMPAR1                 VALUE '1' '3' '5' '7' '9'.
          02 WKS-INDEX           PIC 9(01) VALUE ZEROS.
      *
       PROCEDURE DIVISION.
       100-MAIN SECTION.
           PERFORM 200-ACEPTAR-Y-EVALUAR-REG1
           PERFORM 200-ACEPTAR-Y-EVALUAR-REG2
           STOP RUN.
       100-MAIN-E.            EXIT.

      *-----> SERIE 200 ACEPTA LOS REGISTROS DESDE SYSIN
      *-----> USO DE UN MOVE MANUAL PARA EVALUAR CADA CAMPO
       200-ACEPTAR-Y-EVALUAR-REG1 SECTION.
           ACCEPT WKS-REGISTRO1 FROM SYSIN

           DISPLAY WKS-GUIONES
           DISPLAY
              " CADENA A EVALUAR              : " WKS-REGISTRO1
           DISPLAY
              " ESTAS SON SUS CARACTERISTICAS : "
           DISPLAY WKS-GUIONES
      *
           MOVE 1 TO WKS-INDEX
           MOVE WKS-CAMPO1 TO WKS-EVALUADOR
           PERFORM 210-EVALUAR-REGISTRO1
      *
           MOVE 2 TO WKS-INDEX
           MOVE WKS-CAMPO2 TO WKS-EVALUADOR
           PERFORM 210-EVALUAR-REGISTRO1
      *
           MOVE 3 TO WKS-INDEX
           MOVE WKS-CAMPO3 TO WKS-EVALUADOR
           PERFORM 210-EVALUAR-REGISTRO1
      *
           MOVE 4 TO WKS-INDEX
           MOVE WKS-CAMPO4 TO WKS-EVALUADOR
           PERFORM 210-EVALUAR-REGISTRO1
      *
           MOVE 5 TO WKS-INDEX
           MOVE WKS-CAMPO5 TO WKS-EVALUADOR
           PERFORM 210-EVALUAR-REGISTRO1
           DISPLAY WKS-GUIONES
           DISPLAY " "
           DISPLAY " "
           DISPLAY " ".
       200-ACEPTAR-Y-EVALUAR-REG1-E.            EXIT.

      *-----> USO DE UN CICLO DE VALICAIONES USANDO MOVE INDEXADO
       200-ACEPTAR-Y-EVALUAR-REG2 SECTION.
           ACCEPT WKS-REGISTRO2 FROM SYSIN

           DISPLAY WKS-GUIONES
           DISPLAY
              " CADENA A EVALUAR              : " WKS-REGISTRO2
           DISPLAY
              " ESTAS SON SUS CARACTERISTICAS : "
           DISPLAY WKS-GUIONES

           MOVE 0 TO WKS-INDEX
           PERFORM 210-EVALUAR-REGISTRO2 UNTIL WKS-INDEX = 5.
       200-ACEPTAR-Y-EVALUAR-REG2-E.            EXIT.

      *-----> SERIE 210 EVALUA LOS REGISTROS ACEPTADOS
       210-EVALUAR-REGISTRO1 SECTION.
           EVALUATE TRUE
           WHEN WKS-LETRA1
                PERFORM 220-DISPLAYS-LETRAS
           WHEN WKS-NUMERO1
                PERFORM 220-DISPLAYS-NUMEROS
           WHEN OTHER
                DISPLAY
                   "==> CAMPO   NO. " WKS-INDEX
                   " NO ES UNA LETRA O NUMERO"
                DISPLAY
                   "==> CONTENIDO DE LA CADENA : " WKS-REGISTRO1
                DISPLAY
                   "  > NO PODEMOS EVALUAR CARACTERES NACIONALES"
                DISPLAY
                   "  > VERIFICA E INTENTALO DE NUEVO"
           END-EVALUATE.
       210-EVALUAR-REGISTRO1-E.            EXIT.

       210-EVALUAR-REGISTRO2 SECTION.
           ADD 1 TO WKS-INDEX
           EVALUATE TRUE
           WHEN WKS-LETRA2(WKS-INDEX)
                PERFORM 220-DISPLAYS-LETRAS2
           WHEN WKS-NUMERO2(WKS-INDEX)
                PERFORM 220-DISPLAYS-NUMEROS2
           WHEN OTHER
                DISPLAY
                   "==> CAMPO   NO. " WKS-INDEX
                   " NO ES UNA LETRA O NUMERO"
                DISPLAY
                   "==> CONTENIDO DE LA CADENA : " WKS-REGISTRO2
                DISPLAY
                   "  > NO PODEMOS EVALUAR CARACTERES NACIONALES"
                DISPLAY
                   "  > VERIFICA E INTENTALO DE NUEVO"
           END-EVALUATE.
       210-EVALUAR-REGISTRO2-E.            EXIT.

      *-----> SERIE 220 ENCARGADA DE HACER DISPLAYS EN SPOOL
      *-----> DISPLAYS PARA REGISTRO 1
       220-DISPLAYS-LETRAS SECTION.
           DISPLAY
              "==> CAMPO   NO. " WKS-INDEX " ES ALFABETICO"
           DISPLAY
              "==> CONTENIDO : " WKS-EVALUADOR
           IF WKS-VOCAL1
              DISPLAY "    ES VOCAL"
           ELSE
              DISPLAY "    ES CONSTANTE"
           END-IF
           DISPLAY " ".
       220-DISPLAYS-LETRAS-E.            EXIT.

       220-DISPLAYS-NUMEROS SECTION.
           DISPLAY
              "==> CAMPO   NO. " WKS-INDEX " ES NUMERICO"
           DISPLAY
              "    CONTENIDO : " WKS-EVALUADOR
           IF WKS-PAR1
              DISPLAY "    ES UN NUMERO PAR"
           ELSE
              DISPLAY "    ES UN NUMERO IMPAR"
           END-IF
           DISPLAY " ".
       220-DISPLAYS-NUMEROS-E.            EXIT.

      *-----> DISPLAYS PARA REGISTRO 2
       220-DISPLAYS-LETRAS2 SECTION.
           DISPLAY
              "==> CAMPO   NO. " WKS-INDEX " ES ALFABETICO"
           DISPLAY
              "==> CONTENIDO : " WKS-CAMPO(WKS-INDEX)
           IF WKS-VOCAL2(WKS-INDEX)
              DISPLAY "    ES VOCAL"
           ELSE
              DISPLAY "    ES CONSTANTE"
           END-IF
           DISPLAY " ".
       220-DISPLAYS-LETRAS2-E.            EXIT.

       220-DISPLAYS-NUMEROS2 SECTION.
           DISPLAY
              "==> CAMPO   NO. " WKS-INDEX " ES NUMERICO"
           DISPLAY
              "==> CONTENIDO : " WKS-CAMPO(WKS-INDEX)
           IF WKS-PAR2(WKS-INDEX)
              DISPLAY "    ES UN NUMERO PAR"
           ELSE
              DISPLAY "    ES UN NUMERO IMPAR"
           END-IF
           DISPLAY " ".
       220-DISPLAYS-NUMEROS2-E.            EXIT.