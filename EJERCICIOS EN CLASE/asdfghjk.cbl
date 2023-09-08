      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECTR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WKS-NUMERO                       PIC 9(09) VALUE 0000015556.
       01  WKS-CEROS                        PIC 9(02) VALUE ZEROS.
       01  WKS-ALFA                         PIC X(04) VALUE "HOLA".
       PROCEDURE DIVISION.
       001-MAIN SECTION.
           INSPECT WKS-NUMERO TALLYING WKS-CEROS
                              FOR LEADING ZEROS
           DISPLAY WKS-CEROS.
           ADD 100 TO WKS-NUMERO.
           DISPLAY WKS-NUMERO.
           INITIALIZE WKS-NUMERO
           DISPLAY WKS-NUMERO.
           DISPLAY WKS-ALFA
           INITIALIZE WKS-ALFA
           DISPLAY WKS-ALFA.
       001-MAIN-E. EXIT.
