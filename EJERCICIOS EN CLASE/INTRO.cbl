       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EDU34008.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WKS-LOOPC              PIC 9  VALUE ZEROS.
       01  WKS-LOOPC2             PIC S9 VALUE ZEROS.
       01  WKS-VAREDIT            PIC -9.
      * ///////////////////////////////////////////////////////////////
       PROCEDURE DIVISION.
       MAIN1.
           PERFORM VALIDA-ONE
           PERFORM VALIDA-TWO
           PERFORM VALIDA-THREE
           PERFORM VALIDA-FOUR
           STOP RUN.
       *> cblformat off
      
       *> cblformat on
      *////////////////////////////////////////////////////////////////
      *                          SECCIONES:
      *                        VALIDACION UNO  
       VALIDA-ONE SECTION.
           DISPLAY "START WHILE ITERATION OF LOOPBODY"
           PERFORM LOOPBODY WITH TEST BEFORE
                   VARYING WKS-LOOPC FROM 1 BY 2
                   UNTIL WKS-LOOPC GREATER THAN 5.
                   DISPLAY "FINISHED WHILE ITERATION.  LOOPCOUNT = " 
                   WKS-LOOPC  
           PERFORM EDIT-VAR.
       VALIDA-ONE-E. EXIT.
      *                        VALIDACION DOS 
       VALIDA-TWO SECTION.
           DISPLAY "START REPEAT ITERATION OF LOOPBODY"
           PERFORM LOOPBODY WITH TEST AFTER
                   VARYING WKS-LOOPC FROM 1 BY 2
                   UNTIL WKS-LOOPC GREATER THAN 5.
                   DISPLAY "FINISHED REPEAT ITERATION. LOOPCOUNT = " 
                    WKS-LOOPC.
           PERFORM EDIT-VAR.
       VALIDA-TWO-E. EXIT.
      *                        VALIDACION TRES
       VALIDA-THREE SECTION.
           DISPLAY "START INLINE LOOPS"
             PERFORM VARYING WKS-LOOPC FROM 1 BY 1 UNTIL WKS-LOOPC
                                     GREATER THAN 4
      *    *                       ANIDADO                             *
                  PERFORM VARYING WKS-LOOPC2 FROM 5 BY -2
                     UNTIL WKS-LOOPC2 LESS THAN ZERO
                     DISPLAY "INLINE LOOP " WITH NO ADVANCING
                     DISPLAY "LOOPCOUNT = " WKS-LOOPC 
                     " LOOPCOUNT2 = " WKS-LOOPC2
                  END-PERFORM
      *    *                      FIN ANIDADO                          *
             END-PERFORM.
           DISPLAY "FINISHED INLINE LOOPS".  
           PERFORM EDIT-VAR.
       VALIDA-THREE-E. EXIT.
      *                        VALIDACION CUATRO
       VALIDA-FOUR SECTION.
           DISPLAY "START PERFORM VARYING..AFTER".
           PERFORM LOOPBODY VARYING WKS-LOOPC FROM 1 BY 1
                 UNTIL WKS-LOOPC GREATER THAN 4
                 AFTER WKS-LOOPC2 FROM 5 BY -2
                 UNTIL WKS-LOOPC2 LESS THAN ZERO.
                 DISPLAY "FINISHED PERFORM VARYING..AFTER".     
           PERFORM EDIT-VAR.
       VALIDA-FOUR-E. EXIT.
       
       LOOPBODY SECTION.
           DISPLAY "LOOPBODY "
           PERFORM EDIT-VAR.
               DISPLAY "LOOPCOUNT = " WKS-LOOPC " LOOPCOUNT2 = "
               WKS-LOOPC2. 
       LOOPBODY-E. EXIT.
       
       EDIT-VAR SECTION.
           MOVE WKS-LOOPC2 TO WKS-VAREDIT.
       EDIT-VAR-E. EXIT.

           "| =ID= | NOMBRE COMPLETO                                   "

           "|"