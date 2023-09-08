//JEDR3014 JOB EDUC,SEMILLERO,CLASS=A,MSGCLASS=Q,
//         MSGLEVEL=(1,1),NOTIFY=&SYSUID
//JOBLIB   DD DSN=USERLIB.BATCH,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//*      FORMATEO DE NOMBRES PARA GENERAR UNA PLANTILLA DE CHEQUE      *
//* ********************************************************************
//EDU33014 EXEC PGM=EDU33014
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD *
DEL ROSARIO    GARCIA         DEL CID        DANIELLA       VICTORIA
HERNANDES      DEL SOL        AMARILLO       ANA SOL        HERNESTINA
DEL PILAR      DE LEON        DE MARFIL      JUANA          ANA SOL
GALVEZ         MORALES        ROBLES         ANA            MARIA
MONROY         RAMIREZ        LOPEZ          CINDY          LUCRECIA
PINEDA         MEJIA          ROJAS          ELENY
MONROY                                       CINDY          DEL PILAR
MONASTERIO                                   DEL PILAR
GALVEZ                        ROBLES         ANA            MARIA

               DIVAS                                        DANIEL
               DIVAS                         ERICK          DANIEL
RAMIREZ                                      ERICK
FIN
/*