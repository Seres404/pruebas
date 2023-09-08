//JEDR3018 JOB (BI),'B. INDUSTRIAL',NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*       LIBRERIAS NECESARIAS PARA REALIZAR NUESTRA EJECUCION         *
//* ********************************************************************
//JOBLIB    DD DSN=USERLIB.BATCH,DISP=SHR
//          DD DSN=RW.V1R5M0.SCXRRUN,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*              PASO PARA ELIMINAR EL DATASET DE SALIDA               *
//* ********************************************************************
//STEP000  EXEC DELFILE,DSN=EDUC.EDU33018.SALIDA.UNO
/*
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* ********************************************************************
//EDU33018 EXEC PGM=EDU33018,TIME=1
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSOO7    DD DSN=EDUC.EDU33018.SALIDA.UNO,
//          DISP=(NEW,CATLG),
//          (VOL=SER=EDUC01),
//          SPACE=(TRK,(2,1),RLSE),
//          DCB=(LRECL=86,RECFM=FB,DSORG=PS)
//SYSIN     DD  DSN=EEDR.JCL.CNTL(FECHD03),DISP=SHR
//          DD *
02 001 001 LINEA 01          00000100
02 001 002 LINEA 02          00000200
02 001 003 LINEA 03          00000300
02 001 001 LINEA 04          00000400
02 001 002 LINEA 05          00000500
02 001 003 LINEA 06          00000600
02 002 001 LINEA 07          00000700
02 002 002 LINEA 08          00000800
02 002 003 LINEA 09          00000900
02 002 001 LINEA 10          00001000
02 002 002 LINEA 11          00001100
02 003 003 LINEA 12          00001200
02 003 001 LINEA 13          00001300
02 003 002 LINEA 14          00001400
02 003 003 LINEA 15          00001500
02 003 001 LINEA 16          00001600

AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
0000000000000000000000000000000000000
09
/*