//EEDR3012 JOB (BI),'B. INDUSTRIAL',NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*             LIBRERIAS NECESARIAS PARA LA EJECUCION                 *
//* ********************************************************************
//JOBLIB    DD DSN=USERLIB.BATCH,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* ********************************************************************
//EDU33012  EXEC PGM=EDU33012
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//CFTGEN    DD DSN=EDUC.SEM.CIF.MODGRAL.TABLAS,DISP=SHR
//SYSIN     DD *
01008
01009
05001
10001
10999
AAAAA
00000

FIN
/*