//EEDR3002 JOB EDUC,SEMILLERO,CLASS=A,MSGCLASS=Q,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//JOBLIB   DD DSN=USERLIB.BATCH,DISP=SHR
//* ********************************************************************
//*            PASO PARA EJECUTAR NUESTROO PROGRAMA COBOL              *
//*   USO DE VARIABLES NIVEL 88 PARA VALIDAR EL FORMATO DE UN STRING   *
//* ********************************************************************
//EEDR3002   EXEC PGM=EEDR3002
//SYSPRINT DD SYSOUT=*
F A 8 0 @
ME70$
/*