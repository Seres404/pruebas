//JEDR3004 JOB (BI),'B. INDUSTRIAL',NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*             LIBRERIAS NECESARIAS PARA LA EJECUCION                 *
//* ********************************************************************
//JOBLIB    DD DSN=USERLIB.BATCH,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*            PASO PARA ELIMINAR EL DATASET DE SALIDA                 *
//* ********************************************************************
//DELSALID  EXEC DELFILE,DSN=EDUC.SEMI.EDU33004.SALIDA
/*
//* ********************************************************************
//*            PASO PARA CREAR EL DATASET DE SALIDA (PS)               *
//* ********************************************************************
//DEFFILE   EXEC PGM=IEFBR14,COND=(4,LT)
//SYSPRINT  DD   SYSOUT=*
//DEFINE1   DD   DSN=EDUC.SEMI.EDU33004.SALIDA,DISP=(NEW,CATLG),
//          SPACE=(CYL,(1,1),RLSE),
//          (VOL=SER=EDUC01),
//          DCB=(DSORG=PS,LRECL=098,RECFM=FB)
/*
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* VERIFICA ESTUS DE UN ESTUDIANTE SI ES "A" CALCULA EDAD Y PROMEDIOS *
//* ********************************************************************
//EEDR3004  EXEC PGM=EEDR3004,COND=(4,LT)
//SYSOUT    DD SYSOUT=*
//STUDENT   DD   DSN=EDUC.SEMI.ENTRADA,DISP=SHR
//REPORTE   DD   DSN=EDUC.SEMI.EDU33004.SALIDA,DISP=SHR
/*