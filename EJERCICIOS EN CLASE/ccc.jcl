//JEDR3004   JOB (BI),
//              'BANCO INDUSTRIAL',
//               CLASS=P,
//               REGION=0M,
//               MSGCLASS=X,
//               MSGLEVEL=(1,1),
//               NOTIFY=&SYSUID
//*                        LIBRERIAS A USAR
//JOBLIB     DD  DSN=USERLIB.BATCH,DISP=SHR
//           DD  DSN=RW.V1R5M0.SCXRRUN,DISP=SHR
//LIBPROC    JCLLIB
ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB,PGSI.JCL.CNTL)
//*
//*-------------------------------------------------------------
//* ELIMINA Y GENERA LA SALIDA
//*-------------------------------------------------------------
//DELSALID  EXEC DELFILE,DSN=EDUC.SEMI.EDUx3004.SALIDA
//DEFFILE   EXEC PGM=IEFBR14,COND=(4,LT)
//SYSPRINT  DD   SYSOUT=*
//DEFINE1   DD   DSN=EDUC.SEMI.EDUx3004.SALIDA,DISP=(,CATLG),
//               UNIT=3390,SPACE=(CYL,(1,1),RLSE),
//               DCB=(DSORG=PS,LRECL=098,RECFM=FB)
//* --------------------------------------------------- *
//* + GENERA REPORTE DE ESTUDIANTES *
//* + SEMILLERO +
//* -------------------------------------------------- *
//EDUx3004  EXEC PGM= EDUx3004,COND=(4,LT)
//SYSOUT    DD SYSOUT=*
//ENTRADA   DD   DSN=EDUC.SEMI.ENTRADA,DISP=SHR
//SALIDA    DD   DSN=EDUC.SEMI.EDUx3004.SALIDA,DISP=SHR 