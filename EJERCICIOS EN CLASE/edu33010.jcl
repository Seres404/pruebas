//JEDR3010 JOB (BI),'B. INDUSTRIAL',NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*             LIBRERIAS NECESARIAS PARA LA EJECUCION                 *
//* ********************************************************************
//JOBLIB    DD DSN=USERLIB.BATCH,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*           PASO PARA HACER UNA COPIA DEL ARCHIVO MOMAES             *
//* ********************************************************************
//PASO001  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//DD1      DD DSN=EDUC.SEM.MAESTRO.MONE,DISP=SHR
//SYSIN    DD *
  DELETE EDUC.EEDR.MAESTRO.MONE
  SET MAXCC = 0
  /*CREACION DEL CLUSTER COPIA*/
  DEFINE CLUSTER                                      -
     (NAME(EDUC.EEDR.MAESTRO.MONE)                    -
     SHR(2,3)                                         -
     MODEL(EDUC.SEM.MAESTRO.MONE))
  SET MAXCC = 0
  /* REPRO DE CARGA INICIAL */
  REPRO INFILE (DD1)                                  +
        ODS (EDUC.EEDR.MAESTRO.MONE)
/*
//* ********************************************************************
//*           PASO PARA HACER UNA COPIA DEL ARCHIVO MOMATS             *
//* ********************************************************************
//PASO002  EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//DD1      DD   DSN=EDUC.SEM.MATRIZ.SALDOS,DISP=SHR
//SYSIN    DD *
  DELETE  EDUC.EEDR.MATRIZ.SALDOS
  SET MAXCC = 0
  /*CREACION DEL CLUSTER COPIA*/
  DEFINE CLUSTER                                      -
     (NAME(EDUC.EEDR.MATRIZ.SALDOS)                   -
     SHR(2,3)                                         -
     MODEL(EDUC.SEM.MATRIZ.SALDOS))
  SET MAXCC = 0
  /* REPRO DE CARGA INICIAL */
  REPRO INFILE (DD1)                                  +
        ODS (EDUC.EEDR.MATRIZ.SALDOS)
/*
//* ********************************************************************
//*           PASO PARA HACER UNA COPIA DEL ARCHIVO MOSLIN             *
//* ********************************************************************
//PASO003  EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//DD1      DD   DSN=EDUC.SEM.SALDOS.LINEA,DISP=SHR
//SYSIN    DD *
  DELETE  EDUC.EEDR.SALDOS.LINEA
  SET MAXCC = 0
  /*CREACION DEL CLUSTER COPIA */
  DEFINE CLUSTER                                      -
     (NAME(EDUC.EEDR.SALDOS.LINEA)                    -
     SHR(2,3)                                         -
     MODEL(EDUC.SEM.SALDOS.LINEA))
  SET MAXCC = 0
  /* REPRO DE CARGA INICIAL */
  REPRO INFILE (DD1)                                  +
        ODS (EDUC.EEDR.SALDOS.LINEA)
/*
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* ********************************************************************
//EDU33010  EXEC PGM=EDU33010
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//MOMAES    DD DSN=EDUC.EEDR.MAESTRO.MONE,DISP=SHR
//MOMATS    DD DSN=EDUC.EEDR.MATRIZ.SALDOS,DISP=SHR
//MOSLIN    DD DSN=EDUC.EEDR.SALDOS.LINEA,DISP=SHR
/*