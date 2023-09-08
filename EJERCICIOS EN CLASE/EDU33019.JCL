//JEDR3019 JOB (BI),'B. INDUSTRIAL',
//             NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*             LIBRERIAS NECESARIAS PARA LA EJECUCION                 *
//* ********************************************************************
//JOBLIB   DD DSN=USERLIB.BATCH,DISP=SHR
//         DD DSN=RW.V1R5M0.SCXRRUN,DISP=SHR
//LIBROC   JCLLIB ORDER=(PROCESOS.JCLLIB,EEDR.JCL.CNTL,PROCESOS.VARIOS)
//* ********************************************************************
//*           PASO PARA HACER UNA COPIA DEL ARCHIVO MOMAES             *
//* ********************************************************************
//PASO001  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//DD1      DD DSN=EDUC.SEM.MAESTRO.MONE,DISP=SHR
//SYSIN    DD *
  DELETE EDUC.EDU33019.MAESTRO.KSDS01
  SET MAXCC = 0
  /*CREACION DEL CLUSTER COPIA*/
  DEFINE CLUSTER                                      -
     (NAME(EDUC.EDU33019.MAESTRO.KSDS01)              -
     SHR(2,3)                                         -
     MODEL(EDUC.SEM.MAESTRO.MONE))
  /* REPRO DE CARGA INICIAL */
  REPRO INFILE (DD1)                                  +
     ODS (EDUC.EDU33019.MAESTRO.KSDS01)
  /* DEFINICION AIX  */
  DEFINE AIX(NAME(EDUC.EDU33019.AIXCIF.MONE)          -
         RELATE(EDUC.EDU33019.MAESTRO.KSDS01)         -
         CISZ(2048)                                   -
         RECSZ(23 113)                                -
         CYL(1 1)                                     -
         KEYS(08 10)                                  -
         UPGRADE                                      -
         SHAREOPTIONS(2)                              -
         NONUNIQUEKEY)
  BLDINDEX IDS(EDUC.EDU33019.MAESTRO.KSDS01)          -
           ODS(EDUC.EDU33019.AIXCIF.MONE)
  /* DEFINICION PATH */
  DEFINE PATH(NAME(EDUC.EDU33019.PATHCIF.MONE)        -
         PATHENTRY(EDUC.EDU33019.AIXCIF.MONE))
/*
//PASO002  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG(NAME(EDUC.EDU33019.REPORT01) -
             LIMIT(1) -
             NOEMPTY  -
             SCRATCH)
/*
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* ********************************************************************
//EDU33010  EXEC PGM=EDU33019
//SPRINT    DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//MOMAES    DD DSN=EDUC.EDU33019.MAESTRO.KSDS01,DISP=SHR,
//             AMP=('BUFNI=10','BUFND=30')
//MOMAES1   DD DSN=EDUC.EDU33019.PATHCIF.MONE,DISP=SHR,
//             AMP=('BUFNI=10','BUFND=30')
//MOTGEN    DD DSN=EDUC.SEM.MONETA.RIOS.TABLAS,DISP=SHR,
//             AMP=('BUFNI=10','BUFND=30')
//CFCNAT    DD DSN=EDUC.SEM.CIF.CLIENTE.NATURAL,DISP=SHR,
//             AMP=('BUFNI=10','BUFND=30')
//SYSOO7    DD DSN=EDUC.EDU33019.REPORT01(+1),
//          DISP=(NEW,CATLG),
//          UNIT=3390,
//          SPACE=(TRK,(2,1),RLSE),
//          DCB=(FTP.MODELO,BUFNO=7)
//SYSIN     DD  DSN=EEDR.JCL.CNTL(FECHD03),DISP=SHR
/*