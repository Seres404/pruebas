//JEDR3021 JOB (BI),'B. INDUSTRIAL',
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
//*           PASO PARA HACER UNA COPIA DEL ARCHIVO MAESTRO            *
//* ********************************************************************
//PASO001  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//DD1      DD DSN=EDUC.DECLARA.GUATE.JOURNAL.TRANSAC,DISP=SHR
//SYSIN    DD *
  DELETE  EDUC.DECLARA.EEDR.JOURNAL.TRANSAC
  SET MAXCC = 0
  /*CREACION DEL CLUSTER COPIA*/
  DEFINE CLUSTER                                      -
     (NAME(EDUC.DECLARA.EEDR.JOURNAL.TRANSAC)         -
     SHR(2,3)                                         -
     MODEL(EDUC.DECLARA.GUATE.JOURNAL.TRANSAC))
  /* REPRO DE CARGA INICIAL */
  REPRO INFILE (DD1)                                  +
     ODS (EDUC.DECLARA.EEDR.JOURNAL.TRANSAC)
/*
//* ********************************************************************
//*           PASO PARA CREAR EL GDG BASE SOLO SE CORE UNA VEZ         *
//* ********************************************************************
//PASO002  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
//  DEFINE GDG(NAME(EDUC.EDU33021.REPORT01) -
//             LIMIT(1) -
//             NOEMPTY  -
//             SCRATCH)
/*
//* ********************************************************************
//*             PASO PARA EJECUTAR NUESTRO PROGRAMA COBOL              *
//*     PROGRAMA QUE ACTUALIZA Y CREA UN REPORTE PARA ENVIO POR FTP    *
//* ********************************************************************
//EDU33021  EXEC PGM=EDU33021,TIME=1
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//STFRJO    DD DSN=EDUC.DECLARA.EEDR.JOURNAL.TRANSAC,DISP=SHR,
//          AMP=('BUFNI=05','BUFND=30')
//CFTGEN    DD DSN=EDUC.SEM.CIF.MODGRAL.TABLAS,DISP=SHR,
//          AMP=('BUFNI=10','BUFND=30')
//SYS010    DD DSN=EDUC.EDU33021.REPORT01(+1),
//          DISP=(NEW,CATLG),
//          UNIT=3390,
//          SPACE=(TRK,(2,1),RLSE),
//          DCB=(LRECL=114)
//SYSIN     DD  DSN=PROCESOS.FECHAS(FECHD03),DISP=SHR
/*
