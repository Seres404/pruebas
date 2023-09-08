//ED3D1JB4 JOB (BI),'B. INDUSTRIAL',
//             NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*             LIBRERIAS NECESARIAS PARA LA EJECUCION                 *
//* ********************************************************************
//JOBLIB   DD DSN=USERLIB.BATCH,DISP=SHR
//         DD DSN=RW.V1R5M0.SCXRRUN,DISP=SHR
//LIBROC   JCLLIB ORDER=(PROCESOS.JCLLIB,EEDR.JCL.CNTL)
//* ********************************************************************
//*PASO002  EXEC PGM=IDCAMS
//*SYSPRINT DD SYSOUT=*
//*SYSIN    DD *
//*  DEFINE GDG(NAME(EDUC.ED3D1R01.REPORT01) -
//*             LIMIT(1) -
//*             NOEMPTY  -
//*             SCRATCH)
//*
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* ********************************************************************
//ED3D1R01  EXEC PGM=ED3D1R01
//EDP1AH    DD DSN=EDUC.AHORROS1.V1.M.P.MAESTRO.PATHA.E3,DISP=SHR,
//             AMP=('BUFNI=10','BUFND=30')
//SYS010    DD DSN=EDUC.ED3D1R01.REPORT01(+1),
//          DISP=(NEW,CATLG),
//          UNIT=3390,
//          SPACE=(TRK,(2,1),RLSE),
//          DCB=(LRECL=128)
//SYSIN     DD  DSN=PROCESOS.FECHAS(FECHD03),DISP=SHR
//SYSOUT    DD SYSOUT=*
/*