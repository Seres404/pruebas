//ED3D1JB2 JOB (BI),'B. INDUSTRIAL',
//             NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*            LIBRERIAS NECESARIAS PARA LAS EJECUCIONES               *
//* ********************************************************************
//JOBLIB   DD DSN=USERLIB.BATCH,DISP=SHR
//LIBROC   JCLLIB ORDER=(PROCESOS.JCLLIB,EEDR.JCL.CNTL,PROCESOS.VARIOS)
//**********************************************************************
//*          JCL PARA DEFINIR UN CLUSTER KSDS Y CARGA INICIAL          *
//**********************************************************************
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE EDUC.AHORROS1.V1.M.P.MAESTRO.KSDSA.E3 CLUSTER PURGE
   SET MAXCC = 0
   DEFINE CLUSTER(NAME(EDUC.AHORROS1.V1.M.P.MAESTRO.KSDSA.E3) -
   INDEXED                             -
   TRK(4 2)                            -
   CISZ (512)                          -
   RECSZ(100 100)                      -
   KEYS(10 0)                          -
   VOLUME(EDUC01))
//**********************************************************************
//*          PROGRAMA COBOL BATCH PARA CARGA INICIAL DEL KSDS          *
//**********************************************************************
//EDPD1C01 EXEC PGM=ED3D1C01
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ESD1AH   DD DSN=EDUC.AHORROS1.V1.M.P.MAESTRO.ESDSA.E3,DISP=SHR,
//            AMP=('BUFND=30')
//EDM1AH   DD DSN=EDUC.AHORROS1.V1.M.P.MAESTRO.KSDSA.E3,DISP=SHR,
//            AMP=('BUFNI=5','BUFND=30')
/*
//**********************************************************************
//*          PASO AGREGADO EJERCICIO 5002 VALIDACION DE LISCAT         *
//**********************************************************************
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENT(EDUC.AHORROS1.V1.M.P.MAESTRO.KSDSA.E3) ALL