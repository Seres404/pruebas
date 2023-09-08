//JEDR3013 JOB EDUC,SEMILLERO,CLASS=A,MSGCLASS=Q,
//         MSGLEVEL=(1,1),NOTIFY=&SYSUID
//JOBLIB   DD DSN=USERLIB.BATCH,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*          PASO PARA ELIMINAR EL DATASET DE ENTRADA                  *
//* ********************************************************************
//STEP000   EXEC DELFILE,DSN=EDUC.EDU33013.ENTRADA.UNO
//* ********************************************************************
//*       PASO PARA CREAR EL DATASET DE ENTRADA (PS) ORDENADO          *
//* ********************************************************************
//SORT      EXEC PGM=ICEMAN
//SYSOUT    DD SYSOUT=*
//SORTIN    DD *
4110068980
2310250861
2100170311
0000191205
6550149865
0530154087
2150156616
0120404744
0000418301
0110151198
0120398763
01203AAAAA
9999
8888888
9999999999
CUENTA
//SORTOUT   DD DSN=EDUC.EDU33013.ENTRADA.UNO,
//           DISP=(NEW,CATLG),
//           (VOL=SER=EDUC01),
//           SPACE=(TRK,(2,1),RLSE),
//           DCB=(LRECL=10,RECFM=FB,DSORG=PS)
//SYSIN   DD *
  SORT FIELDS=(1,10,ZD,A)
/*
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//*        LECTRA A VARIOS ARCHIVOS DEL SISTEMA, VALIDACION DE DPI     *
//* ********************************************************************
//EDU33013 EXEC PGM=EDU33013
//SYSPRINT  DD SYSOUT=*
//ENTRADA   DD DSN=EDUC.EDU33013.ENTRADA.UNO,DISP=SHR
//MOMAES    DD DSN=EDUC.SEM.MONETA.RIOS.MAESTRO,DISP=SHR,
//          AMP=('BUFNI=10','BUFNI=30')
//CFCNAT    DD DSN=EDUC.SEM.CIF.CLIENTE.NATURAL,DISP=SHR,
//          AMP=('BUFNI=10','BUFNI=30')
//MOTGEN    DD DSN=EDUC.SEM.MONETA.RIOS.TABLAS,DISP=SHR,
//          AMP=('BUFNI=10','BUFNI=30')
//CFTGEN    DD DSN=EDUC.SEM.CIF.MODGRAL.TABLAS,DISP=SHR,
//          AMP=('BUFNI=10','BUFNI=30')
//SYSOUT    DD SYSOUT=*