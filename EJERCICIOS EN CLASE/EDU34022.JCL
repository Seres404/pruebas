//JEDR4022 JOB (BI),'B. INDUSTRIAL',
//             NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1)
//* ********************************************************************
//*       LIBRERIAS NECESARIAS PARA REALIZAR NUESTRA EJECUCION         *
//* ********************************************************************
//JOBLIB    DD DSN=USERLIB.BATCH,DISP=SHR
//          DD DSN=RW.V1R5M0.SCXRRUN,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*  PASO PARA ELIMINAR LOS DATASETS USADOS EN EL PROGRAMA  E/S        *
//* ********************************************************************
//STEP00A   EXEC DELFILE,DSN=EDUC.EDU34022.ENTRADA.UNO
//STEP00B   EXEC DELFILE,DSN=EDUC.EDU34022.SALIDA.UNO
//* ********************************************************************
//*               PASO PARA CREAR EL DATASET DE ENTRADA (PS)           *
//* ********************************************************************
//STEP001   EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSUT1    DD *
11011150
11022250
12111150
12122250
12133350
13077770
21011150
21022250
22111150
22122250
23011110
23011110
24011150
24150250
24250550
31033350
31044450
32133350
41011150
41022250
42144450
42133350
51111150
51122250
51133350
61122250
61133350
62110150
62102250
62033350
63088880
64011150
64020250
71011150
71022250
71033350
71044450
71055550
72061150
72072250
72083350
//SYSUT2    DD DSN=EDUC.EDU34022.ENTRADA.UNO,
//           DISP=(NEW,CATLG),
//           (VOL=SER=EDUC01),
//           SPACE=(TRK,(1,1),RLSE),
//           DCB=(LRECL=08,RECFM=FB,DSORG=PS)
//SYSIN   DD *
  GENERATE MAXFLDS=1
  RECORD FIELD=(08,,,1)
/*
//* ********************************************************************
//*              PASO PARA EJECUTAR NUETRO PROGRAMA COBOL              *
//*            PROGRAMAR PARA GENERAR UN REPORTE USANDO R-W            *
//* ********************************************************************
//EDU34022  EXEC PGM=EDU34022
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//VENTADAT  DD DSN=EDUC.EDU34022.ENTRADA.UNO,DISP=SHR
//SYS010    DD DSN=EDUC.EDU34022.SALIDA.UNO,
//           DISP=(NEW,CATLG),
//           (VOL=SER=EDUC01),
//           SPACE=(TRK,(1,1),RLSE),
//           DCB=(LRECL=133,RECFM=FB,DSORG=PS)
//SYSIN    DD  DSN=EEDR.JCL.CNTL(FECHD03),DISP=SHR
/*