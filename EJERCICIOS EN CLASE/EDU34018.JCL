//JEDR4018 JOB (BI),'B. INDUSTRIAL',NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//JOBLIB    DD DSN=USERLIB.BATCH,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*          PASO PARA ELIMINAR EL DATASET DE ENTRADA                  *
//* ********************************************************************
//STEP000   EXEC DELFILE,DSN=EDUC.EDU34018.ENTRADA.UNO
//* ********************************************************************
//*               PASO PARA CREAR EL DATASET DE ENTRADA (PS)           *
//* ********************************************************************
//STEP001   EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSUT1    DD *
00121FLIGHT OF THE CONDOR, THE               03
00333PREDATOR                                02
00444LIVING EARTH, THE                       03
01001COMMANDO                                02
01100ROBOCOP                                 01
02001LEOPARD HUNTS IN DARKNESS, A            03
02121DIRTY DANCING                           04
03031COMPETENT CREW                          05
03032YACHT MASTER                            05
04041OPEN OCEAN SAILING                      05
04042PRINCESS BRIDE, THE                     06
04444LIFE ON EARTH                           03
04646JURASSIC PARK                           15
05051OVERBOARD                               01
06061HOPE AND GLORY                          07
07071AMONG THE WILD CHIMPANZEES              03
08081WHALE NATION                            03
09091BESTSELLER                              07
10001WICKED WALTZING                         04
11111TERMINATOR, THE                         02
12020MAKING THE SUPPLIER DELETE INVALID      19
13301MASSACRE AT MASAI MARA                  03
14032KNOTTY PROBLEMS FOR SAILORS             05
17001ALIEN                                   07
17002ALIENS                                  07
17012A LOON WITH THE FLU                     21
17041GARFIELD TAKES A HIKE                   06
18001SURVIVING THE STORM                     05
18001SURVIVING THE STORM                     05
19444PINOCCIO                                02
19444PINOCCIO                                02
19444PINOCCIO                                02
//SYSUT2  DD DSN=EDUC.EDU34018.ENTRADA.UNO,
//           DISP=(NEW,CATLG),
//           (VOL=SER=EDUC01),
//           SPACE=(TRK,(2,1),RLSE),
//           DCB=(LRECL=60,RECFM=FB,DSORG=PS)
//SYSIN   DD *
  GENERATE MAXFLDS=1
  RECORD FIELD=(60,,,1)
/*
//* ********************************************************************
//*       PASO PARA CREAR Y ELIMINAR EL DATASET DE SALIDA  (VSAM)      *
//* ********************************************************************
//STEP002  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE EDUC.EDU34018.KSDS01 CLUSTER PURGE
   SET MAXCC = 0
   DEFINE CLUSTER(NAME(EDUC.EDU34018.KSDS01) -
   INDEXED -
   CYL (1 1) -
   RECSZ(60 60) -
   KEYS(5 0) -
   CISZ(512) -
   VOLUME(EDUC01))
/*
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* ********************************************************************
//EDU34018 EXEC PGM=EDU34018
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//PELIS     DD DSN=EDUC.EDU34018.ENTRADA.UNO,DISP=SHR
//EDPELI    DD DSN=EDUC.EDU34018.KSDS01,DISP=SHR
/*