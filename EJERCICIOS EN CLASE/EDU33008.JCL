//JEDR3008 JOB (BI),'B. INDUSTRIAL',NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//JOBLIB    DD DSN=USERLIB.BATCH,DISP=SHR
//LIBPROC   JCLLIB ORDER=(PROCESOS.VARIOS,PROCESOS.JCLLIB)
//* ********************************************************************
//*           PASO PARA CEJECUTAR NUESTROO PROGRAMA COBOL              *
//* ********************************************************************
//EDU33008 EXEC PGM=EDU33008
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SYSIN     DD *
00001 SANTIAGO JULIAN BARRERA REYES                      27 26091998 M
00002 ELIAS ABRAHAM CABRERA NAVARRO                      25 01012001 M
00003 ESMERALDA ROSA AMARILLO ROSADO                     19 01012005 F
00004 OSCAR OSCAR ANARANJADO VERDE                       18 03081990 M
00007 ESMERALDA ROSA AMARILLO ROSADO                     99 01012005 F
00001 SANTIAGO JULIAN BARRERA REYES                      27 26091998 M
00002 ELIAS ABRAHAM CABRERA NAVARRO                      25 01012001 M
00003 ESMERALDA ROSA AMARILLO ROSADO                     19 01012005 F
00004 OSCAR OSCAR ANARANJADO VERDE                       18 03081990 M
00007 ESMERALDA ROSA AMARILLO ROSADO                     99 01012005 F
00001 SANTIAGO JULIAN BARRERA REYES                      27 26091998 M
00002 ELIAS ABRAHAM CABRERA NAVARRO                      25 01012001 C
00003 ESMERALDA ROSA AMARILLO ROSADO                     19 01012005 B
00004 OSCAR OSCAR ANARANJADO VERDE                       18 03081990 A
00007 ESMERALDA ROSA AMARILLO ROSADO                     99 01012005 G
FIN
/*