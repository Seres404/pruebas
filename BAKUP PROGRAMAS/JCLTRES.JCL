//ED3D1JB3 JOB (BI),'B. INDUSTRIAL',
//             NOTIFY=&SYSUID,
//             REGION=0M,CLASS=S,
//             MSGCLASS=X,MSGLEVEL=(1,1)
//* ********************************************************************
//*          JCL PARA CREAR EL PATH Y EL AIX DE UN KSDS                *
//* ********************************************************************
//PASO001  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE EDUC.AHORROS1.V1.M.P.MAESTRO.PATHA.E3
   DELETE EDUC.AHORROS1.V1.M.P.MAESTRO.AIX1A.E3
   SET MAXCC = 0
   /*CREA UN AIX*/
   DEFINE AIX(NAME(EDUC.AHORROS1.V1.M.P.MAESTRO.AIX1A.E3)  -
   RELATE(EDUC.AHORROS1.V1.M.P.MAESTRO.KSDSA.E3)           -
   RECSZ(23 213)                                           -
   TRK(4 2)                                                -
   CISZ(1536)                                              -
   KEYS(8 10)                                              -
   UPGRADE                                                 -
   NONUNIQUEKEY)
   /*ADD BLDINDEX*/
   BLDINDEX IDS(EDUC.AHORROS1.V1.M.P.MAESTRO.KSDSA.E3)     -
            ODS(EDUC.AHORROS1.V1.M.P.MAESTRO.AIX1A.E3)
   /*ADD DEFINE OF PATH*/
   DEFINE PATH(NAME(EDUC.AHORROS1.V1.M.P.MAESTRO.PATHA.E3) -
          PATHENTRY(EDUC.AHORROS1.V1.M.P.MAESTRO.AIX1A.E3))
/*
//**********************************************************************
//*          PASO AGREGADO EJERCICIO 5002 VALIDACION DE LISCAT         *
//**********************************************************************
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
    LISTCAT ENT(EDUC.AHORROS1.V1.M.P.MAESTRO.PATHA.E3) ALL
    LISTCAT ENT(EDUC.AHORROS1.V1.M.P.MAESTRO.AIX1A.E3) ALL