//ED3D1JB1 JOB EDUC,SEMILLERO,
//         CLASS=A,MSGCLASS=Q,
//         MSGLEVEL=(1,1),
//         REGION=4M,NOTIFY=&SYSUID
//**********************************************************************
//*      JCL PARA CREAR UN ESDS Y HACER CARGA POR MEDIO DE UN REPRO    *
//**********************************************************************
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
    DELETE EDUC.AHORROS1.V1.M.P.MAESTRO.ESDSA.E3 CLUSTER PURGE
    SET MAXCC=0
    DEFINE CLUSTER(NAME(EDUC.AHORROS1.V1.M.P.MAESTRO.ESDSA.E3) -
    NONINDEXED                                                 -
    TRK(4 2)                                                   -
    RECSZ(80 80)                                               -
    CISZ(1536)                                                 -
    VOLUME(EDUC02))
    REPRO IDS(EDUC.MAAHOR)                                     -
          ODS(EDUC.AHORROS1.V1.M.P.MAESTRO.ESDSA.E3)
    LISTCAT ENT(EDUC.AHORROS1.V1.M.P.MAESTRO.ESDSA.E3) ALL