*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTFPSL_STG_CTLG
*   generation date: 17.08.2023 at 14:43:31
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTFPSL_STG_CTLG    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
