*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 17.08.2023 at 14:43:31
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFPSL_STG_CTLG.................................*
DATA:  BEGIN OF STATUS_ZTFPSL_STG_CTLG               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFPSL_STG_CTLG               .
CONTROLS: TCTRL_ZTFPSL_STG_CTLG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTFPSL_STG_CTLG               .
TABLES: ZTFPSL_STG_CTLG                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
