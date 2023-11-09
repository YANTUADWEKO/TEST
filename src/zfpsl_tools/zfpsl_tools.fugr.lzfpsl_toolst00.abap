*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10.02.2023 at 19:13:40
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFPSL_STAGETRUNC................................*
DATA:  BEGIN OF STATUS_ZFPSL_STAGETRUNC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFPSL_STAGETRUNC              .
CONTROLS: TCTRL_ZFPSL_STAGETRUNC
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFPSL_STAGETRUNC              .
TABLES: ZFPSL_STAGETRUNC               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
