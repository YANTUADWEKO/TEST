*&---------------------------------------------------------------------*
*& Include          ZFPSL_GEN_DLL_HANA_TABLES_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-s00.

  PARAMETERS package TYPE info_tabl-devclass DEFAULT 'TMP_SCHMIDT_DLL'.

SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.

  PARAMETERS testrun TYPE /ba1/xx_dte_test_run DEFAULT abap_true.
  PARAMETERS dbcon   TYPE /ba1/tl0dll_ds-db_con OBLIGATORY DEFAULT 'DEFAULT'.
  PARAMETERS schema  TYPE /ba1/tl0dll_ds-schema_name OBLIGATORY DEFAULT '/BA1C/FPSL_DATA_LOAD_LAYER'.

SELECTION-SCREEN END OF BLOCK b1.
