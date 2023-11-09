*&---------------------------------------------------------------------*
*& Report ZFPSL_GEN_DLL_HANA_TABLES
*&---------------------------------------------------------------------*
*& Program is an enhanced version of /BA1/AL_L1_GEN_DLL_HANA_TABLES
*& in order to manage the HANA staging in a runtime license.
*& The program looks up ABAP DDIC stuctures for table and view create
*&
*& 2023/07/12 - Christian Schmidt - ADWEKO
*&
*& Change History:
*&
*& 2099/01/01 - sample change description
*&
*&---------------------------------------------------------------------*

INCLUDE zfpsl_gen_dll_hana_tables_top            .   " global data - class defintion
INCLUDE zfpsl_gen_dll_hana_tables_sel            .   " sel.screen
INCLUDE zfpsl_gen_dll_hana_tables_ci1            .   " main class - impl.

START-OF-SELECTION.

  DATA(lo_dll_hana) = NEW lcl_dll_hana( iv_testrun = testrun
                                        iv_package = package
                                        iv_dbcon   = dbcon
                                        iv_schema  = schema  ).
  lo_dll_hana->start( ).

  CLEAR lo_dll_hana.

  " todo:
  " - select * from views where schema_name = '/BA1C/FPSL_DATA_LOAD_LAYER'
  " - build up table for SQL statements
  " - add method for create table / view
  " - add method for alter table / view
  " - change documents
