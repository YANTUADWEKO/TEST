*&---------------------------------------------------------------------*
*& Report ZHDT_FPSL_MDF_ZDP_DEMAND
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhdt_fpsl_mdf_zdp_demand.

INCLUDE zfpsl_staging_manager.

START-OF-SELECTION.

  DATA(lo_dll_hana) = NEW lcl_dll_hana( iv_testrun       = testrun
                                        iv_dbcon         = dbcon
                                        iv_schema        = schema
                                        iv_tab_view_name = CONV #( sy-repid ) ).

  DATA(ddl_name) = lo_dll_hana->get_ddl_name( ).

**/>>> ALTER PATTERN - 2023/07/15 - Version 002
*  DATA(ddl_alter) =
*       `ALTER TABLE ` && ddl_name  && ` ALTER ` &&
*       ` ( val4 char(100) NOT NULL )`.
*
*  lo_dll_hana->alter_table( ddl_alter ).
**/<<< ALTER PATTERN - 2023/07/15 - Version 002

*/>>> YTU - CREATE PATTERN - 2023/08/16 - Version 001
*  DATA(ddl_create) =
*        |CREATE TABLE { ddl_name } (| &&
*        |"CREATION_TIMESTAMP" TIMESTAMP NOT NULL ,\n| &&
*        |"OBJECT_HASH_KEY" INTEGER NOT NULL ,\n| &&
*        |"TARGET_CLIENT" NVARCHAR(3) NOT NULL ,\n| &&
*        |"/BA1C/HDREXTNO" NVARCHAR(40) NOT NULL ,\n| &&
*        |"/BA1C/HDRPTYPE" NVARCHAR(2)  ,\n| &&
*        |"0DATE" NVARCHAR(8) NOT NULL ,\n| &&
*        |"/BA1C/HDRAUTHG" NVARCHAR(12)  ,\n| &&
*        |"/BA1C/HDRTEMPL" NVARCHAR(10)  ,\n| &&
*        |"/BA1C/HDRPVERS" NVARCHAR(10)  ,\n| &&
*        |"/BA1C/HDRNODID" NVARCHAR(5)  ,\n| &&
*        |"/BA1C/ORGSY" NVARCHAR(10) NOT NULL ,\n| &&
*        |"/BA1C/OBJST" NVARCHAR(3)  ,\n| &&
*        |"/BA1C/CTRST" NVARCHAR(8)  ,\n| &&
*        |"/BA1C/CTREN" NVARCHAR(8)  ,\n| &&
*        |"/BA1C/OBJCR" NVARCHAR(5)  ,\n| &&
*        |"/BA1C/ORGUO" NVARCHAR(10)  ,\n| &&
*        |"/BA1/C11PRDCTR" NVARCHAR(20)  ,\n| &&
*        |"/BA1/C55LGENT" NVARCHAR(10)  ,\n| &&
*        |"/BA1C/HDRVALFR" NVARCHAR(8)  ,\n| &&
*        |"/BA1C/HDRVALTO" NVARCHAR(8)  ,\n| &&
*        |"/BA1C/BPRROLE" NVARCHAR(6)  ,\n| &&
*        |"/BA1C/BPRIDTYP" NVARCHAR(6)  ,\n| &&
*        |"/BA1C/BPREXTNO" NVARCHAR(60)  ,\n| &&
*        |"ZCCUSTID" NVARCHAR(70)  ,\n| &&
*        |"ZCCUSTTYP" NVARCHAR(50)  ,\n| &&
*        |"ZCCNTRYCD" NVARCHAR(3)  ,\n| &&
*        |"ZCREGION" NVARCHAR(2)  ,\n| &&
*        |"ZCCLACTYP" NVARCHAR(50)  ,\n| &&
*        |"ZCPRDTYP" NVARCHAR(50)  ,\n| &&
*        |"ZCPRDSBTY" NVARCHAR(20)  ,\n| &&
*        |"ZCFICNTNM" NVARCHAR(128)  ,\n| &&
*        |"ZCCFIIBID" NVARCHAR(128)  ,\n| &&
*        |"ZCCFIOBID" NVARCHAR(128)  \n| &&
*        |);|.
*
*  lo_dll_hana->create_table( ddl_create ).
*/<<< YTU - CREATE PATTERN - 2023/08/16 - Version 001

  lo_dll_hana->generate( ).


  lo_dll_hana->display_log( ).
  CLEAR lo_dll_hana.
