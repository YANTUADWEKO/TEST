*&---------------------------------------------------------------------*
*& Include          ZFPSL_GEN_DLL_HANA_TABLES_TOP
*&---------------------------------------------------------------------*
REPORT zfpsl_gen_dll_hana_tables.

CLASS lcl_dll_hana DEFINITION FINAL.

  PUBLIC SECTION.

    TYPE-POOLS abap.


    TYPES: BEGIN OF ty_abap_detail,
             tabname TYPE tabname.
    TYPES:   _fields TYPE ddfields.
    TYPES:   _dbftype TYPE iwp_t_ddfields.
    TYPES: END OF ty_abap_detail.
    TYPES tty_abap_detail TYPE STANDARD TABLE OF ty_abap_detail.

    TYPES: BEGIN OF ty_hana_detail.
             INCLUDE TYPE adbc_table_descr AS _tab.
    TYPES:   _fields TYPE rsdbc_t_fie.
    TYPES: END OF ty_hana_detail.

    TYPES tty_hana_detail TYPE STANDARD TABLE OF ty_hana_detail.

    METHODS constructor IMPORTING iv_testrun TYPE /ba1/xx_dte_test_run
                                  iv_package TYPE info_tabl-devclass
                                  iv_dbcon   TYPE /ba1/tl0dll_ds-db_con
                                  iv_schema  TYPE /ba1/tl0dll_ds-schema_name.

    METHODS start       .

    METHODS on_display_details FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.

  PRIVATE SECTION.

    CONSTANTS gc_icon_atta TYPE icon_d VALUE icon_attachment.

    DATA gv_testrun        TYPE /ba1/xx_dte_test_run.
    DATA gv_package        TYPE info_tabl-devclass.
    DATA gv_dbcon          TYPE /ba1/tl0dll_ds-db_con.
    DATA gv_schema         TYPE /ba1/tl0dll_ds-schema_name.
    DATA gt_alv            TYPE STANDARD TABLE OF zfpsl_alv_dll_hana_analyzer.

    METHODS fill_alv_dll_hana .
    METHODS get_abap_structures EXPORTING et_abap TYPE tty_abap_detail.
    METHODS get_hana_objects    EXPORTING et_hana TYPE tty_hana_detail.
    METHODS display_alv       .



ENDCLASS.                    "lcl_mje_report DEFINITION
