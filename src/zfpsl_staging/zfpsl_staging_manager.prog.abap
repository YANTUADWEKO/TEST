*&---------------------------------------------------------------------*
*& Include          ZFPSL_STAGING_MANAGER
*&---------------------------------------------------------------------*

TYPES:
  tt_stg_ctlg TYPE STANDARD TABLE OF ztfpsl_stg_ctlg WITH DEFAULT KEY,
  tt_stmts    TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.

  PARAMETERS testrun TYPE /ba1/xx_dte_test_run DEFAULT abap_true.
  PARAMETERS dbcon   TYPE /ba1/tl0dll_ds-db_con OBLIGATORY DEFAULT 'DEFAULT'.
  PARAMETERS schema  TYPE /ba1/tl0dll_ds-schema_name OBLIGATORY DEFAULT '/BA1C/FPSL_DATA_LOAD_LAYER'.

SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_dll_hana DEFINITION FINAL.

  PUBLIC SECTION.

    TYPE-POOLS abap.

    METHODS constructor IMPORTING iv_testrun       TYPE /ba1/xx_dte_test_run
                                  iv_dbcon         TYPE /ba1/tl0dll_ds-db_con
                                  iv_schema        TYPE /ba1/tl0dll_ds-schema_name
                                  iv_tab_view_name TYPE /ba1/tl0dll_ds-tab_view_name.

    METHODS get_ddl_name  RETURNING VALUE(rv_ddl) TYPE string.

*/    YTU - Dynamic table generation
    METHODS generate.
*/    ******************************

*/  DDL changes
    METHODS create_table IMPORTING iv_ddl TYPE string.
    METHODS alter_table  IMPORTING iv_ddl TYPE string.
    METHODS drop_table   IMPORTING iv_ddl TYPE string.

*/  Transport log
    METHODS display_log.

  PRIVATE SECTION.

    DATA go_sql_con        TYPE REF TO cl_sql_connection.
    DATA gv_testrun        TYPE /ba1/xx_dte_test_run.
    DATA gv_dbcon          TYPE /ba1/tl0dll_ds-db_con.
    DATA gv_schema         TYPE /ba1/tl0dll_ds-schema_name.
    DATA gv_tab_view_name  TYPE /ba1/tl0dll_ds-tab_view_name.

    METHODS execute_ddl              IMPORTING iv_ddl           TYPE string
                                     RETURNING VALUE(rv_failed) TYPE boolean.

*/    YTU - methods for query execution
    METHODS execute_query              IMPORTING iv_query  TYPE string
                                       EXPORTING
                                                 ev_failed TYPE boolean
                                                 e_result  TYPE REF TO cl_sql_result_set .
*/    ********************************************


*/    YTU - methods for table generation statements
    METHODS create_table_check_stmt     RETURNING VALUE(rv_chk_stmt) TYPE string.
    METHODS create_table_compare_stmt   RETURNING VALUE(rv_cmp_stmt) TYPE string.
    METHODS create_table_create_stmt    RETURNING VALUE(rv_create_stmt)  TYPE string.
    METHODS create_table_alter_stmt
      IMPORTING
        it_add_cols   TYPE tt_stg_ctlg
        it_upd_cols   TYPE tt_stg_ctlg
        it_del_cols   TYPE tt_stg_ctlg
        it_pky_cols   TYPE tt_stg_ctlg
      EXPORTING
        rt_alter_stmt TYPE tt_stmts.
    METHODS get_catalog_table_columns   RETURNING VALUE(rt_cols)         TYPE tt_stg_ctlg.
    METHODS compare_table
      EXPORTING
        rt_add_cols TYPE tt_stg_ctlg
        rt_upd_cols TYPE tt_stg_ctlg
        rt_del_cols TYPE tt_stg_ctlg
        rt_pky_cols TYPE tt_stg_ctlg.
    METHODS determine_precision
      IMPORTING is_stg_ctlg    TYPE ztfpsl_stg_ctlg
      RETURNING VALUE(rv_prec) TYPE string.
    METHODS delimit_statement
      IMPORTING iv_stmt        TYPE string
      RETURNING VALUE(rv_stmt) TYPE string.
*/    ********************************************

    METHODS add_msg_to_transport_log IMPORTING io_ex TYPE REF TO cx_root OPTIONAL.

ENDCLASS.


CLASS lcl_dll_hana IMPLEMENTATION.

  METHOD constructor.

*/  selection parameters
    gv_testrun       = iv_testrun .
    gv_dbcon         = iv_dbcon   .
    gv_schema        = iv_schema  .
    gv_tab_view_name = iv_tab_view_name  .

  ENDMETHOD.

  METHOD get_ddl_name.

    rv_ddl = |"{ gv_schema }"."{ gv_tab_view_name }" |.

  ENDMETHOD.

  METHOD generate.
    DATA lt_chk TYPE STANDARD TABLE OF i.

*/  Table name check
    TRY.
        DATA(lv_chk_query) = create_table_check_stmt( ).
        execute_query(
          EXPORTING
                iv_query    = lv_chk_query
          IMPORTING
                ev_failed   = DATA(lv_failed)
                e_result    = DATA(lr_result) ).
        IF lv_failed = abap_true.
          MESSAGE i999(67) WITH 'Table generation failed: ' gv_tab_view_name INTO DATA(lv_dummy).
          add_msg_to_transport_log( ).
          EXIT.
        ENDIF.

        lr_result->set_param_table( itab_ref = REF #( lt_chk ) ).
        DATA(lr_pkg) = lr_result->next_package( ).
        DATA(lv_count) = lt_chk[ 1 ].
        lr_result->close( ).

        CASE lv_count.
          WHEN 0.
            DATA(lv_stmt) = create_table_create_stmt( ).
            create_table( lv_stmt ).
          WHEN 1.
            compare_table(
                IMPORTING
                    rt_add_cols = DATA(lt_add_cols)
                    rt_upd_cols = DATA(lt_upd_cols)
                    rt_del_cols = DATA(lt_del_cols)
                    rt_pky_cols = DATA(lt_pky_cols) ).
            IF NOT (    lt_add_cols IS INITIAL AND
                        lt_upd_cols IS INITIAL AND
                        lt_del_cols IS INITIAL AND
                        lt_pky_cols IS INITIAL  ).
              create_table_alter_stmt(
                  EXPORTING
                      it_add_cols = lt_add_cols
                      it_upd_cols = lt_upd_cols
                      it_del_cols = lt_del_cols
                      it_pky_cols = lt_pky_cols
                  IMPORTING
                      rt_alter_stmt = DATA(lt_stmts) ).
              LOOP AT lt_stmts ASSIGNING FIELD-SYMBOL(<stmt>).
                alter_table( <stmt> ).
              ENDLOOP.
            ENDIF.
        ENDCASE.

      CATCH cx_sql_exception INTO DATA(lx_error).
        MESSAGE i999(67) WITH 'SQL error: ' gv_tab_view_name INTO lv_dummy.
        add_msg_to_transport_log( lx_error ).
        EXIT.
    ENDTRY.
  ENDMETHOD.

  METHOD create_table.

    DATA lv_dummy TYPE char1.

    MESSAGE i999(67) WITH 'CREATE for' gv_tab_view_name iv_ddl INTO lv_dummy.
    add_msg_to_transport_log( ).
    IF execute_ddl( iv_ddl ) EQ abap_true.
      MESSAGE e999(67) WITH 'CREATE failed' gv_tab_view_name iv_ddl INTO lv_dummy.
    ELSE.
      MESSAGE s999(67) WITH 'CREATE successful' gv_tab_view_name iv_ddl INTO lv_dummy.
    ENDIF.
    add_msg_to_transport_log( ).


  ENDMETHOD.

  METHOD alter_table.

    DATA lv_dummy TYPE char1.

    MESSAGE i999(67) WITH 'ALTER for' gv_tab_view_name iv_ddl INTO lv_dummy.
    add_msg_to_transport_log( ).
    IF execute_ddl( iv_ddl ) EQ abap_true.
      MESSAGE e999(67) WITH 'ALTER failed' gv_tab_view_name iv_ddl INTO lv_dummy.
    ELSE.
      MESSAGE s999(67) WITH 'ALTER sucessful' gv_tab_view_name iv_ddl INTO lv_dummy.
    ENDIF.
    add_msg_to_transport_log( ).

  ENDMETHOD.

  METHOD drop_table.

    DATA lv_dummy TYPE char1.

    MESSAGE i707(do) WITH gv_tab_view_name INTO lv_dummy.
    add_msg_to_transport_log( ).
    IF execute_ddl( iv_ddl ) EQ abap_true.
      MESSAGE e655(do) WITH gv_schema gv_tab_view_name INTO lv_dummy..
    ELSE.
      MESSAGE i728(do) WITH gv_tab_view_name INTO lv_dummy.
    ENDIF.
    add_msg_to_transport_log( ).

  ENDMETHOD.

  METHOD execute_ddl.

    TRY.
        DATA(sql_con)  = cl_sql_connection=>get_connection( gv_dbcon ).
        DATA(sql_stmt) = sql_con->create_statement( CONV #( gv_tab_view_name ) ).
        sql_stmt->execute_ddl( iv_ddl ).

      CATCH cx_sql_exception
            cx_root INTO DATA(lx).
        rv_failed = abap_true.
        add_msg_to_transport_log( lx ).
    ENDTRY.

  ENDMETHOD.

  METHOD execute_query.

    TRY.
        DATA(sql_con)  = cl_sql_connection=>get_connection( gv_dbcon ).
        DATA(sql_stmt) = sql_con->create_statement( CONV #( gv_tab_view_name ) ).
        e_result = sql_stmt->execute_query( iv_query ).

      CATCH cx_sql_exception
            cx_root INTO DATA(lx).
        ev_failed = abap_true.
        add_msg_to_transport_log( lx ).
    ENDTRY.

  ENDMETHOD.

  METHOD add_msg_to_transport_log.

    CONSTANTS: BEGIN OF gc_level,
                 debug   TYPE n     VALUE '4', "SAP-intern
                 info    TYPE n     VALUE '3', "Information
                 success TYPE n     VALUE '3', "Success
                 warning TYPE n     VALUE '2', "Warning
                 error   TYPE n     VALUE '2', "Error
               END   OF gc_level.

    DATA lt_protocol  TYPE sprot_u_t.

    IF io_ex IS BOUND.

      DATA(messages) = /ba1/cl_al_x0_msg_helper=>s_extract_messages( io_ex ).

      LOOP AT messages ASSIGNING FIELD-SYMBOL(<msg>).
        APPEND INITIAL LINE TO lt_protocol ASSIGNING FIELD-SYMBOL(<prot>).
        IF <msg>-msgty  EQ 'E'.
          <prot>-severity = 'E'.
          <prot>-level    = gc_level-error.
        ELSE.
          <prot>-level    = gc_level-info.
        ENDIF.
*
*        IF <msg>-msgty  EQ 'I'.
*          <prot>-level    = gc_level-info.
*        ELSEIF <msg>-msgty  EQ 'S'.
*          <prot>-level    = gc_level-success.
*        ELSEIF <msg>-msgty  EQ 'W'.
*          <prot>-level    = gc_level-warning.
*        ELSEIF <msg>-msgty  EQ 'E'.
*          <prot>-level    = gc_level-error.
*        ENDIF.
        <prot>-ag       = <msg>-msgid.
        <prot>-msgnr    = <msg>-msgno.
        <prot>-var1     = <msg>-msgv1.
        <prot>-var2     = <msg>-msgv2.
        <prot>-var3     = <msg>-msgv3.
        <prot>-var4     = <msg>-msgv4.
        <prot>-langu    = sy-langu.
      ENDLOOP.
    ELSE.
      APPEND INITIAL LINE TO lt_protocol ASSIGNING <prot>.
      IF sy-msgty  EQ 'E'.
        <prot>-severity = 'E'.
        <prot>-level    = gc_level-error.
      ELSE.
        <prot>-level    = gc_level-info.
      ENDIF.
      <prot>-ag       = sy-msgid.
      <prot>-msgnr    = sy-msgno.
      <prot>-var1     = sy-msgv1.
      <prot>-var2     = sy-msgv2.
      <prot>-var3     = sy-msgv3.
      <prot>-var4     = sy-msgv4.
      <prot>-langu    = sy-langu.
    ENDIF.

    CALL FUNCTION 'TR_APPEND_LOG'
      TABLES
        xmsg           = lt_protocol
      EXCEPTIONS
        file_not_found = 0
        wrong_call     = 0
        OTHERS         = 0.

  ENDMETHOD.

  METHOD create_table_check_stmt.
    rv_chk_stmt = |SELECT COUNT(*) AS table_line FROM TABLES WHERE SCHEMA_NAME = '{ gv_schema }' AND TABLE_NAME = '{ gv_tab_view_name }'|.
  ENDMETHOD.

  METHOD create_table_compare_stmt.
    rv_cmp_stmt =
      |SELECT a.TABLE_NAME, a.COLUMN_NAME, | &
      |CASE WHEN b.IS_PRIMARY_KEY = 'TRUE' THEN 'X' ELSE '' END as IS_KEY, |   &
      |CASE WHEN a.IS_NULLABLE = 'TRUE' THEN '' ELSE 'X' END as NOT_NULL, | &
      |a.DATA_TYPE_NAME as TYPES, a.LENGTH, a.SCALE as DECIMALS |    &
      |FROM TABLE_COLUMNS a LEFT OUTER JOIN CONSTRAINTS b ON |    &
      |a.SCHEMA_NAME = b.SCHEMA_NAME AND a.TABLE_NAME = b.TABLE_NAME  AND a.COLUMN_NAME = b.COLUMN_NAME |  &
      |WHERE a.SCHEMA_NAME = '{ gv_schema }' AND a.TABLE_NAME = '{ gv_tab_view_name }'|.
  ENDMETHOD.

  METHOD create_table_create_stmt.
    rv_create_stmt = |CREATE COLUMN TABLE "{ gv_schema }"."{ gv_tab_view_name }" (|.

    DATA(lt_ctlg) = get_catalog_table_columns( ).
    LOOP AT lt_ctlg ASSIGNING FIELD-SYMBOL(<str_ctlg>).

      rv_create_stmt = |{ rv_create_stmt }"{ <str_ctlg>-column_name }" { <str_ctlg>-type }|.
      rv_create_stmt = |{ rv_create_stmt }{ determine_precision( <str_ctlg> ) } |.
      rv_create_stmt = |{ rv_create_stmt }{ COND #( WHEN <str_ctlg>-not_null = abap_true THEN 'NOT NULL ' ELSE 'NULL ' ) }, |.
    ENDLOOP.

    LOOP AT lt_ctlg ASSIGNING <str_ctlg> WHERE is_key = abap_true.
      AT FIRST.
        rv_create_stmt = |{ rv_create_stmt }PRIMARY KEY (|.
      ENDAT.

      rv_create_stmt = |{ rv_create_stmt }{ <str_ctlg>-column_name }, |.

      AT LAST.
        rv_create_stmt = |{ delimit_statement( rv_create_stmt ) }), |.
      ENDAT.
    ENDLOOP.

*/    Other creation clauses
*/

    rv_create_stmt = |{ delimit_statement( rv_create_stmt ) })|.
  ENDMETHOD.

  METHOD create_table_alter_stmt.
    DATA(lv_stmt_hdr) = |ALTER TABLE "{ gv_schema }"."{ gv_tab_view_name }" |.

    IF it_add_cols IS NOT INITIAL.
      LOOP AT it_add_cols ASSIGNING FIELD-SYMBOL(<str_col>).
        AT FIRST.
          DATA(lv_alter_stmt) = |{ lv_stmt_hdr }ADD (|.
        ENDAT.

        lv_alter_stmt = |{ lv_alter_stmt }"{ <str_col>-column_name }" { <str_col>-type }|.
        lv_alter_stmt = |{ lv_alter_stmt }{ determine_precision( <str_col> ) } |.
        lv_alter_stmt = |{ lv_alter_stmt }{ COND #( WHEN <str_col>-not_null = abap_true THEN 'NOT NULL ' ELSE 'NULL ' ) }, |.

        AT LAST.
          lv_alter_stmt = |{ delimit_statement( lv_alter_stmt ) }) |.
        ENDAT.
      ENDLOOP.

      APPEND lv_alter_stmt TO rt_alter_stmt.
    ENDIF.

    IF it_upd_cols IS NOT INITIAL.
      LOOP AT it_upd_cols ASSIGNING <str_col>.
        AT FIRST.
          lv_alter_stmt = |{ lv_stmt_hdr }ALTER (|.
        ENDAT.

        lv_alter_stmt = |{ lv_alter_stmt }"{ <str_col>-column_name }" { <str_col>-type }|.
        lv_alter_stmt = |{ lv_alter_stmt }{ determine_precision( <str_col> ) } |.
        lv_alter_stmt = |{ lv_alter_stmt }{ COND #( WHEN <str_col>-not_null = abap_true THEN 'NOT NULL ' ELSE 'NULL ' ) }, |.

        AT LAST.
          lv_alter_stmt = |{ delimit_statement( lv_alter_stmt ) }) |.
        ENDAT.
      ENDLOOP.

      APPEND lv_alter_stmt TO rt_alter_stmt.
    ENDIF.

    IF it_del_cols IS NOT INITIAL.
      LOOP AT it_del_cols ASSIGNING <str_col>.
        AT FIRST.
          lv_alter_stmt = |{ lv_stmt_hdr }DROP (|.
        ENDAT.

        lv_alter_stmt = |{ lv_alter_stmt }"{ <str_col>-column_name }", |.

        AT LAST.
          lv_alter_stmt = |{ delimit_statement( lv_alter_stmt ) }) |.
        ENDAT.
      ENDLOOP.

      APPEND lv_alter_stmt TO rt_alter_stmt.
    ENDIF.

    IF it_pky_cols IS NOT INITIAL.
      LOOP AT it_pky_cols ASSIGNING <str_col>.
        AT FIRST.
          lv_alter_stmt = |{ lv_stmt_hdr }ADD PRIMARY KEY (|.
        ENDAT.

        lv_alter_stmt = |{ lv_alter_stmt }"{ <str_col>-column_name }", |.

        AT LAST.
          lv_alter_stmt = |{ delimit_statement( lv_alter_stmt ) }) |.
        ENDAT.
      ENDLOOP.

      APPEND lv_alter_stmt TO rt_alter_stmt.
    ENDIF.
  ENDMETHOD.

  METHOD compare_table.
    DATA:
          lt_old_cols TYPE tt_stg_ctlg.

    TRY.
        DATA(lt_new_cols) = get_catalog_table_columns( ).
        DATA(lv_cmp_query) = create_table_compare_stmt( ).
        execute_query(
          EXPORTING
                iv_query    = lv_cmp_query
          IMPORTING
                ev_failed   = DATA(lv_failed)
                e_result    = DATA(lr_result) ).
        IF lv_failed = abap_true.
          MESSAGE i999(67) WITH 'Table generation failed: ' gv_tab_view_name INTO DATA(lv_dummy).
          add_msg_to_transport_log( ).
          EXIT.
        ENDIF.

        lr_result->set_param_table( itab_ref = REF #( lt_old_cols ) ).
        DATA(lr_pkg) = lr_result->next_package( ).
        lr_result->close( ).

      CATCH cx_sql_exception INTO DATA(lx_error).
        MESSAGE i999(67) WITH 'SQL error: ' gv_tab_view_name INTO lv_dummy.
        add_msg_to_transport_log( lx_error ).
        EXIT.
    ENDTRY.

*    Primary key check - Cannot drop/un-assign existing primary key columns!
    DATA(lv_pk_changed) = abap_false.
*    ****************************************************
    LOOP AT lt_old_cols ASSIGNING FIELD-SYMBOL(<str_old_col>).
      READ TABLE lt_new_cols WITH KEY column_name = <str_old_col>-column_name ASSIGNING FIELD-SYMBOL(<str_new_col>).
      IF sy-subrc <> 0.
        IF <str_old_col>-is_key = abap_true.
          lv_pk_changed = abap_true.
          EXIT.
        ELSE.
          APPEND <str_old_col> TO rt_del_cols.
        ENDIF.
      ELSE.
        IF <str_old_col>-is_key <> <str_new_col>-is_key.
          IF <str_old_col>-is_key = abap_true.
            lv_pk_changed = abap_true.
            EXIT.
          ELSE.
            APPEND <str_new_col> TO rt_pky_cols.
          ENDIF.
        ENDIF.

        IF ( <str_new_col>-length <> 0 AND NOT (
          <str_old_col>-length    = <str_new_col>-length    AND
          <str_old_col>-decimals  = <str_new_col>-decimals ) )  OR
          <str_old_col>-not_null  <> <str_new_col>-not_null  .
          IF <str_new_col>-length <> 0 AND <str_new_col>-length < <str_old_col>-length.
            MESSAGE i999(67) WITH |Shortening of fields prohibited: | |{ gv_tab_view_name }/|  <str_new_col>-column_name DISPLAY LIKE 'E'.
            add_msg_to_transport_log( ).
            LEAVE PROGRAM.
          ELSE.
            APPEND <str_new_col> TO rt_upd_cols.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lv_pk_changed = abap_true.
      MESSAGE i999(67) WITH |Primary key change/deletion prohibited: | |{ gv_tab_view_name }/|  <str_new_col>-column_name DISPLAY LIKE 'E'.
      add_msg_to_transport_log( ).
      LEAVE PROGRAM.
    ENDIF.

    LOOP AT lt_new_cols ASSIGNING <str_new_col>.
      READ TABLE lt_old_cols WITH KEY column_name = <str_new_col>-column_name ASSIGNING <str_old_col>.
      IF sy-subrc <> 0.
        IF <str_new_col>-is_key = abap_true.
          APPEND <str_new_col> TO rt_pky_cols.
        ENDIF.

        APPEND <str_new_col> TO rt_add_cols.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_catalog_table_columns.
    SELECT *
      INTO TABLE rt_cols
      FROM ztfpsl_stg_ctlg
      WHERE table_name = gv_tab_view_name.

    IF sy-subrc <> 0.
      MESSAGE i999(67) WITH |Failed reading table category: | |{ gv_schema }/|  gv_tab_view_name DISPLAY LIKE 'E'.
      add_msg_to_transport_log( ).
      LEAVE PROGRAM.
    ENDIF.
  ENDMETHOD.

  METHOD delimit_statement.
    DATA(lv_stmt) = iv_stmt.
    CONDENSE lv_stmt.
    DATA(lv_len) = strlen( lv_stmt ) - 1.
    rv_stmt = |{ lv_stmt+0(lv_len) }|.
  ENDMETHOD.

  METHOD determine_precision.
    rv_prec = COND string(
          WHEN is_stg_ctlg-length IS INITIAL AND is_stg_ctlg-decimals IS INITIAL THEN ''
          WHEN is_stg_ctlg-length IS NOT INITIAL AND is_stg_ctlg-decimals IS INITIAL THEN |({ is_stg_ctlg-length })|
          ELSE |({ is_stg_ctlg-length },{ is_stg_ctlg-decimals })| ).
  ENDMETHOD.

  METHOD display_log.

    CALL FUNCTION 'TR_FLUSH_LOG'.

  ENDMETHOD.

ENDCLASS.
