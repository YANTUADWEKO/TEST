class ZCL_FPSL_DATALOAD_TRUNCATE definition
  public
  final
  create public .

public section.

  interfaces /BA1/IF_AL_FW_ENRICH_PARAM .
  PROTECTED SECTION.
private section.

  class-data S_REF_ENRICH_PARAM type ref to ZCL_FPSL_DATALOAD_TRUNCATE .
  constants:
    BEGIN OF gc_fieldtype,
      both      TYPE zfpsl_dte_cond_field_type VALUE '01',
      timestamp TYPE zfpsl_dte_cond_field_type VALUE '02',
      date      TYPE zfpsl_dte_cond_field_type VALUE '03',
    END OF gc_fieldtype .
  data:
    gt_stage_trunc TYPE STANDARD TABLE OF zfpsl_stagetrunc .
  data MO_SQL type ref to CL_SQL_STATEMENT .
  data GV_KEY_DATE type /BA1/R6_DTE_KEY_DATE_DELETE .
  data GR_MSG_HANDLER type ref to /BA1/IF_AL_X0_MSG_HANDLER .

  methods FROM_CLAUSE
    importing
      !IS_TRUNC type ZFPSL_STAGETRUNC
    returning
      value(RV_SQLSTMT_FROM) type STRING .
  methods WHERE_CLAUSE
    importing
      !IS_TRUNC type ZFPSL_STAGETRUNC
    returning
      value(RV_SQLSTMT_WHERE) type STRING
    raising
      ZCX_FPSL_DATALOAD .
  methods PREPARE_DELETE
    importing
      !IS_TRUNC type ZFPSL_STAGETRUNC
    returning
      value(RV_SQLSTMT) type STRING
    raising
      ZCX_FPSL_DATALOAD .
  methods PREPARE_LOG
    importing
      !IS_TRUNC type ZFPSL_STAGETRUNC
    returning
      value(RS_TRUNC_LOG) type ZFPSL_STR_DATALOAD_TRUNC_LOG .
  methods PREPARE_SELECT
    importing
      !IS_TRUNC type ZFPSL_STAGETRUNC
    returning
      value(RV_SQLSTMT) type STRING
    raising
      ZCX_FPSL_DATALOAD .
  methods INIT_SETTINGS .
  methods TRUNCATE_TABLES
    importing
      !IV_TEST_RUN type BOOLEAN
      !IT_STAGE_TRUNC like GT_STAGE_TRUNC .
  methods CALCULATE_DATE
    importing
      !IV_RETENTION_TIME type TBA_RETENTION_TIME
    returning
      value(RV_DATE) type DATUM .
  methods TO_SQL_STRING
    importing
      !IV_VALUE type ANY
    returning
      value(RV_VARIABLE) type STRING .
ENDCLASS.



CLASS ZCL_FPSL_DATALOAD_TRUNCATE IMPLEMENTATION.


  METHOD /ba1/if_al_fw_enrich_param~execute.

*/  The method is a simple implementation of a CVPM process to delete the
*/  preload and staging Hana tables which meet certain time criteria.
*/  The SAPHANADB user needs to have access to the HANA SCHEMA to perform
*/  the operations.
*/
*/  Testmode:   will run a SELECT to get the number of records
*/  Updatemode: will run DELETE and returns the number of records deleted
*/  Writes CVPM log entry, Will be scheduled monthly, Config table: ZFPSL_STAGETRUNC
*/
*/  Christian Schmidt, ADWEKO 2022/10/31

*/  data declarations
    DATA lv_dummy           TYPE char1.

    init_settings( ).

    c_str_param-keydate = VALUE #( c_str_param-tab_par_value[ parameter = '/BA1/C11KEYDAT' ]-low OPTIONAL ).

    IF c_str_param-keydate IS INITIAL.
      gv_key_date  = sy-datum.
    ELSE.
      gv_key_date  = c_str_param-keydate.
    ENDIF.
    gr_msg_handler = i_ref_msg_handler.

*/  add test run message
    IF c_str_param-is_test_run EQ abap_true.
      MESSAGE i022(zfpsl_tools) WITH gv_key_date INTO lv_dummy.
    ELSE.
      MESSAGE i023(zfpsl_tools) WITH gv_key_date INTO lv_dummy.
    ENDIF.
    gr_msg_handler->add_sy_msg( ).

*/  call the logic - single step run
    truncate_tables( iv_test_run    = c_str_param-is_test_run
                     it_stage_trunc = gt_stage_trunc
                    ).

  ENDMETHOD.


  METHOD /ba1/if_al_fw_enrich_param~s_create.

    IF s_ref_enrich_param IS NOT BOUND.
      s_ref_enrich_param = NEW #( ).
    ENDIF.

    r_ref_instance  = s_ref_enrich_param.

  ENDMETHOD.


  METHOD calculate_date.

    IF gv_key_date IS NOT INITIAL.
      rv_date = gv_key_date - iv_retention_time.
    ELSE.
      rv_date = sy-datum - iv_retention_time.
    ENDIF.
*/  not needed now
*   CHECK iv_inverted_date EQ abap_true.
*
*   TRANSLATE rv_date USING '09182736455463728190'.

  ENDMETHOD.


  METHOD from_clause.

    rv_sqlstmt_from = ` FROM "` && is_trunc-staging_schema && `"."` && is_trunc-staging_table && `"`.

  ENDMETHOD.


  METHOD init_settings.

*/  load the settings / make accessible for UNIT test
    SELECT * FROM zfpsl_stagetrunc INTO TABLE gt_stage_trunc.
    IF sy-subrc NE 0.
      MESSAGE x020(zfpsl_tools) WITH 'Any table'(001).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_delete.

    IF is_trunc-staging_schema IS INITIAL.
      CLEAR rv_sqlstmt.
      RETURN.
    ENDIF.

    rv_sqlstmt = `DELETE` && from_clause( is_trunc ) && where_clause( is_trunc ).

  ENDMETHOD.


  METHOD prepare_log.

    rs_trunc_log-fpsl_hana_table = is_trunc-staging_schema && `.` && is_trunc-staging_table.

*/  get date and time stamp criteria based on settings
    DATA(lv_date) = calculate_date( is_trunc-retention_days ).

    CASE is_trunc-field_type.
      WHEN gc_fieldtype-date.
        rs_trunc_log-sql_statement = `( ` && is_trunc-date_field && ` LESS THAN ` && lv_date.
      WHEN gc_fieldtype-timestamp.
        rs_trunc_log-sql_statement = `( ` && is_trunc-ts_field   && ` LESS THAN ` && lv_date.
      WHEN gc_fieldtype-both.
        rs_trunc_log-sql_statement = `( ` && is_trunc-date_field && ` AND ` && is_trunc-ts_field && ` LESS THAN ` && lv_date.
      WHEN OTHERS.
        CLEAR rs_trunc_log.
        RETURN. " handled
    ENDCASE.

    rs_trunc_log-sql_statement = rs_trunc_log-sql_statement && ` )` .

  ENDMETHOD.


  METHOD prepare_select.

    IF is_trunc-staging_schema IS INITIAL.
      CLEAR rv_sqlstmt.
      RETURN.
    ENDIF.

    rv_sqlstmt = `SELECT COUNT(*)` && from_clause( is_trunc ) && where_clause( is_trunc ).

  ENDMETHOD.


  METHOD to_sql_string.

    rv_variable = `'` && iv_value && `'`.

  ENDMETHOD.


  METHOD truncate_tables.

*/  data declarations
    DATA lv_dummy           TYPE char1.
    DATA lv_sqlstmt         TYPE string.

    IF mo_sql  IS NOT BOUND.
      mo_sql = NEW cl_sql_statement( ).
    ENDIF.

    LOOP AT it_stage_trunc ASSIGNING FIELD-SYMBOL(<stage_trunc>).
      TRY.
          DATA(ls_trunc_log) = prepare_log( <stage_trunc> ).

          IF iv_test_run EQ abap_true.
*/          build SQL statment for test run - SELECT ONLY!
            lv_sqlstmt = prepare_select( <stage_trunc> ).
            DATA(lr_result) = mo_sql->execute_query( lv_sqlstmt ).
            lr_result->set_param( REF #( ls_trunc_log-records_deleted ) ).
            DATA(lrc) = lr_result->next( ).
          ELSE.
*/          build SQL statement for delete run - DELETE RECORDS!
            lv_sqlstmt = prepare_delete( <stage_trunc> ).
            ls_trunc_log-records_deleted = mo_sql->execute_update( lv_sqlstmt ).
          ENDIF.

*/        write log entry into handler
          MESSAGE i021(zfpsl_tools) WITH ls_trunc_log-records_deleted
                                         ls_trunc_log-fpsl_hana_table
                                         ls_trunc_log-sql_statement(50)
                                         ls_trunc_log-sql_statement+50(50)
                                    INTO lv_dummy.
          gr_msg_handler->add_sy_msg( ).

        CATCH zcx_fpsl_dataload     INTO DATA(lx_fpsl).
          gr_msg_handler->add_exception( lx_fpsl ).

        CATCH cx_sql_exception      INTO DATA(lx_sql).
          gr_msg_handler->add_exception( lx_sql ).

        CATCH cx_parameter_invalid  INTO DATA(lx_par).
          gr_msg_handler->add_exception( lx_par ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD where_clause.

*/  get date based on settings
    DATA(lv_date) = to_sql_string( calculate_date( is_trunc-retention_days ) ).

    CASE is_trunc-field_type.
      WHEN gc_fieldtype-date.
        rv_sqlstmt_where = ` WHERE ` && is_trunc-date_field && ` < ` && lv_date.
      WHEN gc_fieldtype-timestamp.
        rv_sqlstmt_where = ` WHERE ` && is_trunc-ts_field   && ` < ` && `TO_TIMESTAMP(` && lv_date && `)`.
      WHEN gc_fieldtype-both.
        rv_sqlstmt_where = ` WHERE ` && is_trunc-date_field && ` < ` && lv_date  &&
                           ` AND `   && is_trunc-ts_field   && ` < ` && `TO_TIMESTAMP(` && lv_date && `)`.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_fpsl_dataload
          EXPORTING
            textid = zcx_fpsl_dataload=>setup_missing
            msgv1  = CONV #( is_trunc-staging_schema && '.' && is_trunc-staging_table ).

    ENDCASE.

*    IF is_trunc-status_processed IS NOT INITIAL.
*      rv_sqlstmt_where = rv_sqlstmt_where && ` AND ` && is_trunc-status_field && ` = ` &&
*                         to_sql_string( is_trunc-status_processed ).
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
