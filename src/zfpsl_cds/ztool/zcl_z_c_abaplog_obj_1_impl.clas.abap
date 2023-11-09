CLASS zcl_z_c_abaplog_obj_1_impl DEFINITION INHERITING FROM zcl_z_c_abaplog_MSG_1_impl
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
             tt_log_obj   TYPE STANDARD TABLE OF z_c_abaplog_obj_2.

    METHODS:
      if_rap_query_provider~select REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      handle_aggregate REDEFINITION,

      parse_context
        IMPORTING
          iv_extnum TYPE balnrext
          iv_lognum TYPE balognr
          iv_handle TYPE balloghndl
          it_msgs   TYPE bal_t_msgr
        EXPORTING
          et_ctxt   TYPE tt_log_obj.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_z_c_abaplog_obj_1_impl IMPLEMENTATION.

  METHOD if_rap_query_provider~select .
    DATA:
      lv_extnum      TYPE balnrext,
      lv_lognum      TYPE balognr,
      lv_handle      TYPE balloghndl,
      lt_msg_unpaged TYPE bal_t_msgr,
      lt_context_out TYPE tt_log_obj,
      lt_hdl         TYPE bal_t_logh,
      lt_out         TYPE STANDARD TABLE OF z_c_abaplog_obj_2.

*      Data/Count request flag checks are removed in unmanaged aggregation view -
*      Grouping layout will generate requests that need data but come without the check flag
      get_log_keyset(
        EXPORTING
            io_request = io_request
        IMPORTING
            ev_extnum = lv_extnum
            ev_lognum = lv_lognum
            ev_handle = lv_handle ).
      load_messages(
        EXPORTING
          iv_handle = lv_handle
        IMPORTING
          et_msgs = lt_msg_unpaged ).

      parse_context(
        EXPORTING
        iv_extnum = lv_extnum
        iv_lognum = lv_lognum
        iv_handle = lv_handle
          it_msgs = lt_msg_unpaged
        IMPORTING
          et_ctxt = DATA(lt_context) ).

*        Get paging and user filter
      get_paging_index(
          EXPORTING
              io_request  = io_request
          IMPORTING
              ev_idx_fr   = DATA(lv_offset)
              ev_idx_to    = DATA(lv_row_count) ).

*        Apply user filter
      handle_filter(
      EXPORTING
          io_request  = io_request
          it_data     = lt_context
      IMPORTING
          et_filtered_data = lt_context_out  ).

*        Apply aggregation
      handle_aggregate(
      EXPORTING
          io_request  = io_request
          it_data     = lt_context_out
      IMPORTING
          et_aggregated_data = lt_context_out  ).

*        Apply user sorting
      handle_sort(
      EXPORTING
          io_request  = io_request
          it_data     = lt_context_out
      IMPORTING
          et_sorted_data = lt_context_out ).

*        Apply user filter and paging range to create output data set
      LOOP AT lt_context_out ASSIGNING FIELD-SYMBOL(<str_ctxt>) FROM lv_offset TO ( lv_offset + lv_row_count ).

        INSERT VALUE z_c_abaplog_obj_2(
            extnumber     = lv_extnum
            lognumber     = lv_lognum
            log_handle    = lv_handle
            msg_num       = <str_ctxt>-msg_num ) INTO TABLE lt_out ASSIGNING FIELD-SYMBOL(<str_msg_out>).
        MOVE-CORRESPONDING <str_ctxt> TO <str_msg_out>.
      ENDLOOP.

*      Set return data
      io_response->set_data( lt_out ).

      io_response->set_total_number_of_records( lines( lt_context_out ) ).
  ENDMETHOD.

  METHOD parse_context.
    DATA:
      lr_tab_obj   TYPE REF TO cl_abap_tabledescr,
      lr_struc_obj TYPE REF TO cl_abap_structdescr,
      lr_struc_msg TYPE REF TO cl_abap_structdescr,
      lt_ctxt      TYPE tt_log_obj,
      lt_ctxt_pre  TYPE tt_log_obj,
      lt_bt        TYPE STANDARD TABLE OF /ba1/f2_bt_flat.

    lr_tab_obj ?= cl_abap_structdescr=>describe_by_data( lt_ctxt ) .
    lr_struc_obj ?= lr_tab_obj->get_table_line_type( ).
    DATA(lt_obj_flds) = lr_struc_obj->get_components( ).
    LOOP AT it_msgs ASSIGNING FIELD-SYMBOL(<str_msgs>) WHERE context IS NOT INITIAL.

      DATA(lv_ctxt_val) = <str_msgs>-context-value.
      INSERT INITIAL LINE INTO TABLE lt_ctxt ASSIGNING FIELD-SYMBOL(<str_obj>).
      <str_obj>-extnumber   = iv_extnum.
      <str_obj>-log_handle  = iv_handle.
      <str_obj>-lognumber   = iv_lognum.
      <str_obj>-msg_num     = <str_msgs>-msgnumber.
      <str_obj>-msgty       = <str_msgs>-msgty.

      lr_struc_msg ?= cl_abap_structdescr=>describe_by_name( <str_msgs>-context-tabname ).
      DATA(lt_ctxt_flds) = lr_struc_msg->get_ddic_field_list( ).
      LOOP AT lt_ctxt_flds ASSIGNING FIELD-SYMBOL(<str_ctxt_flds>).
        IF line_exists( lt_obj_flds[ name = <str_ctxt_flds>-fieldname ] ).
          ASSIGN COMPONENT <str_ctxt_flds>-fieldname OF STRUCTURE <str_obj> TO FIELD-SYMBOL(<val_ctxt_val>).
          <val_ctxt_val> = substring( val = lv_ctxt_val off = <str_ctxt_flds>-offset / 2 len = <str_ctxt_flds>-leng ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    SORT lt_ctxt BY /ba1/c35btsrc /ba1/c35btran /ba1/c35posnr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_ctxt.

    IF lt_ctxt IS NOT INITIAL.
      SELECT
            b~extnumber,
            b~log_handle,
            b~lognumber,
            b~msg_num,
            b~msgty,
            a~ext_contract_id   AS /ba1/c55contid,
            a~source_system     AS /ba1/c35btsrc,
            a~bt_id             AS /ba1/c35btran,
            a~p_number             AS /ba1/c35posnr,
            a~trans_amount      AS /ba1/k5samobj,
            a~trans_curr        AS /ba1/obj_curr
        FROM /ba1/f2_bt_flat    AS a
        INNER JOIN @lt_ctxt     AS b ON
        ( a~bt_id             = b~/ba1/c35btran   AND b~/ba1/c35btran  IS NOT INITIAL )    AND
        ( a~p_number          = b~/ba1/c35posnr   AND b~/ba1/c35posnr  IS NOT INITIAL )    AND
        ( a~source_system     = b~/ba1/c35btsrc   AND b~/ba1/c35btsrc  IS NOT INITIAL )
    INTO CORRESPONDING FIELDS OF TABLE @lt_ctxt_pre.
      SELECT
          extnumber,
          log_handle,
          lognumber,
          msg_num,
          msgty,
          /ba1/c55contid,
          /ba1/c35btsrc,
          /ba1/c35btran,
          /ba1/k5samobj,
          /ba1/obj_curr
      FROM @lt_ctxt_pre AS p
      GROUP BY
          extnumber,
          log_handle,
          lognumber,
          msg_num,
          msgty,
          /ba1/c55contid,
          /ba1/c35btsrc,
          /ba1/c35btran,
          /ba1/k5samobj,
          /ba1/obj_curr
      INTO CORRESPONDING FIELDS OF TABLE @et_ctxt.
    ENDIF.
  ENDMETHOD.

  METHOD handle_aggregate.
    DATA:
      lt_source_data     TYPE tt_log_obj,
      lt_aggregated_data TYPE tt_log_obj.

    lt_source_data = it_data.

    get_aggregate_statements(
        EXPORTING
            io_request = io_request
        IMPORTING
            ev_select   = DATA(lv_select)
            ev_group_by = DATA(lv_group_by) ).

    SELECT (lv_select)
    FROM @lt_source_data AS a
    GROUP BY (lv_group_by)
    INTO CORRESPONDING FIELDS OF TABLE @lt_aggregated_data.

    et_aggregated_data = lt_aggregated_data.
  ENDMETHOD.

ENDCLASS.
