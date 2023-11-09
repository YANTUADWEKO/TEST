CLASS zcl_z_c_abaplog_msg_1_impl DEFINITION INHERITING FROM zcl_z_c_abaplog_1_impl
  PUBLIC

  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
    METHODS:
      load_messages
        IMPORTING
          iv_handle TYPE balloghndl
        EXPORTING
          et_msgs   TYPE bal_t_msgr ,
      handle_aggregate REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_z_c_abaplog_msg_1_impl IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    DATA:
      lv_extnum      TYPE balnrext,
      lv_lognum      TYPE balognr,
      lv_handle      TYPE balloghndl,
      lv_msg_text    TYPE string,
      lt_msg_unpaged TYPE bal_t_msgr,
      lt_msg_out     TYPE STANDARD TABLE OF bal_s_msgr,
      lt_hdl         TYPE bal_t_logh,
      lt_out         TYPE STANDARD TABLE OF z_c_abaplog_msg_1.

    IF io_request->is_data_requested( ).
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

      DATA(lt_rng_filter) = get_filter_range( io_request ).

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
          it_data     = lt_msg_unpaged
      IMPORTING
          et_filtered_data = lt_msg_out  ).

*        Apply user sorting
      handle_sort(
      EXPORTING
          io_request  = io_request
          it_data     = lt_msg_out
      IMPORTING
          et_sorted_data = lt_msg_out ).

*        Apply user filter and paging range to create output data set
      LOOP AT lt_msg_out ASSIGNING FIELD-SYMBOL(<str_msg>) FROM lv_offset TO ( lv_offset + lv_row_count ).

        INSERT VALUE z_c_abaplog_msg_1(
            extnumber     = lv_extnum
            lognumber     = lv_lognum
            log_handle    = lv_handle
            msg_num       = <str_msg>-msgnumber ) INTO TABLE lt_out ASSIGNING FIELD-SYMBOL(<str_msg_out>).
        MOVE-CORRESPONDING <str_msg> TO <str_msg_out>.
        <str_msg_out>-text = <str_msg>-msg_txt.
        <str_msg_out>-criticality = determine_criticality( <str_msg>-msgty ).
      ENDLOOP.

      io_response->set_total_number_of_records( lines( lt_msg_out ) ).
*      Set return data
      io_response->set_data( lt_out ).
    ENDIF.

  ENDMETHOD.

  METHOD load_messages.
    DATA:
        lt_hdl      TYPE bal_t_logh.

*    Return cached messages if retrieving the same log, or save the handle and load messages from DB
    IF iv_handle <> pv_hdl.
      pv_hdl = iv_handle.
      CLEAR pt_msgs.

      CALL FUNCTION 'BAL_LOG_EXIST'
        EXPORTING
          i_log_handle  = iv_handle
        EXCEPTIONS
          log_not_found = 1.
      IF sy-subrc <> 0.
        INSERT iv_handle INTO TABLE lt_hdl.
        CALL FUNCTION 'BAL_DB_RELOAD'
          EXPORTING
            i_t_log_handle = lt_hdl.
      ENDIF.

      CALL FUNCTION 'BAL_LOG_READ'
        EXPORTING
          i_log_handle = iv_handle
          i_read_texts = abap_true
        IMPORTING
          et_msg       = pt_msgs.
    ENDIF.

    et_msgs = pt_msgs.
  ENDMETHOD.

  METHOD handle_aggregate.

  ENDMETHOD.

ENDCLASS.
