CLASS zcl_z_c_abaplog_param_1_impl DEFINITION INHERITING FROM zcl_z_c_abaplog_1_impl
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
             tt_z_c_abaplog_param_1 TYPE STANDARD TABLE OF z_c_abaplog_param_1 WITH KEY lognumber log_handle extnumber param_id.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
    METHODS:
      load_parameters
        IMPORTING
                  io_request      TYPE REF TO if_rap_query_request
        RETURNING
                  VALUE(rt_param) TYPE tt_z_c_abaplog_param_1
        RAISING   cx_rap_query_provider,
      handle_aggregate REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_z_c_abaplog_param_1_impl IMPLEMENTATION.
  METHOD if_rap_query_provider~select.
    DATA:
      lt_param_unpaged TYPE STANDARD TABLE OF z_c_abaplog_param_1,
      lt_param_out     TYPE STANDARD TABLE OF z_c_abaplog_param_1,
      lt_out           TYPE STANDARD TABLE OF z_c_abaplog_param_1.

    IF io_request->is_data_requested( ).
      lt_param_unpaged = load_parameters( io_request ).

      get_paging_index(
      EXPORTING
          io_request  = io_request
          IMPORTING
          ev_idx_fr = DATA(lv_offset)
          ev_idx_to = DATA(lv_row_count) ).

*        Apply user filter
      handle_filter(
      EXPORTING
          io_request  = io_request
          it_data     = lt_param_unpaged
      IMPORTING
          et_filtered_data = lt_param_out  ).

*        Apply user sorting
      handle_sort(
      EXPORTING
          io_request  = io_request
          it_data     = lt_param_out
      IMPORTING
          et_sorted_data = lt_param_out ).

*      Move output data per current paging offset
      LOOP AT lt_param_out ASSIGNING FIELD-SYMBOL(<str_out>) FROM lv_offset TO ( lv_offset + lv_row_count ).
        INSERT <str_out> INTO TABLE lt_out.
      ENDLOOP.

      io_response->set_total_number_of_records( lines( lt_param_out ) ).
      io_response->set_data( lt_out ).
    ENDIF.
  ENDMETHOD.

  METHOD load_parameters.

    DATA(lt_rng_filter) = get_filter_range( io_request ).

    DATA(l_pair_run_id) = lt_rng_filter[ name = 'EXTNUMBER' ].
    DATA(l_rng_run_id) = l_pair_run_id-range[ 1 ].
    DATA(lv_run_id) = CONV /ba1/fw_dte_run( l_rng_run_id-low ).
    DATA(l_pair_hdl) = lt_rng_filter[ name = 'LOG_HANDLE' ].
    DATA(l_rng_handle) = l_pair_hdl-range[ 1 ].
    DATA(lv_handle) = CONV balloghndl( l_rng_handle-low ).
    DATA(l_pair_log_id) = lt_rng_filter[ name = 'LOGNUMBER' ].
    DATA(l_rng_log_id) = l_pair_log_id-range[ 1 ].
    DATA(lv_log_id) = CONV /ba1/fw_dte_run( l_rng_log_id-low ).

*    Retrieve CVPM run instance
    TRY.
        DATA(lr_run) = /ba1/cl_al_fw_run=>s_get_instance( i_run = lv_run_id ).
        DATA(lt_cvpm_param) = lr_run->get_parameter( ).

*        Read process parameter
        LOOP AT lt_cvpm_param-tab_par_value ASSIGNING FIELD-SYMBOL(<str_param>).
          INSERT VALUE z_c_abaplog_param_1(
            lognumber = lv_log_id log_handle = lv_handle extnumber = lv_run_id param_id = sy-tabix char_type = 'Process Parameters' )
            INTO TABLE rt_param ASSIGNING FIELD-SYMBOL(<str_out>).
          MOVE-CORRESPONDING <str_param> TO <str_out>.
          <str_out>-characteristic  = <str_param>-parameter.
          <str_out>-opt             = <str_param>-option.
        ENDLOOP.

*        Read technical parameter
        DATA(lv_indx) = lines( rt_param ).
        LOOP AT lt_cvpm_param-tab_tech_par_value ASSIGNING FIELD-SYMBOL(<str_tech_param>).
          INSERT VALUE z_c_abaplog_param_1(
            lognumber = lv_log_id log_handle = lv_handle extnumber = lv_run_id param_id = ( lv_indx + sy-tabix ) char_type = 'Technical Parameters' )
            INTO TABLE rt_param ASSIGNING <str_out>.
          <str_out>-characteristic  = <str_tech_param>-tech_param.
          <str_out>-low             = <str_tech_param>-tech_param_value.
        ENDLOOP.

*        Read free-selection parameter
        lv_indx = lines( rt_param ).
        LOOP AT lt_cvpm_param-free_selections-tab_char_value ASSIGNING FIELD-SYMBOL(<str_fs_param>).
          INSERT VALUE z_c_abaplog_param_1(
            lognumber = lv_log_id log_handle = lv_handle extnumber = lv_run_id param_id = ( lv_indx + sy-tabix ) char_type = 'Dynamic Selection Parameters' )
            INTO TABLE rt_param ASSIGNING <str_out>.
          MOVE-CORRESPONDING <str_fs_param> TO <str_out>.
        ENDLOOP.

*        Read free-selection key figures
        lv_indx = lines( rt_param ).
        LOOP AT lt_cvpm_param-free_selections-tab_unit_value ASSIGNING FIELD-SYMBOL(<str_fs_kf>).
          INSERT VALUE z_c_abaplog_param_1(
            lognumber = lv_log_id log_handle = lv_handle extnumber = lv_run_id param_id = ( lv_indx + sy-tabix ) char_type = 'Dynamic Selection Parameters' )
            INTO TABLE rt_param ASSIGNING <str_out>.
          MOVE-CORRESPONDING <str_fs_kf> TO <str_out>.
          <str_out>-characteristic = <str_fs_kf>-fn_unit.
        ENDLOOP.

*      Exception - run not found, most likely the log is not from a CVPM run.
      CATCH /ba1/cx_fw_not_found INTO DATA(lx_not_cvpm_run).
        INSERT VALUE z_c_abaplog_param_1(
          lognumber = lv_log_id log_handle = lv_handle extnumber = lv_run_id param_id = 1 char_type = 'N/A' )
          INTO TABLE rt_param.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_aggregate.

  ENDMETHOD.

ENDCLASS.
