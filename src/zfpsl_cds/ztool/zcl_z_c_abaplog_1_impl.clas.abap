CLASS zcl_z_c_abaplog_1_impl DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
    CLASS-DATA:
      pv_hdl  TYPE balloghndl,          "Cached handle of the last ABAP log
      pt_msgs TYPE bal_t_msgr.          "Cached messages of the last ABAP log

    METHODS:
      get_log_keyset
        IMPORTING
                  io_request TYPE REF TO if_rap_query_request
        EXPORTING
                  ev_extnum  TYPE balnrext
                  ev_lognum  TYPE balognr
                  ev_handle  TYPE balloghndl
        RAISING   cx_rap_query_provider,

      get_filter_range
        IMPORTING
                  io_request       TYPE REF TO if_rap_query_request
        RETURNING
                  VALUE(rt_filter) TYPE if_rap_query_filter=>tt_name_range_pairs
        RAISING   cx_rap_query_provider,

      get_paging_index
        IMPORTING
          io_request TYPE REF TO if_rap_query_request
        EXPORTING
          ev_idx_fr  TYPE i
          ev_idx_to  TYPE i,
      handle_filter
        IMPORTING
                  io_request       TYPE REF TO if_rap_query_request
                  it_data          TYPE ANY TABLE
        EXPORTING
                  et_filtered_data TYPE ANY TABLE
        RAISING   cx_rap_query_provider,
      handle_sort
        IMPORTING
                  io_request     TYPE REF TO if_rap_query_request
                  it_data        TYPE ANY TABLE
        EXPORTING
                  et_sorted_data TYPE ANY TABLE
        RAISING   cx_rap_query_provider,

      handle_aggregate ABSTRACT
        IMPORTING
                  io_request         TYPE REF TO if_rap_query_request
                  it_data            TYPE ANY TABLE
        EXPORTING
                  et_aggregated_data TYPE ANY TABLE
        RAISING   cx_rap_query_provider,

      get_aggregate_statements
        IMPORTING
                  io_request  TYPE REF TO if_rap_query_request
        EXPORTING
                  ev_select   TYPE string
                  ev_group_by TYPE string
        RAISING   cx_rap_query_provider,

      determine_criticality
        IMPORTING
          iv_msgty              TYPE symsgty
        RETURNING
          VALUE(rv_criticality) TYPE symsgty.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_z_c_abaplog_1_impl IMPLEMENTATION.

  METHOD get_log_keyset.
    DATA(lr_filter) = io_request->get_filter( ).
    DATA(lt_rng_filter) = get_filter_range( io_request ).
    DATA(l_pair_extnum) = lt_rng_filter[ name = 'EXTNUMBER' ].
    DATA(l_rng_extnum) = l_pair_extnum-range[ 1 ].
    DATA(l_pair_lognum) = lt_rng_filter[ name = 'LOGNUMBER' ].
    DATA(l_rng_lognumb) = l_pair_lognum-range[ 1 ].
    DATA(l_pair_hdl) = lt_rng_filter[ name = 'LOG_HANDLE' ].
    DATA(l_rng_handle) = l_pair_hdl-range[ 1 ].
    ev_extnum = CONV balnrext( l_rng_extnum-low ).
    ev_lognum = CONV balognr( l_rng_lognumb-low ).
    ev_handle = CONV balloghndl( l_rng_handle-low ).
  ENDMETHOD.

  METHOD get_filter_range.
    TRY.
        rt_filter = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_range).
        RAISE EXCEPTION TYPE zcx_rap_query_provider
          EXPORTING
            previous = lx_no_range.
    ENDTRY.
  ENDMETHOD.

  METHOD get_paging_index.

*    Get Paging Info
    DATA(lr_paging) = io_request->get_paging( ).
    ev_idx_fr = lr_paging->get_offset( ) + 1.
    DATA(lv_page_size) = lr_paging->get_page_size( ).
    ev_idx_to = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                            THEN 0
                            ELSE lv_page_size - 1 ).
  ENDMETHOD.

  METHOD get_aggregate_statements.
    DATA(lt_req_flds) = io_request->get_requested_elements( ).

    DATA(lt_aggr_flds) = io_request->get_aggregation( )->get_aggregated_elements( ).
    IF lt_aggr_flds IS NOT INITIAL.
      LOOP AT lt_aggr_flds ASSIGNING FIELD-SYMBOL(<fs_aggr_element>).
        DELETE lt_req_flds WHERE table_line = <fs_aggr_element>-result_element.
        DATA(lv_aggr) = |{ <fs_aggr_element>-aggregation_method }( { <fs_aggr_element>-input_element } ) as { <fs_aggr_element>-result_element }|.
        APPEND lv_aggr TO lt_req_flds.
      ENDLOOP.
    ENDIF.

    DATA(lt_grouped_flds) = io_request->get_aggregation( )->get_grouped_elements( ).

    ev_select  = concat_lines_of( table = lt_req_flds sep = `, ` ).
    ev_group_by = concat_lines_of(  table = lt_grouped_flds sep = `, ` ).
  ENDMETHOD.

  METHOD handle_filter.
    DATA(l_rng_filter) = get_filter_range( io_request ).

    CLEAR et_filtered_data.
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<str_data>).
      DATA(lv_filtered) = abap_false.
      LOOP AT l_rng_filter ASSIGNING FIELD-SYMBOL(<str_filter>).
*        Get field name of the current filter
        ASSIGN COMPONENT <str_filter>-name OF STRUCTURE <str_data> TO FIELD-SYMBOL(<val_field>).
*        If field doesn't exist in current structure, skip (possible as filters for parent entities are passed in too)
        IF sy-subrc <> 0 .
          CONTINUE.
        ENDIF.

        IF <val_field> NOT IN <str_filter>-range.
          lv_filtered = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

*      If no filters fail, write to output table
      IF lv_filtered = abap_false.
        INSERT <str_data> INTO TABLE et_filtered_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_sort.
    et_sorted_data = it_data.

    DATA(lt_sort_elements) = io_request->get_sort_elements( ).
    IF lt_sort_elements IS NOT INITIAL.

      DATA(lt_sort_tab) = VALUE abap_sortorder_tab(
          FOR i = 1 UNTIL i > lines( lt_sort_elements )
          ( name = lt_sort_elements[ i ]-element_name descending = lt_sort_elements[ i ]-descending ) ).
      SORT et_sorted_data BY (lt_sort_tab).
    ENDIF.
  ENDMETHOD.


  METHOD determine_criticality.

    rv_criticality  = COND symsgty(
          WHEN    iv_msgty CA   'EAX'   THEN 1
          WHEN    iv_msgty =    'W'     THEN 2
          WHEN    iv_msgty CA   'IS'    THEN 3
          ELSE                                0 ).

  ENDMETHOD.

ENDCLASS.
