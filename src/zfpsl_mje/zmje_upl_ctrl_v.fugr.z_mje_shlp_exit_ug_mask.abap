FUNCTION Z_MJE_SHLP_EXIT_UG_MASK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
  TYPES:
    BEGIN OF ts_ug,
      user_group TYPE domvalue_l,
      text       TYPE val_text,
    END OF ts_ug,
    tt_ug TYPE STANDARD TABLE OF ts_ug WITH DEFAULT KEY.

  DATA:
    lv_mask       TYPE string,
    lo_salvr      TYPE REF TO cl_salv_table,
    lo_mje_helper TYPE REF TO zcl_mje_util_helper,
    lt_dom_vals   TYPE tt_ug,
    lt_all_ug     TYPE dd07vtab,
    lt_dd07t      TYPE dd07vtab.

  lo_mje_helper = zcl_mje_util_helper=>get_instance( ).

  SELECT domvalue_l, ddtext
      FROM dd07t INTO TABLE @lt_dom_vals
      WHERE domname = 'Z_MJE_GRP_IND' AND ddlanguage = 'E' AND as4local = 'A'
      ORDER BY valpos ASCENDING.
  IF sy-subrc <> 0.

  ENDIF.

  LOOP AT lt_dom_vals ASSIGNING FIELD-SYMBOL(<str_dom_vals>).
    APPEND INITIAL LINE TO lt_all_ug ASSIGNING FIELD-SYMBOL(<str_all_ug>).
    <str_all_ug>-domvalue_l = <str_dom_vals>-user_group.
  ENDLOOP.

*  Initialize ALV
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_salvr
                               CHANGING t_table = lt_dom_vals ).

    CATCH cx_salv_msg INTO DATA(lx_error).
  ENDTRY.


*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normally only SHLP-SELOPT should be changed in this step.


*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard selection, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpful in this
* step.

  IF callcontrol-step = 'SELECT'.

    DATA(lv_curr_mask) = CONV int4( shlp-interface[ 1 ]-value ).
    DATA(lt_sel) = lo_salvr->get_selections( )->get_selected_rows( ).
    lo_mje_helper->decode_ug_mask(
        EXPORTING
            iv_mask = lv_curr_mask
            it_all_ug = lt_all_ug
        IMPORTING
            rv_error = DATA(lv_err)
            rt_dom_val = DATA(lt_curr_dom_vals) ).
    IF lv_err  = abap_false.
      CLEAR lt_sel.
      LOOP AT lt_dom_vals ASSIGNING <str_dom_vals>.
        DATA(lv_tabix) = sy-tabix.
        READ TABLE lt_curr_dom_vals WITH KEY domvalue_l = <str_dom_vals>-user_group TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.

          APPEND INITIAL LINE TO lt_sel ASSIGNING FIELD-SYMBOL(<str_sel>).
          <str_sel> = lv_tabix.
        ENDIF.
      ENDLOOP.
      lo_salvr->get_selections( )->set_selected_rows( lt_sel ).
    ELSE.
      MESSAGE w002(sy) WITH |Invalid user group mask value! Selection is reset.|.
    ENDIF.

    PERFORM display_popup USING lo_salvr callcontrol.

    CLEAR lt_dd07t.
    LOOP AT lo_salvr->get_selections( )->get_selected_rows( ) ASSIGNING <str_sel>.
      READ TABLE lt_dom_vals INDEX <str_sel> ASSIGNING <str_dom_vals>.
      APPEND INITIAL LINE TO lt_dd07t ASSIGNING FIELD-SYMBOL(<str_dd07t>).
      <str_dd07t>-domvalue_l = <str_dom_vals>-user_group.
    ENDLOOP.

    lv_mask = lo_mje_helper->encode_ug_mask( lt_dd07t ).
    APPEND INITIAL LINE TO record_tab ASSIGNING FIELD-SYMBOL(<str_record>).
    <str_record>-string = lv_mask.

    callcontrol-step = 'RETURN'.
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.

ENDFUNCTION.


FORM display_popup USING
                            io_salv     TYPE REF TO cl_salv_table
                            is_control  TYPE ddshf4ctrl.

  DATA:
    lv_offset TYPE int4,
    lv_val    TYPE c LENGTH 60,
    lv_length TYPE int4.

  DATA(lo_functions) = io_salv->get_functions( ).
  lo_functions->set_all( ).

  DATA(lo_columns) = io_salv->get_columns( ).
  lo_columns->set_optimize( abap_true ).

  DATA(lo_selection) = io_salv->get_selections( ).
  lo_selection->set_selection_mode( if_salv_c_selection_mode=>multiple ).

*   Set new title bar
  DATA(lo_display) = io_salv->get_display_settings( ).
  lo_display->set_striped_pattern( abap_false ).
  lo_display->set_list_header( |Choose User Group (Ctrl/Shift for multi-selection)| ).

  io_salv->set_screen_popup(
                start_column    =   is_control-cucol + 50
                end_column      =   is_control-cucol + 100
                start_line      =   is_control-curow + 5
                end_line        =   is_control-curow + 15 ).

  io_salv->refresh( ).
  io_salv->display( ).
ENDFORM.
