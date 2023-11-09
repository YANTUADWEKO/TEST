*----------------------------------------------------------------------*
***INCLUDE LZMJE_UPL_CTRL_VF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ZMJE_UPL_CTRL_UG_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zmje_upl_ctrl_ug_check .
  DATA:
    lv_error      TYPE boolean,
    ls_mje_ctrl   TYPE zmje_upl_ctrl,
    lt_dom_vals   TYPE dd07vtab,
    lo_mje_helper TYPE REF TO zcl_mje_util_helper.

  lv_error = abap_true.

  lo_mje_helper = zcl_mje_util_helper=>get_instance( ).

  IF lt_dom_vals IS INITIAL.
    SELECT domvalue_l, ddtext
      FROM dd07t INTO CORRESPONDING FIELDS OF TABLE @lt_dom_vals
      WHERE domname = 'Z_MJE_GRP_IND' AND ddlanguage = 'E' AND as4local = 'A'
      ORDER BY valpos ASCENDING.
    IF sy-subrc <> 0.

    ENDIF.
  ENDIF.

  IF fcode = 'EDIT'.
    MOVE-CORRESPONDING zmje_upl_ctrl TO ls_mje_ctrl.

    DATA(lv_curr_mask) = ls_mje_ctrl-mandatory_mask.
    lo_mje_helper->decode_ug_mask(
        EXPORTING
            iv_mask = lv_curr_mask
            it_all_ug = lt_dom_vals
        IMPORTING
            rv_error = DATA(lv_err)
            rt_dom_val = DATA(lt_curr_dom_vals) ).
    IF lv_err  = abap_true.
      MESSAGE s002(sy) WITH |{ ls_mje_ctrl-iobjnm }: Invalid user group mask.| DISPLAY LIKE 'E'.
      lv_error = abap_false.
    ENDIF.

  ELSEIF fcode = 'SAVE'.
    LOOP AT total ASSIGNING FIELD-SYMBOL(<str_total>).
      MOVE-CORRESPONDING <str_total> TO ls_mje_ctrl.

      lv_curr_mask = ls_mje_ctrl-mandatory_mask.
    lo_mje_helper->decode_ug_mask(
        EXPORTING
            iv_mask = lv_curr_mask
            it_all_ug = lt_dom_vals
        IMPORTING
            rv_error = lv_err
            rt_dom_val = lt_curr_dom_vals ).
    IF lv_err  = abap_true.
        MESSAGE s002(sy) WITH |{ ls_mje_ctrl-iobjnm }: Invalid user group mask.| DISPLAY LIKE 'E'.
        lv_error = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lv_error = abap_false.

    vim_abort_saving = 'X'.
    EXIT.
  ENDIF.

ENDFORM.
