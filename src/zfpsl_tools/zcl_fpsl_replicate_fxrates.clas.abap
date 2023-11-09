CLASS zcl_fpsl_replicate_fxrates DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS /ba1/if_al_fw_enrich_data
                 /ba1/if_al_fw_enrich_param .

  PUBLIC SECTION.

    INTERFACES /ba1/if_al_fw_enrich_data .
    INTERFACES /ba1/if_al_fw_enrich_param .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_tcurr,
      /ba1/c11objcur TYPE /ba1/bf_dte_pos_curr,
      /ba1/c55curpos TYPE /ba1/bf_dte_pos_curr,
      /ba1/c55yctype TYPE /ba1/f4_dte_yctype,
      /ba1/crpstdat  TYPE /ba1/fj_dte_start_date,
      /ba1/krccfvrf  TYPE /ba1/f4_dte_fx_rate_dec,
    END OF ty_tcurr .

  class-data S_REF_ENRICH type ref to ZCL_FPSL_REPLICATE_FXRATES .
  data MR_MSG_HANDLER type ref to /BA1/IF_AL_X0_MSG_HANDLER .
  data MV_IS_TESTRUN type BOOLEAN .
  constants GC_QUOT_DIRECT type /BA1/F4_DTE_FX_NOTATION value '1' ##NO_TEXT.
  constants GC_QUOT_INDIRECT type /BA1/F4_DTE_FX_NOTATION value '2' ##NO_TEXT.

  methods CREATE_FXRATE
    importing
      !IS_FXDATA type /BA1/BAPIF4_STR_FX_MAPIPA .
  methods GET_FACTORS
    importing
      !IV_RATETYPE type /BA1/BAPIF4_STR_FX_MAPIPA-RATETYPE
      !IV_FROMCCY type /BA1/BAPIF4_STR_FX_MAPIPA-FROMCCY
      !IV_TOCCY type /BA1/BAPIF4_STR_FX_MAPIPA-TOCCY
      !IV_VALIDDATE type DATS
    exporting
      !EV_FFACT type FFACT_CURR
      !EV_TFACT type TFACT_CURR .
  methods READ_PARAM
    importing
      value(I_STR_PARAM) type /BA1/IF_AL_FW_RUN=>T_STR_PARAM optional
      value(I_PARAM_NAME) type /BA1/F0_DTE_CHARACTERISTIC
    exporting
      value(E_PARAM_VALUE) type TVARV_LOW .
  methods MAP_FXRATE
    importing
      !IV_MDCODE type /BA1/F4_DTE_MDCODE
      !IV_RATETYPE type /BA1/F4_DTE_FX_RATE_TYPE
      !IS_TCURR type TY_TCURR
    returning
      value(RS_FXDATA) type /BA1/BAPIF4_STR_FX_MAPIPA .
  methods MODIFY_FXRATE
    importing
      !IS_FXDATA type /BA1/BAPIF4_STR_FX_MAPIPA .
ENDCLASS.



CLASS ZCL_FPSL_REPLICATE_FXRATES IMPLEMENTATION.


  METHOD /ba1/if_al_fw_enrich_data~execute.

    DATA ls_container    TYPE /ba1/fw_str_container.
    DATA lv_dummy_msg    TYPE bapi_msg.

    DATA lv_update       TYPE c.
    DATA lv_mdcode       TYPE /ba1/f4_dte_mdcode.
    DATA lv_ratetype     TYPE /ba1/f4_dte_fx_rate_type.

    FIELD-SYMBOLS:
      <step_data>       TYPE /ba1/fw_str_container,
      <tab_container>   TYPE /ba1/fw_tab_container,
      <tab_faulty_data> TYPE /ba1/fw_tab_container.

*/  init globals and get parameters
    mr_msg_handler = i_ref_msg_handler.
    mv_is_testrun  = i_ref_process->get_parameter_values( )-is_test_run.

    lv_update   = i_str_param-tab_par_value[ parameter = '/BIC/ZUPDATE' ]-low.
    lv_mdcode   = i_str_param-tab_par_value[ parameter = '/BIC/ZMDCODE' ]-low.
    lv_ratetype = i_str_param-tab_par_value[ parameter = '/BA1/C55YCTYPE' ]-low.

    TRY.

        e_rda_enriched_data = NEW /ba1/fw_tab_container( ).

        ASSIGN e_rda_enriched_data->* TO <tab_container>.
        ASSIGN e_tab_faulty_data      TO <tab_faulty_data>.

        BREAK-POINT.
        LOOP AT i_tab_step_data ASSIGNING <step_data>.
          ASSIGN <step_data>-data->* TO FIELD-SYMBOL(<tcurr_wl>).
          DATA(ls_fxdata) = map_fxrate( iv_mdcode   = lv_mdcode
                                        iv_ratetype = lv_ratetype
                                        is_tcurr = CORRESPONDING #( <tcurr_wl> ) ).

          IF lv_update EQ abap_false.
            create_fxrate( ls_fxdata ).
          ELSE.
            modify_fxrate( ls_fxdata ).
          ENDIF.

          ls_container-id   = ls_container-id + 1.
          ls_container-data = NEW /ba1/bapif4_str_fx_mapipa( ls_fxdata ).
          APPEND ls_container TO <tab_container>.

        ENDLOOP.


      CATCH /ba1/cx_hx_static_check INTO DATA(l_cx).
        i_ref_msg_handler->add_exception( l_cx ).

    ENDTRY.


  ENDMETHOD.


  METHOD /ba1/if_al_fw_enrich_data~s_create.

    IF s_ref_enrich IS NOT BOUND.
      s_ref_enrich = NEW #( ).
    ENDIF.

    r_ref_instance  = s_ref_enrich.

  ENDMETHOD.


  METHOD /ba1/if_al_fw_enrich_param~execute.


    BREAK-POINT.

    IF c_str_param-tab_par_value[ parameter = '/BIC/ZINIT' ]-low EQ abap_true. " initialization
      IF VALUE #( c_str_param-tab_par_value[ parameter = '/BA1/CRPSTDAT' ]-low OPTIONAL ) IS INITIAL." get date

        DATA(lv_exchangeratetype) = c_str_param-tab_par_value[ parameter = '/BA1/C55YCTYPE' ]-low.

        SELECT MIN( validitystartdate ) INTO @DATA(lv_startdate)
              FROM i_exchangeraterawdata
             WHERE exchangeratetype EQ @lv_exchangeratetype.

        IF sy-subrc NE 0.
*          MESSAGE e.... todo
          e_flg_stop = abap_true.
          RETURN.
        ENDIF.

        DELETE c_str_param-tab_par_value WHERE parameter = '/BA1/CRPSTDAT'.
        APPEND VALUE #( parameter = '/BA1/CRPSTDAT' sign = 'I' option = 'GE' low = lv_startdate ) TO c_str_param-tab_par_value.
      ENDIF.

    ELSE. " not initialization
      IF VALUE #( c_str_param-tab_par_value[ parameter = '/BA1/CRPSTDAT' ]-low OPTIONAL ) IS INITIAL." get date
        DATA(lv_mdcode) = c_str_param-tab_par_value[ parameter = '/BIC/ZMDCODE' ]-low.

        SELECT MAX( valid_date ) INTO @DATA(lv_valid_date)
              FROM /ba1/f4_fxrates
             WHERE mdcode = @lv_mdcode.

*       todo should we start begin of year if there is nothing?

        DELETE c_str_param-tab_par_value WHERE parameter = '/BA1/CRPSTDAT'.
        APPEND VALUE #( parameter = '/BA1/CRPSTDAT' sign = 'I' option = 'GT' low = lv_valid_date(8) ) TO c_str_param-tab_par_value.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD /ba1/if_al_fw_enrich_param~s_create.

    IF s_ref_enrich IS NOT BOUND.
      s_ref_enrich = NEW #( ).
    ENDIF.

    r_ref_instance  = s_ref_enrich.

  ENDMETHOD.


  METHOD create_fxrate.

    DATA lt_return TYPE bapiret2_t.

    CALL FUNCTION '/BA1/BAPI_F4_FX_CREATE'
      EXPORTING
        mdcode     = is_fxdata-mdcode
        ratetype   = is_fxdata-ratetype
        fromfactor = is_fxdata-fromfactor
        fromccy    = is_fxdata-fromccy
        tofactor   = is_fxdata-tofactor
        toccy      = is_fxdata-toccy
        convtype   = is_fxdata-convtype
        notation   = is_fxdata-notation
        validdate  = is_fxdata-validdate
        fxrate     = is_fxdata-fxrate
        testrun    = is_fxdata-testrun
      TABLES
        return     = lt_return.

    mr_msg_handler->add_bapirettab( lt_return ).

  ENDMETHOD.


  METHOD get_factors.

    CALL FUNCTION '/BA1/F4_DB_FX_TCURR_FACTORS'
      EXPORTING
        i_kurst         = iv_ratetype
        i_fcurr         = iv_fromccy
        i_tcurr         = iv_toccy
        i_valid_date    = iv_validdate
      IMPORTING
        e_ffact         = ev_ffact
        e_tfact         = ev_tfact
      EXCEPTIONS
        function_failed = 1
        OTHERS          = 2.

    IF sy-subrc NE 0.
      mr_msg_handler->add_sy_msg( ).
    ENDIF.

  ENDMETHOD.


  METHOD map_fxrate.

    CLEAR rs_fxdata.

    rs_fxdata-mdcode     = iv_mdcode.
    rs_fxdata-ratetype   = iv_ratetype.
    rs_fxdata-fromccy    = is_tcurr-/ba1/c11objcur.
    rs_fxdata-toccy      = is_tcurr-/ba1/c55curpos.
    rs_fxdata-convtype   = 0."is_tcurr-/ba1/c55yctype. Zero is middle rate, to hardcode this, or to put to selection screen???
    rs_fxdata-validdate  = is_tcurr-/ba1/crpstdat.
    rs_fxdata-fxrate     = is_tcurr-/ba1/krccfvrf.
    rs_fxdata-testrun    = mv_is_testrun.


    get_factors( EXPORTING iv_ratetype  = rs_fxdata-ratetype
                           iv_fromccy   = rs_fxdata-fromccy
                           iv_toccy     = rs_fxdata-toccy
                           iv_validdate = rs_fxdata-validdate
                 IMPORTING
                           ev_ffact     = rs_fxdata-fromfactor
                           ev_tfact     = rs_fxdata-tofactor ).

    IF is_tcurr-/ba1/krccfvrf GE 0.
      rs_fxdata-notation = gc_quot_direct.
    ELSE.
      rs_fxdata-notation = gc_quot_indirect.
    ENDIF.

  ENDMETHOD.


  METHOD modify_fxrate.

    DATA lt_return TYPE bapiret2_t.

    CALL FUNCTION '/BA1/BAPI_F4_FX_MODIFY'
      EXPORTING
        mdcode     = is_fxdata-mdcode
        ratetype   = is_fxdata-ratetype
        fromfactor = is_fxdata-fromfactor
        fromccy    = is_fxdata-fromccy
        tofactor   = is_fxdata-tofactor
        toccy      = is_fxdata-toccy
        convtype   = is_fxdata-convtype
        notation   = is_fxdata-notation
        validdate  = is_fxdata-validdate
        fxrate     = is_fxdata-fxrate
        testrun    = is_fxdata-testrun
      TABLES
        return     = lt_return.

    mr_msg_handler->add_bapirettab( lt_return ).

  ENDMETHOD.


  METHOD read_param.
    DATA: l_par_value TYPE /ba1/fw_str_par_value.

    READ TABLE i_str_param-tab_par_value INTO l_par_value WITH KEY parameter = i_param_name.

    e_param_value = l_par_value-low.

  ENDMETHOD.
ENDCLASS.
