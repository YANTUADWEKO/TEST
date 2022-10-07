class ZCL_MJE_UTIL_HELPER definition
  public
  final
  create public .

public section.

  types:
    tt_br_adj_head TYPE STANDARD TABLE OF /ba1/br_str_adj_head WITH DEFAULT KEY .

  constants GV_MJE_EMAIL_TITLE type STRING value 'Manual Posting - Journal Entry Release' ##NO_TEXT.
  constants GV_APP_ID_SBWP type STRING value 'Workflow-display' ##NO_TEXT.
  constants GV_APP_ID_MASS_REL type STRING value 'Z_MJE_RELEASE_UI-create' ##NO_TEXT.
  constants GV_APP_ID_ATT_VIEW type STRING value 'Z_MJE_VIEWER-create' ##NO_TEXT.
  constants GV_EMAIL_SNDR_ID type STRING value 'SAP_WFRT' ##NO_TEXT.
  constants G_ACCRT_PSLPD type /BA1/BR_DTE_RDL_RES value '905' ##NO_TEXT.
  constants G_DYN_METH_MD_PREF type STRING value 'CHECK_MD_' ##NO_TEXT.
  constants G_DYN_METH_MD_EXP_P1 type STRING value 'IV_VALUE' ##NO_TEXT.
  constants G_DYN_METH_MD_EXP_P2 type STRING value 'IV_MSG_DOC_ID' ##NO_TEXT.
  constants G_DYN_METH_MD_EXP_P3 type STRING value 'IV_LOGHANDLE' ##NO_TEXT.
  constants G_DYN_METH_MD_RET_PAR type STRING value 'RV_PASSED' ##NO_TEXT.
  constants G_DYN_METH_AH_PREF type STRING value 'CHECK_ADHOC_' ##NO_TEXT.
  constants G_DYN_METH_AH_EXP_P1 type STRING value 'IS_MJE_CTRL' ##NO_TEXT.
  constants G_DYN_METH_AH_EXP_P2 type STRING value 'IS_MJE_DOC' ##NO_TEXT.
  constants G_DYN_METH_AH_EXP_P3 type STRING value 'IV_MSG_DOC_ID' ##NO_TEXT.
  constants G_DYN_METH_AH_EXP_P4 type STRING value 'IV_LOGHANDLE' ##NO_TEXT.
  constants G_DYN_METH_AH_RET_PAR type STRING value 'RV_PASSED' ##NO_TEXT.
  constants G_DOC_OFFSET type BOOLEAN value ABAP_FALSE ##NO_TEXT.
  constants G_OFFSET_SL_ACC type /BA1/BF_DTE_SL_ACCOUNT value 'Z90200101' ##NO_TEXT.
  constants G_OFFSET_GL_ACC type /BA1/BF_DTE_GL_ACCOUNT value '90930005' ##NO_TEXT.

  class-methods GET_FIORI_LINK
    importing
      !IV_APP_ID type STRING
    returning
      value(RV_LINK) type STRING
    exceptions
      CX_ROOT .
  class-methods SEND_EMAIL_NOTIF
    importing
      !IV_INITIATOR type HRP1001-SOBID
      !IV_EMAIL_TITLE type STRING
      !IV_EMAIL_LINK type STRING
      !IV_BT_ID type /BA1/F2_DTE_BT_ID
      !IT_OU_RECIPIENTS type HRP1001_T
      !IV_REL_STATUS type /BA1/BR_DTE_ADJ_REL_STATUS
    raising
      CX_SWF_CNT_ELEM_NOT_FOUND
      CX_SWF_CNT_ELEM_TYPE_CONFLICT
      CX_SWF_CNT_UNIT_TYPE_CONFLICT
      CX_SWF_CNT_CONTAINER
      CX_STATIC_CHECK
      CX_DYNAMIC_CHECK .
  class-methods GET_HTML_ANCHOR_STRING
    importing
      !IV_LINK type STRING
      !IV_TEXT type STRING
    returning
      value(RV_HTML_STRING) type STRING .
  class-methods GET_INSTANCE
    returning
      value(R_RESULT) type ref to ZCL_MJE_UTIL_HELPER .
  methods GET_LOGHANDLE
    importing
      !IV_LOG_OBJ type BALOBJ_D optional
      !IV_LOG_SOBJ type BALSUBOBJ optional
    returning
      value(RV_LOGHANDLE) type BALLOGHNDL .
  methods MSG_ADD
    importing
      !IV_TYPE type SYMSGTY optional
      !IV_LOGHANDLE type BALLOGHNDL .
  methods MSG_SAVE
    importing
      !IV_EXIT type BOOLEAN optional
      !IV_LOGHANDLE type BALLOGHNDL .
  methods MSG_DISPLAY
    importing
      !IV_LOGHANDLE type BALLOGHNDL .
  methods EXCEPTION_ADD
    importing
      !I_REF_EX type ref to CX_ROOT .
  methods BAPIRETTAB_ADD
    importing
      !IT_TAB_MESSAGE type BAPIRETTAB .
  methods GET_OU_APPROVERS
    importing
      !IV_REQUESTOR type HRP1001-SOBID
    returning
      value(RT_APPROVERS) type HRP1001_T .
  methods GET_OU_REQUESTORS
    importing
      !IV_APPROVER type HRP1001-SOBID
    returning
      value(RT_REQUESTORS) type HRP1001_T .
  methods DECODE_UG_MASK
    importing
      !IV_MASK type INT4
      !IT_ALL_UG type DD07VTAB
    exporting
      !RV_ERROR type BOOLEAN
      !RT_DOM_VAL type DD07VTAB .
  methods ENCODE_UG_MASK
    importing
      !IT_DOM_VAL type DD07VTAB
    returning
      value(RV_MASK) type INT4 .
  methods GET_MJE_UPLOAD_CONTROL
    returning
      value(RT_MJE_CTRL) type ZTT_MJE_UPL_CTRL .
  methods VALIDATE_DATA_TYPE
    importing
      !IV_HEADER type STRING
      !IV_FIELD type ANY
      !IV_VALUE type ANY
    returning
      value(RV_PASSED) type BOOLEAN .
  methods CHECK_MANDATORY_FIELD
    importing
      !IV_UG type Z_DTE_MJE_GRP_IND
      !IT_MJE_CTRL type ZTT_MJE_UPL_CTRL
      !IS_MJE_DOC type /BA1/HFPPD
      !IV_MSG_DOC_ID type STRING
      !IV_LOGHANDLE type BALLOGHNDL
    returning
      value(RV_PASSED) type BOOLEAN .
  methods CHECK_MD
    importing
      !IT_MJE_CTRL type ZTT_MJE_UPL_CTRL
      !IS_MJE_DOC type /BA1/HFPPD
      !IV_MSG_DOC_ID type STRING
      !IV_LOGHANDLE type BALLOGHNDL
    returning
      value(RV_PASSED) type BOOLEAN .
  methods CHECK_ADHOC
    importing
      !IT_MJE_CTRL type ZTT_MJE_UPL_CTRL
      !IS_MJE_DOC type /BA1/HFPPD
      !IV_MSG_DOC_ID type STRING
      !IV_LOGHANDLE type BALLOGHNDL
    returning
      value(RV_PASSED) type BOOLEAN .
  methods ENRICH_TECHNICAL_FIELDS
    importing
      !IV_OFFSET type BOOLEAN
      !IV_TS type TIMESTAMPL
    changing
      !IS_MJE_DOC type /BA1/HFPPD .
  methods ENRICH_FPSL_GL_DERIVATION
    changing
      !IS_MJE_DOC type /BA1/HFPPD
    returning
      value(RV_PASSED) type BOOLEAN .
  methods ENRICH_FPSL_MNT
    changing
      !IT_MJE_DOC type ZTT_MJE_POST_DOCS
      !IV_LOGHANDLE type BALLOGHNDL
    returning
      value(RV_PASSED) type BOOLEAN .
  methods OFFSET_DOCUMENT
    changing
      !IT_MJE_DOC type ZTT_MJE_POST_DOCS .
  methods CHECK_JE_AMOUNT
    importing
      !IT_MJE_DOC type ZTT_MJE_POST_DOCS
      !IV_LOGHANDLE type BALLOGHNDL
    returning
      value(RV_PASSED) type BOOLEAN .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: s_instance TYPE REF TO zcl_mje_util_helper.

    DATA:
      pv_rdl_area       TYPE /ba1/hm_dte_area,
      pv_rdl_type       TYPE /ba1/hm_dte_type,
      pv_rdl_data_tab   TYPE /ba1/hm_dte_data_table,

      pt_tcur           TYPE STANDARD TABLE OF tcurc,
      pt_lgent_acc_sys  TYPE STANDARD TABLE OF /ba1/tbr_lgenta,
      pt_acc_sys        TYPE STANDARD TABLE OF /ba1/tbr_accsys,
      pt_comp_code      TYPE STANDARD TABLE OF t001,
      pt_md_cost_cntr   TYPE STANDARD TABLE OF csks,
      pt_md_contid      TYPE STANDARD TABLE OF /ba1/f1_con_flat-ext_contract,
      pt_md_lgent       TYPE STANDARD TABLE OF /ba1/tbr_lgentv,
      pt_md_sl_acc      TYPE STANDARD TABLE OF /ba1/tbr_sla,
      pt_md_prof_cntr   TYPE STANDARD TABLE OF cepc,
      pt_md_chrt_acc    TYPE STANDARD TABLE OF ska1,
      pt_md_gl_acc      TYPE STANDARD TABLE OF ska1,
      pt_post_per       TYPE STANDARD TABLE OF /ba1/tbr_pper,

      pt_hrp1001        TYPE hrp1001_t,
      pt_mje_ctrl       TYPE ztt_mje_upl_ctrl,
      pt_mje_fld_elem   TYPE /ibs/tyrb_dd03l,

      ps_supp_rdl_types TYPE /ba1/if_in_hx_helper=>ty_str_supported_result_types,
      po_br_rules       TYPE REF TO /ba1/cl_al_br_rules,

      s_loghandle       TYPE balloghndl.

    METHODS:
      get_derivation_helper
        RETURNING VALUE(ro_br_rules) TYPE REF TO /ba1/cl_al_br_rules,

      convert_post_doc_ext_2_int
        IMPORTING
                  it_doc_ext        TYPE tt_br_adj_head
        RETURNING VALUE(rt_mje_doc) TYPE ztt_mje_post_docs,

      convert_post_doc_int_2_ext
        IMPORTING
                  it_mje_doc        TYPE ztt_mje_post_docs
        RETURNING VALUE(rt_doc_ext) TYPE tt_br_adj_head,

      init_mnt_proc_param
        RETURNING VALUE(rs_proc_param) TYPE /ba1/if_al_fw_step_control=>t_str_param,

      get_mje_fld_data_elem
        RETURNING VALUE(rt_mje_fld_elem) TYPE /ibs/tyrb_dd03l ,

      get_cust_iobj_md_tab
        IMPORTING iv_iobj            TYPE rsdiobjnm
        RETURNING VALUE(rv_tab_name) TYPE tabname,

      get_mje_iobj_field
        IMPORTING is_mje_ctrl     TYPE zts_mje_upl_ctrl
        RETURNING VALUE(rv_field) TYPE /ba1/f0_dte_iobj_prim ,

      check_md_currency
        IMPORTING
                  iv_curr          TYPE waers_curc
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean,

      check_md_fpsl_iobj
        IMPORTING
                  iv_iobj          TYPE rsdiobjnm
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean,

      check_md_custom_iobj
        IMPORTING
                  is_mje_ctrl      TYPE zts_mje_upl_ctrl
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean,

      check_md_/ba1/c55accsy
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/c55bukrs
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/c55cocntr
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/c55contid
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/c55dbcdf
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/c55lgent
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/c55slacc
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/c80prfctr
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/ichrt_acc
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_md_/ba1/igl_acct
        IMPORTING
                  iv_value         TYPE any
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_posting_period
        IMPORTING
                  is_mje_ctrl      TYPE zts_mje_upl_ctrl
                  is_mje_doc       TYPE /ba1/hfppd
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_adhoc_/ba1/c55accsy
        IMPORTING
                  is_mje_ctrl      TYPE zts_mje_upl_ctrl
                  is_mje_doc       TYPE /ba1/hfppd
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_adhoc_/ba1/c55dbcdf
        IMPORTING
                  is_mje_ctrl      TYPE zts_mje_upl_ctrl
                  is_mje_doc       TYPE /ba1/hfppd
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_adhoc_/ba1/c55pdres
        IMPORTING
                  is_mje_ctrl      TYPE zts_mje_upl_ctrl
                  is_mje_doc       TYPE /ba1/hfppd
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean ,

      check_adhoc_/ba1/c55postd
        IMPORTING
                  is_mje_ctrl      TYPE zts_mje_upl_ctrl
                  is_mje_doc       TYPE /ba1/hfppd
                  iv_msg_doc_id    TYPE string
                  iv_loghandle     TYPE balloghndl
        RETURNING VALUE(rv_passed) TYPE boolean .

ENDCLASS.



CLASS ZCL_MJE_UTIL_HELPER IMPLEMENTATION.


  METHOD validate_data_type.
    DESCRIBE FIELD iv_field TYPE DATA(lv_type).

    rv_passed = abap_true.
    CASE lv_type.
      WHEN 'C'. "Character type
*        EXIT.   "Automatic pass - No way to validate against character type.
      WHEN 'D'. "Date type
        DATA lv_date TYPE d.
        lv_date = iv_value.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = lv_date
          EXCEPTIONS
            plausibility_check_failed = 1.  "Non date values are caught by exception
        IF iv_value IS NOT INITIAL AND sy-subrc <> 0.
          MESSAGE s551(zfpsl_mje) WITH iv_header  iv_value.
          msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
          rv_passed = abap_false.
        ENDIF.
      WHEN 'N'. "Numeric text type
        IF iv_value CN '0123456789'.
          MESSAGE s552(zfpsl_mje) WITH iv_header iv_value.
          msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
          rv_passed = abap_false.
        ENDIF.
      WHEN 'P' OR 'i' OR 'f'.   "Numeric type
        TRY.
            DATA            lr_field    TYPE REF TO data.
            FIELD-SYMBOLS   <val_field> TYPE any.

            CREATE DATA lr_field TYPE (lv_type).
            ASSIGN lr_field->* TO <val_field>.
            <val_field> = iv_value.
          CATCH cx_sy_conversion_error INTO DATA(lx_conv_err).
            MESSAGE s553(zfpsl_mje) WITH iv_header iv_value.
            msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
            rv_passed = abap_false.
        ENDTRY.
      WHEN OTHERS.
        EXIT.   "Passed - Other types not checked.
    ENDCASE.
  ENDMETHOD.


  METHOD check_md_currency.
    rv_passed = abap_true.
*    Populate currency key table if empty
    IF pt_tcur IS INITIAL.
      SELECT * FROM tcurc INTO CORRESPONDING FIELDS OF TABLE @pt_tcur.
    ENDIF.

    READ TABLE pt_tcur WITH KEY waers = iv_curr TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s615(zfpsl_mje) WITH iv_msg_doc_id iv_curr.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD decode_ug_mask.
    DATA :
      lv_hex  TYPE x LENGTH 8,
      lv_hex1 TYPE x LENGTH 8,
      lv_hex2 TYPE x LENGTH 8 VALUE 0.

    rv_error = abap_false.

    IF iv_mask < 0.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    DATA(lt_all_ug) = it_all_ug.
    SORT lt_all_ug BY domvalue_l DESCENDING.
    DATA(lv_max_ug) = CONV i( lt_all_ug[ 1 ]-domvalue_l ).

    DO.
      DATA(lv_val) = 2 ** ( sy-index - 1 ).
      IF lv_val > iv_mask.
        EXIT.
      ENDIF.

      lv_hex = iv_mask.
      lv_hex1 = lv_val.
      lv_hex = lv_hex BIT-AND lv_hex1.

      IF lv_hex <> 0.
        IF sy-index > lv_max_ug.
          rv_error = abap_true.
          CLEAR rt_dom_val.
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO rt_dom_val ASSIGNING FIELD-SYMBOL(<str_dom_val>).

        <str_dom_val>-domvalue_l = CONV z_dte_mje_grp_ind( sy-index ).
        lv_hex2 = lv_hex2 BIT-OR lv_hex1.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD get_fiori_link.
    TRY.

        cl_http_server=>if_http_server~get_location(
            EXPORTING
              application  = /ui2/if_start_url=>co_flp
            IMPORTING
              host         = DATA(lv_hwd)
              port         = DATA(lv_port)
              out_protocol = DATA(lv_protocol) ).

        rv_link = |{ lv_protocol }://{ lv_hwd }:{ lv_port }{ /ui2/if_start_url=>co_flp }| &&
                  |?sap-client={ sy-mandt }#{ iv_app_id }| .
      CATCH cx_root INTO DATA(lx_error).
        MESSAGE e406(zfpsl_mje) RAISING cx_root .
    ENDTRY.
  ENDMETHOD.


  METHOD get_html_anchor_string.
    rv_html_string = |<A href="{ iv_link }">{ iv_text }</A>|.
  ENDMETHOD.


  METHOD get_ou_approvers.
    DATA:
      ls_appr_pos LIKE LINE OF pt_hrp1001.

    IF pt_hrp1001 IS INITIAL.

      SELECT * FROM hrp1001
      INTO TABLE @pt_hrp1001      WHERE
              begda <= @sy-datlo  AND
              endda >= @sy-datlo.
      IF sy-subrc <> 0.
        MESSAGE e401(zfpsl_mje).
        RETURN.
      ENDIF.
    ENDIF.

    DATA(lv_requestor) = iv_requestor.

    LOOP AT pt_hrp1001 ASSIGNING FIELD-SYMBOL(<str_hrp1001>)
*        Find requester's org structure position - ONLY 1 GROUP SHALL RETURN!
        WHERE   sobid = lv_requestor
        AND     relat = '008'.

*      Find requester's position's org unit superior position
      READ TABLE pt_hrp1001 WITH KEY objid = <str_hrp1001>-objid relat = '002' rsign = 'A' INTO ls_appr_pos.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF ls_appr_pos IS INITIAL.
      MESSAGE e402(zfpsl_mje) WITH lv_requestor.
      RETURN.
    ENDIF.

*    Return all users under this position
    LOOP AT pt_hrp1001 ASSIGNING <str_hrp1001>
        WHERE   objid = ls_appr_pos-sobid
        AND     relat = '008'
        AND     rsign = 'A'.

      APPEND <str_hrp1001> TO rt_approvers.
    ENDLOOP.
  ENDMETHOD.


  METHOD exception_add.
    DATA:
      l_tab_message TYPE /ba1/xx_tty_bal_msg,
      l_s_msg       TYPE bal_s_msg.

    l_tab_message = /ba1/cl_al_x0_msg_helper=>s_extract_messages( i_ref_ex ).
    LOOP AT l_tab_message ASSIGNING FIELD-SYMBOL(<str_tab_msg>).

* define data of message for Application Log
      l_s_msg-msgty     = <str_tab_msg>-msgty.
      l_s_msg-msgid     = <str_tab_msg>-msgid.
      l_s_msg-msgno     = <str_tab_msg>-msgno.
      l_s_msg-msgv1     = <str_tab_msg>-msgv1.
      l_s_msg-msgv2     = <str_tab_msg>-msgv2.
      l_s_msg-msgv3     = <str_tab_msg>-msgv3.
      l_s_msg-msgv4     = <str_tab_msg>-msgv4.

* add this message to log file
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_s_msg       = l_s_msg
          i_log_handle  = s_loghandle
        EXCEPTIONS
          log_not_found = 0
          OTHERS        = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD send_email_notif.
    DATA :
      lt_email_content TYPE STANDARD TABLE OF solisti1 WITH DEFAULT KEY,
      lt_recipients    TYPE STANDARD TABLE OF somlreci1 WITH DEFAULT KEY.

    DATA(lv_email_title) = CONV so_obj_des( iv_email_title ).
    lt_recipients = VALUE #( FOR i = 1 THEN i + 1 UNTIL i > lines( it_ou_recipients )
                                ( receiver = it_ou_recipients[ i ]-sobid rec_type = 'B' )   ).
    DATA(ls_doc_data) = VALUE sodocchgi1( obj_descr = iv_email_title ).
    DATA(lv_decision_txt) = COND #( WHEN iv_rel_status = '02'  THEN 'approved'
                                    WHEN iv_rel_status = '03'  THEN 'rejected'
                                    ELSE '' ).

*    Generate email body
    IF iv_rel_status = '01'.    "In release, send approver email
      lt_email_content = VALUE #( ( CONV solisti1(  |Hi,<br><br>Kindly be advised that manual journal documents |         &&
                                                    | with Business Transaction ID { iv_bt_id }, |                        &&
                                                    |submitted by user { iv_initiator }, is in queue for|              ) )
                                  ( CONV solisti1(  | release approval.<br>Please go to { iv_email_link } and |           &&
                                                    |check to review/make release decision accordingly. Thank you.|       ) ) ).
    ELSE.
      lt_email_content = VALUE #( ( CONV solisti1(    |Hi,<br><br>Kindly be advised that your manual journal documents|   &&
                                                      | with Business Transaction ID { iv_bt_id } |                       &&
                                                      |has been { lv_decision_txt } by { iv_initiator }.<br>|              ) )
                                  ( CONV solisti1(    |Please go to { iv_email_link } to review decision detail |         &&
                                                      |and/or contact approver for more infomation. Thank you.|               ) ) ).
    ENDIF.
*    Add the executing user as a Cc recipient
    DATA(ls_sender_rec) = VALUE somlreci1( receiver = sy-uname rec_type = 'B' copy = 'X' ).
    APPEND ls_sender_rec TO lt_recipients.

*    Swap executing user with workflow user while sending email
    DATA(lv_sys_uname) = sy-uname.
    sy-uname = gv_email_sndr_id.
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        document_data  = ls_doc_data
        document_type  = 'HTM'
        commit_work    = 'X'
        put_in_outbox  = 'X'
      TABLES
        object_content = lt_email_content
        receivers      = lt_recipients.

    sy-uname = lv_sys_uname.
  ENDMETHOD.


  METHOD check_md_/ba1/c55accsy.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/c55accsy.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If accounting system table is not loaded, read from DB
    IF pt_acc_sys IS INITIAL.
      SELECT * FROM /ba1/tbr_accsys INTO TABLE @pt_acc_sys .
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |/BA1/TBR_ACCSYS|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_acc_sys WITH KEY mandt = sy-mandt acc_system = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C55ACCSY|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD convert_post_doc_int_2_ext.

    LOOP AT it_mje_doc ASSIGNING FIELD-SYMBOL(<str_mje_doc>).
      AT FIRST.
        APPEND INITIAL LINE TO rt_doc_ext ASSIGNING FIELD-SYMBOL(<str_doc_ext>).
        MOVE-CORRESPONDING <str_mje_doc> TO <str_doc_ext>.
      ENDAT.

      IF <str_doc_ext>-/ba1/c55docnum <> <str_mje_doc>-/ba1/c55docnum.
        APPEND INITIAL LINE TO rt_doc_ext ASSIGNING <str_doc_ext>.
        MOVE-CORRESPONDING <str_mje_doc> TO <str_doc_ext>.
      ENDIF.
      APPEND INITIAL LINE TO <str_doc_ext>-items ASSIGNING FIELD-SYMBOL(<str_items>).
      MOVE-CORRESPONDING <str_mje_doc> TO <str_items>.
      APPEND INITIAL LINE TO <str_items>-tab_gaap ASSIGNING FIELD-SYMBOL(<str_gaap>).
      MOVE-CORRESPONDING <str_mje_doc> TO <str_gaap>.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_mje_upload_control.
    DATA:
      ls_mje_ctrl      LIKE LINE OF pt_mje_ctrl,
      lt_dom_vals      TYPE dd07vtab,
      lt_mje_ctrl      LIKE pt_mje_ctrl,
      lt_curr_dom_vals TYPE dd07vtab,
      lv_err           TYPE boolean.

    IF pt_mje_ctrl IS INITIAL.
      SELECT * FROM zmje_upl_ctrl INTO CORRESPONDING FIELDS OF TABLE @pt_mje_ctrl.
      IF sy-subrc <> 0.
        MESSAGE s602(zfpsl_mje).
        msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
        msg_save( iv_exit = abap_true iv_loghandle = s_loghandle ).
      ENDIF.

      SELECT domvalue_l, ddtext
        FROM dd07t INTO CORRESPONDING FIELDS OF TABLE @lt_dom_vals
        WHERE domname = 'Z_MJE_GRP_IND' AND ddlanguage = 'E' AND as4local = 'A'
        ORDER BY valpos ASCENDING.
      IF sy-subrc <> 0.
        MESSAGE s603(zfpsl_mje).
        msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
        msg_save( iv_exit = abap_true iv_loghandle = s_loghandle ).
      ENDIF.

      LOOP AT pt_mje_ctrl ASSIGNING FIELD-SYMBOL(<str_mje_ctrl>).
        CLEAR lt_curr_dom_vals.
        decode_ug_mask(
        EXPORTING
            iv_mask = <str_mje_ctrl>-mandatory_mask
            it_all_ug = lt_dom_vals
        IMPORTING
            rv_error = lv_err
            rt_dom_val = lt_curr_dom_vals ).

        LOOP AT lt_curr_dom_vals ASSIGNING FIELD-SYMBOL(<str_dom_vals>).
          DATA(lv_grp_ind) = CONV z_dte_mje_grp_ind( <str_dom_vals>-domvalue_l ).
          INSERT lv_grp_ind INTO TABLE <str_mje_ctrl>-tab_mandatory_ug.
        ENDLOOP.

*        Duplicate the control table with field name
        CLEAR ls_mje_ctrl.
        MOVE-CORRESPONDING <str_mje_ctrl> TO ls_mje_ctrl.
        IF ls_mje_ctrl-field_name IS NOT INITIAL.
          ls_mje_ctrl-iobjnm = ls_mje_ctrl-field_name.
        ENDIF.
        INSERT ls_mje_ctrl INTO TABLE lt_mje_ctrl.
      ENDLOOP.
    ENDIF.

    rt_mje_ctrl = pt_mje_ctrl.
  ENDMETHOD.


  METHOD check_md.
    rv_passed = abap_true.

    LOOP  AT it_mje_ctrl ASSIGNING FIELD-SYMBOL(<str_mje_ctrl>) WHERE md_check = 'X'.
      DATA(lv_field) = get_mje_iobj_field( <str_mje_ctrl> ).
      ASSIGN COMPONENT lv_field OF STRUCTURE is_mje_doc TO FIELD-SYMBOL(<val_fld>).
      IF sy-subrc <> 0.
        CONTINUE.       "Skipping if field not found in upload file.
      ENDIF.

*      If field value is supplied, validate master data
      IF <val_fld> IS NOT INITIAL.
        READ TABLE pt_mje_fld_elem WITH KEY fieldname = lv_field ASSIGNING FIELD-SYMBOL(<str_fld_elem>).
        CASE <str_fld_elem>-datatype.
          WHEN 'CUKY'.  "Validate currency
            DATA(lv_curr) = CONV waers_curc( <val_fld> ).
            DATA(lv_valid_curr) = check_md_currency( iv_curr = lv_curr iv_msg_doc_id = iv_msg_doc_id iv_loghandle = iv_loghandle ).
            rv_passed = COND #( WHEN rv_passed = abap_true AND lv_valid_curr = abap_true THEN abap_true ELSE abap_false ).

          WHEN OTHERS.  "Validate other fields
            DATA(lv_ns) = <str_mje_ctrl>-iobjnm+0(1).
            CASE lv_ns.
              WHEN 'Z'.     "Custom InfoObjects
                DATA(lv_valid_md)   = check_md_custom_iobj( is_mje_ctrl = <str_mje_ctrl> iv_value = <val_fld> iv_msg_doc_id = iv_msg_doc_id iv_loghandle = iv_loghandle ).
              WHEN OTHERS.  "Standard InfoObjects
                lv_valid_md         =   check_md_fpsl_iobj( iv_iobj = <str_mje_ctrl>-iobjnm iv_value = <val_fld> iv_msg_doc_id = iv_msg_doc_id iv_loghandle = iv_loghandle ).
            ENDCASE.
            rv_passed = COND #( WHEN rv_passed = abap_true AND lv_valid_md = abap_true THEN abap_true ELSE abap_false ).
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_md_/ba1/c55dbcdf.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/c55dbcdf.

    lv_value = iv_value.
    rv_passed = abap_true.

    IF lv_value <> 'D' AND lv_value <> 'C'.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C55DBCDF|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_md_/ba1/c55cocntr.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/c55cocntr.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If cost center table is not loaded, read from DB
    IF pt_md_cost_cntr IS INITIAL.
      SELECT * FROM csks INTO TABLE @pt_md_cost_cntr .
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |CSKS|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_md_cost_cntr WITH KEY mandt = sy-mandt kostl = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C55COCNTR|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_md_/ba1/c55lgent.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/c55lgent.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If legal entity master data table is empty, load the table.
    IF pt_md_lgent IS INITIAL.
      SELECT * INTO TABLE pt_md_lgent FROM /ba1/tbr_lgentv.
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |/BA1/TBR_LGENTV|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_md_lgent WITH KEY client = sy-mandt legal_entity = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C55LGENT|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_md_/ba1/c55contid.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/c55contid.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If contract ID table is empty, load the table.
*    For performance, versions are selected only if:
*      - they are current
*      - they include the run date in its validity date range ( sys_current - partition_valid_to ).
    IF pt_md_contid IS INITIAL.
      DATA(lv_ts) = |{ sy-datlo }{ sy-timlo }|.
      SELECT ext_contract INTO TABLE pt_md_contid FROM /ba1/f1_con_flat WHERE
        sys_current         =   'X'     AND
        bus_valid_from      <=  lv_ts   AND
        partition_valid_to  >   lv_ts .
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |/BA1/F1_CON_FLAT|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_md_contid WITH KEY table_line  = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C55CONTID|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD msg_add.
    DATA:
      l_s_msg TYPE bal_s_msg,
      lv_type LIKE iv_type.

*    If msg. type override is not provided, default to "S".
    IF iv_type IS NOT SUPPLIED .
      lv_type = 'S'.
    ELSE.
      lv_type = iv_type.
    ENDIF.

* define data of message for Application Log
    l_s_msg-msgty     = lv_type.
    l_s_msg-msgid     = sy-msgid.
    l_s_msg-msgno     = sy-msgno.
    l_s_msg-msgv1     = sy-msgv1.
    l_s_msg-msgv2     = sy-msgv2.
    l_s_msg-msgv3     = sy-msgv3.
    l_s_msg-msgv4     = sy-msgv4.

* add this message to log file
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg       = l_s_msg
        i_log_handle  = iv_loghandle
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD check_md_/ba1/ichrt_acc.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/ichrt_accts.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If chart of account master data table is empty, load the table.
    IF pt_md_chrt_acc IS INITIAL.
      SELECT mandt, ktopl INTO CORRESPONDING FIELDS OF TABLE @pt_md_chrt_acc FROM ska1 GROUP BY mandt, ktopl.
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |SKA1|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_md_chrt_acc WITH KEY mandt = sy-mandt ktopl = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/ICHRT_ACC|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD init_mnt_proc_param.
*  Create a dummy parameter set required for the M&T process using:
*  - A dummy timestamp reference object
*  - A CVPM parameter structure
*
*  If exceptions, return null.

    DATA(lr_tc) = /ba1/cl_al_f3_cta_tc=>s_create_new( 'MJE_DUMMY' ).
    TRY.
        lr_tc->add_timestamp( i_dagid = /ba1/if_al_f3_cta_dagid=>con_source_data    i_timestamp = lr_tc->get_creation_ts( ) ).
        lr_tc->add_timestamp( i_dagid = /ba1/if_al_f3_cta_dagid=>con_results_data   i_timestamp = lr_tc->get_creation_ts( ) ).
        lr_tc->add_timestamp( i_dagid = /ba1/if_al_f3_cta_dagid=>con_market_data    i_timestamp = lr_tc->get_creation_ts( ) ).
        lr_tc->add_timestamp( i_dagid = /ba1/if_al_f3_cta_dagid=>con_register_data  i_timestamp = lr_tc->get_creation_ts( ) ).
      CATCH /ba1/cx_al_f3_cta_invalid_call /ba1/cx_al_f3_cta_failure INTO DATA(lx_error).
        exception_add( lx_error ).
        CLEAR rs_proc_param.
        RETURN.
    ENDTRY.

    rs_proc_param = VALUE /ba1/if_al_fw_step_control=>t_str_param(
        run_sequentially = 'X'
        sequence = '_DEFAULT'
        ref_tc = lr_tc ).
  ENDMETHOD.


  METHOD convert_post_doc_ext_2_int.
    DATA:
         ls_mje_doc TYPE /ba1/hfppd.

    LOOP AT it_doc_ext ASSIGNING FIELD-SYMBOL(<str_doc_ext>).
      LOOP AT <str_doc_ext>-items ASSIGNING FIELD-SYMBOL(<str_items>).
        MOVE-CORRESPONDING <str_doc_ext>                  TO ls_mje_doc.
        MOVE-CORRESPONDING <str_items>                    TO ls_mje_doc.
        MOVE-CORRESPONDING <str_items>-tab_gaap[ 1 ]      TO ls_mje_doc.

        APPEND ls_mje_doc TO rt_mje_doc.
        CLEAR ls_mje_doc.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD offset_document.
    DATA:
      lt_offset_docs LIKE it_mje_doc,
      ls_mje_doc     LIKE LINE OF lt_offset_docs,
      ls_offset_doc  LIKE LINE OF lt_offset_docs.

    DATA(lv_num_lines) = lines( it_mje_doc ).
    LOOP AT it_mje_doc ASSIGNING FIELD-SYMBOL(<str_mje_doc>).
      MOVE-CORRESPONDING <str_mje_doc> TO ls_mje_doc.
      MOVE-CORRESPONDING <str_mje_doc> TO ls_offset_doc.

*      Copy the original document number (TBD)

*      Assign new document & item number - document number part starts with 1 (1XXXXXX) if offset mode is on
      DATA(lv_doc_num) = |{ sy-datlo }{ sy-timlo }1{ sy-tabix WIDTH = 5 ALIGN = RIGHT PAD = '0' }|.
      ls_mje_doc-/ba1/c55docnum  = lv_doc_num.
      ls_offset_doc-/ba1/c55docnum  = lv_doc_num.
      ls_mje_doc-/ba1/c55docitm  = 1.
      ls_offset_doc-/ba1/c55docitm  = 2.

*      Assign new offset SL/GL account
      ls_offset_doc-/ba1/c55slacc       = g_offset_sl_acc.
      ls_offset_doc-/ba1/igl_account    = g_offset_gl_acc.

*      Swap D/C indicator
      DATA(lv_offset_drcr) = COND #(  WHEN <str_mje_doc>-/ba1/c55dbcdf = 'C' THEN 'D'
                                      WHEN <str_mje_doc>-/ba1/c55dbcdf = 'D' THEN 'C' ).
      ls_offset_doc-/ba1/c55dbcdf = lv_offset_drcr.

*      Negate currency fields
      LOOP AT pt_mje_fld_elem ASSIGNING FIELD-SYMBOL(<str_mje_fld_elem>) WHERE datatype = 'CURR'.
        ASSIGN COMPONENT <str_mje_fld_elem>-fieldname OF STRUCTURE ls_offset_doc TO FIELD-SYMBOL(<val_curr>).
        IF sy-subrc = 0.
          <val_curr> = <val_curr> * -1.
        ENDIF.
      ENDLOOP.

      INSERT ls_mje_doc     INTO TABLE lt_offset_docs.
      INSERT ls_offset_doc  INTO TABLE lt_offset_docs.

      CLEAR ls_mje_doc.
      CLEAR ls_offset_doc.
    ENDLOOP.

    CLEAR it_mje_doc.
    INSERT LINES OF lt_offset_docs INTO TABLE it_mje_doc.
  ENDMETHOD.


  METHOD check_md_/ba1/igl_acct.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/igl_account.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If chart of account master data table is empty, load the table.
    IF pt_md_gl_acc IS INITIAL.
      SELECT mandt, saknr  INTO CORRESPONDING FIELDS OF TABLE @pt_md_gl_acc FROM ska1 GROUP BY mandt, saknr .
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |SKA1|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_md_gl_acc WITH KEY mandt = sy-mandt saknr = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/IGL_ACCT|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_adhoc.
    DATA:
      lv_meth  TYPE string,
      lt_param TYPE abap_parmbind_tab,
      lv_valid TYPE boolean.

    rv_passed = abap_true.

    LOOP AT it_mje_ctrl ASSIGNING FIELD-SYMBOL(<str_mje_ctrl>) WHERE ad_hoc_check = 'X'.
      lv_meth = |{ g_dyn_meth_ah_pref }{ <str_mje_ctrl>-iobjnm }|.

*      Logic for dynamic method call: invoke Ad-hoc check method "check_adhoc_{IOBJ_NAME}"
      lt_param = VALUE #(
                  ( name  = g_dyn_meth_ah_exp_p1
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( <str_mje_ctrl>  ) )
                  ( name  = g_dyn_meth_ah_exp_p2
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( is_mje_doc ) )
                  ( name  = g_dyn_meth_ah_exp_p3
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_msg_doc_id ) )
                  ( name  = g_dyn_meth_ah_exp_p4
                    kind  = cl_abap_objectdescr=>exporting
                    value = REF #( iv_loghandle ) )
                  ( name  = g_dyn_meth_md_ret_par
                    kind  = cl_abap_objectdescr=>returning
                    value = REF #( lv_valid ) ) ).

      TRY.
          CALL METHOD (lv_meth)
            PARAMETER-TABLE
            lt_param.

        CATCH cx_sy_dyn_call_error INTO DATA(lx_error).
          exception_add( lx_error ).
          MESSAGE s622(zfpsl_mje) WITH iv_msg_doc_id <str_mje_ctrl>-iobjnm.
          msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
          rv_passed = abap_false.
      ENDTRY.

      rv_passed = COND #( WHEN rv_passed = abap_true AND lv_valid = abap_true THEN abap_true ELSE abap_false ).
    ENDLOOP.
  ENDMETHOD.


  METHOD check_adhoc_/ba1/c55postd.
    rv_passed = check_posting_period(
        is_mje_ctrl     = is_mje_ctrl
        is_mje_doc      = is_mje_doc
        iv_msg_doc_id   = iv_msg_doc_id
        iv_loghandle    = iv_loghandle ).
  ENDMETHOD.


  METHOD check_adhoc_/ba1/c55pdres.
    rv_passed = abap_true.

*** this ad-hoc check examines the posting date of reset >= posting date ***

    ASSIGN is_mje_doc-/ba1/c55pdres TO FIELD-SYMBOL(<val_post_dat_res>).
    DATA(lv_rc_1) = sy-subrc.
    ASSIGN is_mje_doc-/ba1/c55postd TO FIELD-SYMBOL(<val_post_dat>).
    DATA(lv_rc_2) = sy-subrc.
    IF NOT ( lv_rc_1 = 0 AND lv_rc_2 = 0 ).
      MESSAGE s650(zfpsl_mje) WITH |{ iv_msg_doc_id }| |/BA1/C55PDRES|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
      RETURN.
    ENDIF.

    IF ( <val_post_dat_res> IS NOT INITIAL AND <val_post_dat_res> < <val_post_dat> ).
      MESSAGE s652(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ <val_post_dat_res> }| |{ <val_post_dat> }|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_posting_period.
    DATA:
      dt_fr    TYPE i,
      dt_to    TYPE i,
      lv_lgent TYPE acc_legent.

*** this ad-hoc check examines the dates in MJE postings against posting period ***

    lv_lgent = is_mje_doc-/ba1/c55lgent.
    DATA(lv_post_dat) = is_mje_doc-/ba1/c55postd.

*  Convert legal entity to alpha-numeric format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_lgent
      IMPORTING
        output = lv_lgent.

*    If posting period table is not loaded, read from DB
    IF pt_post_per IS INITIAL.
      SELECT * FROM /ba1/tbr_pper INTO TABLE @pt_post_per WHERE posting_process = 3.    "posting process = manual adjustment
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |/BA1/TBR_PPER|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(lv_pd) = lv_post_dat+0(6).
    rv_passed = abap_false.
    READ TABLE pt_post_per WITH KEY legal_entity = lv_lgent ASSIGNING FIELD-SYMBOL(<str_post_period>).
    IF sy-subrc = 0.
      CLEAR dt_fr.
      CLEAR dt_to.

      CONCATENATE <str_post_period>-year_from <str_post_period>-period_from+1(2) INTO DATA(lv_val).
      dt_fr = lv_val.
      CONCATENATE <str_post_period>-year_to <str_post_period>-period_to+1(2) INTO lv_val.
      dt_to = lv_val.

      IF lv_pd <= dt_to AND lv_pd >= dt_fr.
        rv_passed = abap_true.
      ENDIF.
    ENDIF.
    IF rv_passed = abap_false.
      MESSAGE s651(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ lv_post_dat }| |{ lv_lgent }|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
    ENDIF.
  ENDMETHOD.


  METHOD check_adhoc_/ba1/c55dbcdf.
    rv_passed = abap_true.

*** this ad-hoc check examines all amount fields that signage is respected for Dr/Cr ***

    ASSIGN is_mje_doc-/ba1/c55dbcdf TO FIELD-SYMBOL(<val_drcr>).

    LOOP AT pt_mje_fld_elem ASSIGNING FIELD-SYMBOL(<str_mje_fld_elem>) WHERE datatype = 'CURR'.
      ASSIGN COMPONENT <str_mje_fld_elem>-fieldname OF STRUCTURE is_mje_doc TO FIELD-SYMBOL(<val_curr>).
      IF sy-subrc = 0.  "Amount field available in upload
        IF NOT ( ( <val_drcr> = 'D' AND <val_curr> >= 0 )
          OR ( <val_drcr> = 'C' AND <val_curr> <= 0 ) ).
          MESSAGE s655(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ <val_drcr> }| |{ <val_curr> }|.
          msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
          rv_passed = abap_false.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_mje_fld_data_elem.
*    Populate the HFPPD field data element table for data type determination
    IF pt_mje_fld_elem IS INITIAL.
      SELECT SINGLE data_table FROM /ba1/hm_gen_obj INTO pv_rdl_data_tab WHERE area = pv_rdl_area AND object_name = pv_rdl_type AND object_type = 'T'.
      IF sy-subrc <> 0.
        CLEAR pt_mje_ctrl.
        MESSAGE s604(zfpsl_mje).
        msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
        msg_save( iv_exit = abap_true iv_loghandle = s_loghandle ).
      ENDIF.

      SELECT fieldname, datatype FROM dd03l
      WHERE tabname = @pv_rdl_data_tab
      INTO CORRESPONDING FIELDS OF TABLE @pt_mje_fld_elem .

      IF sy-subrc <> 0.
        CLEAR pt_mje_ctrl.
        MESSAGE s604(zfpsl_mje).
        msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
        msg_save( iv_exit = abap_true iv_loghandle = s_loghandle ).
      ENDIF.
    ENDIF.

    rt_mje_fld_elem = pt_mje_fld_elem.
  ENDMETHOD.


  METHOD check_adhoc_/ba1/c55accsy.
    rv_passed = abap_true.

*** this ad-hoc check examines the posting's accounting system is allowed for its legal entity ***

    IF pt_lgent_acc_sys IS INITIAL.
      SELECT * FROM /ba1/tbr_lgenta INTO TABLE @pt_lgent_acc_sys .
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |/BA1/TBR_LGENTA|.
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(lv_lgent) =  is_mje_doc-/ba1/c55lgent.
    DATA(lv_acc_sys) = is_mje_doc-/ba1/c55accsy.

*  Convert legal entity to alpha-numeric format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_lgent
      IMPORTING
        output = lv_lgent.

*    If accounting system is assigned, look up the accounting system entry in legal entity customizing
    IF lv_acc_sys IS NOT INITIAL.
      READ TABLE pt_lgent_acc_sys WITH KEY legal_entity = lv_lgent acc_system = lv_acc_sys ASSIGNING FIELD-SYMBOL(<val_lgent_acc_sys>).
      IF sy-subrc <> 0.
        MESSAGE s656(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ lv_acc_sys }| |{ lv_lgent }|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_je_amount.
    TYPES:
      BEGIN OF ts_curr_amounts,
        field  TYPE /ba1/f0_dte_characteristic,
        amount TYPE /ba1/bf_dte_amount_objcurr,
      END OF ts_curr_amounts,
      tt_curr_amounts TYPE STANDARD TABLE OF ts_curr_amounts.

    DATA:
      lv_curr_total   TYPE /ba1/bf_dte_amount_objcurr   VALUE 0,
      lv_curr_lgent   TYPE acc_legent                   VALUE IS INITIAL,
      lv_curr_doc_num TYPE /ba1/bf_dte_document_number  VALUE IS INITIAL,
      lt_curr_amounts TYPE tt_curr_amounts.

    rv_passed = abap_true.

*** this check examines the totals of all amount fields on document      ***
*** level add up to zero (therefore batch total adds up to zero as well) ***

    LOOP AT it_mje_doc ASSIGNING FIELD-SYMBOL(<str_mje_doc>).
      ASSIGN:
      <str_mje_doc>-/ba1/c55docnum TO FIELD-SYMBOL(<val_doc_num>),
      <str_mje_doc>-/ba1/c55lgent TO FIELD-SYMBOL(<val_lgent>).

      IF lv_curr_doc_num <> <val_doc_num>.
        LOOP AT lt_curr_amounts ASSIGNING FIELD-SYMBOL(<str_curr_amounts>).
          IF <str_curr_amounts>-amount <> 0.
            MESSAGE s700(zfpsl_mje) WITH |Document { lv_curr_lgent }\|{ lv_curr_doc_num }| |{ <str_curr_amounts>-amount }| |{ <str_curr_amounts>-field }|.
            msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
            rv_passed = abap_false.
          ENDIF.
        ENDLOOP.

        CLEAR lt_curr_amounts.
        lv_curr_doc_num = <val_doc_num>.
        lv_curr_lgent = <val_lgent>.
        LOOP AT pt_mje_fld_elem ASSIGNING FIELD-SYMBOL(<str_mje_fld_elem>) WHERE datatype = 'CURR'.
          APPEND INITIAL LINE TO lt_curr_amounts ASSIGNING <str_curr_amounts>.
          <str_curr_amounts>-field = <str_mje_fld_elem>-fieldname.

          ASSIGN COMPONENT <str_curr_amounts>-field OF STRUCTURE <str_mje_doc> TO FIELD-SYMBOL(<val_amount>).
          IF sy-subrc = 0.
            <str_curr_amounts>-amount = <val_amount>.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT pt_mje_fld_elem ASSIGNING <str_mje_fld_elem> WHERE datatype = 'CURR'.
          READ TABLE lt_curr_amounts WITH KEY field = <str_mje_fld_elem>-fieldname ASSIGNING <str_curr_amounts>.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO lt_curr_amounts ASSIGNING <str_curr_amounts>.
            <str_curr_amounts>-field = <str_mje_fld_elem>-fieldname.
          ENDIF.

          ASSIGN COMPONENT <str_curr_amounts>-field OF STRUCTURE <str_mje_doc> TO <val_amount>.
          IF sy-subrc = 0.
            <str_curr_amounts>-amount = <str_curr_amounts>-amount + <val_amount>.
          ENDIF.
        ENDLOOP.
      ENDIF.

      AT LAST.
        LOOP AT lt_curr_amounts ASSIGNING <str_curr_amounts>.
          IF <str_curr_amounts>-amount <> 0.
            MESSAGE s700(zfpsl_mje) WITH |Document { lv_curr_lgent }\|{ lv_curr_doc_num }| |{ <str_curr_amounts>-amount }| |{ <str_curr_amounts>-field }|.
            msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
            rv_passed = abap_false.
          ENDIF.
        ENDLOOP.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_mje_iobj_field.
    IF is_mje_ctrl-field_name IS NOT INITIAL.
      rv_field = is_mje_ctrl-field_name.
    ELSE.
      rv_field = is_mje_ctrl-iobjnm.
    ENDIF.
  ENDMETHOD.


  METHOD enrich_fpsl_gl_derivation.
    DATA:
      ls_doc_hdr   TYPE /ba1/br_str_sldoc_hdr,
      ls_doc_items TYPE /ba1/br_str_sldoc_itm.

    rv_passed = abap_true.

    MOVE-CORRESPONDING is_mje_doc TO ls_doc_hdr.
    MOVE-CORRESPONDING is_mje_doc TO ls_doc_items.
    APPEND INITIAL LINE TO ls_doc_items-tab_gaap ASSIGNING FIELD-SYMBOL(<str_gaap>).
    MOVE-CORRESPONDING is_mje_doc TO <str_gaap>.
    TRY.
        po_br_rules->/ba1/if_al_br_rules~set_new_document( i_str_hdr = ls_doc_hdr ).
        po_br_rules->_derive_gl_account_item(
            EXPORTING
                i_str_hdr = ls_doc_hdr
            CHANGING
                c_str_item = ls_doc_items ).
      CATCH /ba1/cx_al_br_rules INTO DATA(lx_error).
        exception_add( lx_error ).
        rv_passed = abap_false.
    ENDTRY.
    IF sy-msgty = 'W' AND sy-msgid = '/BA1/BR_COMMON_OBJ' AND sy-msgno = 414.
*      Captures G/L derivation error
      msg_add( iv_type = 'W' iv_loghandle = s_loghandle ).
    ENDIF.

    is_mje_doc-/ba1/igl_account = ls_doc_items-tab_gaap[ 1 ]-/ba1/igl_account.
    is_mje_doc-/ba1/ichrt_accts = ls_doc_items-tab_gaap[ 1 ]-/ba1/ichrt_accts.
  ENDMETHOD.


  METHOD enrich_fpsl_mnt.
*  METHOD DEACTICVATED AS M&T IS ALREADY DONE IN OTHER STANDARD CODE    *******
*******************************************************************************
*
*    DATA:
*      ls_str_param TYPE /ba1/if_al_fw_step_control=>t_str_param,
*      lt_mnt_items TYPE /ba1/br_tab_adj_itms.
*
*    rv_passed = abap_true.
*
*    DATA(lt_doc_ext) = convert_post_doc_int_2_ext( it_mje_doc ).
*    LOOP AT lt_doc_ext ASSIGNING FIELD-SYMBOL(<str_doc_ext>).
*      IF ls_str_param IS INITIAL.
*        ls_str_param = init_mnt_proc_param( ).
*        IF ls_str_param IS INITIAL.             "Empty parameter structure indicates error
*          MESSAGE s711(zfpsl_mje).
*          msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
*          rv_passed = abap_false.
*          RETURN.
*        ENDIF.
*      ENDIF.
*
**      Assign mandatory process parameters
*      ls_str_param-tab_par_value = VALUE /ba1/fw_tty_par_value(
*                      ( parameter = '/BA1/C55LGENT' sign = 'I' option = 'EQ' low = <str_doc_ext>-/ba1/c55lgent )
*                      ( parameter = '/BA1/C55POSTD' sign = 'I' option = 'EQ' low = <str_doc_ext>-/ba1/c55postd ) ).
*
*      TRY.
*          /ba1/cl_al_br_adj_relay=>s_get_instance(
*                ir_msg_handler  = /ba1/cl_al_x0_msg_helper=>s_ref_null_handler
*                i_str_param     = ls_str_param
**            ASSUMPTION: accounting systems are the same across all journal items!!!!!
*                 )->enrich_by_mt_doclines(
*                    EXPORTING
*                        is_header       = <str_doc_ext>
*                        i_acc_system    = <str_doc_ext>-items[ 1 ]-tab_gaap[ 1 ]-/ba1/c55accsy
*                    IMPORTING
*                        et_mt_item = lt_mnt_items ).
*        CATCH /ba1/cx_al_br_adjust INTO DATA(lx_error).
*          exception_add( lx_error ).
*          MESSAGE s712(zfpsl_mje) WITH <str_doc_ext>-/ba1/c55docnum.
*          msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
*          rv_passed = abap_false.
*          CONTINUE.
*      ENDTRY.
*
**      If M&T yields any results, replace original items table with the M&T results.
*      IF lines( lt_mnt_items ) <> lines( <str_doc_ext>-items ).
*        CLEAR <str_doc_ext>-items.
*        APPEND LINES OF lt_mnt_items TO <str_doc_ext>-items.
*        MESSAGE s003(zfpsl_mje) WITH <str_doc_ext>-/ba1/c55docnum.
*        msg_add( iv_loghandle ).
*      ENDIF.
*    ENDLOOP.
*
*    CLEAR it_mje_doc.
*    DATA(lt_mje_doc_res) = convert_post_doc_ext_2_int( lt_doc_ext ).
*    APPEND LINES OF lt_mje_doc_res TO it_mje_doc.
  ENDMETHOD.


  METHOD enrich_technical_fields .
    DATA:
      lv_bt_id     TYPE /ba1/f2_dte_bt_id,
      lv_doc_num   TYPE /ba1/bf_dte_document_number,
      lv_cont_ct   TYPE /ba1/bf_dte_contract_category,
      lv_cont_node TYPE /ba1/f1_dte_node_no.

*---    Add all technical constants here    ---*
*    Technical process parameters - user name, client, creation timestamp
    is_mje_doc-client           = sy-mandt.
    is_mje_doc-/ba1/c55ppcrus   = sy-uname.
    is_mje_doc-/ba1/cr0tstmp = iv_ts.

*  31 character BT ID format: "MJE_{12 char long user name}_YYYYMMDDHHMMSS"
    is_mje_doc-/ba1/c35btran = |MJE_{ sy-uname WIDTH = 12 ALIGN = LEFT PAD = '_' }_{ iv_ts }|.
    is_mje_doc-/ba1/c35btran = is_mje_doc-/ba1/c35btran+0(31).

*  Create a document number prefix to differentiate documents from repeated uploads
    is_mje_doc-/ba1/c55docnum = COND #( WHEN iv_offset = abap_true     THEN   |{ is_mje_doc-/ba1/c55docnum WIDTH = 6 ALIGN = RIGHT PAD = '0' }|
                                        WHEN iv_offset = abap_false    THEN   |{ sy-datlo }{ sy-timlo }{ is_mje_doc-/ba1/c55docnum WIDTH = 6 ALIGN = RIGHT PAD = '0' }| ).

*  Derive contract category and number number
    is_mje_doc-/ba1/c55contct   = COND #( WHEN is_mje_doc-/ba1/c55contid IS NOT INITIAL THEN 1 ELSE '' ).
    is_mje_doc-/ba1/c11nodeno   = COND #( WHEN is_mje_doc-/ba1/c55contid IS NOT INITIAL THEN 1 ELSE '' ).
***********************************************
  ENDMETHOD.


  METHOD bapirettab_add.
    DATA:
        l_s_msg       TYPE bal_s_msg.

    LOOP AT it_tab_message ASSIGNING FIELD-SYMBOL(<str_bapi_msg>).
*      define data of message for Application Log
      l_s_msg-msgty     = <str_bapi_msg>-type.
      l_s_msg-msgid     = <str_bapi_msg>-id.
      l_s_msg-msgno     = <str_bapi_msg>-number.
      l_s_msg-msgv1     = <str_bapi_msg>-message_v1.
      l_s_msg-msgv2     = <str_bapi_msg>-message_v2.
      l_s_msg-msgv3     = <str_bapi_msg>-message_v3.
      l_s_msg-msgv4     = <str_bapi_msg>-message_v4.

*      add this message to log file
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_s_msg       = l_s_msg
          i_log_handle  = s_loghandle
        EXCEPTIONS
          log_not_found = 0
          OTHERS        = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_mandatory_field.
    rv_passed = abap_true.

    LOOP AT it_mje_ctrl ASSIGNING FIELD-SYMBOL(<str_mje_ctrl>) WHERE tab_mandatory_ug IS NOT INITIAL.
      READ TABLE <str_mje_ctrl>-tab_mandatory_ug WITH KEY table_line = iv_ug TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.   "Not a mandatory field for this user group. Skipping.
      ENDIF.

      DATA(lv_field) = get_mje_iobj_field( <str_mje_ctrl> ).
      ASSIGN COMPONENT lv_field OF STRUCTURE is_mje_doc TO FIELD-SYMBOL(<val_fld>).
      IF sy-subrc <> 0.
        MESSAGE s610(zfpsl_mje) WITH  iv_msg_doc_id lv_field.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        CONTINUE.
      ENDIF.

      IF <val_fld> IS INITIAL.
        MESSAGE s611(zfpsl_mje) WITH iv_msg_doc_id lv_field.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_md_/ba1/c55bukrs.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/c55bukrs.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If company code table is not loaded, read from DB
    IF pt_comp_code IS INITIAL.
      SELECT * FROM t001 INTO TABLE @pt_comp_code .
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |T001|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_comp_code WITH KEY mandt = sy-mandt bukrs = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C55BUKRS|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_md_/ba1/c55slacc.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/c55slacc.

    lv_value = iv_value.
    rv_passed = abap_true.

*    If subledger account master data table is empty, load the table.
    IF pt_md_sl_acc IS INITIAL.
      SELECT client, sla, sla_group FROM /ba1/tbr_sla
      UNION
      SELECT @sy-mandt, sla, sla_group FROM /ba1/tbrssla
       INTO TABLE @pt_md_sl_acc.
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |/BA1/TBR_SLA|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_md_sl_acc WITH KEY client = sy-mandt sla = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C55SLACC|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_md_/ba1/c80prfctr.
    DATA:
      lv_value TYPE /ba1/hfppd-/ba1/cp0prfctr.

    lv_value = iv_value.
    rv_passed = abap_true.

*    Convert value to alpha-numeric format
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_value
      IMPORTING
        output = lv_value.

*    If profit center master data table is empty, load the table.
    IF pt_md_prof_cntr IS INITIAL.
      SELECT * INTO TABLE pt_md_prof_cntr FROM cepc.
      IF sy-subrc <> 0.
        MESSAGE s620(zfpsl_mje) WITH |{ iv_msg_doc_id }| |CEPC|.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    READ TABLE pt_md_prof_cntr WITH KEY mandt = sy-mandt prctr = lv_value TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |/BA1/C80PRFCTR|.
      msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
      rv_passed = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_md_custom_iobj.
    DATA:
      lv_val  TYPE string.

    rv_passed = abap_true.

*lv_iobj = get_mje_control_field( iv_iobj ).
    DATA(lv_tab_name) = get_cust_iobj_md_tab( is_mje_ctrl-iobjnm ).
    DATA(lv_query_filter) = |{ is_mje_ctrl-field_name } = '{ iv_value }' AND OBJVERS = 'A'|.

    TRY.
        SELECT SINGLE (is_mje_ctrl-field_name) INTO lv_val FROM (lv_tab_name) WHERE (lv_query_filter).
        IF sy-subrc <> 0.
          MESSAGE s621(zfpsl_mje) WITH |{ iv_msg_doc_id }| |{ iv_value }| |{ is_mje_ctrl-iobjnm }|.
          msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
          rv_passed = abap_false.
        ENDIF.
      CATCH cx_root INTO DATA(lx_error).
        exception_add( i_ref_ex = lx_error ).
        rv_passed = abap_false.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD check_md_fpsl_iobj.
    DATA:
      lv_meth  TYPE string,
      lt_param TYPE abap_parmbind_tab.


*    Logic for dynamic method call: invoke master data check method "check_md_{STANDARD_IOBJ_NAME}"
    DATA(lv_iobj_qualified) = iv_iobj.
*    REPLACE '/' IN lv_iobj_qualified WITH '_'.
    lv_meth = |{ g_dyn_meth_md_pref }{ lv_iobj_qualified }|.
    lt_param = VALUE #(
                ( name  = g_dyn_meth_md_exp_p1
                  kind  = cl_abap_objectdescr=>exporting
                  value = REF #( iv_value  ) )
                ( name  = g_dyn_meth_md_exp_p2
                  kind  = cl_abap_objectdescr=>exporting
                  value = REF #( iv_msg_doc_id ) )
                ( name  = g_dyn_meth_md_exp_p3
                  kind  = cl_abap_objectdescr=>exporting
                  value = REF #( iv_loghandle ) )
                ( name  = g_dyn_meth_md_ret_par
                  kind  = cl_abap_objectdescr=>returning
                  value = REF #( rv_passed ) ) ).

    TRY.
        CALL METHOD (lv_meth)
          PARAMETER-TABLE
          lt_param.

      CATCH cx_sy_dyn_call_error INTO DATA(lx_error).
        exception_add( lx_error ).
        MESSAGE s622(zfpsl_mje) WITH iv_msg_doc_id iv_iobj.
        msg_add( iv_type = 'E' iv_loghandle = iv_loghandle ).
        rv_passed = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD encode_ug_mask.
    DATA :
      lv_hex  TYPE x LENGTH 8 VALUE 0,
      lv_hex1 TYPE x LENGTH 8.

    LOOP AT it_dom_val ASSIGNING FIELD-SYMBOL(<str_dom_val>).
      DATA(lv_val) = 2 ** ( <str_dom_val>-domvalue_l - 1 ).
      lv_hex1 = lv_val.

      lv_hex = lv_hex BIT-OR lv_hex1.
    ENDLOOP.

    rv_mask = lv_hex.
  ENDMETHOD.


  METHOD get_cust_iobj_md_tab.

*    Get infoObject name from the field name ("/BIC/*******" )
*    SPLIT iv_field AT '/' INTO TABLE DATA(lt_fields).
*    DATA(lv_idx) = lines( lt_fields ).

*    Return master data table name for the InfoObject ("/BIC/P*******" )
*    rv_tab_name = |/BIC/P{ lt_fields[ lv_idx ] }|.
    rv_tab_name = |/BIC/P{ iv_iobj }|.
  ENDMETHOD.


  METHOD get_derivation_helper.
    TRY.
        /ba1/cl_al_br_rules=>s_get_instance(
        EXPORTING
            i_log =  /ba1/cl_al_br_application_log=>s_get_instance(
                        i_context_gen_name = ''
                        i_ref_msg_handler  = /ba1/cl_al_x0_msg_helper=>s_ref_null_handler )
            i_customizing = /ba1/cl_al_br_common_obj_cust=>s_get_instance( )
        IMPORTING
            e_instance = DATA(lo_br_rules) ).

        ro_br_rules ?= lo_br_rules.
        po_br_rules = ro_br_rules.
      CATCH  /ba1/cx_al_br_customizing INTO DATA(lx_error).
        MESSAGE s605(zfpsl_mje).
        msg_add( iv_type = 'E' iv_loghandle = s_loghandle  ).
        msg_save( iv_exit = abap_true iv_loghandle = s_loghandle ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_instance.
    IF s_instance IS INITIAL.
      CREATE OBJECT s_instance TYPE zcl_mje_util_helper.
    ENDIF.

*    Retrieve MJE result area/type/data table
    TRY.
        /ba1/cl_al_br_cus_bs=>get_instance( )->get_result_type( EXPORTING i_result = g_accrt_pslpd
                                                          IMPORTING e_result_area = s_instance->pv_rdl_area e_result_type = s_instance->pv_rdl_type ).

        SELECT SINGLE data_table FROM /ba1/hm_gen_obj INTO s_instance->pv_rdl_data_tab WHERE area = s_instance->pv_rdl_area AND object_name = s_instance->pv_rdl_type AND object_type = 'T'.
      CATCH /ba1/cx_al_br_customizing.
        MESSAGE s601(zfpsl_mje) WITH g_accrt_pslpd.
        s_instance->msg_add( iv_type = 'W' iv_loghandle = s_instance->s_loghandle  ).
        s_instance->pv_rdl_area = 'SAFI'.
        s_instance->pv_rdl_type = '_S_PSLPD'.
        s_instance->pv_rdl_data_tab = '/BA1/HFPPD'.
    ENDTRY.
    s_instance->get_mje_upload_control( ).
    s_instance->get_mje_fld_data_elem( ).
    s_instance->get_derivation_helper( ).               " Initialize FPSL result management helper class

    r_result = s_instance.
  ENDMETHOD.


  METHOD get_loghandle.
    IF s_loghandle IS INITIAL.
      DATA(ls_log) = VALUE bal_s_log(
            extnumber  = sy-repid
            object     = iv_log_obj
            subobject  = iv_log_sobj
            aluser     = sy-uname
            alprog     = sy-repid ).

      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log      = ls_log
        IMPORTING
          e_log_handle = s_loghandle
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    rv_loghandle = s_loghandle.
  ENDMETHOD.


  METHOD get_ou_requestors.

    IF pt_hrp1001 IS INITIAL.
      DATA:
        ls_req_pos LIKE LINE OF pt_hrp1001,
        lt_req_pos LIKE pt_hrp1001.

      SELECT * FROM hrp1001
      INTO TABLE @pt_hrp1001      WHERE
              begda <= @sy-datlo    AND
              endda >= @sy-datlo    AND
              plvar = '01'.
      IF sy-subrc <> 0.
        MESSAGE e401(zfpsl_mje).
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT pt_hrp1001 ASSIGNING FIELD-SYMBOL(<str_hrp1001>)
*        Find approver's org structure position - 1 OR MULTIPLE GROUPS MAY RETURN!
        WHERE   sobid = iv_approver
        AND     relat = '008'.

*      Find approver's position's org unit subordinate position
      READ TABLE pt_hrp1001 WITH KEY objid = <str_hrp1001>-objid relat = '002' rsign = 'B' INTO ls_req_pos.
      IF sy-subrc = 0.
        APPEND ls_req_pos TO lt_req_pos.
      ENDIF.

      CLEAR ls_req_pos.
    ENDLOOP.

    IF lt_req_pos IS INITIAL.
      MESSAGE e403(zfpsl_mje) WITH iv_approver.
      RETURN.
    ENDIF.

*    Return all users under this position
    LOOP AT lt_req_pos ASSIGNING FIELD-SYMBOL(<str_appr_pos>).
      LOOP AT pt_hrp1001 ASSIGNING <str_hrp1001>
          WHERE   objid = <str_appr_pos>-sobid
          AND     relat = '008'
          AND     rsign = 'A'.

        APPEND <str_hrp1001> TO rt_requestors.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD msg_display.
    DATA lt_log_handle TYPE bal_t_logh.
    APPEND iv_loghandle TO lt_log_handle.

*  Display generated logs
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle = lt_log_handle
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD msg_save.
    DATA lt_log_handle TYPE bal_t_logh.
    APPEND iv_loghandle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAL_LOG_REFRESH'
        EXPORTING
          i_log_handle = iv_loghandle
        EXCEPTIONS
          OTHERS       = 0.
    ELSE.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF iv_exit IS SUPPLIED AND iv_exit = abap_true.
      LEAVE PROGRAM.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
