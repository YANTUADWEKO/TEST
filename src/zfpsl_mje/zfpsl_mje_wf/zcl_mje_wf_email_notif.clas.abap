CLASS zcl_mje_wf_email_notif DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
             tt_wfsyst    TYPE STANDARD TABLE OF wfsyst-act_agent WITH DEFAULT KEY.

    INTERFACES bi_object .
    INTERFACES bi_persistent .
    INTERFACES if_workflow .

    CLASS-METHODS class_constructor .
    CLASS-METHODS s_get_instance RETURNING VALUE(r_instance) TYPE REF TO zcl_mje_wf_email_notif.

    METHODS send_email_notif
      IMPORTING
        iv_initiator TYPE hrobjec_14
        iv_approvers TYPE tt_wfsyst
        is_doc_key   TYPE /ba1/br_str_adj_document_key
        iv_released  TYPE xfeld OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA s_instance TYPE REF TO zcl_mje_wf_email_notif.
ENDCLASS.



CLASS zcl_mje_wf_email_notif IMPLEMENTATION.
  METHOD class_constructor.
    s_get_instance( ).
  ENDMETHOD.

  METHOD s_get_instance.
    IF s_instance IS INITIAL.
      CREATE OBJECT s_instance.
    ENDIF.

    r_instance = s_instance.
  ENDMETHOD.

  METHOD bi_object~default_attribute_value.

  ENDMETHOD.

  METHOD bi_object~execute_default_method.

  ENDMETHOD.

  METHOD bi_persistent~find_by_lpor.
    result = s_instance.
  ENDMETHOD.

  METHOD bi_persistent~lpor.

    result-catid  = swfut_catid_cl.
    result-typeid = 'ZCL_MJE_WF_EMAIL_NOTIF'.
    result-instid = 1.

  ENDMETHOD.

  METHOD bi_persistent~refresh.
    CLEAR s_instance.
  ENDMETHOD.

  METHOD bi_object~release.

  ENDMETHOD.

  METHOD send_email_notif.
    DATA(lo_bcs) = cl_bcs=>create_persistent( ).

*    Create a parameter structure with the same data definition of the source CDS view
    IF iv_released IS SUPPLIED.
      DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = 'ZMJE_EML_TPL_RELEASE' ).
      DATA(ls_eml_param) = VALUE z_c_mje_eml_tpl(
*        type    = 'DEFAULT'
          initiator_id    = iv_initiator
*        approver_id     = iv_approvers
          doc_key         = |{ is_doc_key-/ba1/c55year }\|{ is_doc_key-/ba1/c55lgent }\|{ is_doc_key-/ba1/c55docnum }|
          status          = COND char10( WHEN iv_released = 'X' THEN 'Released' ELSE 'Rejected' ) ).
    ELSE.
      lo_email_api = cl_smtg_email_api=>get_instance( iv_template_id = 'ZMJE_EML_TPL_SUBMIT' ).
      ls_eml_param = VALUE z_c_mje_eml_tpl(
*        type    = 'DEFAULT'
          initiator_id    = iv_initiator
*        approver_id     = iv_approvers
          doc_key         = |{ is_doc_key-/ba1/c55year }\|{ is_doc_key-/ba1/c55lgent }\|{ is_doc_key-/ba1/c55docnum }| ).
    ENDIF.
    DATA(lr_eml_param) = REF #( ls_eml_param ).
    lo_email_api->render_bcs_w_data(
    EXPORTING
        io_bcs      = lo_bcs
        ir_data     = lr_eml_param
        iv_language = 'E'
     ).

***  Add sender and receiver detail based on the template of choice **
    DATA(lo_sender) = cl_sapuser_bcs=>create( sy-uname ).
    lo_bcs->set_sender( i_sender = lo_sender ).

    " Set Email Receiver(s)
    DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( 'yan.tu@adweko.com' ).
    lo_bcs->add_recipient( EXPORTING i_recipient = lo_recipient ).
**********************************************************************

    " Send Email
    lo_bcs->send( ).
    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.
