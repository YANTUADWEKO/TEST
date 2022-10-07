*&---------------------------------------------------------------------*
*& Report Z_MJE_RELEASE_UI
*&---------------------------------------------------------------------*
*& Program to handle Manual Journal Entries for FPSL
*& Mass release interface for postings
*&
*& 2022/09/01 - Yan Tu - Adweko
*&
*& Change History:
*&
*& 2099/01/01 - sample change
*&
*&---------------------------------------------------------------------*
REPORT z_mje_release_ui.

DATA: g_rel_ui_proc   TYPE REF TO zcl_mje_release_ui_process,
      g_grid_initialized TYPE char1,
      ok_code            TYPE c.

CONSTANTS gc_accrt TYPE /ba1/br_dte_rdl_res VALUE '905'.

PARAMETERS:
  p_accrt TYPE /ba1/tbr_res_sto-rdl_result OBLIGATORY DEFAULT '010'.

INITIALIZATION.

  " make authorization check on TCODE
  AUTHORITY-CHECK OBJECT 'S_TCODE'
           ID 'TCD' FIELD 'ZFPSL_E003_MJE'.
  IF sy-subrc <> 0.
    LEAVE PROGRAM.
  ENDIF.

  DATA: lsto TYPE /ba1/tbr_res_sto.
  SELECT SINGLE * FROM /ba1/tbr_res_sto INTO lsto WHERE rdl_result = gc_accrt. "p_accrt
  IF sy-subrc <> 0.
    LEAVE PROGRAM.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'F_BAHW_RES'
           ID 'ACTVT'      FIELD '01'
           ID '/BA1/HWRDA' FIELD lsto-area
           ID '/BA1/HWRT'  FIELD lsto-type
           ID '/BA1/F0CH1' DUMMY
           ID '/BA1/F0CH2' DUMMY
           ID '/BA1/F0CH3' DUMMY
           ID '/BA1/F0CH4' DUMMY
           ID '/BA1/F0CH5' DUMMY
           ID '/BA1/F0CH6' DUMMY
           ID '/BA1/F0CH7' DUMMY.
  IF sy-subrc <> 0.
    LEAVE PROGRAM.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  zcl_mje_release_ui_process=>get_instance(
                                         EXPORTING p_accrt = gc_accrt "p_accrt
                                         IMPORTING p_rel_ui_proc = g_rel_ui_proc
                                        ).
  IF g_rel_ui_proc IS NOT INITIAL.
    g_rel_ui_proc->init_show_grid( ).
    SET PF-STATUS 'MAIN1001'.
    TRY.
        CALL SCREEN '1001' .
      CATCH cx_root INTO DATA(l_cx_root).
*       MESSAGE l_cx_root TYPE 'I'.
        MESSAGE TEXT-e01 TYPE 'I'.
        g_rel_ui_proc->exit( ). " to clear the screen
        CALL SCREEN '1001' .
    ENDTRY.
  ENDIF.

*start-of-selection.

*todo: ask if it makes sense to move show_ddic_overview somewhere
FORM show_ddic_overview.
  TYPES: BEGIN OF t1,
           area            TYPE /ba1/hm_dte_area,
           type            TYPE /ba1/hm_dte_type,
           result_storage  TYPE /ba1/br_dte_rdl_res,
           crccfcatg       TYPE  /ba1/hk_dte_cf_category,
           strucname       TYPE tabname,
           result_category TYPE /ba1/hm_dte_result_category,
           pers_schema     TYPE /ba1/hm_dte_pers_schema,
           version_schema  TYPE /ba1/hw_dte_version_schema,
           cluster_char    TYPE /ba1/f0_dte_characteristic,
           description     TYPE /ba1/hx_dte_description,
           strucname_rdl   TYPE tabname,
           tabname_rdl     TYPE tabname,
         END OF t1.

  DATA:
    l_tab     TYPE STANDARD TABLE OF t1,
    lt_rdlres TYPE STANDARD TABLE OF /ba1/tbr_res_sto.

  FIELD-SYMBOLS:
  <tab> TYPE ANY TABLE.

*initialization.
  TRY.
      /ba1/cl_in_hx_helper=>s_get_instance( )->get_supported_result_types(
        IMPORTING
          e_tas_supported_result_type = DATA(e_tas_supp) ).
    CATCH /ba1/cx_hx_static_check.
      LEAVE PROGRAM.
  ENDTRY.

  SELECT * FROM /ba1/tbr_res_sto INTO TABLE @lt_rdlres.
  IF sy-subrc <> 0.
    LEAVE PROGRAM.
  ENDIF.

  LOOP AT e_tas_supp ASSIGNING FIELD-SYMBOL(<fs>).
    LOOP AT <fs>-tab_supp_cluster ASSIGNING FIELD-SYMBOL(<clu>).
      APPEND INITIAL LINE TO l_tab ASSIGNING FIELD-SYMBOL(<new>).
      MOVE-CORRESPONDING <fs> TO <new>.
      MOVE-CORRESPONDING <clu> TO <new>.
    ENDLOOP.
  ENDLOOP.

  LOOP AT l_tab ASSIGNING <new>.
    READ TABLE lt_rdlres WITH KEY
      rdl_result = <new>-result_storage
      area       = <new>-area
      type       = <new>-type
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DELETE l_tab.
    ENDIF.
  ENDLOOP.

  SORT l_tab BY result_storage.

  GET REFERENCE OF l_tab INTO DATA(lref).

  CALL FUNCTION '/BA1/FF_API_ALV_DISP'
    EXPORTING
      i_rda_data = lref
    EXCEPTIONS
      OTHERS     = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.


*module pbo_1001 output.
*
*endmodule.

MODULE pai_1001 INPUT.
  CASE sy-ucomm.
    WHEN 'LOAD_MJE'.
      g_rel_ui_proc->load_mje( ).
    WHEN 'SWITCH_NAM'.
      g_rel_ui_proc->switch_grid_names( ).
    WHEN 'ATTACHMENT'.
      g_rel_ui_proc->create_attachment( ).
    WHEN 'VIEW'.
      g_rel_ui_proc->view_attachment( ).
    WHEN 'RELEASE'.
      g_rel_ui_proc->release( ).
    WHEN 'REJECT'.
      g_rel_ui_proc->reject( ).
    WHEN 'BACK'.
      g_rel_ui_proc->exit( ).
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*     do nothing
  ENDCASE.

ENDMODULE.
