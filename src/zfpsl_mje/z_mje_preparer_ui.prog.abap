*&---------------------------------------------------------------------*
*& Report Z_MJE_PREPARER_UI
*&---------------------------------------------------------------------*
*& Program to handle Manual Journal Entries for FPSL
*& Provides user interface to prepare data
*&
*& 2022/09/01 - Yan Tu - Adweko
*&
*& Change History:
*&
*& 2099/01/01 - sample change
*&
*&---------------------------------------------------------------------*

REPORT z_mje_preparer_ui.

DATA: g_prep_ui_proc     TYPE REF TO zcl_mje_preparer_ui_process,
      g_grid_initialized TYPE char1,
      ok_code            TYPE c.

CONSTANTS gc_accrt TYPE /ba1/br_dte_rdl_res VALUE '905'.

PARAMETERS:
  p_accrt TYPE /ba1/tbr_res_sto-rdl_result OBLIGATORY DEFAULT '010'.

INITIALIZATION.

  " make authorization check on TCODE
  AUTHORITY-CHECK OBJECT 'S_TCODE'
           ID 'TCD' FIELD 'ZFPSL_E001_MJE'.
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
  zcl_mje_preparer_ui_process=>get_instance(
                                         EXPORTING p_accrt = gc_accrt "p_accrt
                                         IMPORTING p_prep_ui_proc = g_prep_ui_proc
                                       ).
  IF g_prep_ui_proc IS NOT INITIAL.
    g_prep_ui_proc->init_show_grid( ).
    SET PF-STATUS 'MAIN1001'.
    TRY.
        CALL SCREEN '1001' .
      CATCH cx_root INTO DATA(l_cx_root).
*       MESSAGE l_cx_root TYPE 'I'.
        MESSAGE TEXT-e01 TYPE 'I'.
        g_prep_ui_proc->exit( ). " to clear the screen
        CALL SCREEN '1001' .
    ENDTRY.
  ENDIF.

*start-of-selection.


*module pbo_1001 output.
*
*endmodule.

MODULE pai_1001 INPUT.
  CASE sy-ucomm.
    WHEN 'IMPORT'.
      g_prep_ui_proc->import_upload( ).
    WHEN 'EXECUTE'.
      g_prep_ui_proc->save_mje( ).
    WHEN 'SWITCH_NAM'.
      g_prep_ui_proc->switch_grid_names( ).
    WHEN 'ATTACHMENT'.
      g_prep_ui_proc->create_attachment( ).
    WHEN 'VIEW'.
      g_prep_ui_proc->view_attachment( ).
    WHEN 'DELETE'.
      g_prep_ui_proc->delete_record( ).
    WHEN 'BACK'.
      g_prep_ui_proc->exit( ).
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*     do nothing
  ENDCASE.

ENDMODULE.
