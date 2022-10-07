*&---------------------------------------------------------------------*
*& Report Z_MJE_USER_UPLOAD
*&---------------------------------------------------------------------*
*& Program to handle Manual Journal Entries for FPSL
*& Provides upload of Excel file
*&
*& 2022/09/01 - Yan Tu - Adweko
*&
*& Change History:
*&
*& 2099/01/01 - sample change
*&
*&---------------------------------------------------------------------*

REPORT z_mje_user_upload.


TYPES:
  tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY,

  BEGIN OF ts_const_vals,
    field TYPE /ba1/f0_dte_characteristic,
    value TYPE string,
  END OF ts_const_vals,

  tt_const_vals TYPE STANDARD TABLE OF ts_const_vals WITH DEFAULT KEY.

CONSTANTS:
  g_log_obj        TYPE balobj_d                VALUE 'ZFPSL',
  g_log_subobj     TYPE balsubobj               VALUE 'ZFPSL_MJE',
  g_file_field_idx TYPE i                       VALUE 3.


*Global tables initialization
DATA(gt_log_flds) = VALUE tt_string(
       ( CONV #( '/BA1/C55DOCNUM'   ) )
       ( CONV #( '/BA1/C55DOCITM'   ) )
       ( CONV #( '/BA1/C55DCHTXT'   ) )
       ( CONV #( '/BA1/C55LGENT'    ) )      ).

DATA:
  lo_mje_helper     TYPE REF TO zcl_mje_util_helper,
  lv_ts             TYPE timestampl,
  lv_batch_passed   TYPE boolean,
  lv_bad_amt_format TYPE boolean,
  lv_missing_fld    TYPE boolean,
  lv_per_open       TYPE boolean,
  lv_valid_bt_src   TYPE boolean,
  lv_drcr_violated  TYPE boolean,
  lv_log_handle     TYPE balloghndl,
  lv_headerxstring  TYPE xstring,
  lv_filelength     TYPE i,
  lv_filename       TYPE rlgrap-filename,
  lv_row_log        TYPE string,
  lv_formatted_curr TYPE /ba1/bf_dte_amount_objcurr,
  lt_records        TYPE solix_tab,
  lt_headers        TYPE tt_string,
  ls_mje_docs       TYPE /ba1/hfppd,
  lt_mje_docs       TYPE ztt_mje_post_docs,

  lo_excel_ref      TYPE REF TO cl_fdt_xl_spreadsheet.

FIELD-SYMBOLS:
  <gt_xsl_data>  TYPE STANDARD TABLE,
  <str_mje_doc>  TYPE /ba1/hfppd,
  <str_data_row> TYPE any.

***************************************************************
* Screen definition
***************************************************************
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE localfile OBLIGATORY,
              p_ug   TYPE z_dte_mje_grp_ind AS LISTBOX OBLIGATORY VISIBLE LENGTH 15.
SELECTION-SCREEN END OF BLOCK block1.

***************************************************************
* Screen control interaction
***************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      static    = 'X'
    CHANGING
      file_name = p_file.

***************************************************************
* Screen Initialization
***************************************************************
INITIALIZATION.
  p_file = TEXT-003.

***************************************************************
* Main Program
***************************************************************
START-OF-SELECTION.

  lo_mje_helper = zcl_mje_util_helper=>get_instance( ).
  lv_log_handle = lo_mje_helper->get_loghandle( iv_log_obj = g_log_obj iv_log_sobj = g_log_subobj ).

*  Initialization after screen selection
  MESSAGE s001(zfpsl_mje) WITH sy-datum sy-uzeit .
  lo_mje_helper->msg_add( lv_log_handle ).

****************** File upload *************************************
  lv_filename = p_file.

*-- Read the Input File
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = CONV string( lv_filename )
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_filelength
      header                  = lv_headerxstring
    TABLES
      data_tab                = lt_records
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).
    lo_mje_helper->msg_display( iv_loghandle = lv_log_handle ).
    lo_mje_helper->msg_save( iv_loghandle = lv_log_handle iv_exit = abap_true ).
  ENDIF.
*  convert binary data to xstring
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_filelength
    IMPORTING
      buffer       = lv_headerxstring
    TABLES
      binary_tab   = lt_records
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).
    lo_mje_helper->msg_display( iv_loghandle = lv_log_handle ).
    lo_mje_helper->msg_save( iv_loghandle = lv_log_handle iv_exit = abap_true ).
  ENDIF.

  TRY .
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
                              document_name = CONV string( lv_filename )
                              xdocument     = lv_headerxstring ) .
    CATCH cx_fdt_excel_core INTO DATA(lx_excel_error).
      MESSAGE s501(zfpsl_mje) WITH lx_excel_error->get_text( ).
      lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).
      lo_mje_helper->msg_display( iv_loghandle = lv_log_handle ).
      lo_mje_helper->msg_save( iv_loghandle = lv_log_handle iv_exit = abap_true ).
  ENDTRY .

  IF lo_excel_ref IS BOUND.
*    Get List of Worksheets
    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheets) ).

    IF NOT lt_worksheets IS INITIAL.
*      Get the LAST worksheet which contains data of upload journals
      READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX lines( lt_worksheets ).
      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                               lv_woksheetname ).
      IF lo_data_ref IS NOT BOUND.
        MESSAGE s502(zfpsl_mje).
        lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).
        lo_mje_helper->msg_display( iv_loghandle = lv_log_handle ).
        lo_mje_helper->msg_save( iv_loghandle = lv_log_handle iv_exit = abap_true ).
        RETURN.
      ENDIF.

      ASSIGN lo_data_ref->* TO <gt_xsl_data>.

*      Load journal entry field names
      READ TABLE <gt_xsl_data> INDEX g_file_field_idx ASSIGNING FIELD-SYMBOL(<str_upl_data>).
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <str_upl_data> TO FIELD-SYMBOL(<val_hdr>).
*        Exit when no more headers are available
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        APPEND <val_hdr> TO lt_headers.
      ENDDO.

*      Delete header rows
      DO g_file_field_idx TIMES.
        DELETE <gt_xsl_data> INDEX 1.
      ENDDO.
    ENDIF.
  ENDIF.

*** MJE upload Preparation *********************************************
  ASSIGN ls_mje_docs TO <str_mje_doc>  .
*  Read MJE upload control table
  DATA(lt_mje_ctrl) = lo_mje_helper->get_mje_upload_control( ).
  IF lt_mje_ctrl IS INITIAL.
    lo_mje_helper->msg_display( iv_loghandle = lv_log_handle ).
    lo_mje_helper->msg_save( iv_loghandle = lv_log_handle iv_exit = abap_true ).
    RETURN.
  ENDIF.
************************************************************************


**************** Table load logic **************************************
  GET TIME STAMP FIELD lv_ts.
  lv_batch_passed = abap_true.
  LOOP AT <gt_xsl_data> ASSIGNING <str_data_row>.

    CLEAR:  <str_mje_doc>, lv_bad_amt_format, lv_formatted_curr,lv_missing_fld,
            lv_per_open, lv_valid_bt_src, lv_drcr_violated, lv_row_log.
    LOOP AT lt_headers INTO DATA(lv_hdr_val).
      ASSIGN COMPONENT lv_hdr_val OF STRUCTURE <str_mje_doc> TO FIELD-SYMBOL(<val_fld>).
*      If field doesn't exist in target structure, log error and exit.
      IF sy-subrc <> 0.
        IF lv_hdr_val IS INITIAL OR lv_hdr_val CO ' '.
          MESSAGE s502(zfpsl_mje).
        ELSE.
          MESSAGE s503(zfpsl_mje) WITH lv_hdr_val.
        ENDIF.
        lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).
        lv_batch_passed = abap_false.
      ENDIF.

*      Get corresponding field value from data row
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <str_data_row> TO FIELD-SYMBOL(<val_data>).
      IF sy-subrc = 0.

*      Data type validation - Date, Amounts (excl. currency field), numeric char
        DATA(lv_dv_passed) = lo_mje_helper->validate_data_type( iv_header = lv_hdr_val iv_field = <val_fld> iv_value = <val_data> ).
        lv_batch_passed = COND #( WHEN lv_dv_passed = abap_true AND lv_dv_passed = abap_true THEN abap_true ELSE abap_false ).
        IF lv_dv_passed = abap_true.
          <val_fld> = <val_data>.
        ENDIF.
      ELSE.
        MESSAGE s504(zfpsl_mje) WITH lv_hdr_val.
        lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).
        lv_batch_passed = abap_false.
      ENDIF.
    ENDLOOP.

***    Skip empty input lines
    IF <str_mje_doc> IS INITIAL.
      CONTINUE.
    ENDIF.
************************************************************************

************** Validation defined by MJE Control Table *****************
*    Create a document number / item number string for use in log messages.
    DATA(lv_doc_id) = |{ text-002 } { <str_mje_doc>-/ba1/c55docnum }/{ <str_mje_doc>-/ba1/c55docitm }|.
    DATA(lv_val_passed) = abap_true.

** Mandatory field check **
    lv_batch_passed = COND #( WHEN lo_mje_helper->check_mandatory_field(
        iv_ug               = p_ug
        it_mje_ctrl         = lt_mje_ctrl
        is_mje_doc          = <str_mje_doc>
        iv_msg_doc_id       = lv_doc_id
        iv_loghandle        = lv_log_handle ) = abap_true AND lv_batch_passed = abap_true THEN abap_true ELSE abap_false ).

** Master data check     **
    lv_batch_passed = COND #( WHEN lo_mje_helper->check_md(
        it_mje_ctrl         = lt_mje_ctrl
        is_mje_doc          = <str_mje_doc>
        iv_msg_doc_id       = lv_doc_id
        iv_loghandle        = lv_log_handle ) = abap_true AND lv_batch_passed = abap_true THEN abap_true ELSE abap_false ).

** Ad-hoc check          **
    lv_batch_passed = COND #( WHEN lo_mje_helper->check_adhoc(
        it_mje_ctrl         = lt_mje_ctrl
        is_mje_doc          = <str_mje_doc>
        iv_msg_doc_id       = lv_doc_id
        iv_loghandle        = lv_log_handle ) = abap_true AND lv_batch_passed = abap_true THEN abap_true ELSE abap_false ).


*************** Perform FPSL enrichment ********************************
* Assign technical fields
    lo_mje_helper->enrich_technical_fields(
        EXPORTING
            iv_offset   = zcl_mje_util_helper=>g_doc_offset
            iv_ts       = lv_ts
        CHANGING
            is_mje_doc  = <str_mje_doc> ).

*    G/L derivation
    lv_batch_passed = COND #( WHEN lo_mje_helper->enrich_fpsl_gl_derivation(
        CHANGING
            is_mje_doc  = <str_mje_doc> ) = abap_true AND lv_batch_passed = abap_true THEN abap_true ELSE abap_false ).

************************************************************************

*    Add processed line to internal table and log entry for rows added
    INSERT <str_mje_doc> INTO TABLE lt_mje_docs.
    PERFORM trap_log_fields USING <str_mje_doc> CHANGING lv_row_log.
    MESSAGE s002(zfpsl_mje) WITH lv_row_log.
    lo_mje_helper->msg_add( lv_log_handle ).
  ENDLOOP.

***************** Move & Transform *************************************
*  lv_batch_passed = COND #( WHEN lo_mje_helper->enrich_fpsl_mnt(
*      CHANGING
*          it_mje_doc          = lt_mje_docs
*          iv_loghandle        = lv_log_handle ) = abap_true AND lv_batch_passed = abap_true THEN abap_true ELSE abap_false ).

*************** JE amount validation ***********************************
  lv_batch_passed = COND #( WHEN lo_mje_helper->check_je_amount(
        it_mje_doc          = lt_mje_docs
        iv_loghandle        = lv_log_handle ) = abap_true AND lv_batch_passed = abap_true THEN abap_true ELSE abap_false ).
************************************************************************

*************** Document offsetting ************************************
  IF zcl_mje_util_helper=>g_doc_offset = abap_true.
    lo_mje_helper->offset_document(
        CHANGING
          it_mje_doc = lt_mje_docs   ).
  ENDIF.
************************************************************************

*    If any validation error occurred, failed the batch and exit
  IF lv_batch_passed = abap_false.
    MESSAGE s500(zfpsl_mje).
    lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).
  ELSE.
*    Export result to memory
    PERFORM export_upload_table USING lt_mje_docs CHANGING lv_batch_passed.
  ENDIF.

*  Finally the log display and save - If the memory export still fails, exit to calling transaction
  lo_mje_helper->msg_display( lv_log_handle ).
  lo_mje_helper->msg_save( iv_loghandle = lv_log_handle iv_exit = abap_true ).
  IF lv_batch_passed = abap_false.
    LEAVE TO CURRENT TRANSACTION.
  ENDIF.

********* End of user upload program **************




***********************************************************************
*--------------------------------------------------------------------
* FORM trap_log_fields
*--------------------------------------------------------------------

FORM trap_log_fields  USING     VALUE(is_tgt_struc)         TYPE any
                      CHANGING  iv_row_log                  TYPE string.

  LOOP AT gt_log_flds ASSIGNING FIELD-SYMBOL(<val_log_flds>).
    ASSIGN COMPONENT <val_log_flds> OF STRUCTURE is_tgt_struc TO FIELD-SYMBOL(<val_log>).
    CONCATENATE iv_row_log '|' <val_log> INTO iv_row_log.
  ENDLOOP.
  SHIFT iv_row_log LEFT IN CHARACTER MODE BY 1 PLACES .
ENDFORM.

***********************************************************************
*--------------------------------------------------------------------
* FORM export_upload_table
*--------------------------------------------------------------------

FORM export_upload_table    USING       it_upload           TYPE SORTED TABLE
                            CHANGING    iv_passed            TYPE boolean.

  DATA lv_mem_id TYPE c LENGTH 20 .
  lv_mem_id =   |MJE_{ sy-uname }|.

  TRY.
      FREE MEMORY ID lv_mem_id.
      EXPORT table = it_upload TO MEMORY ID lv_mem_id.

    CATCH cx_root INTO DATA(lx_error).
      lo_mje_helper->exception_add( lx_error ).
      MESSAGE s510(zfpsl_mje).
      lo_mje_helper->msg_add( iv_type = 'E' iv_loghandle = lv_log_handle ).

      iv_passed = abap_false.
  ENDTRY.
ENDFORM.
