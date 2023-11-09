CLASS zcl_mje_preparer_ui_process DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
                 g_upload_prog_tcode  TYPE programm VALUE 'Z_MJE_JOURNAL_UPLOAD'.

    CLASS-METHODS get_instance
      IMPORTING
        !p_accrt        TYPE /ba1/tbr_res_sto-rdl_result
      EXPORTING
        !p_prep_ui_proc TYPE REF TO zcl_mje_preparer_ui_process .
    METHODS init_show_grid .
    METHODS save_mje .
    METHODS import_upload .
    METHODS switch_grid_names .
    METHODS create_attachment .
    METHODS view_attachment .
    METHODS delete_record .
    METHODS exit .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF gs_changed_cell,
        row_id    TYPE int4,
        fieldname TYPE lvc_fname,
        value     TYPE lvc_value,
        error     TYPE char1,
      END OF gs_changed_cell .
    TYPES:
      gs_changed_cells TYPE STANDARD TABLE OF gs_changed_cell .

    DATA g_accrt TYPE /ba1/tbr_res_sto-rdl_result .
    DATA g_grid TYPE REF TO cl_gui_alv_grid .
    DATA g_changed_cells TYPE gs_changed_cells .
    DATA g_last_rowid TYPE i .
    DATA:
      g_map_grid_change_tab TYPE STANDARD TABLE OF int4 .
    DATA g_ref_factory TYPE REF TO /ba1/if_al_hx_factory .
    DATA g_ref_cx_static TYPE REF TO /ba1/cx_hx_static_check .
    DATA g_area TYPE /ba1/hm_dte_area .
    DATA g_type TYPE /ba1/hm_dte_type .
    DATA g_strucname TYPE seocpdname .
    DATA g_versioning_schema TYPE /ba1/hw_dte_version_schema .
    DATA g_grid_tab TYPE REF TO data .
    DATA g_master_fieldcat TYPE lvc_t_fcat .
    DATA g_is_tech_names TYPE char1 .

    DATA pr_mje_helper TYPE REF TO zcl_mje_util_helper.

    METHODS handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        !er_data_changed .
    METHODS set_grid_structure
      IMPORTING
        !p_accrt TYPE /ba1/tbr_res_sto-rdl_result
      RAISING
        /ba1/cx_al_br_customizing
        /ba1/cx_hx_static_check .
    METHODS populate_grid_tab_with_changes
      IMPORTING
        !p_changed_cells TYPE gs_changed_cells .
    METHODS get_fields
      IMPORTING
        !p_area          TYPE /ba1/hm_dte_area
        !p_view          TYPE /ba1/hm_dte_view
      CHANGING
        !p_tab_char      TYPE /ba1/f0_tab_char_free_sel
        !p_tab_keyf_unit TYPE /ba1/f0_tab_keyf_unit_free_sel
      RAISING
        /ba1/cx_hx_static_check .
    METHODS refresh_grid .
    METHODS show_ret_table
      IMPORTING
        !p_tab_msg TYPE bapirettab .
    METHODS prepare_master_fieldcat .
    METHODS convert_value_to_date
      IMPORTING
        !p_value TYPE lvc_value
      EXPORTING
        !p_date  TYPE d .
    METHODS convert_value_to_number
      IMPORTING
        !p_value  TYPE lvc_value
      EXPORTING
        !p_number TYPE any .
    METHODS convert_ext_to_int_cell
      CHANGING
        !cs_cell_modified TYPE lvc_s_modi .

    METHODS import_upload_table
      EXPORTING it_upload TYPE SORTED TABLE .
ENDCLASS.



CLASS ZCL_MJE_PREPARER_UI_PROCESS IMPLEMENTATION.


  METHOD convert_ext_to_int_cell.

    DATA:
      l_component        TYPE char30,
      l_fieldcatalog     TYPE lvc_t_fcat,
      l_changed_cells    TYPE gs_changed_cells,
      l_changed_cell     TYPE gs_changed_cell,
      l_value            TYPE lvc_value,
      l_date             TYPE d,
      l_str_detail_small TYPE /ba1/f0_str_char_detail_small,
      l_func_name        TYPE char30,
      l_ref              TYPE REF TO data,
      l_im_currency_code TYPE tcurc-isocd,
      l_ex_currency_code TYPE tcurc-waers,
      l_ref_gdt          TYPE REF TO cx_gdt_conversion,
      l_tcurx            TYPE tcurx,
      l_amnt             TYPE /ba1/fu_dte_amnt,
      l_amntf            TYPE /ba1/fu_dte_famnt.

    FIELD-SYMBOLS:
      <tab>        TYPE STANDARD TABLE,
      <fs_value>   TYPE any,
      <unit_value> TYPE any,
      <kf_field>   TYPE any.

    DATA:
      l_str_details         TYPE  /ba1/ff_str_details,
      l_structtype          TYPE  /ba1/ff_dte_string,
      l_tab_units_not_found TYPE  /ba1/ff_tab_technames,
      l_tab_fields          TYPE  /ba1/ff_tab_fieldprop,
      l_tab_return          TYPE  bapirettab.
    CALL FUNCTION '/BA1/FF_API_F_STRUCT_DET_GET'
      EXPORTING
        i_rda_fields          = g_grid_tab
      IMPORTING
        e_str_details         = l_str_details
        e_structtype          = l_structtype
        e_tab_units_not_found = l_tab_units_not_found
        e_tab_fields          = l_tab_fields
        e_tab_return          = l_tab_return
      EXCEPTIONS
        get_fieldnames_failed = 1
        get_technames_failed  = 2
        get_char_det_failed   = 3
        get_kf_det_failed     = 4
        get_unit_det_failed   = 5
        get_field_det_failed  = 6
        get_ddic_info_failed  = 7
        OTHERS                = 8.
    IF sy-subrc <> 0.
      " ignore errors
      "MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    SORT l_str_details-char_detail BY fieldname ASCENDING.
    SORT l_tab_fields BY fieldname ASCENDING.

    g_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = l_fieldcatalog ).

    DATA lt_fieldcatalog_sorted LIKE l_fieldcatalog.
    lt_fieldcatalog_sorted  = l_fieldcatalog.
    SORT lt_fieldcatalog_sorted BY fieldname ASCENDING.

    ASSIGN g_grid_tab->* TO <tab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE <tab> INDEX cs_cell_modified-row_id ASSIGNING FIELD-SYMBOL(<wrk_data>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR l_changed_cells.

    LOOP AT l_fieldcatalog ASSIGNING FIELD-SYMBOL(<catalog>) WHERE fieldname = cs_cell_modified-fieldname.
*    LOOP AT l_fieldcatalog ASSIGNING FIELD-SYMBOL(<catalog>) WHERE no_out IS INITIAL.

      IF <fs_value> IS ASSIGNED.
        UNASSIGN <fs_value>.
      ENDIF.

      ASSIGN COMPONENT <catalog>-fieldname OF STRUCTURE <wrk_data> TO FIELD-SYMBOL(<fs_value_mod>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN cs_cell_modified-value TO <fs_value>.

      IF <fs_value> IS INITIAL.
        CONTINUE.
      ENDIF.

      l_component = <catalog>-fieldname.

      l_changed_cell-fieldname = l_component.
      l_changed_cell-value     = <fs_value>.
      l_changed_cell-error     = ''.

      CLEAR l_str_detail_small.
      READ TABLE l_tab_fields ASSIGNING FIELD-SYMBOL(<fields>)
        WITH KEY fieldname = l_component BINARY SEARCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      CASE <fields>-type.
        WHEN 'C'.
          READ TABLE l_str_details-char_detail ASSIGNING FIELD-SYMBOL(<char>) WITH KEY fieldname = l_component BINARY SEARCH.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          MOVE-CORRESPONDING <char> TO l_str_detail_small.
          IF <char>-lowercase IS INITIAL.
            TRANSLATE <fs_value> TO UPPER CASE.
            TRANSLATE l_changed_cell-value TO UPPER CASE.
          ENDIF.
        WHEN 'K'.
          convert_value_to_number( EXPORTING p_value  = <fs_value> IMPORTING p_number = <fs_value> ).
          READ TABLE l_str_details-kf_detail ASSIGNING FIELD-SYMBOL(<keyf>) WITH KEY fieldname = l_component .
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          MOVE-CORRESPONDING <keyf> TO l_str_detail_small.
          l_str_detail_small-data_element = <keyf>-dtename.
          l_str_detail_small-datatype     = <keyf>-dtype.

          CASE <keyf>-type.
            WHEN '01'.
              IF <kf_field> IS ASSIGNED.
                UNASSIGN <kf_field>.
              ENDIF.

              IF NOT  <keyf>-fn_unit IS INITIAL.
                IF <unit_value> IS ASSIGNED.
                  UNASSIGN <unit_value>.
                ENDIF.
                ASSIGN COMPONENT <keyf>-fn_unit OF STRUCTURE <wrk_data> TO <unit_value>.
                IF sy-subrc = 0.
                  l_im_currency_code = <unit_value>.
                ENDIF.
              ENDIF.

              IF <keyf>-dtype = 'CURR' AND NOT ( <keyf>-kf_category CS 'LAM' OR <keyf>-kf_category CS 'LDEC' ).
                ASSIGN l_amnt TO <kf_field>. " empty
                DATA: lv_amount TYPE /ba1/fx_dte_amount.
                lv_amount = <fs_value>.
                IF NOT ( lv_amount IS INITIAL AND l_im_currency_code IS INITIAL ).
                  TRY.
                      CALL METHOD cl_gdt_conversion=>amount_inbound
                        EXPORTING
                          im_value         = lv_amount
                          im_currency_code = l_im_currency_code
                        IMPORTING
                          ex_value         = <kf_field>
                          ex_currency_code = l_ex_currency_code.
                    CATCH cx_gdt_conversion INTO l_ref_gdt.
                      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                      MESSAGE i028(/ba1/in_hx) WITH |{ lv_amount }/{ l_im_currency_code }| l_component '-' '-' .
                      RETURN.
                  ENDTRY.
                  l_changed_cell-value = <kf_field>.
                ENDIF.
              ELSE.
                ASSIGN l_amntf TO <kf_field>.
                SELECT SINGLE * FROM tcurx INTO l_tcurx
                  WHERE currkey = l_im_currency_code.
                IF sy-subrc NE 0.
                  <kf_field> = <fs_value>.
                ELSE.
                  "<kf_field> = <fs_value> * 10 ** ( l_tcurx-currdec - 2 ).
                  " avoid implicit conversion to type f
                  <kf_field> = <fs_value>.
                  DO l_tcurx-currdec - 2 TIMES.
                    <kf_field> = <kf_field> * 10.
                  ENDDO.
                ENDIF.
                DATA lr_number TYPE REF TO data.
                CREATE DATA lr_number TYPE (<keyf>-dtename).
                ASSIGN lr_number->* TO FIELD-SYMBOL(<kf1>).
                <kf1> = <kf_field>.
                l_changed_cell-value = <kf1>.
              ENDIF.

            WHEN '02'.
              IF <keyf>-dtype = 'FLTP'.
                l_changed_cell-value = <fs_value>.
              ELSE.
                l_changed_cell-value = <fs_value>. "percentage
              ENDIF.
            WHEN '03'.
              l_changed_cell-value = <fs_value>. "quantity
              "unit     = quantity-unit_code.
            WHEN '04'.
              l_changed_cell-value = <fs_value>.  "integer_value
          ENDCASE.

        WHEN 'U'.
          READ TABLE l_str_details-unit_detail ASSIGNING FIELD-SYMBOL(<unit>) WITH KEY fn_unit = l_component .
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          l_str_detail_small-data_element = <unit>-dtename_unit.
          l_str_detail_small-datatype     = <unit>-dtype_unit.
          TRANSLATE <fs_value> TO UPPER CASE.
          l_changed_cell-value = <fs_value>.
        WHEN 'D'.
          READ TABLE l_str_details-ddic_detail ASSIGNING FIELD-SYMBOL(<ddic>) WITH KEY fieldname = l_component .
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          MOVE-CORRESPONDING <ddic> TO l_str_detail_small.
          l_str_detail_small-data_element = <ddic>-rollname.
          IF <ddic>-lowercase IS INITIAL.
            TRANSLATE <fs_value> TO UPPER CASE.
          ENDIF.
          l_changed_cell-value = <fs_value>.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      IF NOT l_str_detail_small-convexit IS INITIAL.
        CONCATENATE 'CONVERSION_EXIT_' l_str_detail_small-convexit '_INPUT' INTO l_func_name.
        CONDENSE l_func_name NO-GAPS.
        CREATE DATA l_ref TYPE (l_str_detail_small-data_element).
        ASSIGN l_ref->* TO FIELD-SYMBOL(<value_cexit>).
        <value_cexit> = <fs_value>.
        IF NOT <value_cexit> IS INITIAL.
          CALL FUNCTION l_func_name
            EXPORTING
              input  = <fs_value>
            IMPORTING
              output = <value_cexit>
            EXCEPTIONS
              OTHERS = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MESSAGE i028(/ba1/in_hx) WITH <fs_value> l_component '-' '-' .
            RETURN.
          ENDIF.
          l_changed_cell-value = <value_cexit>.
        ENDIF.
      ENDIF.

      IF l_str_detail_small-datatype = 'DATS'.
        l_value = <fs_value>.
        IF strlen( l_value ) > 8.
          convert_value_to_date( EXPORTING p_value = l_value IMPORTING p_date = l_date ).
          l_changed_cell-value = l_date.
        ENDIF.
      ENDIF.

      IF l_str_detail_small-datatype = 'CUKY' AND 1 = 2. " done by basis-ALV
        DATA:
          l_im_iso_code TYPE  tcurc-isocd,
          l_ex_sap_code TYPE  tcurc-waers.
        l_im_iso_code = <fs_value>.
        IF NOT l_im_iso_code IS INITIAL.
          CALL FUNCTION 'CURRENCY_CODE_ISO_TO_SAP'
            EXPORTING
              iso_code  = l_im_iso_code
            IMPORTING
              sap_code  = l_ex_sap_code
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MESSAGE i028(/ba1/in_hx) WITH l_im_iso_code l_component '-' '-' .
            RETURN.
          ENDIF.
          l_changed_cell-value = l_ex_sap_code.
        ENDIF.
      ENDIF.

      IF l_str_detail_small-datatype = 'UNIT' AND 1 = 2. " done by basis-ALV
        " see also DB-table T006 or transaction code CUNI
        DATA:
          l_im_iso_code_u TYPE t006-isocode,
          l_ex_sap_code_u TYPE  t006-msehi,
          l_ex_unique     TYPE  isofields-unique.
        l_im_iso_code_u = <fs_value>.
        IF NOT l_im_iso_code_u IS INITIAL.
          CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
            EXPORTING
              iso_code  = l_im_iso_code_u
            IMPORTING
              sap_code  = l_ex_sap_code_u
              unique    = l_ex_unique
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
            MESSAGE i028(/ba1/in_hx) WITH l_im_iso_code_u l_component '-' '-' .
            RETURN.
          ENDIF.
          l_changed_cell-value = l_ex_sap_code_u.
        ENDIF.
      ENDIF.

      " ok, no error occured
      IF cs_cell_modified-value <> l_changed_cell-value.
        cs_cell_modified-value = l_changed_cell-value.
      ENDIF.
      SHIFT cs_cell_modified-value LEFT DELETING LEADING space.

    ENDLOOP.

*  ENDLOOP.

  ENDMETHOD.


  METHOD convert_value_to_number.
    DATA: ss_usr01 TYPE usr01.
    SELECT SINGLE * FROM usr01 INTO ss_usr01 WHERE bname = sy-uname.
    DATA: l_tmp       TYPE string,
          l_thousands TYPE char01,
          l_decimal   TYPE char01.
    l_tmp = p_value.
    CASE ss_usr01-dcpfm.
      WHEN space.  " 1.234.567,89
        l_thousands = '.'.
        l_decimal   = ','.
      WHEN 'X'.    " 1,234,567.89
        l_thousands = ','.
        l_decimal   = '.'.
      WHEN 'Y'.    " 1 234 567,89
        l_thousands = space.
        l_decimal   = ','.
    ENDCASE.
* internal format 1234567.89
*    replace '.' with space into l_tmp.
*    replace ',' with '.' into l_tmp.
    REPLACE ALL OCCURRENCES OF l_thousands IN l_tmp WITH space.
    REPLACE l_decimal WITH '.' INTO l_tmp.
    CONDENSE l_tmp NO-GAPS.
    TRY.
        p_number = l_tmp.
      CATCH cx_sy_conversion_no_number. " leave it empty
    ENDTRY.
  ENDMETHOD.


  METHOD create_attachment.

    DATA: ls_row_no TYPE lvc_t_roid,
          ls_rows   TYPE lvc_t_row,
          lv_row    TYPE i,
          lv_btran  TYPE /ba1/f2_dte_bt_id.

    DATA: manager TYPE REF TO cl_gos_manager,
          obj     TYPE borident.

    CONSTANTS objtype TYPE borident-objtype VALUE 'ZBTID'.

    FIELD-SYMBOLS <fs_grid_tab> TYPE STANDARD TABLE.

*   Get the selected row number
    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_row_no     = ls_row_no
        et_index_rows = ls_rows.

    READ TABLE ls_row_no ASSIGNING FIELD-SYMBOL(<fs_row_no>) INDEX 1.
    IF sy-subrc EQ 0.
      lv_row = <fs_row_no>-row_id.
    ELSE.
      MESSAGE TEXT-e02 TYPE 'S'.
      EXIT.
    ENDIF.

*   Get the Business Transaction ID
    ASSIGN g_grid_tab->* TO <fs_grid_tab>.
    READ TABLE <fs_grid_tab> ASSIGNING FIELD-SYMBOL(<fs_grid_line>) INDEX ( lv_row ).
    IF sy-subrc EQ 0.
      CLEAR lv_btran.

      ASSIGN COMPONENT '/BA1/C35BTRAN' OF STRUCTURE <fs_grid_line> TO FIELD-SYMBOL(<fs_btran>).
      lv_btran = <fs_btran>.
    ENDIF.

*   SAP services for object
    IF NOT lv_btran IS INITIAL.
      obj-objtype = objtype.
      obj-objkey  = lv_btran.

*     Create the object of class 'CL_GOS_MANAGER'
      CREATE OBJECT manager
        EXPORTING
          is_object    = obj
          ip_no_commit = 'R'
        EXCEPTIONS
          OTHERS       = 1.

      IF sy-subrc EQ 0.
*       Open dialog box for attachment
        CALL METHOD manager->start_service_direct
          EXPORTING
            ip_service       = 'PCATTA_CREA'
            is_object        = obj
          EXCEPTIONS
            no_object        = 1
            object_invalid   = 2
            execution_failed = 3
            OTHERS           = 4.
        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

    ELSE.
      MESSAGE TEXT-e03 TYPE 'S'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD delete_record.

    DATA: ls_row_no TYPE lvc_t_roid,
          ls_rows   TYPE lvc_t_row,
          lv_row    TYPE i,
          lv_btran  TYPE /ba1/f2_dte_bt_id.

    FIELD-SYMBOLS <fs_grid_tab> TYPE STANDARD TABLE.

*   Get the selected row number
    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_row_no     = ls_row_no
        et_index_rows = ls_rows.

    READ TABLE ls_row_no ASSIGNING FIELD-SYMBOL(<fs_row_no>) INDEX 1.
    IF sy-subrc EQ 0.
      lv_row = <fs_row_no>-row_id.
    ELSE.
      MESSAGE TEXT-e02 TYPE 'S'.
      EXIT.
    ENDIF.

*   Get the Business Transaction ID
    ASSIGN g_grid_tab->* TO <fs_grid_tab>.
    READ TABLE <fs_grid_tab> ASSIGNING FIELD-SYMBOL(<fs_grid_line>) INDEX ( lv_row ).
    IF sy-subrc EQ 0.
      CLEAR lv_btran.

      ASSIGN COMPONENT '/BA1/C35BTRAN' OF STRUCTURE <fs_grid_line> TO FIELD-SYMBOL(<fs_btran>).
      lv_btran = <fs_btran>.
    ENDIF.

*   Delete records which have the same Business Transaction ID
    LOOP AT <fs_grid_tab> ASSIGNING FIELD-SYMBOL(<fs_grid_line_2>).
      ASSIGN COMPONENT '/BA1/C35BTRAN' OF STRUCTURE <fs_grid_line_2> TO FIELD-SYMBOL(<fs_btran_2>).
      IF <fs_btran_2> EQ lv_btran.
        DELETE <fs_grid_tab>.
      ENDIF.
    ENDLOOP.

*   Refresh the ALV grid
    refresh_grid( ).

**   Update table ZFPSL_MJE_LOG
*    SELECT SINGLE ernam INTO @DATA(lv_user)
*    FROM zfpsl_mje_log
*    WHERE ernam EQ @sy-uname.
*    IF sy-subrc EQ 0.
*      lv_tstmp = |{ sy-datum }| & | | & |{ sy-uzeit+0(2) }{ ':' }{ sy-uzeit+2(2) }{ ':' }{ sy-uzeit+4(2) }|.
*
*      UPDATE zfpsl_mje_log SET tstmp = lv_tstmp
*      WHERE ernam = sy-uname.
*
*      COMMIT WORK.
*    ELSE.
*      lv_tstmp = |{ sy-datum }| & | | & |{ sy-uzeit+0(2) }{ ':' }{ sy-uzeit+2(2) }{ ':' }{ sy-uzeit+4(2) }|.
*
*      INSERT zfpsl_mje_log FROM @( VALUE #( ernam = sy-uname
*                                            tstmp = lv_tstmp ) ).
*
*      COMMIT WORK.
*    ENDIF.

  ENDMETHOD.


  METHOD exit.

    CALL METHOD g_grid->free.
    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.


  METHOD handle_data_changed.
    DATA:
      l_changed_cell TYPE gs_changed_cell,
      l_mod_cell     TYPE lvc_s_modi.
    READ TABLE er_data_changed->mt_mod_cells WITH KEY error = 'X' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0. " no errors
* data to change grid is saved for another event data_change_finished to avoid note 453255
      DATA:
        l_fieldcatalog TYPE lvc_t_fcat.
      g_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = l_fieldcatalog ).
      LOOP AT er_data_changed->mt_mod_cells INTO l_mod_cell.
        IF l_mod_cell-error = ''.
          READ TABLE er_data_changed->mt_inserted_rows WITH KEY row_id = l_mod_cell-row_id TRANSPORTING NO FIELDS.
          IF sy-subrc = 0 . " it is a new row. put default values in place
            CASE l_mod_cell-fieldname.
              WHEN '/BA1/C55ACCRCT'.
                l_mod_cell-value = g_accrt.
              WHEN '/BA1/CR0RSKYDT'.
                l_mod_cell-value = sy-datum.
              WHEN '/BA1/CR0KEYDAT'.
                l_mod_cell-value = sy-datum.
            ENDCASE.
          ENDIF.

          " no sophisticated ext-2-int conversion on UI, because ALV already checks a few formats by itself
          convert_ext_to_int_cell( CHANGING cs_cell_modified = l_mod_cell ).

**       data in customers format need to be converted to internal format
*          l_datatype = l_fieldcatalog[ fieldname = l_mod_cell-fieldname ]-inttype.
*
*          CASE l_datatype.
*            WHEN 'D'.
*              convert_value_to_date( EXPORTING p_value = l_mod_cell-value
*                                     IMPORTING p_date = l_date ).
*              l_mod_cell-value = l_date.
*            WHEN 'P' OR 'F'.
*              CREATE DATA l_ref TYPE (g_strucname).
*              ASSIGN l_ref->* TO <stru>.
*              ASSIGN COMPONENT l_mod_cell-fieldname OF STRUCTURE <stru> TO <number>.
*              convert_value_to_number( EXPORTING p_value = l_mod_cell-value
*                                       IMPORTING p_number = <number> ).
*              l_mod_cell-value = <number>.
*          ENDCASE.

          MOVE-CORRESPONDING l_mod_cell TO l_changed_cell.
          APPEND l_changed_cell TO g_changed_cells.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD init_show_grid.
    FIELD-SYMBOLS:
      <fs_field>  TYPE lvc_s_fcat.
    DATA:
      l_str_layout        TYPE lvc_s_layo,
      l_fieldcat          TYPE lvc_t_fcat,
      l_variant           TYPE disvariant,
      l_col_pos           TYPE int1,
      l_struct_descr      TYPE REF TO cl_abap_structdescr,
      l_new_struct_descr  TYPE REF TO cl_abap_structdescr,
      l_struct_components TYPE cl_abap_structdescr=>component_table,
      l_table_descr       TYPE REF TO cl_abap_tabledescr,
      l_component         TYPE abap_componentdescr,
      l_toolbar_excluding TYPE ui_functions.
    FIELD-SYMBOLS:
      <fs_grid_tab> TYPE STANDARD TABLE.
    CLEAR g_master_fieldcat.
    CLEAR g_is_tech_names.

* because additional fields need to be added to l_strucname
* instead of simple "create data l_grid_tab type standard table of (l_strucname)" we do the below
    l_struct_descr ?= cl_abap_typedescr=>describe_by_name( p_name = g_strucname ).
    l_struct_components = l_struct_descr->get_components( ).
    l_component-name = 'LINECOLOR'.
    l_component-type = cl_abap_elemdescr=>get_c( 4 ).
    APPEND l_component TO l_struct_components.
    l_component-name = 'ERRCELLCOLOR'.
    l_component-type ?= cl_abap_typedescr=>describe_by_name( p_name = 'LVC_T_SCOL' ). " casting CL_ABAP_TYPEDESCR to cl_abap_datadescr
    APPEND l_component TO l_struct_components.
    l_component-name = 'ERRORCELLMESSAGE'.
    l_component-type = cl_abap_elemdescr=>get_c( 256 ).
    APPEND l_component TO l_struct_components.
    l_component-name = 'ROWID'.
    l_component-type = cl_abap_elemdescr=>get_i( ).
    APPEND l_component TO l_struct_components.

    l_new_struct_descr = cl_abap_structdescr=>create( l_struct_components ).
    l_table_descr = cl_abap_tabledescr=>create( p_line_type = l_new_struct_descr ).
    CREATE DATA g_grid_tab TYPE HANDLE l_table_descr. " g_grid_tab is to keep content of the grid
    ASSIGN g_grid_tab->* TO <fs_grid_tab>.

    CLEAR g_map_grid_change_tab.
    DATA:  l_container           TYPE REF TO cl_gui_custom_container.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = l_container.

    prepare_master_fieldcat( ).

* show non-technical columns in right order as per g_strucname
    DATA: l_field_list       TYPE ddfields,
          l_field_white_list TYPE ddfields.
    l_struct_descr ?= cl_abap_typedescr=>describe_by_name( p_name = g_strucname ).
    l_field_list = l_struct_descr->get_ddic_field_list( ).

* get list of non-technical fields
    l_struct_descr ?= cl_abap_typedescr=>describe_by_name( p_name = '/BA1/BR_STR_RDL_MC_DFT_COLS' ).
    l_field_white_list = l_struct_descr->get_ddic_field_list( ).

    l_col_pos = 0.
    LOOP AT l_field_list ASSIGNING FIELD-SYMBOL(<fs_component>).
*      READ TABLE l_field_white_list WITH KEY fieldname = <fs_component>-fieldname TRANSPORTING NO FIELDS.
*      IF sy-subrc = 0.
      READ TABLE g_master_fieldcat WITH KEY fieldname = <fs_component>-fieldname ASSIGNING <fs_field> BINARY SEARCH.
      IF sy-subrc = 0.
        l_col_pos = l_col_pos + 1.
        <fs_field>-col_pos = l_col_pos.
        APPEND <fs_field> TO l_fieldcat.
      ENDIF.
*      ENDIF.
    ENDLOOP.

* the rest columns are added hidden. however if necessary they can be added using layout editor in the grid
    LOOP AT g_master_fieldcat ASSIGNING <fs_field>.
      CASE <fs_field>-fieldname.
        WHEN '/BA1/CR0SRCRTM' OR '/BA1/CR2DLVPKG' OR '/BA1/K11PRTKY' OR '/BA1/CR2DLVPKG'.
          <fs_field>-no_out = 'X'.
      ENDCASE.
      READ TABLE l_fieldcat WITH KEY fieldname = <fs_field>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        l_col_pos = l_col_pos + 1.
        <fs_field>-col_pos = l_col_pos.
        APPEND <fs_field> TO l_fieldcat.
      ENDIF.
    ENDLOOP.
    LOOP AT l_fieldcat ASSIGNING <fs_field>.
      <fs_field>-valexi = '!'. " to avoid domain checks. the tool does all checks itself
*      <fs_field>-no_init_ch = 'A'.
    ENDLOOP.

*    Set ALV grid selection mode and DISPLAY-ONLY
*   l_str_layout-edit = 'X'.
    l_str_layout-edit = ' '.
    l_str_layout-sel_mode = 'D'.

    l_str_layout-info_fname = 'LINECOLOR'.
    l_str_layout-ctab_fname = 'ERRCELLCOLOR'.
    l_str_layout-col_opt = 'X'. " todo: doesn't seem to be working. it is supposed to optimize columns width. check CL_GUI_ALV_GRID_BASE->OPTIMIZE_ALL_COLS. it is protected method
    l_variant-report = sy-repid && '_' && g_accrt.

    APPEND g_grid->mc_fc_loc_insert_row TO l_toolbar_excluding.
    APPEND g_grid->mc_fc_loc_copy_row TO l_toolbar_excluding.
    g_last_rowid = 0.
    g_grid->set_table_for_first_display(
         EXPORTING is_layout = l_str_layout
                   is_variant = l_variant
                   it_toolbar_excluding = l_toolbar_excluding
                   i_save = 'A'
         CHANGING  it_outtab = <fs_grid_tab>
                   it_fieldcatalog = l_fieldcat
         ).
  ENDMETHOD.


  METHOD populate_grid_tab_with_changes.
* g_grid->refresh_table_display needs to be called to color changed lines
* it means that <fs_grid_tab> must be updated with data from e_changed_cells, even though grid already reflects changes on the screen
* transfer e_changed_cells to <fs_grid_tab> and color changed lines
    FIELD-SYMBOLS:
      <fs>          TYPE any,
      <fs_grid_tab> TYPE STANDARD TABLE.
    IF p_changed_cells IS NOT INITIAL.
* assumption is that p_changed_cells are from same row
      ASSIGN g_grid_tab->* TO <fs_grid_tab>.
* get change grid line number from first modified cell
      READ TABLE <fs_grid_tab> ASSIGNING FIELD-SYMBOL(<fs_grid_line>) INDEX p_changed_cells[ 1 ]-row_id.
      IF <fs_grid_line> IS NOT ASSIGNED.
        APPEND INITIAL LINE TO <fs_grid_tab> ASSIGNING <fs_grid_line>.
      ELSE.
* clear cells with error (if any)
        ASSIGN COMPONENT 'ERRCELLCOLOR' OF STRUCTURE <fs_grid_line> TO <fs>.
        CLEAR <fs>.
        ASSIGN COMPONENT 'ERRORCELLMESSAGE' OF STRUCTURE <fs_grid_line> TO <fs>.
        CLEAR <fs>.
        ASSIGN COMPONENT 'LINECOLOR' OF STRUCTURE <fs_grid_line> TO <fs>.
        CLEAR <fs>.
      ENDIF.

      ASSIGN COMPONENT 'ROWID' OF STRUCTURE <fs_grid_line> TO FIELD-SYMBOL(<fs_rowid>).
      IF <fs_rowid> = 0.
        g_last_rowid = g_last_rowid + 1.
        <fs_rowid> = g_last_rowid.
      ENDIF.

* color line yellow if a row doesn't have errors
      ASSIGN COMPONENT 'LINECOLOR' OF STRUCTURE <fs_grid_line> TO <fs>.
      <fs> = 'C311'.

* populate grid-to-change mapping table if a row doesn't have errors
      APPEND <fs_rowid> TO g_map_grid_change_tab.

* transfer changes to grid table
      DATA:
        l_fieldcatalog   TYPE lvc_t_fcat.

      g_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = l_fieldcatalog ).
      LOOP AT p_changed_cells ASSIGNING FIELD-SYMBOL(<fs_changed_cell>).
        ASSIGN COMPONENT <fs_changed_cell>-fieldname OF STRUCTURE <fs_grid_line> TO <fs> .
        <fs> = <fs_changed_cell>-value.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD prepare_master_fieldcat.

    DATA:
      l_fieldcat_tab TYPE REF TO data,
      l_fieldcat_aux TYPE slis_t_fieldcat_alv.
    CREATE DATA l_fieldcat_tab TYPE STANDARD TABLE OF (g_strucname).
    CALL FUNCTION '/BA1/FF_API_ALV_FIELDCAT_SET'
      EXPORTING
        i_rda_data      = l_fieldcat_tab " data reference to the data table
        i_flg_unit      = ''
        i_flg_char_text = ''
      IMPORTING
        e_tab_fieldcat  = l_fieldcat_aux
      EXCEPTIONS
        not_qualified   = 1
        failed          = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
* at the moment (20171106) the /BA1/FF_API_ALV_FIELDCAT_SET function doesn't populate domname and ref_table fields
* as a workaround the fields are populated manually using DDIF_TABL_GET
    DATA: l_dd03p_tab      TYPE STANDARD TABLE OF dd03p,
          l_structure_name TYPE ddobjname.
    l_structure_name = g_strucname.
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = l_structure_name
      TABLES
        dd03p_tab     = l_dd03p_tab
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
* transforming slis_t_fieldcat_alv to lvc_t_fcat
    LOOP AT l_fieldcat_aux ASSIGNING FIELD-SYMBOL(<fs>).
      READ TABLE l_dd03p_tab ASSIGNING FIELD-SYMBOL(<fs_dd03p_tab_line>) WITH KEY fieldname = <fs>-fieldname.
      IF sy-subrc = 0. " if result of /BA1/FF_API_ALV_FIELDCAT_SET is in DDIC then keep it in final structure
        APPEND INITIAL LINE TO g_master_fieldcat ASSIGNING FIELD-SYMBOL(<fs_master_fieldcat>).
        MOVE-CORRESPONDING <fs> TO <fs_master_fieldcat>.
        <fs_master_fieldcat>-scrtext_l = <fs>-seltext_l.
        <fs_master_fieldcat>-scrtext_m = <fs>-seltext_m.
        <fs_master_fieldcat>-scrtext_s = <fs>-seltext_s.
        <fs_master_fieldcat>-reptext   = <fs>-reptext_ddic.
        <fs_master_fieldcat>-hotspot = ''.
        <fs_master_fieldcat>-domname = <fs_dd03p_tab_line>-domname.
        <fs_master_fieldcat>-ref_table = <fs_dd03p_tab_line>-tabname.
      ENDIF.
    ENDLOOP.

    SORT g_master_fieldcat BY fieldname ASCENDING. " to improve read table performance
  ENDMETHOD.


  METHOD show_ret_table.
* based on /BA1/R6_API_DP_MSG_RETTAB_SHOW
    DATA:
      l_log_writer  TYPE REF TO /ba1/if_xx_log_writer,
      l_msg_handler TYPE REF TO /ba1/if_al_x0_msg_handler,
      l_log_handler TYPE REF TO /ba1/if_xx_log_handler,
      l_object      TYPE balobj_d,
      l_subobject   TYPE balsubobj,
      l_bal_t_logh  TYPE bal_t_logh,
      l_balloghndl  TYPE balloghndl.

    IF p_tab_msg IS INITIAL.
      RETURN.
    ENDIF.
* begin from FUNCTION /BA1/R6_API_JC_LOGHDL_OPEN
    l_object = '/BA1/H_MAIN'.
    l_subobject = 'HX'.
    TRY.
        /ba1/cl_xx_log_writer_factory=>init( EXPORTING i_object = l_object
                                                       i_subobject = l_subobject ).
      CATCH /ba1/cx_xx_static_check_gen. " no handler
    ENDTRY.
    TRY.
        /ba1/cl_xx_log_writer_factory=>open(
          EXPORTING
            i_object = l_object
            i_subobject = l_subobject
          RECEIVING
            r_log_writer = l_log_writer ).
      CATCH /ba1/cx_xx_static_check_gen. " no handler
    ENDTRY.
    l_log_writer->set_cumulative(
      EXPORTING
        im_cumulative_on      = 'X'
        im_compare_attributes = abap_true
        im_compare_context    = abap_true
        im_compare_parameters = abap_true ).
* end from FUNCTION /BA1/R6_API_JC_LOGHDL_OPEN.

    l_msg_handler ?= l_log_writer.
    l_msg_handler->add_bapirettab(
      EXPORTING
        i_tab_message = p_tab_msg ).

    l_log_handler ?= l_log_writer.
    l_log_handler->get_log_handle(
      RECEIVING
        re_log_handle = l_balloghndl ).

    APPEND l_balloghndl TO l_bal_t_logh.

    l_log_handler->display(
      EXPORTING
        i_flg_popup  = 'X'
        i_new_window = ''
        i_tab_handle = l_bal_t_logh ).
    l_log_writer->close_log(
      EXCEPTIONS
        failed = 1
        OTHERS = 2 ).
  ENDMETHOD.


  METHOD view_attachment.

    DATA: ls_row_no TYPE lvc_t_roid,
          ls_rows   TYPE lvc_t_row,
          lv_row    TYPE i,
          lv_btran  TYPE /ba1/f2_dte_bt_id.

    DATA: manager TYPE REF TO cl_gos_manager,
          obj     TYPE borident.

    CONSTANTS: objtype     TYPE borident-objtype VALUE 'ZBTID',
               lc_reltype  TYPE oblreltype VALUE 'ATTA',
               lc_typeid_a TYPE sibftypeid VALUE 'ZBTID'.

    FIELD-SYMBOLS <fs_grid_tab> TYPE STANDARD TABLE.

*   Get the selected row number
    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_row_no     = ls_row_no
        et_index_rows = ls_rows.

    READ TABLE ls_row_no ASSIGNING FIELD-SYMBOL(<fs_row_no>) INDEX 1.
    IF sy-subrc EQ 0.
      lv_row = <fs_row_no>-row_id.
    ELSE.
      MESSAGE TEXT-e02 TYPE 'S'.
      EXIT.
    ENDIF.

*   Get the Business Transaction ID
    ASSIGN g_grid_tab->* TO <fs_grid_tab>.
    READ TABLE <fs_grid_tab> ASSIGNING FIELD-SYMBOL(<fs_grid_line>) INDEX ( lv_row ).
    IF sy-subrc EQ 0.
      CLEAR lv_btran.

      ASSIGN COMPONENT '/BA1/C35BTRAN' OF STRUCTURE <fs_grid_line> TO FIELD-SYMBOL(<fs_btran>).
      lv_btran = <fs_btran>.
    ENDIF.

*   Check if there is an existing attachment
    IF NOT lv_btran IS INITIAL.
      SELECT SINGLE instid_b INTO @DATA(lv_instid_b)
      FROM srgbtbrel
      WHERE reltype  EQ @lc_reltype
        AND instid_a EQ @lv_btran
        AND typeid_a EQ @lc_typeid_a.

      IF sy-subrc NE 0.
        MESSAGE TEXT-e04 TYPE 'S'.
        EXIT.
      ENDIF.
    ENDIF.

*   SAP services for object
    IF NOT lv_btran IS INITIAL.
      obj-objtype = objtype.
      obj-objkey  = lv_btran.

*     Create the object of class 'CL_GOS_MANAGER'
      CREATE OBJECT manager
        EXPORTING
          is_object    = obj
          ip_no_commit = 'R'
        EXCEPTIONS
          OTHERS       = 1.

      IF sy-subrc EQ 0.
*       Open dialog box for attachment
        CALL METHOD manager->start_service_direct
          EXPORTING
            ip_service       = 'VIEW_ATTA'
            is_object        = obj
          EXCEPTIONS
            no_object        = 1
            object_invalid   = 2
            execution_failed = 3
            OTHERS           = 4.
        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

    ELSE.
      MESSAGE TEXT-e03 TYPE 'S'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD import_upload.
    TYPES:
      BEGIN OF ls_csv_grid_map,
        csv_name  TYPE char256,
        grid_name TYPE char256,
        data_type TYPE char30,
      END OF ls_csv_grid_map.

    DATA:
      l_columns               TYPE STANDARD TABLE OF char255,
      l_column                TYPE char255,
      l_csv_cols_to_grid_cols TYPE STANDARD TABLE OF ls_csv_grid_map,
      l_csv_col_to_grid_col   TYPE ls_csv_grid_map,
      l_component             TYPE char30,
      l_datatype              TYPE char30,
      l_fieldcatalog          TYPE lvc_t_fcat,
      l_changed_cells         TYPE gs_changed_cells,
      l_changed_cell          TYPE gs_changed_cell,
      l_row_id                TYPE int4,
      l_value                 TYPE lvc_value,
      l_date                  TYPE d,
      l_str_detail_small      TYPE /ba1/f0_str_char_detail_small,
      l_func_name             TYPE char30,
      l_ref                   TYPE REF TO data,
      l_im_currency_code      TYPE tcurc-isocd,
      l_ex_currency_code      TYPE tcurc-waers,
      l_ref_gdt               TYPE REF TO cx_gdt_conversion,
      l_tcurx                 TYPE tcurx,
      l_amnt                  TYPE /ba1/fu_dte_amnt,
      l_str_details           TYPE  /ba1/ff_str_details,
      l_structtype            TYPE  /ba1/ff_dte_string,
      l_tab_units_not_found   TYPE  /ba1/ff_tab_technames,
      l_tab_fields            TYPE  /ba1/ff_tab_fieldprop,
      l_tab_return            TYPE  bapirettab,

      l_mapped                TYPE abap_bool,

      lt_mje_docs             TYPE ztt_mje_post_docs,
      lo_struct_descr         TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <fs_grid_tab> TYPE STANDARD TABLE,
      <kf_field>    TYPE any.

*    Prepare ALV grid data table reference
    ASSIGN g_grid_tab->* TO <fs_grid_tab>.
    IF <fs_grid_tab> IS NOT INITIAL.
      CLEAR <fs_grid_tab>.
      refresh_grid( ).
    ENDIF.

*   Call MJE user upload program and import upload result when done.
    TRY.
        CALL TRANSACTION g_upload_prog_tcode WITH AUTHORITY-CHECK.
      CATCH cx_sy_authorization_error INTO DATA(lx).
        MESSAGE lx TYPE 'E'.
    ENDTRY.

    import_upload_table(
        IMPORTING
            it_upload = lt_mje_docs ).
    IF lt_mje_docs IS INITIAL.
      MESSAGE e512(zfpsl_mje).
      RETURN.
    ELSE.
      CALL FUNCTION '/BA1/FF_API_F_STRUCT_DET_GET'
        EXPORTING
          i_rda_fields          = g_grid_tab
        IMPORTING
          e_str_details         = l_str_details
          e_structtype          = l_structtype
          e_tab_units_not_found = l_tab_units_not_found
          e_tab_fields          = l_tab_fields
          e_tab_return          = l_tab_return
        EXCEPTIONS
          get_fieldnames_failed = 1
          get_technames_failed  = 2
          get_char_det_failed   = 3
          get_kf_det_failed     = 4
          get_unit_det_failed   = 5
          get_field_det_failed  = 6
          get_ddic_info_failed  = 7
          OTHERS                = 8.
      IF sy-subrc <> 0.
        " ignore errors
        "MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      SORT l_str_details-char_detail BY fieldname ASCENDING.
      SORT l_tab_fields BY fieldname ASCENDING.

      g_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = l_fieldcatalog ).

*      Describe MJE input structure and extract columns to use in ALV
      lo_struct_descr ?= cl_abap_structdescr=>describe_by_data( lt_mje_docs[ 1 ] ).
      DATA(lt_components) = lo_struct_descr->components.
      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<str_components>).
        APPEND <str_components>-name TO l_columns.
      ENDLOOP.

*      map column names to technical names
      LOOP AT l_columns INTO l_column.
        l_csv_col_to_grid_col-csv_name = l_column.
        TRY.
* this is an implementation with names in CSV as it is shown in grid
*            l_csv_col_to_grid_col-grid_name = l_fieldcatalog[ reptext = l_column ]-fieldname.
* this is an implementation with technical names in CSV
            l_csv_col_to_grid_col-grid_name = l_fieldcatalog[ fieldname = l_column ]-fieldname.
            l_csv_col_to_grid_col-data_type = l_fieldcatalog[ fieldname = l_column ]-inttype.
            l_mapped = 'X'. " at least one is mapped
          CATCH cx_sy_itab_line_not_found.
            l_csv_col_to_grid_col-grid_name = ''.
        ENDTRY.
        APPEND l_csv_col_to_grid_col TO l_csv_cols_to_grid_cols.
      ENDLOOP.
      IF NOT l_mapped = 'X'.
        MESSAGE e539(/ba1/hw) WITH '1'.
        "MESSAGE 'Please make sure that first line in the CSV file contains techincal names of columns' TYPE 'I'.
        RETURN.
      ENDIF.

      DATA lt_fieldcatalog_sorted LIKE l_fieldcatalog.
      lt_fieldcatalog_sorted  = l_fieldcatalog.
      SORT lt_fieldcatalog_sorted BY fieldname ASCENDING.

      l_row_id = lines( <fs_grid_tab> ). " import rows to to bottom of current table

*      Loop through input table and populate ALV
      LOOP AT lt_mje_docs ASSIGNING FIELD-SYMBOL(<str_mje_docs>).
        l_row_id = l_row_id + 1.
        CLEAR: l_changed_cells, l_columns.
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <str_mje_docs> TO FIELD-SYMBOL(<val_mje_doc>).
*        Exit when no more headers are available
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          APPEND <val_mje_doc> TO l_columns.
        ENDDO.

*        Loop through columns and format values
        LOOP AT l_columns ASSIGNING FIELD-SYMBOL(<fs_value>).
          l_component = l_csv_cols_to_grid_cols[ sy-tabix ]-grid_name.
          l_datatype = l_csv_cols_to_grid_cols[ sy-tabix ]-data_type.
          IF l_component IS INITIAL.

            CONTINUE.
          ENDIF.
          l_changed_cell-row_id    = l_row_id.
          l_changed_cell-fieldname = l_component.
          l_changed_cell-value     = <fs_value>.
          l_changed_cell-error     = ''.

          CLEAR l_str_detail_small.
          READ TABLE l_tab_fields ASSIGNING FIELD-SYMBOL(<fields>)
            WITH KEY fieldname = l_component BINARY SEARCH.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          CASE <fields>-type.
            WHEN 'C'.
              READ TABLE l_str_details-char_detail ASSIGNING FIELD-SYMBOL(<char>) WITH KEY fieldname = l_component BINARY SEARCH.
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.
              MOVE-CORRESPONDING <char> TO l_str_detail_small.
              IF <char>-lowercase IS INITIAL.
                TRANSLATE l_changed_cell-value TO UPPER CASE.
                TRANSLATE <fs_value> TO UPPER CASE.
              ENDIF.
            WHEN 'K'.
              READ TABLE l_str_details-kf_detail ASSIGNING FIELD-SYMBOL(<keyf>) WITH KEY fieldname = l_component .
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.
              MOVE-CORRESPONDING <keyf> TO l_str_detail_small.
              l_str_detail_small-data_element = <keyf>-dtename.
              l_str_detail_small-datatype     = <keyf>-dtype.

              REPLACE ALL OCCURRENCES OF ',' IN <fs_value> WITH '.'.

              CASE <keyf>-type.
                WHEN '01'.
                  IF <kf_field> IS ASSIGNED.
                    UNASSIGN <kf_field>.
                  ENDIF.

                  READ TABLE l_csv_cols_to_grid_cols ASSIGNING FIELD-SYMBOL(<gridcol>) WITH KEY grid_name = <keyf>-fn_unit.
*                      READ TABLE lt_fieldcatalog_sorted ASSIGNING FIELD-SYMBOL(<fcat>) WITH KEY fieldname = <keyf>-fn_unit BINARY SEARCH.
                  IF sy-subrc <> 0.
                    CONTINUE.
                  ENDIF.
                  READ TABLE l_columns INDEX sy-tabix ASSIGNING FIELD-SYMBOL(<unit_value>).
                  IF sy-subrc <> 0.
                    CONTINUE.
                  ENDIF.
                  l_im_currency_code = <unit_value>.

                  IF <keyf>-dtype = 'CURR' AND NOT ( <keyf>-kf_category CS 'LAM' OR <keyf>-kf_category CS 'LDEC' ).
                    ASSIGN l_amnt TO <kf_field>. " empty
                    DATA: lv_amount TYPE /ba1/fx_dte_amount.
                    lv_amount = <fs_value>.
                    IF NOT ( lv_amount IS INITIAL AND l_im_currency_code IS INITIAL ).
                      TRY.
                          CALL METHOD cl_gdt_conversion=>amount_inbound
                            EXPORTING
                              im_value         = lv_amount
                              im_currency_code = l_im_currency_code
                            IMPORTING
                              ex_value         = <kf_field>
                              ex_currency_code = l_ex_currency_code.
                        CATCH cx_gdt_conversion INTO l_ref_gdt.
                          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                          MESSAGE i028(/ba1/in_hx) WITH |{ lv_amount }/{ l_im_currency_code }| l_component '-' '-' .
                          RETURN.
                      ENDTRY.
                      l_changed_cell-value = <kf_field>.
                    ENDIF.
                  ELSE.
                    DATA lr_number TYPE REF TO data.
                    CREATE DATA lr_number TYPE (<keyf>-dtename).
                    ASSIGN lr_number->* TO <kf_field>.
                    SELECT SINGLE * FROM tcurx INTO l_tcurx
                      WHERE currkey = l_im_currency_code.
                    IF sy-subrc NE 0.
                      <kf_field> = <fs_value>.
                    ELSE.
                      "<kf_field> = <fs_value> * 10 ** ( l_tcurx-currdec - 2 ).
                      " avoid implicit conversion to type f
                      <kf_field> = <fs_value>.
                      DO l_tcurx-currdec - 2 TIMES.
                        <kf_field> = <kf_field> * 10.
                      ENDDO.
                    ENDIF.
                    l_changed_cell-value = <kf_field>.
                  ENDIF.

                WHEN '02'.
                  IF <keyf>-dtype = 'FLTP'.
                    l_changed_cell-value = <fs_value>.
                  ELSE.
                    l_changed_cell-value = <fs_value>. "percentage
                  ENDIF.
                WHEN '03'.
                  l_changed_cell-value = <fs_value>. "quantity
                  "unit     = quantity-unit_code.
                WHEN '04'.
                  l_changed_cell-value = <fs_value>.  "integer_value
              ENDCASE.

            WHEN 'U'.
              READ TABLE l_str_details-unit_detail ASSIGNING FIELD-SYMBOL(<unit>) WITH KEY fn_unit = l_component .
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.
              l_str_detail_small-data_element = <unit>-dtename_unit.
              l_str_detail_small-datatype     = <unit>-dtype_unit.
              TRANSLATE <fs_value> TO UPPER CASE.
              l_changed_cell-value = <fs_value>.
            WHEN 'D'.
              READ TABLE l_str_details-ddic_detail ASSIGNING FIELD-SYMBOL(<ddic>) WITH KEY fieldname = l_component .
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.
              MOVE-CORRESPONDING <ddic> TO l_str_detail_small.
              l_str_detail_small-data_element = <ddic>-rollname.
              IF <ddic>-lowercase IS INITIAL.
                TRANSLATE <fs_value> TO UPPER CASE.
              ENDIF.
              l_changed_cell-value = <fs_value>.
            WHEN OTHERS.
              CONTINUE.
          ENDCASE.

          IF NOT l_str_detail_small-convexit IS INITIAL.
            CONCATENATE 'CONVERSION_EXIT_' l_str_detail_small-convexit '_INPUT' INTO l_func_name.
            CONDENSE l_func_name NO-GAPS.
            CREATE DATA l_ref TYPE (l_str_detail_small-data_element).
            ASSIGN l_ref->* TO FIELD-SYMBOL(<value_cexit>).
            <value_cexit> = <fs_value>.
            IF NOT <value_cexit> IS INITIAL.
              CALL FUNCTION l_func_name
                EXPORTING
                  input  = <fs_value>
                IMPORTING
                  output = <value_cexit>
                EXCEPTIONS
                  OTHERS = 1.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                MESSAGE i028(/ba1/in_hx) WITH <fs_value> l_component '-' '-' .
                RETURN.
              ENDIF.
              l_changed_cell-value = <value_cexit>.
            ENDIF.
          ENDIF.

          IF l_str_detail_small-datatype = 'DATS'.
            l_value = <fs_value>.
            IF strlen( l_value ) > 8.
              convert_value_to_date( EXPORTING p_value = l_value IMPORTING p_date = l_date ).
              l_changed_cell-value = l_date.
            ENDIF.
          ENDIF.

          IF l_str_detail_small-datatype = 'CUKY'.
            DATA:
              l_im_iso_code TYPE  tcurc-isocd,
              l_ex_sap_code TYPE  tcurc-waers.
            l_im_iso_code = <fs_value>.
            IF NOT l_im_iso_code IS INITIAL.
              CALL FUNCTION 'CURRENCY_CODE_ISO_TO_SAP'
                EXPORTING
                  iso_code  = l_im_iso_code
                IMPORTING
                  sap_code  = l_ex_sap_code
                EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                MESSAGE i028(/ba1/in_hx) WITH l_im_iso_code l_component '-' '-' .
                RETURN.
              ENDIF.
              l_changed_cell-value = l_ex_sap_code.
            ENDIF.
          ENDIF.

          IF l_str_detail_small-datatype = 'UNIT'.
            " see also DB-table T006 or transaction code CUNI
            DATA:
              l_im_iso_code_u TYPE t006-isocode,
              l_ex_sap_code_u TYPE  t006-msehi,
              l_ex_unique     TYPE  isofields-unique.
            l_im_iso_code_u = <fs_value>.
            IF NOT l_im_iso_code_u IS INITIAL.
              CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
                EXPORTING
                  iso_code  = l_im_iso_code_u
                IMPORTING
                  sap_code  = l_ex_sap_code_u
                  unique    = l_ex_unique
                EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
                MESSAGE i028(/ba1/in_hx) WITH l_im_iso_code_u l_component '-' '-' .
                RETURN.
              ENDIF.
              l_changed_cell-value = l_ex_sap_code_u.
            ENDIF.
          ENDIF.
          SHIFT l_changed_cell-value LEFT DELETING LEADING space.
          APPEND l_changed_cell TO l_changed_cells.
        ENDLOOP.
        me->populate_grid_tab_with_changes( EXPORTING p_changed_cells = l_changed_cells ).

      ENDLOOP.
      refresh_grid( ).
    ENDIF.
  ENDMETHOD.


  METHOD import_upload_table.
    DATA:
      lv_mem_id TYPE c LENGTH 20.

    lv_mem_id =   |MJE_{ sy-uname }|.

    TRY.
*        Import the upload data from SAP memory then free it
        IMPORT table = it_upload FROM MEMORY ID lv_mem_id.
        FREE MEMORY ID lv_mem_id.

      CATCH cx_root INTO DATA(lx_error).
        MESSAGE e511(zfpsl_mje).
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD convert_value_to_date.

    IF strlen( p_value ) = 8.
      p_date = p_value.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = p_value
      IMPORTING
        date_internal            = p_date
      EXCEPTIONS
        date_external_is_invalid = 1.
    IF sy-subrc <> 0. " if CONVERT_DATE_TO_INTERNAL returns an exception, leave date as it is in the source file
      p_date = p_value.
    ENDIF.

  ENDMETHOD.


  METHOD get_fields.
* todo: get rid of get_fields once a helper method is provided
*  get_fields is copied from /BA1/R_UI_HW_RESULT_VIEWERF01
    DATA:
      l_ref_view      TYPE REF TO /ba1/if_al_hm_view,
      l_tab_field     TYPE /ba1/if_al_hm_view=>ty_tah_field,
      l_wrk_field     LIKE LINE OF l_tab_field,
      l_wrk_char      LIKE LINE OF p_tab_char,
      l_wrk_keyf_unit LIKE LINE OF p_tab_keyf_unit.

    CLEAR:
    p_tab_char,
    p_tab_keyf_unit.

    IF p_area IS INITIAL OR p_view IS INITIAL.
      EXIT.
    ENDIF.

*  factory_get_instance is copied from /BA1/R_UI_HW_RESULT_VIEWERF01
    IF g_ref_factory IS NOT BOUND.
      TRY.
          g_ref_factory = /ba1/cl_al_hx_factory=>s_get_instance( ).
        CATCH /ba1/cx_hx_static_check INTO g_ref_cx_static.
          MESSAGE g_ref_cx_static TYPE 'A'.
      ENDTRY.
    ENDIF.

    TRY.
        CALL METHOD g_ref_factory->get_view
          EXPORTING
            i_area                 = p_area
            i_view                 = p_view
            i_flg_complete_version = abap_true
          RECEIVING
            r_ref_view             = l_ref_view.

      CATCH /ba1/cx_hx_static_check INTO g_ref_cx_static.
        MESSAGE g_ref_cx_static TYPE 'E'.
    ENDTRY.

    l_tab_field = l_ref_view->get_fields( ).

    LOOP AT l_tab_field INTO l_wrk_field.

      IF    ( l_wrk_field-is_unit <> 'X')
        AND ( l_wrk_field-characteristic <> /ba1/if_hx_names=>c_ch_filter )
        AND ( l_wrk_field-characteristic <> /ba1/if_hx_names=>c_ch_scenario ).
*        and ( l_wrk_field-characteristic <> /ba1/if_hx_names=>c_ch_keydate ).

        IF    ( l_wrk_field-characteristic IS NOT INITIAL ).
* characteristic
          MOVE:  l_wrk_field-characteristic TO l_wrk_char-characteristic,
                 abap_false TO l_wrk_char-sng_val.

          APPEND l_wrk_char TO p_tab_char.

        ELSEIF ( l_wrk_field-keyfigure IS NOT INITIAL )
           AND ( l_wrk_field-unit_fieldname IS NOT INITIAL ).
* keyfigure
          MOVE: l_wrk_field-keyfigure      TO l_wrk_keyf_unit-kf_id,
                l_wrk_field-unit_fieldname TO l_wrk_keyf_unit-fn_unit.

          APPEND l_wrk_keyf_unit TO p_tab_keyf_unit.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    " get_fieldes


  METHOD get_instance.
    DATA:
      l_mje_prep_ui_proc    TYPE REF TO zcl_mje_preparer_ui_process,
      l_ref_cx_al_br_custom TYPE REF TO /ba1/cx_al_br_customizing,
      l_ref_cx_static       TYPE REF TO /ba1/cx_hx_static_check.
    TRY.
        CREATE OBJECT l_mje_prep_ui_proc.
        l_mje_prep_ui_proc->set_grid_structure( EXPORTING p_accrt = p_accrt ).
        p_prep_ui_proc = l_mje_prep_ui_proc.
      CATCH /ba1/cx_al_br_customizing INTO  l_ref_cx_al_br_custom.
        MESSAGE l_ref_cx_al_br_custom TYPE 'I'.
      CATCH  /ba1/cx_hx_static_check  INTO l_ref_cx_static.
        MESSAGE l_ref_cx_static TYPE 'I'.
    ENDTRY.

    l_mje_prep_ui_proc->pr_mje_helper = zcl_mje_util_helper=>get_instance( ).
  ENDMETHOD.


  METHOD refresh_grid.
    DATA: l_stbl              TYPE lvc_s_stbl.
    l_stbl-row = 'X'.
    l_stbl-col = 'X'.

    g_grid->refresh_table_display( EXPORTING is_stable = l_stbl ).

  ENDMETHOD.


  METHOD save_mje.
    TYPES:
      BEGIN OF ls_error,
        rowno       TYPE int4,
        err_message TYPE string,
        err_field   TYPE lvc_fname,
      END OF ls_error,
      ls_errors TYPE STANDARD TABLE OF ls_error.

    TYPES: BEGIN OF ts_objkey,
             objkey TYPE sibfboriid,
           END OF ts_objkey.

    DATA: l_tab_return      TYPE bapirettab,
          l_line_return     LIKE LINE OF l_tab_return,
          l_err_field       TYPE lvc_fname,
          l_err_message     TYPE string,
          l_err_field_color TYPE lvc_s_scol,
          l_rowid           TYPE int4,
          l_rowno           TYPE int4,
          l_change_tab      TYPE REF TO data,
          c_err_cell_color  TYPE lvc_s_colo,
          l_error           TYPE ls_error,
          l_errors          TYPE ls_errors,
          l_fieldcatalog    TYPE lvc_t_fcat,


          l_rng_bt_id       TYPE RANGE OF string,
          lt_appr_wl        TYPE STANDARD TABLE OF ztab_mje_rel_wl.

    FIELD-SYMBOLS:
      <fs_errcellcolor> TYPE lvc_t_scol,
      <fs_linecolor>    TYPE c,
      <fs_change_tab>   TYPE STANDARD TABLE,
      <fs_grid_tab>     TYPE STANDARD TABLE,
      <fs_grid_line>    TYPE any,
      <fs_rowid>        TYPE int4,

      <fs_btran>        TYPE /ba1/f2_dte_bt_id.

    CREATE DATA l_change_tab TYPE STANDARD TABLE OF (g_strucname).

    ASSIGN l_change_tab->* TO <fs_change_tab>.

    c_err_cell_color-col = '6'.
    c_err_cell_color-int = '1'.
    c_err_cell_color-inv = '1'.

* populate changed lines collection from <fs_grid_tab> and g_map_grid_change_tab
    SORT g_map_grid_change_tab.
    DELETE ADJACENT DUPLICATES FROM g_map_grid_change_tab.
    ASSIGN g_grid_tab->* TO <fs_grid_tab>.
    g_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = l_fieldcatalog ).
    LOOP AT <fs_grid_tab> ASSIGNING <fs_grid_line>.

      " populate <fs_change_tab>
      APPEND INITIAL LINE TO <fs_change_tab> ASSIGNING FIELD-SYMBOL(<fs_change_tab_line>).
      MOVE-CORRESPONDING <fs_grid_line> TO <fs_change_tab_line>.
*      ENDIF.

      ASSIGN COMPONENT '/BA1/C35BTRAN'  OF STRUCTURE <fs_change_tab_line> TO <fs_btran>.

      APPEND INITIAL LINE TO l_rng_bt_id ASSIGNING FIELD-SYMBOL(<rng_bt_id>).
      <rng_bt_id>-sign = 'I'.
      <rng_bt_id>-option = 'EQ'.
      <rng_bt_id>-low = <fs_btran>.
    ENDLOOP.

    IF lines( <fs_change_tab> ) > 0.
      SORT <fs_change_tab>.
      DELETE ADJACENT DUPLICATES FROM <fs_change_tab> COMPARING ALL FIELDS.

      CALL FUNCTION '/BA1/IN_HK_DLL_CREATE'
        EXPORTING
          i_area                    = g_area
          i_type                    = g_type
          i_tab_data                = <fs_change_tab>
          i_flg_internal_amt_format = 'X'
        IMPORTING
          e_tab_return              = l_tab_return
        EXCEPTIONS
          OTHERS                    = 0. " l_tab_return is analyzed to get errors if any
      COMMIT WORK AND WAIT.

      LOOP AT l_tab_return INTO l_line_return.
        TRY.
            l_rowid = g_map_grid_change_tab[ l_line_return-row ].
          CATCH cx_sy_itab_line_not_found.
            l_rowid = 0.
        ENDTRY.
        IF l_rowid <> 0. " it is a line specific issue
          l_rowno = 0.
* it has to be done via loop with field symbol due to generic nature of <fs_grid_tab>.
* "read table" requires known structure
          LOOP AT <fs_grid_tab> ASSIGNING <fs_grid_line>.
            ASSIGN COMPONENT 'ROWID' OF STRUCTURE <fs_grid_line> TO <fs_rowid>.
            IF <fs_rowid> = l_rowid.
              l_rowno = sy-tabix.
            ENDIF.
          ENDLOOP.

          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = l_line_return-id
              no        = l_line_return-number
              v1        = l_line_return-message_v1
              v2        = l_line_return-message_v2
              v3        = l_line_return-message_v3
              v4        = l_line_return-message_v4
            IMPORTING
              msg       = l_err_message
            EXCEPTIONS
              not_found = 1.
          IF sy-subrc <> 0.
            l_err_message = ''.
          ENDIF.
          l_error-rowno = l_rowno.
          l_error-err_message = l_err_message.
          l_error-err_field = ''.  " highlight whole line
*hard coding is to highlight cells with errors as much as it makes sense
*the full error anyway is displayed in the application log
          IF l_line_return-id = '/BA1/IN_HX'.
            CASE l_line_return-number.
              WHEN '023'.
                l_err_field = l_line_return-message_v3.
              WHEN '602'.
                l_err_field = l_line_return-message_v2.
              WHEN '026'.
                l_err_field = l_line_return-message_v1.
                IF l_err_field = 'KEYDATE'.
                  l_err_field = '/BA1/CR0RSKYDT'.
                ENDIF.
              WHEN '025'.
                l_err_field = '/BA1/C55CONTID'.
              WHEN OTHERS.
                l_err_field = ''. " highlight whole line
            ENDCASE.
            l_error-err_field = l_err_field.
          ENDIF.
        ELSE.
          l_error-rowno = -1.
          l_error-err_message = 'Non-row specific error'(m01).
        ENDIF.
        APPEND l_error TO l_errors.
      ENDLOOP.

      IF lines( l_tab_return ) > 0.
        me->show_ret_table( p_tab_msg = l_tab_return ).
      ENDIF.

      IF lines( l_errors ) = 0.
        DATA(lv_line_cnt) = lines( <fs_change_tab> ).
        MESSAGE s050(zfpsl_mje) WITH lv_line_cnt.

        DATA: lv_wi_ts          TYPE timestampl.
        GET TIME STAMP FIELD lv_wi_ts.

        SORT <fs_change_tab>.
*       End of customization C. Baclig 05/04/2021

*        Write document key to mass release worklist with initial status
        IF lines( <fs_change_tab> ) <> 0.
          DATA(lv_rel_stats) = CONV /ba1/br_dte_adj_rel_status( '01' ).

          TRY.
              SELECT
                client AS mandt,
                /ba1/c55postd,
                /ba1/c55lgent,
                /ba1/c55docnum,
                /ba1/k11prtky,
                /ba1/c55year,
                /ba1/c35btran,
                @lv_wi_ts AS creation_ts,
                @sy-uname AS /ba1/c55ppcrus,
                @lv_rel_stats AS release_status
                INTO CORRESPONDING FIELDS OF TABLE @lt_appr_wl
                FROM /ba1/hfppd
                WHERE   /ba1/c35btran IN @l_rng_bt_id
                AND     /ba1/c55docitm = '000001'.
              IF sy-subrc <> 0.
                RAISE EXCEPTION TYPE /ba1/cx_al_br_adjust.
              ENDIF.

              SORT lt_appr_wl BY /ba1/c55lgent /ba1/c55postd /ba1/c55docnum.
              DELETE ADJACENT DUPLICATES FROM lt_appr_wl.
*              Upload current batch to the approver UI worklist table.
              INSERT ztab_mje_rel_wl  FROM TABLE lt_appr_wl.
              IF sy-subrc <> 0.
                RAISE EXCEPTION TYPE /ba1/cx_al_br_adjust.
              ENDIF.

              DATA(lv_requestor) = CONV sobid( sy-uname ).
              DATA(lt_approvers) = pr_mje_helper->get_ou_approvers( iv_requestor = lv_requestor ).
              IF lt_approvers IS INITIAL.
                RAISE EXCEPTION TYPE /ba1/cx_al_br_adjust.
              ENDIF.
              LOOP AT lt_appr_wl ASSIGNING FIELD-SYMBOL(<str_appr_wl>)
              GROUP BY <str_appr_wl>-/ba1/c35btran WITHOUT MEMBERS INTO <fs_btran>.
                zcl_mje_util_helper=>send_email_notif(
                    iv_initiator    = CONV sobid( sy-uname )
                    iv_email_title  = zcl_mje_util_helper=>gv_mje_email_title
                    iv_email_link   = zcl_mje_util_helper=>get_html_anchor_string(
                        iv_link = zcl_mje_util_helper=>get_fiori_link(
                            iv_app_id = zcl_mje_util_helper=>gv_app_id_mass_rel )
                        iv_text = 'Fiori Launchpad' )
                    iv_rel_status   = '01'
                    iv_bt_id        = <fs_btran>
                    it_ou_recipients = lt_approvers ).
              ENDLOOP.

*              Lastly, clear the processed entries from grid and refresh.
              ASSIGN g_grid_tab->* TO FIELD-SYMBOL(<tab_grid>).
              CLEAR <tab_grid>.
              refresh_grid( ).

            CATCH   cx_swf_cnt_elem_not_found
                    cx_swf_cnt_elem_type_conflict
                    cx_swf_cnt_unit_type_conflict
                    cx_swf_cnt_container.
              MESSAGE e404(zfpsl_mje).
              RETURN.

            CATCH cx_root.
              MESSAGE e405(zfpsl_mje).
              RETURN.
          ENDTRY.
        ENDIF.
      ELSE.
        LOOP AT l_errors INTO l_error.
          READ TABLE <fs_grid_tab> ASSIGNING <fs_grid_line> INDEX l_error-rowno.
          ASSIGN COMPONENT 'ERRORCELLMESSAGE' OF STRUCTURE <fs_grid_line> TO FIELD-SYMBOL(<fs_errcellmessage>).
          <fs_errcellmessage> = <fs_errcellmessage> && ' ' && l_error-err_message.
          IF l_error-err_field <> ''. "highlight cell
            l_err_field_color-fname = l_error-err_field.
            l_err_field_color-color = c_err_cell_color.
            ASSIGN COMPONENT 'ERRCELLCOLOR' OF STRUCTURE <fs_grid_line> TO <fs_errcellcolor>.
            APPEND l_err_field_color TO <fs_errcellcolor>.
          ELSE. "highlight whole line
            ASSIGN COMPONENT 'LINECOLOR' OF STRUCTURE <fs_grid_line> TO <fs_linecolor>.
            <fs_linecolor> = 'C611'.
          ENDIF.
        ENDLOOP.
        MESSAGE 'Data saving issues' TYPE 'S'.
      ENDIF.
      "everything what hasn't been saved will have to be either changed again or deleted anyway, so it naturally goes again into g_map_grid_change_tab
      CLEAR g_map_grid_change_tab.
      refresh_grid( ).
    ELSE.
      MESSAGE 'No changes to save' TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD set_grid_structure.
* it sets g_accrt, g_area, g_type, g_strucname
    DATA:
      l_str_supported_result_type TYPE /ba1/if_in_hx_helper=>ty_str_supported_result_types.
    g_accrt = p_accrt.
    TEST-SEAM cl_al_br_cus_bs.
      /ba1/cl_al_br_cus_bs=>get_instance( )->get_result_type( EXPORTING i_result = p_accrt
                                                              IMPORTING e_result_area = g_area e_result_type = g_type ).
    END-TEST-SEAM.
    /ba1/cl_in_hx_helper=>s_get_instance( )->get_supported_result_types( EXPORTING i_area = g_area
                                                                                   i_type = g_type
                                                                         IMPORTING e_str_supported_result_type = l_str_supported_result_type ).
    TRY.
        g_strucname = l_str_supported_result_type-tab_supp_cluster[ result_storage = p_accrt ]-strucname.
      CATCH cx_sy_itab_line_not_found.
        g_strucname = l_str_supported_result_type-strucname_rdl.
    ENDTRY.
    g_versioning_schema = l_str_supported_result_type-version_schema.
  ENDMETHOD.


  METHOD switch_grid_names.
    DATA:
      l_fieldcat     TYPE lvc_t_fcat,
      l_master_field TYPE lvc_s_fcat.
    FIELD-SYMBOLS:
        <fs_field>          TYPE lvc_s_fcat.

    g_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = l_fieldcat ).
    IF g_is_tech_names = ''.
      g_is_tech_names = 'X'.
      LOOP AT l_fieldcat ASSIGNING <fs_field>.
        <fs_field>-scrtext_l = <fs_field>-scrtext_m = <fs_field>-scrtext_s = <fs_field>-reptext = <fs_field>-seltext = <fs_field>-fieldname.
      ENDLOOP.
    ELSE.
      g_is_tech_names = ''.
      LOOP AT l_fieldcat ASSIGNING <fs_field>.
        READ TABLE g_master_fieldcat WITH KEY fieldname = <fs_field>-fieldname INTO l_master_field BINARY SEARCH.
        <fs_field>-scrtext_l = l_master_field-scrtext_l.
        <fs_field>-scrtext_m = l_master_field-scrtext_m.
        <fs_field>-scrtext_s = l_master_field-scrtext_s.
        <fs_field>-seltext   = l_master_field-seltext.
        <fs_field>-reptext   = l_master_field-reptext.
      ENDLOOP.
    ENDIF.
    g_grid->set_frontend_fieldcatalog( EXPORTING
                                            it_fieldcatalog = l_fieldcat ).
    refresh_grid( ).
  ENDMETHOD.
ENDCLASS.
