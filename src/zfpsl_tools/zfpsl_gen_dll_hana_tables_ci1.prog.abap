*&---------------------------------------------------------------------*
*& Include          ZFPSL_GEN_DLL_HANA_TABLES_CI1
*&---------------------------------------------------------------------*

CLASS lcl_dll_hana IMPLEMENTATION.

  METHOD constructor.
*/  selection parameters
    gv_testrun = iv_testrun .
    gv_package = iv_package .
    gv_dbcon   = iv_dbcon   .
    gv_schema  = iv_schema  .

  ENDMETHOD.

  METHOD start.

    fill_alv_dll_hana( ).
    display_alv( ).

  ENDMETHOD.

  METHOD fill_alv_dll_hana.

    get_abap_structures( IMPORTING et_abap = DATA(staging_structures) ) .
    get_hana_objects( IMPORTING et_hana = DATA(hana_objects) ) .

    LOOP AT staging_structures ASSIGNING FIELD-SYMBOL(<str>).
      APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<alv>).
      <alv>-abap_structure = <str>-tabname.
      IF line_exists( hana_objects[ _tab-table_name = <str>-tabname ] ).
        <alv>-hana_object    = <str>-tabname.
      ELSE.
        <alv>-hana_object    = 'NOT CREATED'.
      ENDIF.
      IF     <alv>-abap_structure CP 'ZHDT_*'.  " table
        <alv>-hana_type = 'T'.
      ELSEIF <alv>-abap_structure CP 'ZHDV_*'.  " view
        <alv>-hana_type = 'V'.
      ENDIF.
    ENDLOOP.

    LOOP AT hana_objects ASSIGNING FIELD-SYMBOL(<hana>).
      IF line_exists( staging_structures[ tabname = <hana>-_tab-table_name ] ).
        CONTINUE. " added in first loop
      ELSE.
        APPEND INITIAL LINE TO gt_alv ASSIGNING <alv>.
        <alv>-abap_structure = 'NOT EXISITING'.
        <alv>-hana_schema    = <hana>-_tab-schema.
        <alv>-hana_object    = <hana>-_tab-table_name.
        <alv>-hana_type      = <hana>-table_type.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_abap_structures.

    DATA rc TYPE char1.
    DATA ddfields TYPE STANDARD TABLE OF DDFIELD.

    SELECT tabname FROM info_tabl INTO CORRESPONDING FIELDS OF TABLE @et_abap
         WHERE tabclass EQ 'INTTAB'
           AND devclass EQ @gv_package.

    LOOP AT et_abap ASSIGNING FIELD-SYMBOL(<str>).
      <str>-_fields = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( <str>-tabname )
                                                                 )->get_ddic_field_list( ).
      ddfields = CORRESPONDING #( <str>-_fields ).
      PERFORM map_ddfields IN PROGRAM sdb2fhdb IF FOUND

        TABLES ddfields "<str>-_fields
               <str>-_dbftype
        USING abap_true abap_true
        CHANGING rc.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_hana_objects.

    DATA ls_hana    LIKE LINE OF et_hana.
    DATA rng_schema TYPE adbc_name_rgtab.

    TRY.
        DATA(lo_conn)   = cl_sql_connection=>get_connection( EXPORTING sharable = abap_true ).
        DATA(lo_meta)   = lo_conn->get_metadata( ).

        rng_schema = VALUE #( ( sign = 'I' option = 'EQ' low =  gv_schema ) ).

        lo_meta->get_tables( EXPORTING schema_rgtab    = rng_schema
                             IMPORTING table_descr_tab = DATA(lt_tab) ).

        DATA(lo_details) = NEW /ba1/cl_al_l1_sql_metadata_hdb( lo_conn ).
        LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<tab>).
          ls_hana-_tab = <tab>.
          lo_details->get_columns( EXPORTING i_schema_name = CONV #( <tab>-schema )
                                             i_table_name = CONV #( <tab>-table_name )
                                   IMPORTING e_t_column = ls_hana-_fields ).
          APPEND ls_hana TO et_hana.
        ENDLOOP.
        lo_conn->close( ).

      CATCH cx_sql_exception INTO DATA(lx).
        BREAK-POINT. " todo
    ENDTRY.

  ENDMETHOD.

  METHOD display_alv.

    DATA lr_column TYPE REF TO cl_salv_column_table.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lr_salv)
                                CHANGING  t_table      = gt_alv ).

        lr_salv->set_screen_status( report        = 'SAPLSALV'
                                    pfstatus      = 'STANDARD'
                                    set_functions = lr_salv->c_functions_all ).

        lr_salv->get_columns( )->set_optimize( ) .
*        lr_column ?= lr_salv->get_columns( )->get_column( 'ICON_ID' ) .
*        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*        lr_column->set_short_text( TEXT-l01 ).
*        lr_column->set_medium_text( TEXT-l02 ).
*        lr_column->set_long_text( TEXT-l02 ).

*        SET HANDLER on_display_attachment FOR lr_salv->get_event( ).

        lr_salv->display( ).

      CATCH cx_salv_msg
            cx_salv_not_found INTO DATA(lr_cx_salv).
        MESSAGE lr_cx_salv TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD on_display_details.

*    READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX row.
*    CHECK sy-subrc EQ 0.
*
*    CASE column.
*      WHEN 'ICON_ID'.
*
*    ENDCASE.


  ENDMETHOD.

ENDCLASS.
