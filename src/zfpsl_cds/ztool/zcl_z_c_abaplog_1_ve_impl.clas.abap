CLASS zcl_z_c_abaplog_1_ve_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_z_c_abaplog_1_ve_impl IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~calculate.
    LOOP AT it_original_data ASSIGNING FIELD-SYMBOL(<str_orig_data>).
      ASSIGN COMPONENT 'LOGTYPE' OF STRUCTURE <str_orig_data> TO FIELD-SYMBOL(<val_log_type>).
      ASSIGN COMPONENT 'IS_CVPM' OF STRUCTURE ct_calculated_data[ sy-tabix ] TO FIELD-SYMBOL(<val_is_cvpm>).
      IF <val_log_type> = 'CVPM'.
        <val_is_cvpm> = abap_false.
      ELSE.
        <val_is_cvpm> = abap_true.
      ENDIF.

      ASSIGN COMPONENT 'ALPROG' OF STRUCTURE <str_orig_data> TO FIELD-SYMBOL(<val_prog>).
      ASSIGN COMPONENT 'IS_REG' OF STRUCTURE ct_calculated_data[ sy-tabix ] TO FIELD-SYMBOL(<val_is_reg>).
      IF <val_prog> = '/BA1/RBR_REGISTER'.
        <val_is_reg> = abap_false.
      ELSE.
        <val_is_reg> = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    INSERT |LOGTYPE|    INTO TABLE et_requested_orig_elements.
    INSERT |ALPROG|     INTO TABLE et_requested_orig_elements.
  ENDMETHOD.

ENDCLASS.
