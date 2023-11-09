*----------------------------------------------------------------------*
***INCLUDE LZFPSL_TOOLSF01.
*----------------------------------------------------------------------*

FORM check_schema.

  IF zfpsl_stagetrunc-staging_schema IS INITIAL or zfpsl_stagetrunc-staging_table IS INITIAL."Schema and Tables fields are mandatory
    MESSAGE e025(zfpsl_tools) DISPLAY LIKE 'I'.
  elseif zfpsl_stagetrunc-staging_schema EQ 'SAPHANADB'."SAPHANADB schema is not allowed
    CLEAR zfpsl_stagetrunc-staging_schema.
    MESSAGE e024(zfpsl_tools) DISPLAY LIKE 'I'.
  elseif zfpsl_stagetrunc-retention_days = '0000'. "Retention days cannot be null
    MESSAGE e026(zfpsl_tools) DISPLAY LIKE 'I'.
  ENDIF.

ENDFORM.
