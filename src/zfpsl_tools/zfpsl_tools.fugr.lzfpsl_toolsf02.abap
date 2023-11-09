*----------------------------------------------------------------------*
***INCLUDE LZFPSL_TOOLSF02.
*----------------------------------------------------------------------*


FORM on_save_changes.


DATA(retention_days) = strlen( total ) - 7.
DATA(table) = strlen( total ) - 97.
DATA(schema) = strlen( total ) - 127.

IF <action> EQ 'N' OR <action> EQ 'U'.
  LOOP AT total.
    if total+schema(30) IS INITIAL or total+table(30) IS INITIAL.
       MESSAGE e025(zfpsl_tools) DISPLAY LIKE 'I'. "Schema and Tables fields are mandatory
    elseif total+schema(30) = 'SAPHANADB'. "SAPHANADB schema is not allowed
       MESSAGE e024(zfpsl_tools) DISPLAY LIKE 'I'.
    elseif total+retention_days(6) = '020000'. "Retention days cannot be null
       MESSAGE e026(zfpsl_tools) DISPLAY LIKE 'I'.
    endif.

  ENDLOOP.
ENDIF.
endform.
