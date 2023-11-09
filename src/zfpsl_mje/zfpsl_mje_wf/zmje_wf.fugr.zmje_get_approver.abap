FUNCTION zmje_get_approver.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      AC_CONTAINER STRUCTURE  SWCONT
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"----------------------------------------------------------------------
  DATA:
    lv_ou        TYPE short_d,
    lv_requester TYPE varyf,
    ls_appr_pos  TYPE hrp1001,
    lt_hrp1001   TYPE STANDARD TABLE OF hrp1001.

*  CALL FUNCTION 'SWC_ELEMENT_GET'
*    EXPORTING
*      element   = 'ORG_UNIT'
*    IMPORTING
*      field     = lv_ou
*    TABLES
*      container = ac_container
*    EXCEPTIONS
*      not_found = 8
*      is_null   = 4
*      OTHERS    = 1.

  lv_ou = ac_container[ element = 'ORG_UNIT' ]-value.
  lv_requester = ac_container[ element = 'ORG_OBJECT' ]-value.

  IF lt_hrp1001 IS INITIAL.

    SELECT * FROM hrp1001
    INTO TABLE @lt_hrp1001      WHERE
            begda <= @sy-datlo  AND
            endda >= @sy-datlo  AND
            plvar = '01'.
    IF sy-subrc <> 0.
      MESSAGE e401(zfpsl_mje).
      RETURN.
    ENDIF.
  ENDIF.

  LOOP AT lt_hrp1001 ASSIGNING FIELD-SYMBOL(<str_hrp1001>)
*        Find requester's org structure position - ONLY 1 GROUP SHALL RETURN!
      WHERE   varyf = lv_requester
      AND     relat = '008'
      AND     rsign = 'A'
      AND     otype = 'S'.

*      Find requester's position's org unit superior position
    READ TABLE lt_hrp1001 WITH KEY objid = <str_hrp1001>-objid relat = '002' rsign = 'A' otype = 'S' INTO ls_appr_pos.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF ls_appr_pos IS INITIAL.
    MESSAGE e402(zfpsl_mje) WITH lv_requester.
    RETURN.
  ENDIF.

*    Return all users under this position
  LOOP AT lt_hrp1001 ASSIGNING <str_hrp1001>
      WHERE   objid = ls_appr_pos-sobid
      AND     relat = '008'
      AND     rsign = 'A'.

    APPEND VALUE #( otype = <str_hrp1001>-sclas objid = <str_hrp1001>-sobid ) TO  actor_tab .
  ENDLOOP.
ENDFUNCTION.
