*&---------------------------------------------------------------------*
*& Report ZNBC_WRITE_DSO_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZNBC_WRITE_DSO_TEST.

*DATA: BEGIN OF line,
*        /B20C/S_BTSOURSY(10)  TYPE c,
*        /B20C/S_BTTRNSID(35)  TYPE c,
*        SOURSYSTEM(2)         TYPE c,
*      END OF line.

*DATA itab LIKE SORTED TABLE OF line WITH UNIQUE SORTED KEY /B20C/S_BTSOURSY, /B20C/S_BTTRNSID, SOURSYSTEM.
data itab type table of /BIC/AZ_NBC_0012.
data: line LIKE LINE OF itab.
Data: it_data TYPE table of /BIC/AZ_NBC_0011.

*data: line LIKE LINE OF it_data.

CONSTANTS: lc_adso_open_pos TYPE rsoadsonm VALUE 'Z_NBC_001'.

line-/B20C/S_BTSOURSY = 'ZPIPO'.
line-/B20C/S_BTTRNSID = 'ZBT_NBC_DEMO'.
line-SOURSYSTEM       = '11'.
line-/B20C/S_BTCLASS = 'SBS2'.
line-/B20C/S_BTDTORIG = '20230609'.
line-/B20C/S_BTPOSTDT = '20230609'.
line-/B20C/S_BTVALDT = '20230609'.
line-/B20C/S_DPAKID = '1'.
line-DATE0 = '20230609'.
line-/B20C/S_OBJHASH = '1'.
line-/B20C/S_BTTMORIG = '050000'.
line-/B20C/S_BTITMCLS = 'SIS2'.

append line to itab.

MOVE-CORRESPONDING itab to it_data.


if itab[] is not initial.


*  CALL FUNCTION 'RSDSO_DU_WRITE_API'
*    EXPORTING
*      I_ADSONM                  = lc_adso_open_pos
**     I_ALLOW_NEW_SIDS          = RS_C_TRUE
**     I_INSERT_ONLY             = RS_C_TRUE
**     I_TRIGGER_MERGE           = RS_C_TRUE
*      IT_DATA                   = it_data
**   IMPORTING
**     E_LINES_INSERTED          =
**     ET_MSG                    =
**     E_UPD_REQ_TSN             =
**   EXCEPTIONS
**     WRITE_FAILED              = 1
**     DATASTORE_NOT_FOUND       = 2
**     OTHERS                    = 3
*            .
*  IF SY-SUBRC = 0.
*    WRITE: 'Successful'.
*  ENDIF.

CALL FUNCTION 'RSDSO_WRITE_API'
  EXPORTING
    I_ADSONM                  = lc_adso_open_pos
*   I_ALLOW_NEW_SIDS          = RS_C_TRUE
*   I_ACTIVATE_DATA           = RS_C_FALSE
*   IT_AGGREGATION            =
    IT_DATA                   = it_data
*   I_DEBUG                   = RS_C_FALSE
* IMPORTING
*   E_LINES_INSERTED          =
*   ET_MSG                    =
*   E_UPD_REQ_TSN             =
*   ET_ACT_REQ_TSN            =
* EXCEPTIONS
*   WRITE_FAILED              = 1
*   ACTIVATION_FAILED         = 2
*   DATASTORE_NOT_FOUND       = 3
*   OTHERS                    = 4
          .
IF SY-SUBRC = 0.
 WRITE: 'Successful'.
ENDIF.


ENDIF.
