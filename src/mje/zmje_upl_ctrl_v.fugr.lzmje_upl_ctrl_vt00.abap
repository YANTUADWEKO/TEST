*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 17.11.2021 at 22:36:07
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMJE_UPL_CTRL_V.................................*
TABLES: ZMJE_UPL_CTRL_V, *ZMJE_UPL_CTRL_V. "view work areas
CONTROLS: TCTRL_ZMJE_UPL_CTRL_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMJE_UPL_CTRL_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMJE_UPL_CTRL_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMJE_UPL_CTRL_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMJE_UPL_CTRL_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMJE_UPL_CTRL_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMJE_UPL_CTRL_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMJE_UPL_CTRL_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMJE_UPL_CTRL_V_TOTAL.

*.........table declarations:.................................*
TABLES: RSDIOBJT                       .
TABLES: ZMJE_UPL_CTRL                  .
