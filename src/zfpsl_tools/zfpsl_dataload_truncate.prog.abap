*---------------------------------------------------------------------*
* generated at 13.02.2023 20:35:07 by CSCHMIDT in S4H-110
*---------------------------------------------------------------------*
REPORT ZFPSL_DATALOAD_TRUNCATE.

INCLUDE /ba1/fw_process_macros_hw.

mac_process_begin ZFPSL_DATALOAD_TRUNCATE.


mac_block_begin spec.
        mac_param_single          01 /BA1/C11KEYDAT /BA1/F3_DTE_KEYDATE .



mac_keep_proc_cat 3.
mac_param_stepsq.

mac_block_end spec.

mac_block_begin dag.
mac_block_end dag.

mac_block_begin tech.
mac_block_tech.
mac_block_end tech.
mac_free_selection_non_display.

      mac_param_request 01 single.




mac_process_end.
