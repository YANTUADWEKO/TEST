@EndUserText.label: 'Custom View - ABAP Log Object - Register'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_Z_C_ABAPLOG_OBJ_1_IMPL'
@UI.presentationVariant: [
  {
    qualifier: 'ContractView',
    text: 'Contract View',
    requestAtLeast: [
      '/ba1/k5samobj',
      '/ba1/obj_curr'
    ],
    sortOrder: [
      {
        by: '/ba1/c55contid',
        direction: #ASC
      },
      {
        by: '/ba1/obj_curr',
        direction: #ASC
      }
    ] }]
define custom entity Z_C_ABAPLOG_OBJ_2
{
      @UI            : {
      lineItem       : [ { label: 'Log Number', hidden: true } ]
      }
  key lognumber      : balognr;

      @UI            : {
      lineItem       : [ { label: 'Log Handle', hidden: true } ]
      }
  key log_handle     : balloghndl;

      @UI            : {
      lineItem       : [ { label: 'External ID', hidden: true } ]
      }
  key extnumber      : balnrext;

      @UI            : {
      lineItem       : [ { position: 10, label: 'Log Msg. Num.' } ]
      }
  key msg_num        : integer;

      @UI            : {
      lineItem       : [ { position: 30, type: #FOR_INTENT_BASED_NAVIGATION, semanticObjectAction: 'Show' } ]
      }
      @Consumption.semanticObject: 'Z_HW_RESULTVIEWER'
  key /ba1/c55contid : /ba1/bf_dte_contract_id;

      @UI            : {
      lineItem       : [ { position: 40 } ]
      }
  key /ba1/c35btsrc  : /ba1/f2_dte_bt_source_sys;

      @UI            : {
      lineItem       : [ { position: 50 } ]
      }
      
  key /ba1/c35btran  : /ba1/f2_dte_bt_id;

      @UI            : {
      lineItem       : [ { position: 60 } ]
      }
  key /ba1/c35posnr  : /ba1/f2_dte_bt_pos_number;

      @UI            : {
      lineItem       : [ { hidden: true } ]
      }
      /ba1/c55accsy  : /ba1/br_dte_acc_system;

      @UI            : {
      lineItem       : [ { position: 20, label: 'Status' }]
      }
      msgty          : symsgty;

      @UI            : {
      lineItem       : [ { position: 70 } ]
      }
      @Semantics.amount.currencyCode: '/ba1/obj_curr'
      @Aggregation.default: #SUM
      /ba1/k5samobj  : /ba1/bf_dte_amount_objcurr;

      @UI            : {
      lineItem       : [ { label: 'Trans. Currency', position: 80 } ]
      }
      @Semantics.currencyCode: true
      /ba1/obj_curr  : /ba1/fu_dte_objcurr;
      
//      Criticality doesn't work for aggregated field
//      @UI            : {
//      lineItem       : [ { label: 'Criticality', hidden: true } ]
//      }
//      criticality    : symsgty;

      _handle        : association to parent Z_C_ABAPLOG_2 on  _handle.log_handle = $projection.log_handle
                                                           and _handle.lognumber  = $projection.lognumber
                                                           and _handle.extnumber  = $projection.extnumber;
}
