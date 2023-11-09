@EndUserText.label: 'Custom View - ABAP Log - CVPM Parameters'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_Z_C_ABAPLOG_PARAM_1_IMPL'

@UI: {
    headerInfo: {
      typeName: 'CVPM Parameter View',
      typeNamePlural: 'CVPM Parameter Views',
      title: {
        type: #STANDARD,
        value: 'characteristic'
      },
      description: {
        value   : 'characteristic'
        }
    },
    presentationVariant: [{
        id: 'default',
        text: 'Default',
        sortOrder: [{by: 'param_id',  direction: #ASC}] }]
}
define custom entity Z_C_ABAPLOG_PARAM_1
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
      lineItem       : [{ label: 'External ID', hidden: true }]   }
  key extnumber      : balnrext;
      @UI            : {
      lineItem       : [{ label: 'Parameter ID', hidden: true }]   }
  key param_id       : abap.int1;
      @UI            : {
      lineItem       : [{ position: 10, importance: #HIGH, label: 'Parameter Type' }]   }
      char_type      : /ba1/f0_dte_txtlg;
      @UI            : {
      lineItem       : [{ position: 20, importance: #HIGH, label: 'Parameter Name' }]   }
      characteristic : /ba1/f0_dte_characteristic;
      @UI            : {
      lineItem       : [{ position: 30, importance: #HIGH, label: 'Sign' }]   }
      sign           : raldb_sign;
      @UI            : {
      lineItem       : [{ position: 40, importance: #HIGH, label: 'Option' }]   }
      opt            : raldb_opti;
      @UI            : {
      lineItem       : [{ position: 50, importance: #HIGH, label: 'Value (- Lower Limit)' }]   }
      low            : /ba1/f3_dte_char_value_low;
      @UI            : {
      lineItem       : [{ position: 60, importance: #HIGH, label: 'Value - Higher Limit' }]   }
      high           : /ba1/f3_dte_char_value_high;

      _extid         : association to parent Z_C_ABAPLOG_1 on  _extid.log_handle = $projection.log_handle
                                                           and _extid.lognumber  = $projection.lognumber
                                                           and _extid.extnumber  = $projection.extnumber;
}
