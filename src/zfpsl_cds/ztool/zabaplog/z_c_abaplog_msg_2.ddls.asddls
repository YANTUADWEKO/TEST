@EndUserText.label: 'Custom View - ABAP Log Messages'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_Z_C_ABAPLOG_MSG_1_IMPL'

define custom entity Z_C_ABAPLOG_MSG_2
{
      @UI         : {
      lineItem    : [ { label: 'Log Number', hidden: true } ]
      }
  key lognumber   : balognr;

      @UI         : {
      lineItem    : [ { label: 'Log Handle', hidden: true } ]
      }
  key log_handle  : balloghndl;

      @UI         : {
      lineItem    : [ { label: 'External ID', hidden: true } ]
      }
  key extnumber   : balnrext;

      @UI         : {
      lineItem    : [ { position: 10, label: 'Log Msg. Num.' } ]
      }
  key msg_num     : integer;

      @UI         : {
      lineItem    : [ { position: 20, label: 'Message Type', criticality: 'criticality' } ],
      selectionField: [{position: 10 }],
      fieldGroup  : [ { position: 10, label: 'Message Type', qualifier: 'GeneralGroup', criticality: 'criticality' }]
          }
      msgty       : symsgty;
      @UI         : {
      lineItem    : [ { position: 30, label: 'Message ID' } ],
      fieldGroup  : [{position: 15, label: 'Message ID', qualifier: 'GeneralGroup' }]
      }
      msgid       : symsgid;
      @UI         : {
      lineItem    : [ { position: 40, label: 'Message No.' } ],
      fieldGroup  : [{position: 20, label: 'Message No.', qualifier: 'GeneralGroup' }]
      }
      msgno       : symsgno;
      @UI         : {
      fieldGroup  : [{position: 10, label: 'Message Variable 1', qualifier: 'TechnicalGroup' }]
      }
      @EndUserText: {
      label       : 'Message Variable 1'
      }
      msgv1       : symsgv;
      @UI         : {
      fieldGroup  : [{position: 20, label: 'Message Variable 2', qualifier: 'TechnicalGroup' }]
      }
      @EndUserText: {
      label       : 'Message Variable 2'
      }
      msgv2       : symsgv;
      @UI         : {
      fieldGroup  : [{position: 30, label: 'Message Variable 3', qualifier: 'TechnicalGroup' }]
      }
      @EndUserText: {
      label       : 'Message Variable 3'
      }
      msgv3       : symsgv;
      @UI         : {
      fieldGroup  : [{position: 40, label: 'Message Variable 4', qualifier: 'TechnicalGroup' }]
      }
      @EndUserText: {
      label       : 'Message Variable 4'
      }
      msgv4       : symsgv;
      @UI         : {
      lineItem    : [ { position: 50, label: 'Message Text' } ]
      }
      @Search     : {
      defaultSearchElement: true,
      fuzzinessThreshold: 0.90
      }
      text        : abap.string(300);
      @UI         : {
      lineItem    : [ { label: 'Criticality', hidden: true } ]
      }
      criticality : symsgty;

      _handle     : association to parent Z_C_ABAPLOG_2 on  _handle.log_handle = $projection.log_handle
                                                        and _handle.lognumber  = $projection.lognumber
                                                        and _handle.extnumber  = $projection.extnumber;
}
