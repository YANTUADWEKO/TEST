@EndUserText.label: 'Custom View - ABAP Log Messages'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_Z_C_ABAPLOG_MSG_1_IMPL'

@UI: {
    headerInfo: {
      typeName: 'Message View',
      typeNamePlural: 'Message Views',
      title: {
        type: #STANDARD,
        value: 'text'
      },
      description: {
        value   : 'lognumber'
        }
    }
}
define custom entity Z_C_ABAPLOG_MSG_1
{
      @UI.facet   : [
        {
            id    : 'MessageCollection',
            type  : #COLLECTION,
            label : 'Message Information',
            position    : 10
        },
        {
            id    : 'MessageGeneral',
            purpose         : #STANDARD,
            type  : #FIELDGROUP_REFERENCE,
            parentId        : 'MessageCollection',
            label : 'General Information',
            position        : 10,
            targetQualifier : 'GeneralGroup'
        },
        {
            id    : 'Separator',
            purpose         : #STANDARD,
            type  : #FIELDGROUP_REFERENCE,
            parentId        : 'MessageCollection',
            position        : 20
        },
        {
            id    : 'MessageTechnical',
            purpose         : #STANDARD,
            type  : #FIELDGROUP_REFERENCE,
            parentId        : 'MessageCollection',
            label : 'Technical Information',
            position        : 30,
            targetQualifier : 'TechnicalGroup'
        }   ]


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
      lineItem    : [ { label: 'Message ID', hidden: true } ]
      }
  key msg_num     : integer;

      @UI         : {
      lineItem    : [ { position: 10, label: 'Message Type', criticality: 'criticality' } ],
      selectionField: [{position: 10 }],
      fieldGroup  : [ { position: 10, label: 'Message Type', qualifier: 'GeneralGroup', criticality: 'criticality' }]
          }
      msgty       : symsgty;
      @UI         : {
      lineItem    : [ { position:30, label: 'Message ID' } ],
      fieldGroup  : [{position: 15, label: 'Message ID', qualifier: 'GeneralGroup' }]
      }
      msgid       : symsgid;
      @UI         : {
      lineItem    : [ { position: 20, label: 'Message No.' } ],
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
      lineItem    : [ { position: 40, label: 'Message Text' } ]
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

      _handle     : association to parent Z_C_ABAPLOG_1 on  _handle.log_handle = $projection.log_handle
                                                        and _handle.lognumber  = $projection.lognumber
                                                        and _handle.extnumber  = $projection.extnumber;
}
