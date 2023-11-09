@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #M,
  dataClass: #MIXED     }

define root view entity Z_C_ABAPLOG_1
  as select from balhdr
  composition [1..*] of z_c_abaplog_msg_1   as _messages
  composition [1..*] of z_c_abaplog_param_1 as _params
{

  key lognumber,

  key log_handle,

  key extnumber,

      object,

      subobject,

      aldate,

      aluser,

      alprog,

      case
        when object     = '/BA1/FW'                                     then 'CVPM'
        when object     = 'BW_PROCESS'  and subobject   = 'FS_DLL_BA'   then 'DLL'
        else                                                                 'Other'
      end as logtype,

      case
        when msg_cnt_a is not initial or msg_cnt_e is not initial       then 1
        when msg_cnt_w is not initial                                   then 2
        else                                                                 3
      end as criticality,
      
//    @Aggregation.default: #SUM
//    cast(1 as integer) as row_count,

      _messages,
      _params
}
