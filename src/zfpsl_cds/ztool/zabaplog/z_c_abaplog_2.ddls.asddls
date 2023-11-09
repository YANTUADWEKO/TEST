@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #M,
  dataClass: #MIXED     }

define root view entity Z_C_ABAPLOG_2
  as select from balhdr
  composition [1..*] of z_c_abaplog_msg_2   as _messages
  composition [1..*] of z_c_abaplog_param_2 as _params
  composition [1..*] of z_c_abaplog_obj_2   as _objects
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
      end                                  as logtype,

      @ObjectModel: {
        readOnly: true,
        virtualElement: true,
        virtualElementCalculatedBy: 'ABAP:ZCL_Z_C_ABAPLOG_1_VE_IMPL'
      }
      cast('' as boolean preserving type ) as is_cvpm,

      @ObjectModel: {
        readOnly: true,
        virtualElement: true,
        virtualElementCalculatedBy: 'ABAP:ZCL_Z_C_ABAPLOG_1_VE_IMPL'
      }
      cast('' as boolean preserving type ) as is_reg,

      case
        when msg_cnt_a is not initial or msg_cnt_e is not initial       then 1
        when msg_cnt_w is not initial                                   then 2
        else                                                                 3
      end                                  as criticality,

      _messages,
      _params,
      _objects
}
