@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption view - MJE Eml. Tmpl. Param.'

define root view entity Z_C_MJE_EML_TPL as select from t000 {
    key 'DEFAULT'           as ID,
    cast('' as  char0016)   as initiator_id,
    cast('' as  char0016)   as approver_id,
    cast('' as  char0128)   as doc_key,
    cast('' as  char0010)   as status,
    cast('' as  char0241)   as email_link
}
