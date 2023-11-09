@AbapCatalog.sqlViewName: 'ZP_FPSL_SLDOCS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZP_FPSL_SUBLEDGERDOCUMENTS'

@VDM.viewType: #BASIC
@Analytics.dataCategory: #FACT
@Analytics.dataExtraction.enabled: true

define view ZP_FPSL_SUBLEDGERDOCUMENTS as select from /ba1/hfspd {
    *
}
