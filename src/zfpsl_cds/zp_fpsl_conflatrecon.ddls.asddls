@AbapCatalog.sqlViewName: 'ZP_FPSL_CONFLAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZP_FPSL_CONFLATRecon'
define view ZP_FPSL_CONFLATRecon as select from /ba1/f1_con_flat {
    key substring(ext_contract,6,35) as ext_contract,
    key left(bus_valid_from,8) as Bus_Date,
    original_system
}
