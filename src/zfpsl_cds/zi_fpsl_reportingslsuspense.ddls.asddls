@AbapCatalog.sqlViewName: 'ZFPSL_SLSUSPENSE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_FPSL_ReportingSLSuspense'

@VDM.viewType: #COMPOSITE
@Analytics.dataCategory: #CUBE
@Analytics.dataExtraction.enabled: true
@Analytics.internalName: #LOCAL

@ClientHandling.algorithm: #SESSION_VARIABLE
@AbapCatalog.buffering.status: #NOT_ALLOWED

@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.dataClass:  #MIXED
@ObjectModel.usageType.serviceQuality: #D
@ClientHandling.type: #CLIENT_DEPENDENT

define view ZI_FPSL_ReportingSLSuspense as select from /ba1/hfspd {
    *
} 
