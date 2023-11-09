@AbapCatalog.sqlViewName: 'ZC_FPSL_GLDOCREC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_FPSL_GLDocumentRecon'
@VDM.viewType: #CONSUMPTION

@Analytics.query: true

@Analytics.settings.maxProcessingEffort: #HIGH
@ClientHandling.algorithm: #SESSION_VARIABLE
@AbapCatalog.buffering.status: #NOT_ALLOWED

@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.dataClass:  #MIXED
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.representativeKey: ['/ba1/c55docnum']
@ClientHandling.type: #CLIENT_DEPENDENT

define view ZI_FPSL_GLDocumentRecon as select from ZC_FPSL_GLDocumentRecon {
    key /ba1/c55docnum
}
