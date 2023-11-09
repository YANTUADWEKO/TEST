@AbapCatalog.sqlViewName: 'ZI_FPSL_GLDOCREC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZC_FPSL_GLDocumentRecon'

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

define view ZC_FPSL_GLDocumentRecon as select from /ba1/hfgpd as A
left outer join acdoca as B
    on A./ba1/c55docnum = B.aworg {
    key /ba1/c55docnum
    
}
