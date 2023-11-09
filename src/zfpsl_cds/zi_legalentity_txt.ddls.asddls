@AbapCatalog.sqlViewName: 'ZI_LEGALENTITY_T'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Legal Entity'
@VDM.viewType: #BASIC
@Analytics.dataCategory: #DIMENSION
@ObjectModel.representativeKey: 'FPSLegalEntity'
@Search.searchable: true

@ClientHandling.algorithm: #SESSION_VARIABLE

@ObjectModel.usageType.sizeCategory: #L
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.usageType.dataClass: #CUSTOMIZING

define view ZI_LEGALENTITY_TXT as select from /ba1/tbr_lgentd as lgent

association [0..*] to ZI_LEGALENTITY_DESC as _Text on $projection.FPSLegalEntity = _Text.FPSLegalEntity

{
    @Search.defaultSearchElement: true
    @Search.ranking : #HIGH
    @ObjectModel.text.association: '_Text'  
    key lgent.legal_entity as FPSLegalEntity,
    
    @Search.defaultSearchElement: true         //Trial if this makes the legal entity text searchable
    _Text 
}
