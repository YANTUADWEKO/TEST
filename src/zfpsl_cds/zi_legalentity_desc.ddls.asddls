@AbapCatalog.sqlViewName: 'ZI_LEGALENTITY_D'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Legal Entity Description'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.representativeKey: 'FPSLegalEntity'
@VDM.viewType: #BASIC

@Search.searchable: true

@ClientHandling.algorithm: #SESSION_VARIABLE

@ObjectModel.usageType.sizeCategory: #L
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.usageType.dataClass: #CUSTOMIZING

define view ZI_LEGALENTITY_DESC as select from /ba1/tbr_lgentdt  as TextProvider 
{

key TextProvider.legal_entity as FPSLegalEntity,
@Semantics.language: true
key TextProvider.langu as Language,
@Semantics.text: true
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.8  
TextProvider.description_l as FPSLegalEntityName,
@Semantics.text: true
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.8  
TextProvider.description_m as FPSLegalEntityName_M,
@Semantics.text: true
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.8  
TextProvider.description_m as FPSLegalEntityName_L


}
