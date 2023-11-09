@AbapCatalog.sqlViewName: 'ZC_FPSL_BTREG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZC_FPSL_BTRegister'
@VDM.viewType: #CONSUMPTION

@Analytics.query: true

@Analytics.settings.maxProcessingEffort: #HIGH
@ClientHandling.algorithm: #SESSION_VARIABLE
@AbapCatalog.buffering.status: #NOT_ALLOWED

@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.dataClass:  #MIXED
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.representativeKey: ['BusinessTransactionID', 'SourceSystem', 'ItemNumber', 'PartitioningKey']
@ClientHandling.type: #CLIENT_DEPENDENT

@OData.publish: true
@Analytics.dataExtraction.enabled: true

define view ZC_FPSL_BTRegister as select from ZI_FPSL_BTRegister {
 key BusinessTransactionID,
 key SourceSystem,
 key ItemNumber,
 key PartitioningKey,
 RandomNumber,
 PostingDate,
 TimeCreated,
 ProcessStatus,
 @AnalyticsDetails.query.display: #KEY_TEXT
 ErrorStatus,
 ContractPortfolioID,
 ContractPortfolioCategory   
}
