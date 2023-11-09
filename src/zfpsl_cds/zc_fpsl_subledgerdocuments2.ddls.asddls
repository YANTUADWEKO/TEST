@AbapCatalog.sqlViewName: 'ZC_FPSL_SLDOCS2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZC_FPSL_SubledgerDocuments'

@VDM.viewType: #CONSUMPTION
@Analytics.query: true
@OData.publish: true
@Analytics.settings.maxProcessingEffort: #HIGH
@ClientHandling.algorithm: #SESSION_VARIABLE
@AbapCatalog.buffering.status: #NOT_ALLOWED
@Analytics.dataExtraction.enabled: true

@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.dataClass:  #MIXED
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.representativeKey: ['LegalEntity', 'JournalEntryNumber']
@ClientHandling.type: #CLIENT_DEPENDENT

define view ZC_FPSL_SubledgerDocuments
as select from ZI_FPSL_SubledgerDocuments // (p_date: $parameters.c_date)
//association to I_GLAccountHierarchyNode as _glhier on $projection.GLAccount = _glhier.GLAccount
{ 
     key LegalEntity,
//    @ObjectModel.hierarchy.association: '_glhier'
//     key GLAccount as GLAccount,
     key JournalEntryNumber,
     AmountinTransactionCurrency,
     ObjectCurrTransactionCurrency,
     PostingDate,
     //@Consumption.filter.mandatory: true
     //@Consumption.filter.selectionType: #INTERVAL 
     Fisc_Year,
//     @Consumption.filter.mandatory: true
 //    @Consumption.filter.selectionType: #INTERVAL 
     Posting_YEAR
 //    _glhier
     
} 
;
