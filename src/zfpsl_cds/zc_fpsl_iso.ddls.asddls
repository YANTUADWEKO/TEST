@AbapCatalog.sqlViewName: 'ZCFPSLISO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption CDS View for ISO'

@VDM.viewType: #CONSUMPTION

@Analytics.query: true

@Analytics.settings.maxProcessingEffort: #HIGH
@ClientHandling.algorithm: #SESSION_VARIABLE
@AbapCatalog.buffering.status: #NOT_ALLOWED

@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.usageType.dataClass:  #MIXED
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.representativeKey: ['JournalEntryNumber']
@ClientHandling.type: #CLIENT_DEPENDENT

@OData.publish: true

define view ZC_FPSL_ISO 
with parameters c_date: dats
as select from ZI_FPSL_ISO (p_date: $parameters.c_date) 
{
    key PostingDate,
    key LegalEntity,
    key JournalEntryNumber,
    key JournalEntryLineItemNumber,
    key AccountingSystem,
    AmountinTransactionCurrency,
    ObjectCurrTransactionCurrency,
    PreliminaryGeneralJournalEntry,
    /* Associations */
    _FPSLegalEntity
}
