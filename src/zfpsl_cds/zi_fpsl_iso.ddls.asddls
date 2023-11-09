@AbapCatalog.sqlViewName: 'ZIFPSLISO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite Interface CDS View for ISO'

@VDM.viewType: #COMPOSITE
@Analytics.dataCategory: #CUBE
@ClientHandling.type: #CLIENT_DEPENDENT

define view ZI_FPSL_ISO 
with parameters p_date: dats
as select from /ba1/hfspd as A
left outer join /ba1/hfglc as B
    on A./ba1/c55postd = B./ba1/c55postd
    and A./ba1/c55lgent = B./ba1/c55lgent
    and A./ba1/c55docnum = B./ba1/c55docnum
    and A./ba1/c55docitm = B./ba1/c55docitm
    and A./ba1/c55accsy = B./ba1/c55accsy
    
association [0..1] to I_FPSLegalEntity  as _FPSLegalEntity  on  $projection.LegalEntity = _FPSLegalEntity.FPSLegalEntity
{
    @EndUserText.label: 'Posting Date'
    key A./ba1/c55postd  as PostingDate,
    @EndUserText.label: 'Legal Entity'
    @ObjectModel.foreignKey.association: '_FPSLegalEntity'
    key A./ba1/c55lgent  as LegalEntity,
    @EndUserText.label:'Journal Entry Number'
    key A./ba1/c55docnum as JournalEntryNumber,
    @EndUserText.label:'Journal Entry Line Item Number'
    key A./ba1/c55docitm as JournalEntryLineItemNumber,
    @EndUserText.label:'Accounting System'
    key A./ba1/c55accsy  as AccountingSystem, 
    @EndUserText.label:'Amount in Transaction Currency'
    @DefaultAggregation: #SUM
    @Semantics.amount.currencyCode: 'ObjectCurrTransactionCurrency'
    A./ba1/k5samobj      as AmountinTransactionCurrency,
    @EndUserText.label:'Object Currency/Transaction Currency'
    A./ba1/obj_curr      as ObjectCurrTransactionCurrency,
    @EndUserText.label:'Preliminary General Journal Entry'
    B./ba1/c55docngl    as PreliminaryGeneralJournalEntry,
    _FPSLegalEntity 
}
where A./ba1/c55postd = $parameters.p_date
