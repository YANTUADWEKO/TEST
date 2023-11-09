@AbapCatalog.sqlViewName: 'ZI_FPSL_SLDOCS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AccessControl.personalData.blocking: #BLOCKED_DATA_EXCLUDED
@EndUserText.label: 'ZI_FPSL_SubledgerDocuments'

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

define view ZI_FPSL_SubledgerDocuments 
as select from /ba1/hfspd 
//association [0..1] to ZI_LEGALENTITY_TXT as _FPSLegalEntity on  $projection.LegalEntity = _FPSLegalEntity.FPSLegalEntity

{
    @EndUserText.label: 'Legal Entity'
 //   @ObjectModel.foreignKey.association: '_FPSLegalEntity'
//    @Analytics.internalName: #GLOBAL
       
    key /ba1/c55lgent  as LegalEntity,
//    @EndUserText.label:'G/L Account'
//    key /ba1/igl_account   as GLAccount,
    @EndUserText.label: 'Journal Entry Number'
    key /ba1/c55docnum as JournalEntryNumber,
    @EndUserText.label:'Amount in Transaction Currency'
    @DefaultAggregation: #SUM
    @Semantics.amount.currencyCode: 'ObjectCurrTransactionCurrency'
    /ba1/k5samobj      as AmountinTransactionCurrency,
    @EndUserText.label:'Object Currency/Transaction Currency'
    @Semantics.currencyCode: true
    /ba1/obj_curr      as ObjectCurrTransactionCurrency,
    @EndUserText.label:'Posting Date'
    /ba1/c55postd as PostingDate,
    @EndUserText.label:'Fiscal Year'
    /ba1/c55year as Fisc_Year,
    @EndUserText.label:'Year from Date'
    left(/ba1/c55postd,4) as Posting_YEAR
 //   _FPSLegalEntity
} //where /BA1/C55YEAR = left( $parameters.p_date,4)
where /ba1/c55lgent = 'A750' and (/ba1/c55docnum = '00000000000000000551')
