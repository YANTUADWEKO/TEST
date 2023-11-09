@AbapCatalog.sqlViewName: 'ZI_FPSL_GLSAITA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_FPSL_ReportingGLSAITA'

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

define view ZI_FPSL_ReportingGLSAITA as select from /ba1/hfspd as A
left outer join /ba1/hfglc as B
on A./ba1/c55postd = B./ba1/c55postd
and A./ba1/c55lgent = B./ba1/c55lgent
and A./ba1/c55docnum = B./ba1/c55docnum
and A./ba1/c55docitm = B./ba1/c55docitm
and A./ba1/c55accsy = B./ba1/c55accsy
and A./ba1/c55prcsct = B./ba1/c55prcsct
and A./ba1/k11prtky = B./ba1/k11prtky

left outer join /ba1/br_glc_stat as C
on B./ba1/c55docngl = C./ba1/c55docnum
and B./ba1/c55lgent = C./ba1/c55lgent
//and B./ba1/k11prtky = C./ba1/k11prtky
//and B./ba1/k11prtkg = C./ba1/k11prtky
{
    @EndUserText.label: 'Posting Date'
    key A./ba1/c55postd  as PostingDate,
    @EndUserText.label: 'Legal Entity'
//    @ObjectModel.foreignKey.association: '_FPSLegalEntity'
    @Analytics.internalName: #GLOBAL
    key A./ba1/c55lgent  as LegalEntity,
    @EndUserText.label:'Journal Entry Number'
    key A./ba1/c55docnum as JournalEntryNumber,
    @EndUserText.label:'Journal Entry Line Item Number'
    key A./ba1/c55docitm as JournalEntryLineItemNumber,
    @EndUserText.label:'Accounting System'
    key A./ba1/c55accsy  as AccountingSystem,
    @EndUserText.label:'Process Category'
    key A./ba1/c55prcsct as ProcessCategory,
    @EndUserText.label:'Partitioning Key'
    key A./ba1/k11prtky  as PartitioningKey,
    @EndUserText.label:'Results Data Area'
    key A./ba1/cr0rda    as ResultsDataArea,
    @EndUserText.label:'Result Type'
    key A./ba1/cr0type   as ResultType,
    @EndUserText.label:'Amount in Transaction Currency'
    @DefaultAggregation: #SUM
    @Semantics.amount.currencyCode: 'ObjectCurrTransactionCurrency'
    A./ba1/k5samobj      as AmountinTransactionCurrency,
    @EndUserText.label:'Object Currency/Transaction Currency'
    @Semantics.currencyCode: true
   /ba1/obj_curr      as ObjectCurrTransactionCurrency,
 //   C./ba1/c55lgent,
 //   C./ba1/c55year,
 //   C./ba1/c55docnum,
 //   C./ba1/k11prtky,
    C./ba1/c55glcst
 //   C./ba1/c77runid,
 //   C.timestamp_cre as TimestampCre,
  //  C.timestamp_chg as TimestampChg,
  //  C./ba1/c55prcsct,
  //  C.rdl_storage as RdlStorage,
   // C.preparation_categ as PreparationCateg
}
