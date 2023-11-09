@AbapCatalog.sqlViewName: 'ZI_FPSL_BTREG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_FPSL_BTRegister'

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

define view ZI_FPSL_BTRegister as select from /ba1/br_reg_bt {
/*@UI.facet: [ { id:'idGeneralInformation' ,
                                 type: #COLLECTION ,
                                 label: 'General Information' ,
                                 position: 10 } ,
                               { type: #IDENTIFICATION_REFERENCE ,
                                 label : 'General Information',
                                 parentId: 'idGeneralInformation' ,
                                 id: 'idIdentification' } ]*/
    @EndUserText.label: 'Business Transaction ID'
    key bt_id as BusinessTransactionID,
    @EndUserText.label: 'Source System of Business Transaction'
    key source_system as SourceSystem,
    @EndUserText.label: 'Item Number'
    key item_number as ItemNumber,
    @EndUserText.label: 'Partitioning Key'
    key /ba1/k11prtky as PartitioningKey,
    @EndUserText.label: 'Random Number'
    /ba1/k11rndm as RandomNumber,
    @EndUserText.label: 'Posting Date'
    posting_date_ext as PostingDate,
    @EndUserText.label: 'Time Created'
    system_timestamp as TimeCreated,
    @EndUserText.label: 'Processing Status of BT'
    process_status as ProcessStatus,
    @EndUserText.label: 'Error Status for Businees Transaction'
    @UI.selectionField: [{ position: 10 }]
    @UI.lineItem: [{ position: 10 }]
    error_status as ErrorStatus,
    @EndUserText.label: 'Contract/Portfolio ID'
    /ba1/c55contid as ContractPortfolioID,
    @EndUserText.label: 'Contract/Portfolio Category'
    /ba1/c55contct as ContractPortfolioCategory
} where error_status <> '00'
