@AbapCatalog.sqlViewName: 'ZFPS_CTR_SDL_3'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Contract Master Data'
@ClientHandling.type: #CLIENT_DEPENDENT
@Search.searchable: true

@ObjectModel.representativeKey: 'ExtContract'

@ObjectModel.usageType.dataClass:  #TRANSACTIONAL
@ObjectModel.usageType.serviceQuality: #A
@ObjectModel.usageType.sizeCategory: #L

@Analytics: { dataCategory: #CUBE, dataExtraction.enabled: true }


define view Z_FPS_CONTRACTSDL_2
  as select from /ba1/f1_con_flat
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
  key /ba1/f1_con_flat.ext_contract    as ExtContract,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
  key /ba1/f1_con_flat.prodtype        as ProductType,
  
  key /ba1/f1_con_flat.bus_valid_from  as ValidFrom,  // 20170915000000

  

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat.original_system as ExtContractSourceSys,  //,

//      @Search.defaultSearchElement: true
//      @Search.fuzzinessThreshold: 0.8
//      @Search.ranking: #HIGH
      /ba1/f1_con_flat./ba1/c55lgent   as LegalEntity,
      
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat.prod_control as ProductionControl, 
      
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat./ba1/cidbuscs as LineofBusiness,
      
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat.contract_cuky as ObjectCurrency,
      
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat.orgunit_owner as OrganizationalUnit,
      
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat.product as Template,
      
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat.version as TemplateVersion,
      
 //     @Search.defaultSearchElement: true
   //   @Search.fuzzinessThreshold: 0.8
 //     @Search.ranking: #HIGH
 //     /ba1/f1_con_flat.contract_startd as StartDate,
      
 //     @Search.defaultSearchElement: true
 //     @Search.fuzzinessThreshold: 0.8
 //     @Search.ranking: #HIGH
 //     /ba1/f1_con_flat.contract_endd as EndDate,
   
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      /ba1/f1_con_flat.sec_acc_cat as SecurityAccountCategory
      
     
      }
where
      mandt       = $session.client
  and sys_current = 'X'
