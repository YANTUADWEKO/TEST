@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'FPSL SLPD view entity - read only'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
//@Search.searchable: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity Z_I_SLPD_R
  as select from /ba1/hfspd as SLPD_R

  association [1..1] to /BA1/CBR_LGENT as LGENT on $projection./ba1/c55lgent = LGENT.legal_entity
{

  key /ba1/c55postd,
      @Consumption.valueHelpDefinition: [{  entity: {name: '/BA1/CBR_LGENT', element: 'legal_entity'}  }]
  key /ba1/c55lgent,
      //      @Search.defaultSearchElement: true
      //      @Search.fuzzinessThreshold: 0.90
  key /ba1/c55docnum,

  key /ba1/c55docitm,

  key /ba1/c55accsy,
  key /ba1/c55prcsct,
  key /ba1/k11prtky,
  key /ba1/cr0rda,
  key /ba1/cr0type,
      /ba1/cr0cruser,
      /ba1/cr0tstmp,
      /ba1/c93tcid,
      /ba1/k11rndm,
      /ba1/cr0ndtp00,
      /ba1/kr0spdhd,
      /ba1/cr0ndtp01,
      /ba1/kr0spdit,
      /ba1/cr0ndtp02,
      /ba1/kr0spdgp,
      archive_flud                                        as ArchiveFlud,
      archive_status                                      as ArchiveStatus,
      rejection                                           as Rejection,
      /ba1/c55accsc,
      /ba1/cr0scrfdt,
      /ba1/c55ascrid,
      /ba1/c55dchtxt,
      /ba1/c55ppcrus,
      /ba1/c55pprlus,
      /ba1/c55year,
      /ba1/c55docdat,
      /ba1/c55valdt,
      cast (/ba1/c55yper as abap.numc(7))                 as /ba1/c55yper,
      /ba1/c55fxref,

      /ba1/c35btran,

      /ba1/c35btsrc,
      /ba1/c35trxtyp,
      /ba1/c35revind,
      /ba1/c35rvrfbt,
      /ba1/c35btidrf,
      /ba1/c35btinrf,
      /ba1/c20tpart,
      /ba1/c35posnr,
      /ba1/c35datsrc,
      /ba1/c35timsrc,
      /ba1/c20tporgu,
      /ba1/c55tpcocr,
      /ba1/c55postrc,
      /ba1/c55blart,
      /ba1/c55comp,
      /ba1/c55bukrs,
      /ba1/c55coarea,
      /ba1/c55cocntr,
      /ba1/c55contid,
      /ba1/c55contct,
      /ba1/c41finst,
      /ba1/c11nodeno,
      /ba1/c55secnno,
      /ba1/c55invind,
      /ba1/c55invref,
      /ba1/c55resind,
      /ba1/c55resref,
      /ba1/c55prkref,
      /ba1/c55pdres,
      /ba1/c55procid,
      /ba1/c55rmcuct,
      /ba1/cidpseid,
      /ba1/crcleid,
      /ba1/c80bussg,
      /ba1/cp0prfctr,
      /ba1/c55pcomp,
      /ba1/c55pbukrs,
      /ba1/c80pbussg,
      /ba1/cp0partpc,
      /ba1/c55tpcomp,
      /ba1/c55tpbkrs,
      /ba1/c80tpbssg,
      /ba1/c80tppctr,
      /ba1/c55dbcdf,
      /ba1/c55slacc,
      /ba1/c55slacur,
      /ba1/c55consrc,
      /ba1/c55consta,
      /ba1/c41srcsys,
      /ba1/c41objsta,
      /ba1/c55onstir,
      /ba1/c55statsg,
      /ba1/c11prdctr,
      /ba1/c55saccct,
      /ba1/c11fitype,
      /ba1/cr4pfct,
      /ba1/c55trapr,
      /ba1/c55prodsg,
      /ba1/c55slalc,
      /ba1/c55ocybas,
      /ba1/c80orguni,
      /ba1/c55chgrsn,
      /ba1/c55clcltr,
      /ba1/c55cmeth,
      /ba1/c55cmethy,
      /ba1/c55ioind,
      /ba1/c55acckey,
      /ba1/c55methal,
      /ba1/c58sptfct,
      /ba1/c58sptfid,
      /ba1/crccsdat,
      /ba1/c55tcntry,
      /ba1/c55tcode,
      /ba1/c55tcoder,
      /ba1/c55bpcocr,
      /ba1/c55bpins,
      /ba1/c55bpinst,
      /ba1/c55bpisgt,
      /ba1/c55acqdat,
      /ba1/c55acldat,
      /ba1/c55ocyacq,
      /ba1/c55r5cnid,
      /ba1/c55r0cnid,
      /ba1/c55r1cnid,
      /ba1/c55r2cnid,
      /ba1/c55r3cnid,
      /ba1/c55r4cnid,
      /ba1/c55inccat,
      /ba1/c55insein,
      /ba1/c55locomp,
      /ba1/c55loreco,
      /ba1/c55slagrp,
      /ba1/crcbasam,
      /ba1/crcbecfvn,
      /ba1/crcbecfvr,
      /ba1/crccanind,
      /ba1/crccfcatg,
      /ba1/crpibty,
      /ba1/crpiigr,
      /ba1/crptrdat,
      /ba1/crcpaycat,
//      @DefaultAggregation: #SUM
      @Semantics.amount.currencyCode: '/ba1/objCurr'
      /ba1/k5samobj,

      /ba1/obj_curr                                       as /ba1/objCurr,
      @Semantics.amount.currencyCode: '/ba1/k55amnomc'
      /ba1/k55amnom,
      /ba1/k55amnomc,
      @Semantics.quantity.unitOfMeasure: 'unit'
      /ba1/k55quant,
      unit                                                as Unit,
      @Semantics.amount.currencyCode: '/ba1/bilCurr'
      /ba1/k5sambal,
      /ba1/bil_curr                                       as /ba1/bilCurr,
      @Semantics.amount.currencyCode: '/ba1/grpCurr'
      /ba1/k5samgrp,
      /ba1/grp_curr                                       as /ba1/grpCurr,
      @Semantics.amount.currencyCode: '/ba1/hrdCurr'
      /ba1/k5samhrd,
      /ba1/hrd_curr                                       as /ba1/hrdCurr,
      @Semantics.amount.currencyCode: '/ba1/idxCurr'
      /ba1/k5samidx,
      /ba1/idx_curr                                       as /ba1/idxCurr,
      @Semantics.amount.currencyCode: '/ba1/locCurr'
      /ba1/k5samloc,
      /ba1/loc_curr                                       as /ba1/locCurr,
      @Semantics.amount.currencyCode: '/ba1/k5samac1c'
      /ba1/k5samac1,
      /ba1/k5samac1c,
      @Semantics.amount.currencyCode: '/ba1/k5samac2c'
      /ba1/k5samac2,
      /ba1/k5samac2c,
      @Semantics.amount.currencyCode: '/ba1/k5samac3c'
      /ba1/k5samac3,
      /ba1/k5samac3c,
      @Semantics.amount.currencyCode: '/ba1/k5samac4c'
      /ba1/k5samac4,
      /ba1/k5samac4c,
      @Semantics.amount.currencyCode: '/ba1/objCurr'
      /ba1/k5staxbo,
      @Semantics.amount.currencyCode: '/ba1/objCurr'
      /ba1/k5staxbr,
      dummy_sldoc_itm_procchar                            as DummySldocItmProcchar,
      /ba1/c55accpr,
      /ba1/c55lotid,
      /ba1/c55covsps,
      /ba1/c55alst,
      /ba1/c55holdct,
      /ba1/c58accrst,
      /ba1/c58impsta,
      /ba1/c58mcnfst,
      /ba1/c58wdst,
      /ba1/c55fvlevl,
      /ba1/c55onrst,
      /ba1/c55termsg,
      /ba1/imove_type                                     as /ba1/imoveType,
      /ba1/igl_account                                    as /ba1/iglAccount,
      /ba1/ichrt_accts                                    as /ba1/ichrtAccts,
      /ba1/c55fssubs,
      dummy_sldoc_gaap_procchar                           as DummySldocGaapProcchar,
      dummy_spd_db_ch_sap                                 as DummySpdDbChSap,
      /ba1/c11book,
      /ba1/c20bpart,
      /ba1/c20bpgrpk,
      /ba1/c20bpind,
      /ba1/c20bpinsy,
      /ba1/c20bporgu,
      /ba1/c35dirpcc,
      /ba1/c35ppstat,
      /ba1/c55bpinsg,
      /ba1/cp0partle,
      /b20c/s_cbustrnc                                    as /b20c/sCbustrnc,
      /bic/z_partner                                      as /bic/zPartner,
      /bic/z_account                                      as /bic/zAccount,
      /1ba/_ba1c_dctry                                    as /1ba/Ba1cDctry,
      /bic/zhsbankid,
      /bic/zhsbnkacc,
      /bic/z_test1                                        as /bic/zTest1,
      /bic/z_product                                      as /bic/zProduct,
      /bic/zherkunft,
      /bic/zcbrchnm,
      /bic/zcbrctrn,
      /bic/ztrntp,
      /bic/zccanal,
      /bic/zcgrntia,
      /bic/zcpais,
      /bic/zcprdct,
      /bic/zcsbprdct,
      /bic/zcsbsctor,
      /bic/zcscrsl,
      /bic/zcsector,
      /bic/zcsku,
      /bic/zcstsdtrr,
      /bic/zcunddngc,
      /bic/z_field                                        as /bic/zField,
      /bic/zccdgtrns,
      /bic/zccncpt,
      /bic/zcodcon,
      /bic/zctop,
      dummy_spd_db_kf_sap                                 as DummySpdDbKfSap,

      concat(concat(/ba1/c55docnum, '/'), /ba1/c55docitm) as doc_key,

      LGENT
}
