@AbapCatalog.sqlViewName: 'zks_test_v'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS-Test Kai Sicker'

@OData.publish: true

define view zks_test_dd as select from cobk {
    //cobk
    mandt,
    key belnr,
    kokrs,
    gjahr,
    versn,
    vrgng,
    timestmp,
    perab,
    perbi,
    bldat,
    budat
}
where kokrs = 'DB01'