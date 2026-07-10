#' Create NewMsigDB object for new versions of MsigDB database
#'
#' @param xml path of xml msigdb file path
#' @importFrom set %not%
#' @export
#' @return dataframe which can be used inner package
NewMsigDB <- function(xml){
    msigdb <- read_msigdb_xml(xml)
    # colnames(msigdb)[colnames(msigdb) == 'members'] <- 'Ensembl'
    # colnames(msigdb)[colnames(msigdb) == 'members_symbolized'] <- 'Genesymbol'
    # colnames(msigdb)[colnames(msigdb) == 'members_ezid'] <- 'EntrezId'
    colnames(msigdb) <- do::Replace0(colnames(msigdb),'_code')

    msigdb <- msigdb[,colnames(msigdb) %not% c('geneset_listing_url','pmid','geoid',
                                               'filtered_by_similarity',
                                               'founder_names',
                                               'refinement_datasets',
                                               'validation_datasets',
                                               'members','members_symbolized',
                                               'members_ezid','exact_source','external_details_url')]

    msigdb <- msigdb[,c("standard_name", "systematic_name",
                        "category", "sub_category", "organism",
                        "description_brief", "description_full",
                        'members_mapping',"chip",
                        "authors","contributor", "contributor_org")]
    colnames(msigdb)[colnames(msigdb) == 'category'] <- 'collection'
    colnames(msigdb)[colnames(msigdb) == 'sub_category'] <- 'sub_collection'
    colnames(msigdb)[colnames(msigdb) == 'authors'] <- 'author'
    colnames(msigdb)[colnames(msigdb) == 'members_mapping']='gene'
    msigdb
}
