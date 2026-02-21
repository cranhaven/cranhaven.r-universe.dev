
#martEnv <- new.env()
#assign("ensembl1", biomaRt::useEnsembl("ensembl",mirror = "useast"),envir = martEnv)
#ensembl1 <- martEnv[["ensembl1"]]

####################### CHILDREN ##################

xx.ch <- as.list(GO.db::GOBPCHILDREN)
xx.ch1 <- as.list(GO.db::GOMFCHILDREN)
xx.ch2 <- as.list(GO.db::GOCCCHILDREN)
offspringbp <- as.list(GO.db::GOBPOFFSPRING)
offspringmf <- as.list(GO.db::GOMFOFFSPRING)
offspringcc <- as.list(GO.db::GOCCOFFSPRING)
################## Ancentors #######################

xx.an <- as.list(GO.db::GOBPPARENTS)
xx.an1 <- as.list(GO.db::GOMFPARENTS)
xx.an2 <- as.list(GO.db::GOCCPARENTS)

############### Environment ########################

go2h <- new.env() # for BP GOTERMS
go2h1 <- new.env() # for MC GOTERMS
go2h2 <- new.env() # for CC GOTERMS

####################################################
## Building the three different Ontologies using root BP, MF and CC GO root ####

GOTermsApp.B("GO:0008150")
GOTermsApp.M("GO:0003674")
GOTermsApp.C("GO:0005575")

SupportedOrganism <- c("HOMO SAPIENS","HUMAN", "RATTUS NORVEGICUS", "RAT", "MUS MUSCULUS", "MOUSE", "DANIO RERIO" ,"ZEBRAFISH",
                       "CAENORHABDITIS ELEGANS", "WORM", "ARABIDOPSIS THALIANA", "CRESS", "SACCHAROMYCES CEREVISIAE", "YEAST",
                       "SCHIZOSACCHAROMYCES POMBE", "FISSION YEAST","DROSOPHILA MELANOGASTER", "FRUIT FLY","ESCHERICHIA COLI",
                       "E.COLI")


SupportedOrganismv2 <- c("HOMO SAPIENS / HUMAN", "RATTUS NORVEGICUS / RAT", "MUS MUSCULUS / MOUSE", "DANIO RERIO / ZEBRAFISH",
                       "CAENORHABDITIS ELEGANS / WORM", "ARABIDOPSIS THALIANA / CRESS", "SACCHAROMYCES CEREVISIAE / YEAST",
                       "SCHIZOSACCHAROMYCES POMBE / FISSION YEAST", "DROSOPHILA MELANOGASTER / FRUIT FLY","ESCHERICHIA COLI / E.COLI")

################################## Loading internal data (To be commented later)

#all_bp_go_tree <- load("/home/facihul/Desktop/GO PROJECT/biological_t_edgetree.RData")
#all_mf_go_tree <- load("/home/facihul/Desktop/GO PROJECT/molecular_t_edgetree.RData")
#all_cc_go_tree <- load("/home/facihul/Desktop/GO PROJECT/cellular_t_edgetree.RData")


#all_bp_leaf_nodes <- load("/home/facihul/Desktop/GO PROJECT/edgebp.RData")
#all_mf_leaf_nodes <- load("/home/facihul/Desktop/GO PROJECT/edgemf.RData")
#all_cc_leaf_nodes <- load("/home/facihul/Desktop/GO PROJECT/edgecc.RData")


### This was the version of the save data prior to GOxploreR version 1.2.0
#usethis::use_data(EdgeBP,EdgeMF,EdgeCC, biological_f_edgelist, molecular_f_edgelist, cellular_f_edgelist, BP, MF, CC,
#                BPTair, CCTair, MFTair, BPElegan, CCElegan, MFElegan,
#                 BPDrosophila, CCDrosophila, MFDrosophila, BPHuman, CCHuman, MFHuman, BPMouse, CCMouse, MFMouse,
#                  BPRat, CCRat, MFRat, BPPombe, CCPombe, MFPombe, BPYeast, CCYeast, MFYeast, BPZebrafish, CCZebrafish, MFZebrafish,
#                  GOgeneralbp, GOgeneralmf, GOgeneralcc,AthalianAll,DrosophilaAll, EleganAll,
#                 HumanAll,MouseAll,PombeAll,RatAll,YeastAll,ZebrafishAll, Athalian, Drosophila, Elegan,
#                 Human, Mouse, Pombe, Rat, Yeast, Zebrafish, BPEcoli, CCEcoli, MFEcoli, EcoliAll, Ecoli,
#               internal = TRUE, overwrite = TRUE)

#usethis::use_data(g.GO_DAG.BP, g.GO_DAG.MF, g.GO_DAG.CC,internal = TRUE)

### package internal data for GOxploreR version 1.2.0
# usethis::use_data(biological_f_edgelist, molecular_f_edgelist, cellular_f_edgelist, BP, MF,CC,
#                  BPTair, CCTair, MFTair, BPElegan, CCElegan, MFElegan,
#                  BPDrosophila, CCDrosophila, MFDrosophila, BPHuman, CCHuman, MFHuman , BPPombe,
#                   CCPombe, MFPombe, BPYeast, CCYeast, MFYeast, BPZebrafish, CCZebrafish, MFZebrafish,
#                   GOgeneralbp, GOgeneralmf, GOgeneralcc,AthalianAll,DrosophilaAll, EleganAll,
#                   HumanAll,PombeAll,YeastAll,ZebrafishAll, Athalian, Drosophila, Elegan,
#                   Human, Pombe, Yeast, Zebrafish, BPEcoli, CCEcoli, MFEcoli, EcoliAll, Ecoli,
#                   internal = TRUE, overwrite = TRUE)

############################## organism supported ##############################
#' Title
#'
#' @param x species name
#' @keywords internal
#' @return GO objects from ensembl

goterm.org <- function(x){
  if(toupper(x) == "HOMO SAPIENS" || toupper(x) == "HUMAN"){
    ensembl <- useEnsembl("ensembl")
    ensembl = useDataset("hsapiens_gene_ensembl",mart=ensembl)
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    human <- list()
    human[["ensembl"]] <- ensembl
    human[["filters"]] <- filters
    human[["attributes"]] <- attributes
    return(human)

  }else if(toupper(x) == "RATTUS NORVEGICUS" || toupper(x) == "RAT"){
    ensembl <- useEnsembl("ensembl",mirror = "useast")
    ensembl = useDataset("rnorvegicus_gene_ensembl",mart=ensembl)
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    rat <- list()
    rat[["ensembl"]] <- ensembl
    rat[["filters"]] <- filters
    rat[["attributes"]] <- attributes
    return(rat)

  }else if(toupper(x) == "MUS MUSCULUS" || toupper(x) == "MOUSE"){
    ensembl <- useEnsembl("ensembl",mirror = "useast")
    ensembl = useDataset("mmusculus_gene_ensembl",mart=ensembl)
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    mouse <- list()
    mouse[["ensembl"]] <- ensembl
    mouse[["filters"]] <- filters
    mouse[["attributes"]] <- attributes
    return(mouse)

  }else if(toupper(x) == "DANIO RERIO" || toupper(x) == "ZEBRAFISH"){
    ensembl <- useEnsembl("ensembl",mirror = "useast")
    ensembl = useDataset("drerio_gene_ensembl",mart=ensembl)
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    zebrafish <- list()
    zebrafish[["ensembl"]] <- ensembl
    zebrafish[["filters"]] <- filters
    zebrafish[["attributes"]] <- attributes
    return(zebrafish)

  }else if(toupper(x) == "DROSOPHILA MELANOGASTER" || toupper(x) == "FRUIT FLY"){
    ensembl <- useEnsembl("ensembl",mirror = "useast")
    ensembl = useDataset("dmelanogaster_gene_ensembl",mart=ensembl)
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    zebrafish <- list()
    zebrafish[["ensembl"]] <- ensembl
    zebrafish[["filters"]] <- filters
    zebrafish[["attributes"]] <- attributes
    return(zebrafish)

  }else if(toupper(x) == "CAENORHABDITIS ELEGANS" || toupper(x) == "WORM"){
    ensembl <- useEnsembl("ensembl",mirror = "useast")
    ensembl = useDataset("celegans_gene_ensembl",mart=ensembl)
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    elegans <- list()
    elegans[["ensembl"]] <- ensembl
    elegans[["filters"]] <- filters
    elegans[["attributes"]] <- attributes
    return(elegans)

  }else if(toupper(x) == "ARABIDOPSIS THALIANA" || toupper(x) == "CRESS" ){
    ensembl <- useMart(biomart = "plants_mart",
    dataset = "athaliana_eg_gene", host = "plants.ensembl.org")
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    cress <- list()
    cress[["ensembl"]] <- ensembl
    cress[["filters"]] <- filters
    cress[["attributes"]] <- attributes
    return(cress)

  }else if(toupper(x) == "SACCHAROMYCES CEREVISIAE" || toupper(x) == ("YEAST") ){
    ensembl <- useEnsembl("ensembl",mirror = "useast")
    ensembl = useDataset("scerevisiae_gene_ensembl",mart=ensembl)
    filters = listFilters(ensembl)
    attributes = listAttributes(ensembl)
    yeast <- list()
    yeast[["ensembl"]] <- ensembl
    yeast[["filters"]] <- filters
    yeast[["attributes"]] <- attributes
    return(yeast)

  }else if(toupper(x) == "SCHIZOSACCHAROMYCES POMBE" || toupper(x) == "FISSION YEAST"){
    ensembl = useMart(biomart = "fungi_mart",dataset = "spombe_eg_gene", host = "fungi.ensembl.org")
      filters = listFilters(ensembl)
      attributes = listAttributes(ensembl)
      pombe <- list()
      pombe[["ensembl"]] <- ensembl
      pombe[["filters"]] <- filters
      pombe[["attributes"]] <- attributes
      return(pombe)
  }

}















