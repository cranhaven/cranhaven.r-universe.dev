
#' Get the Gene Ontology (GO) terms associated with a gene(s) and their levels
#'
#' @param genes A character vector of entrezgene id's belonging to an organism of interest
#'
#' @param organism An object of class character. A character string that defines the scientific / common name of the organism eg.
#'                 Homo sapiens, Mouse.
#'
#' @param domain An optional object of class character. GO-terms have three domains .i.e. biological process (BP),
#'            molecular function (MF) and cellular component (CC). This character string identifies which of the three GO
#'            categories associated with the genes to return. GO terms from one domain can be returned at a time.
#'            If NULL the biological process GO-terms will be obtained (default)
#'
#' @importFrom annotate Ontology
#' @importFrom biomaRt useEnsembl
#' @importFrom biomaRt listFilters listAttributes useDataset getBM
#'
#' @description Retrieves the gene association GO-terms, also, the ontology of the terms as
#'              well as the respective levels based on the directed acyclic graph (DAG)
#'              are returned. A fast internet connection is needed for this funciton
#'              execution. The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'              "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress",
#'              "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast", "Drosophila melanogaster / Fruit fly".
#'
#' @return A data.frame object that contains the genes, GO-terms, ontology and GO-levels
#' @export
#'
#' @note This function does not provide support for Escherichia coli.
#' @examples
#' \donttest{
#' # human genes
#' v <- c(6713,4605, 10212, 9833, 6713)
#'
#' # No value for domain is given so the default ("BP") is used
#' Gene2GOTermAndLevel_ON(genes = v, organism = "Human")
#'
#' # The scientific names of the species can also be used
#' Gene2GOTermAndLevel_ON(genes = v, organism = "Homo sapiens", domain = "CC")
#'
#' Gene2GOTermAndLevel_ON(genes = v, organism = "Human", domain = "MF")
#' }
#'
#'
Gene2GOTermAndLevel_ON <- function(genes, organism, domain  = NULL){
  ontology <- domain
  Organism <- organism

  if(is.null(genes)){
    stop("The genes are not given with no default")
  }
  if(!is.null(domain) && toupper(domain) != "BP" && toupper(domain) != "MF" && toupper(domain) != "CC"){
    stop("The argument \"domain\" can only be \"BP\", \"MF\" or \"CC\".")
  }

  if(!(toupper(Organism) %in% SupportedOrganism[-c(length(SupportedOrganism), length(SupportedOrganism) - 1)])){
    print(SupportedOrganismv2[-length(SupportedOrganismv2)])
    stop("The \"organism\" argument should be given from the list above.")
  }
  species <- Organism
  gonames <- c("Entrezgene ID", "GO ID", "Domain")
  go.org <- goterm.org(species)
  ensembl <- go.org$ensembl
  attributes <- go.org$attributes
  filters <- go.org$filters

  Proc <- TRUE
  tryCatch({
    goids = getBM(                            # GOterms
      attributes = c('entrezgene_id','go_id'),
      filters='entrezgene_id',
      values = genes,
      mart=ensembl
      )
  }, error = function(e){
    Proc = FALSE
    stop("Make sure you have a good internet connection and that the genes belong to the organism in question")
  },silent = FALSE)

  if(Proc){
    J <- goids

    a <- which(J[,2] == '')
    if(length(a) != 0){
      J <- J[-a,]
    }

    if(length(J[,2]) > 0){
      gene_ontology <- Ontology(J[,2])
    }else{
      return(warning("make sure the genes belongs to the organism in question."))
    }

    J <- cbind(J, gene_ontology)

    a1 <- which(is.na(J[,3]))
    if(length(a1 )!= 0){
      J <- J[-a1,]
    }

    colnames(J) <- gonames

    if(length(J) != 0){
      if(is.null(ontology) || toupper(ontology) == "BP"){
        dval <- which(!J[,3] == 'BP')
        if(length(dval) > 0){
          J <- J[-dval,]
        }

        level <- lapply(J[,2], function(x) go2h[[x]][length(go2h[[x]])] - 1)   # Get BP Goterm levels
        h <- which(level == 'NULL')
        if(length(h) == 0){
          level <- unlist(level)
          Level <- level
          J <- cbind(J,Level)
        }else{level[h] = 'NA'
        level <- unlist(level)
        Level <- level
        J <- cbind(J,Level)
        J <- J[-which(J[,4] == 'NA'),]}
        rownames(J) <- NULL
        return(J)

      }else if(toupper(ontology) == "MF"){
        dval <- which(!J[,3] == 'MF')
        if(length(dval) > 0){
          J <- J[-dval,]
        }

        level <- lapply(J[,2], function(x) go2h1[[x]][length(go2h1[[x]])] - 1)   # Get BP Goterm levels
        h <- which(level == 'NULL')
        if(length(h) == 0){
          level <- unlist(level)
          Level <- level
          J <- cbind(J,Level)
        }else{level[h] = 'NA'
        level <- unlist(level)
        Level <- level
        J <- cbind(J,Level)
        J <- J[-which(J[,4] == 'NA'),]}
        rownames(J) <- NULL
        return(J)

      }else if(toupper(ontology) == "CC"){
        dval <- which(!J[,3] == 'CC') # Remove goterms with a different ontology
        if(length(dval) > 0){
          J <- J[-dval,]
        }

        level <- lapply(J[,2], function(x) go2h2[[x]][length(go2h2[[x]])] - 1)   # Get BP Goterm levels
        h <- which(level == 'NULL')
        if(length(h) == 0){
          level <- unlist(level)
          Level <- level
          J <- cbind(J,Level)
        }else{level[h] = 'NA'
        level <- unlist(level)
        Level <- level
        J <- cbind(J,Level)
        J <- J[-which(J[,4] == 'NA'),]}
        rownames(J) <- NULL
        return(J)
      }
    }

  }


}

