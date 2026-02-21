
#' Get the Gene Ontology (GO) terms associated with a gene(s) and their levels
#'
#' @param genes   A character vector of entrezgene id's belonging to an organism of interest
#'
#' @param organism An object of class character. A character string that defines the scientific / common name of the organism  eg.
#'                Homo sapiens, Mouse.
#'
#' @param domain   An optional object of class character. GO-terms have three domains .i.e. biological process (BP),
#'                 molecular function (MF) and cellular component (CC). This character string identifies which of the three GO
#'                 categories associated with the genes to return. GO-terms from one domain can be returned at a time.
#'                 If NULL the biological process GO-terms will be obtained (default).
#'
#' @importFrom annotate Ontology
#' @importFrom biomaRt useEnsembl
#' @importFrom biomaRt listFilters listAttributes useDataset getBM useMart
#' @import gontr
#' @description Retrieves the gene association GO-terms, also, the ontology of the terms as
#'              well as the respective levels based on the directed acyclic graph (DAG)
#'              are returned. The supported organisms are "Homo sapiens / Human",
#'              "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm",
#'              "Arabidopsis thaliana / cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast", "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return A data.frame object that contains the genes, GO terms, ontology and GO-levels
#' @export
#'
#'
#' @note This function is similar to \code{\link{Gene2GOTermAndLevel_ON}}, the difference is that the function do not query the
#' Ensembl database for GO terms (It is relatively faster) which means the results from Gene2GOTermAndLevel_ON function is always up to date.
#'
#'
#' @examples
#'  # human genes
#' v <- c(6713,4605,55143,10615,10212,4001,2146,11130,983,4085,9833,9134)
#'
#' # No value for domain is given so the default ("BP") is used
#' Gene2GOTermAndLevel(genes = v, organism = "Homo sapiens")
#'
#' # The scientific names of the species can also be used
#' Gene2GOTermAndLevel(genes = v, organism = "Homo sapiens", domain = "CC")
#'
#' Gene2GOTermAndLevel(genes = v, organism = "Human", domain = "MF")
#'
Gene2GOTermAndLevel <- function(genes, organism, domain = NULL){
  ontology <- domain
  Organism <- organism

  if(is.null(genes)){
    stop("The argument \"genes\" is missing with no default.")
  }
  if(!is.null(domain) && toupper(domain) != "BP" &&  toupper(domain) != "MF" && toupper(domain) != "CC"){
    stop("The argument \"domain\" can only be \"BP\", \"MF\" or \"CC\".")
  }

  gonames <- c("Entrezgene ID", "GO ID", "Domain" )
  if(!(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }
  species <- Organism

  if(toupper(species) == "HOMO SAPIENS" || toupper(species) == "HUMAN"){
    i <- lapply(genes, function(x){
      which(HumanAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- HumanAll[i,]

  }else if(toupper(species) == "RATTUS NORVEGICUS" || toupper(species) == "RAT"){
    i <- lapply(genes, function(x){
      which(gontr::RatAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- gontr::RatAll[i,]

  }else if(toupper(species) == "MUS MUSCULUS" || toupper(species) == "MOUSE"){
    i <- lapply(genes, function(x){
      which(gontr::MouseAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- gontr::MouseAll[i,]

  }else if(toupper(species) == "DANIO RERIO" || toupper(species) == "ZEBRAFISH"){
    i <- lapply(genes, function(x){
      which(ZebrafishAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- ZebrafishAll[i,]

  }else if(toupper(species) == "CAENORHABDITIS ELEGANS" || toupper(species) == "WORM"){
    i <- lapply(genes, function(x){
      which(EleganAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- EleganAll[i,]


  }else if(toupper(species) == "ARABIDOPSIS THALIANA" || toupper(species) == "CRESS"){
    i <- lapply(genes, function(x){
      which(AthalianAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- AthalianAll[i,]

  }else if(toupper(species) == "SACCHAROMYCES CEREVISIAE" || toupper(species) == ("YEAST")){
    i <- lapply(genes, function(x){
      which(YeastAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- YeastAll[i,]

  }else if(toupper(species) == "SCHIZOSACCHAROMYCES POMBE" || toupper(species) == "FISSION YEAST"){
    i <- lapply(genes, function(x){
      which(PombeAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- PombeAll[i,]

  }else if(toupper(species) == "DROSOPHILA MELANOGASTER" || toupper(species) == "FRUIT FLY"){
    i <- lapply(genes, function(x){
      which(DrosophilaAll[,1] %in% x)
    })
    i <- unlist(i)
    J <- DrosophilaAll[i,]

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    i <- lapply(genes, function(x){
      which(EcoliAll[,1] %in% x)
    })
    i <- unlist(i)
    if(length(unlist(i)) > 0){
      J <- EcoliAll[i,c(1,2)]
    }

  }

  if(length(i) == 0){
    return(warning("make sure the genes belongs to the organism in question."))
  }

  a <- which(J[,2] == '')
  if(length(a) != 0){
    J <- J[-a,]
  }
  gene_ontology <- Ontology(J[,2])            # Retrieve ontology for the goterms
  J <- cbind(J,gene_ontology)

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
