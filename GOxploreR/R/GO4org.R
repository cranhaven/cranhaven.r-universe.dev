
#' All gene association GO-terms for an organism
#'
#' @description The function contains all the GO-terms associated with the genes of an organism and their levels.
#'              If "BP", "MF" or "CC" the GO-terms from the whole ontology is return and their levels.
#'              The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'              "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress",
#'              "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @param organism The organism supported by the package. Both the scientific / common name of the organism can be use
#' @param domain     The ontology of the GO-terms. The default is BP.
#' @return A two-column matrix that contains the GO-terms and their GO-levels
#' @export
#'
#' @import gontr
#' @examples
#' # Return all the BP GO-terms associated with organism Homo sapiens
#' GO4Organism(organism = "Homo sapiens")
#'
#' # Return all the CC GO-terms associated with organism Rat
#' GO4Organism(organism = "Rat", domain = "CC")

GO4Organism <- function(organism, domain = "BP"){
  species <- organism
  ont <- domain
  gonames <- c("Entrezgene ID", "GO ID", "Ont.", "Level" )

  if(toupper(domain) != "BP" &&  toupper(domain) != "MF" && toupper(domain) != "CC"){
    stop("The argument \"domain\" can only be \"BP\", \"MF\" or \"CC\".")
  }

  if(is.null(species)){
    stop("The \"organism\" argument is missing with no default")
  }
  if(!is.null(species) && toupper(species) != "BP" && toupper(species) != "MF" && toupper(species) != "CC" && !(toupper(species) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above")
  }
  if(toupper(species) == "BP"){
    return(BP)
  }
  else if(toupper(species) == "MF"){
    return(MF)
  }
  else if(toupper(species) == "CC"){
    #return(CC)
    return(GOTermCCOnLevel(names(as.list(go2h2))))
  }

  else if(toupper(species) == "HOMO SAPIENS" || toupper(species) == "HUMAN"){
    goids <- HumanAll

  }else if(toupper(species) == "RATTUS NORVEGICUS" || toupper(species) == "RAT"){
    goids <- gontr::RatAll

  }else if(toupper(species) == "MUS MUSCULUS" || toupper(species) == "MOUSE"){
    goids <- gontr::MouseAll

  }else if(toupper(species) == "DANIO RERIO" || toupper(species) == "ZEBRAFISH"){
    goids <- ZebrafishAll

  }else if(toupper(species) == "CAENORHABDITIS ELEGANS" || toupper(species) == "WORM"){
    goids <- EleganAll

  }else if(toupper(species) == "ARABIDOPSIS THALIANA" || toupper(species) == "CRESS"){
    goids <- AthalianAll

  }else if(toupper(species) == "SACCHAROMYCES CEREVISIAE" || toupper(species) == "YEAST"){
    goids <- YeastAll

  }else if(toupper(species) == "SCHIZOSACCHAROMYCES POMBE" || toupper(species) == "FISSION YEAST"){
    goids <- PombeAll

  }else if(toupper(species) == "DROSOPHILA MELANOGASTER" || toupper(species) == "FRUIT FLY"){
    goids <- DrosophilaAll

  }else if(toupper(species) == "ESCHERICHIA COLI" || toupper(species) == "E.COLI"){
    goids <- EcoliAll[,c(1,2)]
  }

  J <- goids
  a <- which(J[,2] == '')
  if(length(a) != 0){
    J <- J[-a,]
  }

  gene_ontology <- Ontology(J[,2])
  J <- cbind(J , gene_ontology)

  a1 <- which(is.na(J[,3]))
  if(length(a1 )!= 0){
    J <- J[-a1,]
  }

  if(length(J) != 0){
    if(is.null(ont) || toupper(ont) == "BP"){
      J <- J[-which(!J[,3] == 'BP'),]
      go_level <- lapply(J[,2], function(x) go2h[[x]][length(go2h[[x]])] - 1)   # Get BP Goterm levels
      h <- which(go_level == 'NULL')
      if(length(h) == 0){
        go_level <- unlist(go_level)
        J <- cbind(J,go_level)
      }else{go_level[h] = 'NA'
      go_level <- unlist(go_level)
      J <- cbind(J, go_level)
      J <- J[-which(J[,4] == 'NA'),]}
      rownames(J) <- NULL
      colnames(J) <- gonames
      J <- unique(J[,c(2,4)])


      return(J)

    }else if(toupper(ont) == "MF"){
      J <- J[-which(!J[,3] == "MF"),]

      go_level <- lapply(J[,2], function(x) go2h1[[x]][length(go2h1[[x]])] - 1)   # Get BP Goterm levels
      h <- which(go_level == 'NULL')
      if(length(h) == 0){
        go_level <- unlist(go_level)
        J <- cbind(J,go_level)
      }else{go_level[h] = 'NA'
      go_level <- unlist(go_level)
      J <- cbind(J,go_level)
      J <- J[-which(J[,4] == 'NA'),]}

      colnames(J) <- gonames
      J <- unique(J[,c(2,4)])
      rownames(J) <- NULL
      return(J)

    }else if(toupper(ont) == "CC"){
      J <- J[-which(!J[,3] == "CC"),] # Remove goterms with a different ontology

      go_level <- lapply(J[,2], function(x) go2h2[[x]][length(go2h2[[x]])] - 1 )   # Get BP Goterm levels
      h <- which(go_level == 'NULL')
      if(length(h) == 0){
        go_level <- unlist(go_level)
        J <- cbind(J,go_level)
      }else{go_level[h] = 'NA'
      go_level <- unlist(go_level)
      J <- cbind(J,go_level)
      J <- J[-which(J[,4] == 'NA'),]}
      colnames(J) <- gonames
      J <- unique(J[,c(2,4)])
      rownames(J) <- NULL
      return(J)
    }
  }
}


