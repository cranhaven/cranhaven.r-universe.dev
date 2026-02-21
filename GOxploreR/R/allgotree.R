
#' Organism-specific GO-DAG edgelist
#'
#' @param organism An object of class character that represents the organism. If the organism option is
#'        "BP", "MF" or "CC" the biological process, molecular function and cellular component general GO edgelist
#'        are obtained respectively
#'
#' @param domain The ontology of the GO-terms. The default is BP.
#'
#' @return A two-column matrix of the nodes and the edges to which they are connected
#' @export
#' @import gontr
#' @description Derive the organism gene association GO-terms as an edgelist. It indicates how the terms are linked together.
#'              The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'              "Danio rerio / Zebrafish","Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast", "Drosophila melanogaster / Fruit fly", " Escherichia coli / E.coli"
#' @examples
#' # Edgelist for entire GO BP ontology
#' \donttest{
#' GetDAG(organism = "BP")
#'
#' # Edgelist for cellular component gene association GO-terms for organism Yeast
#' GetDAG(organism = "Yeast", domain = "CC")
#'
#' # Edgelist for molecular function gene association GO-terms for organism Zebrafish
#' GetDAG(organism = "Zebrafish", domain = "MF")
#' }

GetDAG <- function(organism, domain = "BP"){
  Organism <- organism
  ont <- domain
  if(is.numeric(organism)){
    stop("The \"organism\" argument should be a character string or null.")
  }

  if(toupper(domain) != "BP" &&  toupper(domain) != "MF" && toupper(domain) != "CC"){
    stop("The \"domain\" argument can only be \"BP\", \"MF\" or \"CC\".")
  }

  if(!is.null(Organism) && toupper(Organism) != "BP" && toupper(Organism) != "MF" && toupper(Organism) != "CC" && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }

  if(toupper(Organism) == "BP"){
    return(as.matrix(biological_f_edgelist))
  }
  else if(toupper(Organism) == "MF"){
    return(as.matrix(molecular_f_edgelist))
  }
  else if(toupper(Organism) == "CC"){
    return(as.matrix(cellular_f_edgelist))
  }
  else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    goids <- Human

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    goids <- gontr::Rat

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    goids <- gontr::Mouse

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    goids <- Zebrafish

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    goids <- Elegan

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    goids <- Athalian

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    goids <- Yeast

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    goids <- Pombe
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    goids <- Drosophila

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    goids <- Ecoli
  }

  if(toupper(ont) == "BP"){
    dag <- goids$BPlist
    colnames(dag) <- NULL
    return(dag)
  }
  else if(toupper(ont) == "MF"){
    dag <- goids$MFlist
    colnames(dag) <- NULL
    return(dag)
  }
  if(toupper(ont) == "CC"){
    dag <- goids$CClist
    colnames(dag) <- NULL
    return(dag)
  }

}


#' All the BP GO-DAG leaf nodes
#'
#' @param organism  An object of class character that represents an organism
#'
#' @description Returns all the leaf nodes from a particular organism-specific GO-DAG. If empty (or if "BP"), all the general BP
#'              GO-DAG leaf nodes will be returned. The supported organism are "Homo sapiens / Human", "Rattus norvegicus / Rat",
#'              "Mus musculus / Mouse", "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress",
#'              "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return A two-coloum matrix of all the leaf nodes in the organism-specific DAG and their respective levels
#' @export
#' @import gontr
#'
#' @examples
#' \donttest{
#' # General GO BP leaf nodes
#' GetLeafNodesBP("BP")
#'
#' # Human BP GO-DAG leaf nodes
#' GetLeafNodesBP(organism = "Human")
#'
#' # Mouse BP GO-DAG leaf nodes
#' GetLeafNodesBP(organism = "DANIO RERIO")
#'}
GetLeafNodesBP <- function(organism = NULL){
  Organism <- organism
  if(is.numeric(organism)){
    stop("The \"organism\" argument should be a character string or null.")
  }

  if(!is.null(Organism) && toupper(Organism) != "BP" && !(toupper(Organism) %in% SupportedOrganism) ){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }

  if(is.null(Organism) || toupper(Organism) == "BP" ){
    #return(EdgeBP)
    nonLeafNode <- GOgeneralbp$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(GOgeneralbp$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    nonLeafNode <- BPHuman$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPHuman$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))


  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    nonLeafNode <- gontr::BPRat$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(gontr::BPRat$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    nonLeafNode <- gontr::BPMouse$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(gontr::BPMouse$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    nonLeafNode <- BPZebrafish$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPZebrafish$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    nonLeafNode <- BPElegan$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPElegan$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    nonLeafNode <- BPTair$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPTair$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    nonLeafNode <- BPYeast$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPYeast$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    nonLeafNode <- BPPombe$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPPombe$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    nonLeafNode <- BPDrosophila$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPDrosophila$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))
  }
  else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    nonLeafNode <- BPEcoli$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(BPEcoli$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermBPOnLevel(l))
  }

}

#' All the MF GO-DAG leaf nodes
#'
#' @param organism  An object of class character that represents an organism.
#'
#' @description Returns all the leaf nodes from a particular organism-specific GO-DAG. If empty (or if "MF"), all
#'              the general MF GO-DAG leaf nodes are returned. The supported organisms are "Homo sapiens / Human",
#'              "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm",
#'              "Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return A two-coloum matrix of all the leaf nodes in the organism-specific DAG and their respective levels
#' @export
#' @import gontr
#'
#' @examples
#' \donttest{
#' # Mouse MF GO-DAG leaf nodes
#' GetLeafNodesMF(organism = "Human")
#'
#' # Arabidopsis thaliana MF GO-DAG leaf nodes
#' GetLeafNodesMF(organism = "Arabidopsis thaliana")
#'
#' # Drosophila melanogaster MF GO-DAG leaf nodes
#' GetLeafNodesMF(organism = "Drosophila melanogaster")
#' }

GetLeafNodesMF <- function(organism = NULL){
  Organism <- organism

  if(is.numeric(organism)){
    stop("The \"organism\" argument should be a character string or null")
  }

  if(!is.null(Organism) && toupper(Organism) != "MF" && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above")
  }

  if(is.null(Organism) || toupper(Organism) == "MF" ){
    nonLeafNode <- GOgeneralmf$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(GOgeneralmf$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    nonLeafNode <- MFHuman$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFHuman$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))


  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    nonLeafNode <- gontr::MFRat$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(gontr::MFRat$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    nonLeafNode <- gontr::MFMouse$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(gontr::MFMouse$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    nonLeafNode <- MFZebrafish$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFZebrafish$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM" ){
    nonLeafNode <- MFElegan$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFElegan$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    nonLeafNode <- MFTair$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFTair$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    nonLeafNode <- MFYeast$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFYeast$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    nonLeafNode <- MFPombe$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFPombe$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    nonLeafNode <- MFDrosophila$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFDrosophila$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    nonLeafNode <- MFEcoli$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(MFEcoli$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermMFOnLevel(l))
  }

}



#' All the CC GO-DAG leaf nodes
#'
#' @param organism  An object of class character that represents an organism
#'
#' @description Returns all the leaf nodes from a particular organism-specific GO-DAG. If empty (or if "CC"),
#'              all the general CC GO-DAG leaf nodes are returned. The supported organism are "Homo sapiens / Human",
#'              "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm",
#'              "Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return A two-coloum matrix of all leaf nodes in the organism-specific DAG and their respective levels
#' @export
#' @import gontr
#'
#' @examples
#' \donttest{
#' # Zebrafish CC GO-DAG leaf nodes
#'  GetLeafNodesCC("Danio rerio")
#'
#' # Mouse CC GO-DAG leaf nodes
#' GetLeafNodesCC("Zebrafish")
#'
#' }


GetLeafNodesCC <- function(organism = NULL){
  Organism <- organism
  if(is.numeric(Organism)){
    stop("The \"organism\" argument should be a character string or NULL.")
  }

  if(!is.null(Organism) && toupper(Organism) != "CC" && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }

  if(is.null(Organism) || toupper(Organism) == "CC" ){
    #return(EdgeCC)

    nonLeafNode <- GOgeneralcc$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(GOgeneralcc$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    nonLeafNode <- CCHuman$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCHuman$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))


  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    nonLeafNode <- gontr::CCRat$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(gontr::CCRat$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    nonLeafNode <- gontr::CCMouse$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(gontr::CCMouse$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    nonLeafNode <- CCZebrafish$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCZebrafish$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    nonLeafNode <- CCElegan$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCElegan$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    nonLeafNode <- CCTair$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCTair$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    nonLeafNode <- CCYeast$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCYeast$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    nonLeafNode <- CCPombe$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCPombe$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    nonLeafNode <- CCDrosophila$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCDrosophila$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    nonLeafNode <- CCEcoli$df[,3]
    l <- lapply(nonLeafNode, function(i){
      if(i != 0){
        return(CCEcoli$v[[as.character(i)]])
      }
    })
    l.val <- Ontology(unlist(l)); l <- unlist(l)
    a <- which(is.na(l.val))
    if(length(a) > 0){
      l <- l[-a]
    }
    return(GOTermCCOnLevel(l))
  }
}



