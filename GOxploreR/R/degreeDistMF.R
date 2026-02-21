
#' GO molecular function (MF) level leaf nodes getter
#'
#' @param level A numeric value for the GO-level
#'
#' @param organism An organism of interest from the list of supported organism. If NULL the results are from the general
#'                 GO-DAG (default).
#'
#' @return All leaf nodes on the level
#' @export
#' @import gontr
#' @description Derive all the leaf nodes from a GO MF level. The supported organism are "Homo sapiens / Human",
#'              "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm",
#'              "Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @examples
#' # level 9 leaf nodes for Human GO-DAG
#' Level2LeafNodeMF(9, "Human")
#'
#' # level 5 leaf nodes for Mouse GO-DAG
#' Level2LeafNodeMF(5, "Mouse")
#'
Level2LeafNodeMF <- function(level, organism = NULL){
  Organism <- organism
  if(is.null(level) || !is.numeric(level)){
    stop("The argument \"level\" is missing with no default or is non numeric")
  }

  Level <- level + 1

  if(!is.null(Organism) && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above")
  }

  if(is.null(Organism)){
    tryCatch({
      y <- Level2GOTermMF(Level - 1)
      dat <- lapply(as.list(y), function(i){
        if(all(is.na(xx.ch1[[i]]))){
          return(i)}})
      return(unlist(dat))

    },error = function(e){
      print(paste("No such level exist for the general GO mf tree, the highest level is", 16 , sep = " "))

    })

  }
  else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      leafNode <- MFHuman$df[Level,][3]
      if(leafNode != 0){
        return(MFHuman$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(MFHuman$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      leafNode <- gontr::MFRat$df[Level,][3]
      if(leafNode != 0){
        return(gontr::MFRat$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::MFRat$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      leafNode <- gontr::MFMouse$df[Level,][3]
      if(leafNode != 0){
        return(gontr::MFMouse$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::MFMouse$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({

      leafNode <- MFZebrafish$df[Level,][3]
      if(leafNode != 0){
        return(MFZebrafish$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(MFZebrafish$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      leafNode <- MFElegan$df[Level,][3]
      if(leafNode != 0){
        return(MFElegan$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(MFElegan$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      leafNode <- MFTair$df[Level,][3]
      if(leafNode != 0){
        return(MFTair$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(MFTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      leafNode <- MFYeast$df[Level,][3]
      if(leafNode != 0){
        return(MFYeast$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(MFYeast$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      leafNode <- MFPombe$df[Level,][3]
      if(leafNode != 0){
        return(MFPombe$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(MFPombe$df[,1]) - 1, sep = " "))
    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      leafNode <- MFDrosophila$df[Level,][3]
      if(leafNode != 0){
        return(MFDrosophila$v[[as.character(leafNode)]])
      }
      else{
        print(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(MFDrosophila$df[,1]) - 1, sep = " "))

    })
  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      leafNode <- MFEcoli$df[Level,][3]
      if(leafNode != 0){
        return(MFEcoli$v[[as.character(leafNode)]])
      }
      else{
        print(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(MFEcoli$df[,1]) - 1, sep = " "))

    })
  }
}


#' Degree distribution of the GO molecular function (MF) terms on a GO-level
#'
#' @param level A numeric value for the GO-level
#'
#' @return A plot showing the degree distribution
#' @export
#'
#' @description For a directed graph, the in-degree nodes are the nodes which have edges coming into them
#'              and the out-degree nodes are those which have edges going out of them. The degreeDistMF
#'              function shows the distribution of these degrees over a particular GO-level. A bar plot is obtain
#'              which shows how many nodes in the GO-level have a certain degree k.
#'
#' @examples
#' # Degree distribution of GO-terms on level 3
#' degreeDistMF(level = 3)
#'
#' # Degree distribution of GO-terms on level 10
#' degreeDistMF(level = 10)
degreeDistMF <- function(level){
  Level <- level
  if(is.numeric(Level) && Level <= 16){
    dat  <- Level2GOTermMF(Level)  # get the goterms on the level
    dat1 <- Level2GOTermMF(Level-1) # get the goterms on the level above it
    i.degree <- lapply(dat, function(y){
      if(all(is.na(xx.ch1[[y]]))){y <- 0}
      else{length(xx.ch1[[y]])}})

    out.degree <- lapply(dat, function(x){
      y <- sum(xx.an1[[x]] %in% dat1)})
    i.degree   <- table(unlist(i.degree))
    out.degree <- table(unlist(out.degree))
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mfrow=c(1,2))
    barplot(i.degree,col = "steelblue", xlab = "in degree",ylab = "Frequency",xlim = c(0,70),width = 2)
    barplot(out.degree,col = "steelblue", xlab = "out degree",ylab = "Frequency", xlim = c(0,100), width = 2.5 )
  }
  else{
    print("Incorrect value for level")
  }

}


#' Non-leaf GO-terms on a GO MF level
#'
#' @param level A numeric value for the GO-level
#'
#' @param organism The organism of interest from the list of supported organism. If null the results are from the general GO-DAG (default)
#'
#' @return A set of non-leaf GO-terms
#'
#' @description This function returned all the non-leaf GO-terms on a particular GO MF level that are not. The supported
#'              organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast", "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#' @export
#' @import gontr
#' @examples
#' # Non-leaf nodes on level 15
#' Level2NoLeafNodeMF(15)
#'
#' # Non-leaf nodes from level 10 for Rat MF GO-DAG
#' Level2NoLeafNodeMF(10, "Rat")
Level2NoLeafNodeMF <- function(level, organism = NULL){
  Organism <- organism
  if(is.null(level) || !is.numeric(level)){
    stop("The argument \"level\" is missing with no default or is non numeric")
  }

  Level <- level + 1

  if(!is.null(Organism) && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above")
  }

  if(is.null(Organism)){
    tryCatch({
      leaf_node <- Level2LeafNodeMF(Level - 1)
      non_leaf_node <- Level2GOTermMF(Level - 1)
      return(non_leaf_node[!non_leaf_node %in% leaf_node])

    },error = function(e){
      print(paste("No such level exist for the General GO MF tree, the highest level is", 16 , sep = " "))

    })
  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      nonLeafNode <- MFHuman$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFHuman$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(MFHuman$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      nonLeafNode <- gontr::MFRat$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(gontr::MFRat$v[[as.character(i)]])
        }
      })
      return(unlist(l))
    },error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::MFRat$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      nonLeafNode <- gontr::MFMouse$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(gontr::MFMouse$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::MFMouse$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      nonLeafNode <- MFZebrafish$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFZebrafish$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(MFZebrafish$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      nonLeafNode <- MFElegan$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFElegan$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(MFElegan$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      nonLeafNode <- MFTair$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFTair$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(MFTair$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      nonLeafNode <- MFYeast$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFYeast$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(MFYeast$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      nonLeafNode <- MFPombe$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFPombe$v[[as.character(i)]])
        }
      })
      return(unlist(l))


    },error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(MFPombe$df[,1]) - 1, sep = " "))

    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      nonLeafNode <- MFDrosophila$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFDrosophila$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(MFDrosophila$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      nonLeafNode <- MFEcoli$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(MFEcoli$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(MFEcoli$df[,1]) - 1, sep = " "))

    })

  }

}




#' Get the level of a molecular function (MF) GO term's children
#'
#' @param goterm A character string of a valid gene ontology id e.g "GO:0004518"
#'
#' @return A list of children terms and their respective levels
#' @export
#'
#' @description This function retrieves a GO MF term children's level i.e. for a GO-term it's children level are derived.
#' @examples
#' # Retrieve "GO:0000978" children's level
#' GOTermMF2ChildLevel(goterm = "GO:0000978")
#'
#' # Retrieve "GO:0004518" children's level
#' GOTermMF2ChildLevel(goterm = "GO:0004518")
#'
GOTermMF2ChildLevel <- function(goterm){
  Goid <- goterm
  go_children <- xx.ch1[[Goid]]
  if(!all(is.na(go_children))){
    dat <- lapply(go_children, function(y){
      d <- go2h1[[y]][length(go2h1[[y]])] - 1
    })
    dat1 <- c()
    for (i in 1:length(unlist(dat))){
      dat1 <- c(dat1,dat[[i]])
    }
    d <- lapply(go_children, function(x){
      return(x)
    })

    dUnlist <- c()
    for(k in 1:length(d)){
      dUnlist <- c(dUnlist, d[[k]])
    }

    L <- list()
    L[["Terms"]] <- dUnlist
    L[["Level"]] <- dat1
    return(L)
  }
  else{
    return(NULL)
  }
}






