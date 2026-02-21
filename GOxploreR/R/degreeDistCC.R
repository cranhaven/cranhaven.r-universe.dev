
#' GO cellular component (CC) level leaf nodes getter
#'
#' @param organism An organism of interest from the list of supported organism. If the  argument is NULL the results are
#'                 from the general GO-DAG (default).
#'
#' @param level A numeric value for the GO level
#' @return  All leaf nodes on the level
#' @export
#' @import gontr
#'
#' @description Derive all the leaf nodes from a GO CC level. The supported organism are "Homo sapiens / Human",
#'             "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm",
#'             "Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'             "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#' @examples
#' # level 6 leaf nodes for Yeast GO-DAG
#' Level2LeafNodeCC(6, "Yeast")
#'
#' # level 4 leaf nodes for Mouse GO-DAG
#' Level2LeafNodeCC(4, "Mouse")
#'
Level2LeafNodeCC <- function(level, organism = NULL){
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
      y <- Level2GOTermCC(Level - 1)
      dat <- lapply(as.list(y), function(i){
        if(all(is.na(xx.ch2[[i]]))){return(i)}})
      return(unlist(dat))

    },error = function(e){
      print(paste("No such level exist for the general GO cc tree, the highest level is", 17 , sep = " "))

    })

  }
  else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      leafNode <- CCHuman$df[Level,][3]
      if(leafNode != 0){
        return(CCHuman$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(CCHuman$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      leafNode <- gontr::CCRat$df[Level,][3]
      if(leafNode != 0){
        return(gontr::CCRat$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::CCRat$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      leafNode <- gontr::CCMouse$df[Level,][3]
      if(leafNode != 0){
        return(gontr::CCMouse$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::CCMouse$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({

      leafNode <- CCZebrafish$df[Level,][3]
      if(leafNode != 0){
        return(CCZebrafish$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(CCZebrafish$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      leafNode <- CCElegan$df[Level,][3]
      if(leafNode != 0){
        return(CCElegan$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(CCElegan$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      leafNode <- CCTair$df[Level,][3]
      if(leafNode != 0){
        return(CCTair$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(CCTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      leafNode <- CCYeast$df[Level,][3]
      if(leafNode != 0){
        return(CCYeast$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(CCYeast$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      leafNode <- CCPombe$df[Level,][3]
      if(leafNode != 0){
        return(CCPombe$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(CCPombe$df[,1]) - 1, sep = " "))
    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      leafNode <- CCDrosophila$df[Level,][3]
      if(leafNode != 0){
        return(CCDrosophila$v[[as.character(leafNode)]])
      }
      else{
        print(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(CCDrosophila$df[,1]) - 1, sep = " "))

    })
  }
  else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      leafNode <- CCEcoli$df[Level,][3]
      if(leafNode != 0){
        return(CCEcoli$v[[as.character(leafNode)]])
      }
      else{
        print(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Escherichia coli , the highest level is", length(CCEcoli$df[,1]) - 1, sep = " "))
    })
  }
}


#' Degree distribution of the GO cellular component (CC) terms on a GO level
#'
#' @param level A numeric value for the GO level
#'
#' @return A plot showing the degree distribution
#' @export
#'
#' @description For a directed graph, the in-degree nodes are the nodes which have edges coming into them
#'              and the out-degree nodes are those which have edges going out of them. The degreeDistCC
#'              function shows the distribution of these degrees over a particular GO level. A bar plot is obtain
#'              which shows how many nodes in the GO level have a certain degree k.
#'
#' @examples
#' # Degree distribution of GO terms on level 8
#' degreeDistCC(level = 8)
#'
#' # Degree distribution of GO terms on level 6
#' degreeDistCC(level = 6)
#'
degreeDistCC <- function(level){
  Level <- level
  if(is.numeric(Level) && Level <= 17){
    dat  <- Level2GOTermCC(Level)
    dat1 <- Level2GOTermCC(Level-1)
    i.degree <- lapply(dat, function(y){
      if(all(is.na(xx.ch2[[y]]))){y <- 0}
      else{length(xx.ch2[[y]])}})

    out.degree <- lapply(dat, function(x){
      y <- sum(xx.an2[[x]] %in% dat1)})
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


#' Non-leaf GO-terms on a GO CC level
#'
#' @param level A numeric value for the GO level
#'
#' @param organism The organism of interest from the list of supported organism. If NULL the results are from the general GO-DAG (default)
#'
#' @return A set of non-leaf GO terms
#'
#' @description This function returned all the non-leaf GO terms on a particular GO CC level. The supported
#'              organism are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast", "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli"
#' @export
#' @import gontr
#'
#' @examples
#' # Non-leaf nodes from level 16 for Yeast CC GO-DAG
#' Level2NoLeafNodeCC(level = 16, "Yeast")
#'
#' # Non-leaf nodes from level 10 for Mouse CC GO-DAG
#' Level2NoLeafNodeCC( 10, "Mus musculus")
#'
Level2NoLeafNodeCC <- function(level, organism = NULL){
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
      leaf_node <- Level2LeafNodeCC(Level - 1)
      non_leaf_node <- Level2GOTermCC(Level - 1)
      return(non_leaf_node[!non_leaf_node %in% leaf_node])

    },error = function(e){
      print(paste("No such level exist for the General GO CC tree, the highest level is", 17 , sep = " "))

    })
  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      nonLeafNode <- CCHuman$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCHuman$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(CCHuman$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      nonLeafNode <- gontr::CCRat$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(gontr::CCRat$v[[as.character(i)]])
        }
      })
      return(unlist(l))
    },error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::CCRat$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      nonLeafNode <- gontr::CCMouse$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(gontr::CCMouse$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::CCMouse$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      nonLeafNode <- CCZebrafish$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCZebrafish$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(CCZebrafish$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      nonLeafNode <- CCElegan$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCElegan$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(CCElegan$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      nonLeafNode <- CCTair$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCTair$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(CCTair$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      nonLeafNode <- CCYeast$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCYeast$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(CCYeast$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      nonLeafNode <- CCPombe$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCPombe$v[[as.character(i)]])
        }
      })
      return(unlist(l))


    },error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(CCPombe$df[,1]) - 1, sep = " "))

    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      nonLeafNode <- CCDrosophila$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCDrosophila$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(CCDrosophila$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      nonLeafNode <- CCEcoli$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(CCEcoli$v[[as.character(i)]])
        }
      })
      return(unlist(l))
    },error = function(e){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(CCEcoli$df[,1]) - 1, sep = " "))
    })
  }
}

#' Get the level of a cellular component (CC) GO term's children
#'
#' @param goterm A character string of a valid gene ontology id e.g "GO:0005737"
#'
#' @return A list of children terms and their respective levels
#' @export
#'
#' @description This function retrieves a GO CC term children's level i.e. for a GO-term it's children level are derived.
#' @examples
#' # Retrieve "GO:0005737" children's level
#' GOTermCC2ChildLevel("GO:0005737")
#'
#' # Retrieve "GO:0099568" children's level
#' GOTermCC2ChildLevel("GO:0099568")


GOTermCC2ChildLevel <- function(goterm){
  Goid <- goterm
  go_children <- xx.ch2[[Goid]]
  if(!all(is.na(go_children))){
    dat <- lapply(go_children, function(y){
      d <- go2h2[[y]][length(go2h2[[y]])] - 1
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















