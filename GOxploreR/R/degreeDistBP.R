
#' GO biological process (BP) level leaf nodes getter
#'
#' @param level A numeric value for the GO-level
#'
#' @param organism An organism of interest from the list of supported organism. If the parameter is NULL the results
#'                  are from the general GO-DAG (default).
#'
#'
#' @return All leaf nodes on the level.
#' @export
#' @import gontr
#'
#' @description Derive all the leaf nodes from a GO BP level. The supported organism are "Homo sapiens / Human",
#'              "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm", "Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast", "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#' @examples
#'
#' # level 2 leaf nodes for Drosophila melanogaster GO-DAG
#' Level2LeafNodeBP(2, "Drosophila melanogaster")
#'
#' # level 10 leaf nodes for Drosophila melanogaster GO-DAG
#' Level2LeafNodeBP(10, "Drosophila melanogaster")
#'
Level2LeafNodeBP <- function(level, organism = NULL){
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
      y <- Level2GOTermBP(Level - 1)
      dat <- lapply(as.list(y), function(i){
        if(all(is.na(xx.ch[[i]]))){
          return(i)}})
      return(unlist(dat))

    },error = function(e){
      print(paste("No such level exist for the general GO bp tree, the highest level is", 19 , sep = " "))

    })

  }
  else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      leafNode <- BPHuman$df[Level,][3]
      if(leafNode != 0){
        return(BPHuman$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(BPHuman$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      leafNode <- gontr::BPRat$df[Level,][3]
      if(leafNode != 0){
        return(gontr::BPRat$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::BPRat$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      leafNode <- gontr::BPMouse$df[Level,][3]
      if(leafNode != 0){
        return(gontr::BPMouse$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::BPMouse$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({

      leafNode <- BPZebrafish$df[Level,][3]
      if(leafNode != 0){
        return(BPZebrafish$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(BPZebrafish$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      leafNode <- BPElegan$df[Level,][3]
      if(leafNode != 0){
        return(BPElegan$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(BPElegan$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      leafNode <- BPTair$df[Level,][3]
      if(leafNode != 0){
        return(BPTair$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(BPTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      leafNode <- BPYeast$df[Level,][3]
      if(leafNode != 0){
        return(BPYeast$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(BPYeast$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      leafNode <- BPPombe$df[Level,][3]
      if(leafNode != 0){
        return(BPPombe$v[[as.character(leafNode)]])
      }
      else{
        return(NULL)
      }
    },error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(BPPombe$df[,1]) - 1, sep = " "))
    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      leafNode <- BPDrosophila$df[Level,][3]
      if(leafNode != 0){
        return(BPDrosophila$v[[as.character(leafNode)]])
      }
      else{
        print(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(BPDrosophila$df[,1]) - 1, sep = " "))

    })
  }
  else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      leafNode <- BPEcoli$df[Level,][3]
      if(leafNode != 0){
        return(BPEcoli$v[[as.character(leafNode)]])
      }
      else{
        print(NULL)
      }

    },error = function(e){
      print(paste("No such level exist for Escherichia coli , the highest level is", length(BPEcoli$df[,1]) - 1, sep = " "))

    })
  }
}


#' Degree distribution of the GO biological process (BP) terms on a GO-level
#'
#' @param level A numeric value for the GO-level
#'
#' @return A plot showing the degree distribution
#' @export
#'
#' @importFrom graphics par barplot
#'
#'
#' @description For a directed graph, the in-degree nodes are the nodes which have edges coming into them
#'              and the out-degree nodes are those which have edges going out of them. The degreeDistBP
#'              function shows the distribution of these degrees over a particular GO-level. A bar plot is obtain
#'              which shows how many nodes in the GO-level have a certain degree k.
#'
#' @examples
#' # Degree distribution of GO-terms on level 3
#' degreeDistBP(level = 3)
#'
#' # Degree distribution of GO-terms on level 9
#' degreeDistBP(level = 9)
#'
degreeDistBP <- function(level){
  Level <- level

  if(is.numeric(Level) && Level <= 19){
    tryCatch({
      dat  <- Level2GOTermBP(Level)
      dat1 <- Level2GOTermBP(Level - 1)
      i.degree <- lapply(dat, function(y){
        if(all(is.na(xx.ch[[y]]))){y <- 0}
        else{length(xx.ch[[y]])}})

      out.degree <- lapply(dat, function(x){
        y <- sum(xx.an[[x]] %in% dat1)})
      i.degree   <- table(unlist(i.degree))
      out.degree <- table(unlist(out.degree))
      par(mfrow=c(1,2))
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      par(mfrow=c(1,2))
      barplot(i.degree,col = "steelblue", xlab = "in degree",ylab = "Frequency",xlim = c(0,70),width = 2)
      barplot(out.degree,col = "steelblue", xlab = "out degree",ylab = "Frequency", xlim = c(0,100), width = 2.5)

    }, error = function(e){
      print("")
    })


    #i.degree <- as.data.frame(i.degree)
    #colnames(i.degree) <- c("in_degree","Frequency")
    #condition <- i.degree$in_degree
    #p <- ggplot(data=i.degree, aes(x= in_degree, y=Frequency, fill = condition))  +
     #geom_bar(stat="identity")

    #out.degree <- as.data.frame(out.degree)
    #colnames(out.degree) <- c("out_degree","Frequency")
    #condition1 <- out.degree$out_degree
    #p1 <- ggplot(data=out.degree, aes(x= out.degree$out_degree, y= out.degree$Frequency, fill = condition1,width = .5))  +
     #geom_bar(stat="identity")

    #final_ps <- grid.arrange(p, p1, nrow = 2)
  }
  else{
    print("Incorrect value for level")
  }

}



#' Non-leaf GO-terms on a GO BP level
#'
#' @param level A numeric value for the GO-level
#'
#' @param organism The organism of interest from the list of supported organism. If NULL the results are from the general
#'                 GO-DAG (default)
#'
#' @return A set of non-leaf GO-terms
#' @export
#' @import gontr
#'
#' @description This function returns all the non-leaf GO-terms on a particular GO BP level. The supported
#'             organism are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'            "Caenorhabditis elegans / Worm", "Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'            "Schizosaccharomyces pombe / Fission yeast", "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli"
#'
#' @examples
#' # Non-leaf nodes from level 13 GO-DAG
#' Level2NoLeafNodeBP(13)
#'
#' # Non-leaf nodes from level 3 for Rat BP GO-DAG
#' Level2NoLeafNodeBP(3, "Rattus norvegicus")
Level2NoLeafNodeBP <- function(level, organism = NULL){
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
      leaf_node <- Level2LeafNodeBP(Level - 1)
      non_leaf_node <- Level2GOTermBP(Level - 1)
      return(non_leaf_node[!non_leaf_node %in% leaf_node])

    },error = function(e){
      print(paste("No such level exist for the General GO BP tree, the highest level is", 19 , sep = " "))

    })
  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      nonLeafNode <- BPHuman$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPHuman$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(BPHuman$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      nonLeafNode <- gontr::BPRat$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(gontr::BPRat$v[[as.character(i)]])
        }
      })
      return(unlist(l))
    },error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::BPRat$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      nonLeafNode <- gontr::BPMouse$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(gontr::BPMouse$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::BPMouse$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      nonLeafNode <- BPZebrafish$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPZebrafish$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(BPZebrafish$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      nonLeafNode <- BPElegan$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPElegan$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(BPElegan$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      nonLeafNode <- BPTair$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPTair$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(BPTair$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      nonLeafNode <- BPYeast$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPYeast$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(BPYeast$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      nonLeafNode <- BPPombe$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPPombe$v[[as.character(i)]])
        }
      })
      return(unlist(l))


    },error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(BPPombe$df[,1]) - 1, sep = " "))

    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      nonLeafNode <- BPDrosophila$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPDrosophila$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(BPDrosophila$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      nonLeafNode <- BPEcoli$df[Level,][1:2]
      l <- lapply(nonLeafNode, function(i){
        if(i != 0){
          return(BPEcoli$v[[as.character(i)]])
        }
      })
      return(unlist(l))

    },error = function(e){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(BPEcoli$df[,1]) - 1, sep = " "))

    })

  }

}

#' Get the level of a biological process (BP) GO term's children
#'
#' @param goterm A character string of a valid gene ontology id e.g "GO:0097278"
#'
#' @return A list of children terms and their respective levels
#' @export
#'
#' @description This function retrieves a GO BP term children's level i.e. for a GO-term it's children level are derived.
#'
#' @examples
#'
#' # Retrieve "GO:0097278" children's level
#' GOTermBP2ChildLevel(goterm = "GO:0097278")
#'
#' # Retrieve "GO:0051775" children's level
#' GOTermBP2ChildLevel(goterm = "GO:0051775")

GOTermBP2ChildLevel <- function(goterm){
  Goid <- goterm

  go_children <- xx.ch[[Goid]]
  if(!all(is.na(go_children))){
    dat <- lapply(go_children, function(y){
      d <- go2h[[y]][length(go2h[[y]])] - 1
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

