

#' GO molecular function (MF) term level getter
#'
#' @param goterm An object of class character. A character string of GO molecular function (MF) terms.
#'          The object can be a GO-term or a vector of GO-terms
#'
#'
#' @return A two-column matrix of the GO-terms and the level they map to
#' @export
#'
#' @description Gets the level of a molecular function (MF) term based on the directed acyclic graph (DAG)
#'              defined by the Gene Ontology consortium
#'
#' @note The Gene Ontology (GO) molecular function (MF) tree was built using the root node "GO:0003674"
#'
#' @examples
#' # molecular function GO terms
#'
#' goterms <- c("GO:0003674","GO:0005515","GO:0003712","GO:0002039","GO:0000978","GO:0016740")
#'
#' GOTermMFOnLevel(goterms)
#'
#' GOTermMFOnLevel("GO:0005542")
#'

GOTermMFOnLevel <- function(goterm){
  if(is.numeric(goterm)){
    stop("The \"goterm\" argument should be a GO-term(s)")
  }
  x <- goterm
  ont <- lapply(x, function(y){
    Ontology(y)
  })
  isna <- which(is.na(ont))
  nonretired <- which(Ontology(x) != "MF")
  if(length(isna) > 0 && length(nonretired) > 0){
    index <- c(nonretired,isna)
    warning(paste(c("Check that the term on index",x[index],"are mf GO-terms and not obsolete"), collapse = " "))
    x <- x[-c(isna,nonretired)]
  }
  else if(length(isna) > 0 ){
    warning(paste(c("Check that the term ", x[isna],"are mf GO-terms and not obsolete"), collapse = " "))
    x <- x[-isna]
  }else if(length(nonretired) > 0){
    warning(paste(c("Check that the term ", x[nonretired],"are mf GO-terms and not obsolete"), collapse = " "))
    x <- x[-nonretired]
  }

  if(length(x) > 0){
    dat <- data.frame()
    for(i in 1:length(x)){
      dat[i,1] <- x[i]
      dat[i,2] <- go2h1[[x[i]]][length(go2h1[[x[i]]])] - 1
    }
    colnames(dat) = c("Term", "Level")
    return(dat)
  }



}


#' GO molecular function (MF) terms on a level
#'
#' @param level A numeric value for the GO-level
#'
#' @param organism The organism of interest. If NULL the results will be from the general GO MF tree
#'
#' @return  A list of GO-terms from the level
#' @export
#'
#' @description Gets all the molecular function (MF) GO-terms on a GO-level.
#'              The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'              "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @note The Gene Ontology (GO) molecular function (MF) tree was built using the root node GO:0003674
#' @import gontr
#' @examples
#' # Gene association GO-terms for organism Mouse on level 9
#' Level2GOTermMF(level = 9, organism = "mouse")
#'
#' # Gene association GO-terms for organism Zebrafish on level 10
#' Level2GOTermMF(level = 10, organism = "zebrafish")
#'
Level2GOTermMF <- function(level, organism = NULL){
  Organism <- organism

  if(is.null(level) || !is.numeric(level)){
    stop("The argument \"level\" is missing with no default or is non numeric")
  }

  x <- level + 1

  if(!is.null(Organism) && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above")
  }

  if(is.null(Organism)){
    if(x > 17){
      stop(paste("No such level exist for the general GO mf tree, the highest level is", 16 , sep = " "))
    }
    l <- lapply(names(go2h1), function(y){
      if(go2h1[[y]][length(go2h1[[y]])] == x){
        return(y)
      }
    })
    return(unlist(l))
  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      if(x <= length(MFHuman$df[,1]) ){
        currentLevel <- MFHuman$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFHuman$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Human, the highest level is", length(MFHuman$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      if(x <= length(gontr::MFRat$df[,1])){
        currentLevel <- gontr::MFRat$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- gontr::MFRat$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::MFRat$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({

      if(x <= length(gontr::MFMouse$df[,1])){
        currentLevel <- gontr::MFMouse$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- gontr::MFMouse$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::MFMouse$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      if(x <= length(MFZebrafish$df[,1])){
        currentLevel <- MFZebrafish$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFZebrafish$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Zebrafish, the highest level is", length(MFZebrafish$df[,1]) - 1, sep = " "))
    })



  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      if(x <= length(MFElegan$df[,1])){
        currentLevel <- MFElegan$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFElegan$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(MFElegan$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      if(x <= length(MFTair$df[,1])){
        currentLevel <- MFTair$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFTair$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(MFTair$df[,1]) - 1, sep = " "))
    })



  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      if(x <= length(MFYeast$df[,1])){
        currentLevel <- MFYeast$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFYeast$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(MFYeast$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      if(x <= length(MFPombe$df[,1])){
        currentLevel <- MFPombe$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFPombe$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(MFPombe$df[,1]) - 1, sep = " "))
    })


  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      if(x <= length(MFDrosophila$df[,1])){
        currentLevel <- MFDrosophila$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFDrosophila$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(MFDrosophila$df[,1]) - 1, sep = " "))
    })

  } else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      if(x <= length(MFEcoli$df[,1])){
        currentLevel <- MFEcoli$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- MFEcoli$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(MFEcoli$df[,1]) - 1, sep = " "))
    })
  }
}






