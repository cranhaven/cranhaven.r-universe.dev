
#' GO cellular component (CC) terms level getter
#'
#' @param goterm An object of class character. A character string of GO cellular component (CC) terms.
#'          The object can be a GO-term or a vector of GO-terms
#'
#' @return  A two-column matrix of the GO-terms and the level they map to
#' @export
#'
#' @description Get the level of a GO cellular component (CC) term based on the directed acyclic graph (DAG)
#'
#' @note The Gene Ontology (GO) cellular component (CC) tree was built using the root node "GO:0005575"
#' @examples
#'
#' # Cellular component GO terms
#' goterms <- c("GO:0005634", "GO:0005737", "GO:0016020", "GO:0005743", "GO:0005739",
#'"GO:0005759", "GO:0005829")
#'
#' GOTermCCOnLevel(goterms)
#'
#' GOTermCCOnLevel("GO:0005730")
#'
#'

GOTermCCOnLevel <- function(goterm){
  if(is.numeric(goterm)){
    stop("The \"goterm\" argument should be a GO-term(s)")
  }

  x <- goterm

  ont <- lapply(x, function(y){
    Ontology(y)
  })
  isna <- which(is.na(ont))
  nonretired <- which(Ontology(x) != "CC")
  if(length(isna) > 0 && length(nonretired) > 0){
    index <- c(nonretired,isna)
    warning(paste(c("Check that the term on index",x[index],"are cc GO-terms and not obsolete"), collapse = " "))
    x <- x[-c(isna,nonretired)]
  }
  else if(length(isna) > 0 ){
    warning(paste(c("Check that the term", x[isna],"are cc GO-terms and not obsolete"), collapse = " "))
    x <- x[-isna]
  }else if(length(nonretired) > 0){
    warning(paste(c("Check that the term ", x[nonretired],"are cc GO-terms and not obsolete"), collapse = " "))
    x <- x[-nonretired]
  }

  if(length(x) > 0){
    dat <- data.frame()
    for(i in 1:length(x)){
      dat[i,1] <- x[i]
      dat[i,2] <- go2h2[[x[i]]][length(go2h2[[x[i]]])] - 1
    }
    colnames(dat) = c("Term", "Level")
    return(dat)
  }
}


#' GO cellular component (CC) terms on a level
#'
#' @param level A numeric value for the GO-level
#'
#' @param organism The organism of interest. If NULL the results will be from the general GO CC tree
#'
#' @return A list of GO-terms from the level
#' @export
#' @import gontr
#' @description Gets all the biological process (CC) GO-terms on a GO-level.
#'              The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'              "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress",
#'              "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @note The Gene Ontology (GO) cellular component (CC) tree was built using the root node GO:0005575
#'
#' @examples
#' # Gene association GO-terms for organism Rat on level 4
#' Level2GOTermCC(level = 4, organism = "Rat")
#'
#' # Gene association GO-terms for the General GO CC tree
#' Level2GOTermCC(level = 7)
#'
Level2GOTermCC <- function(level, organism = NULL){

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
    if(x > 15){
      stop(paste("No such level exist for the general GO cc tree, the highest level is", 14 , sep = " "))
    }

    l <- lapply(names(go2h2), function(y){
      if(go2h2[[y]][length(go2h2[[y]])] == x){
        return(y)
      }
    })
    return(unlist(l))
  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      if(x <= length(CCHuman$df[,1])){
        currentLevel <- CCHuman$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCHuman$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Human, the highest level is", length(CCHuman$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      if(x <= length(gontr::CCRat$df[,1])){
        currentLevel <- gontr::CCRat$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- gontr::CCRat$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::CCRat$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      if(x <= length(gontr::CCMouse$df[,1])){
        currentLevel <- gontr::CCMouse$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- gontr::CCMouse$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))
    },error = function(x){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::CCMouse$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      if(x <= length(CCZebrafish$df[,1])){
        currentLevel <- CCZebrafish$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCZebrafish$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Zebrafish, the highest level is", length(CCZebrafish$df[,1]) - 1, sep = " "))
    })



  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      if(x <= length(CCElegan$df[,1])){
        currentLevel <- CCElegan$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCElegan$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(CCElegan$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      if(x <= length(CCTair$df[,1]) ){
        currentLevel <- CCTair$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCTair$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))
    },error = function(x){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(CCTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      if(x <= length(CCYeast$df[,1])){
        currentLevel <- CCYeast$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCYeast$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(CCYeast$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      if(x <= length(CCPombe$df[,1])){
        currentLevel <- CCPombe$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCPombe$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(CCPombe$df[,1]) - 1, sep = " "))
    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      if(x <= length(CCDrosophila$df[,1])){
        currentLevel <- CCDrosophila$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCDrosophila$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(CCDrosophila$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      if(x <= length(CCEcoli$df[,1])){
        currentLevel <- CCEcoli$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- CCEcoli$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))
    },error = function(x){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(CCEcoli$df[,1]) - 1, sep = " "))
    })

  }

}
