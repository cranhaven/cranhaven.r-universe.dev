
#' GO biological process (BP) terms level getter
#'
#' @param goterm An object of class character. A character string of GO biological process (BP) term id.
#'          The object can be a GO-term or a vector of GO-terms
#'
#' @return  A two-column matrix of the GO-terms and the level they map to
#' @export
#' @importFrom annotate Ontology
#' @description Gets the level of a GO biological process (BP) term based on the directed acyclic graph (DAG).
#'
#' @note The Gene Ontology (GO) biological process (BP) tree was built using the root node "GO:0001850"
#'
#' @examples
#' # A vector of biological process GO terms
#' goterms <- c("GO:0006805","GO:0009083","GO:0006631")
#'
#' GOTermBPOnLevel(goterms)
#'
#' GOTermBPOnLevel("GO:0006629")

GOTermBPOnLevel <- function(goterm){
  if(is.numeric(goterm)){
    stop("The \"goterm\" argument should be a GO-term(s)")
  }
  x <- goterm
  ont <- lapply(x, function(y){
    Ontology(y)
  })

  isna <- which(is.na(ont))
  nonretired <- which(ont != "BP")
  if(length(isna) > 0 && length(nonretired) > 0){
    index <- c(nonretired,isna)
    warning(paste(c("Check that the term ",x[index],"are bp GO-terms and not obsolete"), collapse = " "))
    x <- x[-c(isna,nonretired)]
  }
  else if(length(isna) > 0 ){
    #x <- x[-isna]
    warning(paste(c("Check that the term ", x[isna],"are bp GO-terms and not obsolete"), collapse = " "))
    x <- x[-isna]
  }else if(length(nonretired) > 0){

    warning(paste(c("Check that the term on index", x[nonretired],"are bp GO-terms and not obsolete"), collapse = " "))
    x <- x[-nonretired]
  }

  if(length(x) > 0){
    dat <- data.frame()
    for(i in 1:length(x)){
      dat[i,1] <- x[i]
      dat[i,2] <- go2h[[x[i]]][length(go2h[[x[i]]])] - 1
    }
    colnames(dat) = c("Term", "Level")
    return(dat)

  }
}


#' GO biological process (BP) terms on a level
#'
#' @param level A numeric value for the GO-level
#'
#' @param organism The organism of interest. If NULL the results will be from the general GO BP tree
#'
#' @return A list of GO-terms from the level
#' @export
#' @import gontr
#' @description Gets all the biological process (BP) GO-terms on a GO-level.
#'              The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'              "Danio rerio / Zebrafish", "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress",
#'              "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @note The Gene Ontology (GO) biological process (BP) tree was built using the root node GO:0001850
#'
#' @examples
#' # Gene association GO-terms for organism Yeast on level 6
#' Level2GOTermBP(level = 6, organism = "Yeast")
#'
#' # Gene association GO-terms for organism Fruit fly on level 2
#' Level2GOTermBP(level = 2, organism = "Fruit fly")
#'
Level2GOTermBP <- function(level, organism = NULL){
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
    if(x > 20){
      stop(paste("No such level exist for the general GO bp tree, the highest level is", 19 , sep = " "))
    }

    l <- lapply(names(go2h), function(y){
      if(go2h[[y]][length(go2h[[y]])] == x ){
        return(y)
      }
    })
    return(unlist(l))

  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      if(x <= length(BPHuman$df[,1]) ){
        currentLevel <- BPHuman$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPHuman$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))
    },error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(BPHuman$df[,1]) - 1, sep = " "))

    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      if(x <= length(gontr::BPRat$df[,1]) ){
        currentLevel <- gontr::BPRat$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- gontr::BPRat$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))

    },error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::BPRat$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      if(x <= length(gontr::BPMouse$df[,1])  ){
        currentLevel <- gontr::BPMouse$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- gontr::BPMouse$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::BPMouse$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      if(x <= length(BPZebrafish$df[,1]) ){
        currentLevel <- BPZebrafish$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPZebrafish$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))
    },error = function(x){
      print(paste("No such level exist for Zebrafish, the highest level is", length(BPZebrafish$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      if(x <= length(BPElegan$df[,1]) ){
        currentLevel <- BPElegan$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPElegan$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))
    },error = function(x){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(BPElegan$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      if(x <= length(BPTair$df[,1]) ){
        currentLevel <- BPTair$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPTair$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(BPTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      if(x <= length(BPYeast$df[,1]) ){
        currentLevel <- BPYeast$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPYeast$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(BPYeast$df[,1]) - 1, sep = " "))
    })


  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      if(x <= length(BPPombe$df[,1]) ){
        currentLevel <- BPPombe$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPPombe$v[[as.character(k)]]}
        })
      }

      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(BPPombe$df[,1]) - 1, sep = " "))
    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      if(x <= length(BPDrosophila$df[,1]) ){
        currentLevel <- BPDrosophila$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPDrosophila$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(BPDrosophila$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      if(x <= length(BPEcoli$df[,1]) ){
        currentLevel <- BPEcoli$df[x,]
        goTerms <- lapply(currentLevel, function(k){
          if(k != 0){
            termOnLevel <- BPEcoli$v[[as.character(k)]]}
        })
      }
      return(unlist(goTerms))

    },error = function(x){
      print(paste("No such level exist for Escherichia coli the highest level is", length(BPEcoli$df[,1]) - 1, sep = " "))
    })

  }

}

