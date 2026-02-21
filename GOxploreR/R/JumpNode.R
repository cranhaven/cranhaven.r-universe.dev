#' Jump nodes from a GO BP level
#'
#' @param level  A numeric value for the GO-level
#' @param organism Organism of interest that is supported by the package
#'
#' @description  The function retrieves the Jump nodes (JNs) from a GO-level. JNs are those GO-terms which have at least
#'               one child term not present in the level below it. If the optional parameter "organism" is missing then
#'               the results are from the general GO tree. The supported organisms are "Homo sapiens / Human",
#'              "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return All jump nodes from the level
#' @export
#' @import gontr
#' @examples
#' # Jump nodes from level 3
#' Level2JumpNodeBP(level = 3, organism = "Homo sapiens")
#'
#' # Jump nodes from level 6
#' Level2JumpNodeBP(level = 6, organism = "Arabidopsis thaliana")
#'
Level2JumpNodeBP <- function(level, organism = NULL){
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
    tryCatch({
      nonLeafNode <- GOgeneralbp$df[x,][1]
      if(nonLeafNode != 0){
        return(GOgeneralbp$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for the General GO BP tree, the highest level is", 19 , sep = " "))
    })


  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      nonLeafNode <- BPHuman$df[x,][1]
      if(nonLeafNode != 0){
        return(BPHuman$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for the General GO BP tree, the highest level is", length(BPHuman$df[,1]) - 1 , sep = " "))
    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      nonLeafNode <- gontr::BPRat$df[x,][1]
      if(nonLeafNode != 0){
        return(gontr::BPRat$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }
    }, error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::BPRat$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      nonLeafNode <- gontr::BPMouse$df[x,][1]
      if(nonLeafNode != 0){
        return(gontr::BPMouse$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }
    }, error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::BPMouse$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      nonLeafNode <- BPZebrafish$df[x,][1]
      if(nonLeafNode != 0){
        return(BPZebrafish$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(BPZebrafish$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      nonLeafNode <- BPElegan$df[x,][1]
      if(nonLeafNode != 0){
        return(BPElegan$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(BPElegan$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({

      nonLeafNode <- BPTair$df[x,][1]
      if(nonLeafNode != 0){
        return(BPTair$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(BPTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      nonLeafNode <- BPYeast$df[x,][1]
      if(nonLeafNode != 0){
        return(BPYeast$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(BPYeast$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      nonLeafNode <- BPPombe$df[x,][1]
      if(nonLeafNode != 0){
        return(BPPombe$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(BPPombe$df[,1]) - 1, sep = " "))
    })
  }

  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      nonLeafNode <- BPDrosophila$df[x,][1]
      if(nonLeafNode != 0){
        return(BPDrosophila$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(BPDrosophila$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      nonLeafNode <- BPEcoli$df[x,][1]
      if(nonLeafNode != 0){
        return(BPEcoli$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(BPEcoli$df[,1]) - 1, sep = " "))
    })

  }
}

#' Jump nodes from a GO MF level
#'
#' @param level  A numeric value for the GO-level
#' @param organism Organism of interest that is supported by the package
#'
#' @description The function retrieves the Jump nodes (JNs) from a GO-level. JNs are those GO-terms which have
#'               at least one child term not present in the level below it. If the optional parameter "organism" is
#'               missing then the results are from the general GO tree. The supported organisms are "Homo sapiens / Human",
#'              "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return All jump nodes from the level
#' @export
#' @import gontr
#' @examples
#' # Jump nodes on level 3
#' Level2JumpNodeMF(level = 3, organism = "Danio rerio" )
#'
#' # Jump nodes on level 6
#' Level2JumpNodeMF(level = 6, organism = "Caenorhabditis elegans" )

Level2JumpNodeMF <- function(level, organism = NULL){
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
    tryCatch({
      nonLeafNode <- GOgeneralmf$df[x,][1]
      if(nonLeafNode != 0){
        return(GOgeneralmf$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }
    }, error = function(e){
      print(paste("No such level exist for the General GO MF tree, the highest level is", 16 , sep = " "))
    })

  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      nonLeafNode <- MFHuman$df[x,][1]
      if(nonLeafNode != 0){
        return(MFHuman$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(MFHuman$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      nonLeafNode <- gontr::MFRat$df[x,][1]
      if(nonLeafNode != 0){
        return(gontr::MFRat$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::MFRat$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      nonLeafNode <- gontr::MFMouse$df[x,][1]
      if(nonLeafNode != 0){
        return(gontr::MFMouse$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::MFMouse$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      nonLeafNode <- MFZebrafish$df[x,][1]
      if(nonLeafNode != 0){
        return(MFZebrafish$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(MFZebrafish$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      nonLeafNode <- MFElegan$df[x,][1]
      if(nonLeafNode != 0){
        return(MFElegan$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }
    }, error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(MFElegan$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      nonLeafNode <- MFTair$df[x,][1]
      if(nonLeafNode != 0){
        return(MFTair$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(MFTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      nonLeafNode <- MFYeast$df[x,][1]
      if(nonLeafNode != 0){
        return(MFYeast$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(MFYeast$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      nonLeafNode <- MFPombe$df[x,][1]
      if(nonLeafNode != 0){
        return(MFPombe$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(MFPombe$df[,1]) - 1, sep = " "))
    })
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      nonLeafNode <- MFDrosophila$df[x,][1]
      if(nonLeafNode != 0){
        return(MFDrosophila$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(MFDrosophila$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      nonLeafNode <- MFEcoli$df[x,][1]
      if(nonLeafNode != 0){
        return(MFEcoli$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(MFEcoli$df[,1]) - 1, sep = " "))
    })
  }
}

#' Jump nodes from a GO CC level
#'
#' @param level  A numeric value for the GO-level
#' @param organism Organism of interest that is supported by the package
#'
#' @description  The function retrieves the Jump nodes (JNs) from a GO-level. JNs are those GO-terms which have at least
#'               one child term not present in the level below it. If the optional parameter "organism" is missing then
#'               the result are from the general GO tree. The supported organisms are "Homo sapiens / Human",
#'               "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'               "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'               "Schizosaccharomyces pombe / Fission yeast",
#'               "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return All jump nodes from the level
#' @export
#' @import gontr
#' @examples
#' # Jump nodes on level 5
#' Level2JumpNodeCC(level = 5, organism = "Saccharomyces cerevisiae" )
#'
#' # Jump nodes on level 3
#' Level2JumpNodeCC(level = 3, organism = "Schizosaccharomyces pombe" )

Level2JumpNodeCC <- function(level, organism = NULL){
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
    tryCatch({
      nonLeafNode <- GOgeneralcc$df[x,][1]
      if(nonLeafNode != 0){
        return(GOgeneralcc$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for the General GO CC tree, the highest level is", 17 , sep = " "))
    })

  }else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    tryCatch({
      nonLeafNode <- CCHuman$df[x,][1]
      if(nonLeafNode != 0){
        return(CCHuman$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Human, the highest level is", length(CCHuman$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    tryCatch({
      nonLeafNode <- gontr::CCRat$df[x,][1]
      if(nonLeafNode != 0){
        return(gontr::CCRat$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Rat, the highest level is", length(gontr::CCRat$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    tryCatch({
      nonLeafNode <- gontr::CCMouse$df[x,][1]
      if(nonLeafNode != 0){
        return(gontr::CCMouse$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Mouse, the highest level is", length(gontr::CCMouse$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    tryCatch({
      nonLeafNode <- CCZebrafish$df[x,][1]
      if(nonLeafNode != 0){
        return(CCZebrafish$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }
    }, error = function(e){
      print(paste("No such level exist for Zebrafish, the highest level is", length(CCZebrafish$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    tryCatch({
      nonLeafNode <- CCElegan$df[x,][1]
      if(nonLeafNode != 0){
        return(CCElegan$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Caenorhabditis elegans, the highest level is", length(CCElegan$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    tryCatch({
      nonLeafNode <- CCTair$df[x,][1]
      if(nonLeafNode != 0){
        return(CCTair$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Arabidopsis thaliana, the highest level is", length(CCTair$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    tryCatch({
      nonLeafNode <- CCYeast$df[x,][1]
      if(nonLeafNode != 0){
        return(CCYeast$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }
    }, error = function(e){
      print(paste("No such level exist for Saccharomyces cerevisiae, the highest level is", length(CCYeast$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    tryCatch({
      nonLeafNode <- CCPombe$df[x,][1]
      if(nonLeafNode != 0){
        return(CCPombe$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Schizosaccharomyces pombe, the highest level is", length(CCPombe$df[,1]) - 1, sep = " "))
    })

  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    tryCatch({
      nonLeafNode <- CCDrosophila$df[x,][1]
      if(nonLeafNode != 0){
        return(CCDrosophila$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Drosophila melanogaster, the highest level is", length(CCDrosophila$df[,1]) - 1, sep = " "))
    })

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    tryCatch({
      nonLeafNode <- CCEcoli$df[x,][1]
      if(nonLeafNode != 0){
        return(CCEcoli$v[[as.character(nonLeafNode)]])
      }
      else{
        return(NULL)
      }

    }, error = function(e){
      print(paste("No such level exist for Escherichia coli, the highest level is", length(CCEcoli$df[,1]) - 1, sep = " "))
    })

  }
}

