
#' Building tree
#'
#' @param xx.ch The list Go child terms
#' @param terms The Go terms whoes children need to be determine
#' @keywords internal
#' @return returns GO terms.
#'
#' @importFrom  GO.db GOBPCHILDREN GOMFCHILDREN GOCCCHILDREN

all_terms_level_h2 <- function(xx.ch, terms){
  terms2 <- c()
  L <- length(terms)
  for(i in 1:L){
    terms2 <- c(terms2, xx.ch[[terms[i]]])
  }
  terms <- unique(terms2)
  terms <- terms[!is.na(terms)]
  return(terms)
}

#' Retrieve GO terms in each category
#'
#' @param term The skeleton of the reduced DAG
#' @param v The actual GO terms
#' @keywords  internal
#'
#' @return Returns a list of GO terms in each node category
#
getNodeTerms <- function(term, v){
  goterm <- new.env()
  for (i in 1:length(term[,1])) {
    value <- term[i,]
    for (j in 1:length(value)) {
      if(j == 1 && value[j] != 0){
        current <- i - 1
        u <- paste("L", current, sep = "")
        assign(paste(u, "JN" , sep = " "),  v[[as.character(value[j])]] ,envir = goterm)
      }
      else if(j == 2 && value[j] != 0){
        current <- i - 1
        u <- paste("L", current, sep = "")
        assign(paste(u, "RN" , sep = " "),  v[[as.character(value[j])]] ,envir = goterm)
      }
      else if(j == 3 && value[j] != 0){
        current <- i - 1
        u <- paste("L", current, sep = "")
        assign(paste(u, "LN" , sep = " "),  v[[as.character(value[j])]] ,envir = goterm)
      }
    }

  }
  return(goterm)
}



#' GO-term category getter
#'
#' @description Get the category of a GO-term. It checks if a GO-term is a jump node (RN), regular node (RN) or
#' leaf node (LN).
#'
#' @param goterm An object with a category
#'
#' @return
#' A data.frame object containing the terms, category and ontology.
#' @export
#'
#' @examples
#' goterm <- c("GO:0106003","GO:0032991","GO:1990429","GO:0002133","GO:0089713",
#' "GO:1990666","GO:0036125")
#'
#'
#' getGOcategory(goterm)
#'
getGOcategory <- function(goterm){
  ont <- lapply(goterm, function(x){
    Ontology(x)
  })
  go_ont <- unlist(ont)
  go_ont_store <- c()
  for (i in 1:length(go_ont)) {
    go_ont_store <- c(go_ont_store, go_ont[[i]])
  }

  if(length(which(is.na(ont)) > 0)){
    stop(paste(c("Check that the term on index ", which(is.na(ont)), "is not obsolete"), collapse = " "))
  }

  go_category <- lapply(goterm, function(x){
    if(Ontology(x) == "BP"){
      go_level <- GOTermBPOnLevel(x)$Level
      go_ch_level <- unique(GOTermBP2ChildLevel(x)$Level)
      if(length(go_ch_level) == 1 && go_ch_level == go_level + 1){
        return("RN")
      }
      else if(all(is.na(xx.ch[[x]]))){
        return("LN")
      }else{
        return("JN")
      }
    }else if(Ontology(x) == "MF"){
      go_level <- GOTermMFOnLevel(x)$Level
      go_ch_level <- unique(GOTermMF2ChildLevel(x)$Level)
      if(length(go_ch_level) == 1 && go_ch_level == go_level + 1){
        return("RN")
      }
      else if(all(is.na(xx.ch1[[x]]))){
        return("LN")
      }else{
        return("JN")
      }
    }else if(Ontology(x) == "CC"){
      go_level <- GOTermCCOnLevel(x)$Level
      go_ch_level <- unique(GOTermCC2ChildLevel(x)$Level)
      if(length(go_ch_level) == 1 && go_ch_level == go_level + 1){
        return("RN")
      }
      else if(all(is.na(xx.ch2[[x]]))){
        return("LN")
      }else{
        return("JN")
      }
    }

  })
  go_category <- unlist(go_category)

  go_final_category <- list("Term" = goterm, "Category" = go_category, "Domain" = go_ont_store)
  return(as.data.frame(go_final_category))
}

#' Return length of depth for bp
#'
#' @param Organism The name of the organism
#'
#' @keywords internal
#' @return length of depth
#' @import gontr

maxLevelBP <- function(Organism){

  if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    return(length(BPHuman$df[,1])-1)

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    return(length(gontr::BPRat$df[,1])-1)
  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    return(length(gontr::BPMouse$df[,1])-1)

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    return(length(BPZebrafish$df[,1])-1)

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    return(length(BPElegan$df[,1])-1)

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    return(length(BPTair$df[,1])-1)

  }else if(toupper(Organism) == "SABPHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    return(length(BPYeast$df[,1])-1)

  }else if(toupper(Organism) == "SCHIZOSABPHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    return(length(BPPombe$df[,1])-1)
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    return(length(BPDrosophila$df[,1])-1)

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    return(length(BPEcoli$df[,1])-1)
  }
}

#' Return length of depth for mf
#'
#' @param Organism The name of the organism
#'
#' @keywords internal
#' @return length of depth
#' @import gontr
#'
maxLevelMF <- function(Organism){

  if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    return(length(MFHuman$df[,1])-1)

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    return(length(gontr::MFRat$df[,1])-1)
  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    return(length(gontr::MFMouse$df[,1])-1)

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    return(length(MFZebrafish$df[,1])-1)

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    return(length(MFElegan$df[,1])-1)

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    return(length(MFTair$df[,1])-1)

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    return(length(MFYeast$df[,1])-1)

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    return(length(MFPombe$df[,1])-1)
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    return(length(MFDrosophila$df[,1])-1)

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    return(length(MFEcoli$df[,1])-1)
  }
}

#' Return length of depth for cc
#'
#' @param Organism The name of the organism
#'
#' @keywords internal
#' @return length of depth
#'
#' @import gontr
maxLevelCC <- function(Organism){
  if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    return(length(CCHuman$df[,1])-1)

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    return(length(gontr::CCRat$df[,1])-1)
  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    return(length(gontr::CCMouse$df[,1])-1)

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    return(length(CCZebrafish$df[,1])-1)

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    return(length(CCElegan$df[,1])-1)

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    return(length(CCTair$df[,1])-1)

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    return(length(CCYeast$df[,1])-1)

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    return(length(CCPombe$df[,1])-1)
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    return(length(CCDrosophila$df[,1])-1)

  }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
    return(length(CCEcoli$df[,1])-1)
  }
}

