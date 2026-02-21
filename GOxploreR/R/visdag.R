
#' Visualise the GO-DAG for a specific organism based on certain GO-terms
#'
#' @param goterm  A vector of biological process GO-terms
#'
#' @param organism The organism whose DAG we want to visualise based on the GO-terms supplied. If this argument is "BP"
#'                 the general reduced GO BP tree is used.
#'
#' @return Returns a plot highlighting the important categories i.e. the categories containing the GO-terms of interest
#' @export
#' @import gontr
#' @description A category in the organism specific DAG is regarded as important if it contains at least one GO-terms from
#'              the vector supplied. The function does not visualise the entire organism's DAG, it accepts a range of GO-terms
#'              and DAG
#'              categories that contains
#'              these GO-terms are visualise. We refer to these categories as the important and everything else is faded out.
#'               The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'               "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#

#' @examples
#' Terms <- c("GO:0000278", "GO:0006414","GO:0022403","GO:0006415",
#' "GO:0006614","GO:0045047","GO:0072599","GO:0000279")
#'
#' # Visualised Human GO-DAG based on the GO-terms given
#' visRsubDAGBP(goterm = Terms, organism = "Human")
#'
visRsubDAGBP <- function(goterm, organism){
  Organism <- organism

  if(is.null(goterm)){
    stop("The \"goterm\" argument is missing without a default.")
  }

  if(is.numeric(goterm)){
    stop("The \"goterm\" argument should be only GO-terms.")
  }

  ont <- lapply(goterm, function(x){
    Ontology(x)
  })

  if(length(which(ont != "BP")) > 0){
    stop(paste(c("Check that the GO-term on index", which(ont != "BP"), "are BP term(s) and not obsolete." ), collapse = " "))
  }

  x <- goterm

  if(!(toupper(Organism) %in% SupportedOrganism) && toupper(Organism) != "BP"){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }

  saveTerm <- c()
  l <- lapply(x, function(k){
    if(!is.na(Ontology(k)) ){
      return(k)}
  })
  GO_terms <- unlist(l)

  if(toupper(Organism) == "BP"){
    j <- GOgeneralbp$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(GOgeneralbp$dat.d, GOgeneralbp$df, "Biological process", GOgeneralbp$v ,"BP",saveTerm, Organism)
    return(sSP)
  }


  else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    j <- BPHuman$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(BPHuman$dat.d, BPHuman$df, "Biological process", BPHuman$v ,"BP",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    j <- gontr::BPRat$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(gontr::BPRat$dat.d, gontr::BPRat$df, "Biological process", gontr::BPRat$v,"BP",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    j <- gontr::BPMouse$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(gontr::BPMouse$dat.d, gontr::BPMouse$df, "Biological process", gontr::BPMouse$v ,"BP",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    j <- BPZebrafish$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(BPZebrafish$dat.d, BPZebrafish$df, "Biological process", BPZebrafish$v,"BP" ,saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    j <- BPElegan$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(BPElegan$dat.d, BPElegan$df, "Biological process", BPElegan$v,"BP" ,saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    j <- BPTair$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(BPTair$dat.d, BPTair$df, "Biological process", BPTair$v,"BP", saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    j <- BPYeast$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(BPYeast$dat.d, BPYeast$df, "Biological process", BPYeast$v, "BP", saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    j <- BPPombe$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(BPPombe$dat.d, BPPombe$df, "Biological process", BPPombe$v, "BP", saveTerm, Organism)
    return(sSP)
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    j <- BPDrosophila$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(BPDrosophila$dat.d, BPDrosophila$df, "Biological process", BPDrosophila$v,"BP", saveTerm, Organism)
    return(sSP)
  }
}

#' Visualise the GO-DAG for a specific organism based on certain GO-terms
#'
#' @param goterm  A vector of molecular function GO-terms
#' @param organism The organism whose DAG we want to visualise based on the GO-terms supplied. If this argument is "MF"
#'                 the general reduced GO MF tree is used.
#'
#' @return Returns a plot highlighting the important categories i.e. the categories containing the GO-terms of interest
#' @export
#' @import gontr
#' @description A category in the organism-specific DAG is regarded as important if it contains at least one GO-terms from
#'              the vector supplied. The function does not visualise the entire organism's DAG, it accepts a range of GO-terms
#'               and DAG categories that contains
#'              these GO-terms are visualise. We refer to these categories as the important and everything else
#'              is faded out. The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat",
#'              "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'               "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @examples
#' Terms <- c("GO:0034040","GO:0008374" ,"GO:0003777","GO:0003674","GO:0015440","GO:0005464")
#' visRsubDAGMF(Terms, "Human")

visRsubDAGMF <- function(goterm, organism){

  Organism <- organism

  if(is.null(goterm)){
    stop("The \"goterm\" argument is missing without a default.")
  }

  if(is.numeric(goterm)){
    stop("The \"goterm\" argument should be only GO-terms.")
  }
  ont <- lapply(goterm, function(x){
    Ontology(x)
  })

  if(length(which(ont != "MF")) > 0){
    stop(paste(c("Check that the GO-term on index", which(ont != "MF"), "are mf term(s) and not obsolete." ), collapse = " "))
  }

  x <- goterm
  if(!is.null(Organism) && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }
  saveTerm <- c()
  l <- lapply(x, function(k){
    if(!is.na(Ontology(k)) ){
      return(k)}
  })
  GO_terms <- unlist(l)

  if(toupper(Organism) == "MF"){
    j <- GOgeneralmf$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(GOgeneralmf$dat.d, GOgeneralmf$df, "Biological process", GOgeneralmf$v ,"MF",saveTerm, Organism)
    return(sSP)
  }

  else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    j <- MFHuman$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(MFHuman$dat.d, MFHuman$df, "Molecular function", MFHuman$v ,"MF",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    j <- gontr::MFRat$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(gontr::MFRat$dat.d, gontr::MFRat$df, "Molecular function", gontr::MFRat$v,"MF",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    j <- gontr::MFMouse$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(gontr::MFMouse$dat.d, gontr::MFMouse$df, "Molecular function", gontr::MFMouse$v ,"MF",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    j <- MFZebrafish$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(MFZebrafish$dat.d, MFZebrafish$df, "Molecular function", MFZebrafish$v,"MF" ,saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    j <- MFElegan$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(MFElegan$dat.d, MFElegan$df, "Molecular function", MFElegan$v,"MF" ,saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    j <- MFTair$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(MFTair$dat.d, MFTair$df, "Molecular function", MFTair$v,"MF", saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    j <- MFYeast$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(MFYeast$dat.d, MFYeast$df, "Molecular function", MFYeast$v, "MF", saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    j <- MFPombe$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(MFPombe$dat.d, MFPombe$df, "Molecular function", MFPombe$v, "MF", saveTerm, Organism)
    return(sSP)
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    j <- MFDrosophila$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(MFDrosophila$dat.d, MFDrosophila$df, "Molecular function", MFDrosophila$v,"MF", saveTerm, Organism)
    return(sSP)
  }
}

#' Visualise the GO-DAG for a specific organism based on certain GO-terms
#'
#' @param goterm  A vector of cellular component GO-terms
#' @param organism The organism whose DAG we want to visualise based on the GO-terms supplied
#'
#' @return Returns a plot highlighting the important categories i.e. the categories containing the GO-terms of interest.
#'                  If this argument is "CC"
#'                 the general reduced GO CC tree is used.
#' @export
#' @import gontr
#' @description A category in the organism-specific DAG is regarded as important if it contains at least one GO-terms from
#'              the vector given. The function does not visualise the entire organism's DAG, it accepts a range of GO-terms and DAG
#'              categories that contains
#'              these GO-terms are visualise. We refer to these categories as the important and everything else is faded out.
#'              The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'              "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @examples
#'
#' # Visualised Human GO-DAG based on the GO-terms given
#' Terms <- c("GO:0030054","GO:0045171","GO:0043204")
#'
#' visRsubDAGCC(goterm = Terms, organism = "Human")
#'
visRsubDAGCC <- function(goterm, organism){
  Organism <- organism

  if(is.null(goterm)){
    stop("The \"goterm\" argument is missing without a default.")
  }

  if(is.numeric(goterm)){
    stop("The \"goterm\" argument should be only GO-terms.")
  }
  ont <- lapply(goterm, function(x){
    Ontology(x)
  })

  if(length(which(ont != "CC")) > 0){
    stop(paste(c("Check that the GO-term on index", which(ont != "CC"), "are cc term(s) and not obsolete." ), collapse = " "))
  }

  x <- goterm

  if(!(toupper(Organism) %in% SupportedOrganism) && toupper(Organism) != "BP"){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }

  saveTerm <- c()
  l <- lapply(x, function(k){
    if(!is.na(Ontology(k)) ){
      return(k)}
  })
  GO_terms <- unlist(l)

  if(toupper(Organism) == "CC"){
    j <- GOgeneralcc$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(GOgeneralcc$dat.d, GOgeneralcc$df, "Biological process", GOgeneralcc$v ,"CC",saveTerm, Organism)
    return(sSP)
  }

  else if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
    j <- CCHuman$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(CCHuman$dat.d, CCHuman$df, "Cellular component", CCHuman$v ,"CC",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
    j <- gontr::CCRat$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(gontr::CCRat$dat.d, gontr::CCRat$df, "Cellular component", gontr::CCRat$v,"CC",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
    j <- gontr::CCMouse$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(gontr::CCMouse$dat.d, gontr::CCMouse$df, "Cellular component", gontr::CCMouse$v ,"CC",saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
    j <- CCZebrafish$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(CCZebrafish$dat.d, CCZebrafish$df, "Cellular component", CCZebrafish$v,"CC" ,saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM"){
    j <- CCElegan$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(CCElegan$dat.d, CCElegan$df, "Cellular component", CCElegan$v,"CC" ,saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
    j <- CCTair$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(CCTair$dat.d, CCTair$df, "Cellular component", CCTair$v,"CC", saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
    j <- CCYeast$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(CCYeast$dat.d, CCYeast$df, "Cellular component", CCYeast$v, "CC", saveTerm, Organism)
    return(sSP)

  }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
    j <- CCPombe$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(CCPombe$dat.d, CCPombe$df, "Cellular component", CCPombe$v, "CC", saveTerm, Organism)
    return(sSP)
  }
  else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
    j <- CCDrosophila$v
    for (k in 1:length(GO_terms)) {
      for(i in 1:length(j)){
        if(GO_terms[k] %in% j[[as.character(i)]]){
          saveTerm <- c(saveTerm, i)
        }
      }
    }
    saveTerm <- unique(saveTerm)
    sSP <- drawGraph4Vis(CCDrosophila$dat.d, CCDrosophila$df, "Cellular component", CCDrosophila$v,"CC", saveTerm, Organism)
    return(sSP)
  }

}



