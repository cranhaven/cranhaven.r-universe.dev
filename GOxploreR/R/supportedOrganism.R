
#' Species-specific biological process GO-DAG
#' @description A sub-dag of biological process terms for a certain species e.g DAG of the gene association GO-terms for
#'              Arabidopsis thaliana. The label "J","R" and "L" on the right side of the plot
#'              gives the number of connections between the regular node (RN) on the level and the
#'              nodes right below it (RN are nodes that have all their children nodes represented in the next level)
#'              .The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat",
#'              "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @param organism The organism supported by the package. Both the scientific / common name of the organism can be use
#'                 to derive the Sub-DAG. If this argument
#'                 is "BP" the reduced  BP GO-DAG is visualised.
#' @param plot  If TRUE, both the reduced DAG and the GO-terms contained in each node are derived
#'
#' @return A list containing the plot of the DAG and all the GO terms presents in each node
#' @export
#' @import gontr
#' @examples
#' # Reduced GO-DAG for Human
#' visRDAGBP(organism = "Homo sapiens")
#'
#' # Reduced GO-DAG for Rat
#' visRDAGBP(organism = "Rat")
#'
#' # RN GO-terms on level 2 can be access as follows
#' visRDAGBP(organism = "Human", plot = FALSE)$"L2 RN"
#'
#' # JN GO-terms on level 16 can be access as follows
#' visRDAGBP(organism = "Human", plot = FALSE)$"L16 JN"
#'
#' # LN GO-terms on level 18 can be access as follows
#' visRDAGBP(organism = "Human", plot = FALSE)$"L18 LN"

visRDAGBP <- function(organism, plot = TRUE){
  if(is.null(organism)){
    stop("The \"organism\" argument is missing with no default")
  }

  species <- organism

  if(!is.null(species) && !(toupper(species) %in% SupportedOrganism) && toupper(species) != "BP"){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }
  if(toupper(species) == "BP"){
    len <- 1:length(GOgeneralbp$v)
    val <- lapply(len, function(x){
      return(GOgeneralbp$v[[as.character(x)]])
    })
    p <- drawGraph(GOgeneralbp$dat.d, GOgeneralbp$df, "Biological process", GOgeneralbp$v ,"BP", organism)
    goterm <- getNodeTerms(GOgeneralbp$df, GOgeneralbp$v )

    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }
  }

  else if(toupper(species) == "HOMO SAPIENS" || toupper(species) == "HUMAN"){
    len <- 1:length(BPHuman$v)
    val <- lapply(len, function(x){
      return(BPHuman$v[[as.character(x)]])
    })
    p <- drawGraph(BPHuman$dat.d, BPHuman$df, "Biological process", BPHuman$v ,"BP", organism)
    goterm <- getNodeTerms(BPHuman$df, BPHuman$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "RATTUS NORVEGICUS" || toupper(species) == "RAT"){
    len <- 1:length(gontr::BPRat$v)
    val <- lapply(len, function(x){
      return(gontr::BPRat$v[[as.character(x)]])
    })
    p <- drawGraph(gontr::BPRat$dat.d, gontr::BPRat$df, "Biological process", gontr::BPRat$v,"BP", organism)
    goterm <- getNodeTerms(gontr::BPRat$df, gontr::BPRat$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "MUS MUSCULUS" || toupper(species) == "MOUSE"){
    len <- 1:length(gontr::BPMouse$v)
    val <- lapply(len, function(x){
      return(gontr::BPMouse$v[[as.character(x)]])
    })
    p <- drawGraph(gontr::BPMouse$dat.d, gontr::BPMouse$df, "Biological process", gontr::BPMouse$v ,"BP", organism)
    goterm <- getNodeTerms(gontr::BPMouse$df, gontr::BPMouse$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "DANIO RERIO" || toupper(species) == "ZEBRAFISH"){
    len <- 1:length(BPZebrafish$v)
    val <- lapply(len, function(x){
      return(BPZebrafish$v[[as.character(x)]])
    })
    p <- drawGraph(BPZebrafish$dat.d, BPZebrafish$df, "Biological process", BPZebrafish$v,"BP", organism)
    goterm <- getNodeTerms(BPZebrafish$df, BPZebrafish$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "CAENORHABDITIS ELEGANS" || toupper(species) == "WORM"){
    len <- 1:length(BPElegan$v)
    val <- lapply(len, function(x){
      return(BPElegan$v[[as.character(x)]])
    })
    p <- drawGraph(BPElegan$dat.d, BPElegan$df, "Biological process", BPElegan$v,"BP", organism)
    goterm <- getNodeTerms(BPElegan$df, BPElegan$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "ARABIDOPSIS THALIANA" || toupper(species) == "CRESS"){
    len <- 1:length(BPTair$v)
    val <- lapply(len, function(x){
      return(BPTair$v[[as.character(x)]])
    })
    p <- drawGraph(BPTair$dat.d, BPTair$df, "Biological process", BPTair$v,"BP", organism)
    goterm <- getNodeTerms(BPTair$df, BPTair$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "SACCHAROMYCES CEREVISIAE" || toupper(species) == "YEAST"){
    len <- 1:length(BPYeast$v)
    val <- lapply(len, function(x){
      return(BPYeast$v[[as.character(x)]])
    })
    p <- drawGraph(BPYeast$dat.d, BPYeast$df, "Biological process", BPYeast$v, "BP", organism)
    goterm <- getNodeTerms(BPYeast$df, BPYeast$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "SCHIZOSACCHAROMYCES POMBE" || toupper(species) == "FISSION YEAST"){
    len <- 1:length(BPPombe$v)
    val <- lapply(len, function(x){
      return(BPPombe$v[[as.character(x)]])
    })
    p <- drawGraph(BPPombe$dat.d, BPPombe$df, "Biological process", BPPombe$v, "BP", organism)
    goterm <- getNodeTerms(BPPombe$df, BPPombe$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }
  else if(toupper(species) == "DROSOPHILA MELANOGASTER" || toupper(species) == "FRUIT FLY"){
    len <- 1:length(BPDrosophila$v)
    val <- lapply(len, function(x){
      return(BPDrosophila$v[[as.character(x)]])
    })
    p <- drawGraph(BPDrosophila$dat.d, BPDrosophila$df, "Biological process", BPDrosophila$v,"BP", organism)
    goterm <- getNodeTerms(BPDrosophila$df, BPDrosophila$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "ESCHERICHIA COLI" || toupper(species) == "E.COLI"){
    len <- 1:length(BPEcoli$v)
    val <- lapply(len, function(x){
      return(BPEcoli$v[[as.character(x)]])
    })
    p <- drawGraph(BPEcoli$dat.d, BPEcoli$df, "Biological process", BPEcoli$v, "BP", organism)
    goterm <- getNodeTerms(BPEcoli$df, BPEcoli$v )
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }
  }
}


#' Species-specific molecular function GO-DAG
#'
#' @description A sub-dag of molecular function terms for a certain species e.g DAG of the gene association GO-terms for
#'              Arabidopsis thaliana. The label "J","R" and "L" on the right side of the plot
#'              gives the number of connections between the regular node (RN) on the level and the
#'              nodes right below it (RN are nodes that have all their children nodes represented in the next level).
#'              The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse",
#'             "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast",
#'              "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @param organism The organism supported by the package. Both the scientific / common name of the organism can be
#'                 use to derive the Sub-DAG.  If this argument is "MF" the reduced MF GO-DAG is visualised.
#'
#' @param plot If TRUE, both the reduced DAG and the GO-terms contained in each node is derived
#'
#' @return A list containing the plot of the DAG and all the GO terms presents in each node
#'
#' @export
#' @import gontr
#' @examples
#'
#' # Reduced GO-DAG for Zebrafish
#' visRDAGMF(organism = "Zebrafish")
#'
#' # Reduced GO-DAG for Caenorhabditis elegans
#' visRDAGMF(organism = "Caenorhabditis elegans")
#'
#' # RN GO-terms on level 1 can be access as follows
#' visRDAGMF(organism = "Caenorhabditis elegans", plot = FALSE)$"L1 RN"
#'
#' # JN GO-terms on level 9 can be access as follows
#' visRDAGMF(organism = "Caenorhabditis elegans", plot = FALSE)$"L9 JN"
#'
#' # LN GO-terms on level 14 can be access as follows
#' visRDAGMF(organism = "Caenorhabditis elegans", plot = FALSE)$"L14 LN"

visRDAGMF <- function(organism, plot = TRUE){
  if(is.null(organism)){
    stop("The \"organism\" argument is missing with no default.")
  }
  species <- organism
  if(!is.null(species) && !(toupper(species) %in% SupportedOrganism) && toupper(species) != "MF"){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }

  if(toupper(species) == "MF"){
    len <- 1:length(GOgeneralmf$v)
    val <- lapply(len, function(x){
      return(GOgeneralmf$v[[as.character(x)]])
    })
    p <- drawGraph(GOgeneralmf$dat.d, GOgeneralmf$df, "Biological process", GOgeneralmf$v ,"MF", organism)
    goterm <- getNodeTerms(GOgeneralmf$df, GOgeneralmf$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }
  else if(toupper(species) == "HOMO SAPIENS" || toupper(species) == "HUMAN"){
    len <- 1:length(MFHuman$v)
    val <- lapply(len, function(x){
      return(MFHuman$v[[as.character(x)]])
    })
    p <- drawGraph(MFHuman$dat.d, MFHuman$df, "Molecular function", MFHuman$v, "MF", organism)
    goterm <- getNodeTerms(MFHuman$df, MFHuman$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }


  }else if(toupper(species) == "RATTUS NORVEGICUS" || toupper(species) == "RAT"){
    len <- 1:length(gontr::MFRat$v)
    val <- lapply(len, function(x){
      return(gontr::MFRat$v[[as.character(x)]])
    })
    p <- drawGraph(gontr::MFRat$dat.d, gontr::MFRat$df, "Molecular function", gontr::MFRat$v, "MF", organism)
    goterm <- getNodeTerms(gontr::MFRat$df, gontr::MFRat$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "MUS MUSCULUS" || toupper(species) == "MOUSE"){
    len <- 1:length(gontr::MFMouse$v)
    val <- lapply(len, function(x){
      return(gontr::MFMouse$v[[as.character(x)]])
    })
    p <- drawGraph(gontr::MFMouse$dat.d, gontr::MFMouse$df, "Molecular function", gontr::MFMouse$v,"MF", organism)
    goterm <- getNodeTerms(gontr::MFMouse$df, gontr::MFMouse$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "DANIO RERIO" || toupper(species) == "ZEBRAFISH"){
    len <- 1:length(MFZebrafish$v)
    val <- lapply(len, function(x){
      return(MFZebrafish$v[[as.character(x)]])
    })
    p <- drawGraph(MFZebrafish$dat.d, MFZebrafish$df, "Molecular function", MFZebrafish$v,"MF", organism)
    goterm <- getNodeTerms(MFZebrafish$df, MFZebrafish$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "CAENORHABDITIS ELEGANS" || toupper(species) == "WORM"){
    len <- 1:length(MFElegan$v)
    val <- lapply(len, function(x){
      return(MFElegan$v[[as.character(x)]])
    })
    p <- drawGraph(MFElegan$dat.d, MFElegan$df, "Molecular function", MFElegan$v, "MF", organism)
    goterm <- getNodeTerms(MFElegan$df, MFElegan$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "ARABIDOPSIS THALIANA" || toupper(species) == "CRESS"){
    len <- 1:length(MFTair$v)
    val <- lapply(len, function(x){
      return(MFTair$v[[as.character(x)]])
    })
    p <- drawGraph(MFTair$dat.d, MFTair$df, "Molecular function", MFTair$v, "MF", organism)
    goterm <- getNodeTerms(MFTair$df, MFTair$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }


  }else if(toupper(species) == "SACCHAROMYCES CEREVISIAE" || toupper(species) == "YEAST"){
    len <- 1:length(MFYeast$v)
    val <- lapply(len, function(x){
      return(MFYeast$v[[as.character(x)]])
    })
    p <- drawGraph(MFYeast$dat.d, MFYeast$df, "Molecular function", MFYeast$v, "MF", organism)
    goterm <- getNodeTerms(MFYeast$df, MFYeast$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "SCHIZOSACCHAROMYCES POMBE" || toupper(species) == "FISSION YEAST"){
    len <- 1:length(MFPombe$v)
    val <- lapply(len, function(x){
      return(MFPombe$v[[as.character(x)]])
    })
    p <- drawGraph(MFPombe$dat.d, MFPombe$df, "Molecular function", MFPombe$v, "MF", organism)
    goterm <- getNodeTerms(MFPombe$df, MFPombe$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }
  else if(toupper(species) == "DROSOPHILA MELANOGASTER" || toupper(species) == "FRUIT FLY"){
    len <- 1:length(MFDrosophila$v)
    val <- lapply(len, function(x){
      return(MFDrosophila$v[[as.character(x)]])
    })
    p <- drawGraph(MFDrosophila$dat.d, MFDrosophila$df, "Molecular function", MFDrosophila$v, "MF", organism)
    goterm <- getNodeTerms(MFDrosophila$df, MFDrosophila$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "ESCHERICHIA COLI" || toupper(species) == "E.COLI"){
    len <- 1:length(MFEcoli$v)
    val <- lapply(len, function(x){
      return(MFEcoli$v[[as.character(x)]])
    })
    p <- drawGraph(MFEcoli$dat.d, MFEcoli$df, "Molecular function", MFEcoli$v, "MF", organism)
    goterm <- getNodeTerms(MFEcoli$df, MFEcoli$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }
  }

}

#' Species-specific cellular component GO-DAG
#'
#' @description A sub-dag of cellular component terms for a certain species e.g DAG of the gene association terms for
#'               Arabidopsis thaliana.The label "J","R" and "L" on the right side of the plot
#'              gives the number of connections between the regular node (RN) on the level and the
#'              nodes right below it (RN are nodes that have all their children nodes represented in the next level). The supported organisms are "Homo sapiens / human", "Rattus norvegicus / Rat",
#'               "Mus musculus / Mouse",
#'               "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / cress", "Saccharomyces cerevisiae / Yeast",
#'               "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @param organism The organism supported by the package. Both the scientific / common name of the organism can be use
#'                 to derive the Sub-DAG.  If this argument
#'                 is "CC" the general CC GO-DAG is visualised.
#'
#' @param plot  If TRUE, both the reduced DAG and the GO-terms contained in each node is derived
#'
#' @return A list containing the plot of the DAG and all the GO-terms presents in each node
#' @export
#' @import gontr
#' @examples
#' # Reduced GO-DAG for Arabidopsis thaliana
#' visRDAGCC(organism = "Arabidopsis thaliana")
#'
#' # Reduced GO-DAG for Saccharomyces cerevisiae
#' visRDAGCC(organism = "Worm")
#'
#' # RN GO-terms on level 2 can be access as follows
#' visRDAGCC(organism = "Human", plot = FALSE)$"L2 RN"
#'
#' # JN GO-terms on level 12 can be access as follows
#' visRDAGCC(organism = "Human", plot = FALSE)$"L12 JN"
#'
#' # LN GO-terms on level 16 can be access as follows
#' visRDAGCC(organism = "Human", plot = FALSE)$"L16 LN"
#'
visRDAGCC <- function(organism, plot = TRUE){

  if(is.null(organism)){
    stop("The \"organism\" argument is missing with no default")
  }

  species <- organism

  if(!is.null(species) && !(toupper(species) %in% SupportedOrganism) && toupper(species) != "CC"){
    print(SupportedOrganismv2)
    stop("The \"organism\" argument should be given from the list above.")
  }

  if(toupper(species) == "CC"){
    len <- 1:length(GOgeneralcc$v)
    val <- lapply(len, function(x){
      return(GOgeneralcc$v[[as.character(x)]])
    })
    p <- drawGraph(GOgeneralcc$dat.d, GOgeneralcc$df, "Biological process", GOgeneralcc$v ,"CC", organism)
    goterm <- getNodeTerms(GOgeneralcc$df, GOgeneralcc$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }
  }

  else if(toupper(species) == "HOMO SAPIENS" || toupper(species) == "HUMAN"){
    len <- 1:length(CCHuman$v)
    val <- lapply(len, function(x){
      return(CCHuman$v[[as.character(x)]])
    })
    p <- drawGraph(CCHuman$dat.d, CCHuman$df, "Cellular component", CCHuman$v, "CC", organism)
    goterm <- getNodeTerms(CCHuman$df, CCHuman$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "RATTUS NORVEGICUS" || toupper(species) == "RAT"){
    len <- 1:length(gontr::CCRat$v)
    val <- lapply(len, function(x){
      return(gontr::CCRat$v[[as.character(x)]])
    })
    p <- drawGraph(gontr::CCRat$dat.d, gontr::CCRat$df, "Cellular component", gontr::CCRat$v, "CC", organism)
    goterm <- getNodeTerms(gontr::CCRat$df, gontr::CCRat$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "MUS MUSCULUS" || toupper(species) == "MOUSE"){
    len <- 1:length(gontr::CCMouse$v)
    val <- lapply(len, function(x){
      return(gontr::CCMouse$v[[as.character(x)]])
    })
    p <- drawGraph(gontr::CCMouse$dat.d, gontr::CCMouse$df, "Cellular component", gontr::CCMouse$v, "CC", organism)
    goterm <- getNodeTerms(gontr::CCMouse$df, gontr::CCMouse$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "DANIO RERIO" || toupper(species) == "ZEBRAFISH"){
    len <- 1:length(CCZebrafish$v)
    val <- lapply(len, function(x){
      return(CCZebrafish$v[[as.character(x)]])
    })
    p <-  drawGraph(CCZebrafish$dat.d, CCZebrafish$df, "Cellular component", CCZebrafish$v, "CC", organism)
    goterm <- getNodeTerms(CCZebrafish$df, CCZebrafish$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "CAENORHABDITIS ELEGANS" || toupper(species) == "WORM"){
    len <- 1:length(CCElegan$v)
    val <- lapply(len, function(x){
      return(CCElegan$v[[as.character(x)]])
    })
    p <- drawGraph(CCElegan$dat.d, CCElegan$df, "Cellular component", CCElegan$v, "CC", organism)
    goterm <- getNodeTerms(CCElegan$df, CCElegan$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "ARABIDOPSIS THALIANA" || toupper(species) == "CRESS"){
    len <- 1:length(CCTair$v)
    val <- lapply(len, function(x){
      return(CCTair$v[[as.character(x)]])
    })
    p <-  drawGraph(CCTair$dat.d, CCTair$df, "Cellular component", CCTair$v, "CC", organism)
    goterm <- getNodeTerms(CCTair$df, CCTair$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "SACCHAROMYCES CEREVISIAE" || toupper(species) == "YEAST"){
    len <- 1:length(CCYeast$v)
    val <- lapply(len, function(x){
      return(CCYeast$v[[as.character(x)]])
    })
    p <- drawGraph(CCYeast$dat.d, CCYeast$df, "Cellular component", CCYeast$v, "CC", organism)
    goterm <- getNodeTerms(CCYeast$df, CCYeast$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "SCHIZOSACCHAROMYCES POMBE" || toupper(species) == "FISSION YEAST"){
    len <- 1:length(CCPombe$v)
    val <- lapply(len, function(x){
      return(CCPombe$v[[as.character(x)]])
    })
    p <- drawGraph(CCPombe$dat.d, CCPombe$df, "Cellular component", CCPombe$v, "CC", organism)
    goterm <- getNodeTerms(CCPombe$df, CCPombe$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }
  }
  else if(toupper(species) == "DROSOPHILA MELANOGASTER" || toupper(species) == "FRUIT FLY"){
    len <- 1:length(CCDrosophila$v)
    val <- lapply(len, function(x){
      return(CCDrosophila$v[[as.character(x)]])
    })
    p <-  drawGraph(CCDrosophila$dat.d, CCDrosophila$df , "Cellular component", CCDrosophila$v, "CC", organism)
    goterm <- getNodeTerms(CCDrosophila$df, CCDrosophila$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }

  }else if(toupper(species) == "ESCHERICHIA COLI" || toupper(species) == "E.COLI"){
    len <- 1:length(CCEcoli$v)
    val <- lapply(len, function(x){
      return(CCEcoli$v[[as.character(x)]])
    })
    p <-  drawGraph(CCEcoli$dat.d, CCEcoli$df , "Cellular component", CCEcoli$v, "CC", organism)
    goterm <- getNodeTerms(CCEcoli$df, CCEcoli$v)
    if(plot){
      output <- list("terms" = as.list(goterm), "plot" = p)
      return(output)
    }else{
      return(as.list(goterm))
    }
  }

}

