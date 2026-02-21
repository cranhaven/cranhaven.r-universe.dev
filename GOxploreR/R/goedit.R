


#' prioritization of a lists of GO-terms
#'
#' @param lst The GO ids we want to prioritize
#'
#' @param sp  If the argument "sp" is TRUE, only shortest paths are used, otherwise all paths
#' @param organism If organism is given then only the gene association GO-terms of that organism are considered during
#'                 the prioritization otherwise, all the (non-retired) GO-terms from a particular ontology
#'                 are used in the ranking.
#' @param domain Ontology of the GO-terms. The default is BP.
#'
#' @importFrom igraph graph.edgelist simplify get.all.shortest.paths all_simple_paths
#'
#'
#' @description Given a vector of GO-terms, this function prioritizes the GO-terms by expoiting the structure of a DAG.
#'              Starting from the GO-term on the highest level and searching all the paths to the root node iteratively.
#'              If any GO-terms in the input
#'              vector are found along this path, these GO-terms are removed. This is because the GO-term at the end of a path
#'              is more specific than the GO-terms along the path. For an organism, the GO-terms of that organism are used for
#'              the prioritization. The supported organisms are "Homo sapiens / Human", "Rattus norvegicus / Rat", "Mus musculus / Mouse", "Danio rerio / Zebrafish",
#'              "Caenorhabditis elegans / Worm" ,"Arabidopsis thaliana / Cress", "Saccharomyces cerevisiae / Yeast", "Schizosaccharomyces pombe / Fission yeast",
#'              "Drosophila melanogaster / Fruit fly", "Escherichia coli / E.coli".
#'
#' @return A list containing the prioritize GO-terms, their ranking, original GO-terms and their GO-levels is given.
#' @export
#' @import gontr
#'
#'
#' @examples
#'
#' Terms <-c("GO:0000278", "GO:0006414","GO:0022403","GO:0006415","GO:0006614",
#'  "GO:0045047","GO:0022411","GO:0001775","GO:0046649","GO:045321")
#' prioritizedGOTerms(Terms, organism = "Human", sp=TRUE, domain = "BP")

prioritizedGOTerms <- function(lst = NULL, organism = NULL, sp = TRUE , domain = "BP"){
  Organism <- organism
  ont <- domain

  if(!is.null(Organism) && toupper(Organism) != "BP" && toupper(Organism) != "MF" && toupper(Organism) != "CC" && !(toupper(Organism) %in% SupportedOrganism)){
    print(SupportedOrganismv2)
    stop("This organism is not supported by the package, the supported organism are given above")
  }

  res <- H <- rankHF <- HI <- rankHI <- rnk1 <- NULL
  golev <- 1

  if(toupper(ont) == "BP"){
    tr <- "BPlist"
    BP[,2] <- BP[,2] - 1
    term <- BP
    entireEdge <- biological_f_edgelist[,c(2,1)]
  }else if(toupper(ont) == "MF"){
    tr <- "MFlist"
    MF[,2] <- MF[,2] - 1
    term <- MF
    entireEdge <- cellular_f_edgelist[,c(2,1)]
  }else if(toupper(ont) == "CC"){
    tr <- "CClist"
    CC[,2] <- CC[,2] - 1
    term <- CC
    entireEdge <- molecular_f_edgelist[,c(2,1)]
  }

  if(!is.null(Organism)){
    if(toupper(Organism) == "HOMO SAPIENS" || toupper(Organism) == "HUMAN"){
      g <- graph.edgelist(as.matrix(Human[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "RATTUS NORVEGICUS" || toupper(Organism) == "RAT"){
      g <- graph.edgelist(as.matrix(gontr::Rat[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "MUS MUSCULUS" || toupper(Organism) == "MOUSE"){
      g <- graph.edgelist(as.matrix(gontr::Mouse[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "DANIO RERIO" || toupper(Organism) == "ZEBRAFISH"){
      g <- graph.edgelist(as.matrix(Zebrafish[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "CAENORHABDITIS ELEGANS" || toupper(Organism) == "WORM" ){
      g <- graph.edgelist(as.matrix(Elegan[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "ARABIDOPSIS THALIANA" || toupper(Organism) == "CRESS"){
      g <- graph.edgelist(as.matrix(Athalian[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "SACCHAROMYCES CEREVISIAE" || toupper(Organism) == "YEAST"){
      g <- graph.edgelist(as.matrix(Yeast[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "SCHIZOSACCHAROMYCES POMBE" || toupper(Organism) == "FISSION YEAST"){
      g <- graph.edgelist(as.matrix(Pombe[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "DROSOPHILA MELANOGASTER" || toupper(Organism) == "FRUIT FLY"){
      g <- graph.edgelist(as.matrix(Drosophila[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]

    }else if(toupper(Organism) == "ESCHERICHIA COLI" || toupper(Organism) == "E.COLI"){
      g <- graph.edgelist(as.matrix(Ecoli[[tr]]), directed=T)
      g <- simplify(g)
      ALLGObptermAndLevel <- term
      rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
      igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]
    }
  }
  else{
    g <- graph.edgelist(as.matrix(entireEdge), directed=T)
    g <- simplify(g)
    ALLGObptermAndLevel <- term
    rownames(ALLGObptermAndLevel) <- ALLGObptermAndLevel[,1]
    igraph::V(g)$lev <- ALLGObptermAndLevel[igraph::V(g)$name,2]
  }

  tryCatch({
    lst <- setdiff(lst, igraph::V(g)$name[which(igraph::V(g)$lev==golev)])
    level <- igraph::V(g)$lev
    names(level) <- igraph::V(g)$name
    rnk <- rnk1 <- sort(level[lst],decreasing=T)
    H <- c()
    while(1){
      if(sp){
        ptmp <- get.all.shortest.paths(g, names(rnk)[1], igraph::V(g)$name[which(igraph::V(g)$lev==golev)], mode="in")
        nm <- unique(names(unlist(ptmp$res)))
      }
      else{
        ptmp <- all_simple_paths(g, names(rnk)[1], igraph::V(g)$name[which(igraph::V(g)$lev==golev)], mode="in")
        nm <- unique(names(unlist(ptmp)))
      }

      nm <- setdiff(nm, igraph::V(g)$name[which(igraph::V(g)$lev==golev)])
      ins <- intersect(nm, lst)
      ktmp <- c()
      for(i in ins){
        ktmp <- c(ktmp, which(names(rnk)==i))
      }
      H <- c(H, names(rnk)[1])

      rnk <- rnk[-ktmp]
      if(length(rnk)==0){
        break
      }

    }
  }, error = function(e) {
    print(e)
  })
  res <- list(HF=H,rankHF=rnk1[H], HI=lst,rankHI=rnk1)
  class(res) <- c("GOxploreR", "GPrior")
  invisible(res)
  return(res)
}
