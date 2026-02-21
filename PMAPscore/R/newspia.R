#' @title newspia
#' @description Function `newspia` This function is based on SPIA algorithm to analyse KEGG signal pathway for single sample..
#' @param beta Weights to be assigned to each type of gene/protein relation type.
#' It should be a named numeric vector of length 23, whose names must be: c("activation","compound","binding/association","expression",
#' "inhibition","activation_phosphorylation","phosphorylation", "indirect","inhibition_phosphorylation","dephosphorylation_inhibition",
#' "dissociation","dephosphorylation","activation_dephosphorylation", "state","activation_indirect","inhibition_ubiquination","ubiquination",
#' "expression_indirect","indirect_inhibition","repression", "binding/association_phosphorylation","dissociation_phosphorylation","indirect_phosphorylation")
#' If set to null, beta will be by default chosen as: c(1,0,0,1,1,1,0,0,1,1,0,0,1,0,1,1,0,1,1,1,0,0,0).
#' @param de  A named vector containing the statue of particular genes in a particular sample.The names of this numeric vector are Entrez gene IDs.
#' @param all A vector with the Entrez IDs in the reference set. If the data was obtained from a microarray experiment,this set will contain all genes present on the specific array used for the experiment.This vector should contain all names of the de argument.
#' @param organism A three letter character designating the organism. See a full list at ftp://ftp.genome.jp/pub/kegg/xml/organisms.
#' @param data.dir Location of the "organism"SPIA.RData file containing the pathways data .If set to NULL will look for this file in the extdata folder of the PMAPscore library.
#' @param pathids A character vector with the names of the pathways to be analyzed.If left NULL all pathways available will be tested.
#' @param verbose If set to TRUE, displays the number of pathways already analyzed.
#' @return Get one Data in data frame format,which cotains pathway's id,pathway's name and PFS_score.
#' @export

newspia<-function (de = NULL, all = NULL, organism = "hsa", data.dir = NULL,
                   pathids = NULL, verbose = TRUE, beta = NULL) {
  if (is.null(de) | is.null(all)) {
    stop("de and all arguments can not be NULL!")
  }
  rel <- c("activation", "compound", "binding/association",
           "expression", "inhibition", "activation_phosphorylation",
           "phosphorylation", "inhibition_phosphorylation",
           "inhibition_dephosphorylation", "dissociation",
           "dephosphorylation", "activation_dephosphorylation",
           "state change", "activation_indirect effect",
           "inhibition_ubiquination", "ubiquination",
           "expression_indirect effect", "inhibition_indirect effect",
           "repression", "dissociation_phosphorylation",
           "indirect effect_phosphorylation", "activation_binding/association",
           "indirect effect", "activation_compound",
           "activation_ubiquination")
  if (is.null(beta)) {
    beta = c(1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1,
             1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1)
    names(beta) <- rel
  }
  else {
    if (!all(names(beta) %in% rel) | length(names(beta)) !=
        length(rel)) {
      stop(paste("beta must be a numeric vector of length",
                 length(rel), "with the following names:",
                 "\n", paste(rel, collapse = ",")))
    }
  }
  .myDataEnv <- new.env(parent = emptyenv())
  datload <- paste(organism, "SPIA", sep = "")
  if (is.null(data.dir)) {
    if (!paste(datload, ".RData", sep = "") %in%
        dir(system.file("extdata", package = "PMAPscore"))) {
      cat("The KEGG pathway data for your organism is not present in the extdata folder of the SPIA package!!!")
      cat("\n")
      cat("Please generate one first using makeSPIAdata and specify its location using data.dir argument or copy it in the extdata folder of the SPIA package!")
    }
    else {
      load(file = paste(system.file("extdata", package = "PMAPscore"),
                        paste("/", organism, "SPIA", sep = ""),
                        ".RData", sep = ""), envir = .myDataEnv)
    }
  }
  if (!is.null(data.dir)) {
    if (!paste(datload, ".RData", sep = "") %in%
        dir(data.dir)) {
      cat(paste(data.dir, " does not contin a file called ",
                paste(datload, ".RData", sep = "")))
    }
    else {
      load(file = paste(data.dir, paste(datload, ".RData",
                                        sep = ""), sep = ""), envir = .myDataEnv)
    }
  }
  datpT = .myDataEnv[["path.info"]]
  if (!is.null(pathids)) {
    if (all(pathids %in% names(datpT))) {
      datpT = datpT[pathids]
    }
    else {
      stop(paste("pathids must be a subset of these pathway ids: ",
                 paste(names(datpT), collapse = " "), sep = " "))
    }
  }
  datp <- list()
  path.names <- NULL
  hasR <- NULL
  for (jj in 1:length(datpT)) {
    sizem <- dim(datpT[[jj]]$activation)[1]
    s <- 0
    con <- 0
    for (bb in 1:length(rel)) {
      con = con + datpT[[jj]][[rel[bb]]] * abs(sign(beta[rel[bb]]))
      s = s + datpT[[jj]][[rel[bb]]] * beta[rel[bb]]
    }
    z = matrix(rep(apply(con, 2, sum), dim(con)[1]), dim(con)[1],
               dim(con)[1], byrow = TRUE)
    z[z == 0] <- 1
    datp[[jj]] <- s/z
    path.names <- c(path.names, datpT[[jj]]$title)
    hasR <- c(hasR, datpT[[jj]]$NumberOfReactions >= 1)
  }
  names(datp) <- names(datpT)
  names(path.names) <- names(datpT)
  tor <- lapply(datp, function(d) {
    sum(abs(d))
  }) == 0 | hasR | is.na(path.names)
  datp <- datp[!tor]
  path.names <- path.names[!tor]
  IDsNotP <- names(de)[!names(de) %in% all]
  if (length(IDsNotP)/length(de) > 0.01) {
    stop("More than 1% of your de genes have IDs are not present in the reference array!. Are you sure you use the right reference array?")
  }
  if (!length(IDsNotP) == 0) {
    cat("The following IDs are missing from all vector...:\n")
    cat(paste(IDsNotP, collapse = ","))
    cat("\nThey were added to your universe...")
    all <- c(all, IDsNotP)
  }
  if (length(intersect(names(de), all)) != length(de)) {
    stop("de must be a vector of log2 fold changes. The names of de should be included in the refference array!")
  }
  nGP<-pSize<-smPFS<-NULL
  for (i in 1:length(names(datp))) {
    path <- names(datp)[i]
    M <- datp[[path]]
    diag(M) <- diag(M) - 1
    X <- de[rownames(M)]
    noMy <- sum(!is.na(X))
    if ((noMy) > 0 & (abs(det(M)) > 1e-07)) {
      X[is.na(X)] <- 0
      pfs <- solve(M, -X)
      smPFS[i] <- sum(pfs)
      smPFS[i]<-smPFS[i]/noMy
    }
    else {
      smPFS[i]<-NA
    }
    if (verbose) {
      cat("\n")
      cat(paste("Done pathway ", i, " : ",
                substr(path.names[names(datp)[i]], 1, 30), "..", sep = ""))
    }
  }
  Name = path.names[names(datp)]
  res <- data.frame(Name, ID = names(datp), smPFS,stringsAsFactors = FALSE)
  res <- res[!is.na(res$smPFS), ]
  rownames(res) <- NULL
  return(res)
}
