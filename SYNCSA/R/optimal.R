#' @title Searching for optimal traits
#'
#' @description Maximize trait-convergence assembly patterns (TCAP = roTE), trait-divergence
#' assembly patterns (TDAP = roXE.T), maximize both trait-divergence assembly
#' patterns and trait-convergence assembly patterns (TCAP.TDAP = roXE) or
#' alpha divergence (roRE) For more details, see \code{\link{syncsa}}.
#'
#' @details Package \strong{SYNCSA} requires that the species and community sequence in
#' the data.frame or matrix must be the same for all dataframe/matrices.
#' The function \code{\link{organize.syncsa}} organizes the data for the functions
#' of the package, placing the matrices of community, traits and
#' environmental varibles in the same order. The function
#' use of function organize.syncsa is not requered for run the functions, but
#' is recommended. In this way the arguments comm, traits, envir, as well as the argument
#' put.together, can be specified them as normal arguments or by passing them
#' with the object returned by the function \code{\link{organize.syncsa}} using, in this
#' case only the argument comm. Using the object returned by organize.syncsa, the comm argument
#' is used as an alternative way of entering to set all data.frames/matrices, and therefore
#' the other arguments (traits, envir, and put.together) must be null.
#'
#'
#' @encoding UTF-8
#' @importFrom vegan vegdist
#' @importFrom stats cor
#' @importFrom utils combn
#' @aliases optimal print.optimal
#' @param comm Community data, with species as columns and sampling units as
#' rows. This matrix can contain either presence/absence or abundance data.
#' Alternatively comm can be an object of class metacommunity.data, an alternative
#' way to set all data.frames/matrices. When you use the class metacommunity.data the arguments
#' traits, envir and put.together must be null. See details.
#' @param traits Matrix data of species described by traits, with traits as
#' columns and species as rows (Default traits = NULL).
#' @param envir Environmental variables for each community, with variables as
#' columns and sampling units as rows (Default envir = NULL).
#' @param checkdata Logical argument (TRUE or FALSE) to check if species
#' sequence in the community data follows the same order as the one in the
#' trait and if sampling units in the community data follows the same order as the one in the
#' environmental matrices (Default checkdata = TRUE).
#' @param subset.min Minimum of traits in each subset (Default subset.min = 1).
#' @param subset.max Maximum of traits in each subset (Default subset.max = ncol(traits)).
#' @param pattern Patterns for maximize correlation, "tcap", "tdap",
#' "tcap.tdap" or "rao" (Default pattern = NULL).
#' @param ro.method Method to obtain the correlation, "mantel" or "procrustes"
#' (Default ro.method = "mantel").
#' @param method Correlation method, as accepted by cor: "pearson", "spearman"
#' or "kendall".
#' @param dist Dissimilarity index, as accepted by vegdist: "manhattan",
#' "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower",
#' "altGower", "morisita", "horn", "mountford", "raup" , "binomial" or "chao".
#' @param scale Logical argument (TRUE or FALSE) to specify if the traits are
#' measured on different scales (Default Scale = TRUE). When scale = TRUE traits
#' are measured on different scales and the matrix T is subjected to
#' standardization within each trait. When scale = FALSE if traits are measured on
#' the same scale and the matrix T is not subjected to standardization.
#' Furthermore, if scale = TRUE the matrix of traits is subjected to
#' standardization within each trait, and Gower Index is used to calculate the
#' degree of belonging to the species, and if scale = FALSE the matrix of
#' traits is not subjected to standardization, and Euclidean distance is
#' calculated to determine the degree of belonging to the species.
#' @param scale.envir Logical argument (TRUE or FALSE) to specify if the
#' environmental variables are measured on different scales (Default scale =
#' TRUE). If the enviromental variables are measured on different scales, the
#' matrix is subjected to centralization and standardization within each
#' variable.
#' @param ranks Logical argument (TRUE or FALSE) to specify if ordinal variables are
#' convert to ranks (Default ranks = TRUE).
#' @param ord Method to be used for ordinal variables, see \code{\link{gowdis}}
#' (Default ord = "metric").
#' @param na.rm Logical argument (TRUE or FALSE) to specify if pairwise
#' deletion of missing observations when computing dissimilarities (Default
#' na.rm = FALSE).
#' @param notification Logical argument (TRUE or FALSE) to specify if
#' notifications of missing observations are shown (Default notification =
#' TRUE).
#' @param put.together List to specify group traits that are added or removed
#' together (Default put.together = NULL). This argument must be a list, see
#' examples.
#' @param progressbar Logical argument (TRUE or FALSE) to specify if display a
#' progress bar on the R console (Default progressbar = FALSE).
#' @param x An object of class optimal.
#' @param ... Other parameters for the respective functions.
#' @return \item{Subset}{Subset of traits that maximizes the correlation.}
#' \item{ro}{Correlation for the subset of traits.}
#' @note \strong{IMPORTANT}: The sequence species show up in community data
#' matrix MUST be the same as they show up in traits matrix. See details and
#' \code{\link{organize.syncsa}}.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{syncsa}}, \code{\link{organize.syncsa}}
#' @references Pillar, V.D.; Duarte, L.d.S. (2010). A framework for
#' metacommunity analysis of phylogenetic structure. Ecology Letters, 13,
#' 587-596.
#'
#' Pillar, V.D., Duarte, L.d.S., Sosinski, E.E. & Joner, F. (2009).
#' Discriminating trait-convergence and trait-divergence assembly patterns in
#' ecological community gradients. Journal of Vegetation Science, 20, 334:348.
#' @keywords SYNCSA
#' @examples
#' data(flona)
#' optimal(flona$community, flona$traits, flona$environment, subset.min = 3,
#'    subset.max = 5, pattern = "tcap")
#' optimal(flona$community, flona$traits, flona$environment, subset.min = 3,
#'    subset.max = 5, pattern = "tdap")
#' optimal(flona$community, flona$traits, flona$environment, subset.min = 3,
#'    subset.max = 5, pattern = "tcap.tdap")
#' put.together <- list(c("fol", "sem"), c("tam", "red"))
#' put.together
#' optimal(flona$community, flona$traits, flona$environment, subset.min = 1,
#'    subset.max = 3, pattern = "tcap", put.together = put.together)
#' @export
optimal<-function (comm, traits = NULL, envir = NULL, checkdata = TRUE,
                   subset.min = 1, subset.max = ncol(traits),
                   pattern = NULL, ro.method = "mantel", dist = "euclidean", method = "pearson",
                   scale = TRUE, scale.envir = TRUE, ranks = TRUE, ord = "metric",
                   put.together = NULL, na.rm = FALSE, notification = TRUE,
                   progressbar = FALSE)
{
  res <- list(call = match.call())
  roMETHOD <- c("mantel", "procrustes")
  romethod <- pmatch(ro.method, roMETHOD)
  if (length(romethod) > 1) {
    stop("\n Only one argument is accepted in ro.method \n")
  }
  if (is.na(romethod)) {
    stop("\n Invalid ro.method \n")
  }
  PATTERNS <- c("tcap", "tdap", "tcap.tdap", "rao")
  pattern <- pmatch(pattern, PATTERNS)
  if (length(pattern) != 1) {
    stop("\n Only one argument is accepted in pattern \n")
  }
  if (is.na(pattern)) {
    stop("\n Invalid pattern \n")
  }
  if (inherits(comm, "metacommunity.data")) {
    if (!is.null(traits) | !is.null(envir) | !is.null(put.together)) {
      stop("\n When you use an object of class metacommunity.data the arguments traits, envir and put.together must be null. \n")
    }
    traits <- comm$traits
    envir <- comm$environmental
    put.together <- comm$put.together
    comm <- comm$community
  }
  list.warning <- list()
  if(checkdata){
    organize.temp <- organize.syncsa(comm, traits = traits, envir = envir, check.comm = TRUE)
    if(!is.null(organize.temp$stop)){
      organize.temp$call <- match.call()
      return(organize.temp)
    }
    list.warning <- organize.temp$list.warning
    comm <- organize.temp$community
    traits <- organize.temp$traits
    envir <- organize.temp$environmental
  }
  if(length(list.warning)>0){
    res$list.warning <- list.warning
  }
  if (is.null(traits)) {
    stop("\n traits is NULL \n")
  }
  if (is.null(envir)) {
    stop("\n envir is NULL \n")
  }
  if (notification & !checkdata) {
    if (any(is.na(comm))) {
      warning("Warning: NA in community data", call. = FALSE)
    }
    if (any(is.na(traits))) {
      warning("Warning: NA in traits matrix", call. = FALSE)
    }
    if (any(is.na(envir))) {
      warning("Warning: NA in environmental data", call. = FALSE)
    }
  }
  if(!checkdata){
    commvartype <- var.type(comm)
    if(any(commvartype == "n")){
      stop("\n comm must contain only numeric, binary or ordinal variables \n")
    }
    if (!is.null(traits)) {
      traitsvartype <- var.type(traits)
      if(any(traitsvartype == "n")){
        stop("\n trait must contain only numeric, binary or ordinal variables \n")
      }
    }
    if (!is.null(envir)) {
      envirvartype <- var.type(envir)
      if(any(envirvartype == "n")){
        stop("\n envir must contain only numeric, binary or ordinal variables \n")
      }
    }
  }
  make.names <- is.null(colnames(traits))
  colnames(traits) <- colnames(traits, do.NULL = FALSE, prefix = "T")
  if (scale.envir) {
    envir <- cent.norm(envir, na.rm = na.rm)
  }
  if (romethod == 1) {
    dist.y <- vegan::vegdist(envir, method = dist, na.rm = na.rm)
  }
  m <- ncol(traits)
  weights <- rep(1, m)
  names(weights) <- colnames(traits)
  p <- 1:subset.max
  names.traits <- colnames(traits)
  if(!is.null(put.together)){
    if(class(put.together) != "list"){
      stop("\n put.together must be a object of class list\n")
    }
    if(make.names){
      for(k in 1:length(put.together)){
        put.together[[k]] <- paste("T", put.together[[k]], sep = "")
      }
    }
    if(max(table(unlist(put.together)))>1){
      stop("\n The same trait appears more than once in put.together\n")
    }
    if(length(setdiff(unlist(put.together), colnames(traits)))>0){
      stop("\n Check traits names in put.together\n")
    }
    for(k in 1:length(put.together)){
      names.traits[which(names.traits == put.together[[k]][1])] <- paste(put.together[[k]], collapse = " ")
      names.traits <- setdiff(names.traits, put.together[[k]][-1])
      weights[put.together[[k]]] <- 1/length(put.together[[k]])
    }
    m <- length(names.traits)
    put.together2 <- list()
    for(k in 1:length(put.together)){
      put.together2[[k]] <- paste(put.together[[k]], collapse = " ")
    }
  }
  if (subset.max > m) {
    stop("\n subset must be lower than the number of traits\n")
  }
  bin <- factorial(m)/(factorial(p) * factorial(m - p))
  nT <- sum(bin[subset.min:subset.max])
  comb <- matrix(NA, nrow = sum(bin[subset.min:subset.max]), ncol = 1)
  n <- 0
  for (i in subset.min:subset.max) {
    combinations <- utils::combn(names.traits, i, simplify = TRUE)
    for (j in 1:bin[i]) {
      n <- n + 1
      comb[n, 1] <- paste(combinations[, j], collapse = " ")
    }
  }
  n <- 0
  correlation <- matrix(NA, nrow = sum(bin[subset.min:subset.max]), ncol = 1)
  for (i in subset.min:subset.max) {
    combinations1 <- utils::combn(names.traits, i, simplify = TRUE)
    for (j in 1:bin[i]) {
      if (pattern == 1) {
        n <- n + 1
        choose.traits <- combinations1[, j]
        if(!is.null(put.together)){
          if(sum(match(choose.traits, unlist(put.together2)), na.rm = TRUE)>0){
            choose.traits2 <- intersect(choose.traits, unlist(put.together2))
            choose.traits3 <- c()
            for(k in 1:length(choose.traits2)){
              for(l in 1:length(put.together)){
                if(choose.traits2[k] == put.together2[[l]]){
                  choose.traits3 <- c(choose.traits3, put.together[[l]])
                }
              }
            }
            choose.traits <- c(choose.traits3, setdiff(choose.traits, unlist(put.together2)))
          }
        }
        T <- matrix.t(comm, traits[, choose.traits, drop = FALSE], scale = scale, ranks = ranks, notification = FALSE)
        if (romethod == 1) {
          correlation[n, 1] <- stats::cor(vegan::vegdist(as.matrix(T$matrix.T), method = dist, na.rm = na.rm), dist.y, method = method)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
        if (romethod == 2) {
          correlation[n, 1] <- procrustes.syncsa(T$matrix.T, envir)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
      }
      if (pattern == 2) {
        n <- n + 1
        choose.traits <- combinations1[, j]
        if(!is.null(put.together)){
          if(sum(match(choose.traits,unlist(put.together2)), na.rm = TRUE)>0){
            choose.traits2 <- intersect(choose.traits, unlist(put.together2))
            choose.traits3 <- c()
            for(k in 1:length(choose.traits2)){
              for(l in 1:length(put.together)){
                if(choose.traits2[k] == put.together2[[l]]){
                  choose.traits3 <- c(choose.traits3, put.together[[l]])
                }
              }
            }
            choose.traits <- c(choose.traits3, setdiff(choose.traits, unlist(put.together2)))
          }
        }
        T <- matrix.t(comm, as.matrix(traits[, choose.traits, drop=FALSE]), scale = scale, ranks = ranks, notification = FALSE)
        X <- matrix.x(comm, traits[, choose.traits, drop = FALSE], scale = scale, ranks = ranks, ord = ord, notification = FALSE, w = weights[choose.traits])
        if (romethod == 1) {
          dist.x <- vegan::vegdist(X$matrix.X, method = dist, na.rm = na.rm)
          dist.z <- vegan::vegdist(T$matrix.T, method = dist, na.rm = na.rm)
          rxy <- stats::cor(dist.x, dist.y, method = method)
          rxz <- stats::cor(dist.x, dist.z, method = method)
          ryz <- stats::cor(dist.y, dist.z, method = method)
          correlation[n, 1] <- part.cor(rxy, rxz, ryz)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
        if (romethod == 2) {
          correlation[n, 1] <- procrustes.partial(X$matrix.X, envir, T$matrix.T)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
      }
      if (pattern == 3) {
        n <- n + 1
        choose.traits <- combinations1[, j]
        if(!is.null(put.together)){
          if(sum(match(choose.traits, unlist(put.together2)), na.rm = TRUE)>0){
            choose.traits2 <- intersect(choose.traits, unlist(put.together2))
            choose.traits3 <- c()
            for(k in 1:length(choose.traits2)){
              for(l in 1:length(put.together)){
                if(choose.traits2[k] == put.together2[[l]]){
                  choose.traits3 <- c(choose.traits3, put.together[[l]])
                }
              }
            }
            choose.traits <- c(choose.traits3, setdiff(choose.traits, unlist(put.together2)))
          }
        }
        X <- matrix.x(comm, traits[, choose.traits, drop = FALSE], scale = scale, ranks = ranks, ord = ord, notification = FALSE, w = weights[choose.traits])
        if (romethod == 1) {
          correlation[n, 1] <- stats::cor(vegan::vegdist(as.matrix(X$matrix.X), method = dist, na.rm = na.rm), dist.y, method = method)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
        if (romethod == 2) {
          correlation[n, 1] <- procrustes.syncsa(X$matrix.X, envir)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
      }
      if (pattern == 4) {
        n <- n + 1
        choose.traits <- combinations1[, j]
        if(!is.null(put.together)){
          put.together.temp <- put.together
          if(sum(match(choose.traits,unlist(put.together2)), na.rm = TRUE)>0){
            choose.traits2 <- intersect(choose.traits, unlist(put.together2))
            choose.traits3 <- c()
            for(k in 1:length(choose.traits2)){
              for(l in 1:length(put.together)){
                if(choose.traits2[k] == put.together2[[l]]){
                  choose.traits3 <- c(choose.traits3, put.together[[l]])
                }
              }
            }
            choose.traits <- c(choose.traits3, setdiff(choose.traits, unlist(put.together2)))
          }
          put.together.temp <- put.together.temp[sapply(put.together.temp, function(x) any(x%in% choose.traits))]
          if(length(put.together.temp)==0){
            put.together.temp <- NULL
          }
        } else{
          put.together.temp <- NULL
        }
        T <- matrix.t(comm, as.matrix(traits[, choose.traits, drop=FALSE]), scale = scale, ranks = ranks, notification = FALSE)
        RAO <- cbind(rao.diversity(comm, traits = T$matrix.b, checkdata = FALSE, put.together = put.together.temp)$FunRao)
        colnames(RAO) <- "FunRao"
        if (romethod == 1) {
          correlation[n, 1] <- stats::cor(vegan::vegdist(RAO, method = dist, na.rm = na.rm), dist.y, method = method)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
        if (romethod == 2) {
          correlation[n, 1] <- procrustes.syncsa(RAO, envir)
          if (progressbar) {
            ProgressBAR(n, nT, style = 3)
          }
        }
      }
    }
  }
  result <- data.frame(Subset = comb, ro = correlation, stringsAsFactors = FALSE)
  if(pattern == 1 | pattern ==3 | pattern == 4){
    result <- result[order(result[, 2], decreasing = TRUE), ]
  } else{
    result <- result[order(abs(result[, 2]), decreasing = TRUE), ]
  }
  res$N_subset <- nT
  res$optimization <- result
  res$weights <- weights
  class(res) <- "optimal"
  return(res)
}
