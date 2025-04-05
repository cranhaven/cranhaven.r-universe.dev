### This file is part of 'rpcss' package for R.

### Copyright (C) 2024-2025, ICAR-NBPGR.
#
# rpcss is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# rpcss is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/

#' Principal Component Scoring to Generate Core collections
#'
#' Generate a Core Collection with Principal Component Scoring Strategy (PCSS)
#' \insertCite{hamon_proposed_1990,noirot_principal_1996,noirot_method_2003}{rpcss}
#' using qualitative and/or quantitative trait data. \loadmathjax
#'
#' A core collection is constituted from an entire collection of \mjseqn{N}
#' genotypes using quantitative data of \mjseqn{J} traits using Principal
#' Component Scoring Strategy (PCSS)
#' \insertCite{hamon_proposed_1990,noirot_principal_1996,noirot_method_2003}{rpcss}
#' as follows:
#'
#' \enumerate{
#'
#' \item Principal Component Analysis (PCA) is performed on the standardized
#' genotype \mjseqn{\times} trait data. This takes care of multicollinearity
#' between the traits to generate \mjseqn{J} standardized and independent
#' variables or factors or principal component.
#'
#' \item Considering only a subset of factors \mjseqn{K}, the Generalized Sum of
#'  Squares (GSS) of N individuals in K factorial spaces is computed as
#' \mjseqn{N \times K}.
#'
#' \mjseqn{K} can be the number of factors for which the eigen value
#' \mjseqn{\lambda} is greater than a threshold value such as 1 (Kaiser-Guttman
#' criterion) or the average of all the eigen values.
#'
#' \item The contribution of the \mjseqn{i}th genotype to GSS (\mjseqn{P_{i}})
#' or total variability is calculated as below.
#'
#' \mjsdeqn{P_{i} = \sum_{j = 1}^{K} x_{ij}^{2}}
#'
#' Where \mjseqn{x_{ij}} is the component score or coordinate of the
#' \mjseqn{i}th genotype on the \mjseqn{j}th principal component.
#'
#' \item For each genotype, its relative contribution to GSS or total
#' variability is computed as below.
#'
#' \mjsdeqn{CR_{i} = \frac{P_{i}}{N \times K}}
#'
#' \item The genotypes are sorted in descending order of magnitude of their
#' contribution to GSS and then the cumulative contribution of successive
#' genotypes to GSS is computed.
#'
#' \item The core collection can then be selected by three different methods.
#'
#'  \enumerate{
#'
#'  \item Selection of fixed proportion or percentage or number of the top
#'  accessions.
#'
#'  \item Selection of the top accessions that contribute up to a fixed
#'  percentage of the GSS.
#'
#'  \item Fitting a logistic regression model of the following form to the
#'  cumulative contribution of successive genotypes to GSS
#'  \insertCite{balakrishnan_method_2000}{rpcss}.
#'
#'  \mjsdeqn{\frac{y}{A-y} = e^{a + bn}}
#'
#'  The above equation can  be reparameterized as below.
#'
#'  \mjsdeqn{\log_{e} \left ( {\frac{y}{A-y}} \right ) = a + bn}
#'
#'  Where, \mjseqn{a} and \mjseqn{b} are the intercept and regression
#'  coefficient, respectively; \mjseqn{y} is the cumulative contribution of
#'  successive genotypes to GSS; \mjseqn{n} is the rank of the genotype when
#'  sorted according to the contribution to GSS and \mjseqn{A} is the asymptote
#'  of the curve (\mjseqn{A = 100}).
#'
#'  The rate of increase in the successive contribution of genotypes to GSS can
#'  be computed by the following equation to find the point of inflection where
#'  the rate of increase starts declining.
#'
#'  \mjseqn{\frac{\mathrm{d} y}{\mathrm{d} x} = by(A-y)}
#'
#'  The number of accessions included till the peak or infection point are
#'  selected to constitute the core collection.
#'
#'  }
#'
#' }
#'
#' Similarly for qualitative traits, standardized and independent variables or
#' factors can be obtained by Correspondence Analysis (CA) on complete
#' disjunctive table of genotype \mjseqn{\times} trait data or to be specific
#' Multiple Correspondence Analysis (MCA). In \code{rpcss}, this has also been
#' extended for data sets having both quantitative and qualitative traits by
#' implementing Factor Analysis for Mixed Data (FAMD) for obtaining standardized
#' and independent variables or factors.
#'
#' In \code{rpcss}, PCA, MCA and FAMD are implemented via the
#' \code{\link[FactoMineR]{FactoMineR}} package.
#' \insertCite{le_FactoMineR_2008,husson_Exploratory_2017}{rpcss}.
#'
#' @param data The data as a data frame object. The data frame should possess
#'   one row per individual and columns with the individual names and multiple
#'   trait/character data.
#' @param names Name of column with the individual/genotype names as a character
#'   string.
#' @param quantitative Name of columns with the quantitative traits as a
#'   character vector.
#' @param qualitative Name of columns with the qualitative traits as a character
#'   vector.
#' @param eigen.threshold The lower limit of the eigen value of factors to be
#'   included in the estimation. The default value is the average of all the
#'   eigen values.
#' @param size The desired core set size proportion.
#' @param var.threshold The desired proportion of total variability to be
#'
#' @return A list of class \code{pcss.core} with the following components.
#'   \item{details}{The details of the core set generation process.}
#'   \item{raw.out}{The original output of \code{\link[FactoMineR]{PCA}},
#'   \code{\link[FactoMineR]{CA}} and \code{\link[FactoMineR]{FAMD}} functions
#'   of \code{\link[FactoMineR]{FactoMineR}}} \item{eigen}{A data frame with
#'   eigen values and their partial and cumulative contribution to percentage of
#'   variance.} \item{eigen.threshold}{The threshold eigen value used.}
#'   \item{rotation}{A matrix of rotation values or loadings.} \item{scores}{A
#'   matrix of scores from PCA, CA or FAMD.} \item{variability.ret}{A data frame
#'   of individuals/genotypes ordered by variability retained.}
#'   \item{cores.info}{A data frame of core set size and percentage variability
#'   retained according to the method used.}
#'
#' @seealso \code{\link[FactoMineR]{PCA}}, \code{\link[FactoMineR]{CA}} and
#'   \code{\link[FactoMineR]{FAMD}}
#'
#' @import gslnls
#' @import mathjaxr
#' @importFrom FactoMineR PCA CA FAMD
#' @importFrom Rdpack reprompt
#' @importFrom stats coef
#' @importFrom utils data
#' @export
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Prepare example data
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' suppressPackageStartupMessages(library(EvaluateCore))
#'
#' # Get data from EvaluateCore
#'
#' data("cassava_EC", package = "EvaluateCore")
#' data = cbind(Genotypes = rownames(cassava_EC), cassava_EC)
#' quant <- c("NMSR", "TTRN", "TFWSR", "TTRW", "TFWSS", "TTSW", "TTPW", "AVPW",
#'            "ARSR", "SRDM")
#' qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#' rownames(data) <- NULL
#'
#' # Convert qualitative data columns to factor
#' data[, qual] <- lapply(data[, qual], as.factor)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (quantitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out1 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = NULL, eigen.threshold = NULL, size = 0.2,
#'                   var.threshold = 0.75)
#'
#' out1
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
#'                   qualitative = qual, eigen.threshold = NULL,
#'                   size = 0.2, var.threshold = 0.75)
#'
#' out2
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (quantitative and qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out3 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = qual, eigen.threshold = NULL)
#'
#' out3
#'
#'
pcss.core <- function(data, names, quantitative, qualitative,
                      eigen.threshold = NULL,
                      size = 0.2, var.threshold = 0.75) {

  # Checks ----

  if (missing(quantitative)) {
    quantitative <- NULL
  }

  if (missing(qualitative)) {
    qualitative <- NULL
  }

  if (length(c(quantitative, qualitative)) == 1) {
    stop("Only one trait specified.")
  }

  # check if 'data' is a data frame object
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  # check if 'names' argument is character vector of unit length
  if (!(is.character(names) && length(names) == 1)) {
    stop('"names" should be a character vector of unit length.')
  }

  # check if 'quantitative' argument is a character vector
  if (!is.null(quantitative)) {
    if (!is.character(quantitative)) {
      stop('"quantitative" should be a character vector.')
    }
  }

  # check if 'qualitative' argument is a character vector
  if (!is.null(qualitative)) {
    if (!is.character(qualitative)) {
      stop('"qualitative" should be a character vector.')
    }
  }

  # check if 'names' column is present in 'data'
  if (!(names %in% colnames(data))) {
    stop(paste('Column ', names,
               ' specified as the "names" column is not present in "data".',
               sep = ""))
  }

  # check if 'quantitative' columns are present in 'data'
  if (!is.null(quantitative)) {
    if (FALSE %in% (quantitative %in% colnames(data)))  {
      stop(paste('The following column(s) specified in "quantitative" ',
                 'not present in "data":\n',
                 paste(quantitative[!(quantitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'qualitative' columns are present in 'data'
  if (!is.null(qualitative)) {
    if (FALSE %in% (qualitative %in% colnames(data)))  {
      stop(paste('The following column(s) specified in "qualitative" ',
                 'not present in "data":\n',
                 paste(qualitative[!(qualitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if overlap exists between 'quantitative' and 'qualitative'
  if ((!is.null(quantitative)) && (!is.null(qualitative))) {
    if (length(intersect(quantitative, qualitative)) != 0) {
      stop(paste('The following column(s) is/are specified in both ',
                 '"quantitative" and "qualitative":\n',
                 paste(intersect(quantitative, qualitative),
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'names' column is of type character
  if (!is.character(data[, names])) {
    stop('"names" column in "data" should be of type character.')
  }


  # check if 'quantitative' columns are of type numeric/integer
  if (!is.null(quantitative)) {
    intquantcols <-
      unlist(lapply(data[, quantitative],
                    function(x) FALSE %in% (is.vector(x, mode = "integer") |
                                              is.vector(x, mode = "numeric"))))
    if (TRUE %in% intquantcols) {
      stop(paste('The following "quantitative" column(s) in "data" are not ',
                 'of type numeric:\n',
                 paste(names(intquantcols[intquantcols]), collapse = ", ")))
    }
  }

  # check if 'qualitative' columns are of type factor
  if (!is.null(qualitative)) {
    intqualcols <- unlist(lapply(data[, qualitative],
                                 function(x) is.factor(x)))
    if (FALSE %in% intqualcols) {
      stop(paste('The following "qualitative" column(s) in "data" are not ',
                 'of type factor:\n',
                 paste(names(intqualcols[!intqualcols]), collapse = ", ")))
    }
  }

  # check for missing values
  missvcols <- unlist(lapply(data[, quantitative],
                             function(x) TRUE %in% is.na(x)))
  if (TRUE %in% missvcols) {
    stop(paste('The following column(s) in "data" have missing values:\n',
               paste(names(missvcols[missvcols]), collapse = ", ")))
  }

  # check for duplication in names
  if (any(duplicated(data[, names]))) {
    stop('Duplicated entries exist in "names" column.')
  }

  # check if 'eigen.threshold' argument is numeric vector of unit length
  if (!is.null(eigen.threshold)) {
    if (!(is.numeric(eigen.threshold) && length(eigen.threshold) == 1)) {
      stop('"eigen.threshold" should be a numeric vector of unit length.')
    }
  }

  # check if 'size' argument is numeric vector of unit length
  if (!is.null(size)) {
    if (!(is.numeric(size) && length(size) == 1)) {
      stop('"size" should be a numeric vector of unit length.')
    }
  }

  # check if 'size' is a proportion between 0 and 1
  if (size <= 0 || size >= 1) {
    stop('"size" should be a proportion between 0 and 1.')
  }

  # check if 'var.threshold' argument is numeric vector of unit length
  if (!is.null(var.threshold)) {
    if (!(is.numeric(var.threshold) && length(var.threshold) == 1)) {
      stop('"var.threshold" should be a numeric vector of unit length.')
    }
  }

  # check if 'var.threshold' is a proportion between 0 and 1
  if (var.threshold <= 0 || var.threshold >= 1) {
    stop('"var.threshold" should be a proportion between 0 and 1.')
  }

  # Prepare data ----

  dataf <- data[, c(names, quantitative, qualitative)]
  rownames(dataf) <- dataf[, names]
  dataf[, names] <- NULL

  pca_out <- NULL
  mca_out <- NULL
  famd_out <- NULL

  method <- NULL

  # PCA ----

  if (is.null(qualitative) && !is.null(quantitative)) {

    dataf <- dataf[, quantitative]

    dataf[, quantitative] <- lapply(dataf[, quantitative], function(x) {
      as.numeric(x)
    })

    ## Run PCA ----
    pca_out <- FactoMineR::PCA(X = dataf, scale.unit = TRUE,
                               ncp = length(quantitative),
                               graph = FALSE)

    ## Get Eigen values ----
    eig <- pca_out$eig[, "eigenvalue"]

    # round(sum(eig)) == length(quantitative)

    ## Get Loadings ----
    rot <- pca_out$svd$V

    ## Get Importance of factors/principal coordinates ----
    imp <-
      data.frame(# `Standard deviation` = pca_out$svd$vs,
        `Eigen value` = pca_out$eig[, "eigenvalue"],
        `Percentage of variance` = pca_out$eig[, "percentage of variance"],
        `Cumulative percentage of variance` =
          pca_out$eig[, "cumulative percentage of variance"],
        check.names = FALSE)

    rownames(imp) <- gsub("comp", "Dim", rownames(imp))

    ## Get Principal component scores ----
    scores <- pca_out$ind$coord

    ## Method ----
    method <- "PCA"
  }

  # Run MCA ----

  if (!is.null(qualitative) && is.null(quantitative)) {

    dataf <- dataf[, qualitative]

    # dataf[, qualitative] <- lapply(dataf[, qualitative], function(x) {
    #   as.numeric(as.factor(x))
    # })

    # dataf[, qualitative] <- lapply(dataf[, qualitative], function(x) {
    #   as.factor(x)
    # })

    ncp <- sum(unlist(lapply(dataf, function(x) length(levels(x)))))

    ## Run MCA ----
    mca_out <- FactoMineR::MCA(X = dataf,
                               ncp = ncp,
                               graph = FALSE)

    ## Get Eigen values ----
    eig <- mca_out$eig[, "eigenvalue"]

    ## Get Loadings ----
    rot <- mca_out$svd$V

    ## Get Importance of factors/principal coordinates ----
    imp <-
      data.frame(# `Standard deviation` =
                 #   mca_out$svd$vs[1:(length(qualitative) - 1)],
        `Eigen value` = mca_out$eig[, "eigenvalue"],
        `Percentage of variance` = mca_out$eig[, "percentage of variance"],
        `Cumulative percentage of variance` =
          mca_out$eig[, "cumulative percentage of variance"],
        check.names = FALSE)

    rownames(imp) <- gsub("dim", "Dim", rownames(imp))

    ## Get Principal component scores ----
    scores <- mca_out$ind$coord

    ## Method ----
    method <- "MCA"

  }

  # Run FAMD ----

  if (!is.null(qualitative) && !is.null(quantitative)) {

    dataf[, quantitative] <- lapply(dataf[, quantitative], function(x) {
      as.numeric(x)
    })

    dataf[, qualitative] <- lapply(dataf[, qualitative], function(x) {
      as.factor(x)
    })

    ## Run FAMD ----
    famd_out <- FactoMineR::FAMD(base = dataf,
                                 ncp = length(c(quantitative, qualitative)),
                                 graph = FALSE)

    ## Get Eigen values ----
    eig <- famd_out$eig[, "eigenvalue"]

    # round(sum(eig)) == length(quantitative)

    ## Get Loadings ----
    rot <- famd_out$svd$V

    ## Get Importance of factors/principal coordinates ----
    imp <-
      data.frame(#`Standard deviation` = famd_out$svd$vs,
        `Eigen value` = famd_out$eig[, "eigenvalue"],
        `Percentage of variance` = famd_out$eig[, "percentage of variance"],
        `Cumulative percentage of variance` =
          famd_out$eig[, "cumulative percentage of variance"],
        check.names = FALSE)

    rownames(imp) <- gsub("comp", "Dim", rownames(imp))

    ## Get Principal component scores ----
    scores <- famd_out$ind$coord

    ## Method ----
    method <- "FAMD"
  }

  # Contribution of individuals/genotypes to total GSS ----

  if (is.null(eigen.threshold)) {
    eigen.threshold <- mean(eig)
  } else {
    if (!(any(eig >= eigen.threshold))) {
      eigen.threshold <- mean(eig)
      warning('There are no eigen values \u2265 "eigen.threshold".\n',
              'Using average of the eigen values (', round(eigen.threshold, 2),
              ') as "eigen.threshold"', sep = "")
    }
  }


  N <- nrow(scores)
  K <- max(which((eig > eigen.threshold)))

  GSS <- N * K

  Pi <- rowSums(scores[, 1:K] ^ 2)
  Pimax <- rowSums(scores ^ 2)

  CRi <- Pi / GSS
  CRimax <- Pimax / GSS

  CRi <- sort(CRi, decreasing = TRUE)
  CRimax <- sort(CRimax, decreasing = TRUE)

  cumCRi <- cumsum(CRi)
  # cumCRimax <- cumsum(CRimax)

  # plot(cumCRimax, col = "green")
  # points(cumCRi, col = "red")

  # Select the core collection ----

  # Generalized sum of squares
  gssdf <- data.frame(Rank = seq_along(cumCRi),
                      VarRet = (cumCRi / max(cumCRi)) * 100)
  gssdf <- cbind(Id = rownames(gssdf), gssdf)
  rownames(gssdf) <- NULL

  ## By size specified ----
  size.sel <- ceiling(size * N)
  size.var <- gssdf[gssdf$Rank == size.sel, ]$VarRet

  ## By threshold variance ----
  var.threshold <- var.threshold * 100
  var.sel <- max(which(gssdf$VarRet <= var.threshold))

  ## With logistic regression ----

  # Fit a logistic model
  y <- gssdf$VarRet
  starta <- y[1] / (1 - y[1])
  startb <- -0.5

  maxiter <- 1024
  warnOnly <- TRUE

  dat <- data.frame(n = gssdf$Rank, y = gssdf$VarRet)

  mod <-
    gslnls::gsl_nls(
      y ~ 100 / (1 + exp(a + (b * n))),
      data = dat,
      algorithm = "lm",
      start = list(a = starta, b = startb),
      control = list(maxiter = maxiter, warnOnly = warnOnly,
                     scale = "levenberg")
    )

  # dat$pred <- 100 / (1 + exp(coef(mod)["a"] + (coef(mod)["b"] * dat$n)))

  # Compute rate of increase in variability retained
  b <- stats::coef(mod)["b"]
  dat$rate <- -b * dat$y * (100 - dat$y)

  reg.sel <- dat[dat$rate == max(dat$rate), ]$n
  reg.var <- gssdf[gssdf$Rank == reg.sel, ]$VarRet


  # Generate ouput ----

  rawout_ind <- c(pca_out = !is.null(pca_out),
                  mca_out = !is.null(mca_out),
                  famd_out = !is.null(famd_out))

  detailsdf <-
    data.frame(`Total number of individuals/genotypes` = N,
               `Quantitative traits` =  paste(quantitative, collapse = ", "),
               `Qualitative traits` =  paste(qualitative, collapse = ", "),
               `Method` = method,
               `Threshold eigen value` = eigen.threshold,
               `Number of eigen values selected` = K,
               `Threshold size` = size,
               `Threshold variance (%)` = var.threshold,
               check.names = FALSE)

  detailsdf <- t(detailsdf)
  detailsdf <- cbind(Detail = rownames(detailsdf),
                     Value = detailsdf[, 1])
  detailsdf <- data.frame(detailsdf, check.names = FALSE)

  rownames(detailsdf) <- NULL

  coreinfodf <- data.frame(Method = c("By size specified",
                                      "By threshold variance",
                                      "By logistic regression"),
                           Size = c(size.sel, var.sel, reg.sel),
                           VarRet = c(size.var, var.threshold, reg.var))

  rownames(coreinfodf) <- NULL

  out <- list(details = detailsdf,
              raw.out = get(names(which(rawout_ind))),
              eigen = imp,
              eigen.threshold = eigen.threshold,
              rotation = rot,
              scores = scores,
              variability.ret = gssdf,
              cores.info = coreinfodf)

  quali.levels <- NULL

  if (!is.null(qualitative)) {
    quali.levels <-  lapply(dataf[, qualitative],
                            function(x) levels(x))
  }

  class(out) <- "pcss.core"

  attr(x = out, which = "method") <- method
  attr(x = out, which = "quant") <- quantitative
  attr(x = out, which = "quali") <- qualitative
  attr(x = out, which = "quali.levels") <- quali.levels
  attr(x = out, which = "slope") <- b

  return(out)

}
