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

#' Plot Contribution or Loadings of Traits for each Dimension/Factor from
#' \code{pcss.core} Output
#'
#' \code{contrib.pcss.core} generates bar plots of contributions or loadings
#' ("right singular vectors") of traits for each dimension/factor from the
#' output of \code{pcss.core}.
#'
#' @param x An object of class \code{pcss.core}.
#' @param ndim The number of dimensions for which contribution or loadings of
#'   traits are to be plotted.
#' @param plot.loadings If \code{TRUE}, the loadings or "right singular vectors"
#'   are plotted instead of contributions. Default is \code{FALSE}.
#' @param use.sign If \code{TRUE}, contributions of variables are given the sign
#'   of their corresponding coordinates. Default is \code{TRUE}.
#' @param sort.value If \code{TRUE}, the bars are sorted according to their
#'   value.
#' @param ... Unused.
#'
#' @return The contributions/loadings bar plot as a \code{ggplot} object.
#'
#' @seealso \code{\link[rpcss]{pcss.core}}
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @export
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
#'
#' library(FactoMineR)
#' suppressPackageStartupMessages(library(factoextra))
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # With quantitative data
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out1 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = NULL, eigen.threshold = NULL, size = 0.2,
#'                   var.threshold = 0.75)
#'
#'\donttest{
#' # Plot contributions of genotypes - with sign - sorted
#' contrib(x = out1, ndim = 5)
#'
#' # Plot contributions of genotypes - without sign - sorted
#' contrib(x = out1, ndim = 5, use.sign = FALSE)
#'
#' # Plot loadings/coordinates of genotypes - with sign - sorted
#' contrib(x = out1, ndim = 5, plot.loadings = TRUE)
#'
#' # Plot contributions of genotypes - with sign - unsorted
#' contrib(x = out1, ndim = 5, sort.value = FALSE)
#'
#' # Plot biplot with factoextra
#' fviz_contrib(out1$raw.out, choice = "var", axes = 1)
#' fviz_contrib(out1$raw.out, choice = "var", axes = 2)
#'}
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out2 <- pcss.core(data = data, names = "Genotypes", quantitative = NULL,
#'                   qualitative = qual, eigen.threshold = NULL,
#'                   size = 0.2, var.threshold = 0.75)
#'
#'\donttest{
#' # Plot contributions of genotypes - with sign - sorted
#' contrib(x = out2, ndim = 5)
#'
#' # Plot contributions of genotypes - without sign - sorted
#' contrib(x = out2, ndim = 5, use.sign = FALSE)
#'
#' # Plot loadings/coordinates of genotypes - with sign - sorted
#' contrib(x = out2, ndim = 5, plot.loadings = TRUE)
#'
#' # Plot contributions of genotypes - with sign - unsorted
#' contrib(x = out2, ndim = 5, sort.value = FALSE)
#'
#' # Plot biplot with factoextra
#' fviz_contrib(out2$raw.out, choice = "var", axes = 1)
#' fviz_contrib(out2$raw.out, choice = "var", axes = 2)
#'}
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Get core sets with PCSS (quantitative and qualitative data)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' out3 <- pcss.core(data = data, names = "Genotypes",
#'                   quantitative = quant,
#'                   qualitative = qual, eigen.threshold = NULL)
#'
#'\donttest{
#' # Plot contributions of genotypes - sorted
#' contrib(x = out3, ndim = 5)
#'
#' # Plot contributions of genotypes - without sign - sorted
#' contrib(x = out3, ndim = 5, use.sign = FALSE)
#'
#' # Plot loadings/coordinates of genotypes - sorted
#' contrib(x = out3, ndim = 5, plot.loadings = TRUE)
#'
#' # Plot contributions of genotypes - with sign - unsorted
#' contrib(x = out3, ndim = 5, sort.value = FALSE)
#'
#' # Plot biplot with factoextra
#' # fviz_contrib(out3$raw.out, choice = "quanti.var", axes = 1)
#' # fviz_contrib(out3$raw.out, choice = "quali.var", axes = 1)
#' # fviz_contrib(out3$raw.out, choice = "quanti.var", axes = 2)
#' # fviz_contrib(out3$raw.out, choice = "quali.var", axes = 2)
#'}
#'
contrib <- function(x, ndim = NULL,
                    plot.loadings = FALSE,
                    use.sign = TRUE,
                    sort.value = TRUE, ...) {
  UseMethod("contrib")
}

#' @name contrib
#' @method contrib default
#' @export
contrib.default <- function(x, ndim = NULL,
                            plot.loadings = FALSE,
                            use.sign = TRUE,
                            sort.value = TRUE, ...) {

  contrib.pcss.core(x, ndim = NULL,
                    plot.loadings = FALSE,
                    use.sign = TRUE,
                    sort.value = TRUE)
}

#' @name contrib
#' @export
contrib.pcss.core <- function(x, ndim = NULL,
                              plot.loadings = FALSE,
                              use.sign = TRUE,
                              sort.value = TRUE, ...) {
  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  if (!is.null(ndim)) {
    # check if 'ndim' argument is integer vector of unit length
    if (!(as.integer(ndim) == ndim && length(ndim) == 1)) {
      stop('"ndim" should be a integer vector of unit length.')
    }

    # check if at least one dimension are plotted in ndim
    if (ndim < 1L) {
      stop('At least 1 dimension is to be specified in "ndim".')
    }

    # check if ndim is not greater than total dimensions
    total.ndim <- ncol(x$raw.out$svd$V)

    if (ndim > total.ndim) {
      warning('"ndim" is greater than the total number of dimensions.\n',
              paste('Using the total number of dimensions (',
                    total.ndim, ') as "ndim".', sep = ""))
      ndim <- total.ndim
    }

  }

  method <- attr(x, "method")

  if (plot.loadings) {

    pdata <- x$raw.out$svd$V

    if (method != "FAMD") {

      rownames(pdata) <- rownames(x$raw.out$var$coord)
      colnames(pdata) <- colnames(x$raw.out$var$coord)

    } else {

      quali.levels <- attr(x, "quali.levels")
      quant <- attr(x, "quant")

      rownames(pdata) <- c(quant,
                           unlist(
                             lapply(seq_along(quali.levels), function(i) {
                               paste(names(quali.levels)[i], quali.levels[[i]],
                                     sep = "_")
                             })
                           ))
      colnames(pdata) <- gsub("Dim.", "Dim ", colnames(pdata))
    }


  } else {


    if (method != "FAMD") {

      pdata <- x$raw.out$var$contrib

      if (use.sign) {
        pdata <- pdata * sign(x$raw.out$var$coord)
      }

      rownames(pdata) <- rownames(x$raw.out$var$coord)
      colnames(pdata) <- colnames(x$raw.out$var$coord)

    } else {

      pdata <- rbind(x$raw.out$quanti.var$contrib,
                     x$raw.out$quali.var$contrib)

      if (use.sign) {
        pdata <- pdata *  rbind(sign(x$raw.out$quanti.var$coord),
                                sign(x$raw.out$quali.var$coord))
      }

      quali.levels <- attr(x, "quali.levels")
      quant <- attr(x, "quant")

      rownames(pdata) <-
        c(quant,
          unlist(
            lapply(seq_along(quali.levels), function(i) {
              paste(names(quali.levels)[i], quali.levels[[i]], sep = "_")
            })
          ))
      colnames(pdata) <- gsub("Dim.", "Dim ", colnames(pdata))

    }

  }

  pdata <- data.frame(pdata, check.rows = FALSE)

  eigdf <- x$eigen

  if (!is.null(ndim)) {
    if (nrow(eigdf) > ndim) {
      pdata <- pdata[, 1:ndim]
    }
  }

  pdata$Trait <- rownames(pdata)

  pdata_long <- tidyr::pivot_longer(data = pdata, cols = !Trait,
                                    names_to = "Dimension",
                                    values_to = "Value")
  pdata_long$Dimension <- as.factor(as.integer(gsub("\\D", "",
                                                    pdata_long$Dimension)))
  levels(pdata_long$Dimension) <- paste("Dim", levels(pdata_long$Dimension))

  pdata_long$Group <- ifelse(pdata_long$Value >= 0, TRUE, FALSE)

  pdata_long <- pdata_long[rev(order(pdata_long$Value)), ]
  pdata_long$Trait_Dim <- paste(pdata_long$Trait,
                                pdata_long$Dimension, sep = "#")
  pdata_long$Trait_Dim <- factor(pdata_long$Trait_Dim,
                                 levels = rev(paste(pdata_long$Trait,
                                                    pdata_long$Dimension,
                                                    sep = "#")))

  if (sort.value) {

    contribg <-
      ggplot(pdata_long, aes(y = Trait_Dim, x = Value, fill = Group)) +
      geom_bar(stat = "identity", show.legend = FALSE,
               colour = "transparent") +
      facet_wrap(vars(Dimension), scales = "free_y") +
      scale_y_discrete(labels = function(x) gsub("#.+$", "", x)) +
      ylab("Trait") +
      theme_bw()

  } else {

    contribg <-
      ggplot(pdata_long, aes(y = Trait, x = Value, fill = Group)) +
      geom_bar(stat = "identity", show.legend = FALSE,
               colour = "transparent") +
      facet_wrap(vars(Dimension), scales = "free_y") +
      theme_bw()
  }

  if (any(pdata_long$Value < 0)) {

    contribg <- contribg +
      scale_fill_manual(values = c("firebrick2", "steelblue3")) +
      geom_vline(xintercept = 0)


  } else {

    contribg <- contribg +
      scale_fill_manual(values = c("gray10", "gray10")) +
      geom_vline(xintercept = 0)

  }

  return(contribg)

}
