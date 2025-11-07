#' @title andurinha: Tools to make spectroscopic data processing easier
#'
#' @description This package contains a set of functions that makes
#'   spectroscopic data processing easier and faster. It is intended for
#'   processing several spectra (tens to hundreds) of samples with similar
#'   composition. It compiles spectroscopy data files, produces standardised and
#'   second derivative spectra, finds peaks and allows to select the most
#'   significant ones based on the second derivative/absorbance sum spectrum. It
#'   also provides functions for graphic evaluation of the outputs.
#'
#' @section andurinha functions:
#' \enumerate{
#'  \item \code{\link{importSpectra}}: in case you have your spectra in
#'  separated files (.csv) this function imports and bind them in a single data
#'  frame.
#'   \item \code{\link{findPeaks}}: finds peaks and allows to select the most
#'   relevant based on the second derivative sum spectrum.
#'   \item \code{\link{gOverview}}: generates a graphic overview of the
#'   spectroscopic data.
#'   \item \code{\link{plotPeaks}}: makes a graphic representation of the
#'   peaks over the second derivative/absorbance sum spectrum.
#' }
#'
#' @docType package
#' @name andurinha
#'
#' @importFrom stats aggregate sd
#' @importFrom utils read.csv
#' @importFrom signal sgolayfilt
#' @importFrom plyr compact
#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs theme element_blank
#'   element_line element_text margin scale_color_manual scale_color_grey
#'   scale_x_reverse scale_y_continuous
#' @importFrom cowplot plot_grid
#' @importFrom tidyr gather
#' @importFrom rlang abort .data
#'
NULL
