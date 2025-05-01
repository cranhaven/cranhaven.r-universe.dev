#' Companion Animal Population Management. Provides functions for quantitative Companion Animal Population Management. Further information can be found in the URL given below.
#'
#' \tabular{ll}{
#' Package: \tab capm\cr
#' Type: \tab Package\cr
#' Version: \tab 0.14.0\cr
#' Date: \tab 2019-10-24\cr
#' Depends: \tab R (>= 3.4)\cr
#' Imports: \tab deSolve, FME, survey, dplyr, tidyr, magrittr, ggplot2, grid, stats, utils, sf\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' URL: \tab \url{http://oswaldosantos.github.io/capm}\cr
#' Author: \tab Oswaldo Santos Baquero \email{baquero@@usp.br}\cr
#' Maintainer: \tab Oswaldo Santos Baquero \email{baquero@@usp.br}\cr
#' Contributors: \tab Marcos Amaku \email{amaku@@vps.fmvz.usp.br}, Fernando Ferreira \email{fernando@@vps.fmvz.usp.br}
#' }
#'
#' @name capm-package
#' @docType package
#' @title The capm Package
#' @keywords package
#' @import ggplot2
#' @importFrom survey svydesign calibrate svytotal svymean SE deff cv
#' @importFrom deSolve ode
#' @importFrom FME sensFun sensRange
#' @importFrom dplyr filter group_by group_by_ summarise arrange ungroup mutate left_join select select_ transmute n desc
#' @importFrom tidyr gather
#' @importFrom magrittr "%>%"
#' @importFrom grid viewport pushViewport grid.newpage grid.layout
#' @importFrom stats aggregate confint qnorm runif sd var na.omit relevel
#' @importFrom circlize circos.clear circos.par chordDiagram circos.trackPlotRegion get.cell.meta.data get.cell.meta.data circos.text circos.axis
#' @importFrom utils write.table
#' @importFrom sf st_read st_write st_transform
NULL
