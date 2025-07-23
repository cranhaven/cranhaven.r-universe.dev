#' NBA data
#'
#' The data contains four descriptive statistics for all 95 guards for the 1992-1993
#' season. There are many ways to measure the (statistical) performance of guards in
#' the NBA. Of interest is how the the player's height (Height), minutes per game (MPG),
#' and free throw percentage (FTP) affect points per game (PPM).
#'
#' @usage NBA
#'
#' @format A data frame containing 95 observations and the following 4 variables.
#' \describe{
#'   \item{Height:}{ height of the player.}
#'   \item{MPG:}{ minutes per game.}
#'   \item{FTP:}{ free throw percentage.}
#'   \item{PPM:}{ points per game.}
#' }
#'
#' @references
#'   Chatterjee, S., Handcock, M. S., and Simonoff, J. S. (1995). A casebook for a
#'   first course in statistics and data analysis (No. 04; QA276. 12, C4.).
#'   New York: Wiley.
#'
#'   Xiang, S. and Yao, W. (2020). Semiparametric mixtures of regressions with
#'   single-index for model based clustering. Advances in Data Analysis and
#'   Classification, 14(2), 261-292.
#'
#' @keywords datasets
"NBA"


#' ROE data
#'
#' The data contains a total of 2110 Chinese listed companies on their Return on
#' Equity (ROE), which represents the amount of net income returned as a percentage of
#' shareholders' equity. ROE is an important index used to measure a corporation's
#' profitability and is also a useful indicator for fundamental analysts to assess
#' the value of stocks.
#'
#' @usage ROE
#'
#' @format A data frame containing 2110 observations.
#'
#' @references Huang, M., Wang, S., Wang, H., and Jin, T. (2018). Maximum smoothed
#' likelihood estimation for a class of semiparametric Pareto mixture densities.
#' Statistics and Its Interface, 11(1), 31-40.
#'
#' @keywords datasets
"ROE"


#' AFDP data
#'
#' The data contains the 11 sensor measures aggregated over one hour (by means of
#' average or sum) from a gas turbine located in Turkey's north western region for
#' the purpose of studying flue gas emissions.
#'
#' @usage AFDP
#'
#' @format A data frame containing 7411 observations.
#'
#' @keywords datasets
"AFDP"


#' Elbow data
#'
#' The data contains elbow dimension measurements on 507 individuals (247 men and
#' 260 women), primarily in their twenties and thirties, all of whom exercise several
#' hours a week.
#'
#' @usage elbow
#'
#' @format A data frame containing 507 observations (elbow diameter, sum of two elbows).
#'
#' @references Heinz, G., Peterson, L. J., Johnson, R. W., and Kerk, C. J. (2003).
#' Exploring relationships in body dimensions. Journal of Statistics Education, 11(2)
#'
#' @keywords datasets
"elbow"


#' Ethanol data
#'
#' The data contains 88 sets of measurements of peak nitrogen oxide emission levels from an experiment
#' in which ethanol was burned in a single-cylinder engine. The emission levels of nitrogen oxides were
#' recorded under different settings of the compression ratio and equivalence ratio of the engine.
#'
#' @usage ethanol
#'
#' @format A data frame containing 88 observations and the following 3 variables.
#' \describe{
#'   \item{NO:}{ concentration of nitric oxide (NO) and nitrogen dioxide (NO2) in engine exhaust.}
#'   \item{Compression:}{ compression ratio of engine.}
#'   \item{Equivalence:}{ equivalence ratio. This is a measure of the richness of the air and ethanol fuel mixture.}
#' }
#'
#' @source
#' Original source:
#'
#' Brinkman, N. D. (1981). Ethanol fuel—single—cylinder engine study of efficiency and exhaust emissions.
#' SAE transactions, 1410-1424.
#'
#' R source:
#'
#' Wand M (2018). SemiPar: Semiparametic Regression. R package version 1.0-4.2,
#' <https://CRAN.R-project.org/package=SemiPar>.
#'
#' Sarkar D (2008). Lattice: Multivariate Data Visualization with R. Springer, New York. ISBN
#' 978-0-387-75968-5, <http://lmdvr.r-forge.r-project.org>.
#'
#' @references Ruppert, D., Wand, M. P., and Carroll, R. J. (2003). Semiparametric regression (No. 12).
#' Cambridge university press.
#'
#' Hurn, M., Justel, A., and Robert, C. P. (2003). Estimating mixtures of regressions.
#' Journal of computational and graphical statistics, 12(1), 55-79.
#'
#' @keywords datasets
"ethanol"


#' Tone perception data
#'
#' The data originates from an experiment of Cohen (1980) and has been analyzed in de Veaux (1989) and
#' Viele and Tong (2002). In this experiment, a pure fundamental tone was played to a trained musician.
#' To create different auditory conditions, electronically generated overtones were introduced, which were
#' determined by a stretching ratio. When \code{stretchratio = 2.0}, it aligns with the harmonic pattern
#' usually heard in traditional definite pitched instruments. The musician was asked to tune an adjustable
#' tone to the octave above the fundamental tone. The variable \code{tuned} gives the ratio of the adjusted
#' tone to the fundamental tone. For example, \code{tuned = 2.0},  would be the correct tuning for all
#' \code{stretchratio} values. This dataset comprises data collected from 150 trials conducted with
#' the same musician. In the original study, data were gathered from an additional four musicians as well.
#' The dataset and the description have been sourced from the \code{tonedata} of the `fpc' package.
#'
#' @usage tone
#'
#' @format A data frame containing 150 observations and the following 2 variables.
#' \describe{
#'   \item{stretchratio:}{ electronically generated overtones added to a pure fundamental tone.}
#'   \item{tuned:}{ ratio of the adjusted tone to the fundamental tone.}
#' }
#'
#' @source
#' Original source:
#'
#' Cohen, E. (1980). Inharmonic tone perception. Unpublished Ph. D. Dissertation, Stanford University.
#'
#' R source:
#'
#' Hennig C (2023). fpc: Flexible Procedures for Clustering. R package version 2.2-10,
#' <https://CRAN.R-project.org/package=fpc>
#'
#' Benaglia, T., Chauveau, D., Hunter, D. R., and Young, D. S. (2010).
#' mixtools: an R package for analyzing mixture models. Journal of statistical software, 32, 1-29.
#'
#' @references De Veaux, R. D. (1989). Mixtures of linear regressions. Computational Statistics & Data Analysis,
#' 8(3), 227-245.
#'
#' Viele, K. and Tong, B. (2002). Modeling with mixtures of linear regressions. Statistics and Computing, 12, 315-330.
#'
#' @keywords datasets
"tone"
