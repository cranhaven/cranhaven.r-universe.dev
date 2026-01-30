#' Phenological parameters estimation
#' 
#' Estimation of 6 phenological parameters from a numeric vector. The estimated parameters are: \bold{green up},
#' \bold{start of season}, \bold{maturity}, \bold{senescence}, \bold{end of season} and \bold{dormancy}. These parameters
#' are critical points of some derivatives of an idealized curve which, in turn, is obtained 
#' through a functional principal component analysis (FPCA)-based regression model.
#' 
#' @param          x a numeric vector.
#' @param  startYear integer, time series initial year
#' @param    endYear integer, time series final year
#' @param  frequency integer giving number of observations per season. Default, 23.
#' @param     method character. Should \code{OLS} or \code{WLS} be used for smoothing \code{x}
#'                   through a harmonic regression model. See \bold{Details}.
#' @param      sigma numeric vector of length equal to \code{frequency}. Each entry gives the 
#'                   standard deviation of observations acquired at same day of the year.
#'                   Pertinent when \code{method=WLS} only.
#' @param    numFreq integer specifying number of frequencies used in harmonic regression
#'                   model.
#' @param      delta numeric. Default, 0. When harmonic regression problem is ill-posed, this 
#'                   parameter allows a simple regularization. See \bold{Details}.
#' @param   distance character indicating what distance to use in hierarchical clustering.
#'                   All distances in \code{\link[dtwclust]{tsclust}} are allowed. See 
#'                   \bold{Details}.
#' @param    samples integer with number of samples to draw from smoothed version of \code{x}. Used
#'                   exclusively in Functional Principal Components Analysis (FPCA)-based
#'                   regression. See \bold{Details}.
#' @param      basis list giving numeric basis used in FPCA-based regression. See \bold{Details}.
#' @param       corr Default \code{NULL}. Object defining correlation structure, can be 
#'                   numeric vector, matrix or function.
#' @param          k integer, number of principal components used in FPCA-based regression.   
#' @param      trace logical. If \code{TRUE}, progress on the hierarchical clustering
#'                   is printed on console. Default, \code{FALSE}.
#' 
#' @export
#' 
#' @importFrom dtwclust tsclust
#' @importFrom geoTS haRmonics
#' @importFrom eBsc drbasis
#' 
#' @details In order to estimate the phenological parameters, first \code{x} is assembled 
#' as a matrix. This matrix has as many rows as years (\code{length(startYear:endYear)}) in the studied period and as many columns
#' as observations (\code{frequency}) per year. Then, each vector row is smoothed through 
#' the harmonic regression model \code{\link[geoTS]{haRmonics}}. This function allows 
#' for homogeneous (\code{OLS}) and heterogeneous (\code{WLS}) errors in the model. When
#' \code{method=WLS}, \code{sigma} must be provided, \code{\link[geoTS]{hetervar}} is recommended for such a purpose.
#' Additional parameters for \code{\link[geoTS]{haRmonics}} are \code{numFreq} and
#' \code{delta}.
#' 
#' Next, equally spaced \code{samples} are drawn from each harmonic regression fit, 
#' the resulting observations are stored in the matrix \code{m_aug_smooth}. 
#' \code{\link[dtwclust]{tsclust}} 
#' is applied to \code{m_aug_smooth} in order to obtain clusters of years sharing 
#' similar characteristics; 2 clusters are produced. The next step is applied
#' to the dominating cluster (the one with the majority of years, >=10), or to the 
#' whole of columns of \code{m_aug_smooth} when no dominating cluster can be determined.
#' 
#' Based on the observations produced in the hierarchical clustering step, a regression 
#' model with the following representation is applied:
#' 
#' \eqn{f_i(t) = \tau(t) + \sum_{j=1}^{k} \varepsilon_j(t) \nu_{ij} + \epsilon_i},
#' 
#' where \eqn{f_i(t)} is substituted by the vector of sample observations of the \eqn{i}-th 
#' year; \eqn{\varepsilon_j(t)} is the \eqn{j}-th functional principal component (FPC); 
#' \eqn{\nu_{ij}} is the score associated with the \eqn{j}-th FPC and the \eqn{i}-th vector
#' of sampled observations; and \eqn{\epsilon_i} is a normally distributed random variable
#' with variance \eqn{\sigma^2}, see \cite{Krivobokova et al. (2022)} for further details. 
#' From this step, an estimate of \eqn{\tau} is produced -\code{fpca}- 
#' this is an idealized version of the original observations contained in \code{x}.
#' 
#' Parameter \code{basis} can be supplied through a call to \code{\link[eBsc]{drbasis}}
#' with parameters \code{nn=samples} and \code{qq=2}. Parameter \code{corr} indicates
#' whether correlation between annual curves must be considered; the current implementation
#' does not incorporate correlation. The number of principal components is controlled
#' by \code{k}. 
#' 
#' Next, a harmonic regression is fitted to \code{fpca} (a numeric vector of length 
#' equal to \code{samples}) with the parameters provided above (\code{method}, \code{sigma}, 
#' \code{numFreq}, \code{delta}). Based on the estimated parameters of this fit 
#' (\code{fpca_harmfit_params}) a R function is calculated along with its first, 
#' second, third and fourth derivatives. These derivatives are used in establishing 
#' the phenological parameters (\code{phenoparams}) utilizing basic calculus
#' criteria similar to what \cite{Baumann et al. (2017)} have proposed.
#' 
#' Finally, when 6 \code{phenoparams} are found \code{status=Success}, otherwise 
#' \code{status=Partial}. 
#' 
#' @examples 
#' # --- Load dataset for testing
#' data("deciduous_polygon")
#' 
#' # --- Extracting first pixel of deciduous_polygon
#' pixel_deciduous <- vecFromData(data=deciduous_polygon, numRow=3)
#' 
#' # --- Following objects are used in this example
#' # --- for CRAN testing purposes only. In real life examples
#' # --- there is no need to shorten time series length
#' 
#' EndYear <- 2010
#' number_observations <- 23*11
#' 
#' # --- needed parameter
#' BASIS <- drbasis(n=50, q=2) 
#' 
#' # --- testing phenopar
#' sephora_deciduous <- phenopar(x=pixel_deciduous$vec[1:number_observations],
#'                               startYear=2000, endYear=EndYear,
#'                               numFreq=3, distance="dtw2",
#'                               samples=50, basis=BASIS, k=3)
#'                               
#' # --- testing ndvi_derivatives
#' f <- ndvi_derivatives(amp = sephora_deciduous$fpca_harmfit_params$amplitude,
#'                       pha = sephora_deciduous$fpca_harmfit_params$phase,
#'                       degree = 0, L = 365)
#' fprime <- ndvi_derivatives(amp = sephora_deciduous$fpca_harmfit_params$amplitude,
#'                            pha = sephora_deciduous$fpca_harmfit_params$phase,
#'                            degree = 1, L = 365)
#' fbiprime <- ndvi_derivatives(amp = sephora_deciduous$fpca_harmfit_params$amplitude,
#'                              pha = sephora_deciduous$fpca_harmfit_params$phase,
#'                              degree = 2, L = 365)
#' f3prime <- ndvi_derivatives(amp = sephora_deciduous$fpca_harmfit_params$amplitude,
#'                             pha = sephora_deciduous$fpca_harmfit_params$phase,
#'                             degree = 3, L = 365)
#' f4prime <- ndvi_derivatives(amp = sephora_deciduous$fpca_harmfit_params$amplitude,
#'                             pha = sephora_deciduous$fpca_harmfit_params$phase,
#'                             degree = 4, L = 365)
#'                             
#' # --- testing global_min_max and local_min_max
#' intervalo <- seq(1,365, length=365)
#' GU_Mat <- global_min_max(f=fbiprime, f1der=f3prime, f2der=f4prime, D=intervalo)
#' Sen <- local_min_max(f=fbiprime, f1der=f3prime, f2der=f4prime, 
#'                      what="min", x0=GU_Mat$min, D=intervalo)
#' SoS_EoS <- global_min_max(f=fprime, f1der=fbiprime, f2der=f3prime, D=intervalo)
#' Dor <- local_min_max(f=fbiprime, f1der=f3prime, f2der=f4prime, 
#'                      what="max", x0=GU_Mat$max, D=intervalo)
#'                       
#' # --- phenological dates (rough estimates)
#' c(GU=GU_Mat$max, SoS=SoS_EoS$max, Mat=GU_Mat$min,
#'   Sen=Sen$x_opt, EoS=SoS_EoS$min, Dor=Dor$x_opt)
#' # --- phenological dates provided by sephora
#' sephora_deciduous$phenoparams
#' 
#' # --- testing plotting methods
#' plot(x=sephora_deciduous, yLab="NDVI (no rescaled)")
#' plot(x=sephora_deciduous, type="profiles", 
#'      xLab="DoY", yLab="NDVI (no rescaled)")
#'      
#' # --- 2015 forms Cluster 2
#' plot(x=sephora_deciduous, type="ms")      
#' 
#' # --- graphical definition of phenological dates
#' plot(x=sephora_deciduous, type="derivatives")
#' 
#' # --- Overlapping FPCA fit to original time series
#' gg <- plot(x=sephora_deciduous, type="profiles", 
#'            xLab="DoY", yLab="NDVI (no rescaled)")
#' x_axis <- get_metadata_years(x=pixel_deciduous$vec, 
#'                              startYear=2000, endYear=EndYear, frequency=23)  
#' DoY <- seq(1,365, by=16)
#' fpca_DoY <- sephora_deciduous$fpca_fun_0der(t=DoY)
#' COLORS <- unique( ggplot_build(gg)$data[1][[1]]$colour )
#' df <- data.frame(values=c(sephora_deciduous$x, fpca_DoY),
#'                  years=as.factor(rep(c(x_axis$xLabels,"FPCA"), each=23)),
#'                  DoY=factor(DoY, levels=DoY), class=c(rep(1,number_observations), rep(2,23))) 
#' gg_fpca <- ggplot(data=df, 
#'                   aes(x=DoY, y=values, group=years, colour=years)) +
#' ggplot2::geom_line(linewidth = c(rep(1,number_observations), rep(4,23))) + 
#' ggplot2::labs(y="NDVI", x="DoY", color="years+FPCA") + 
#' ggplot2::scale_color_manual(values = c(COLORS, "#FF4500")) +
#' ggplot2::theme(legend.position = "right")
#' gg_fpca
#' 
#' @return A \code{\link[sephora]{sephora-class}} object containing 14 elements
#' \item{x}{numeric vector}
#' \item{startYear}{integer, time series initial year}
#' \item{endYear}{integer, time series final year}
#' \item{freq}{numeric giving number of observations per season. Default is 23.} 
#' \item{sigma}{when \code{method="OLS"}, numeric of length one (standard deviation); 
#' when \code{method="WLS"}, numeric vector of length equal to \code{freq}}
#' \item{m_aug_smooth}{matrix with \code{nrow=samples} and \code{ncol=(length(x)/freq)} 
#' containing sampled observations}
#' \item{clustering}{Formal class \code{HierarchicalTSClusters} with 20 slots. Output from a call 
#' to \code{\link[dtwclust]{tsclust}} with parameters \code{series=m_aug_smooth},
#' \code{type='h'}, \code{distance=distance}}
#' \item{fpca}{numeric vector of length equal to \code{samples}}
#' \item{fpca_harmfit_params}{list of 4: \code{a.coef}, \code{b.coef}, \code{amplitude} 
#' and \code{phase} as in \code{\link[geoTS]{haRmonics}} output.}
#' \item{fpca_fun_0der}{function, harmonic fit for \code{x}}
#' \item{fpca_fun_1der}{function, first derivative of harmonic fit for \code{x}}
#' \item{fpca_fun_2der}{function, second derivative of harmonic fit for \code{x}}
#' \item{fpca_fun_3der}{function, third derivative of harmonic fit for \code{x}}
#' \item{fpca_fun_4der}{function, fourth derivative of harmonic fit for \code{x}}
#' \item{phenoparams}{named numeric vector of length 6}
#' \item{status}{character, specifying whether FPCA model was inverted successfully 
#' (\code{Success}) or partially ("Partial"). In other words, \code{Success} and 
#' \code{Partial} mean that 6 or less than 6 parameters were estimated, respectively.}
#' 
#' @seealso \code{\link[geoTS]{haRmonics}}, \code{\link[geoTS]{hetervar}}, 
#' \code{\link[dtwclust]{tsclust}}, \code{\link[eBsc]{drbasis}}.
#' 
#' @references Krivobokova, T. and Serra, P. and Rosales, F. and Klockmann, K. (2022).
#' \emph{Joint non-parametric estimation of mean and auto-covariances for Gaussian processes}. 
#' Computational Statistics & Data Analysis, \bold{173}, 107519.
#' 
#' @references Baumann, M. and Ozdogan, M. and Richardson, A. and Radeloff, V. (2017).
#' \emph{Phenology from Landsat when data is scarce: Using MODIS and Dynamic Time-Warping to combine 
#' multi-year Landsat imagery to derive annual phenology curves}. International Journal of Applied Earth Observation and Geoinformation,
#' \bold{54}, 72--83 
#' 
phenopar <- function(x, startYear, endYear, frequency=23, method=c("OLS", "WLS"), sigma=NULL,
                     numFreq, delta=0, distance, samples, basis, corr=NULL, k,
                     trace=FALSE){
  
  if(missing(startYear)){
    stop("startYear must be provided")
  }
  
  if(missing(endYear)){
    stop("endYear must be provided")
  }
  
  if(missing(basis)){
    stop("basis must be provided")
  }

  if(missing(numFreq)){
    stop("numFreq must be provided")
  }
  
  method <- match.arg(method)
  
  if(method=="WLS"){
    if(is.null(sigma)){
      stop("sigma must be provided when WLS method is used")
    }
  }
  
  output <- get_phenopar_tryCatch(x=x, frequency=frequency, method=method,
                                  sigma=sigma, numFreq=numFreq, delta=delta, 
                                  distance=distance, samples=samples, basis=basis, 
                                  corr=corr, k=k, trace=trace)
  
  # output <- list()
  
  # output$x <- temporal$x
    
  # output$startYear <- startYear
  # 
  # output$endYear <- endYear
  
  out_sephora <- toSephoraClass(x=output, startYear=startYear,
                                endYear=endYear)
  
  sephora.out <- structure(out_sephora, class="sephora")
  
  sephora.out
}

# #' Plot a phenopar object
# #'
# #' This function draws a \code{\link[ggplot2]{ggplot}} object. Argument \code{type}
# #' controls whether a time series or a set of profile curves is plotted.
# #'
# #' @param               x a numeric vector or a \code{phenopar} object
# #' @param       startYear integer, time series initial year
# #' @param         endYear integer, time series final year
# #' @param       frequency integer giving number of observations per season.
# #'                        Default is \code{NULL}.
# #' @param            type character specifying type of plot: "ts" or "rep.mea.".
# #'                        See \code{Details.}
# #' @param        sizeLine integer giving line size
# #' @param       sizePoint integer giving point size
# #' @param position_legend character. Should a legend be added? Where? See
# #'                        \code{\link[ggplot2]{theme}}
# #' @param            xLab character, label to display in x-axis
# #' @param            yLab character, label to display in y-axis
# #' @param            xLim date vector of length 2 indicating limits of x-axis. When no
# #'                        supplied, \code{x} will be displayed in the full period
# #'                        of time spanned by \code{startYear}, \code{endYear} and
# #'                        \code{frequency}.
# #' @param             ... additional \code{ggplot} parameters.
# #'
# #' @rdname plot.phenopar
# #' @method plot phenopar
# #' @export
# #'
# #' @importFrom ggplot2 ggplot
# #' 
# #' @details This is pending
# #'
# #' @return A \code{\link[ggplot2]{ggplot}}
# #' 
# #' @seealso \code{\link[ggplot2]{ggplot}}
# #' 
# plot.phenopar <- function(x, startYear, endYear, frequency=NULL,
#                              type=c("ts", "rep.mea."),
#                              sizeLine=1, sizePoint=2, position_legend="none",
#                              # title_legend=NULL,
#                              xLab="Time (years)", yLab="NDVI",
#                              # width_breaks, date_label,
#                              xLim, ...){
#   if( inherits(x, "phenopar") ){
#     toPlot <- x$x
#     freq <- x$freq
#   } else {
#     if( is.null(frequency) ){
#       stop("frequency must be provided when x is not of class 'phenopar'")
#     } else {
#       toPlot <- x
#       freq <- frequency
#     }
#   }
# 
#   type <- match.arg(type)
# 
#   if( type == "ts" ){
#     if( missing(startYear) | missing(endYear) ){
#       stop( "startYear and endYear must be provided" )
#     } else {
#       out <- tsPlot(x=toPlot, startYear=startYear, endYear=endYear,
#                     frequency=freq,
#                     sizeLine=sizeLine, sizePoint=sizePoint,
#                     position_legend=position_legend,
#                     # title_legend=title_legend,
#                     xLab=xLab, yLab=yLab,
#                     # width_breaks=width_breaks, date_label=date_label,
#                     xLim=xLim)
#     }
#   }
# 
#   if( type == "rep.mea." ){
#     if( missing(startYear) | missing(endYear) ){
#       stop( "startYear and endYear must be provided" )
#     } else {
#       out <- rmPlot(toPlot, startYear=startYear, endYear=endYear,
#                     frequency=freq,
#                     sizeLine=sizeLine, sizePoint=sizePoint,
#                     position_legend=position_legend,
#                     # title_legend=title_legend,
#                     xLab=xLab, yLab=yLab, xLim=xLim)
#     }
#   }
# 
# out
# }


