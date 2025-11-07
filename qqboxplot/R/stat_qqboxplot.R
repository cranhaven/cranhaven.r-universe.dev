#' Compute values for the Q-Q Boxplot
#'
#' @inheritParams ggplot2::stat_boxplot
#' @param reference_dist Specifies theoretical reference distribution.
#' @param confidence_level Sets confidence level for deviation whisker
#' confidence bands
#' @param geom specifies the geom function to use
#' @param numboots specifies the number of bootstrap draws for bootstrapped CIs
#' needed only if compdata is not NULL
#' @param compdata specifies a data set to use as the reference distribution.
#' If compdata is not NULL, the argument reference_dist will be ignored.
#' @param qtype an integer between 1 and 9 indicating which one of the quantile algorithms to use.
#' @section Computed variables:
#' `stat_qqboxplot()` provides the following variables, some of which depend on the orientation:
#' \describe{
#'   \item{width}{width of boxplot}
#'   \item{ymin *or* xmin}{lower whisker = smallest observation greater than or equal to lower hinge - 1.5 * IQR}
#'   \item{lower *or* xlower}{lower hinge, 25% quantile}
#'   \item{notchlower}{lower edge of notch = median - 1.58 * IQR / sqrt(n)}
#'   \item{middle *or* xmiddle}{median, 50% quantile}
#'   \item{notchupper}{upper edge of notch = median + 1.58 * IQR / sqrt(n)}
#'   \item{upper *or* xupper}{upper hinge, 75% quantile}
#'   \item{ymax *or* xmax}{upper whisker = largest observation less than or equal to upper hinge + 1.5 * IQR}
#' }
#' @return Returns an object of class `StatQqboxplot`, (inherits from `Geom`, `ggproto`),
#'  that helps to render the data for `geom_qqboxplot()`.
#' @export
stat_qqboxplot <- function(mapping = NULL, data = NULL,
                           geom = "qqboxplot", position = "dodge2",
                           ...,
                           coef = 1.5,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           reference_dist = "norm",
                           confidence_level = .95,
                           numboots = 500,
                           qtype=7,
                           compdata = NULL) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQqboxplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      reference_dist = reference_dist,
      numboots = numboots,
      qtype=qtype,
      ...
    )
  )
}


StatQqboxplot <- ggplot2::ggproto("StatQqboxplot", ggplot2::Stat,
                         required_aes = c("y"),
                         non_missing_aes = "weight",
                         setup_data = function(data, params) {
                           data$x <- data$x %||% 0
                           data <- ggplot2::remove_missing(
                             data,
                             na.rm = FALSE,
                             vars = "x",
                             name = "stat_qqboxplot"
                           )
                           data
                         },

                         setup_params = function(data, params) {
                           params$width <- params$width %||% (ggplot2::resolution(data$x %||% 0) * 0.75)

                           if (is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
                             warning(
                               "Continuous x aesthetic -- did you forget aes(group=...)?",
                               call. = FALSE)
                           }

                           params
                         },

                         compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5, reference_dist='norm',
                                                  compdata=NULL, confidence_level=.95, numboots = 500, qtype = 7) {
                           qs <- c(0, 0.25, 0.5, 0.75, 1)

                           conf <- .95

                           estimate_B <- function(data, qtype=qtype)
                           {
                             quants <- c(.005, .01, .025, .05, .1, .25)

                             vals_p <- quantile(data, quants, type=qtype)
                             vals_neg_p <- quantile(data, 1-quants, type=qtype)

                             med_p <- median(data)

                             z_vals <- qnorm(quants)

                             g_p <- -(1/z_vals)*log((vals_neg_p - med_p)/(med_p - vals_p))

                             g <- median(g_p)

                             log_uhs <- log(g*(vals_neg_p - med_p)/(exp(-g*z_vals)-1))

                             regs <- z_vals^2/2

                             my_lm <- lm(y~x, data=data.frame(x=regs, y=log_uhs))

                             exp(coef(my_lm)[1])
                           }


                           if (!is.null(data$weight)) {
                             mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                             stats <- as.numeric(stats::coef(mod))
                           } else {
                             stats <- as.numeric(stats::quantile(data$y, qs, type=qtype))
                           }
                           names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                           iqr <- diff(stats[c(2, 4)])

                           outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + coef * iqr)
                           if (any(outliers)) {
                             stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
                           }

                           if (length(unique(data$x)) > 1)
                             width <- diff(range(data$x)) * 0.9

                           df <- as.data.frame(as.list(stats))
                           df$outliers <- list(data$y[outliers])

                           if (is.null(data$weight)) {
                             n <- sum(!is.na(data$y))
                           } else {
                             # Sum up weights for non-NA positions of y and weight
                             n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                           }

                           df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                           df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)



                           df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                           df$width <- width

                           ##################################
                           ##################################
                           ######## add here ################
                           ##################################

                           calculate_quantile_distance <- function(samp, comparison_samp, isboot=FALSE, qtype=qtype){
                             len_sample <- length(samp)
                             len_compsample <- length(comparison_samp)
                             quants <- seq(from=0, to=1, length.out=len_sample)
                             if(isboot){
                               samp <- sample(comparison_samp, len_compsample, replace=TRUE)
                               samp <- quantile(samp, probs=quants, type=qtype)

                             }
                             samplesort <- sort(samp)
                             compsample <- sort(comparison_samp)


                             if(FALSE){
                               print('here')
                             }
                             compsample <- sort(quantile(comparison_samp, probs=quants, type=qtype))


                             line.p = c(.25, .75)

                             quant_diff_sample <- diff(quantile(samplesort, line.p, type=qtype))
                             quant_diff_compsample <- diff(quantile(compsample, line.p, type=qtype))

                             tmp=(samplesort - median(samplesort))/quant_diff_sample - (compsample - median(compsample))/quant_diff_compsample

                             list(dev=tmp, y=samplesort)

                           }

                           getbootconf <- function(samp, comparison_samp, numboot, conf){
                             bootsamp <- replicate(numboot, calculate_quantile_distance(samp, comparison_samp, isboot = TRUE, qtype=qtype)$dev)
                             alphdivtwo <- (1-conf)/2
                             t(apply(bootsamp, 1, quantile, c(alphdivtwo, conf +alphdivtwo), type=qtype))
                           }

                           if(!is.null(compdata)){


                             data.y.stdze <- (data$y-median(data$y))/estimate_B(data$y, qtype=qtype)

                             compdata.stdze <- (compdata-median(compdata))/estimate_B(compdata, qtype=qtype)


                             devlist <- calculate_quantile_distance(data.y.stdze, compdata.stdze, qtype=qtype)

                             deviat <- devlist$dev



                             confs <- getbootconf(data.y.stdze, compdata.stdze, numboots, conf) #set as parameter

                             upper <- confs[,2]

                             lower <- confs[,1]

                             sampleorder <- order(data$y)

                             sample <- sort(data$y)



                           }else{

                             qdist <- eval(paste0('q', reference_dist))
                             ddist <- eval(paste0('d', reference_dist))
                             conf <- .95

                             sample <- sort(data$y)
                             sampleorder <- order(data$y)
                             n <- length(sample)

                             distribution=reference_dist


                             ########################################################################################
                             ########################################################################################
                             ###################### Following from qqplotr code #####################################
                             ########################################################################################
                             ########################################################################################

                             # equivalence between base R and MASS::fitdistr distribution names
                             corresp <- function(distName) {
                               switch(
                                 distName,
                                 beta = "beta",
                                 cauchy = "cauchy",
                                 chisq = "chi-squared",
                                 exp = "exponential",
                                 f = "f",
                                 gamma = "gamma",
                                 geom = "geometric",
                                 lnorm = "log-normal",
                                 logis = "logistic",
                                 norm = "normal",
                                 nbinom = "negative binomial",
                                 pois = "poisson",
                                 t = "t",
                                 weibull = "weibull",
                                 NULL
                               )
                             }

                             # initial value for some distributions
                             initVal <- function(distName) {
                               switch(
                                 distName,
                                 beta = list(shape1 = 1, shape2 = 1),
                                 chisq = list(df = 1),
                                 f = list(df1 = 1, df2 = 2),
                                 t = list(m=1, s = 1, df=1),
                                 NULL
                               )
                             }

                             suppressWarnings({
                               if(!is.null(corresp(distribution))) {
                                 if(is.null(initVal(distribution))) {
                                   dparams <- MASS::fitdistr(x = sample, densfun = corresp(distribution))$estimate
                                   if(distribution=="norm"){dparams <- list(mean=0, sd=1)}
                                 } else {
                                   dparams <- MASS::fitdistr(x = sample, densfun = corresp(distribution), start = initVal(distribution))$estimate
                                 }
                               }
                             })



                             ########################################################################################
                             ########################################################################################
                             ###################### End from qqplotr code #####################################
                             ########################################################################################
                             ########################################################################################





                             sample.stdz <- (sample-median(sample))/estimate_B(sample, qtype=qtype)

                             quantiles <- ppoints(n)


                             theoretical <- do.call(qdist, c(list(p = quote(quantiles)), dparams))

                             theoretical <- theoretical - median(theoretical)


                             line.p = c(.25, .75)


                             stdErr <- (1 / do.call(ddist, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)
                             zCrit <- qnorm(p = (1 - (1 - conf) / 2))

                             upper <- (stdErr * zCrit)
                             lower <- - (stdErr * zCrit)


                             deviat <- sample.stdz - theoretical




                           }


                           upperupper <- upper[sample>=stats[4] & !outliers[sampleorder]]

                           upperlower <- lower[sample>=stats[4] & !outliers[sampleorder]]
                           uppery <- sample[sample>=stats[4] & !outliers[sampleorder]]


                           lowerupper <- upper[sample<=stats[2] & !outliers[sampleorder]]
                           lowerlower <- lower[sample<=stats[2] & !outliers[sampleorder]]
                           lowery <- sample[sample<=stats[2] & !outliers[sampleorder]]




                           deviatupper <- deviat[sample>=stats[4] & !outliers[sampleorder]]

                           deviatlower <- deviat[sample<=stats[2] & !outliers[sampleorder]]

                           normalizing_constant <- 1.9*(max(c(deviatupper, deviatlower, upperupper, lowerupper))-min(c(deviatupper, deviatlower, upperlower, lowerlower)))/(df$width)


                           df$upperupper <- list(upperupper/normalizing_constant)
                           df$upperlower <- list(upperlower/normalizing_constant)
                           df$lowerupper <- list(lowerupper/normalizing_constant)
                           df$lowerlower <- list(lowerlower/normalizing_constant)
                           df$uppery <- list(uppery)
                           df$lowery <- list(lowery)
                           df$deviatupper <- list(deviatupper/normalizing_constant)
                           df$deviatlower <- list(deviatlower/normalizing_constant)



                           ##################################
                           df$relvarwidth <- sqrt(n)
                           df
                         }
)
