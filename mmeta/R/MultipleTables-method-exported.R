
#' @useDynLib mmeta
#' @title Create an object of class \code{MultipleTables}.
#' @description Create an object of class \code{MultipleTables}, which is
#' a components list of exact posterior inference based on multiple 2x2 tables.
#' @param data a data frame that contains \code{y1}, \code{n1}, \code{y2}, \code{n2} of multiple tables.
#' @param measure a character string specifying a measure. Options are
#' \code{OR}, \code{RR}, and \code{RD}. \code{OR} is odds
#' ratio, \code{RR} is relative risk, and \code{RD} is risk difference.
#' @param model a character string specifying the model. Options are
#' \code{Independent} and \code{Sarmanov}. \code{Independent} is
#' independent beta-binomial model. \code{Sarmanov}is Sarmanov beta-binomial model.
#' @returns An object is returned, inheriting from class \code{MultipleTables}.
#' Objects of this class contain the meta-data for generic functions: \code{MultipleTables.modelFit},
#'  \code{MultipleTables.summary}, and \code{MultipleTables.plot}.
#'  The following valuesof the object must be non-null under \code{MultipleTables.create}.
#'  \item{measure}{the value of \code{measure} argument.}
#'  \item{model}{the value of \code{model} argument.}
#'  \item{data}{a data matrix with rows being \code{y1}, \code{n1}, \code{y2}, and \code{n2}.}
#' @examples
#' \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
#'  data(colorectal)
#'  colorectal['study_name'] <- colorectal['studynames']
#'  multiple_tables_obj <- MultipleTables.create(data=colorectal, measure='OR', model= 'Sarmanov')
#'  }
#' @seealso \code{MultipleTables.modelFit}, \code{MultipleTables.summary}, and \code{MultipleTables.plot}.
#' @export
MultipleTables.create<- function(data=NULL,
                                 measure=NULL,
                                 model= NULL){
  ## sanity check
  checkModelArgument(model)
  checkMeasureArgument(measure)
  checkMultipleTablesData(data)

  ### create object
  multiple_tables_object <- list(
              data = data,
              measure=measure,
              model=model,
              alpha=NULL,
              chi2_value = NULL,
              p_value = NULL,
              prior_mle = NULL,
              hessian_log = NULL,
              cov_matrix_log = NULL,
              overall_measure_estimation = NULL,
              specific_summary = NULL,
              samples= NULL,
              density= NULL
  )

  attr(multiple_tables_object, "class") <- "MultipleTables"
  invisible(multiple_tables_object)

}


#' @useDynLib mmeta
#' @title Exact posterior inference based on multiple 2x2 tables.
#' @description This function conducts exact posterior inference based on the object created by \code{MultipleTables.create}.
#' @param multiple_tables_object The object created by \code{MultipleTables.create}.
#' @param method a character string specifying the method. Options are \code{exact}
#' and \code{sampling}. \code{exact} (default) is a method based on Monte Carlo sampling. \code{exact} is exact method.
#' @param verbose a logical value; if TRUE, the detailed summary messages are displayed, else FALSE (default) the messages are omitted.
#' @param control a list can be specified to control the fitting process. Options are stated in details.
#' @details control list can be specified to control the fitting process:
#' \itemize{
#' \item \code{n_samples}:  number of posterior samples; Defualt is 5000.
#' \item \code{mcmc_initial}:  initial values for (p1, p2) in MCMC; Default is c(0.5, 0.5).
#' \item \code{upper_bound}:  upper bound for the measure. Default is 100.
#' \item \code{lower_bound}: lower bound for the measure. For RD, default is -1. For RR/OR, defualt is 0.
#' \item \code{num_grids}:  number of grids to calculate density; The defualt is 20498.
#' \item \code{optim_method}: optimazation method. Default is “L-BFGS-B”. Please refer to ‘optim’ function.
#' \item \code{maxit}: maximum number of iterations for iteration. Default is 1000. Please refer to ‘optim’ function.
#' \item \code{initial_values}: initial value for optimization.
#' The default approach is to fit beta-bin model to generate initial values via \code{aod} package.
#' }
#'
#'  There are two kinds of study design, i.e., prospective study or
#'  clinical trial, and retrospective or case-control study. In a
#'  prospective study or clinical trial, \code{data} is a data frame that contains \code{y1}, \code{n1}, \code{y2}, \code{n2},
#'  \code{studynames}. \code{y1} is the number of subjects
#'  experienced a certain event in the unexposed group. \code{n1} is the number
#'  of subjects in the unexposed group. \code{y2} is the number of subjects experienced
#'  a certain event in the exposed group. \code{n2} is the number of
#'  subjects in the exposed group. In this study, \code{OR} is odds ratio
#'  of event comparing exposed group with unexposed group. \code{RR}
#'  is relative risk of event comparing exposed group with unexposed group. \code{RD} is risk
#'  difference of event comparing exposed group with unexposed group.
#'
#'  For case-control study, \code{y1} is the number of subjects with
#'  exposure in the control group. \code{n1} is the number of
#'  subjects in the control group. \code{y2} is the number of
#'  subjects with exposure in the case group. \code{n2} is the
#'  number of subjects in the case group. In this study, \code{OR} is odds ratio
#'  of event comparing case group with control group. \code{RR} is
#'  relative risk of event comparing case group with control group. \code{RD} is risk
#'  difference of event comparing case group with control group.
#'
#'  Empirical Bayes method is used to maximize the marginal likelihood
#'  combining all studies to obtained the estimates of the
#'  hyperparameters a1, b1, a2, b2, and rho. When
#'  \code{method="independent"}, only the estimated hyperparameters
#'  of a1, b1, a2, and b2 are used. When \code{model="Sarmanov"},
#'  \code{rho} is subject to constraints. See Chen et al (2011) for
#'  details.
#'
#'  The output \code{cov.matrix} and \code{hessian} are the estimated
#'  covariance matrix and hessian matrix of the estimated
#'  parameters in the transformed scales. The estimated parameters
#'  are log(a1), log(b1), log(a2), log(b2), omega, where the
#'  correlation coefficient rho is a function of a1, b1, a2, b2, and
#'  omega. Please see details on page 7 of Chen et al (2012 b).
#' @returns An object inheriting from class \code{MultipleTables} is returned. Objects of this class including the following non-null values:
#' \item{measure}{the value of \code{measure} argument.}
#' \item{model}{the value of \code{model} argument.}
#' \item{data}{a data matrix with rows being \code{y1}, \code{n1}, \code{y2}, and \code{n2}.}
#' \item{method}{the value of \code{method} argument.}
#' \item{study_names}{a character string indicating all the study names.}
#' \item{chi2_value}{the chi-square test statistics of the likelihood ratio test.}
#' \item{p_value}{the p-value of the likelihood ratio test.}
#' \item{prior_mle}{a numeric vector of the estimated hyperparameters in the
#'   following order: \code{a1}, \code{b1}, \code{a2}, \code{b2}, \code{rho}.}
#' \item{cov_matrix_log}{the estimated covariance matrix of the estimated parameters in the transformed scales.}
#' \item{hessian_log}{the estimated hessian matrix of the estimated parameters in the transformed scales.}
#' \item{samples}{a list of samples for the posterior and prior distributions.}
#' \item{density}{a list of the density of the posterior and prior distributions.}
#' These values are essential for generic functions: \code{MultipleTables.summary} and \code{MultipleTables.plot}.
#'
#' @references  Luo, S., Chen, Y., Su, X., Chu, H., (2014). mmeta: An R Package for
#'  Multivariate Meta-Analysis. \cr
#'  \emph{Journal of Statistical Software}, 56(11), 1-26. \cr
#'  <https://dukespace.lib.duke.edu/dspace/bitstream/handle/10161/15522/2014Luo_Chen_Su_Chu_JSS_mmeta.pdf?sequence=1> \cr
#'  Chen, Y., Luo, S., (2011a). A Few Remarks on "Statistical Distribution of the Difference of
#' Two Proportions' by Nadarajah and Kotz, Statistics in Medicine 2007; 26(18):3518-3523". \cr
#' \emph{Statistics in Medicine, 30(15)}, 1913-1915. \cr
#' <doi:10.1002/sim.4248> \cr
#' Chen, Y., Chu, H., Luo, S., Nie, L., and Chen, S. (2014a). Bayesian
#' analysis on meta-analysis of case-control studies accounting for
#' within-study correlation. \cr
#' \emph{Statistical Methods in Medical Research}, 4.6 (2015): 836-855. \cr
#' <https://doi.org/10.1177/0962280211430889>. \cr
#' Chen, Y., Luo, S., Chu, H., Su, X., and Nie, L. (2014b). An empirical
#' Bayes method for multivariate meta-analysis with an application in
#' clinical trials. \cr
#' \emph{Communication in Statistics: Theory and Methods}, 43.16 (2014): 3536-3551. \cr
#' <https://doi.org/10.1080/03610926.2012.700379>. \cr
#' Chen, Y., Luo, S., Chu, H., Wei, P. (2013). Bayesian inference on risk
#' differences: an application to multivariate meta-analysis of adverse
#' events in clinical trials. \cr
#' \emph{Statistics in Biopharmaceutical Research}, 5(2), 142-155. \cr
#' <https://doi.org/10.1080/19466315.2013.791483>. \cr
#'
#' @examples
#' \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
#'  data(colorectal)
#'  colorectal['study_name'] <- colorectal['studynames']
#'  # ########################## If exact method is used ############################
#'  ## Create object multiple_tables_obj_exact
#'  multiple_tables_obj_exact <- MultipleTables.create(data=colorectal,
#'  measure='OR', model= 'Sarmanov')
#'  ## Model fit default
#'  multiple_tables_obj_exact <- MultipleTables.modelFit(
#'  multiple_tables_obj_exact, method = 'exact')
#'  ## Options for Control; If set number of posterior samples is 5000
#'  multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
#'  control = list(n_samples = 3000))
#'  ## If set intial values correspoinding to c(a1, b1, a2, b2, rho) as c(1,1,1,1,0):
#'  multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
#'  control = list(initial_values = c(1,1,1,1,0)))
#'  ## If maximum number of iterations for iteration is 100
#'  multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
#'  control = list(maxit = 100))
#'  ## If maximum number of iterations for iteration is 100 and number of posterior samples as 3000
#'  multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
#'  control = list(maxit = 100, nsamples = 3000))
#'  # ########################## If sampling method is used ############################
#'  multiple_tables_obj_sampling <- MultipleTables.create(data=colorectal,
#'  measure='OR', model= 'Sarmanov')
#'  multiple_tables_obj_sampling <- MultipleTables.modelFit(
#'  multiple_tables_obj_sampling, method = 'sampling')
#'  ## The options of \code{control} list specifying the fitting process are similar
#'  ## to the codes shown above.
#' }
#'
#' @seealso \code{MultipleTables.create}, \code{MultipleTables.summary}, and \code{MultipleTables.plot}.
#' @export
MultipleTables.modelFit<- function(multiple_tables_object,
                                   method = 'exact',
                                   verbose = FALSE,
                                   control =  list()){

  ## check input
  if (!inherits(multiple_tables_object, "MultipleTables"))
    stop("Use only with 'MultipleTables' objects.\n")

  multiple_tables_object$method <- checkMethodArgument(method,
                                        multiple_tables_object$measure,
                                        verbose = verbose)



  ### load control list
  control <- MultipleTables._setControlList(multiple_tables_object, control)



  ## Estimate prior parameters and do likelihood ratio test for correlation
  prior_estimate_and_test <- MultipleTables._priorParemetersEstimate(multiple_tables_object, control, verbose)
  multiple_tables_object$chi2_value <- prior_estimate_and_test$chi2_value
  multiple_tables_object$p_value <- prior_estimate_and_test$p_value
  multiple_tables_object$prior_mle <- prior_estimate_and_test$prior_mle
  multiple_tables_object$cov_matrix_log <- prior_estimate_and_test$cov_matrix_log
  multiple_tables_object$hessian_log <- prior_estimate_and_test$hessian_log



  ## draw posterior samples
  multiple_tables_object$samples <- MultipleTables._studySpecificPosteriorSamples(multiple_tables_object, control)

  # calculate density  for study specific measures
  multiple_tables_object$density <- MultipleTables._studySpecificPosteriorDensity(multiple_tables_object, control)


  invisible(multiple_tables_object)

}



#' @useDynLib mmeta
#' @title Summarize the object of class \code{MultipleTables}.
#' @description Summarize the model of the class \code{MultipleTables} fitted by \code{MultipleTables.modelFit}.
#' @param multiple_tables_object The object created by \code{MultipleTables.create}
#'  and fitted by \code{MultipleTables.modelFit}.
#' @param alpha a numeric value specifying the significant level. Default value sets to 0.05.
#' @param verbose a logical value; if TRUE (default), the detailed summary messages will display.
#' @param digit an integer value specifying how many decimal places to keep. Default value sets to 3.
#' @param control a list can be specified to control the fitting process.
#' @returns  A list with the following components: model, posterior mean & equal tail confidence interval of overall measure,
#' estimated hyperparameters, the chi-square test statistics & the p-value of the likelihood ratio test,
#' posterior means & the lower/upper bounds of the equal tail confidence intervals of study-specific measure.
#' @examples
#' \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
#'  data(colorectal)
#'  colorectal['study_name'] <- colorectal['studynames']
#'  ## If exact method is used, the codes for sampling method are similar.
#'  ## Create object multiple_tables_obj_exact
#'  multiple_tables_obj_exact <- MultipleTables.create(data=colorectal,
#'  measure='OR', model= 'Sarmanov')
#'  ## Model fit default
#'  multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact')
#'  ## Summary of the fitting process (default)
#'  multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact)
#'  ## Structure of SingleTable object
#'  str(multiple_tables_obj_exact)
#'  ## If set alpha level to 0.1
#'  multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact, alpha = 0.1)
#'  ## If set digit to 4
#'  multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact,
#'  alpha = 0.05, digit = 4)
#'  ## If decided not to print out
#'  multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact,
#'  alpha = 0.05, digit = 4,verbose = FALSE)
#' }
#' @seealso \code{MultipleTables.create}, \code{MultipleTables.modelFit}, and \code{MultipleTables.plot}.
#' @export
MultipleTables.summary<- function(multiple_tables_object,
                                  alpha = 0.05,
                                  verbose = TRUE,
                                  digit = 3,
                                  control = list() ){
  ## check input
  checkAlpha(alpha)
  checkDigit(digit)
  MultipleTables._isObjectFitted(multiple_tables_object)

  ## update object
  multiple_tables_object$alpha <- alpha
  multiple_tables_object$digit <- digit

  ### load control list
  control <- MultipleTables._setControlList(multiple_tables_object, control)


  ## Estimate overall measure(point and Wald confident internal)
  overall_measure_estimate <- MultipleTables._overallMeasureEstimate(multiple_tables_object)
  overall_measure_estimation <- list(point = overall_measure_estimate$overall,
                                    confindent_interval = overall_measure_estimate$CI)
  multiple_tables_object$overall_measure_estimation <- overall_measure_estimation


  ## Estimate study specific summary
  multiple_tables_object$specific_summary <- MultipleTables._StudySpecificMeasureEstimate(
                                              multiple_tables_object,
                                              control)

  ## print out report if user requests
  if(verbose){
    MultipleTables._summaryPrint(multiple_tables_object)
  }


  invisible(multiple_tables_object)
}



#' @useDynLib mmeta
#' @title Plot Method for \code{Multipletables} objects
#' @description  Produces a variety of plots for multiple tables analysis
#' @param multiple_tables_object The object inheriting from class \code{Multipletables}.
#' @param plot_type a character string specifying the kind of plots to
#' produce. Options are \code{density} and \code{forest} (default). See details
#' @param layout_type a character string specifying the type of the density plots (i.e., when \code{plot_type=‘density’}).
#' Options are \code{sidebyside} and \code{overlay} (default). This argument is NULL when \code{plot_type=‘forest’}
#' @param selected_study_names a numeric value or vector specifying which studies to
#' be plotted. By default (when \code{NULL}), all of the studies will be plotted.
#' @param xlim a numeric value specifying the lower and upper limits of the x-axis. Default is NULL.
#' For forest plots, if the lower bound of any measure is smaller than \code{xlim[1]} or the upper bound of
#' any measure is larger than \code{xlim[2]}, arrows will be used at
#' the limits to denote the bounds exceed the specified ranges.
#' @param add_vertical a numeric value specifying the x-value for a vertical
#' reference line at \code{x=addline}. Default is NULL.
#' @param show_CI a logical value; If TRUE (default) the forest plot will show the lower & upper bounds of CIs,
#' else the the lower & upper bounds of CIs will be omitted. This argument is always NULL when \code{plot_type=‘density’}.
#' @param by a character string specifying the way to distinguish different plots. Options are \code{line_type} (default) and \code{color}.
#' @details
#' If \code{plot_type=‘density’} and \code{layout_type='sidebyside'}, the posterior distributions of all
#' study-specific measure are displayed side by side in 4-panel plots with study names.
#'
#' If \code{plot_type=‘density’} and \code{layout_type='overlap'}, the posterior distributions of all
#' study-specific measure are displayed in one graph. To clarity, it
#' is advisable to specify a few studies by \code{selected_study_names} argument.
#'
#' If \code{type='forest')} and \code{layout_type='NULL'}, a forest plot of all study-specific and
#' overall measure with 95\% credible/confidence intervals are plotted.
#' @returns A ggplot2 object is returned.
#' @seealso \code{MultipleTables.create}, \code{MultipleTables.modelFit}, and \code{MultipleTables.summary}.
#' @examples
#' \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
#'  data(colorectal)
#'  colorectal['study_name'] <- colorectal['studynames']
#'  ## If exact method is used, the codes for sampling method are similar.
#'  ## Create object multiple_tables_obj_exact
#'  multiple_tables_obj_exact <- MultipleTables.create(data=colorectal,
#'   measure='OR', model= 'Sarmanov')
#'  ## Model fit default
#'  multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact')
#'  ## Summary of the fitting process (default)
#'  multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact)
#'  ## Density plot, overlay
#'  ## Note: There are no enough types of line, if we have too many densities!
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
#'  layout_type = 'overlay')
#'  ## Choose Set by = ‘color’
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
#'  layout_type = 'overlay',by = 'color')
#'  ## Set by = ‘color’ and specify xlim as 0 to 5.
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
#'  layout_type = 'overlay', by = 'color', xlim = c(0,5))
#'  ## Set by = ‘color’ and specify xlim as 0 to 5 and add vertical line at OR = 1
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
#'  layout_type = 'overlay', by = 'color',xlim = c(0,5), add_vertical = 1)
#'  ## If select three studies
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
#'  layout_type = 'overlay',selected_study_names = c('Bell','Chen','Oda'), xlim = c(0,5))
#'  ## We can add external layouts for the return ggplot2. xlab as Odds ratio
#'  ggplot2_obj <- MultipleTables.plot(multiple_tables_obj_exact,
#'  plot_type = 'density', layout_type = 'overlay', by = 'color',xlim = c(0,5))
#'  ggplot2_obj + xlab('Odds Ratio')  + ggtitle('OR ration for XX cancer')
#'  ## density plot, plot side by side
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
#'  layout_type = 'side_by_side')
#'  ## Forest plot (default)
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'forest')
#'  ## forest plot: not show the CIs
#'  MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'forest',
#'  add_vertical =  1, show_CI = FALSE)
#' }
#' @export
MultipleTables.plot <- function(multiple_tables_object,
                                plot_type = 'forest',
                                layout_type = 'overlay',
                                selected_study_names = NULL,
                                xlim = NULL,
                                add_vertical = NULL,
                                show_CI = TRUE,
                                by = 'line_type'){
  ## check input
  MultipleTables._isObjectFitted(multiple_tables_object)
  checkPlotType(plot_type)
  checkPlotLayoutType(layout_type)
  checkPlotBy(by)
  select_study_names <- checkSelectStudy(selected_study_names, multiple_tables_object)


  if(plot_type == 'density'){

    if(layout_type == 'overlay'){
      ggplot2_obj <- MultipleTables._plotDensityOverlay(multiple_tables_object,
                                         select_study_names,
                                         xlim = xlim,
                                         add_vertical,
                                         by = by)


    }

    if(layout_type == 'side_by_side'){
      ggplot2_obj <- MultipleTables._plotDensitySideBySide(multiple_tables_object,
                                                        select_study_names,
                                                        xlim = xlim,
                                                        add_vertical)
    }

  }

  if(plot_type == 'forest'){
    ggplot2_obj <- MultipleTables._plotForest(multiple_tables_object,
                                                         select_study_names,
                                                         xlim = xlim,
                                                         add_vertical,
                                                         show_CI = show_CI)
  }


  return(ggplot2_obj)
}


