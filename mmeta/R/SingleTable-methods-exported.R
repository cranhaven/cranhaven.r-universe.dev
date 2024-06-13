


#' @useDynLib mmeta
#' @title Create an object of class \code{singletable}.
#' @description Create an object of class \code{SingleTable}, which is
#' a components list of exact posterior inference based on single 2x2 table.
#' @usage SingleTable.create(a1,b1,a2,b2,rho,y1,n1,y2,n2,model,measure)
#' @param a1 a numeric value specifying the first hyperparameter of the beta prior for group 1.
#' @param b1 a numeric value specifying the second hyperparameter of the beta prior for group 1.
#' @param a2 a numeric value specifying the first hyperparameter of the beta prior for group 2.
#' @param b2 a numeric value specifying the second hyperparameter of the beta prior for group 2.
#' @param rho a numeric value specifying correlation coefficient for Sarmanov bivariate prior distribution.
#' @param y1 an integer indicating the number of events in group 1.
#' @param n1 an integer indicating the total number of subjects in group 1.
#' @param y2 an integer indicating the number of events in group 2.
#' @param n2 an integer indicating the total number of subjects in group 2.
#' @param model a character string specifying the model. Options are \code{Independent} and \code{Sarmanov}. \code{Independent} is
#' independent beta-binomial model. \code{Sarmanov}is Sarmanov beta-binomial model.
#' @param measure a character string specifying a measure. Options are
#'   \code{OR}, \code{RR}, and \code{RD}. \code{OR} is odds
#'    ratio, \code{RR} is relative risk, and \code{RD} is risk difference.
#' @details There are two kinds of study design, i.e., prospective study or
#' clinical trial, and retrospective or case-control study.
#' In a prospective study or clinical trial, \code{data} is a data
#' frame that contains \code{y1}, \code{n1}, \code{y2}, \code{n2}.
#' \code{y1} is the number of subjects
#' experienced a certain event in the unexposed group. \code{n1} is the number
#' of subjects in the unexposed group. \code{y2} is the number of subjects experienced
#' a certain event in the exposed group. \code{n2} is the number of
#' subjects in the exposed group. In this study, \code{OR} is odds ratio
#' of event comparing exposed group with unexposed group. \code{RR}
#' is relative risk of event comparing exposed group with unexposed group. \code{RD} is risk
#' difference of event comparing exposed group with unexposed group.
#'
#' For case-control study, \code{y1} is the number of subjects with
#' exposure in the control group. \code{n1} is the number of
#' subjects in the control group. \code{y2} is the number of
#' subjects with exposure in the case group. \code{n2} is the
#' number of subjects in the case group. In this study, \code{OR} is odds ratio
#' of event comparing case group with control group. \code{RR} is
#' relative risk of event comparing case group with control group. \code{RD} is risk
#' difference of event comparing case group with control group.
#' When model='\code{Sarmanov}', \code{rho} is subject to constraints. See Chen et al(2011) for details.
#' @examples
#' ## Specify data (y1, n1, y2, n2), parameters (a1, b1, a2, b2, rho), model (Sarmanov/Independent),
#' ## and Specify measure(OR/RR/RD)
#' ## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
#' ## Create object \code{single_table_obj}
#'  \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  single_table_obj <- SingleTable.create(a1=0.5,b1=0.5,
#'  a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")}
#' @returns  An object is returned, inheriting from class \code{singletable}.
#' The Objects of this class contain the meta-data for generic functions: \code{SingleTable.modelFit},
#' \code{SingleTable.summary}, and \code{SingleTable.plot}. The following values
#' of the object must be non-null under \code{SingleTable.create}:
#' \item{measure}{the value of \code{measure} argument.}
#' \item{model}{the value of \code{model} argument.}
#' \item{data}{a numeric vector of input data with components: \code{y1}, \code{n1}, \code{y2}, \code{n2}}
#' \item{parameter}{a numeric vector of the hyperparameters: \code{a1}, \code{b1}, \code{a2}, \code{b2}, and \code{rho}.}
#' @seealso \code{SingleTable.modelFit}, \code{SingleTable.summary}, \code{SingleTable.plot}.
#' @references Chen, Y., Luo, S., (2011a). A Few Remarks on "Statistical Distribution of the Difference of
#' Two Proportions' by Nadarajah and Kotz, Statistics in Medicine 2007; 26(18):3518-3523". \cr
#' \emph{Statistics in Medicine, 30(15)}, 1913-1915. \cr
#' <doi:10.1002/sim.4248> \cr
#' @export
SingleTable.create <- function(a1 = NULL, b1= NULL,
                               a2 = NULL, b2 = NULL,
                               rho = NULL,
                               y1 = NULL, n1 = NULL,
                               y2 = NULL, n2 = NULL,
                               model = 'Sarmanov',
                               measure = measure){
  ## check input
  checkModelArgument(model)
  checkMeasureArgument(measure)
  checkModelParameters(a1 = a1, b1= b1, a2 = a2, b2 = b2, rho = rho, model = model)
  checkSingleTableData(y1 = y1, n1 = n1, y2 = y2, n2 = n2)

  ## create object
  single_table_object <- list(
    parms_prior = list(a1 = a1, b1= b1, a2 = a2, b2 = b2, rho = rho),
    data = list(y1 = y1, n1 = n1, y2 = y2, n2 = n2),
    model = model,
    measure = measure,
    method = NULL,
    density = NULL,
    samples = NULL,
    alpha = NULL,
    summary = NULL
  )
  attr(single_table_object, "class") <- "SingleTable"


  invisible(single_table_object)

}




#' @useDynLib mmeta
#' @title Exact posterior inference based on a single 2x2 table
#' @description This function conducts exact posterior inference based on the object created by \code{SingleTable.create}.
#' @param single_table_Obj The object created by \code{SingleTable.create}.
#' @param method a character string specifying the method. Options are \code{exact}
#' and \code{sampling}. \code{exact} (default) is a method based on Monte Carlo sampling. \code{exact} is exact method.
#' @param verbose a logical value; if TRUE(default), the detailed summary messages are displayed, else the messages are omitted.
#' @param control a list can be specified to control the fitting process. Options are stated in details.
#' @details control list can be specified to control the fitting process:
#' \itemize{
#' \item \code{n_samples}:  number of posterior samples; Defualt is 5000.
#' \item \code{mcmc_initial}:  initial values for (p1, p2) in MCMC; Default is c(0.5, 0.5).
#' \item \code{upper_bound}:  upper bound for the measure. Default is 100.
#' \item \code{lower_bound}: lower bound for the measure. For RD, default is -1. For RR/OR, defualt is 0.
#' \item \code{num_grids}:  number of grids to calculate density; The defualt is 20498.
#' }
#' @returns An object of \code{singletable} class is returned including the following non-null values:
#' \item{measure}{the value of \code{measure} argument.}
#' \item{model}{the value of \code{model} argument.}
#' \item{data}{a numeric vector of input data with components: \code{y1}, \code{n1}, \code{y2}, \code{n2}.}
#' \item{parameter}{a numeric vector of the hyperparameters: \code{a1}, \code{b1}, \code{a2}, \code{b2}, and \code{rho}.}
#' \item{method}{the value of \code{method} argument.}
#' \item{sample}{a list of samples for the posterior and prior distributions.}
#' \item{density}{a list of the density of the posterior and prior distributions.}
#' @examples
#' ## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
#'  \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  # ########################## If sampling method is used ############################
#'  ## Create object \code{single_table_obj_samling}
#'  single_table_obj_samling <- SingleTable.create(a1=0.5,b1=0.5,
#'  a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")
#'  ## model fit
#'  single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
#'  method = 'sampling')
#'  ## Control list option examples
#'  ## set number of posterior samples as 3000 (default is 5000)
#'  single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
#'  method = 'sampling', control = list(n_sample = 3000))
#'  ## set initial values for MCMC is c(0.2, 0,4) (default is c(0.5,0.5))
#'  single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
#'  method = 'sampling', control = list(mcmc_initial = c(0.2,0.4)))
#'  ## set upper bound for the measure is 20( default is 100)
#'  single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
#'  method = 'sampling', control = list(upper_bound = 20))
#'  # ########################### If exact method is used ##############################
#'  ## Create object \code{single_table_obj_exact}
#'  single_table_obj_exact <- SingleTable.create(a1=0.5, b1=0.5, a2=0.5, b2=0.5,
#'  rho=0.5, y1=40, n1=96, y2=49, n2=109, model="Sarmanov",measure="OR")
#'  ## model fit
#'  single_table_obj_exact <- SingleTable.modelFit(single_table_obj_exact, method = 'exact')
#'  ## The options of \code{control} list specifying the fitting process are similar
#'  ## to the codes shown above.
#'  }
#' @seealso \code{SingleTable.summary}, \code{SingleTable.plot}.
#' @export
SingleTable.modelFit <- function(single_table_Obj,
                                 method = 'exact',
                            verbose = TRUE,
                            control = list()){

  if (!inherits(single_table_Obj, "SingleTable"))
    stop("Use only with 'SingleTable' objects.\n")

  ## check input
  single_table_Obj$method <- checkMethodArgument(method, single_table_Obj$measure, verbose = verbose)




  ## load control list
  control <- SingleTable._setControlList(single_table_Obj, control)


  ## draw MCMC samples and calculate the density
  single_table_Obj$samples <- SingleTable._sampleGen(single_table_Obj, control)
  single_table_Obj$density <- SingleTable._densityCal(single_table_Obj, control)
  invisible(single_table_Obj)
}





#' @useDynLib mmeta
#' @title Summarize the object of class \code{singletable}.
#' @description Summarize model of the single table analysis fitted by \code{SingleTable.modelFit}.
#' @param single_table_Obj The object created by \code{SingleTable.create} and fitted by \code{SingleTable.modelFit}.
#' @param alpha a numeric value specifying the significant level. Default value sets to 0.05.
#' @param verbose a logical value; if TRUE(default), the detailed summary messages will display.
#' @param digit an integer value specifying how many decimal places to keep. Default value sets to 3.
#' @param control a list can be specified to control the fitting process.
#' @returns  A list with the following components: measure, model, posterior mean, posterior median, equal tail CI, and HDR CI.
#' @examples
#' ## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
#'  \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  ## If exact method is used, the codes for sampling method are similar.
#'  ## Create object \code{single_table_obj_exact}
#'  single_table_obj_exact <- SingleTable.create(a1=0.5,b1=0.5,
#'  a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")
#'  ## model fit
#'  single_table_obj_exact <- SingleTable.modelFit(single_table_obj_exact, method = 'exact')
#'  ## Summary of the fitting process (default)
#'  single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, alpha = 0.05)
#'  ## Structure of SingleTable object
#'  str(single_table_obj_exact)
#'  ## If set alpha level to 0.1
#'  single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, alpha = 0.1)
#'  ## If set digit to 2
#'  single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, digit  = 2)
#'  ## If decided not to print out
#'  single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, verbose = FALSE)
#'  }
#' @export
SingleTable.summary <- function(single_table_Obj,
                                alpha = 0.05,
                                verbose = TRUE,
                                digit = 3,
                                control = list()){
  ## check input
  checkAlpha(alpha)
  SingleTable._isObjectFitted(single_table_Obj)
  checkDigit(digit)


  ## load control list
  control <- SingleTable._setControlList(single_table_Obj, control)

  # update single_table_Obj
  single_table_Obj$alpha <- alpha
  single_table_Obj$digit <- digit

  ## calculate summary
  if(single_table_Obj$method == 'sampling'){
    single_table_Obj$summary <- SingleTable._summaryCalSampling(single_table_Obj, control)
  }
  if(single_table_Obj$method == 'exact'){
    single_table_Obj$summary <- SingleTable._summaryCalExact(single_table_Obj, control)
  }

  ## print summary if user requests
  if(verbose){
    SingleTable._summaryPrint(single_table_Obj)
  }

  invisible(single_table_Obj)
}



#' @useDynLib mmeta
#' @title Plot Method for \code{singletable} objects.
#' @description Produces various plots for single table analysis.
#' @param single_table_Obj The object inheriting from class \code{singletable}.
#' @param type a character string specifying the type of plots to
#' produce. Options are \code{sidebyside}(default) and \code{overlay}.
#' @param xlim a numeric value specifying the lower and upper limits of the x-axis. Default is NULL.
#' @param add_vertical a numeric value specifying the x-value for a vertical
#' reference line at \code{x=addline}. Default is NULL.
#' @param by a character string specifying the way to distinguish different plots. Options are \code{line_type}(default) and \code{color}.
#' @details  If \code{type="sidebyside"}, the posterior distribution of measure
#' and the prior distribution are drawn side by side in two plots. If
#' \code{type="overlay"}, the posterior distribution of measure and
#' the prior distribution are overlaid in one plot.
#' @returns A ggplot2 object is returned.
#' @examples
#' ## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
#'  \donttest{
#'  library(mmeta)
#'  library(ggplot2)
#'  ## If exact method is used, the codes for sampling method are similar.
#'  ## Create object \code{single_table_obj_exact}
#'  single_table_obj_exact <- SingleTable.create(a1=0.5,b1=0.5,
#'  a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")
#'  ## model fit
#'  single_table_obj_exact <- SingleTable.modelFit(single_table_obj_exact, method = 'exact')
#'  ## Summary of the fitting process (default)
#'  single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, alpha = 0.05)
#'  ## Plot the densities side-by-side
#'  SingleTable.plot(single_table_obj_exact, type = 'side_by_side')
#'  ## set xlim between 0 to 4 and add vertical line at x = 1
#'  SingleTable.plot(single_table_obj_exact, type = 'side_by_side',
#'  xlim = c(0,4), add_vertical = 1)
#'  ## override xlab and add title via ggplot2
#'  plot_obj <- SingleTable.plot(single_table_obj_exact, type = 'side_by_side',
#'  xlim = c(0,4), add_vertical = 1)
#'  plot_obj + xlab('Odds ratio') + ggtitle("Plot of density function")
#'  ## Overlay plot the density
#'  SingleTable.plot(single_table_obj_exact, type = 'overlay')
#'  ## Plot by color instead of line type
#'  SingleTable.plot(single_table_obj_exact, type = 'overlay',by = 'color')
#'  }
#' @export
SingleTable.plot <- function(single_table_Obj,
                             type = 'side_by_side',
                             xlim = NULL,
                             add_vertical = NULL,
                             by = 'line_type'){
  ## check input
  checkPlotLayoutType(type)
  checkPlotBy(by)
  SingleTable._isObjectFitted(single_table_Obj)


  if(type == 'side_by_side')
    return(SingleTable._plotSideBySide(single_table_Obj,
                                  xlim = xlim,
                                  add_vertical = add_vertical))

  if(type == 'overlay'){
    return(SingleTable._plotOverlay(single_table_Obj,
                                    xlim = xlim,
                                    add_vertical = add_vertical,
                                    by = by))
  }
}


