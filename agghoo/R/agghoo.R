#' agghoo
#'
#' Run the (core) agghoo procedure.
#' Arguments specify the list of models, their parameters and the
#' cross-validation settings, among others.
#'
#' @param data Data frame or matrix containing the data in lines.
#' @param target The target values to predict. Generally a vector,
#'        but possibly a matrix in the case of "soft classification".
#' @param task "classification" or "regression". Default:
#'        regression if target is numerical, classification otherwise.
#' @param gmodel A "generic model", which is a function returning a predict
#'        function (taking X as only argument) from the tuple
#'        (dataHO, targetHO, param), where 'HO' stands for 'Hold-Out',
#'        referring to cross-validation. Cross-validation is run on an array
#'        of 'param's. See params argument. Default: see R6::Model.
#' @param params A list of parameters. Often, one list cell is just a
#'        numerical value, but in general it could be of any type.
#'        Default: see R6::Model.
#' @param loss A function assessing the error of a prediction.
#'        Arguments are y1 and y2 (comparing a prediction to known values).
#'        loss(y1, y2) --> real number (error). Default: see R6::AgghooCV.
#'
#' @return
#' An R6::AgghooCV object o. Then, call o$fit() and finally o$predict(newData)
#'
#' @examples
#' # Basic usage:
#'
#' # Regression:
#' a_reg <- agghoo(iris[,-c(2,5)], iris[,2])
#' a_reg$fit()
#' pr <- a_reg$predict(iris[,-c(2,5)] + rnorm(450, sd=0.1))
#' # Classification
#' a_cla <- agghoo(iris[,-5], iris[,5])
#' a_cla$fit()
#' pc <- a_cla$predict(iris[,-5] + rnorm(600, sd=0.1))
#'
#' \donttest{
#' # Advanced usage:
#' data(iris)
#' library(mlbench)
#' data(PimaIndiansDiabetes)
#'
#' # Run only agghoo on iris dataset (split into train/test, etc).
#' # Default parameters: see ?agghoo and ?AgghooCV
#' compareTo(iris[,-5], iris[,5], agghoo_run)
#'
#' # Run both agghoo and standard CV, specifiying some parameters.
#' compareTo(iris[,-5], iris[,5], list(agghoo_run, standardCV_run), gmodel="tree")
#' compareTo(iris[,-5], iris[,5], list(agghoo_run, standardCV_run),
#'           gmodel="knn", params=c(3, 7, 13, 17, 23, 31),
#'           CV = list(type="vfold", V=5, shuffle=TRUE))
#'
#' # Run both agghoo and standard CV, averaging errors over N=10 runs
#' # (possible for a single method but wouldn't make much sense...).
#' nc <- 1 #for CRAN
#' compareMulti(PimaIndiansDiabetes[,-9], PimaIndiansDiabetes[,9],
#'              list(agghoo_run, standardCV_run), N=10, gmodel="tree", nc=nc)
#'
#' # Compare several values of V
#' compareRange(PimaIndiansDiabetes[,-9], PimaIndiansDiabetes[,9],
#'              list(agghoo_run, standardCV_run), N=10, V_range=c(10, 20, 30), nc=nc)
#'
#' # For example to use average of squared differences.
#' # Default is "mean(abs(y1 - y2))".
#' loss2 <- function(y1, y2) mean((y1 - y2)^2)
#'
#' # In regression on artificial datasets (TODO: real data?)
#' data <- mlbench.twonorm(300, 3)$x
#' target <- rowSums(data)
#' compareMulti(data, target, list(agghoo_run, standardCV_run), nc=nc,
#'              N=10, gmodel="ppr", params=c(1, 3, 5, 7, 9), loss=loss2,
#'              CV = list(type="MC", V=12, test_size=0.3))
#'
#' compareMulti(data, target, list(agghoo_run, standardCV_run), nc=nc,
#'              N=10, floss=loss2, CV = list(type="vfold", V=10, shuffle=FALSE))
#'
#' # Random tests to check that method doesn't fail in 1D case
#' M <- matrix(rnorm(200), ncol=2)
#' compareTo(as.matrix(M[,-2]), M[,2], list(agghoo_run, standardCV_run), gmodel="knn")
#' compareTo(as.matrix(M[,-2]), M[,2], list(agghoo_run, standardCV_run), gmodel="ppr")
#' }
#'
#' @seealso Function \code{\link{compareTo}}
#'
#' @references
#' Guillaume Maillard, Sylvain Arlot, Matthieu Lerasle. "Aggregated hold-out".
#' Journal of Machine Learning Research 22(20):1--55, 2021.
#'
#' @export
agghoo <- function(
  data, target, task = NULL, gmodel = NULL, params = NULL, loss = NULL
) {
	# Args check:
  checkDaTa(data, target)
  task <- checkTask(task, target)
  modPar <- checkModPar(gmodel, params)
  loss <- checkLoss(loss, task)

  # Build Model object (= list of parameterized models)
  model <- Model$new(data, target, task, modPar$gmodel, modPar$params)

  # Return AgghooCV object, to run and predict
  AgghooCV$new(data, target, task, model, loss)
}
