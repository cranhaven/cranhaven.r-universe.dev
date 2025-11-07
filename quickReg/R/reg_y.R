#' Build regression models with more than one dependent variable
#'
#' Build general linear model, generalized linear model, cox regression model,etc.

#' @param data A data.frame
#' @param x Integer column indices or names of the variables to be included in univariate analysis, the default columns are all the variables except `y` and `time` and `cov`.
#' @param y Integer column indices or name of dependent variable
#' @param cov Integer column indices or name of covariate variables
#' @param factors Integer column indices or names of variables to be treated as factor
#' @param model regression model, see \code{\link{lm}}, \code{\link{glm}}, \code{\link[survival]{coxph}} for more details
#' @param time Integer column indices  or name of survival time, used in cox regression, see \code{\link[survival]{coxph}} for more details
# @param \dots Further arguments passed to regression model
#' @param confint_glm A character, 'default' or 'profile'. The default method for 'glm' class to compute confidence intervals assumes asymptotic normality \code{\link[stats]{confint}}, you can also use profile likelihood method \code{\link[MASS]{confint.glm}}, but it is pretty slow.
#' In this case you could specify 'default' for speed.
#' @param  cov_show A logical, whether to create covariates result, default FALSE
#' @return The return result is a concentrated result in a  data.frame.
#' @importFrom stats binomial confint glm lm
#' @export



reg_y <- function(data = NULL, x = NULL, y = NULL,cov=NULL, factors = NULL, model = NULL,
                time = NULL,confint_glm="default", cov_show=FALSE) {

  if(!is.data.frame(data)) {
    tryCatch( {
      data<-as.data.frame(data,stringsAsFactors = FALSE)
    }, error = function(e) stop("`data` is not a data.frame.", call. = FALSE)
    )
  }

  if (!is.character(x)) x<-names(data)[x]
  if (!is.character(y)) y<-names(data)[y]
  if (!is.character(cov)) cov<-names(data)[cov]
  if (!is.character(factors)) factors<-names(data)[factors]
  if (!is.character(time)) time<-names(data)[time]

  if (length(x)==0)
    x = setdiff(names(data), c(y,cov,time))

  if (any(y %in% x) || any(y %in% cov)) {
    warning(paste0("Dependent varibale indice: `", y, "` is also in `x` or `cov`, will be removed from them, please check.\n"),
            call. = FALSE)
    x = setdiff(x, c(y, time))
    cov = setdiff(cov, c(y, time))
  }

  if (length(intersect(x, cov)!=0)) {
    warning(paste0("`x` varibale indice: `", intersect(x, cov), "` is also in `cov`, will be removed from `cov`, please check.\n"),
            call. = FALSE)
    cov = setdiff(cov,x)
  }


    result_dataframe_y <- vector(mode = "list", length = length(y))

  split_line <- paste0(rep.int("#",100),collapse = "")

  if (length(y) >1) {
    for (i in seq_along(y)) {
      group_y <- y[i]
      fit_y<-reg_x(data = data, x = x, y = group_y,cov=cov, factors = factors, model = model,
                   time = time, cov_show=cov_show,confint_glm=confint_glm,detail_show=FALSE)
      result_dataframe_y[[i]]<-as.data.frame(cbind(group_y, fit_y),stringsAsFactors = FALSE)
    }
    result_dataframe_y<-as.data.frame(do.call(rbind,result_dataframe_y), stringsAsFactors = FALSE)
    } else {
    group_y <- y
    fit_y<-reg_x(data = data, x = x, y = group_y,cov=cov, factors = factors, model = model,
                 time = time,confint_glm=confint_glm, cov_show=cov_show,detail_show=FALSE)
    result_dataframe_y<-as.data.frame(cbind(group_y, fit_y),stringsAsFactors = FALSE)
  }

 result <- result_dataframe_y
 names(result)[1]<-"y"
 attr(result,"class")<-c("reg","reg_y",class(result))
 attr(result,"model")<-model
 return(invisible(result))
}


