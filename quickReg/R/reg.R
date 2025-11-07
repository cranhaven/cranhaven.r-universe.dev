#' Build regression models
#'
#' Build general linear model, logistic regression model, cox regression model with one or more dependent variables. Allow regression based on subgroup variables.

#' @param data A data.frame to build the regression model.
#' @param x Integer column indices or names of the variables to be included in univariate analysis. If \code{NULL}, the default columns are all the variables except `y`, `group`, `time` and `cov`.
#' @param y Integer column indice or name of dependent variables, integer or character, allow more than one dependent variables
#' @param group Integer column indice or name of subgroup variables.
#' @param group_combine A logical, subgroup analysis for combination of group variables or each group variables. The default is FALSE (subgroup analysis for each group variable)
#' @param cov Integer column indices or name of covariate variables
#' @param factors Integer column indices or names of variables to be treated as factor
#' @param model regression model, see \code{\link{lm}}, \code{\link{glm}}, \code{\link[survival]{coxph}} for more details
#' @param time Integer column indices  or name of survival time, used in cox regression, see \code{\link[survival]{coxph}} for more details
# @param \dots Further arguments passed to regression model
#' @param confint_glm A character, 'default' or 'profile'. The default method for 'glm' class to compute confidence intervals assumes asymptotic normality \code{\link[stats]{confint}}, you can also use profile likelihood method \code{\link[MASS]{confint.glm}}, but it is pretty slow.
#' In this case you could specify 'default' for speed.
#' @return The return result is a concentrated result in a  data.frame.
#' @param  cov_show A logical, whether to create covariates result, default FALSE
#' @export

reg <- function(data = NULL, x = NULL, y = NULL,group=NULL, cov=NULL, factors = NULL, model = NULL,
                  time = NULL, cov_show=FALSE,confint_glm="default",group_combine=FALSE) {

  if(!is.data.frame(data)) {
    tryCatch( {
      data<-as.data.frame(data,stringsAsFactors = FALSE)
    }, error = function(e) stop("`data` is not a data.frame.", call. = FALSE)
    )
  }

  if (!is.character(x)) x<-names(data)[x]
  if (!is.character(y)) y<-names(data)[y]
  if (!is.character(group)) group<-names(data)[group]
  if (!is.character(cov)) cov<-names(data)[cov]
  if (!is.character(factors)) factors<-names(data)[factors]
  if (!is.character(time)) time<-names(data)[time]

  if (length(x)==0)
    x = setdiff(names(data), c(y,cov,time,group))

  if (any(y %in% x) || any(y %in% cov) || any(y %in% group)) {
    warning(paste0("Dependent varibale indice: `", y, "` is also in `x`, `cov` or `group`, will be removed from them, please check.\n"),
            call. = FALSE)
    x = setdiff(x, c(y, time,group))
    cov = setdiff(cov, c(y, time,group))
  }

  if (length(intersect(x, cov)!=0)) {
    warning(paste0("`x` varibale indice: `", intersect(x, cov), "` is also in `cov`, will be removed from `cov`, please check.\n"),
            call. = FALSE)
    cov = setdiff(cov,x)
  }

  if (! all(group %in% names(data))) {
    stop("Cannot find group variables!", call. = FALSE)
  }

  if (!is.logical(group_combine)) {
    stop("`group_combine` should be TRUE or FALSE!", call. = FALSE)
  }

  if ((!isTRUE(group_combine)) & length(group)>1 ) {
    result_dataframe<-vector(mode = "list", length = length(group))
    for (i in seq_along(group)) {
      group_i <- group[i]
      fit<-data %>%
        group_by_at(vars(group_i))  %>%
        do(reg_y(data = ., x = x, y = y,cov=cov, factors = factors, model = model,
               time = time, cov_show=cov_show,confint_glm=confint_glm)) %>%
        ungroup() %>%
        rename(level=!!group_i)
      result_dataframe[[i]]<-as.data.frame(base::cbind(group=group_i, fit),stringsAsFactors = FALSE)
    }
    result<-as.data.frame(do.call(rbind,result_dataframe), stringsAsFactors = FALSE)

  } else {
    result<-data %>%
      group_by_at(vars(group))  %>%
      do(reg_y(data = ., x = x, y = y,cov=cov, factors = factors, model = model, time = time, cov_show=cov_show,confint_glm=confint_glm)) %>%
      ungroup()
  }
  attr(result,"class")<-c("reg",class(result))
  attr(result,"model")<-model
  return(invisible(result))
}

