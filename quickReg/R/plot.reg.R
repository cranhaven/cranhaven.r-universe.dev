#' @title plot the result of regression result
#'
#' @description plot coefficients, OR or HR of regression models.

#' @param x A reg, reg_y or reg_x object without covariates information, 'cov_show=FALSE'
#' @param limits A numeric vector of length two providing limits of the scale. Use NA to refer to the existing minimum or maximum value.
#' @param sort A character determining the order of variables to plot, 'alphabetical' or 'order'. The later is the default to sort variables according to their effect size.
#' @param title title of plot
#' @param remove A logical, whether to remove infinite and NA value. The default is TRUE
#' @param term A character of x axis variable in plot
#' @param center A character of coefficient, OR or HR variable in plot
#' @param low A character of lower confidence interval variable
#' @param high A character of upper confidence interval variable
#' @param model A character of model, "lm", "glm" or "coxph"
#' @param \dots additional arguments. When using your own regression results rather than from 'quickReg', please provide `term`,`center`,`lower`, `high` and `model` for plot.
#' @seealso \code{\link{reg}}, \code{\link{reg_x}}, \code{\link{reg_y}}
#' @import ggplot2
#' @export
#' @examples
#' reg_glm<-reg(data = diabetes, y = 5, factor = c(1, 3, 4), model = 'glm')
#' plot(reg_glm)
#' plot(reg_glm, limits = c(NA, 3))
#'

plot.reg <- function(x, limits = c(NA, NA), sort = "order", title=NULL,remove=TRUE,...) {
  stopifnot((is.na(limits[1]) || is.numeric(limits[1])) && (is.na(limits[2]) ||
                                                              is.numeric(limits[2])))

  if(!is.data.frame(x)) {
    tryCatch( {
      data<-as.data.frame(x,stringsAsFactors = FALSE)
    }, error = function(e) stop("`x` is not a data.frame, please check.", call. = FALSE)
    )
  } else data <- x


  if (remove) {
    data<-do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),NA)))
    data<-na.omit(data)
  }

    names(data)[(NCOL(x) - 3):(NCOL(x)-1)] <- c("center", "low","high")
    var_model<-attr(x,"model")

  if (is.na(limits[1]))
    limits[1] <- min(data$low, na.rm = TRUE)
  if (is.na(limits[2]))
    limits[2] <- max(data$high, na.rm = TRUE)

  if (!is.na(diff(limits)) && diff(limits) < 0) {
    stop("A problem with limits, please check it out!", call. = FALSE)
  }
  adj <- diff(limits) * 0.05

  data <- cbind(data, beyond1 = ifelse(data$low < limits[1] - adj, TRUE,
                                       NA), beyond2 = ifelse(data$high > limits[2] + adj, TRUE, NA))

  data$low <- ifelse(data$low < limits[1], limits[1], data$low)
  data$high <- ifelse(data$high > limits[2], limits[2], data$high)

  if (sort == "order") {
    data$term <-factor(data$term, levels=unique(unlist(data[order(data$center, decreasing = TRUE), "term"])))
  } else if (sort == "alphabetical") {
    data$term <-factor(data$term, levels=unique(unlist(data[order(data$term, decreasing = TRUE), "term"])))
  }

  if (any(duplicated(data$term))) {
    message(paste0("Some variables are duplicated in your regression result.\nUsing cov_show = FALSE for covariate variables or facet for subgroup result.\n"))
  }


  p <- ggplot(data, aes(term, center, ymin = low, ymax = high, color = term)) +
    geom_linerange() + geom_point(size = 1.5, shape = 15) + theme(plot.title = element_text(size = 30),
                                                                  axis.text.x = element_text(hjust = 1, vjust = 0, size = 8), legend.position = "none") +
    coord_flip(ylim = limits) + geom_segment(aes(x = term, y = low * beyond2,
                                                 xend = term, yend = limits[2] * beyond2 + adj, group = term), arrow = arrow(type = "closed",
                                                                                                                             length = unit(0.15, "inches")), na.rm = TRUE) + geom_segment(aes(x = term,
                                                                                                                                                                                              y = high * beyond1, xend = term, yend = limits[1] * beyond1 - adj,
                                                                                                                                                                                              group = term), arrow = arrow(type = "closed", length = unit(0.15, "inches")),
                                                                                                                                                                                          na.rm = TRUE)
  if (is.null(title)) p<-p+labs(title=substitute(x))


  p <- switch(var_model,
              lm = p + ylab("coefficients(95%CI)")+geom_abline(intercept = 0, slope = 0, colour = "red", size = 0.1),
              glm = p + ylab("OR(95%CI)")+geom_abline(intercept = 1, slope = 0, colour = "red", size = 0.1),
              coxph = p + ylab("HR(95%CI)")+geom_abline(intercept = 1, slope = 0, colour = "red", size = 0.1))


  if ("reg_y" %in% class(x)) p<-p+facet_grid(.~y, scales = "free")
  if (all(c("group","level") %in% names(x))) p<-p+facet_grid(level~group, scales = "free")


  return(p)
}



#' @rdname plot.reg
#' @export

plot_reg <- function(x, limits = c(NA, NA), sort = "order", title=NULL,remove=TRUE,term=NULL,center=NULL,low=NULL,high=NULL,model=NULL,...) {
  if (! "reg" %in% class(x)  ) {
    if(any(sapply(c(term,center,low,high,model),is.null))) {
      stop("Not `reg` object, please provide `term`,`center`,`low`, `high` and `model` for plot.", call. = FALSE)
    }
  }
  stopifnot((is.na(limits[1]) || is.numeric(limits[1])) && (is.na(limits[2]) ||
                                                              is.numeric(limits[2])))

  if(!is.data.frame(x)) {
    tryCatch( {
      data<-as.data.frame(x,stringsAsFactors = FALSE)
    }, error = function(e) stop("`x` is not a data.frame, please check.", call. = FALSE)
    )
  } else data <- x


  if (remove) {
    data<-do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),NA)))
    data<-na.omit(data)
  }

  if ("reg" %in% class(x)) {
    names(data)[(NCOL(x) - 3):(NCOL(x)-1)] <- c("center", "low","high")
    var_model<-attr(x,"model")
  } else {
    data[,c("term","center", "low","high")]<-data[,c(term,center,low,high)]
    var_model<-model
  }

  if (is.na(limits[1]))
    limits[1] <- min(data$low, na.rm = TRUE)
  if (is.na(limits[2]))
    limits[2] <- max(data$high, na.rm = TRUE)

  if (!is.na(diff(limits)) && diff(limits) < 0) {
    stop("A problem with limits, please check it out!", call. = FALSE)
  }
  adj <- diff(limits) * 0.05

  data <- cbind(data, beyond1 = ifelse(data$low < limits[1] - adj, TRUE,
                                       NA), beyond2 = ifelse(data$high > limits[2] + adj, TRUE, NA))

  data$low <- ifelse(data$low < limits[1], limits[1], data$low)
  data$high <- ifelse(data$high > limits[2], limits[2], data$high)

  if (sort == "order") {
    data$term <-factor(data$term, levels=unique(unlist(data[order(data$center, decreasing = TRUE), "term"])))
  } else if (sort == "alphabetical") {
    data$term <-factor(data$term, levels=unique(unlist(data[order(data$term, decreasing = TRUE), "term"])))
  }

  if (any(duplicated(data$term))) {
    message(paste0("Some variables are duplicated in your regression result.\nUsing cov_show = FALSE for covariate variables or facet for subgroup result.\n"))
  }


  p <- ggplot(data, aes(term, center, ymin = low, ymax = high, color = term)) +
    geom_linerange() + geom_point(size = 1.5, shape = 15) + theme(plot.title = element_text(size = 30),
                                                                  axis.text.x = element_text(hjust = 1, vjust = 0, size = 8), legend.position = "none") +
    coord_flip(ylim = limits) + geom_segment(aes(x = term, y = low * beyond2,
                                                 xend = term, yend = limits[2] * beyond2 + adj, group = term), arrow = arrow(type = "closed",
                                                                                                                             length = unit(0.15, "inches")), na.rm = TRUE) + geom_segment(aes(x = term,
                                                                                                                                                                                              y = high * beyond1, xend = term, yend = limits[1] * beyond1 - adj,
                                                                                                                                                                                              group = term), arrow = arrow(type = "closed", length = unit(0.15, "inches")),
                                                                                                                                                                                          na.rm = TRUE)
  if (is.null(title)) p<-p+labs(title=substitute(x))


  p <- switch(var_model,
              lm = p + ylab("coefficients(95%CI)")+geom_abline(intercept = 0, slope = 0, colour = "red", size = 0.1),
              glm = p + ylab("OR(95%CI)")+geom_abline(intercept = 1, slope = 0, colour = "red", size = 0.1),
              coxph = p + ylab("HR(95%CI)")+geom_abline(intercept = 1, slope = 0, colour = "red", size = 0.1))


  if ("reg_y" %in% class(x)) p<-p+facet_grid(.~y, scales = "free")
  if (all(c("group","level") %in% names(x))) p<-p+facet_grid(level~group, scales = "free")


  return(p)
}


utils::globalVariables(c("term", "center", "low", "high", "beyond1", "beyond2"))



