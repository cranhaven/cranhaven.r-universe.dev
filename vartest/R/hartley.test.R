hartley.test <- function (formula, data, size = "mean", alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Hartley's Maximum F-Ratio Test"
  if (na.rm) {
    completeObs <- complete.cases(data)
    data <- data[completeObs, ]
  }
  
  
  if (any(colnames(data) == dp[[3L]]) == FALSE) 
    stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  
  
  if (any(colnames(data) == dp[[2L]]) == FALSE) 
    stop("The name of response variable does not match the variable names in the data.")
  
  y = data[[dp[[2L]]]]
  
  group = data[[dp[[3L]]]]
  
  if (!(is.factor(group) | is.character(group))) 
    stop("The group variable must be a factor or a character.")
  
  if (is.character(group)) 
    group <- as.factor(group)
  
  if (!is.numeric(y)) 
    stop("The response must be a numeric variable.")
  
  if (is.list(y)) {
    if (length(y) < dp[[2L]])
      stop("'y' must be a list with at least 2 elements") 
  }
  
  n <- length(y)
  x.levels <- levels(factor(group))
  k <- length(x.levels)
  ni<- as.numeric(tapply(y, group, length))
  
  
  vars <- tapply(y, group, var)
  vars.max <- max(vars)
  vars.min <- min(vars)
  
  if (size == "mean"){ ni.optimum <- mean(ni)
  }else if (size == "harmonic"){ ni.optimum <- harmonic.mean(ni)
  }else if (size == "maxn"){ ni.optimum <- max(ni)
  }else if (size == "minvar"){ ni.optimum <- ni[which.min(vars)]
  }else stop("Please correct size argument.")
  
  
  if(any(ni!=n/k)) warning("Hartley's maximum F-ratio test may not be precise for imbalanced designs.")
  
  H.test<- vars.max/vars.min
  
  df<- ni.optimum-1
  
  p.value<- pmaxFratio(H.test, df, k, lower.tail = F)
  
  if (verbose) {
  print(structure(list(statistic = c("F-max" = H.test), parameter = c("df" = df), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }
  result <- list()
  result$statistic <- H.test
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "vht"
  invisible(result)
}
