cochrans.test <- function (formula, data,alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Cochran's C Test"
  
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
  
  n <- length(y)
  x.levels <- levels(factor(group))
  k <- length(x.levels)
  ni <- n/k   
  
  vars <- tapply(y, group, var)
  vars.sum <- sum(vars)
  vars.max <- max(vars)
  
  C <- vars.max/vars.sum  
  
  f <- 1/((1/C-1)/(k-1))	  
  
  pval <- pf(f, ni-1, (ni-1)*(k-1), lower.tail=F)*k
  
  if (pval>1){pval <- 1}
  
  df1 <- ni-1
  df2 <- (ni-1)*(k-1)
  
  
  if (verbose) {
  print(structure(list(statistic = c("F" = C), parameter = c("num df" = df1, "denom df" = df2), 
                 p.value = pval, method = METHOD, data.name = DNAME), class = "htest"))
  }
  
  result <- list()
  result$statistic <- C
  result$parameter <- c(df1, df2)
  result$p.value <- pval
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "vht"
  invisible(result)
  
}

