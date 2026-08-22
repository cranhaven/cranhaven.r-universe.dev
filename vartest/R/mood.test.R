mood.test<-function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Mood Test"
  
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
  y <- y - tapply(y,group,median)[group]
  order<- order(y)
  y<- y[order]  
  group<- group[order]
  
  if (!(is.factor(group) | is.character(group))) 
    stop("The group variable must be a factor or a character.")
  if (is.character(group)) 
    group <- as.factor(group)
  if (!is.numeric(y)) 
    stop("The response must be a numeric variable.")
  
  n <- length(y)
  x.levels <- levels(factor(group))
  k <- length(x.levels)
  ni<- tapply(y, group, length)
  rank<- rank(y)
  ani<- (rank-(n+1)/2)^2
  a.ort<- (1/n)*sum(ani)  
  v.square<- (1/(n-1))* sum((ani-a.ort)^2)
  Ai<- tapply(ani, group, mean)
  
  
  Mtest<- sum(ni*((Ai-a.ort)^2)/v.square)
  
  df<- k-1
  p.value<- pchisq(Mtest, df, lower.tail = F)
  
  if (verbose) {
  print(structure(list(statistic = c("X-squared" = Mtest), parameter = c("df" = df), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }
  result <- list()
  result$statistic <- Mtest
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "vht"
  invisible(result)
}
