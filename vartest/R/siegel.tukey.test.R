siegel.tukey.test<-function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Siegel Tukey Test"
  
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
  
  a <- rep(seq(ceiling(n / 4)), each=2)     
  b <- rep(c(0, 1), ceiling(n)/4)
  suppressWarnings(
    rankone <- c(1, (a * 4 + b))[1:ceiling(n / 2)]
  )
  suppressWarnings(
    ranktwo <- rev(c(a * 4 + b - 2)[1:floor(n / 2)])
  )
  
  rank <- c(rankone, ranktwo)
  T <- tapply(rank, group, sum)  
  
  t <- table(y)
  correction <- 1 - sum(t^3 - t) / (n^3 - n)
  
  ani<-rank
  
  a.ort<- (1/n)*sum(ani)  
  v.square<- (1/(n-1))* sum((ani-a.ort)^2)
  Ai<- tapply(ani, group, mean)
  
  
  STtest<- sum(ni*((Ai-a.ort)^2)/v.square)/correction
  
  
  
  df<- k-1
  p.value<- pchisq(STtest, df, lower.tail = F)
  
  if (verbose) {
  print(structure(list(statistic = c("X-squared" = STtest), parameter = c("df" = df), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }
  result <- list()
  result$statistic <- STtest
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "vht"
  invisible(result)
}
