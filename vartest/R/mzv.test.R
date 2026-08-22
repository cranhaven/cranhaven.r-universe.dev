mzv.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Modified Z Variance Test"
  
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
  
  y.variance <- tapply(y, group, var)
  y.means <- tapply(y, group, mean)  

  ni<- tapply(y, group, length)
  
  g<- list()
  for (i in x.levels) {
    g[[i]] <- (y[group==i]-y.means[i])/sqrt(((ni[i]-1)/ni[i])*y.variance[i])  
  }
  
  g_total<- unlist(lapply(1:k, function(i) sum(g[[i]]^4)))


  K<- g_total/(ni-2)
  
  ci <- 2*(((2.9+0.2/ni)/mean(K))^((1.6*(ni-1.8*K+14.7))/ni))
  
  z<- list()
  for (i in x.levels) {
    z[[i]]<- sum((y[group==i]-y.means[i])^2)
  }
  
  mse_nom <- sum(unlist(z)) 
  
  MSE<- mse_nom/(n-k)
  
  zi_left<- sqrt((ci*(ni-1)*y.variance)/MSE)
  zi_right<- sqrt(ci*(ni-1)-(ci/2)) 
  
  zi<- as.numeric(zi_left - zi_right)

vtest<- sum(zi^2)/(k-1)

df1 = k-1
df2 = Inf
  
  p.value = pf(vtest, df1, df2, lower.tail = F)

if (verbose) {
  print(structure(list(statistic = c("F" = vtest), parameter = c("num df" = df1, "denom df" = df2), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
}
result <- list()
result$statistic <- vtest
result$parameter <- c(df1, df2)
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "vht"
invisible(result)
}
