levene.test<-function (formula, data, center = "mean", deviation = "absolute", trim.rate = 0.25, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Levene's Test"
  
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
  
  if(center=="mean"){y.means <- tapply(y, group, mean)
  }else if(center=="median"){y.means <- tapply(y, group, median)
  }else if(center=="trim.mean"){y.means <- tapply(y, group, mean, trim = trim.rate)
  }else {stop("Please correct center option.")
  }
  y.mean <- mean(y)
  
  z <- list()
  
  for(i in x.levels) {
  if(deviation=="absolute"){ z[[i]] <- abs(y[group==i]-y.means[i])
  }else if(deviation=="squared"){ z[[i]] <- (y[group==i]-y.means[i])^2
  }else stop("Please correct deviation type.") 
  }
  
  z.means <- unlist(lapply(1:k, function(i) mean(z[[i]])))
  z.mean <- mean(unlist(lapply(1:k, function(i) z[[i]])))
 
  z.n <- unlist(lapply(1:k, function(i) length(z[[i]])))
  
  nom <- (n-k)*sum(z.n*((z.means-z.mean)^2))
  
  z_gstotal <- unlist(lapply(1:k, function(i) sum((z[[i]]-mean(z[[i]]))^2)))
 
  denom <- (k-1)*sum((z_gstotal))
  
  Ltest= nom/denom
  df1 = k-1
  df2 = n-k
  
 p.value = pf(Ltest, df1, df2, lower.tail = F)
  
  if (verbose) {
  print(structure(list(statistic = c("F" = Ltest), parameter = c("num df" = df1, "denom df" = df2), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }
  result <- list()
  result$statistic <- Ltest
  result$parameter <- c(df1, df2)
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "vht"
  invisible(result)
}