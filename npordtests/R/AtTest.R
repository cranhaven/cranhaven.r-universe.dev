AtTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Adaptive Test"
  TEST <- "AT"

 if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }

if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")
y = data[, dp[[2L]]]
group = data[, dp[[3L]]]
if (!is.factor(group)) stop("The group variable must be a factor.")
if (!is.numeric(y)) stop("The response must be a numeric variable.")


' compute S1 and S2 '
q975=quantile(y,0.975)
q500=quantile(y,0.5)
q025=quantile(y,0.025)
q875=quantile(y,0.875)
q125=quantile(y,0.125)
S1=(q975-q500)/(q500-q025)
S2=(q975-q025)/(q875-q125)

if ((S1<=0.6) & (S2>=1))
  res<-LsTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = FALSE)
if ((S1<=2) & (S1>0.6)){
  if ((S2<=1.5) & (S2>=1))
    res<-StTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = FALSE)
  if ((S2<=2) & (S2>1.5))
    res<-WsTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = FALSE)
  if (S2>2)
    res<-LtTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = FALSE)
}
if ((S1>2) & (S2>=1))
  res<-RsTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", res$statistic, "\n", sep = " ")
  cat("  Mean =", res$mean, "\n", sep = " ")
  cat("  Variance =", res$variance, "\n", sep = " ")
  cat("  Z =", res$Z, "\n", sep = " ")
  cat("  Asymp. p-value =", res$p.value, "\n\n", sep = " ")
  cat(if (res$p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- res$statistic
result$mean <- res$mean
result$variance <- res$variance
result$Z <- res$Z
result$p.value <- res$p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
