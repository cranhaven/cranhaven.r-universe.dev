JtTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Jonckheere-Terpstra Test"
  TEST <- "JT"

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

n <- length(y)
x.levels <- levels(factor(group))
k=NROW(x.levels)
JT =0;s=1
y.n <- NULL

for (i in x.levels) {
  y.n[i] <- length(y[group==i])
}

for (i in x.levels[1:k-1])
 {s=s+1
  for (j in x.levels[s:k]) {
A <- rank(y[group==i|group==j])
JT=JT+sum(A[(y.n[i]+1):(y.n[i]+y.n[j])])-NROW(y[group==j])*(NROW(y[group==j])+1)/2
}}

a=sum(y.n^2);
b=sum((y.n^2)*(2*y.n+3))
EJT=(n^2-a)/4
VJT=((n^2*(2*n+3))-b)/72
Z=(JT-EJT)/sqrt(VJT)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", JT, "\n", sep = " ")
  cat("  Mean =", EJT, "\n", sep = " ")
  cat("  Variance =", VJT, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- JT
result$mean <- EJT
result$variance <- VJT
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
