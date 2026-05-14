LsTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "LS test"
  TEST <- "LS"

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
k<-NROW(x.levels)
aLS=0;LLS=0;
Cvar=0;Cort=0
a=1
b<- y.n <- NULL

for (i in x.levels) {
  y.n[i] <- length(y[group==i])
}

for (i in 1:k) {
  b[i] = i-1
  Cort=Cort+y.n[i]*(i-1)/n
}

for (i in 1:k) {
Cvar=Cvar+y.n[i]*(b[i]-Cort)^2
}

r <- rank(y)

' Calculating aLS '
for (i in 1:n){
  if (r[i]>((n+1)/2))
    aLS[i]=r[i]-(n+1)/2
  else
    aLS[i]=0
}

for (i in 1:k) {
  for (j in 1:y.n[i]){
    LLS=LLS+b[i]*aLS[a]
    a=a+1
}}

ELS=n*Cort*mean(aLS)
VLS=Cvar*var(aLS)
Z=(LLS-ELS)/sqrt(VLS)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", LLS, "\n", sep = " ")
  cat("  Mean =", ELS, "\n", sep = " ")
  cat("  Variance =", VLS, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- LLS
result$mean <- ELS
result$variance <- VLS
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
