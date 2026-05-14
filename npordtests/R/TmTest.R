TmTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Terpstra-Magel Test"
  TEST <- "TM"


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
y.n <- NULL
TM=0
a=1
A<-list()

for (i in x.levels) {
  y.n[i] <- length(y[group==i])
}

for (i in x.levels)
 {A[[a]] <- y[group==i]
 a=a+1}

Xmat=expand.grid(A)

for (i in 1:nrow(Xmat))
  if (is.unsorted(Xmat[i,])==FALSE) TM=TM+1

N=prod(y.n)
l=0:k
Is=1:k
V=0

for (i in 1:(k-1)){
  Ic=combn(Is,i)
  NIc<-rbind(rep(0,ncol(Ic)),Ic)
  for (j in 1:k)
  {
    V1=1
    I=!colSums(j==Ic)
    for (s in 1:k){
      V1=V1*((y.n[s]-1)^I[s])}
    V2=1
    VV1=choose(2*(k-NIc[i+1,j]),k-NIc[i+1,j])/factorial(2*k-i)
    for (s in 1:i)
      V2=V2*choose(2*(NIc[s+1,j]-NIc[s,j]-1),(NIc[s+1,j]-NIc[s,j]-1))
    V=V+V1*(VV1*V2-(1/(factorial(k)^2)))
  }
}


ETM=N/factorial(k)
VTM=N*((1/factorial(k))*(1-1/factorial(k))+V)


Z=(TM-ETM)/sqrt(VTM)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", TM, "\n", sep = " ")
  cat("  Mean =", ETM, "\n", sep = " ")
  cat("  Variance =", VTM, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- TM
result$mean <- ETM
result$variance <- VTM
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
