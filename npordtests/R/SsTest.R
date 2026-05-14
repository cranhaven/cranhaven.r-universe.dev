SsTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Shan's S test"
  TEST <- "S"

 if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }

if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")
y = data[, dp[[2L]]]
r = rank(data[, dp[[2L]]])
group = data[, dp[[3L]]]
if (!is.factor(group)) stop("The group variable must be a factor.")
if (!is.numeric(y)) stop("The response must be a numeric variable.")

n <- length(y)
x.levels <- levels(factor(group))
p<-NROW(x.levels)
y.n <-r.n<-NULL
Eq1=Eq2=Eq3=S=nfak=sy.n=0


for (i in x.levels) {
  y.n[i] <- length(y[group==i])
}

sy.n=cumsum(y.n)
r.n=matrix(c(r,group,y),ncol=3,nrow=n)

for (i in 1:(p-1)) {
  for (j in (i+1):p) {
    nfak=nfak+y.n[i]*y.n[j]
    Eq1=Eq1+y.n[i]*choose(y.n[j],2)
    for (k in (sy.n[i]-y.n[i]+1):sy.n[i]){
      for (m in (sy.n[j-1]+1):sy.n[j]){
      S=S+(r.n[k,3]<r.n[m,3])*(r.n[m,1]-r.n[k,1])
      }
    }
  }
}




CovA=(2*n*n+n-1)/90
CovB=(-7*n*n-11*n-4)/360

for (i in 2:p) {
  for (j in 1:(i-1)) {
    Eq2=Eq2+y.n[i]*choose(y.n[j],2)
  }
}

for (i in 1:(p-2)) {
  for (j in (i+1):(p-1)) {
    for (l in (j+1):p) {
    Eq3=Eq3+y.n[i]*y.n[j]*y.n[l]
  }
}
}

ES=(n+1)/6*nfak
VS=((n*n+n)/12-(n+1)^2/36)*nfak+2*(Eq1+Eq2)*CovA+2*Eq3*CovB


Z=(S-ES)/sqrt(VS)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", S, "\n", sep = " ")
  cat("  Mean =", ES, "\n", sep = " ")
  cat("  Variance =", VS, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- S
result$mean <- ES
result$variance <- VS
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
