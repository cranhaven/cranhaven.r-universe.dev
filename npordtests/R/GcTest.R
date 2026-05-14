GcTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE,c=2) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Gaur's Gc Test"
  TEST <- "Gc"


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
Gc=0

for (i in x.levels) {
  y.n[i] <- length(y[group==i])
}

for (i in 1:(k-1))
{
  Fgh=0
  Xg=combn(y[group==i],c)
  Xh=combn(y[group==(i+1)],c)

  for (a in 1:ncol(Xg))
    for (b in 1:ncol(Xh))
    {
      if (max(Xg[,a])<=min(Xh[,b])) Fgh=Fgh+1
      if (min(Xg[,a])>=max(Xh[,b])) Fgh=Fgh-1
    }
  Gc=Gc+(i*(k-i)/(2*k))*Fgh/(choose(y.n[i],c)*choose(y.n[i+1],c))
}

EGc=0
Dc1=0

for (i in c:(2*c-1))
  for (j in c:(2*c-1))
    Dc1=Dc1+choose((2*c-1),i)*choose((2*c-1),j)/(choose((4*c-2),i+j)*(4*c-1))

Dc=-1+4*Dc1

SIGMA=matrix(0,nrow=k-1,ncol=k-1)
FAC=(factorial(c-1)*factorial(c)/factorial(2*c-1))^2

for (g in 1:(k-1))
  SIGMA[g,g]=FAC*(n/y.n[g]+n/y.n[g+1])*Dc

for (g in 1:(k-2))
  SIGMA[g,g+1]=-FAC*(n/y.n[g+1])*Dc

for (g in 2:(k-1))
  SIGMA[g,g-1]=-FAC*(n/y.n[g])*Dc

W=matrix(0,nrow=1,ncol=k-1)

for (g in 1:(k-1))
  W[g]=g*(k-g)/(2*k)

VGc=W%*%SIGMA%*%t(W)


Z=(Gc-EGc)/sqrt(VGc)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", Gc, "\n", sep = " ")
  cat("  Mean =", EGc, "\n", sep = " ")
  cat("  Variance =", VGc, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- Gc
result$mean <- EGc
result$variance <- VGc
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
