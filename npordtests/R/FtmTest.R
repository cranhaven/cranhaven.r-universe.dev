FtmTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Ferdhiana, Terpstra and Magel Test"
  TEST <- "FTM"


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
S1=S2=S3=S4=FTM=0
a=1
A<-list()

for (i in x.levels) {
  y.n[i] <- length(y[group==i])
}

for (i in x.levels)
 {A[[a]] <- y[group==i]
 a=a+1}

Xmat=expand.grid(A)
Ymat=as.vector(1:k)
FTM=sum(cor(t(Xmat),Ymat,method = "kendall"))

EFTM=0

for (i1 in 1:(k-1))
  for (i2 in (i1+1):k)
    S1=S1+(y.n[i1]+y.n[i2]+1)/(y.n[i1]*y.n[i2])

for (i1 in 1:(k-2))
    S2=S2+(1/y.n[i1])*(choose(k,2)+(i1^2-(2*k-1)*i1)/2)


for (i1 in 1:(k-2))
  for (i2 in (i1+1):(k-1))
      S3=S3+(k-i2)/y.n[i2]

for (i1 in 1:(k-2))
  for (i2 in (i1+1):(k-1))
    for (i3 in (i2+1):k)
      S4=S4+1/y.n[i3]

N=prod(y.n)

VFTM=(2*N/(sqrt(3)*k*(k-1)))^2*(S1+2*S2-2*S3+2*S4)

Z=(FTM-EFTM)/sqrt(VFTM)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", FTM, "\n", sep = " ")
  cat("  Mean =", EFTM, "\n", sep = " ")
  cat("  Variance =", VFTM, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- FTM
result$mean <- EFTM
result$variance <- VFTM
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
