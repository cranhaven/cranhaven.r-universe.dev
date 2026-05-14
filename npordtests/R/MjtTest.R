MjtTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Modified Jonckheere-Terpstra Test"
  TEST <- "MJT"

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
m=choose(k,2)
VUS=EMJT=MJT=0;nc=a=COV=0
y.n <- NULL
NC=matrix(0, nrow=m,ncol=3)

for (i in x.levels) {
  y.n[i] <- length(y[group==i])
}

for (i in 1:(k-1))
 for (j in (i+1):k) {
   a=a+1
A <- rank(y[group==i|group==j])
MJT=MJT+(j-i)*(sum(A[(y.n[i]+1):(y.n[i]+y.n[j])])-NROW(y[group==j])*(NROW(y[group==j])+1)/2)
EMJT=EMJT+(j-i)*y.n[i]*y.n[j]/2
VUS=VUS+(j-i)^2*y.n[i]*y.n[j]*(y.n[i]+y.n[j]+1)/12
NC[a,]=c(i,j,(j-i))
 }
a=0;c=0
for (i in 1:(m-1))
  for (j in (i+1):m) {
    c=2*NC[i,3]*NC[j,3]
    if (sum((NC[i,1]==NC[j,1]))) {COV=COV+c*y.n[NC[i,1]]*y.n[NC[i,2]]*y.n[NC[j,2]]/12}
    if (sum((NC[i,2]==NC[j,2]))) {COV=COV+c*y.n[NC[i,1]]*y.n[NC[i,2]]*y.n[NC[j,2]]/12}
    if (sum((NC[i,1]==NC[j,2]))) {COV=COV-c*y.n[NC[i,1]]*y.n[NC[i,2]]*y.n[NC[j,2]]/12}
    if (sum((NC[i,2]==NC[j,1]))) {COV=COV-c*y.n[NC[i,1]]*y.n[NC[i,2]]*y.n[NC[j,2]]/12}
  }
VMJT=VUS+COV

Z=(MJT-EMJT)/sqrt(VMJT)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", MJT, "\n", sep = " ")
  cat("  Mean =", EMJT, "\n", sep = " ")
  cat("  Variance =", VMJT, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- MJT
result$mean <- EMJT
result$variance <- VMJT
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
