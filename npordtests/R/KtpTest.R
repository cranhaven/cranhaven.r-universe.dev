KtpTest <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "KTP Test"
  TEST <- "KTP"


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
S1=S2=KTP=0
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
KTP=sum(cor(t(Xmat),Ymat,method = "spearman"))

EKTP=0

for (i1 in 1:(k-1))
  for (i2 in (i1+1):k)
    S1=S1+((i2-i1)^2*(y.n[i1]+y.n[i2]+1))/(12*y.n[i1]*y.n[i2])

for (i1 in 1:(k-2))
  for (i2 in (i1+1):(k-1))
    for (i3 in (i2+1):k)
      S2=S2+(((i2-i1)*(i3-i1)/y.n[i1])+((i3-i2)*(i1-i2)/y.n[i2])+((i1-i3)*(i2-i3)/y.n[i3]))/6

S=unname(S1)+unname(S2)
N=prod(y.n)

VKTP=S*144*N*N/(k^2*(k^2-1)^2)

Z=(KTP-EKTP)/sqrt(VKTP)

p.value=1-pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


if (verbose) {
  cat("---------------------------------------------------------","\n", sep = " ")
  cat("  Test :", METHOD, "\n", sep = " ")
  cat("  data :", DNAME, "\n\n", sep = " ")
  cat("  Statistic =", KTP, "\n", sep = " ")
  cat("  Mean =", EKTP, "\n", sep = " ")
  cat("  Variance =", VKTP, "\n", sep = " ")
  cat("  Z =", Z, "\n", sep = " ")
  cat("  Asymp. p-value =", p.value, "\n\n", sep = " ")
  cat(if (p.value > alpha) {"  Result : Null hypothesis is not rejected."}
      else {"  Result : Null hypothesis is rejected."}, "\n")
  cat("---------------------------------------------------------","\n\n", sep = " ")
}

result <- list()
result$statistic <- KTP
result$mean <- EKTP
result$variance <- VKTP
result$Z <- Z
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD
result$data <- data
result$formula <- formula
attr(result, "class") <- "owt"
invisible(result)
}
