Rro.test <- function(x1, y1)
{
x1 <- x1[!is.na(x1)]
y1 <- y1[!is.na(y1)]
nx <- length(x1)
ny <- length(y1)
ux1 <- numeric(nx)
uy1 <- numeric(ny)
for (i1 in 1:nx)
 {
 for (i2 in 1:ny)
 {
 ux1[i1] <- ux1[i1] + 0.5 * sign(x1[i1] - y1[i2]) + 0.5
 uy1[i2] <- uy1[i2] + 0.5 * sign(y1[i2] - x1[i1]) + 0.5
 }
 }
mux1 <- mean(ux1)
muy1 <- mean(uy1)
sux1 <- sum(ux1)
suy1 <- sum(uy1)
dux1 <- ux1 - mux1
duy1 <- uy1 - muy1
Vux1 <- sum(dux1^2)
Vuy1 <- sum(duy1^2)
ufp <- (sux1 - suy1)/2/sqrt(Vux1 + Vuy1 + mux1 * muy1)
p <- (1-pnorm(abs(ufp))) * 2
return(c("z"=ufp, "p.value"=p))
}

## ===

pairwise.Rro.test <- function(x, g, p.adjust.method="BH")
{
p.adjust.method <- match.arg(p.adjust.method)
DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
g <- factor(g)
METHOD <- "Robust rank order test"
compare.levels <- function(i, j)
 {
 xi <- x[as.integer(g) == i]
 xj <- x[as.integer(g) == j]
 Rro.test(xi, xj)["p.value"]
 }
PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
ans <- list(method=METHOD, data.name=DNAME, p.value=PVAL, p.adjust.method=p.adjust.method)
class(ans) <- "pairwise.htest"
ans
}
