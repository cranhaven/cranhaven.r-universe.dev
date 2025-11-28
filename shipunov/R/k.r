K <- function(x, y=NULL, data=NULL, mad=FALSE, na.rm=TRUE)
{
if(is(x, "formula"))
 {
 mf <- model.frame(formula=x, data=data)
 x <- mf[[2]]
 y <- mf[[1]]
 vals = split(y, x)
 x = vals[[1]]
 y = vals[[2]]
 }
if(na.rm)
 {
 x <- na.omit(x)
 y <- na.omit(y)
 }
if(!mad)
 {
 K <- ((mean(x) - mean(y))^2)/(var(x) + var(y))
 } else {
 K <- ((median(x) - median(y))^2)/(mad(x)^2 + mad(y)^2)
 }
class(K) <- "K"
K
}

## ===

print.K <- function(x, ...)
{
cat(x, "\n")
}

## ===

summary.K <- function(object, ..., num=2)
{
ssmd.levels <- c(0, 0.25, 0.5, 0.75, 1, 1.28, 1.645, 2, 3, 5)
levels <- ssmd.levels^2
magnitude <- c("No effect", "Extremely weak", "Very weak", "Weak", "Fairly weak", "Fairly moderate", "Moderate",
 "Fairly strong", "Strong", "Very strong", "Extremely strong")
effect <- magnitude[findInterval(object, levels) + 1]
p <- 1 - pnorm(sqrt(object/2))
res <- c("Lubischew's K"=round(object, num), Effect=effect, P=round(p, num))
noquote(res)
}
