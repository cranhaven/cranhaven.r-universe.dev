Adj.Rand <- function (cl1, cl2, ...) 
{
 tab <- table(cl1, cl2, ...)
 .choose2 <- function(n) n * (n - 1)/2
 .s <- function(v) sum(.choose2(v))
 marg1 <- rowSums(tab)
 marg2 <- colSums(tab)
 n <- sum(tab)
 prod <- .s(marg1) * .s(marg2) / .choose2(n)
 num <- .s(tab) - prod
 den <- 0.5 * (.s(marg1) + .s(marg2)) - prod
 num/den
}
