Root1 <- function(phy, outgroup, select=1, ...) {
res <- try(ape::root(phy, outgroup, ...))
if (inherits(res, "try-error")) {
 res <- ape::root(phy, outgroup[select], ...)
 }
res
}
