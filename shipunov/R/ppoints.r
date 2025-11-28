Points <- function(x, y, pch=1, centers=FALSE, scale=1, cex.min=1, col=1, na.omit=TRUE, plot=TRUE, ...)
{
 M.s <- na.omit(cbind(x, y))
 if (na.omit) {
 s <- paste(M.s[, 1], M.s[, 2])
 } else {
 s <- paste(x, y)
 }
 TAB.s <- table(s)
 TAB.x <- as.numeric(unlist(strsplit(names(TAB.s), " "))[seq(1, 2*length(TAB.s), by=2)])
 TAB.y <- as.numeric(unlist(strsplit(names(TAB.s), " "))[seq(2, 2*length(TAB.s), by=2)])
 addsize <- (as.numeric(cut(TAB.s, 7)) - 1) * scale
 if(plot) {
 points(TAB.x, TAB.y, cex=cex.min + addsize, pch=pch, col=col, ...)
 if (centers) points(TAB.x, TAB.y, cex=1, pch=".", col=col)
 }
 invisible(as.numeric(Recode(s, names(TAB.s), TAB.s)))
}

## ===

PPoints <- function(groups, x, y, cols=as.numeric(groups), pchs=as.numeric(groups), na.omit.all=TRUE, ...)
{
 if (na.omit.all) {
 D <- na.omit(data.frame(groups=groups, x=x, y=y))
 x <- D$x ; y <- D$y ; groups <- D$groups
 }
 if (!is.factor(groups)) stop("Grouping variable must be a factor")
 n <- nlevels(groups)
 a <- as.numeric(groups)
 if (length(pchs) == 1) pchs <- rep(pchs, length(groups))
 if (length(cols) == 1) cols <- rep(cols, length(groups))
 na.omit <- !na.omit.all # to save resources
 res <- numeric(length(x))
 for (i in 1:n) {
 res[a == i] <- Points(x[a==i], y[a==i], col=(cols[a==i]), pch=(pchs[a==i]), na.omit=na.omit, ...)
 }
 invisible(res)
}
