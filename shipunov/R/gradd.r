Gradd <- function(model2var, data2var, spacing=75, what="points",
 trnsp=0.2, pch=16, cex=0.8, lwd=2, lty=2, lcol="grey", palette=NULL,
 type="ids", User.Predict=function(model2var, X) {}, ...) {
 what <- match.arg(what, choices=c("points", "lines"))
 S <- data.frame(apply(data2var, 2, function(.x) seq(range(.x)[1], range(.x)[2], length.out=spacing)))
 X <- expand.grid(S)
 #
 if (type == "ids") newids <- predict(model2var, X) else
 if (type == "lda") newids <- predict(model2var, X)$class else
 if (type == "tree") newids <- predict(model2var, X, type="class") else
 if (type == "user") newids <- User.Predict(model2var, X) else stop("Unknown model type")
 #
 if (!is.factor(newids)) newids <- as.factor(newids)
 if (is.null(palette)) cols <- as.integer(newids) else cols <- palette[as.integer(newids)]
 #
 if (what == "points") points(X, col=adjustcolor(cols, alpha.f=trnsp), pch=pch, cex=cex, ...)
 #
 if (what == "lines") {
 Z <- matrix(as.integer(newids), nrow=spacing)
 K <- length(unique(newids))
 contour(S[, 1], S[, 2], Z, add=TRUE, drawlabels=FALSE, levels=1:(K-1)+0.5, lwd=lwd, lty=lty, col=lcol, ...)
 }
}
