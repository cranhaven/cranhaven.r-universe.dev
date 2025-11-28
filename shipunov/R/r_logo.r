R.logo <- function(x, y, col.e="#B8BABF", col.l="#1E63B5", cex=12)
{
coeff.x <- abs(diff(par("usr")[1:2])) / 0.864
coeff.y <- abs(diff(par("usr")[3:4])) / 0.864
e1.x <- x - (0.05 * cex/12 * coeff.x)
e1.y <- y + (0.07 * cex/12 * coeff.y)
e1.w <- cex/30 * coeff.x
e1.h <- cex/40 * coeff.y
e2.x <- e1.x + (0.03 * cex/12 * coeff.x)
e2.y <- e1.y - (0.02 * cex/12 * coeff.y)
e2.w <- cex/44 * coeff.x
e2.h <- cex/60 * coeff.y
Ell(e1.x, e1.y, width=e1.w, height=e1.h, col=col.e, border=NA)
Ell(e2.x, e2.y, width=e2.w, height=e2.h, col="white", border=NA)
text(x, y, "R", font=2, cex=cex, col=col.l)
}

