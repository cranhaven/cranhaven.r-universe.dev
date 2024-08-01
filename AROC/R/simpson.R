simpson <-
function(ROC, set.p) {
	l.set.p <- length(set.p)
	integral <- (set.p[l.set.p] - set.p[1])/(l.set.p - 1)/3*(ROC[1] + ROC[l.set.p] + 4*sum(ROC[seq(2,l.set.p - 1, by = 2)]) + 2*sum(ROC[seq(3, l.set.p - 2, by = 2)]))
}
