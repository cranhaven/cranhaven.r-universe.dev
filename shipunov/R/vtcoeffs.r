VTcoeffs <- function(table, correct=FALSE, ...)
{
chisquared <- unname(chisq.test(table, correct=correct, ...)$statistic)
phisquared <- chisquared / sum(table)
phisquaredcorr <- max(0, phisquared - (ncol(table)-1)*(nrow(table)-1)/sum(table))
phi <- sqrt(phisquared)
phicorr <- sqrt(phisquaredcorr)
cramersdf <- min(ncol(table)-1, nrow(table)-1)
cramersv <- phi * cramersdf^(-1/2)
cramersvcorr <- phicorr * cramersdf^(-1/2)
tschuprowst <- phi * ((ncol(table)-1)*(nrow(table)-1))^(-1/4)
tschuprowstcorr <- phicorr * ((ncol(table)-1)*(nrow(table)-1))^(-1/4)
##
cramerslevels1 <- c(0.1, 0.3, 0.5)
cramerslevels2 <- c(0.07, 0.21, 0.35)
cramerslevels3 <- c(0.06, 0.17, 0.29)
cramerslevels4 <- c(0.05, 0.15, 0.25)
cramerslevels5 <- c(0.05, 0.13, 0.22)
if (cramersdf==1) { levels <- cramerslevels1 }
if (cramersdf==2) { levels <- cramerslevels2 }
if (cramersdf==3) { levels <- cramerslevels3 }
if (cramersdf==4) { levels <- cramerslevels4 }
if (cramersdf==5) { levels <- cramerslevels5 }
magnitude <- c("negligible", "small", "medium", "large")
cramersvmagnitude <- ifelse(cramersdf > 5, "", magnitude[findInterval(cramersv, levels) + 1])
cramersvcorrmagnitude <- ifelse(cramersdf > 5, "", magnitude[findInterval(cramersvcorr, levels) + 1])
##
coefficients <- c("Cramer's V", "Cramer's V (corrected)", "Tschuprow's T", "Tschuprow's T (corrected)")
values <- c(cramersv, cramersvcorr, tschuprowst, tschuprowstcorr)
comments <- c(cramersvmagnitude, cramersvcorrmagnitude, "", "")
##
res <- data.frame(coefficients, values, comments)
return(res)
}
