###################################################
#  Reference:
#
#  Carl Morris. "Was Ty Cobb ever a TRUE .400 hitter?"  ASA JSM Lecture, August 18, 1982. Cincinnati.
#
#  Ty Cobb's largest numbers of "atbats" were 605 in season 3 (1907), 591 in season 7 (1911), and 625
#     in season 20 (1924).
#  The "seasons" variable: [1:24] represents a pure "linear trend" effect within the fitted model. Ty
#     Cobb's highest "batavg" was 0.4196277 in 1911 (season 7.)
#  CMspl = Carl Morris' Piecewise Linear Spline term stays at 0 for seasons [1:6] then increases linearly [1:18]
#     in seasons [7:24]. Its three "Knots" thus occur in seasons: 1 (1905), 6 (1910) and 24 (1928).
#
#  This RXshrink demo() illustrates use of the eff.ridge() and RXpredict() functions and plots.
#
library(RXshrink)
data(tycobb)
str(tycobb)
#
# formula suitable for use with lm() for p=3 x-variables...
form <- batavg~atbats+seasons+CMspl
#
# Fit a generalized linear regression (GRR) model using eff.ridge()...
tyceobj <- eff.ridge(form, data=tycobb, steps=100)
# tyceobj
#     Only the 5 final lines of printed output are listed here...
#     ===========================================================
#     Most Likely UNRestricted Shrinkage Extent, mUnr = 1.039595
#     Corresponding -2*log(LikelihoodRatio) statistic = 0.0
#     Most Likely m-Value on the Lattice,        mClk = 1.04 <note: steps=100 above>
#     Smallest Observed -2*log(LikelihoodRatio), minC = 0.0003832262
#     dMSE Estimates = 0.01700621 0.9698978 0.973501 
#
plot(tyceobj)    # Show all 5 eff.ridge() TRACE Diagnostic plots...
#
# Display the first 3 "k-star" values for "knots"; final "knot" at m=4 then "k-star"=1...
rep(1,3)/tyceobj$dMSE
#
OLSpred <- RXpredict(tyceobj, data=tycobb, m=0)     # OLS fit occurs at m == 0
minMSEpred <- RXpredict(tyceobj, data=tycobb)       # m="minMSE" fit occurs here at m="1.0396"...
maxShrink <- RXpredict(tyceobj, data=tycobb, m=3)   # Intercept Only: other Coefficients are Zeros...
#
ym <- mean(tycobb$batavg)              # 0.3610738
ys <- sqrt(var(tycobb$batavg))         # 0.03848413
# Calculate value of batavg == 0.400 on the "cry" scale...
crx400 <- ( 0.4 - ym )/ys              # 1.011486  ...slightly greater than 1.00...
#
plot( tycobb$year, OLSpred$cry, ann = FALSE, type = "b") 
lines(tycobb$year, OLSpred$cryprd, lty=2, lwd=2, col="blue")
title(main="Ty Cobb's Batting Averages and Fitted Values",
  xlab="Year", ylab="Centered and Rescaled Batting Averages")
lines(tycobb$year, minMSEpred$cryprd, lty=3, lwd=2, col="limegreen")
abline(h = crx400, col = "red", lty = 2, lwd = 2)
abline(h = 0, col = "gray", lty = 1, lwd = 1)
# Additional plot() annotation...
text(x=1907, y=+1.2, labels="400 Batting", col="red")
text(x=1918, y=-1.5, labels="- o - Ty Cobb's Season Averages", col="black")
text(x=1918, y=-1.8, labels="- - - OLS fitted Averages", col="blue")
text(x=1918, y=-2.1, labels=". . . minMSE optimally biased Averages", col="limegreen") 
#
OLSmax <- max(OLSpred$yvecprd) # = 0.4000915 >> Yes, Ty Cobb was a "True 400-hitter in 1911... 
minMSEmax <- max(minMSEpred$yvecprd) # = 0.3994661 >> NO, not even in 1911 !!!
#
################## End of "tycobb" DEMO...
