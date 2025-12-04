## ---- include=FALSE-----------------------------------------------------------
library(itsadug)
infoMessages("off")

## -----------------------------------------------------------------------------
library(itsadug)
library(mgcv)
data(simdat)

## Not run: 
# Model with random effect and interactions:
m1 <- bam(Y ~ Group + te(Time, Trial, by=Group)
    + s(Time, Subject, bs='fs', m=1, k=5),
    data=simdat)
# Simple model with smooth:
m2 <-  bam(Y ~ Group + s(Time, by=Group)
    + s(Subject, bs='re')
    + s(Subject, Time, bs='re'),
    data=simdat)

## ---- results='asis'----------------------------------------------------------
gamtabs(m1, type='html')

## ---- results='asis'----------------------------------------------------------
gamtabs(m2, type='html')

## ---- fig.width=8, fig.height=4-----------------------------------------------
par(mfrow=c(1,2), cex=1.1)

pvisgam(m1, view=c("Time", "Trial"), select=1,
        main="Children", zlim=c(-12,12))
pvisgam(m1, view=c("Time", "Trial"), select=2,
        main="Adults", zlim=c(-12,12))

## ---- fig.width=8, fig.height=4-----------------------------------------------
par(mfrow=c(1,2), cex=1.1)

fvisgam(m1, view=c("Time", "Trial"), cond=list(Group="Children"),
        main="Children", zlim=c(-12,12), rm.ranef=TRUE)
fvisgam(m1, view=c("Time", "Trial"), cond=list(Group="Adults"),
        main="Adults", zlim=c(-12,12), rm.ranef=TRUE)


## ---- fig.width=8, fig.height=4-----------------------------------------------
par(mfrow=c(1,2), cex=1.1)

plot(m2, select=1, shade=TRUE, rug=FALSE, ylim=c(-15,10))
abline(h=0)
plot(m2, select=2, shade=TRUE, rug=FALSE, ylim=c(-15,10))
abline(h=0)

## ---- fig.width=4, fig.height=4-----------------------------------------------
par(mfrow=c(1,1), cex=1.1)

# Get model term data:
st1 <- get_modelterm(m2, select=1)
st2 <- get_modelterm(m2, select=2)

# plot model terms:
emptyPlot(2000, c(-15,10), h=0,
    main='s(Time)', 
    xmark = TRUE, ymark = TRUE, las=1)
plot_error(st1$Time, st1$fit, st1$se.fit, shade=TRUE)
plot_error(st2$Time, st2$fit, st2$se.fit, shade=TRUE, col='red', lty=4, lwd=2)

# add legend:
legend('bottomleft',
  legend=c('Children', 'Adults'),
  fill=c(alpha('black'), alpha('red')),
  bty='n')

## ---- fig.width=8, fig.height=4-----------------------------------------------
par(mfrow=c(1,2), cex=1.1)

plot_smooth(m1, view="Time", cond=list(Group="Children"),
    rm.ranef=TRUE, ylim=c(-6,10))
plot_smooth(m1, view="Time", cond=list(Group="Adults"),
    col="red", rug=FALSE, add=TRUE,
    rm.ranef=TRUE)

# or alternatively:
plot_smooth(m1, view="Time", plot_all="Group",
    rm.ranef=TRUE)

## ---- fig.width=4, fig.height=4-----------------------------------------------
par(mfrow=c(1,1), cex=1.1)

plot.gam(m1, select=4, all.terms=TRUE, rug=FALSE)

## ---- fig.width=8, fig.height=4, results='hold'-------------------------------
coefs <- get_coefs(m1)
coefs

par(mfrow=c(1,2), cex=1.1)

b <- barplot(coefs[,1], beside=TRUE, 
             main="Parametric terms",
             ylim=c(0,5))
errorBars(b, coefs[,1], coefs[,2], xpd=TRUE)

# Note that the effect of Group is a *difference* estimate
# between intercept (=GroupChildren) and Group Adults

b2 <- barplot(coefs[1,1], beside=TRUE, 
             main="Estimate for Group",
             ylim=c(0,5), xlim=c(0.1,2.5))
mtext(row.names(coefs), at=b, side=1, line=1)
abline(h=coefs[1,1], lty=2)
rect(b[2]-.4, coefs[1,1], b[2]+.4, coefs[1,1]+coefs[2,1],
     col='gray')
errorBars(b, coefs[,1]+c(0,coefs[1,1]), coefs[,2], xpd=TRUE)

## ---- fig.width=4, fig.height=4-----------------------------------------------
pp <- plot_parametric(m1, pred=list(Group=c("Children", "Adults")) )
pp

## ---- fig.width=8, fig.height=4-----------------------------------------------
par(mfrow=c(1,2), cex=1.1)
plot(m2, select=3)
plot(m2, select=4)

## ---- fig.width=4, fig.height=4-----------------------------------------------
pp <- get_random(m2)

emptyPlot(range(pp[[1]]), range(pp[[2]]), h=0,v=0,
     xlab='Random intercepts', ylab='Random slopes',
     main='Correlation')

text(pp[[1]], pp[[2]], labels=names(pp[[1]]), 
     col='steelblue', xpd=TRUE)

## ---- fig.width=8, fig.height=4-----------------------------------------------
par(mfrow=c(1,2), cex=1.1)

inspect_random(m1, select=3, main='s(Time, Subject)')

children <- unique(simdat[simdat$Group=="Children", "Subject"])
adults   <- unique(simdat[simdat$Group=="Adults", "Subject"])

inspect_random(m1, select=3, main='Averages', 
      fun=mean, 
      cond=list(Subject=children))
inspect_random(m1, select=3, 
      fun=mean, cond=list(Subject=adults),
      add=TRUE, col='red', lty=5)

# add legend:
legend('bottomleft',
  legend=c('Children', 'Adults'),
  col=c('black', 'red'), lty=c(1,5),
  bty='n')

