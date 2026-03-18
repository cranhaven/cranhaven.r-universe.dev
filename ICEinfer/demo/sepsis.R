# input the sepsis data.frame...
data(sepsis)
str(sepsis)

# Create Non-Linear MB y-Oucome variable with the default "eta" ratio = 3 + 2 * sqrt(2)
NLMB <- ICEpref(sepsis$icu, sepsis$qalypres, sepsis$totcost, lambda=50000, beta=0.6)   
                                                           
summary(NLMB) # Non-Linear Monetary Benefit calculations with Decreasing Returns (beta < 1)

sepsis8 <- data.frame(cbind(epref=NLMB$pref, sepsis))      # Augmented data.frame...

sub1 <- subset(sepsis8, icu == 1, select = c(epref, totcost, qalypres))
sub0 <- subset(sepsis8, icu == 0, select = c(epref, totcost, qalypres))
                      
par(mfrow=c(2,1))
hist(sub1$epref, breaks=35, xlim=c(-2000,2500), main="Patients in ICU_1", xlab = "epref")
hist(sub0$epref, breaks=35, xlim=c(-2000,2500), main="Patients in ICU_0", xlab = "epref")

par(mfrow=c(1,1))
plot(ICEomega(beta=.6))

form <- epref ~ icu + age + orgfails + apache   
epOLS <- lm(form, sepsis8)
summary(epOLS)

plot(sepsis8$epref, epOLS$fitted.values, col="blue", xlab="EconPref",
    ylab="OLS epref Prediction", main="Predictions of Economic Preferences",
    sub="icu is not an important predictor of epref")

icuOLS <- data.frame(cbind(sepsis8$icu, sepsis8$epref, epOLS$fitted.values))
names(icuOLS) <- c("icu", "epref", "fit")
icu0 <- subset(icuOLS, icu==0)
icu1 <- subset(icuOLS, icu==1)
form2 <- epref ~ fit
lm0 <- lm(form2, icu0)
lm1 <- lm(form2, icu1)

points(icu0$epref, icu0$fit, col="red")
abline(coef=c(lm0$coefficients[1], lm0$coefficients[2]), col="red")
abline(coef=c(lm1$coefficients[1], lm1$coefficients[2]), col="blue")
text(-1000, 2000, "icu = 0 Patients", col= "red",  cex = 1.2)
text( 1500,-1500, "icu = 1 Patients", col= "blue", cex = 1.2)

# Traditional ICE Bootstrap Confidence Wedge Analyses...
ICEunc <- ICEuncrt(sepsis, icu, qalypres, totcost, lambda=50000, R=25000)
ICEunc

# Display the Bootstrap ICE Uncertainty Distribution...
plot(ICEunc)

ICEwdg <- ICEwedge(ICEunc)
ICEwdg
plot(ICEwdg)

# Compute VAGR Acceptability and ALICE Curves...
ICEacc <- ICEalice(ICEwdg)
plot(ICEacc, show="VAGR")

plot(ICEacc, show="Alice")

# Color Interior of Confidence Wedge with LINEAR Economic Preferences...
ICEcol <- ICEcolor(ICEwdg, gamma=1)
plot(ICEcol, show="RBOW")

plot(ICEcol, show="Hist")
(mPS <- mean(ICEcol$pref))
abline(v=mPS, col="red", lwd=3)

# Recolor Confidence Wedge with NON-Linear Preferences...
ICEcol <- ICEcolor(ICEwdg, beta=0.6, gamma=(3+2*sqrt(2))*0.6)
plot(ICEcol, show="RBOW")

plot(ICEcol, show="Hist")
mPS <- mean(ICEcol$pref)
abline(v=mPS, col="red", lwd=3)
text(-200, 4000, "Mean(PS)=-29.6", cex=1.5, col="red")
