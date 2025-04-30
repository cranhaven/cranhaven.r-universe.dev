#### tests for functions 'bcgam', 'predict.bcgam', 'plot.bcgam', 'persp.bcgam'
data(duncan)
bcgam.fit <- bcgam(income~sm.incr(prestige, space="E")+sm.conv(education)+type, data=duncan)
summary(bcgam.fit)
predict(bcgam.fit, newdata=data.frame(prestige=65, education=87 , type="prof"))
plot(bcgam.fit, prestige, col=4) 
persp(bcgam.fit, prestige, education, level=0.90)