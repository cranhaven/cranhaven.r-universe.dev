# some simple testing commands...
#   testthat would be an overshoot...

require(PRSim)

# raw demos
demo( "PRSim", ask=FALSE)
demo( "PRSim-validate", ask=FALSE)



# testing input
data(runoff)
unique(runoff$YYYY)


try( prsim( runoff[1:130,] ))   #  At least one year of data required.
try( prsim( runoff[1:730,] ))   #  No missing values allowed. Some days are missing.
try( prsim( runoff[1:1445,] ))  #  No missing values allowed. Some days are missing.
try( prsim( runoff[runoff$YYYY<1976,] )) # At least one year of data required.



suppressWarnings( out <- prsim( runoff[runoff$YYYY<1977,] ) )

runof <- runoff[runoff$YYYY<1980,]

set.seed(1)
str(out1 <- prsim( runof, marginalpar=FALSE, suppWarn=TRUE))

runo <- runof
names( runo) <- tolower( names(runof))
try( prsim( runo, marginalpar=FALSE, suppWarn=TRUE)) #  Wrong column for observations selected.

runo <- runof[,4:1]
set.seed(1)
out3 <- prsim( runo, marginalpar=FALSE, suppWarn=TRUE) #  ok
identical(out1,out3) 

runo <- runof[,4:1]
set.seed(1)
out4 <- prsim( runo, station_id=1, marginalpar=FALSE, suppWarn=TRUE) #  ok
identical(out1,out4) 

tmp <- paste(runof$YYYY, runof$MM, runof$DD,sep=" ")
runo <- data.frame(time=as.POSIXct(strptime(tmp, format="%Y %m %d", tz="GMT")), Qobs=runof$Qobs)
set.seed(1)
out5 <- prsim( runo, marginalpar=FALSE, suppWarn=TRUE) #  ok
identical(out1,out5) 

# 

######################
# Test 'kappa' distribution with manual construction:
rKappa <- function(n, theta) rand.kappa(n, theta[1], theta[2], theta[3], theta[4])
Kappa_fit <- function(xdat, ...) {
  ll <- Lmoments(xdat)  
  unlist(par.kappa(ll[1],ll[2],ll[4],ll[5]))
}
set.seed(1)
out6a <- prsim( runo, marginalpar=TRUE)
set.seed(1)
out6b <- prsim( runo, marginal="kappa", marginalpar=TRUE)
identical(out6a$pars, out6b$pars)   # columns are differently named...
colSums( (as.matrix(out6a$pars)-out6b$pars)^2)
summary(out6a$simulation-out6b$simulation)

plot(out6a$simulation$r1, type='l')
lines.default(out6b$simulation$r1, col=3)
rug( which(out6b$simulation$r1 != out6a$simulation$r1))

days_diff <- matrix(out6a$simulation$r1 != out6b$simulation$r1,nrow=365)
kap_par <- data.frame(out6a$pars)
thresh <- kap_par$xi + kap_par$alfa*(1 - kap_par$h^(-kap_par$k))/kap_par$k
image( cbind( is.na(thresh), days_diff))


