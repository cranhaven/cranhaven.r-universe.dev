library(gppm)
# generate data according to the linear change model
b0 <- 3
b1 <- 2
sigma <- 1 # abbreviation for \sigma_\epsilon^2
nTime <- 3
trueModel <- function(t) {
  b0 + b1 * t + rnorm(n = 1, mean = 0, sd = sqrt(sigma))
}
tVector <- 1:nTime
yVector <- vapply(tVector, trueModel, 1)

# get results using lm
fittedLM <- lm(yVector ~ tVector)
parasLM <- coefficients(fittedLM)
parasLM[3] <- summary(fittedLM)$sigma
names(parasLM) <- c("LM.b0", "LM.b1", "LM.sigma")
print(parasLM)

# get results using GPPM
myData <- data.frame(ID = rep(1, nTime), t = tVector, y = yVector)
gpModel <- gppm("b0+b1*t", "(t==t#)*sigma", myData, ID = "ID", DV = "y")
gpModel <- fit(gpModel)

# compare results
coefSame <- all.equal(parasLM[1:2], coef(gpModel)[1:2], check.attributes = FALSE, tolerance = 0.0001)
message(sprintf("Coefficients of linear model are the same: %s", coefSame))
# residual variances, i.e. parasLM[3] and coef(gpModel) are not the same  because GPPM uses ML for parameter estimation and lm OLS
