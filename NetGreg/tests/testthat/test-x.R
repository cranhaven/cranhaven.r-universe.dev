# Load and process the data to run NetGreg
library(plsgenomics)
data(Colon)
X = data.frame(Colon$X[,1:100]) ## The first 100 genes
Z = data.frame(Colon$X[,101:102]) ## Two clinical covariates
colnames(Z) = c("Z1", "Z2")
Y = as.vector(Colon$X[,1000])  ## Continuous outcome variable

## Apply identifyHubs():
preNG = identifyHubs(X=X, delta=0.05, tau=5, ebic.gamma = 0.1)

## Explore preNG results:
hubs = preNG$hubs ## Returns the names of the identified hub nodes.
NG = NetworkGuided(Y=Y, X=X,hubs=preNG$hubs, Z=Z, nfolds=5)
NG


# Create a function to check the null or empty results.
sign_check <- function(NG) {
  if (!is.null(NG$coef) && nrow(NG$coef) > 0) {
    return("Successfully Obtained NG Penalized Estimates")
  } else {
    return("ERROR: No Network-Guided Penalized Estimates")
  }
}


# A function to check if results have any negative total number of penalized estimates:
test_that("Check that NG contains penalized estimates", {
  expect_equal(sign_check(NG), "Successfully Obtained NG Penalized Estimates")
})









