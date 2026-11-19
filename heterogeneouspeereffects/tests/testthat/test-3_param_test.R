test_that("test GMM works", {
  
  #######################
  ##### Scenario 1 ######
  #######################
  
  # Parameters
  delta = -3
  thetaW = 0.7
  thetaB = 0.3
  beta_NE = -2
  beta_E = -1
  
  # Number of groups
  G <- 1000
  # Generate the mean and sd of the random vector alpha
  mu_alphaX <- c(8, 4, -3, 2)
  Sigma_alphaX <- matrix(c(4, 1, 0, 0, 1, 4, 0, 0, 0, 0, 4, -4.2, 0, 0, -4.2, 9), nrow = 4)
  # Create a logistic function to correlate D with s
  logistic <- function(x) {
    1 / (1 + exp(-x))
  }
  # Generate the alpha vector
  alphaX <- mvtnorm::rmvnorm(n = G, mean = mu_alphaX, sigma = Sigma_alphaX)
  # Scale s to control correlation strength (optional)
  s <- runif(n = G)
  scaled_s <- (s - mean(s)) / sd(s)  # Standardize s for better control
  # Compute probabilities for D based on scaled s
  prob_D <- logistic(scaled_s)  # Adjust scaling for desired correlation
  # Generate D based on the probabilities
  D <- rbinom(n = G, size = 1, prob = prob_D)
  # Generate YE and YNE 
  YN <- ((1 - thetaW * s ) * (alphaX[,1] + beta_NE * alphaX[,3])  + thetaB * s * (alphaX[,2] + beta_E * alphaX[,4]) + delta * thetaB * s * D)/(1 - thetaW + s*(1-s)*(thetaW^2 - thetaB^2))
  YE <- ((1 - thetaW * s ) * (1 - s) * thetaB * (alphaX[,1] + beta_NE * alphaX[,3]) + (1 + thetaW * (s * (1-s) * thetaW - 1)) * (alphaX[,2] + beta_E * alphaX[,4]) + delta * (1 + thetaW * (s * (1-s) * thetaW - 1)) * D)/((1 - thetaW * s) * (1 - thetaW + s*(1-s)*(thetaW^2 - thetaB^2))) 
  
  result_simple_5param <-heter_endo_gmm(YE, YN, D, s)
  
  
  #######################
  ##### Scenario 2 ######
  #######################
  delta = -3
  thetaWM = 0.6
  thetaWW = 0.2
  thetaBWM = -0.5
  thetaBMW = 0.5
  
  
  beta_W = -2
  beta_M = -1
  
  G <- 1000
  
  # Generate the alpha vector
  alphaX <- mvtnorm::rmvnorm(n = G, mean = mu_alphaX, sigma = Sigma_alphaX)
  
  
  # Generate the vectors (s, T)
  sM <- runif(n = G)
  sWE <- runif(n = G)
  sME <- runif(n = G)
  D <- rbinom(n = G, size = 1, prob = 0.5)
  
  # Generate YE and YNE 
  
  
  YW <- ((1 - thetaWM * sM) * (alphaX[,2] + beta_M * alphaX[,4])  + thetaBWM * sM * (alphaX[,1] + beta_W * alphaX[,3]) + D * delta * (sWE * (1-thetaWM*sM) + sME * thetaBWM * sM))/(1 - thetaWW * (1-sM) - thetaWM * sM + sM*(1-sM)*(thetaWW*thetaWM - thetaBWM * thetaBMW))
  YM <- ((1 - thetaWW * (1-sM)) * (alphaX[,1] + beta_M * alphaX[,3]) + thetaBMW * (1-sM) * (alphaX[,2] + beta_W * alphaX[,4]) + D * delta * (sWE * thetaBMW * (1-sM) + sME * (1 - thetaWW * (1 - sM))))/(1 - thetaWW * (1-sM) - thetaWM * sM + sM*(1-sM)*(thetaWW*thetaWM - thetaBWM * thetaBMW))
  
  
  # Resultats analytique
  result_ortho <- ortho_heter_endo_gmm(YM, YW, D, sM, sME, sWE)
  result_ortho
  
})
