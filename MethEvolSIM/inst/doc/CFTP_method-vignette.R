## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MethEvolSIM)

## -----------------------------------------------------------------------------
# Example with sampled initial methylation frequencies and default parameter values
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"))
initialD <- simulate_initialData(infoStr = custom_infoStr, CFTP = TRUE)

## -----------------------------------------------------------------------------
# Example with customized initial methylation frequencies, customized parameter values 
# and default dt (0.01)
tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
custom_infoStr <- data.frame(n = c(10, 10, 10),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005

evolD <- simulate_evolData(infoStr = custom_infoStr, tree = tree, params = custom_params, CFTP = TRUE)

