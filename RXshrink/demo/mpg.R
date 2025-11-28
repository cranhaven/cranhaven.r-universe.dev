  library(RXshrink)
  #
  # Input the Gasoline Mileage data of Hocking(1976). Essentially the same
  # data (with shorter names) are contained in the "mtcars" data.frame...
  data(mpg)
  #
  # Specify a linear regression model with four predictors of gas mileage...
  form <- mpg~cylnds+cubins+hpower+weight
  #
  # Fit this model using unrestricted generalized ridge regression (GRR)...
  rxefobj <- eff.ridge(form, data=mpg)
  #
  rxefobj
  # SCROLL ^^^ UP ^^^ to see PRINTED output from qm.ridge()...
  #
  plot(rxefobj)
  #
  # Define TRUE parameter values...
  trugam <- matrix(c(-.5,-.1,.1,-.6),4,1)
  truvar <- 0.16             # TRUE residual variance...
  #
  # SIMULATE a "Correct" Linear Model with IID errors from a Normal-distribution...
  trudat <- MLtrue(form, data=mpg, truc=trugam, truv=truvar)
  #
  form2 <- Yvec~cylnds+cubins+hpower+weight
  #  
  ideal <- eff.ridge(form2, data=trudat$new)
  #
  plot(ideal)
  #
  ################## End of "mpg" DEMO...
  