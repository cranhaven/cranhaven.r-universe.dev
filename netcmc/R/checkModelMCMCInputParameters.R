checkModelMCMCInputParameters = function(numberOfSamples, burnin, thin) {
    
  if(numberOfSamples < 1)
    stop("Error: The number of samples cannot be less than 1!")

  if(burnin < 0)
    stop("Error: The length of burnin cannot be negative!")

  if(thin < 0)
    stop("Error: The thinning parameter cannot be negative!")

  if(numberOfSamples %% thin != 0)
    stop("Error: The number of samples requested is not divisible by the thinning parameter!")
  
}