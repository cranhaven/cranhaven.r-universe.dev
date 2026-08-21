getDefPrior<-function(Y){
# %Gives the default prior of classes in Y:
#   %  the sample proportions of each class
SamplePrior <- getFreqY(Y)/max(dim(Y))

return(SamplePrior)
}
