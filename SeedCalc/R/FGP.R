# final germination percentage
# equacao: fgp = n/N x 100
# n is the number of germinated seeds and N is the total number of seeds


FGP <- function(nger, Nseeds) {
 n <- max(nger)
 result <- n/Nseeds * 100
 return(result)
}
