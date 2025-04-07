#################################################################
##
## Code for tutorial/workshop on exactLTRE package
##
#################################################################

# Other libraries needed for this tutorial code:
# If you don't have them, install them from CRAN
library(devtools)
library(Rcompadre)

# Install exactLTRE from github:
# devtools::install_github("chrissy3815/exactLTRE", force=TRUE)
# if that doesn't work, try:
# devtools::install_github("chrissy3815/exactLTRE", ref="main", force=TRUE)
# If that still doesn't work, you can clone the repository from Github and
# install directly (see here: https://kbroman.org/pkg_primer/pages/build.html)

# Load the exactLTRE library:
library(exactLTRE)

## Examples from the package documentation:

# Build some example matrices:
A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)

# Approximate LTRE:
# contributions to the difference in lambda
cont_diff<- approximateLTRE(list(A1,A2), method='fixed')
cont_diff; # matrix of contributions
sum(cont_diff); # sum of approximate contributions
lamDiff(list(A1,A2)); # true difference in lambda

# contributions to the variance of lambda
cont_var<- approximateLTRE(list(A1,A2,A3), method='random')
round(cont_var,digits=5); # matrix of contributions
sum(cont_var); # sum of contributions
lamVar(list(A1,A2,A3)); # true variance in lambda

# Exact LTRE:
# contributions to the difference in lambda
cont_diff<- exactLTRE(list(A1,A2), method='fixed')
cbind(as.character(cont_diff$varying.indices.list),round(cont_diff$epsilons,digits=5))
sum(cont_diff$epsilons); # sum of contributions
lamDiff(list(A1,A2)); # true difference in lambda

# contributions to the variance of lambda
cont_var<- exactLTRE(list(A1,A2,A3), method='random')
cbind(as.character(cont_var$varying.indices.list),round(cont_var$epsilons,digits=5));
sum(cont_var$epsilons); # sum of contributions
lamVar(list(A1,A2,A3)); # true variance in lambda

# Generation Time:
F1<- matrix(0, nrow=3, ncol=3)
F1[1,3]<- A1[1,3]
#F1 is all zeros, except the upper right corner which matches A1 for adult fertility
gen_time <- generation_time(A1, F1)

# R0, the expected lifetime reproductive output for an individual
R0<- r_nought(A1, F1)

# expected lifespan
U1<- A1
U1[1,3]<- 0
# the upper right corner represents adult fertility in this model. U1, the
# survival matrix, contains all the transitions *except* for fertility.
eta<- lifespan(U1, all_ages=TRUE)
eta_1<- lifespan(U1, all_ages=FALSE) # eta_1 should match the first entry of eta

#################################################################
## Examples with COMPADRE
#################################################################

# Load comadre and compadre databases:
comadre<- cdb_fetch("comadre")
compadre<- cdb_fetch("compadre")

## Note: if you can't install Rcompadre because of OS incompatibility, then
# uncomment this line, make sure the file path is correct, and run it to load
# the matrices and metadata for the following few examples.
# This file contains the following four objects:
# geocrinia, geocrinia_mats, spermophilus, spermophilus_mats,
load('example_matrices.Rdata')

# We'll first work through a simple example of fixed design LTREs with white-bellied frogs.
geocrinia<- cdb_flatten(comadre[comadre$SpeciesAuthor=="Geocrinia_alba",])
# cdb_flatten extracts the metadata only, without the `mat` list object.

# You can look through the metadata about these matrices in the object "geocrinia"
# For a full list of the available metadata, try:
names(geocrinia)
# You can learn more about the variables available in metadata here:
# https://jonesor.github.io/CompadreGuides/user-guide.html#variables-in-metadata

# Here's an example of pulling out some metadata:
geocrinia[,c("SpeciesAccepted", "OrganismType", "Country", "MatrixPopulation", "MatrixTreatment")]

# pull out the two matrices of interest:
geocrinia_mats<- c(matA(comadre[comadre$MatrixID==248239,]),matA(comadre[comadre$MatrixID==248238, ]))
lapply(geocrinia_mats, eigen, only.values=TRUE)

# Evaluate a fixed symmetric design LTRE:
result<- exactLTRE(geocrinia_mats, method='fixed', fixed.directional = FALSE)
result$varying.indices.list
barplot(t(result$epsilons[2:length(result$epsilons)]),
        names.arg=c("sJ", "f","sJ, f", "sA", "sJ, sA", "f, sA", "sJ, f, sA"), las=2)

## Spermophilus armatus, ground squirrels.
spermophilus<- cdb_flatten(comadre[comadre$MatrixID %in% c(249840,249844),])
# cdb_flatten extracts the metadata only.

# Should we do a symmetric or directional LTRE?
spermophilus[,c("MatrixID", "MatrixPopulation", "MatrixTreatment")]

# pull out the matrices:
spermophilus_mats<- c(matA(comadre[comadre$MatrixID==249840]), matA(comadre[comadre$MatrixID==249844]))

# Maybe try running it both ways, and see how the interpretation changes.
spermophilus_symm<- exactLTRE(spermophilus_mats, method='fixed', maxint=3, fixed.directional = FALSE)
spermophilus_dir<- exactLTRE(spermophilus_mats, method='fixed', maxint=3, fixed.directional = TRUE)

# plot the comparison:
toplot<- rbind(spermophilus_symm$epsilons, spermophilus_dir$epsilons)
toplot<- toplot[,-1]
xlabels<- c("F1", "P1", "F2", "P2", "F3", "P3",
            "F1+P1", "F1+F2", "F1+P2", "F1+F3", "F1+P3", "P1+F2", "P1+P2",
            "P1+F3", "P1+P3", "F2+P2", "F2+F3", "F2+P3", "P2+F3", "P2+P3",
            "F3+P3", "F1+P1+F2", "F1+P1+P2", "F1+P1+F3", "F1+P1+P3",
            "F1+F2+P2", "F1+F2+F3", "F1+F2+P3", "F1+P2+F3", "F1+P2+P3",
            "F1+F3+P3", "P1+F2+P2", "P1+F2+F3", "P1+F2+P3", "P1+P2+F3",
            "P1+P2+P3", "P1+F3+P3", "F2+P2+F3", "F2+P2+P3", "F2+F3+P3", "P2+F3+P3")
barplot(toplot, beside = TRUE, names.arg = xlabels, las=2,
        legend=c("Symmetric", "Directional"), args.legend = list(x="topright"),)

#################################################################
## Breakout room activity
#################################################################

# In your breakout groups, pick another species to analyse with a random LTRE.



