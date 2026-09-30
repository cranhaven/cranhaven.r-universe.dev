\name{bapred-package}
\alias{bapred-package}
\alias{bapred}
\docType{package}
\title{
The bapred package
}
\description{
This is a short summary of the features of \pkg{bapred}, a package in \R for the treatment and analysis of batch effects based in high-dimensional molecular data via batch effect adjustment and addon quantile normalization. Here, a special focus is set on phenotype prediction in the presence of batch effects.
}
\details{
Various tools dealing with batch effects, in particular enabling the 
removal of discrepancies between training and test sets in prediction scenarios.
Moreover, addon quantile normalization and addon RMA normalization (Kostka & Spang, 
2008) is implemented to enable integrating the quantile normalization step into 
prediction rules. The following batch effect removal methods are implemented: 
FAbatch, ComBat, (f)SVA, mean-centering, standardization, Ratio-A and Ratio-G. 
For each of these we provide  an additional function which enables a posteriori 
('addon') batch effect removal in independent batches ('test data'). Here, the
(already batch effect adjusted) training data is not altered. For evaluating the
success of batch effect adjustment several metrics are provided. Moreover, the 
package implements a plot for the visualization of batch effects using principal 
component analysis. The main functions of the package for batch effect adjustment 
are \code{\link{ba}()} and \code{\link{baaddon}()} which enable batch effect removal and addon batch effect 
removal, respectively, with one of the seven methods mentioned above. Another 
important function here is \code{\link{bametric}()} which is a wrapper function for all implemented
methods for evaluating the success of batch effect removal. For (addon) quantile 
normalization and (addon) RMA normalization the functions \code{\link{qunormtrain}()}, 
\code{\link{qunormaddon}()}, \code{\link{rmatrain}()} and \code{\link{rmaaddon}()} can be used.
}
\author{
Roman Hornung, David Causeur

Maintainer: Roman Hornung
  \email{hornung@ibe.med.uni-muenchen.de}
}
\references{
Hornung, R., Boulesteix, A.-L., Causeur, D. (2016). Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27, <\doi{10.1186/s12859-015-0870-z}>.

Hornung, R., Causeur, D., Bernau, C., Boulesteix, A.-L. (2017). Improving cross-study prediction through addon batch effect adjustment and addon normalization. Bioinformatics 33(3):397–404, <\doi{10.1093/bioinformatics/btw650}>.
}
\examples{
# Load example dataset:

data(autism)



# Subset this example dataset to reduce the
# computational burden of the toy examples:

# Random subset of 150 variables:
set.seed(1234)
Xsub <- X[,sample(1:ncol(X), size=150)]

# In cases of batches with more than 20 observations
# select 20 observations at random:
subinds <- unlist(sapply(1:length(levels(batch)), function(x) {
  indbatch <- which(batch==x)
  if(length(indbatch) > 20)
    indbatch <- sort(sample(indbatch, size=20))
  indbatch
}))
Xsub <- Xsub[subinds,]
batchsub <- batch[subinds]
ysub <- y[subinds]



# Split into training and test sets:

trainind <- which(batchsub \%in\% c(1,2))

Xsubtrain <- Xsub[trainind,]
ysubtrain <- ysub[trainind]
batchsubtrain <- factor(as.numeric(batchsub[trainind]), levels=c(1,2))


testind <- which(batchsub \%in\% c(3,4))

Xsubtest <- Xsub[testind,]
ysubtest <- ysub[testind]

batchsubtest <- as.numeric(batchsub[testind])
batchsubtest[batchsubtest==3] <- 1
batchsubtest[batchsubtest==4] <- 2
batchsubtest <- factor(batchsubtest, levels=c(1,2))



# Batch effect adjustment:

combatparams <- ba(x=Xsubtrain, y=ysubtrain, batch=batchsubtrain, 
  method = "combat")
Xsubtraincombat <- combatparams$xadj

meancenterparams <- ba(x=Xsubtrain, y=ysubtrain, batch=batchsubtrain, 
  method = "meancenter")
Xsubtrainmeancenter <- meancenterparams$xadj



# Addon batch effect adjustment:

Xsubtestcombat <- baaddon(params=combatparams, x=Xsubtest, 
  batch=batchsubtest)

Xsubtestmeancenter <- baaddon(params=meancenterparams, x=Xsubtest, 
  batch=batchsubtest)



# Metrics for evaluating the success of batch effect adjustment:

bametric(xba=Xsubtrain, batch=batchsubtrain, metric = "sep")
bametric(xba=Xsubtrainmeancenter, batch=batchsubtrain, metric = "sep")

bametric(x=Xsubtrain, batch=batchsubtrain, y=ysubtrain, 
  metric = "diffexpr", method = "meancenter")

bametric(xba=Xsubtrainmeancenter, x=Xsubtrain, metric = "cor")



# Principal component analysis plots for the visualization 
# of batch effects:

par(mfrow=c(1,3))
pcplot(x=Xsub, batch=batchsub, y=ysub, alpha=0.25, main="alpha = 0.25")
pcplot(x=Xsub, batch=batchsub, y=ysub, alpha=0.75, main="alpha = 0.75")
pcplot(x=Xsub, batch=batchsub, y=ysub, col=1:length(unique(batchsub)), 
  main="col = 1:length(unique(batchsub))")
par(mfrow=c(1,1))



# (Addon) quantile normalization:

qunormparams <- qunormtrain(x=Xsubtrain)

Xtrainnorm <- qunormparams$xnorm

Xtestaddonnorm <- qunormaddon(qunormparams, x=Xsubtest)
}
