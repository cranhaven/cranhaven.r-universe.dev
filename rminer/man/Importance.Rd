\name{Importance}
\alias{Importance}
\title{
Measure input importance (including sensitivity analysis) given a supervised data mining model.
}
\description{
Measure input importance (including sensitivity analysis) given a supervised data mining model.
}
\usage{
Importance(M, data, RealL = 7, method = "1D-SA", measure = "AAD", 
           sampling = "regular", baseline = "mean", responses = TRUE, 
           outindex = NULL, task = "default", PRED = NULL, 
           interactions = NULL, Aggregation = -1, LRandom = -1,
           MRandom = "discrete", Lfactor = FALSE)
}
\arguments{
  \item{M}{fitted model, typically is the object returned by \code{\link{fit}}. 
           Can also be any fitted model (i.e. not from rminer), provided that the predict function PRED is defined (see examples for details).}
  \item{data}{training data (the same data.frame that was used to fit the model, currently only used to add data histogram to VEC curve).}
  \item{RealL}{the number of sensitivity analysis levels (e.g. 7). Note: you need to use \code{RealL}>=2.}
  \item{method}{input importance method. Options are:
                \itemize{
                         \item 1D-SA -- 1 dimensional sensitivity analysis, very fast, sets interactions to NULL.
                         \item sens or SA -- sensitivity analysis. There are some extra variants:
                         sensa -- equal to \code{sens} but also sets \code{measure="AAD"}; sensv -- sets \code{measure="variance"}; sensg -- sets \code{measure="gradient"}; sensr -- sets \code{measure="range"}. if interactions is not null, then GSA is assumed, else 1D-SA is assumed.
                         \item DSA -- Data-based SA (good option if input interactions need to be detected). 
                         \item MSA -- Monte-Carlo SA.
                         \item CSA -- Cluster-based SA.
                         \item GSA -- Global SA (very slow method, particularly if the number of inputs is large, should be avoided).
                         \item randomForest -- uses method of Leo Breiman (type=1), only makes sense when M is a randomRorest.
                        }
}
  \item{measure}{sensitivity analysis measure (used to measure input importance). Options are:
                \itemize{
                         \item AAD -- average absolute deviation from the median.
                         \item gradient -- average absolute gradient (y_i+1-y_i) of the responses.
                         \item variance -- variance of the responses. 
                         \item range -- maximum - minimum of the responses. 
                        }
}
  \item{sampling}{for numeric inputs, the sampling scan function. Options are:
                \itemize{
                         \item  regular -- regular sequence (uniform distribution), do not change this value, kept here only due to compatibility issues.
                        }
}
  \item{baseline}{baseline vector used during the sensitivity analysis. Options are:
                  \itemize{
                         \item mean -- uses a vector with the mean values of each attribute from \code{data}.
                         \item median -- uses a vector with the median values of each attribute from \code{data}.
                         \item a data.frame with the baseline example (should have the same attribute names as \code{data}). 
                        }
                 }
  \item{responses}{if \code{TRUE} then all sensitivity analysis responses are stored and returned.}
  \item{outindex}{the output index (column) of \code{data} if \code{M} is not a model object (returned by fit).}
  \item{task}{the \code{task} as defined in \code{\link{fit}} if \code{M} is not a model object (returned by fit).}
  \item{PRED}{the prediction function of \code{M}, if \code{M} is not a model object (returned by fit). 
              Note: this function should behave like the rminer \code{\link{predict-methods}}, i.e. return a numeric vector in case of regression;
a matrix of examples (rows) vs probabilities (columns) (\code{task="prob"}) or a factor (\code{task="class"}) in case of classification.
    }
  \item{interactions}{numeric vector with the attributes (columns) used by Ith-D sensitivity analysis (2-D or higher, "GSA" method):
\itemize{
                      \item if \code{NULL} then only a 1-D sensitivity analysis is performed.
                      \item if \code{length(interactions)==1} then a "special" 2-D sensitivity analysis is performed using the index of interactions versus
all remaining inputs. Note: the $sresponses[[interactions]] will be empty (in \code{\link{vecplot}} do not use \code{xval} \code{=interactions}).
                      \item if \code{length(interactions)>1} then a full Ith-D sensitivity analysis is performed, where I=length(interactions).
Note: Computational effort can highly increase if I is too large, i.e. O(RealL^I). Also, you need to preprocess the 
returned list (e.g. using \code{avg_imp}) to use the \code{\link{vecplot}} function (see the examples).
}
                     }
  \item{Aggregation}{numeric value that sets the number of multi-metric aggregation function (used only for "DSA", ""). Options are:
                     \itemize{
                              \item -1 -- the default value that should work in most cases (if regression, sets Aggregation=3, else if classification then sets Aggregation=1).
                              \item 1 -- value that should work for classification (only use the average of all sensitivity values).
                              \item 3 -- value that should work for regression (use 3 metrics, the minimum, average and maximum of all sensitivity values).
                             }
                    }
  \item{LRandom}{number of samples used by DSA and MSA methods. The default value is -1, which means: use a number equal to training set size. If a different value is used (1<= value <= number of training samples), then LRandom samples are randomly selected.}
  \item{MRandom}{sampling type used by MSA: "discrete" (default discrete uniform distribution) or "continuous" (from continuous uniform distribution).}
  \item{Lfactor}{sets the maximum number of sensitivity levels for discrete inputs. if FALSE then a maximum of up to RealL levels are used (most frequent ones), else (TRUE) then all levels of the input are used in the SA analysis.}


}
\details{
This function provides several algorithms for measuring input importance of supervised data mining models and the average effect of a given input (or pair of inputs) in the model.
A particular emphasis is given on sensitivity analysis (SA), which is a simple method that measures the effects on the output of a given model
when the inputs are varied through their range of values. Check the references for more details.
}
\value{
A \code{list} with the components:
\itemize{
         \item $value -- numeric vector with the computed sensitivity analysis measure for each attribute.
         \item $imp -- numeric vector with the relative importance for each attribute (only makes sense for 1-D analysis).
         \item $sresponses -- vector list as described in the Value documentation of \code{\link{mining}}.
         \item $data -- if DSA or MSA, store the used data samples, needed for visualizations made by vecplot.
         \item $method -- SA method
         \item $measure -- SA measure
         \item $agg -- Aggregation value 
         \item $nclasses -- if task="prob" or "class", the number of output classes, else nclasses=1 
         \item $inputs -- indexes of the input attributes
         \item $Llevels -- sensitivity levels used for each attribute (NA means output attribute)
         \item $interactions -- which attributes were interacted when method=GSA. 
        }
}
\references{
\itemize{
\item To cite the Importance function, sensitivity analysis methods or synthetic datasets, please use:\cr
P. Cortez and M.J. Embrechts.\cr
Using Sensitivity Analysis and Visualization Techniques to Open Black Box Data Mining Models.\cr
In Information Sciences, Elsevier, 225:1-17, March 2013.\cr
\doi{10.1016/j.ins.2012.10.039}\cr
}
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\note{
See also \url{http://www3.dsi.uminho.pt/pcortez/rminer.html}
}

\seealso{
\code{\link{vecplot}}, \code{\link{fit}}, \code{\link{mining}}, \code{\link{mgraph}}, \code{\link{mmetric}}, \code{\link{savemining}}.
}
\examples{ 
### dontrun is used when the execution of the example requires some computational effort.

### 1st example, regression, 1-D sensitivity analysis
\dontrun{
data(sa_ssin) # x1 should account for 55%, x2 for 27%, x3 for 18% and x4 for 0%.
M=fit(y~.,sa_ssin,model="ksvm")
I=Importance(M,sa_ssin,method="1D-SA") # 1-D SA, AAD
print(round(I$imp,digits=2))

L=list(runs=1,sen=t(I$imp),sresponses=I$sresponses)
mgraph(L,graph="IMP",leg=names(sa_ssin),col="gray",Grid=10)
mgraph(L,graph="VEC",xval=1,Grid=10,data=sa_ssin,
   main="VEC curve for x1 influence on y") # or:
vecplot(I,xval=1,Grid=10,data=sa_ssin,datacol="gray",
   main="VEC curve for x1 influence on y") # same graph
vecplot(I,xval=c(1,2,3),pch=c(1,2,3),Grid=10,
leg=list(pos="bottomright",leg=c("x1","x2","x3"))) # all x1, x2 and x3 VEC curves
}

### 2nd example, regression, DSA sensitivity analysis:
\dontrun{
I2=Importance(M,sa_ssin,method="DSA")
print(I2)
# influence of x1 and x2 over y
vecplot(I2,graph="VEC",xval=1) # VEC curve
vecplot(I2,graph="VECB",xval=1) # VEC curve with boxplots
vecplot(I2,graph="VEC3",xval=c(1,2)) # VEC surface
vecplot(I2,graph="VECC",xval=c(1,2)) # VEC contour
}

### 3th example, classification (pure class labels, task="cla"), DSA:
\dontrun{
data(sa_int2_3c) # pair (x1,x2) is more relevant than x3, all x1,x2,x3 affect y, 
                 # x4 has a null effect.
M2=fit(y~.,sa_int2_3c,model="mlpe",task="class")
I4=Importance(M2,sa_int2_3c,method="DSA")
# VEC curve (should present a kind of "saw" shape curve) for class B (TC=2):
vecplot(I4,graph="VEC",xval=2,cex=1.2,TC=2,
 main="VEC curve for x2 influence on y (class B)",xlab="x2")
# same VEC curve but with boxplots:
vecplot(I4,graph="VECB",xval=2,cex=1.2,TC=2,
 main="VEC curve with box plots for x2 influence on y (class B)",xlab="x2")
}

### 4th example, regression, DSA and GSA:
\dontrun{
data(sa_psin)
# same model from Table 1 of the reference:
M3=fit(y~.,sa_psin,model="ksvm",search=2^-2,C=2^6.87,epsilon=2^-8)
# in this case: Aggregation should be -1 (default), 1 (class) or 3 (reg), see ref. paper.
I5=Importance(M3,sa_psin,method="DSA",Aggregation=3)
print("Input importances:")
print(round(I5$imp,digits=2)) # INS 2013 similar results

# 2D analysis (check reference for more details), RealL=L=7:
# need to aggregate results into a matrix of SA measure by using the agg_matrix_imp function.
# important notes: 
# - agg_matrix_imp only works for the methods "DSA", "MSA" and "GSA".
# - reliable agg_matrix_imp results for "DSA" or "MSA" only for a 
#   a large LRandom value (e.g., LRandom=1000) or when LRandom=-1 (all training samples)
cm=agg_matrix_imp(I5)
print("show Table 8 DSA results (from the reference):")
print(round(cm$m1,digits=2))
print(round(cm$m2,digits=2))
# internal rminer function:
# show most relevant (darker) input pairs, in this case (x1,x2) > (x1,x3) > (x2,x3)
# to build a nice plot, a fixed threshold=c(0.05,0.05) is used. note that
# in the paper and for real data, we use threshold=0.1, 
# which means threshold=rep(max(cm$m1,cm$m2)*threshold,2)
fcm=cmatrixplot(cm,threshold=c(0.05,0.05)) 
# 2D analysis using pair AT=c(x1,x2') (check reference for more details), RealL=7:
# nice 3D VEC surface plot:
vecplot(I5,xval=c(1,2),graph="VEC3",xlab="x1",ylab="x2",zoom=1.1,
 main="VEC surface of (x1,x2') influence on y")
# same influence but know shown using VEC contour:
par(mar=c(4.0,4.0,1.0,0.3)) # change the graph window space size
vecplot(I5,xval=c(1,2),graph="VECC",xlab="x1",ylab="x2",
 main="VEC surface of (x1,x2') influence on y")
# slower GSA:
I6=Importance(M3,sa_psin,method="GSA",interactions=1:4)
print("Input importances:")
print(round(I6$imp,digits=2)) # INS 2013 similar results
cm2=agg_matrix_imp(I6)
# compare cm2 with cm1, almost identical:
print(round(cm2$m1,digits=2))
print(round(cm2$m2,digits=2))
fcm2=cmatrixplot(cm2,threshold=0.1) 
}

### 5th example, classification, 1D_SA, DSA, MSA and GSA:
\dontrun{
data(sa_ssin_n2p)
# same model from Table 1 of the reference:
M4=fit(y~.,sa_ssin_n2p,model="ksvm",kpar=list(sigma=2^-8.25),C=2^10)

I7=Importance(M4,sa_ssin_n2p,method="1D-SA")
print("1D-SA Input importances:")
print(round(I7$imp,digits=2)) # INS 2013 similar results (Table 6)

I8=Importance(M4,sa_ssin_n2p,method="GSA",interactions=1:4)
print("GSA Input importances:")
print(round(I8$imp,digits=2)) # INS 2013 similar results (Table 6)

I9=Importance(M4,sa_ssin_n2p,method="DSA",LRandom=1000)
print("DSA Ns=1000 Input importances:")
print(round(I9$imp,digits=2)) # INS 2013 similar results (Table 6)

I10=Importance(M4,sa_ssin_n2p,method="DSA",LRandom=10)
print("DSA Ns=10 Input importances:")
print(round(I10$imp,digits=2)) # INS 2013 similar results (Table 6)

I11=Importance(M4,sa_ssin_n2p,method="MSA",LRandom=10)
print("MSA Ns=10 Input importances:")
print(round(I11$imp,digits=2)) # INS 2013 similar results (Table 6)

# 2D analysis:
cm3=agg_matrix_imp(I8)
fcm3=cmatrixplot(cm3,threshold=c(0.05,0.05)) 
cm4=agg_matrix_imp(I9)
fcm4=cmatrixplot(cm4,threshold=c(0.05,0.05)) 
}

### If you want to use Importance over your own model (different than rminer ones):
# 1st example, regression, uses the theoretical sin1reg function: x1=70% and x2=30%
data(sin1reg)
mypred=function(M,data)
{ return (M[1]*sin(pi*data[,1]/M[3])+M[2]*sin(pi*data[,2]/M[3])) }
M=c(0.7,0.3,2000)
# 4 is the column index of y
I=Importance(M,sin1reg,method="sens",measure="AAD",PRED=mypred,outindex=4) 
print(I$imp) # x1=72.3% and x2=27.7%
L=list(runs=1,sen=t(I$imp),sresponses=I$sresponses)
mgraph(L,graph="IMP",leg=names(sin1reg),col="gray",Grid=10)
mgraph(L,graph="VEC",xval=1,Grid=10) # equal to:
par(mar=c(2.0,2.0,1.0,0.3)) # change the graph window space size
vecplot(I,graph="VEC",xval=1,Grid=10,main="VEC curve for x1 influence on y:")

### 2nd example, 3-class classification for iris and lda model:
\dontrun{
data(iris)
library(MASS)
predlda=function(M,data) # the PRED function
{ return (predict(M,data)$posterior) }
LDA=lda(Species ~ .,iris, prior = c(1,1,1)/3)
# 4 is the column index of Species
I=Importance(LDA,iris,method="1D-SA",PRED=predlda,outindex=4)
vecplot(I,graph="VEC",xval=1,Grid=10,TC=1,
main="1-D VEC for Sepal.Lenght (x-axis) influence in setosa (prob.)")
}

### 3rd example, binary classification for setosa iris and lda model:
\dontrun{
data(iris)
library(MASS)
iris2=iris;iris2$Species=factor(iris$Species=="setosa")
predlda2=function(M,data) # the PRED function
{ return (predict(M,data)$class) }
LDA2=lda(Species ~ .,iris2)
I=Importance(LDA2,iris2,method="1D-SA",PRED=predlda2,outindex=4)
vecplot(I,graph="VEC",xval=1,
main="1-D VEC for Sepal.Lenght (x-axis) influence in setosa (class)",Grid=10)
}

### Example with discrete inputs 
\dontrun{
data(iris)
ir1=iris
ir1[,1]=cut(ir1[,1],breaks=4)
ir1[,2]=cut(ir1[,2],breaks=4)
M=fit(Species~.,ir1,model="mlpe")
I=Importance(M,ir1,method="DSA")
# discrete example:
vecplot(I,graph="VEC",xval=1,TC=1,main="class: setosa (discrete x1)",data=ir1)
# continuous example:
vecplot(I,graph="VEC",xval=3,TC=1,main="class: setosa (cont. x1)",data=ir1)
}

}

\keyword{ classif }
\keyword{ neural }
