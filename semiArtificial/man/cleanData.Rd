\name{cleanData}
\alias{cleanData}

\title{ Rejection of new instances based on their distance to existing instances}
\description{
The function contains three data cleaning methods, 
the first two reject instances whose distance to their nearest neighbors in the existing data are too small 
or too large. The first checks distance between instances disregarding class,
the second checks distances between instances taking only instances from the same class into account.
The third method reassigns response variable using the prediction model stored in the generator \code{teObject}.
    }
\usage{
cleanData(teObject, newdat, similarDropP=NA, dissimilarDropP=NA, 
          similarDropPclass=NA, dissimilarDropPclass=NA, 
		  nearestInstK=1, reassignResponse=FALSE, cleaningObject=NULL) 
}

\arguments{
  \item{teObject}{ An object of class \code{TreeEnsemble} containing a generator structure as returned by
    \code{\link{treeEnsemble}}. The \code{teObject} contains generator's training instances from which we compute 
    a distance distribution of instances to their \code{nearestInsK} nearest instances.
    This distance distribution, computed on the training data of the generator, serves as a criterion to reject new instances from \code{newdata}, 
    i.e. based on parameters below we reject the instances too close or to far away from their nearest neighbors in generator's training data.
    The computed distance distributions are stored and returned as \code{cleaningObject} component of returned list. If it is provided on subsequent 
    calls, this reduces computational load.  }
  \item{newdat}{ A \code{data.frame} object with the (newly generated) data to be cleaned.}
  \item{similarDropP}{With numeric parameters \code{similarDropP} and \code{dissimilarDropP} 
  (with the default value NA and the valid value range in [0, 1]) one removes instances in \code{newdat}
   too close to generator's training instances or too far away from these instances. The distance distribution is computed based on instances stored in 
   \code{teObject}. For each instance in $teObject$ we store the distance to its \code{nearestInsK} nearest  instances
   (disregarding the identical instances). These distances are sorted and represent a distribution of nearest distances for all training instances.
	   The values \code{similarDropP} and \code{dissimilarDropP} represent a proportion of allowed smaller/larger distances 
	   computed on the generator's training data contained in the \code{teObject}.   }
    \item{dissimilarDropP}{See \code{similarDropP}.}
   \item{similarDropPclass}{For classification problems only and similarly to the \code{similarDropP} and \cr
    \code{dissimilarDropP} above, 
   with  the \code{similarDropPclass} and \code{dissimilarDropPclass} (also in a [0, 1] range) we also removes instances in \code{newdat}
   too close to generator's training instances or too far away from these instances, but only taking near instances from the same class into account.
   The \code{similarDropPclass} contains either a single integer giving thresholds for all class values or a vector of thresholds, one for each class.
   If the vector is of insufficient length it is replicated using function \code{\link{rep}}.
    The generated distance distributions are stored in the \code{cleaningObject} component of the returned list. }
    \item{dissimilarDropPclass}{See \code{similarDropPclass}.}
    \item{nearestInstK}{An integer with default value of 1, controls how many generator's training instances we take into account when computing 
      the distance distribution of nearest instances.}
    \item{reassignResponse}{is a \code{logical} value controlling whether the response variable of the \code{newdat} shall be set anew 
    using a random forest prediction model or taken as it is. The default value \code{reassign=FALSE} means that values of response are not changed.}
    \item{cleaningObject}{ is a list object with a precomputed distance distributions and predictor from previous runs of the same function.
    If provided, this saves computation time.}
 }
\details{
   The function uses the training instances stored in the generator \code{teObject} to compute distribution of distances from instances to their 
   \code{nearestInstK} nearest instances. For classification problems the distributions can also be computed only for instances from the same class.
   Using these near distance distributions the function rejects all instances too close or too far away from existing instances. 
   
   The default value of \code{similarDropP}, \code{dissimilarDropP}, \code{similarDropPclass}, and \code{dissimilarDropPclass} is NA and means that
   the near/far values are not rejected. The same effect has value 0 for \code{similarDropP} and \code{similarDropPclass}, and value 1 for
   \code{dissimilarDropP} and \code{dissimilarDropPclass}.
  }
\value{
  The method returns a \code{list} object  with two components:
  \item{cleanData}{is a \code{data.frame} containing the instances left after rejection of too close or too distant instances from \code{newdata}.}
  \item{cleaningObject}{is a \code{list} containing computed distributions of nearest distances (also class-based fro classification problems, 
  and possibly a predictor used for reassigning the response variable.  }
  }

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{treeEnsemble}}, \code{\link{newdata.TreeEnsemble}}.
}
\examples{
# inspect properties of the iris data set
plot(iris, col=iris$Species)
summary(iris)

irisEnsemble<- treeEnsemble(Species~.,iris,noTrees=10)

# use the generator to create new data with the generator
irisNewEns <- newdata(irisEnsemble, size=150)

#inspect properties of the new data
plot(irisNewEns, col = irisNewEns$Species) #plot generated data
summary(irisNewEns)

clObj <- cleanData(irisEnsemble, irisNewEns, similarDropP=0.05, dissimilarDropP=0.95, 
                   similarDropPclass=0.05, dissimilarDropPclass=0.95, 
		           nearestInstK=1, reassignResponse=FALSE, cleaningObject=NULL) 
head(clObj$cleanData)
}


\keyword{ datagen }
\keyword{ multivariate }