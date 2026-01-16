\name{imputation}
\alias{imputation}
\alias{imputation,matrix-method}
\alias{imputation,array-method}
\alias{imputation,LongData-method}
\alias{imputation,LongData3d-method}

\title{ ~ Function: imputation ~ }
\description{
\code{imputation} is a function that offer different methods to impute
missing value of a \code{\linkS4class{LongData}} (or a matrix).
}
\usage{
imputation(traj,method="copyMean",lowerBound="globalMin",upperBound="globalMax")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{traj}{\code{[LongData]} or \code{[matrix]} : trajectories to impute.}
  \item{method}{\code{[character]}: Name of the imputation method (see detail)}
  \item{lowerBound}{\code{[character]} or \code{[numeric]}: fixes the
    smallest value that an imputed value can take. If a single value is
    given, it is duplicate for all the column. The special value \code{'min'}
    means that the lower bound will be the smallest value of the
    column. The special value \code{'globalMin'} means that the lower
    bound will be the overall smallest value (of each variable if there
    is several variable-trajectories). The special value \code{'NA'} can
    be used to impute without using a lower bound.}
  \item{upperBound}{\code{[character]} or \code{[numeric]}: fixes the
    biggest value that an imputed value can take. If a single value is
    given, it is duplicate for all the column. The special value \code{'max'}
    means that the upper bound will be the biggest value of the column.
    The special value \code{'globalMax'} means that the upper
    bound will be the overall biggest value (of each variable if there
    is several variable-trajectories). The special value \code{'NA'} can
    be used to impute without using an upper bound.}
}
\details{
  \code{imputation} is a function that impute
  missing value of a \code{\linkS4class{LongData}} or a \code{matrix}.
  Several imputation methods are available. A brief description
  follows. For a fully detailled description, see [3].
  Illustrating examples showing strenghs and weakness of methods are presented section "examples".

  For each method, the imputation has to deal with monotone missing
  value (at start and at end of the trajectories) and intermitant (in
  the middle). Here is a brief description of each methods.


  \describe{
    \item{'linearInterpol.locf' (linear interpolation, locf)}{
      \describe{
	\item{Intermitant:}{ values imediatly surounding the missing are join by a line.}
	\item{Monotone:}{ imputed by 'locf' or 'nocb'.}
      }
    }

    \item{'linearInterpol.global' (linear interpolation, global slope)}{
      \describe{
	\item{Intermitant:}{ values imediatly surounding the missing are join by a line.}
	\item{Monotone:}{ the line joining the first and last non-missing value
	  is considered (this line is the everage progression of the actual
	  individual trajectoire). Missing-value at start and at end are chosen on
	  this line.}
      }
    }

    \item{'linearInterpol.local' (linear interpolation, global slope)}{
      \describe{
	\item{Intermitant:}{ values imediatly surounding the missing are join by a line.}
	\item{Monotone at start:}{ the line joining the first and second non-missing value
	  is considered. Missing-value at start are chose on this line.}
	\item{Monotone at end:}{ the line joining the last and penultimate non-missing value
	  is considered. Missing-value at end are chosen on this line.}
      }
    }

    \item{'linearInterpol.bisector' (linear interpolation, bisector)}{
      \describe{
	\item{Intermitant:}{ values imediatly surounding the missing are join by a line.}
	\item{Monotone:}{ linearInterpol.global is not
	  sensitive to local variation, linearInterpol.local might be too much sensitive to
	  abnormal value. linearInterpol.bisector offer a medium solution by considering the
	  bissectrice of Global and Local solution. Point are chosen on
	  the bissectrices.}
      }
    }

    \item{'copyMean.locf' (copy mean, locf)}{
      this method impute in two stages. First, it use 'linearInterpol.locf'. Then it add to each imputed value a variation that make the imputed value
      follow the shape of the average trajectory. For more details, see [3] and examples' section.
    }

    \item{'copyMean.global' (copy mean, global slope)}{
      this method impute in two stages. First, it use 'linearInterpol.global'. Then it add to each imputed value a variation that make the imputed value
      follow the shape of the average trajectory. For more details, see [3] and examples' section.
    }

    \item{'copyMean.local' (copy mean, local slope)}{
      this method impute in two stages. First, it use 'linearInterpol.local'. Then it add to each imputed value a variation that make the imputed value
      follow the shape of the average trajectory. For more details, see [3] and examples' section.
    }

    \item{'copyMean.bisector' (copy mean, bisector)}{
      this method impute in two stages. First, it use 'linearInterpol.bisector'. Then it add to each imputed value a variation that make the imputed value
      follow the shape of the average trajectory. For more details, see [3] and examples' section.
    }
    \item{locf (Last Occurence Carried Forward)}{
      THIS METHOD HAS BEEN PROUVEN TO NOT BE EFFICIANT SEVERAL TIME BY VARIOUS AUTHOR, we strongly recommand to not use it !
      \describe{
	\item{Intermitant and monotone at end:}{ the previous non-missing value is
	  dipplicated forward.}
	\item{Monotone at start:}{ the first non-missing value is
	  dupplicated backward (nocb).}
      }
    }
    \item{nocb (Next Occurence Carried Backward)}{
      THIS METHOD HAS BEEN PROUVEN TO NOT BE EFFICIANT SEVERAL TIME BY VARIOUS AUTHOR, we strongly recommand to not use it !
      \describe{
	\item{Intermitant and monotone at start:}{ the next non-missing value is
	  dipplicated backward.}
	\item{Monotone at end:}{ the last non-missing value is dupplicated forward (locf).}
      }

    }
    \item{trajMean}{ missing are imputed by the mean of the trajectory.}
    \item{trajMedian}{ missing are imputed by the median of the trajectory.}
    \item{trajHotDeck}{ each missing is imputed by one non-missing (randomly choosen) value of the trajectory.}
    \item{crossMean}{ missing value at time t are imputed by the mean of all value present at time t.}
    \item{crossMedian}{ missing value at time t are imputed by the median of all value present at time t.}
    \item{crossHotDeck}{ each missing value at time t is imputed by one non-missing (randomly choosen) value present at time t.}


  }
}
\value{
  A \code{\linkS4class{LongData}} or a \code{matrix} with no missing values.
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121,
  2011\cr

  [3] Christophe Genolini, René Écochard and Hélène Jacqmin-Gadda\cr
  "Copy Mean: A New Method to Impute Intermittent Missing Values in Longitudinal Studies"\cr
  Open Journal of Statistics, vol 3(26),2013\cr
}

\seealso{
  \code{\linkS4class{LongData}}, \code{\linkS4class{Partition}}, \code{\link{qualityCriterion}}
}

\examples{
##################
### Preparation of the data
par(ask=TRUE)
timeV <- 1:14

matMissing <- matrix(
    c(NA  ,NA  ,NA  ,18  ,22  ,NA  ,NA  ,NA  ,NA  , 24  , 22  , NA  , NA  , NA,
      24  ,21  ,24  ,26  ,27  ,32  ,30  ,22  ,26  , 26  , 28  , 24  , 23  , 21,
      14  ,13  , 10 , 8  , 7  ,18  ,16  , 8  ,12  ,  6  ,  10 ,  10 ,  9  ,  7,
       3  ,1   , 1  , 1  ,  3,9   , 7  , -1 , 3   ,  2   ,  4 ,  1  ,  0  , -2
   ),4,byrow=TRUE
)


matplot(t(matMissing),col=c(2,1,1,1),lty=1,type="l",lwd=c(3,1,1,1),pch=16,
   xlab="Black=trajectories; Green=mean trajectory\nRed=trajectory to impute",
   ylab="",main="Four trajectories")
moy <- apply(matMissing,2,mean,na.rm=TRUE)
lines(moy,col=3,lwd=3)

 # # # # # # # # # # # # # # # # # # # # # # # # # #
#   Illustration of the different imputing method   #
 #           The best are at end  !!!              #
  # # # # # # # # # # # # # # # # # # # # # # # # #



##################
### Methods using cross sectionnal information (cross-methods)

par(mfrow=c(1,3))
mat2 <- matrix(c(
  NA, 9, 8, 8, 7, 6,NA,
   7, 6,NA,NA,NA, 4,5,
   3, 4, 3,NA,NA, 2,3,
  NA,NA, 1,NA,NA, 1,1),4,7,byrow=TRUE)

### crossMean
matplot(t(imputation(mat2,"crossMean")),type="l",ylim=c(0,10),
   lty=1,col=1,main="crossMean")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### crossMedian
matplot(t(imputation(mat2,"crossMedian")),type="l",ylim=c(0,10),
   lty=1,col=1,main="crossMedian")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### crossHotDeck
matplot(t(imputation(mat2,"crossHotDeck")),type="l",ylim=c(0,10),
   lty=1,col=1,main="crossHotDeck")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)



##################
### Methods using trajectory information (traj-methods)

par(mfrow=c(2,3))
mat1 <- matrix(c(NA,NA,3,8,NA,NA,2,2,1,NA,NA),1,11)

### locf
matplot(t(imputation(mat1,"locf")),type="l",ylim=c(0,10),
   main="locf\n DO NOT USE, BAD METHOD !!!")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### nocb
matplot(t(imputation(mat1,"nocb")),type="l",ylim=c(0,10),
   main="nocb\n DO NOT USE, BAD METHOD !!!")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### trajMean
matplot(t(imputation(mat1,"trajMean")),type="l",ylim=c(0,10),
   main="trajMean")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### trajMedian
matplot(t(imputation(mat1,"trajMedian")),type="l",ylim=c(0,10),
   main="trajMedian")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### trajHotDeck
matplot(t(imputation(mat1,"trajHotDeck")),type="l",ylim=c(0,10),
   main="trajHotDeck 1")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)

### spline
matplot(t(imputation(mat1,"spline",lowerBound=NA,upperBound=NA)),
   type="l",ylim=c(-10,10),main="spline")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16)





##################
### Different linear interpolation

par(mfrow=c(2,2))

### linearInterpol.locf
matplot(t(imputation(mat1,"linearInterpol.locf",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="linearInterpol.locf")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)

### linearInterpol.global
matplot(t(imputation(mat1,"linearInterpol.global",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="linearInterpol.global")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)

### linearInterpol.local
matplot(t(imputation(mat1,"linearInterpol.local",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="linearInterpol.local")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)

### linearInterpol.bisector
matplot(t(imputation(mat1,"linearInterpol.bisector",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="linearInterpol.bisector")
matlines(t(mat1),type="o",col=2,lwd=3,pch=16,lty=1)



##################
### Copy mean

mat3 <- matrix(c(
  NA, 9, 8, 8, 7, 6,NA,
   7, 6,NA,NA,NA, 4,5,
   3, 4, 3,NA,NA, 2,3,
  NA,NA, 1,NA,NA, 1,1),4,7,byrow=TRUE)


par(mfrow=c(2,2))

### copyMean.locf
matplot(t(imputation(mat2,"copyMean.locf",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="copyMean.locf")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### copyMean.global
matplot(t(imputation(mat2,"copyMean.global",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="copyMean.global")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### copyMean.local
matplot(t(imputation(mat2,"copyMean.local",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="copyMean.local")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)

### copyMean.bisector
matplot(t(imputation(mat2,"copyMean.bisector",NA,NA)),type="l",
   ylim=c(-5,10),lty=1,col=1,main="copyMean.bisector")
matlines(t(mat2),type="o",col=2,lwd=3,pch=16,lty=1)




### crossMean
matImp <- imputation(matMissing,method="crossMean")
matplot(t(matImp),col=c(2,1,1,1),lty=c(2,1,1,1),type="l",lwd=c(2,1,1,1),pch=16,
   xlab="Dotted red=imputed trajectory\nFull red=trajectory to impute",
   ylab="",main="Method 'crossMean'")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)


### crossMedian
matImp <- imputation(matMissing,method="crossMedian")
matplot(t(matImp),col=c(2,1,1,1),lty=c(2,1,1,1),type="l",lwd=c(2,1,1,1),pch=16,
   xlab="Dotted red=imputed trajectory\nFull red=trajectory to impute",ylab="",
   main="Method 'crossMedian'")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### crossHotDeck
matImp <- imputation(matMissing,method="crossHotDeck")
matplot(t(matImp),col=c(2,1,1,1),lty=c(2,1,1,1),type="l",lwd=c(2,1,1,1),pch=16,
   xlab="Dotted red=imputed trajectory\nFull red=trajectory to impute",ylab="",
   main="Method 'crossHotDeck'")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)


##################
### Method using trajectory

par(mfrow=c(2,3))
### trajMean
matImp <- imputation(matMissing,method="trajMean")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### trajMedian
matImp <- imputation(matMissing,method="trajMedian")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### trajHotDeck
matImp <- imputation(matMissing,method="trajHotDeck")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### locf
matImp <- imputation(matMissing,method="locf")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="locf")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### nocb
matImp <- imputation(matMissing,method="nocb")
plot(timeV,matImp[1,],type="l",lwd=2,ylim=c(10,30),ylab="",xlab="nocb")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

par(mfrow=c(2,2))

### linearInterpol.locf
matImp <- imputation(matMissing,method="linearInterpol.locf")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### linearInterpol.local
matImp <- imputation(matMissing,method="linearInterpol.local")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### linearInterpol.global
matImp <- imputation(matMissing,method="linearInterpol.global")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)

### linearInterpol.bisector
matImp <- imputation(matMissing,method="linearInterpol.bisector")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)


par(mfrow=c(2,2))

### copyMean.locf
matImp <- imputation(matMissing,method="copyMean.locf")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

### copyMean.local
matImp <- imputation(matMissing,method="copyMean.local")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

### copyMean.global
matImp <- imputation(matMissing,method="copyMean.global")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

### copyMean.bisector
matImp <- imputation(matMissing,method="copyMean.bisector")
plot(timeV,matImp[1,],type="o",ylim=c(0,30),ylab="",xlab="LI-Global")
lines(timeV,matMissing[1,],col=2,type="o",lwd=3)
lines(timeV,moy,col=3,type="o",lwd=3)

par(ask=FALSE)
}

\keyword{package}
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
\keyword{NA}
\keyword{methods}
