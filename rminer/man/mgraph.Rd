\name{mgraph}
\alias{mgraph}
\title{
Mining graph function
}
\description{
Plots a graph given a \code{\link{mining}} list, list of several mining lists or given the pair y - target and x - predictions.
}
\usage{
mgraph(y, x = NULL, graph, leg = NULL, xval = -1, PDF = "", PTS = -1, 
       size = c(5, 5), sort = TRUE, ranges = NULL, data = NULL,
       digits = NULL, TC = -1, intbar = TRUE, lty = 1, col = "black",
       main = "", metric = "MAE", baseline = FALSE, Grid = 0, 
       axis = NULL, cex = 1)
}
\arguments{
  \item{y}{if there are predictions (\code{!is.null(x)}), \code{y} should be a numeric vector or factor with the target desired responses (or output values).\cr
         Else, \code{y} should be a list returned by the  \code{\link{mining}} function or a vector list with several mining lists.
          }
  \item{x}{the predictions (should be a numeric vector if \code{task="reg"}, matrix if \code{task="prob"} or factor if \code{task="class"} (use if \code{y} is not a list).}

  \item{graph}{type of graph. Options are:
               \itemize{
                       \item \code{ROC} -- ROC curve (classification);
                       \item \code{LIFT} -- LIFT accumulative curve (classification);
                       \item \code{IMP} -- relative input importance barplot;
                       \item \code{REC} -- REC curve (regression);
                       \item \code{VEC} -- variable effect curve;
                       \item \code{RSC} -- regression scatter plot;
                       \item \code{REP} -- regression error plot;
                       \item \code{REG} -- regression plot;
                       \item \code{DLC} -- distance line comparison (for comparing errors in one line);
                      }
}
  \item{leg}{legend of graph:
             \itemize{
                      \item if \code{NULL} -- not used;
                      \item if -1 and \code{graph="ROC" or "LIFT"}  -- the target class name is used;
                      \item if -1 and \code{graph="REG"}  -- \code{leg=c("Target","Predictions")};
                      \item if -1 and \code{graph="RSC"}  -- \code{leg=c("Predictions")};
                      \item if vector with "character" type (text) -- the text of the legend;
                      \item if is list -- \code{$leg} = vector with the text of the legend and \code{$pos} is the position of the legend (e.g. "top" or c(4,5));
                     }
}
  \item{xval}{auxiliary value, used by some graphs:
              \itemize{
                        \item \code{VEC} -- if -1 means perform several 1-D sensitivity analysis VEC curves, one for each attribute, if >0 means the attribute index (e.g. 1).
                        \item \code{ROC} or \code{LIFT} or \code{REC} -- if -1 then \code{xval=1}. For these graphs, \code{xval} is the maximum x-axis value.
                        \item \code{IMP} -- \code{xval} is the x-axis value for the legend of the attributes.
                        \item \code{REG} -- \code{xval} is the set of plotted examples (e.g. 1:5), if -1 then all examples are used.
                        \item \code{DLC} -- \code{xval} is the \code{val} of the \code{\link{mmetric}} function.
                      }
}
  \item{PDF}{if \code{""} then the graph is plotted on the screen, else the graph is saved into a pdf file with the name set in this argument.}
  \item{PTS}{number of points in each line plot. If -1 then \code{PTS=11} (for \code{ROC}, \code{REC} or \code{LIFT}) or \code{PTS=6} (\code{VEC}).}
  \item{size}{size of the graph, c(width,height), in inches.}
  \item{sort}{if TRUE then sorts the data (works only for some graphs, e.g. \code{VEC}, \code{IMP}, \code{REP}).}
  \item{ranges}{matrix with the attribute minimum and maximum ranges (only used by \code{VEC}). }
  \item{data}{the training data, for plotting histograms and getting the minimum and maximum attribute ranges if not defined in ranges (only used by \code{VEC}).}
  \item{digits}{the number of digits for the axis, can also be defined as c(x-axis digits,y-axis digits) (only used by \code{VEC}).}

  \item{TC}{target class (for multi-class classification class) from 1 to \emph{Nc}, where \emph{Nc} is the number of classes. If multi-class and TC==-1 then TC is set to the index of the last class.}

  \item{intbar}{if 95\% confidence interval bars (according to t-student distribution) should be plotted as whiskers.}
  \item{lty}{the same \code{lty} argument of the \code{\link{par}} function.}
  \item{col}{color, as defined in the \code{\link{par}} function.}
  \item{main}{the title of the graph, as defined in the \code{\link{plot}} function.}
  \item{metric}{the error metric, as defined in \code{\link{mmetric}} (used by \code{DLC}).}
  \item{baseline}{if the baseline should be plotted (used by \code{ROC} and \code{LIFT}).}
  \item{Grid}{if >1 then there are GRID light gray squared grid lines in the plot.}
  \item{axis}{Currently only used by \code{IMP}: numeric vector with the axis numbers (1 -- bottom, 3 -- top). If \code{NULL} then \code{axis=c(1,3)}.}
  \item{cex}{label font size}
}
\details{
Plots a graph given a \code{\link{mining}} list, list of several mining lists or given the pair y - target and x - predictions.
}
\value{
A graph (in screen or pdf file).
}

\references{
\itemize{
\item To check for more details about rminer and for citation purposes:\cr
P. Cortez.\cr
Data Mining with Neural Networks and Support Vector Machines Using the R/rminer Tool.\cr
In P. Perner (Ed.), Advances in Data Mining - Applications and Theoretical Aspects 10th Industrial Conference on Data Mining (ICDM 2010), Lecture Notes in Artificial Intelligence 6171, pp. 572-583, Berlin, Germany, July, 2010. Springer. ISBN: 978-3-642-14399-1.\cr
@Springer: \url{https://link.springer.com/chapter/10.1007/978-3-642-14400-4_44}\cr
\url{http://www3.dsi.uminho.pt/pcortez/2010-rminer.pdf}\cr

\item This tutorial shows additional code examples:\cr
P. Cortez.\cr
A tutorial on using the rminer R package for data mining tasks.\cr
Teaching Report, Department of Information Systems, ALGORITMI Research Centre, Engineering School, University of Minho, Guimaraes, 
Portugal, July 2015.\cr
\url{http://hdl.handle.net/1822/36210}\cr
}
}

\author{ Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/} }
\note{See also \url{http://hdl.handle.net/1822/36210} and \url{http://www3.dsi.uminho.pt/pcortez/rminer.html}}

\seealso{
\code{\link{fit}}, \code{\link{predict.fit}}, \code{\link{mining}}, \code{\link{mmetric}}, \code{\link{savemining}} and \code{\link{Importance}}.
}

\examples{
### regression
y=c(1,5,10,11,7,3,2,1);x=rnorm(length(y),0,1.0)+y
mgraph(y,x,graph="RSC",Grid=10,col=c("blue"))
mgraph(y,x,graph="REG",Grid=10,lty=1,col=c("black","blue"),
       leg=list(pos="topleft",leg=c("target","predictions")))
mgraph(y,x,graph="REP",Grid=10)
mgraph(y,x,graph="REP",Grid=10,sort=FALSE)
x2=rnorm(length(y),0,1.2)+y;x3=rnorm(length(y),0,1.4)+y;
L=vector("list",3); pred=vector("list",1); test=vector("list",1);
pred[[1]]=y; test[[1]]=x; L[[1]]=list(pred=pred,test=test,runs=1)
test[[1]]=x2; L[[2]]=list(pred=pred,test=test,runs=1)
test[[1]]=x3; L[[3]]=list(pred=pred,test=test,runs=1)
# distance line comparison graph:
mgraph(L,graph="DLC",metric="MAE",leg=c("x1","x2","x3"),main="MAE errors")

# new REC multi-curve single graph with NAREC (normalized Area of REC) values
# for maximum tolerance of val=0.5 (other val values can be used)
e1=mmetric(y,x,metric="NAREC",val=5)
e2=mmetric(y,x2,metric="NAREC",val=5)
e3=mmetric(y,x3,metric="NAREC",val=5)
l1=paste("x1, NAREC=",round(e1,digits=2))
l2=paste("x2, NAREC=",round(e2,digits=2))
l3=paste("x3, NAREC=",round(e3,digits=2))
mgraph(L,graph="REC",leg=list(pos="bottom",leg=c(l1,l2,l3)),main="REC curves")

### regression example with mining
\dontrun{
data(sin1reg)
M1=mining(y~.,sin1reg[,c(1,2,4)],model="mr",Runs=5)
M2=mining(y~.,sin1reg[,c(1,2,4)],model="mlpe",nr=3,maxit=50,size=4,Runs=5,feature="simp")
L=vector("list",2); L[[1]]=M2; L[[2]]=M1
mgraph(L,graph="REC",xval=0.1,leg=c("mlpe","mr"),main="REC curve")
mgraph(L,graph="DLC",metric="TOLERANCE",xval=0.01,
       leg=c("mlpe","mr"),main="DLC: TOLERANCE plot")
mgraph(M2,graph="IMP",xval=0.01,leg=c("x1","x2"),
       main="sin1reg Input importance",axis=1)
mgraph(M2,graph="VEC",xval=1,main="sin1reg 1-D VEC curve for x1")
mgraph(M2,graph="VEC",xval=1,
       main="sin1reg 1-D VEC curve and histogram for x1",data=sin1reg)
}

### classification example
\dontrun{
data(iris)
M1=mining(Species~.,iris,model="rpart",Runs=5) # decision tree (DT)
M2=mining(Species~.,iris,model="ksvm",Runs=5) # support vector machine (SVM)
L=vector("list",2); L[[1]]=M2; L[[2]]=M1
mgraph(M1,graph="ROC",TC=3,leg=-1,baseline=TRUE,Grid=10,main="ROC")
mgraph(M1,graph="ROC",TC=3,leg=-1,baseline=TRUE,Grid=10,main="ROC",intbar=FALSE)
mgraph(L,graph="ROC",TC=3,leg=c("SVM","DT"),baseline=TRUE,Grid=10,
       main="ROC for virginica")
mgraph(L,graph="LIFT",TC=3,leg=list(pos=c(0.4,0.2),leg=c("SVM","DT")),
       baseline=TRUE,Grid=10,main="LIFT for virginica")
}

}
\keyword{aplot}
\keyword{classif}
\keyword{regression}
\keyword{neural}
\keyword{nonlinear}
