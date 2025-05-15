\name{holdout}
\alias{holdout}
\title{
Computes indexes for holdout data split into training and test sets.
}
\description{
Computes indexes for holdout data split into training and test sets.
}
\usage{holdout(y, ratio = 2/3, internalsplit = FALSE, mode = "stratified", iter = 1, 
               seed = NULL, window=10, increment=1)
}
\arguments{
  \item{y}{desired target: numeric vector; or factor -- then a stratified holdout is applied (i.e. the proportions of the classes are the same for each set).}
  \item{ratio}{split ratio (in percentage -- sets the training set size; or in total number of examples -- sets the test set size).}
  \item{internalsplit}{if \code{TRUE} then the training data is further split into training and validation sets. The same \code{ratio} parameter is used for the internal split.}
  \item{mode}{sampling mode. Options are:
              \itemize{
                       \item \code{stratified} -- stratified randomized holdout if \code{y} is a factor; else it behaves as standard randomized holdout;
                       \item \code{random} -- standard randomized holdout;
                       \item \code{order} -- static mode, where the first examples are used for training and the later ones for testing (useful for time series data);
                       \item \code{rolling} -- rolling window, also known as sliding window (e.g. useful for stock market prediction), similar to \code{order} except that \code{window} is the window size, \code{iter} is the rolling iteration and \code{increment} is the number of samples slided at each iteration. In each iteration, the training set size is fixed to \code{window}, while the test set size is equal to \code{ratio} except for the last iteration
(where it may be smaller).
                       \item \code{incremental} -- incremental retraining mode, also known as growing windows, similar to \code{order} except that \code{window} is the initial window size, \code{iter} is the incremental iteration and \code{increment} is the number of samples added at each iteration. In each iteration, the training set size grows (+increment), while the test set size is equal to \code{ratio} except
                                                   for the last iteration (where it may be smaller).
                      }
}
  \item{iter}{iteration of the incremental retraining mode (only used when \code{mode="rolling"} or \code{"incremental"}, typically \code{iter} is set within a cycle, see the example below).}
  \item{seed}{if \code{NULL} then no seed is used and the current R randomness is assumed; else a fixed seed is adopted to generate local random sample sequences, returning always the same result for the same seed (local means that it does not affect the state of other random number generations called after this function, see example).}
  \item{window}{training window size (if \code{mode="rolling"}) or initial training window size (if \code{mode="incremental"}).}
  \item{increment}{number of samples added to the training window at each iteration (if \code{mode="incremental"} or \code{mode="rolling"}).}
}
\details{
Computes indexes for holdout data split into training and test sets.
}
\value{
A list with the components:
\itemize{
         \item $tr -- numeric vector with the training examples indexes;
         \item $ts -- numeric vector with the test examples indexes;
         \item $itr -- numeric vector with the internal training examples indexes;
         \item $val -- numeric vector with the internal validation examples indexes;
        }
}
\references{
See \code{\link{fit}}.
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}

\seealso{
\code{\link{fit}}, \code{\link{predict.fit}}, \code{\link{mining}}, \code{\link{mgraph}}, \code{\link{mmetric}}, \code{\link{savemining}}, \code{\link{Importance}}.
}
\examples{
### simple examples:
# preserves order, last two elements go into test set
H=holdout(1:10,ratio=2,internal=TRUE,mode="order")
print(H)
# no seed or NULL returns different splits:
H=holdout(1:10,ratio=2/3,mode="random")
print(H)
H=holdout(1:10,ratio=2/3,mode="random",seed=NULL)
print(H)
# same seed returns identical split:
H=holdout(1:10,ratio=2/3,mode="random",seed=12345)
print(H)
H=holdout(1:10,ratio=2/3,mode="random",seed=12345)
print(H)

### classification example
\dontrun{
data(iris)
# random stratified holdout
H=holdout(iris$Species,ratio=2/3,mode="stratified") 
print(table(iris[H$tr,]$Species))
print(table(iris[H$ts,]$Species))
M=fit(Species~.,iris[H$tr,],model="rpart") # training data only
P=predict(M,iris[H$ts,]) # test data
print(mmetric(iris$Species[H$ts],P,"CONF"))
}

### regression example with incremental and rolling window holdout:
\dontrun{
ts=c(1,4,7,2,5,8,3,6,9,4,7,10,5,8,11,6,9)
d=CasesSeries(ts,c(1,2,3))
print(d) # with 14 examples
# incremental holdout example (growing window)
for(b in 1:4) # iterations
  {
   H=holdout(d$y,ratio=4,mode="incremental",iter=b,window=5,increment=2)
   M=fit(y~.,d[H$tr,],model="mlpe",search=2)
   P=predict(M,d[H$ts,])
   cat("batch :",b,"TR from:",H$tr[1],"to:",H$tr[length(H$tr)],"size:",length(H$tr),
       "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
       "mae:",mmetric(d$y[H$ts],P,"MAE"),"\n")
  }
# rolling holdout example (sliding window)
for(b in 1:4) # iterations
  {
   H=holdout(d$y,ratio=4,mode="rolling",iter=b,window=5,increment=2)
   M=fit(y~.,d[H$tr,],model="mlpe",search=2)
   P=predict(M,d[H$ts,])
   cat("batch :",b,"TR from:",H$tr[1],"to:",H$tr[length(H$tr)],"size:",length(H$tr),
       "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
       "mae:",mmetric(d$y[H$ts],P,"MAE"),"\n")
  }
}

### local seed simple example
\dontrun{
# seed is defined, same sequence for N1 and N2:
# s2 generation sequence is not affected by the holdout call
set.seed(1); s1=sample(1:10,3)
set.seed(1);
N1=holdout(1:10,seed=123) # local seed
N2=holdout(1:10,seed=123) # local seed
print(N1$tr)
print(N2$tr)
s2=sample(1:10,3)
cat("s1:",s1,"\n") 
cat("s2:",s2,"\n") # s2 is equal to s1
}

}
\keyword{ manip }
