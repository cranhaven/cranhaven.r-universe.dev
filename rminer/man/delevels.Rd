\name{delevels}
\alias{delevels}
\title{
Reduce, replace or transform levels of a data.frame or factor variable (useful for preprocessing datasets).
}
\description{
Reduce, replace or transform levels of a data.frame or factor variable (useful for preprocessing datasets).
}
\usage{
delevels(x, levels, label = NULL)
}
\arguments{
  \item{x}{\code{\link{factor}} with several levels or a \code{\link{data.frame}}. If a \code{data.frame}, then all factor attributes are transformed.}
  \item{levels}{character vector with several options: 
                 \itemize{
                  \item \code{idf} -- factor is transformed into a numeric vector using IDF transform. 
                  \item \code{pcp} or \code{c("pcp",perc)} -- factor is transformed using PCP transform. If perc is not provided, the default 0.1 value is used.
                  \item any other values -- all level values are merged into a single factor level according to \code{label}.
                         }
                Another possibility is to define a vector list, with \code{levels[[i]]} values for each \code{factor} of the \code{data.frame} (see example).
               }
  \item{label}{the new label used for all \code{levels} examples (if \code{NULL} then \code{"_OTHER"} is assumed).}
}
\details{ The Inverse Document Frequency (IDF) uses f(x)= log(n/f_x), where n is the length of x and f_x is the frequency of x.\cr
          The Percentage Categorical Pruned (PCP) merges all least frequent levels (summing up to perc percent) into a single level.\cr
          When other values are used for \code{levels}, this function replaces all \code{levels} values with the single \code{label} value.
}
\value{
Returns a transformed factor or data.frame.
}
\references{
\itemize{
\item PCP transform:\cr
L.M. Matos, P. Cortez, R. Mendes, A. Moreau.\cr
Using Deep Learning for Mobile Marketing User Conversion Prediction. 
In Proceedings of the IEEE International Joint Conference on Neural Networks (IJCNN 2019), 
paper N-19327, Budapest, Hungary, July, 2019 (8 pages), IEEE, ISBN 978-1-7281-2009-6.\cr
\doi{10.1109/IJCNN.2019.8851888}\cr
\url{http://hdl.handle.net/1822/62771}\cr

\item IDF transform:\cr
L.M. Matos, P. Cortez, R. Mendes and A. Moreau.\cr
A Comparison of Data-Driven Approaches for Mobile Marketing User Conversion Prediction.
In Proceedings of 9th IEEE International Conference on Intelligent Systems (IS 2018), pp. 140-146,
Funchal, Madeira, Portugal, September, 2018, IEEE, ISBN 978-1-5386-7097-2.\cr
\url{https://ieeexplore.ieee.org/document/8710472}\cr
\url{http://hdl.handle.net/1822/61586}\cr
}
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\seealso{
\code{\link{fit}} and \code{\link{imputation}}.
}
\examples{
### simples examples:
f=factor(c("A","A","B","B","C","D","E"))
print(table(f))
# replace "A" with "a":
f1=delevels(f,"A","a")
print(table(f1))
# merge c("C","D","E") into "CDE":
f2=delevels(f,c("C","D","E"),"CDE")
print(table(f2))
# merge c("B","C","D","E") into _OTHER:
f3=delevels(f,c("B","C","D","E"))
print(table(f3))

\dontrun{
# larger factor:
x=factor(c(1,rep(2,2),rep(3,3),rep(4,4),rep(5,5),rep(10,10),rep(100,100)))
print(table(x))
# IDF: frequent values are close to zero and
# infrequent ones are more close to each other:
x1=delevels(x,"idf")
print(table(x1))
# PCP: infrequent values are merged
x2=delevels(x,c("pcp",0.1)) # around 10%
print(table(x2))

# example with a data.frame:
y=factor(c(rep("a",100),rep("b",20),rep("c",5)))
z=1:125 # numeric
d=data.frame(x=x,y=y,z=z,x2=x)
print(summary(d))

# IDF:
d1=delevels(d,"idf")
print(summary(d1))
# PCP:
d2=delevels(d,"pcp")
print(summary(d2))
# delevels:
L=vector("list",ncol(d)) # one per attribute
L[[1]]=c("1","2","3","4","5")
L[[2]]=c("b","c")
L[[4]]=c("1","2","3") # different on purpose
d3=delevels(d,levels=L,label="other")
print(summary(d3))
} # end dontrun 

}
\keyword{ manip }
