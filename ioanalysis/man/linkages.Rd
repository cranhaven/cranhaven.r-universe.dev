\name{linkages}
\alias{linkages}

\title{Backward and Forward Linkages}
\description{Calculates backward and forward linkages with an option to normalize values. Can calculate total and direct linkages. If the data is multiregional, intraregional and interregional linkages can be calculated. Can also be used on a specified subset of all regions.}

\usage{
linkages(io, ES = NULL, regions = "all", sectors = "all", type = c("total"),
         normalize = FALSE, intra.inter = FALSE)
}

\arguments{
  \item{io}{An object of class \code{InputOutput} calculated from \code{\link{as.inputoutput}}.}
  \item{ES}{An object of class \code{EasySelect} from \code{\link{easy.select}}}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
  \item{sectors}{Character or Integer. Specific sectors to be used. Can either be a character that exactly matches the name of the sector in \code{RS_label} or the number of the sector in the order it \code{RS_label}.}
  \item{type}{Character. Identifying the type of backward and forward linkages to be calculated. Options are \code{"total"} and \code{"direct"}.}
  \item{normalize}{Logical. Identifying whether or not to calculate normalized or raw linkages. Default is \code{TRUE}}
  \item{intra.inter}{Logical. Only applies to multiregional systems. Determines whether or not to calculate intraregional and interregional backward and forward linkages in addition to aggregate linkages.}
}

%%%%%%%%%%%%%
%% Details %%
%%%%%%%%%%%%%
\details{There are arguments for \code{type} of linkages, \code{normalized} linkages, and \code{intra.inter} linkages. Let (r) denote the dimension of the block in the transaction matrix of the region of interest and (s) denote the dimension of the rest. If there are (n) sectors and (m) regions then r = n and s = (m - 1)*s

\code{type}: For the following types, if \code{normalize = TRUE} then the calculation takes the specified form below. Otherwise if \code{normalize = FALSE} then the denominator is removed:

\code{"total"} caclculates the total backward and forward linkages. For backward linkages, this is the column sum of the Leontief inverse.
\deqn{BL_{j}=\frac{\sum_{i=1}^{n}l_{ij}}{\frac{1}{n} \sum_{j=1}^{n}\sum_{i=1}^{n}l_{ij}}}
For forward linkages, this is the row sum of the Goshian inverse.
\deqn{FL_{i}=\frac{\frac{1}{n}\sum_{j=1}^{n}g_{ij}}{\frac{1}{n^{2}}\sum_{j=1}^{n}\sum_{i=1}^{n}g_{ij}}}

\code{"direct"} calculates the direct backward and forward linkages. For backward linkages, this is the column sum of the input matrix of technical coefficients (A):
\deqn{BL_{j}=\frac{\sum_{i=1}^{n}a_{ij}}{\frac{1}{n} \sum_{j=1}^{n}\sum_{i=1}^{n}a_{ij}}}
For forward linkages, this is the row sum of the output matrix of technical coefficients (B):
\deqn{FL_{i}=\frac{\frac{1}{n}\sum_{j=1}^{n}b_{ij}}{\frac{1}{n^{2}}\sum_{j=1}^{n}\sum_{i=1}^{n}b_{ij}}}

\code{intra.inter}: This calculates the intraregional, interregional and aggregate backward and forward linkages. If \code{intra.inter = FALSE}, then only calculates the aggregate. If \code{normalize = FALSE} then the aggregate linkage is equivalent to the sum of the intraregional and interregional linkages. If \code{normalize = TRUE}, then this is not the case. Note that normalizing adds the denominator to the following equations. Using matrix notation we have
\deqn{BL.intra = \frac{1_r^\prime J_{rr}}{ \frac{1}{n*m} 1_r^\prime J_{rr} 1_r}}
\deqn{FL.intra = \frac{ J_{rr} 1_r}{ \frac{1}{n*m} 1_r^\prime J_{rr} 1_r}}
\deqn{BL.inter = \frac{1_s^\prime J_{sr}}{\frac{1}{n*m} 1_s J_{sr} 1_r }}
\deqn{FL.inter = \frac{J_{rs} 1_s}{\frac{1}{n*m} 1_r J_{rs} 1_s}}
\deqn{BL.agg = \frac{ 1 J_{.r}}{\frac{1}{n*m} 1 J_{.r} 1_r}}
\deqn{FL.agg = \frac{ J_{r.} 1}{\frac{1}{n*m} 1_r J_{r.}} 1 }


}


\value{
Returns a \code{data.frame}. The following are assigned to the column names to help identify which column is belongs to which. The first element of the column label is the region of interest, grabbed from \code{RS_label}.
\item{.BL}{Backward linkages}
\item{.FL}{Forward linkages}
\item{.intra}{Intraregional linkages}
\item{.inter}{Interregional linkages}
\item{.agg}{Aggregate linkages}
\item{.tot}{Total linkages}
\item{.dir}{Direct linkages}
}
\references{Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})}
\author{
John J. P. Wade, Ignacio Sarmiento-Barbieri
}

\seealso{\code{\link{leontief.inv}}, \code{\link{ghosh.inv}}, \code{\link{key.sector}} }

\examples{
data(toy.IO)
class(toy.IO)
link1 <- linkages(toy.IO)
link1$Hogwarts

data(toy.ES)
class(toy.ES)
link2 <- linkages(toy.IO, toy.ES)
link2

# More detailed
link3 <- linkages(toy.IO, regions = "Narnia", sectors = c("Wii","Pizza"), 
                  type = c("total", "direct"), normalize = FALSE, intra.inter = TRUE)
link3

link4 <- linkages(toy.IO, regions = 1:2, sectors = c(1:3,5))
link4

}
