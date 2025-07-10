\name{Dstart}
\alias{Dstart}

\title{
Determine starting values for nonlinear parameter estimation in the water retention models
}
\description{
This function determines the starting values for estimation of parameters in the water retention models. The input data must contain at least the x (suction potential) and y(moisture contents) variables. It gives the starting values as close to the expected estimates as possible.
}
\usage{
Dstart(data)
}

\arguments{
  \item{data}{
This is the water retention data with at least x (suction potential) and y(moisture contents) variables. The number of rows of the data should be at least 4 entries/levels of suction potential
}
}
\details{
The data contains x columns and y columns which are used by the function. The initial x value should not be zero but something close like 0.001
}
\value{
A vector of five values:
pars[1]: thr- The residual moisture content,
pars[2]: ths- The saturated moisture content,
Pars[3]: alp- Inverse of air entry potential,
pars[4]: nscal-index of pore-size distribution, and
pars[5]: mscal-scaling parameter related to nscal
}

\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSkosugi}}, \code{\link{SSvgm}},\code{\link{SSgard}}
}
\examples{
data(isric)
pf=subset(isric, Sample=="Benin3")
Dstart(pf)[1]## thr initial value
}
\keyword{ models }
\keyword{ manip }
