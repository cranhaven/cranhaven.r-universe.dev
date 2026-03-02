\name{apc.fit.model.2s}
\alias{apc.fit.model.2s}
\alias{apc.fit.table.2s}
\title{Fits an age period cohort model for 2 samples}
\description{Generalizes \code{\link{apc.fit.model}} to a 2 sample model. For an application, see the vignette \href{https://users.ox.ac.uk/~nuff0078/apc/vignettes/ReproducingN2025.pdf}{ReproducingN2025.pdf}, \href{https://users.ox.ac.uk/~nuff0078/apc/vignettes/ReproducingN2025.R}{ReproducingN2025.R} on \href{https://users.ox.ac.uk/~nuff0078/apc/index.htm}{vignette}.}
\usage{ apc.fit.model.2s(apc.data.list.1,apc.data.list.2,model.family,
            model.design.common="APC",model.design.difference="APC",
            gls.weight=c(1,1),apc.index=NULL,time.series=NULL)
apc.fit.table.2s(apc.data.list.1,apc.data.list.2,model.family,
            restrict="difference",model.design.reference.common="APC",
            model.design.reference.difference="APC",
            gls.weight=c(1,1),time.series=NULL,digits=3)
}
\arguments{
    \item{apc.data.list.1}{List. For 1st sample. See \code{\link{apc.data.list}} for a description of the format.}
    \item{apc.data.list.2}{List. For 2nd sample. See \code{\link{apc.data.list}} for a description of the format.}
    \item{model.family}{Character.  The following options are implemented.	These are used internally when
		calling \code{\link{glm.fit}}.		
	  	\describe{
			\item{"log.normal.rates"}{Gaussian regression for log(rates) and with identity link (Least Squares).}	
			\item{"gls.log.normal.rates"}{Gaussian regression for log(rates) and with identity link (Generalized Least Squares). The option \code{gls.weight} must be used to give the relative weight of the two samples.}	
			\item{"log.normal.response"}{Gaussian regression for log(response) and with identity link (Least Squares).}	
		}}
    \item{model.design.common}{\emph{Optional}. Indicates which sub-model should be fitted for the \emph{common part} of the parameters, that is for the sum of the canonical parameters (xi.1+xi.2)/2. Possible choices: "APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC" and "ATC". Default is "APC"}
    \item{model.design.difference}{\emph{Optional}. Indicates which sub-model should be fitted for the \emph{difference part} of the parameters, that is for the difference of the canonical parameters (xi.1-xi.2)/2. Possible choices: "APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC" and "ATC". Default is "APC"}
    \item{model.design.reference.common}{\emph{Optional} for \code{apc.fit.table.2s}. Default is "APC"}
    \item{model.design.reference.difference}{\emph{Optional} for \code{apc.fit.table.2s}. Default is "APC"}
    \item{restrict}{\emph{Optional} for \code{apc.fit.table.2s}. Character. Either "difference" or "common". Which type of parameter is restricted? Default is "difference}
    \item{gls.weight}{\emph{Optional}. Vector. Use to set relative weights when estimating by GLS using \code{model.family} set to "gls.log.normal.rates". GLS regression divides log(rates) and design for each sample by respective weights Weights could be their residual standard deviation in 1-sample analysis. Or, Weights could be normalized so that 2nd element is 1 and 1st element is 1-sample residual standard deviation for 1st sample divided by that of 2nd sample deviation for the first sample and 1 for second sample. Default is \code{c(1,1)}.}
    \item{apc.index}{\emph{Optional}. List. See \code{\link{apc.get.index}} for a description of the format. If not provided this is computed internally. If \code{apc.fit.model} is used in a simulation study computational effort can be saved when using this option.}
    \item{time.series}{\emph{Optional}. Vector. Should have same length as the number of periods.  Double differences of period parameters will be restricted to follow double differences of the time series. Should be used with \code{model.design.common} and/or \code{model.design.difference} set to "ATC". Default is NULL.}
    \item{digits}{\emph{Optional}. Numerical. Number of digits in output.
		Default=3.}
}
\references{Nielsen, B. (2022) Two-sample age-period-cohort models with an application to Swiss suicide rates. \emph{Download}: \href{https://www.nuffield.ox.ac.uk/economics/Papers/2022/2022-W03apc_2sample.pdf}{Nuffield Discussion Paper 2022-W03}.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 7 Jul 2025}
\keyword{ package }
\keyword{ models }
\keyword{ regression }
\keyword{ htest }


