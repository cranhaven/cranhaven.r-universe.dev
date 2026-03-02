\name{apc.plot.fit.2s}
\alias{apc.plot.fit.2s}
\title{Plots of apc estimates for 2 sample model}
\description{Generalizes \code{\link{apc.plot.fit}} to a 2 sample model. For an application, see the vignette \href{https://users.ox.ac.uk/~nuff0078/apc/vignettes/ReproducingN2025.pdf}{ReproducingN2025.pdf}, \href{https://users.ox.ac.uk/~nuff0078/apc/vignettes/ReproducingN2025.R}{ReproducingN2025.R} on \href{https://users.ox.ac.uk/~nuff0078/apc/index.htm}{vignette}.}
\usage{ apc.plot.fit.2s(apc.fit.model.1,apc.fit.model.2,
            type="macro",which.plot=0,
            col=NULL,label=NULL,lty=NULL)
}
\arguments{
    \item{apc.fit.model.1}{List. See \code{\link{apc.fit.model}} for a description of the format.}
    \item{apc.fit.model.2}{List. See \code{\link{apc.fit.model}} for a description of the format.}
    \item{type           }{Optional. Default is "macro".
        \describe{
            \item{"macro"}{If \code{which.plot} is omitted or set to 0 six plots will be made. Single plot of one of the fits can achieved by giving \code{which.plot} one of the following values. 
                1: both micro age    fits;
                2: both micro period fits;
                3: both micro cohort fits;
                4: difference of micro age    fits;
                5: difference of micro period fits;
                6: difference of micro cohort fits.}
            \item{"macro-detrend"}{Same as \code{macro}.}
            \item{"micro"}{The micro effects only exist when the data have mixed frequency as coded in \code{data.format}. Thus, there are age and period in groups of length G and H respectively, this plots demeaned micro fits as well as the cross-sample difference. Some micro effects do not exist if G=1 or H=1. Single plot of one of the fits can achieved by giving \code{which.plot} one of the following values. 
                1: both micro age    fits if G>1;
                2: both micro period fits if H>1;
                3: both micro cohort fits if G*H>1;
                4: difference of micro age    fits if G>1;
                5: difference of micro period fits if H>1;
                6: difference of micro cohort fits if G*H>1.
                If \code{data.format} is regular nothing is plotted as there are no micro fits.}
        }
    }
    \item{which.plot     }{Optional. If 0, then more than one plot may be made. For the numbering of plots, see type. Default is 0.} 
    \item{col            }{NULL}
    \item{label          }{NULL}
    \item{lty            }{NULL}
}
\references{ Nielsen, B. (2022) Two-sample age-period-cohort models with an application to Swiss suicide rates. \emph{Download}: \href{https://www.nuffield.ox.ac.uk/economics/Papers/2022/2022-W03apc_2sample.pdf}{Nuffield Discussion Paper 2022-W03}.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 3 Jul 2025}

