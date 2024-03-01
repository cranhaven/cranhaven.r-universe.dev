\name{nonparasccs}
\alias{nonparasccs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spline-based non parametric SCCS method
 %%  ~~function to do ... ~~
}
\description{
Fits a spline-based non parametric SCCS model where both the exposure related relative incidence and age related relative incidence functions are represented by spline functions; that is, linear combinations of M-splines. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nonparasccs(indiv, astart, aend, aevent, adrug, aedrug, kn1=12, kn2=12,
            sp1=NULL, sp2=NULL, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{indiv}{
a vector of individual identifiers of cases. %%     ~~Describe \code{adrug} here~~
}

\item{astart}{
a vector of ages at start of observation periods. 
}

\item{aend}{
a vector of ages at end of observation periods. 
}


\item{aevent}{
a vector of ages at event, an individual can experience multiple events.
}


\item{adrug}{
a vector of  ages at which exposure related risk period starts. %%     ~~Describe \code{adrug} here~~
}
  \item{aedrug}{
a vector of ages at which exposure-related risk ends. %%     ~~Describe \code{aedrug} here~~
}
  
  
  \item{kn1}{
an integer >= 5 representing the number of interior knots used to define the M-spline basis functions which are related to the age specific relative incidence function, usually between 8 and 12 knots is sufficient. It defaults to 12 knots. %%     ~~Describe \code{formula} here~~
}
  \item{kn2}{
a an integer >= 5 representing the number of interior knots used to define the M-spline basis functions which are related to the exposure specific relative incidence function, usually between 8 and 12 knots is sufficient. The default value is 12. %%     ~~Describe \code{formula} here~~
}

\item{sp1}{
smoothing parameter value for age related relative incidence function. It defaults to "NULL" where the smoothing parameter is obtained automatically using an approximate cross-validation method. The value of "sp1" must be a number greater or equal to 0.
}

\item{sp2}{
smoothing parameter value for exposure related relative incidence function. It defaults to "NULL" where the smoothing paramter is obtained automatically using an approximate cross-validation method. The value of "sp1" must be a number greater or equal to 0.
}

  \item{data}{
A data frame containing the input data. %%     ~~Describe \code{data} here~~
}
}
\details{
The smoothing parameters for the age and exposure related relative incidence functions are chosen using a cross-validation method. To visualize the exposure-related relative incidence function, use the plot function. %%  ~~ If necessary, more details than the description above ~~
}
\value{
Relative incidence estimates along with their 95\% confidence intervals.
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{estimates}{exposure related relative incidence estimates at each point of time since start of exposure until the maximum difference between the start and end of exposure.}
  
  \item{timesinceexposure}{time units since the start of exposure.}
  
  \item{lci }{lower confidence limits of the exposure related relative incidence estimates.}
  \item{uci }{upper confidence limits of the exposure related relative incidence estimates.}
%% ...
}
\references{

Ghebremichael-Weldeselassie, Y., Whitaker, H. J., Farrington, C. P. (2016). Flexible modelling of vaccine effects in self-controlled case series models. Biometrical Journal, 58(3):607-622.

Ghebremichael-Weldeselassie, Y., Whitaker, H. J., Farrington, C. P. (2017). Spline-based self controlled case series method. Statistics in Medicine 33:639-649.

Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.

}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. %%  ~~who you are~~
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{smoothagesccs}}, \code{\link{smoothexposccs}}
}
\examples{



# ITP and MMR data 

  \donttest{itp.mod <- nonparasccs(indiv=case, astart=sta, aend=end,
                         aevent=itp, adrug=mmr, aedrug=mmr+42, sp1=28000, sp2=1200,
                         data=itpdat)}

  \donttest{itp.mod}
  
# Plot the exposure and age related relative incidence functions

 \donttest{ plot(itp.mod)}
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Splines}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
