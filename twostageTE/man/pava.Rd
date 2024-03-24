\name{pava}
\alias{pava}
\title{
isotonic regression
}
\description{
This is an internal function not meant to be called directly.
Wrapper for gpava in package isotone to apply the pava algorithm for isotonic regression
}
\usage{
pava(explanatory, response, X_0 = NA, Y_0 = NA, w = NA)
}
\arguments{
  \item{explanatory}{
Explanatory sample points
}
  \item{response}{
Observed responses at the explanatory sample points
}
  \item{X_0}{
can ignore
}
  \item{Y_0}{
can ignore
}
  \item{w}{
weights if given repeated observations at same explanatory point 
}
}
\value{
   return(list(x = explanatory, y = response_fit, response_obs = response))
List with 
  \item{x}{Explanatory sample points}
  \item{y}{estimated isotonic regression values}
  \item{response_obs}{Observed responses at the explanatory sample points}
}
\references{
de Leeuw J, Hornik K, Mair P (2009). 'Isotone Optimization in R: Pool-Adjacent-Violators
Algorithm (PAVA) and Active Set Methods.' Journal of Statistical Software, 32(5), 1-24.
ISSN 1548-7660. URL http://www.jstatsoft.org/v32/i05.
}
\author{
Shawn Mankad
}

\examples{
X=runif(25, 0,1)
Y=X^2+rnorm(n=length(X), sd=0.1)
pava(X, Y, 0.25, 0.5)

## The function is currently defined as
function (explanatory, response, X_0 = NA, Y_0 = NA, w = NA) 
{
    require(isotone)
    if (is.na(w)) 
        w = rep(1, length(explanatory))
    ind = order(explanatory, decreasing = FALSE)
    if (sum(diff(ind) < 0) != 0) {
        explanatory = explanatory[ind]
        response = response[ind]
    }
    if (is.na(X_0) && is.na(Y_0)) {
        fit = gpava(explanatory, response)
        response_fit = fit$x
    }
    else if (is.na(X_0) || is.na(Y_0)) {
        warning("Only X_0 or only Y_0 was supplied. Please check arguments.")
    }
    else {
        n = length(explanatory)
        if (sum(response < Y_0) == n && sum(explanatory < X_0) == 
            n) {
            warning("Warning: X_0 and Y_0 are outside observed region")
            fit = gpava(explanatory, response)
            response_fit = fit$x
        }
        else if (sum(response < Y_0) == n && sum(explanatory < 
            X_0) == 0) {
            warning("Warning: X_0 and Y_0 are outside observed region")
            return(list(x = explanatory, y = rep(Y_0, n), y_compressed = rep(Y_0, 
                n)))
        }
        else if (sum(response < Y_0) == n) {
            warning("Warning: Y_0 is outside observed region")
            n2 = n - sum(explanatory < X_0)
            y1 = response[explanatory < X_0]
            x1 = explanatory[explanatory < X_0]
            fit = gpava(x1, y1)
            response_fit = c(sapply(fit$x, min, Y_0), rep(Y_0, 
                n2))
        }
        else if (sum(response >= Y_0) == n && sum(explanatory < 
            X_0) == n) {
            warning("Warning: X_0 and Y_0 are outside observed region")
            return(list(x = explanatory, y = rep(Y_0, n), y_compressed = rep(Y_0, 
                n)))
        }
        else if (sum(response >= Y_0) == n && sum(explanatory < 
            X_0) == 0) {
            warning("Warning: X_0 and Y_0 are outside observed region")
            fit = gpava(explanatory, response)
            response_fit = fit$x
        }
        else if (sum(response >= Y_0) == n) {
            warning("Warning: Y_0 is outside observed region")
            n2 = n - sum(explanatory > X_0)
            y1 = response[explanatory > X_0]
            x1 = explanatory[explanatory > X_0]
            fit = gpava(x1, y1)
            response_fit = c(rep(Y_0, n2), sapply(fit$x, max, 
                Y_0))
        }
        else if (sum(explanatory < X_0) == n) {
            warning("Warning: X_0 is outside observed region")
            fit = gpava(explanatory, response)
            response_fit = sapply(fit$x, min, Y_0)
        }
        else if (sum(explanatory < X_0) == 0) {
            warning("Warning: X_0 is outside observed region")
            fit = gpava(explanatory, response)
            response_fit = sapply(fit$x, max, Y_0)
        }
        else {
            y1 = response[explanatory < X_0]
            x1 = explanatory[explanatory < X_0]
            y2 = response[explanatory >= X_0]
            x2 = explanatory[explanatory >= X_0]
            fit1 = gpava(x1, y1)
            fit2 = gpava(x2, y2)
            response_fit = c(sapply(fit1$x, min, Y_0), sapply(fit2$x, 
                max, Y_0))
        }
    }
    return(list(x = explanatory, y = response_fit, response_obs = response))
  }
}

