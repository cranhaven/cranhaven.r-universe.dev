\name{f2apply}
\alias{f2apply}
\title{
Apply a two-variable function on two fuzzy numbers
}
\description{
Suppose that we are going to put two fuzzy numbers \eqn{x} and \eqn{y} into the monotonic two-variable function \eqn{f(x,y)}. A usual approach is using Zadeh's extension Principle which has a complex computation.
Function \code{f2apply} applies easily two fuzzy numbers to a monotonic two-variable function. Although the theory of \code{f2apply} computation is based on the Zadeh's extension Principle, but it works with the \eqn{\alpha}-cuts of two inputted fuzzy numbers for all \eqn{\alpha \in (0,1]}. It must be mentioned that the ability of computing \eqn{\alpha}-cuts of the result is added to the Version 2.0.
}
\usage{
f2apply(x, y, fun, knot.n=10, I.O.plot="TRUE", ...)
}
\arguments{
  \item{x}{
the first fuzzy number, which must be according to the format of \code{FuzzyNumbers} package
}
  \item{y}{
the second fuzzy number, which must be according to the format of \code{FuzzyNumbers} package
}
  \item{fun}{
a two-variable function which is monotone function on the supports of \code{x} and \code{y} fuzzy numbers
}
  \item{knot.n}{
the number of knots; see package \code{FuzzyNumbers}
}
  \item{I.O.plot}{
a logical argument with default \code{TRUE}. If \code{I.O.plot=TRUE}, then three membership functions of \eqn{x}, \eqn{y} (Inputted fuzzy numbers) and \eqn{f(x,y)} (Outputted fuzzy number) are drawn in a figure. If \code{I.O.plot=FALSE}, then just the membership function of Outputted fuzzy number \eqn{f(x,y)} will be shown in figure.
}
  \item{\dots}{
additional arguments passed from \code{plot}
}
}
\value{
This function returns piecewise linear fuzzy number \eqn{f(x,y)} and also plot the result.
\item{fun.rep }{describes the monotonic behavior of the considered function}
\item{cuts }{returns the \eqn{\alpha}-cuts of the computed fuzzy number \eqn{f(x,y)} }
\item{core }{returns the core of the computed fuzzy number \eqn{f(x,y)} }
\item{support }{returns the support of the computed fuzzy number \eqn{f(x,y)} }
}

\references{
Gagolewski, M., Caha, J., FuzzyNumbers Package: Tools to Deal with Fuzzy Numbers in R. R package version 0.4-1, 2015. https://cran.r-project.org/web/packages=FuzzyNumbers

Klir, G.J., Yuan, B., Fuzzy Sets and Fuzzy Logic: Theory and Applications, Prentice Hall PTR, New Jersey (1995).

Viertl, R., Statistical methods for fuzzy data. New York: John Wiley & Sons (2011)

Zadeh, L.A., Fuzzy sets. Information and Control 8, 338-359 (1965)

Zadeh, L.A., Probability measures of fuzzy events. Journal of Mathematical Analysis and Applications 23, 421-427 (1968)
}
\author{
Abbas Parchami
}
\note{
\code{f2apply} is an extended version of \code{fapply} from package \code{FuzzyNumbers}. The duty of functions \code{fapply} and \code{f2apply} are applying one-variable and two-variable function on fuzzy numbers. 
Two imported fuzzy numbers into \code{f2apply} must be piecewised by \code{PiecewiseLinearFuzzyNumber} function in package \code{FuzzyNumbers}. Moreover, the considered function \eqn{f(x,y)} must be monotone on \eqn{x} and \eqn{y}. 
}


\seealso{
See \code{PiecewiseLinearFuzzyNumber}, \code{as.PiecewiseLinearFuzzyNumber} and \code{piecewiseLinearApproximation} from package \code{FuzzyNumbers}.
}
\examples{

library(FuzzyNumbers)   # For Loud 'FuzzyNumbers' package, after its instalation

# Example 1: Four different cases of function (in respect to increasing/decreasing on x and y)
x = TriangularFuzzyNumber(1,2,5)
y = TrapezoidalFuzzyNumber(3,4,5,6)

g1 = function(x,y) 2*x+y
f2apply(x, y, g1, knot.n=5, type="l", I.O.plot=TRUE)
f2apply(x, y, g1, knot.n=10, xlim=c(0,18), col=4, type="b", I.O.plot=FALSE)
plot(2*x+y, col=2, lty=4, lwd=3, add=TRUE) #Compare the result from "FuzzyNumbers" package

g2 = function(x,y) -2*pnorm(x)+y
f2apply(x, y, g2, type="b")

g3 = function(x,y) 2*x-punif(y, min=1, max=8)
f2apply(x, y, g3, type="l")

g4 = function(x,y) -2*x-y^3
f2apply(x, y, g4, knot.n=20, type="b" )



# Example 2: 
knot.n = 10
A <- FuzzyNumber(-1, .5, 1, 3,
  lower=function(alpha) qbeta(alpha,0.4,3),
  upper=function(alpha) (1-alpha)^4
)
B = PowerFuzzyNumber(1,2,2.5,4, p.left=2, p.right=0.5)
f2apply(A, B, function(x,y) -2*x-y^3, knot.n=knot.n, type="l", col=2, lty=5, lwd=3, I.O.plot=FALSE)
f2apply(A, B, function(x,y) -2*x-y^3, knot.n=knot.n, type="l", col=2, lty=5, lwd=3)

# As another example, change the function and work with the cuts of result:
Result <- f2apply(A, B, function(x,y) abs(y+x-10),knot.n=knot.n,type="l",I.O.plot=TRUE,col=3,lwd=2)
Result
class(Result)

#The result of alphacut for alpha=0.444:
Result$cuts["0.444",] #Or equivalently,  
Result$cuts[6,]  

# Upper bounds of alphacuts:
Result$cuts[,"U"] #Or equivalently,  
Result$cuts[,2]

#The core of the result:
Result$core  

# The support of the result:
Result$support # Or, equivalently:  Result$s


# Example 3: 
knot.n = 10
x = PowerFuzzyNumber(0,1,1,1.3, p.left=1, p.right=1) 
y = PowerFuzzyNumber(3,4,4,6, p.left=1, p.right=1) 
f = function(x,y) 3*x - 2*y
f2apply(x, y, f, knot.n=knot.n, type="l", I.O.plot=TRUE)

g = function(x,y) exp(x^2) + 3*log(sqrt(y+4))
f2apply(x, y, g, knot.n=knot.n, type="l", I.O.plot=TRUE)


# Example 4: 
knot.n = 20
A = PowerFuzzyNumber(.1,.5,.5,.6, p.left=2, p.right=0.5)
B <- FuzzyNumber(.5, .6, .7, .9,
  lower=function(alpha) qbeta(alpha,0.4,3),
  upper=function(alpha) (1-alpha)^4
)
fun1 <- function(x,y) qnorm(x)-qgamma(y,2,4)
f2apply(A, B, fun1, knot.n=knot.n, type="l", I.O.plot=TRUE, col=2, lwd=2)

fun2 <- function(x,y) 0.3*sin(qnorm(x))+tan(qgamma(y,2,4))
f2apply(A, B, fun2, knot.n=knot.n, type="l", I.O.plot=TRUE)


# Example 5: It may be one of considered inputs are crisp.
knot.n = 10
A = 27
B = PowerFuzzyNumber(1,2,2.5,4, p.left=2, p.right=0.5)
f2apply(A, B, function(x,y) -2*x-y^3, knot.n=knot.n, I.O.plot=TRUE)

f2apply(x=4, y=3, function(x,y) sqrt(x)*y^2, knot.n=knot.n, I.O.plot=TRUE)
f2apply(x=4, y=TriangularFuzzyNumber(2,3,5), function(x,y) sqrt(x)-y^2,knot.n=knot.n,I.O.plot=TRUE)
f2apply(x=TriangularFuzzyNumber(2,4,6), y=3, function(x,y) sqrt(x)-y^2,knot.n=knot.n,I.O.plot=TRUE)
f2apply(x=TriangularFuzzyNumber(2,4,6), y=TriangularFuzzyNumber(2,3,5), function(x,y) sqrt(x)-y^2,
        knot.n=knot.n, I.O.plot=TRUE)


## The function is currently defined as
function (x, y, fun, knot.n = 10, I.O.plot = "TRUE", ...) 
{
    x.input <- x
    y.input <- y
    if (class(x) == "numeric") {
        x <- x.input.fuzzy <- TriangularFuzzyNumber(x, x, x)
    }
    if (class(x) == "TriangularFuzzyNumber" | class(x) == "TrapezoidalFuzzyNumber") {
        x.input.fuzzy <- x
        x <- as.PiecewiseLinearFuzzyNumber(x, knot.n)
    }
    if (class(x) == "FuzzyNumber" | class(x) == "PowerFuzzyNumber" |
        class(x) == "PiecewiseLinearFuzzyNumber"  ){
        x.input.fuzzy <- x
        x <- piecewiseLinearApproximation(x, method = "Naive")
    }
    if (class(y) == "numeric") {
        y <- y.input.fuzzy <- TriangularFuzzyNumber(y, y, y)
    }
    if (class(y) == "TriangularFuzzyNumber" | class(y) == "TrapezoidalFuzzyNumber") {
        y.input.fuzzy <- y
        y <- as.PiecewiseLinearFuzzyNumber(y, knot.n)
    }
    if (class(y) == "FuzzyNumber" | class(y) == "PowerFuzzyNumber" | 
        class(y) == "PiecewiseLinearFuzzyNumber"  ){
        y.input.fuzzy <- y
        y <- piecewiseLinearApproximation(y, method = "Naive")
    }
    step.x = length(supp(x))/30
    step.y = length(supp(y))/30
    if (class(x.input) == "numeric") {
        is.inc.on.x <- TRUE
        is.dec.on.x <- FALSE
    }
    else {
        is.inc.on.x = is.increasing.on.x(fun, x.bound = supp(x), 
            y.bound = supp(y), step.x)
        is.dec.on.x = is.decreasing.on.x(fun, x.bound = supp(x), 
            y.bound = supp(y), step.x)
    }
    if (class(y.input) == "numeric") {
        is.inc.on.y <- TRUE
        is.dec.on.y <- FALSE
    }
    else {
        is.inc.on.y = is.increasing.on.y(fun, x.bound = supp(x), 
            y.bound = supp(y), step.y)
        is.dec.on.y = is.decreasing.on.y(fun, x.bound = supp(x), 
            y.bound = supp(y), step.y)
    }
    if ((is.inc.on.x == TRUE) & (is.inc.on.y == TRUE)) {
        fun.rep = "fun is an increasing function from x and y on introduced bounds"
        L.result = fun(alphacut(x.input.fuzzy, seq(0, 1, len = knot.n))[, 
            "L"], alphacut(y.input.fuzzy, seq(0, 1, len = knot.n))[, 
            "L"])
        U.result = fun(alphacut(x.input.fuzzy, seq(0, 1, len = knot.n))[, 
            "U"], alphacut(y.input.fuzzy, seq(0, 1, len = knot.n))[, 
            "U"])
        result = c(L.result, U.result[length(U.result):1])
    }
    else {
        if ((is.dec.on.x == TRUE) & (is.inc.on.y == TRUE)) {
    fun.rep = "fun is a decreasing function on x and increasing function on y on introduced bounds"
            L.result = fun(alphacut(x.input.fuzzy, seq(0, 1, 
                len = knot.n))[, "U"], alphacut(y.input.fuzzy, 
                seq(0, 1, len = knot.n))[, "L"])
            U.result = fun(alphacut(x.input.fuzzy, seq(0, 1, 
                len = knot.n))[, "L"], alphacut(y.input.fuzzy, 
                seq(0, 1, len = knot.n))[, "U"])
            result = c(L.result, U.result[length(U.result):1])
        }
        else {
            if ((is.inc.on.x == TRUE) & (is.dec.on.y == TRUE)) {
   fun.rep = "fun is an increasing function on x and decreasing function on y on introduced bounds"
                L.result = fun(alphacut(x.input.fuzzy, seq(0, 
                  1, len = knot.n))[, "L"], alphacut(y.input.fuzzy, 
                  seq(0, 1, len = knot.n))[, "U"])
                U.result = fun(alphacut(x.input.fuzzy, seq(0, 
                  1, len = knot.n))[, "U"], alphacut(y.input.fuzzy, 
                  seq(0, 1, len = knot.n))[, "L"])
                result = c(L.result, U.result[length(U.result):1])
            }
            else {
                if ((is.dec.on.x == TRUE) & (is.dec.on.y == TRUE)) {
                  fun.rep = "fun is a decreasing function from x and y on introduced bounds"
                  L.result = fun(alphacut(x.input.fuzzy, seq(0, 
                    1, len = knot.n))[, "U"], alphacut(y.input.fuzzy, 
                    seq(0, 1, len = knot.n))[, "U"])
                  U.result = fun(alphacut(x.input.fuzzy, seq(0, 
                    1, len = knot.n))[, "L"], alphacut(y.input.fuzzy, 
                    seq(0, 1, len = knot.n))[, "L"])
                  result = c(L.result, U.result[length(U.result):1])
                }
                else {
                  return(print("fun is not a monoton function on x and y for the introduced bounds.
                                Therefore this function is not appliable for computation."))
                }
            }
        }
    }
    if (class(x.input) == "numeric" | class(y.input) == "numeric") {
        fun.rep = "supports of one/both inputted points are crisp and the exact report on function
                   is not needed"
    }
    Alphacuts = c(seq(0, 1, len = knot.n), seq(1, 0, len = knot.n))
    if (I.O.plot == TRUE) {
        op <- par(mfrow = c(3, 1))
        if (class(x.input) == "numeric") {
            plot(TriangularFuzzyNumber(x.input, x.input, x.input), 
                ylab = "membership func. of x")
        }
        else {
            plot(x.input, ylab = "membership func. of x")
        }
        if (class(y.input) == "numeric") {
            plot(TriangularFuzzyNumber(y.input, y.input, y.input), 
                xlab = "y", ylab = "membership func. of y")
        }
        else {
            plot(y.input, col = 1, xlab = "y", ylab = "membership func. of y")
        }
        plot(result, Alphacuts, xlab = "fun(x,y)", ylab = "membership func. of fun(x,y)", 
            ...)
        abline(v = fun(core(x), core(y)), lty = 3)
        par(op)
    }
    if (I.O.plot == "FALSE") {
        plot(result, Alphacuts, xlab = "fun(x,y)", ylab = "membership func. of fun(x,y)", 
            ...)
    }
    result2 <- c(L.result[length(L.result):1], U.result[length(U.result):1])
    cuts <- matrix(result2, ncol = 2, byrow = FALSE, dimnames = list(round((length(L.result) - 
        1):0/(length(L.result) - 1), 3), c("L", "U")))
    return(list(fun.rep = noquote(fun.rep), cuts = cuts, core = cuts[1, 
        ], support = cuts[dim(cuts)[1], ]))
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
\keyword{ fapply }
\keyword{ f2apply }
\keyword{ FuzzyNumbers }
\keyword{ monoton function }
\keyword{ is.increasing }
\keyword{ is.decreasing }
\keyword{ is.increasing.on.x }
\keyword{ is.decreasing.on.x }
\keyword{ is.increasing.on.y }
\keyword{ is.decreasing.on.y }

