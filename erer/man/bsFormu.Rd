\name{bsFormu}
\alias{bsFormu}
\title{Generating Formula for Models}
\description{Generate a single formula for models like \code{lm} or a list of formula for models like \code{systemfit}.}

\usage{bsFormu(name.y, name.x, intercept = TRUE, ...)}
\arguments{
  \item{name.y }{ a character vector of variables names for dependent variables; when the length is more than one, there will a list of formula generated for each variable in the name.}
  \item{name.x }{ a character vector of indepedent variables.}
  \item{intercept}{ a logical value (default of TRUE) of whether to include intercept or not.}
  \item{\dots }{ additional arguments to be passed.}
}
\details{ This function can generate a single formula for simple model like \code{lm} or a list of formula for systems (\code{systemfit}. Note that the right-hand side variables are the same for each dependent variable. If different, a \code{for} loop can be added by users to address that, as demonstrated by the example below.}
\value{a single formula object or a list of formula objects.}
\author{Changyou Sun (\email{cs258@msstate.edu})}

\examples{

# fake data
y  <- c("y")
ym <- c("y1", "y2", "y3")
x <- c("x")
xAll <- c("x", "xx", "xxx", "xxxx")

bsFormu(name.y = y, name.x = x)
bsFormu(name.y = ym, name.x = xAll)
fm.ym <- bsFormu(name.y = ym, name.x = xAll, intercept = FALSE)
fm.ym

# If independent variables differ by equation,
# add a loop to address the differentiation.
xInd <- c("x1", "x2", "x3")
fm.ym <- list()
for (i in 1:length(ym)) {
    ny <- ym[i]
    nx <- c(xInd[i], xAll)
    fm.ym[[i]] <- bsFormu(name.y = ny, name.x = nx, intercept = FALSE)
}
fm.ym

# real data
data(daIns)
(xx <- colnames(daIns)[-c(1, 14)])
fm.ins <- bsFormu(name.y = "Y", name.x = xx, intercept = TRUE)
fm.ins
(ra <- glm(formula = fm.ins,
          family = binomial(link="logit"),
          data = daIns, x = TRUE))
}

\keyword{manip}