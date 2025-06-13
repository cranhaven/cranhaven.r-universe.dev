\name{re.match}
\alias{re.match}
\title{Pattern Matching and Extraction}
\description{
    Matches and extracts substrings using regular expressions.
}
\usage{
    re.match(pattern, x, ...)
}
\arguments{
    \item{pattern}{character string containing a regular expression.}
    \item{x}{a character vector where matches are sought, or an object which can
             be coerced by \code{as.character} to a character vector.}
    \item{...}{further arguments to be passed to \code{regexec}.}
}
\details{
    This function is a wrapper for consecutive calls to \code{regexec} and
    \code{regmatches}.
}
\value{
    A matrix of matched substrings.
}
\seealso{
    \code{\link{regexec}} and \code{\link{regmatches}}
}
