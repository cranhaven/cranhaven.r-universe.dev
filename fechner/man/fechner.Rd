\name{fechner}
\alias{fechner}
\concept{Fechnerian scaling}
\concept{discrete object sets}
\title{Main Function For Fechnerian Scaling}
\description{
  \code{fechner} provides the Fechnerian scaling computations.  It is
  the main function of this package.
}
\usage{
fechner(X, format = c("probability.different", "percent.same", "general"),
        compute.all = FALSE, check.computation = FALSE)
}
\arguments{
  \item{X}{a required square matrix or data frame of numeric data.
    No \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf} values are
    allowed.}

  \item{format}{an optional character string giving the data format
    that is used.  This must be one of \code{"probability.different"},
    \code{"percent.same"}, or \code{"general"}, with default
    \code{"probability.different"}, and may be abbreviated to a unique
    prefix.}

  \item{compute.all}{an optional logical.  The default value
    \code{FALSE} corresponds to short computation, which yields the
    main Fechnerian scaling computations.  The value \code{TRUE}
    corresponds to long computation, which additionally yields
    intermediate results and also allows for a check of computations
    if \code{check.computation} is set \code{TRUE}.}

  \item{check.computation}{an optional logical.  If \code{TRUE}, the
    check for whether the overall Fechnerian distance of the first
    kind (in the first observation area) is equal to the overall
    Fechnerian distance of the second kind (in the second observation
    area) is performed.  The check requires \code{compute.all} to be
    set \code{TRUE}.}
}
\details{
  The \code{format} argument specifies the data format that is used.
  \code{"probability.different"} and \code{"percent.same"} are for
  datasets in the probability-different and percent-same formats, and
  in the latter case, the data are automatically transformed prior to
  the analysis using the transformation \eqn{(100 - X) / 100}.
  \code{"general"} is to be used for datasets that are properly in the
  general data format.  Note that for \code{"percent.same"}, the data
  must satisfy regular maximality, for \code{"probability.different"}
  and \code{"general"}, regular minimality (otherwise function
  \code{fechner} produces respective messages).  In particular, data
  in the general format may possibly need to be transformed manually
  prior to calling the function \code{fechner}.

  If \code{compute.all = TRUE} and \code{check.computation = TRUE},
  the performed check computes the difference \sQuote{overall
  Fechnerian distance of the first kind minus overall Fechnerian
  distance of the second kind}.  By theory, this difference is zero.
  The function \code{fechner} calculates that difference and checks
  for equality of these Fechnerian distances up to machine precision
  (see \sQuote{Value}). \code{fechner} calls
  \code{\link{check.regular}}, which in turn calls
  \code{\link{check.data}}.  In particular, the specified data format
  and regular minimality/maximality are checked, and the rows and
  columns of the canonical representation matrix (see
  \code{\link{check.regular}}) are canonically relabeled based on the
  labeling provided by \code{\link{check.data}}.

  The function \code{fechner} returns an object of the class
  \code{fechner} (see \sQuote{Value}), for which \code{plot},
  \code{print}, and \code{summary} methods are provided;
  \code{\link{plot.fechner}}, \code{\link{print.fechner}}, and
  \code{\link{summary.fechner}}, respectively.  Moreover, objects of
  the class \code{fechner} are set the specific named attribute
  \code{computation}, which is assumed to have the value \code{short}
  or \code{long} indicating whether short computation
  (\code{compute.all = FALSE}) or long computation
  (\code{compute.all = TRUE}) was performed, respectively.
}
\value{
  If the arguments \code{X}, \code{format}, \code{compute.all}, and
  \code{check.computation} are of required types, \code{fechner}
  returns a named list, of the class \code{fechner} and with the
  attribute \code{computation}, which consists of \eqn{6} or \eqn{18}
  components, depending on whether short computation
  (\code{computation} is then set \code{short}) or long computation
  (\code{computation} is then set \code{long}) was performed,
  respectively.  \cr

  The short computation list contains the following first \eqn{6}
  components, the long computation list the subsequent ones:

  \item{points.of.subjective.equality}{a data frame giving the
    permutation of the columns of \code{X} used to produce the
    canonical representation of \code{X}.  The first and second
    variables of this data frame, \code{observation.area.1} and
    \code{observation.area.2}, respectively, represent the pairs
    of points of subjective equality (PSEs).  The third variable,
    \code{common.label}, lists the identical labels assigned to the
    pairs of PSEs.  (first component of short computation list)}
  \item{canonical.representation}{a matrix giving the representation
    of \code{X} in which regular minimality/maximality is satisfied in
    the canonical form.  That is, the single minimal/maximal entries
    of the rows and columns lie on the main diagonal (of the canonical
    representation).  In addition, the rows and columns are
    canonically relabeled.}
  \item{overall.Fechnerian.distances}{a matrix of the overall
    Fechnerian distances (of the first kind); by theory, invariant
    from observation area.}
  \item{geodesic.loops}{a data frame of the geodesic loops of the
    first kind; must be read from left to right for the first kind,
    and from right to left for the second kind.}
  \item{graph.lengths.of.geodesic.loops}{a matrix of the
    graph-theoretic (edge/link based) lengths of the geodesic loops
    (of the first kind).}
  \item{S.index}{a matrix of the generalized \sQuote{Shepardian}
    dissimilarity (or S-index) values.  An S-index value is defined as
    the psychometric length of the loop between a row stimulus and a
    column stimulus containing only these two stimuli.  (last
    component of short computation list)}
  \item{points.of.subjective.equality}{the same as in case of short
    computation; see above.  (first component of long computation
    list)}
  \item{canonical.representation}{the same as in case of short
    computation; see above.}
  \item{psychometric.increments.1}{a matrix of the psychometric
    increments of the first kind.}
  \item{psychometric.increments.2}{a matrix of the psychometric
    increments of the second kind.}
  \item{oriented.Fechnerian.distances.1}{a matrix of the oriented
    Fechnerian distances of the first kind.}
  \item{overall.Fechnerian.distances.1}{a matrix of the overall
    Fechnerian distances of the first kind.}
  \item{oriented.Fechnerian.distances.2}{a matrix of the oriented
    Fechnerian distances of the second kind.}
  \item{overall.Fechnerian.distances.2}{a matrix of the overall
    Fechnerian distances of the second kind.}
  \item{check}{if \code{check.computation = TRUE}, a list of two
    components: \code{difference} and \code{are.nearly.equal}.  The
    component \code{difference} is a matrix of the differences of the
    overall Fechnerian distances of the first and second kind; ought
    to be a zero matrix.  The component \code{are.nearly.equal} is a
    logical indicating whether this matrix of differences is equal to
    the zero matrix up to machine precision.  If
    \code{check.computation = FALSE}, a character string saying
    \emph{\dQuote{computation check was not requested}}.}
  \item{geodesic.chains.1}{a data frame of the geodesic chains of the
    first kind.}
  \item{geodesic.loops.1}{a data frame of the geodesic loops of the
    first kind.}
  \item{graph.lengths.of.geodesic.chains.1}{a matrix of the
    graph-theoretic (edge/link based) lengths of the geodesic chains
    of the first kind.}
  \item{graph.lengths.of.geodesic.loops.1}{a matrix of the
    graph-theoretic (edge/link based) lengths of the geodesic loops of
    the first kind.}
  \item{geodesic.chains.2}{a data frame of the geodesic chains of the
    second kind.}
  \item{geodesic.loops.2}{a data frame of the geodesic loops of the
    second kind.}
  \item{graph.lengths.of.geodesic.chains.2}{a matrix of the
    graph-theoretic (edge/link based) lengths of the geodesic chains
    of the second kind.}
  \item{graph.lengths.of.geodesic.loops.2}{a matrix of the
    graph-theoretic (edge/link based) lengths of the geodesic loops of
    the second kind.}
  \item{S.index}{the same as in case of short computation; see above.
    (last component of long computation list)}
}
\author{
  Thomas Kiefer, Ali Uenlue.  Based on original MATLAB source by Ehtibar N. Dzhafarov.
}
\references{
  Dzhafarov, E. N. and Colonius, H. (2006) Reconstructing
  distances among objects from their discriminability.
  \emph{Psychometrika}, \bold{71}, 365--386.

  Dzhafarov, E. N. and Colonius, H. (2007) Dissimilarity
  cumulation theory and subjective metrics. \emph{Journal of
  Mathematical Psychology}, \bold{51}, 290--304.

  Uenlue, A. and Kiefer, T. and Dzhafarov, E. N.
  (2009) Fechnerian scaling in \R: The package \pkg{fechner}.
  \emph{Journal of Statistical Software}, \bold{31}(6), 1--24.
  URL \url{http://www.jstatsoft.org/v31/i06/}.
}
\seealso{
  \code{\link{check.data}} for checking data format;
  \code{\link{check.regular}} for checking regular
  minimality/maximality; \code{\link{plot.fechner}}, the S3 method for
  plotting objects of the class \code{fechner};
  \code{\link{print.fechner}}, the S3 method for printing objects of
  the class \code{fechner}; \code{\link{summary.fechner}}, the S3
  method for summarizing objects of the class \code{fechner}, which
  creates objects of the class \code{summary.fechner};
  \code{\link{print.summary.fechner}}, the S3 method for printing
  objects of the class \code{summary.fechner}.  See also
  \code{\link{fechner-package}} for general information about this
  package.
}
\examples{
##
## (1) examples based on dataset \link{morse}
##

## dataset \link{morse} satisfies regular maximality in canonical form
morse
check.regular(morse, type = "percent.same")

## a self-contained 10-code subspace consisting of the codes for the
## letter B and the digits 0, 1, 2, 4, \ldots, 9
indices <- which(is.element(names(morse), c("B", c(0, 1, 2, 4:9))))
f.scal.morse <- fechner(morse, format = "percent.same")
f.scal.morse$geodesic.loops[indices, indices]
morse.subspace <- morse[indices, indices]
check.regular(morse.subspace, type = "percent.same")

## since the subspace is self-contained, results must be the same
f.scal.subspace.mo <- fechner(morse.subspace, format = "percent.same")
identical(f.scal.morse$geodesic.loops[indices, indices],
          f.scal.subspace.mo$geodesic.loops)
identical(f.scal.morse$overall.Fechnerian.distances[indices, indices],
          f.scal.subspace.mo$overall.Fechnerian.distances)

## Fechnerian scaling analysis using short computation
f.scal.subspace.mo
str(f.scal.subspace.mo)
attributes(f.scal.subspace.mo)
## for instance, the S-index
f.scal.subspace.mo$S.index

## Fechnerian scaling analysis using long computation
f.scal.subspace.long.mo <- fechner(morse.subspace,
                                   format = "percent.same",
                                   compute.all = TRUE,
                                   check.computation = TRUE)
f.scal.subspace.long.mo
str(f.scal.subspace.long.mo)
attributes(f.scal.subspace.long.mo)
## for instance, the geodesic chains of the first kind
f.scal.subspace.long.mo$geodesic.chains.1

## check whether the overall Fechnerian distance of the first kind is
## equal to the overall Fechnerian distance of the second kind
## the difference, by theory a zero matrix
f.scal.subspace.long.mo$check[1]
## or, up to machine precision
f.scal.subspace.long.mo$check[2]

## plot of the S-index versus the overall Fechnerian distance
## for all (off-diagonal) pairs of stimuli
plot(f.scal.subspace.long.mo)
## for all (off-diagonal) pairs of stimuli with geodesic loops
## containing at least 3 links
plot(f.scal.subspace.long.mo, level = 3)

## corresponding summaries, including Pearson correlation and C-index
summary(f.scal.subspace.long.mo)
## in particular, accessing detailed summary through assignment
detailed.summary.mo <- summary(f.scal.subspace.long.mo, level = 3)
str(detailed.summary.mo)

##
## (2) examples based on dataset \link{wish}
##

## dataset \link{wish} satisfies regular minimality in canonical form
wish
check.regular(wish, type = "probability.different")

## a self-contained 10-code subspace consisting of S, U, W, X,
## 0, 1, \ldots, 5
indices <- which(is.element(names(wish), c("S", "U", "W", "X", 0:5)))
f.scal.wish <- fechner(wish, format = "probability.different")
f.scal.wish$geodesic.loops[indices, indices]
wish.subspace <- wish[indices, indices]
check.regular(wish.subspace, type = "probability.different")

## since the subspace is self-contained, results must be the same
f.scal.subspace.wi <- fechner(wish.subspace,
                              format = "probability.different")
identical(f.scal.wish$geodesic.loops[indices, indices],
          f.scal.subspace.wi$geodesic.loops)
identical(f.scal.wish$overall.Fechnerian.distances[indices, indices],
          f.scal.subspace.wi$overall.Fechnerian.distances)

## dataset \link{wish} transformed to percent-same format
check.data(100 - (wish * 100), format = "percent.same")

## Fechnerian scaling analysis using short computation
f.scal.subspace.wi
str(f.scal.subspace.wi)
attributes(f.scal.subspace.wi)
## for instance, the graph-theoretic lengths of geodesic loops
f.scal.subspace.wi$graph.lengths.of.geodesic.loops

## Fechnerian scaling analysis using long computation
f.scal.subspace.long.wi <- fechner(wish.subspace,
                                   format = "probability.different",
                                   compute.all = TRUE,
                                   check.computation = TRUE)
f.scal.subspace.long.wi
str(f.scal.subspace.long.wi)
attributes(f.scal.subspace.long.wi)
## for instance, the oriented Fechnerian distances of the first kind
f.scal.subspace.long.wi$oriented.Fechnerian.distances.1
## or, graph-theoretic lengths of chains and loops
identical(f.scal.subspace.long.wi$graph.lengths.of.geodesic.chains.1 +
          t(f.scal.subspace.long.wi$graph.lengths.of.geodesic.chains.1),
          f.scal.subspace.long.wi$graph.lengths.of.geodesic.loops.1)

## overall Fechnerian distances are not monotonically related to
## discrimination probabilities; however, there is a strong positive
## correlation
cor(as.vector(f.scal.wish$overall.Fechnerian.distances),
    as.vector(as.matrix(wish)))

## check whether the overall Fechnerian distance of the first kind is
## equal to the overall Fechnerian distance of the second kind
## the difference, by theory a zero matrix
f.scal.subspace.long.wi$check[1]
## or, up to machine precision
f.scal.subspace.long.wi$check[2]

## plot of the S-index versus the overall Fechnerian distance
## for all (off-diagonal) pairs of stimuli
plot(f.scal.subspace.long.wi)
## for all (off-diagonal) pairs of stimuli with geodesic loops
## containing at least 5 links
plot(f.scal.subspace.long.wi, level = 5)

## corresponding summaries, including Pearson correlation and C-index
summary(f.scal.subspace.long.wi)
## in particular, accessing detailed summary through assignment
detailed.summary.wi <- summary(f.scal.subspace.long.wi, level = 5)
str(detailed.summary.wi)
}
\keyword{attribute}
\keyword{classes}
\keyword{manip}
\keyword{models}
\keyword{univar}
