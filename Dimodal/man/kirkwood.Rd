
\name{kirkwood}
\alias{kirkwood}
\docType{data}
\title{Asteroid Orbital Axis Showing Kirkwood Gaps}
\description{
The distance of asteroids from the sun is multi-modal, with modes
corresponding to asteroid families and anti-modes to the Kirkwood gaps.
}

\usage{
data(kirkwood)
}

\format{
\code{kirkwood} is a numeric vector of 2093 values representing the
semi-major axis of an asteroid in Astronomical Units (A.U.).
}

\details{
The axis is stored in column 18 of the original data file.  We keep only
those asteroids within the orbit of Jupiter, with an axis below 5 A.U.  We
further keep only those asteroids whose diameter is known or not blank, in
column 7 of the data file.
}

\source{
Lowell Observatory maintains the asteroid ephemeris website at
\url{https://asteroid.lowell.edu/astorb/}
The \code{kirkwood} dataset is a subset of the "astorb.txt.gz" static copy
of the ephemeris, downloaded on 1 May 25.  The full file contains more than
1.4 million entries, and is growing daily.  Lowell Observatory provides a
query builder for downloading up-to-date values at
\url{https://asteroid.lowell.edu/query-builder}

The ephemeris is supported by grants from NASA and the Lowell Observatory;
details are found on the website.  Further distribution of the data requires
acknowledging these funding sources.
}

\references{
N. Moskovitz, L. Wasserman, B. Burt, R. Schottland, E. Bowell, M. Bailen,
M. Granvik,
The \verb{astorb} database at Lowell Observatory,
\emph{Astronomy and Computing}, 41, Oct 2022, pp. 100661.
}

\examples{

## Load data.
data(kirkwood, package="Dimodal")

## The Ditrack and changepoints can take a while to run, so to pass
## the CRAN time limit in examples they are marked don't run.  To see
## the complete example call example(kirkwood, run.dontrun=TRUE).

## Start from scratch.
## Normally you would not need to save the results of the call, but
## this clutters up the screen.
opt <- Diopt(NULL)

## Set up the analysis.
## This is a large data set so spacing in features is small but
## smooth, which allows tighter detector parameters.  Values depend
## on the range of the spacing and are found by trial and error.
## Leave off changepoints for repeatability in this example.
## Set RNG seeds for repeatability.
## Using bars to mark flats is easier to read with dense data.
opt1 <- Diopt(peak.fht=0.015, flat.fripple=0.0075, analysis=c("lp", "diw"),
              excur.seed=3, perm.seed=5, mark.flat="bar")

## Use Ditrack to see where passing features (filled in dots) appear.
## Interval spacing is the same cut-off.  Wrapped in \donttest because of
## the run time.
\donttest{trk <- Ditrack(kirkwood, "lp")}
\donttest{dev.new(width=8,height=4) ; plot(trk)}
opt1 <- c(opt1, Diopt(lp.window=0.05, diw.window=0.05))

## Run analysis.
m <- Dimodal(kirkwood)

## Summarize the data and spacing.
m$data

## Print features that exist in both low-pass and interval spacing.
## Allow large distance between peaks because  of data set size.
mtch <- match.features(m, near=30)

## Gap distance (in AU), dropping the extra LP peak.
## There are gaps at 2.30, 2.48, 2.86, 2.93, and 3.06 AU.
a_gap <- (select.peaks(m$lp.peaks)$x[-3L] + select.peaks(m$diw.peaks)$x) / 2
a_gap

## Orbital resonance with Jupiter, which has orbital axis of
## 5.201 AU, from Kepler's third law.  The corresponding
## resonance is 3.40 (ratio 7:2 or 10:3), 3.04 (3:1),  2.46 (5:2),
## 2.36 (7:3), and 2.22 (9:4).  Simulations confirm all but the
## first and last.
resonance <- (5.201 / a_gap) ^ (3/2)
resonance

## The flats include families of asteroids, although other
## considerations must be taken into account to find true
## family members (composition of asteroid, orbital
## eccentricity and inclination).
## The second range at 2.76 AU includes the Ceres family,
## the third at 3.13 AU the Themis.  The first range at
## 2.64 AU includes the Eunomia and Prosperina families.
m$diw.flats

## 3 graphs side-by-side with results.
dev.new(width=12, height=4) ; plot(m)

## The full test results of the low-pass peaks.
m$lp.peaks

## In the Ditrack plot there are two peaks in the very lower right
## corner.  We need smaller filter kernels/intervals to find these.
## The parameters already set will still apply.
opt3 <- Diopt(lp.window=0.015, diw.window=0.015, peak.fht=0.025, peak.frelht=0.10)
## Running Dimodal with such small windows will generate warnings
## about the model tests being pushed out of bounds.
## This adds gaps at 1.93 (2:1) and 1.74 (7:4), both expected
## from simulations.  The 2:1 gap position shifts because the
## spacing is so large that there is data point to anchor the
## value and a second, smaller increase pulls the peak to 1.93.
m2 <- Dimodal(kirkwood) ; mtch <- match.features(m2, near=30)
dev.new(width=12, height=4) ; plot(m2)

## To run changepoints, do this.
## Using all supported libraries they surround the 3:1, 2:1,
## and 7:4 gaps to the side of the peak, but not directly at
## the feature.  They also mark one side of the 5:2, 7:3, and
## 9:4 gaps.
\dontrun{ mcpt <- Dimodal(kirkwood, Diopt.local(analysis="cpt")) ; mcpt }
\dontrun{ dev.new(width=8, height=4) ; plot(mcpt) }
## Show the voting from all libraries.  This is a tall graph.
\dontrun{ dev.new(height=10, width=6) ; plot(mcpt$cpt) }

## Restore default.
opt <- Diopt(opt)
}

\keyword{datasets}
