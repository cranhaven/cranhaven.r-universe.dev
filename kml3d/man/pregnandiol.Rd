\name{pregnandiol}
\Rdversion{1.1}
\alias{pregnandiol}
\docType{data}

\title{~ Pregnandiol measure (from QUIDEL database, René Écochard) ~}

\description{
  These longitudinal data are extract form the QUIDEL database whose aims is to
  studies hormone profiles among women who have no fertility problem.
}

\usage{data(pregnandiol)}

\format{
  Some longitudinal data in wide format. It includes 107 women who have been followed during up to 49
  days. Each column correspond to a specific time meseaurement. The
  outcome is the hormone "pregnandiol".

  \describe{
    \item{\code{id}}{unique idenfier for each patient.}
    \item{\code{day1}}{Measurement of pregnandiol at day 1.}
    \item{\code{day2}}{Measurement of pregnandiol at day 2.}
    \item{\code{day3}}{Measurement of pregnandiol at day 3.}
    \item{\code{...}}{...}
    \item{\code{day 49}}{Measurement of pregnandiol at day 49.}
  }
}

\details{
The QUIDEL database aims to gain better knowledge of hormone profiles
among women who have no fertility problem. This database has been
described as the largest existing database on hormone profiles in the
normal human menstrual cycle, involving ultrasound scan of the day of
ovulation [eco06]. It involves 107 women and 283 cycles in all, with
identification of the day of ovulation and daily titration of the levels
of the four main hormones in the ovulation cycle. The database belongs
to the laboratory in charge of the analysis of hormone trajectories
(CNRS 5558, René Ecochard).
It has already been the subject of numerous
publications, including [eco00, eco01].}

\source{QUIDEL cohort}

\references{
[eco00]	Ecochard R, Gougeon A. Side of ovulation and cycle characteristics in normally fertile women. Human reproduction (Oxford, England). 2000;15(4):752-755.

[eco01]	Ecochard R et al. Chronological aspects of ultrasonic, hormonal, and other indirect indices of ovulation. BJOG : an international journal of obstetrics and gynaecology. 2001;108(8):822-829.
}


\keyword{clusters}%datasets}
\keyword{documentation}


