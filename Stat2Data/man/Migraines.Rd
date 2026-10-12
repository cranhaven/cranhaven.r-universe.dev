\name{Migraines}
\alias{Migraines}
\docType{data}
\title{Migraines and TMS}
\description{
Effects of transcranial magnetic stimulation (TMS) on migraine headaches
}
\format{
  A data frame with 2 observations on the following 4 variables.
  \describe{
    \item{\code{Group}}{Treatment group (\code{Placebo} or \code{TMS})}
    \item{\code{Yes}}{Count of number of patients that were pain-free in each group}
    \item{\code{No}}{Count of number of patients that had pain in each group}
    \item{\code{Trials}}{Number of patients in each group}
  }
}
\details{
A study investigated whether a handheld device that sends a magnetic pulse into a person's head
might be an effective treatment for migraine headaches. Researchers recruited 200 subjects who
suffered from migraines and randomly assigned them to receive either the TMS (transcranial magnetic
stimulation) treatment or a sham (placebo) treatment from a device that did not deliver any
stimulation. Subjects were instructed to apply the device at the onset of migraine symptoms and
then assess how they felt two hours later. This dataset is a two-way table of the results.

This dataset was called TMS in the first edition.

}
\source{
Based on results in R. B. Lipton, et al, (2010) "Single-pulse Transcranial Magnetic Stimulation
for Acute Treatment of Migraine with Aura: A Randomised, Double-blind, Parallel-group, Shamcontrolled
Trial," Lancet Neurology, 9(4):373-380.
}
\keyword{datasets}
