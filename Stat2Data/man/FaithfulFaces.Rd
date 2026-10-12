\name{FaithfulFaces}
\alias{FaithfulFaces}
\docType{data}
\title{Faithfulness from a Photo?}
\description{
Ratings from a facial photo and actual faithfulness.
}
\format{
  A data frame with 170 observations on the following 7 variables.
  \describe{
    \item{\code{SexDimorph}}{Rating of sexual dimorphism (masculinity for males, femininity for females)}
    \item{\code{Attract}}{Rating of attractiveness}
    \item{\code{Cheater}}{Was the face subject unfaithful to a partner? (\code{1}=yes or \code{0}=no)}
    \item{\code{Trust}}{Rating of trustworthiness}
    \item{\code{Faithful}}{Rating of faithfulness}
    \item{\code{FaceSex}}{Sex of face (\code{F}=female or \code{M}=male)}
    \item{\code{RaterSex}}{Sex of rater (\code{F}=female or \code{M}=male)}
  }
}
\details{
College students were asked to look at a photograph of an opposite-sex adult face and to rate the person, on a scale from 1 (low) to 10 (high), for attractiveness. They were also asked to rate trustworthiness, faithfulness, and sexual dimorphism (i.e., how masculine a male face is and how feminine a female face is). Overall, 68 students (34 males and 34 females) rated 170 faces (88 men and 82 women).
}
\source{
This dataset is based on G. Rhodes et al. (2012), "Women can judge sexual unfaithfulness from unfamiliar men's faces," Biology Letters, November 2012. All of the 68 raters were heterosexual Caucasians, as were the 170 persons who were rated. (We have deleted 3 subjects with missing values and 16 subjects who were over age 35.)
}
\keyword{datasets}
