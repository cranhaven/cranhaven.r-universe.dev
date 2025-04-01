\name{vampyr_example}
\alias{vampyr_example}
\docType{data}
\title{
vampyr_example database
}
\description{
A database to be used as example in the functions included on \code{controlSDAQ} package. It contains the answers of 300 participants to the Physical aggression subscale of IDAQ questionnaire (Ruiz-Pamies, Lorenzo-Seva, Morales-Vives, Cosi, Vigil-Colet, 2014), which was developed for assessing Physical, Verbal and Indirect aggression. The original questionnaire contains 27 Likert-items, ranging from 1 to 5, and this dataset only contains the 4 SD markers and the 6 Physical aggression items.
}
\usage{data("vampyr_example")}
\format{
A data frame with 300 observations and 10 variables, where 6 items measuring Physical aggression and 4 Social Desirability markers. Items number 1, 2, 3 and 4 are SD markers (pure SD measures), and the remaining 6 items measure  Physical aggression. Items number 5, 7 and 8 are positively worded, and items number 6, 9 and 10 are negatively worded.
}
\details{
The original sample contains 750 participants, and the following database only contains 300 selected at random.

In the original dataset, which contains 27 items, the SD items correspond to items number 2, 8, 13 and 21, and the Phyisical aggression items are (in order) items number 1, 6, 17, 19, 20 and 25.
}
\source{
More information about the questionnaire can be found at:

\url{http://psico.fcep.urv.cat/tests/idaq/en/descripcion.html}
}
\references{
Ruiz-Pamies, M., Lorenzo-Seva, U., Morales-Vives, F., Cosi, S., & Vigil-Colet, A. (2014). I-DAQ: a new test to assess direct and indirect aggression free of response bias. The Spanish Journal of Psychology, 17, E41. \doi{10.1017/sjp.2014.43}
}
\examples{
data(vampyr_example)
}
\keyword{datasets}
