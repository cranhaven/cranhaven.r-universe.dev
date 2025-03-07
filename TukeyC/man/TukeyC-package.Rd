\name{Tukey test}
\alias{TukeyC-package}

\docType{package}

\title{
  Conventional Tukey Test
}

\description{
  This package performs what is known as the Tukey HSD test in the conventional 
  way. It also uses an algorithm which divides the set of all means in groups 
  and assigns letters to the different groups, allowing for overlapping. This is 
  done for simple experimental designs and schemes. The most usual designs are:
  Completely Randomized Design (\samp{CRD}),
  Randomized Complete Block Design (\samp{RCBD}) and 
  Latin Squares Design (\samp{LSD}).
  The most usual schemes are:
  Factorial Experiment (\samp{FE}),
  Split-Plot Experiment (\samp{SPE}) and
  Split-Split-Plot Experiment (\samp{SPE}).
  
  The package can be used for both balanced or unbalanced (when possible), experiments.

  \R has some functions
  (\code{\link[stats]{TukeyHSD}} provided by \code{stats},
  \code{\link[multcomp]{glht}} provided by \code{multcomp},
  \code{\link[agricolae]{HSD.test}} provided by \code{agricolae} and
  \code{\link[multcomp]{cld}} provided by \code{multcomp}) which also performs
  the Tukey test. The \code{TukeyHSD} returns intervals based on the range of the
  sample means rather than the individual differences. Those intervals are based 
  on Studentized range statistics and are, in essence, confidence intervals.
  This approach has two advantages: the p-value is showed allowing the user to
  flexibilize the inferencial decision and also make it possible to plot the 
  result of the test. However, it has one disadvantage, since the final result is 
  more difficult to understand and summarize. Others (\code{glht}, \code{cld}) 
  are also useful but difficult to manage.
  Additionally, most of users of other statistical softwares are very used with 
  letters grouping the means of the factor tested, making unattractive or 
  difficult to adapt to the current aproach of \R.

  So, the main aim of this package is make available in \R environment the
  conventional aproach of Tukey test with a set of flexible funtions and
  S3 methods.
}
  
\references{
  Miller, R.G. (1981) \emph{Simultaneous Statistical Inference}. Springer.

  Ramalho M.A.P, Ferreira D.F and Oliveira A.C. (2000) \emph{Experimentacao em Genetica
  e Melhoramento de Plantas}. Editora UFLA.

  Steel, R.G., Torrie, J.H and Dickey D.A. (1997) \emph{Principles and procedures of statistics:
  a biometrical approach}. Third Edition.
  
  Yandell, B.S. (1997) \emph{Practical Data Analysis for Designed Experiments}. 
  Chapman & Hall.
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\keyword{package}
