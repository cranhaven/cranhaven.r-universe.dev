\name{MouseBrain}
\alias{MouseBrain}
\docType{data}
\title{Effects of Serotonin in Mice}
\description{
Effects of altering serotonin levels on social interactions of mice
}
\format{
  A data frame with 48 observations on the following 3 variables.
  \describe{
    \item{\code{Contacts}}{Number of social contacts the mouse had during the experiment}
    \item{\code{Sex}}{ \code{F}=female or \code{M}=male}
    \item{\code{Genotype}}{\code{Minus}, \code{Mixed}, or \code{Plus} (see description below)}
  }
}
\details{
Serotonin is a chemical that influences mood balance in humans. But how does it affect mice? Scientists genetically altered mice by "knocking out" the expression of a gene, tryptophan hydroxylase 2 (Tph2), that regulates serotonin production. With careful breeding, the scientists produced three types of mice that we label as ``Minus'' for Tph2-/-, ``Plus'' for Tph2+/+, ``Mixed'' for Tph2+/-. The variable Genotype records Minus/Plus/Mixed. The variable Contacts is the number of social contacts that a mouse had with other mice during an experiment and the variable Sex is ``M'' for males and ``F'' for females.
}
\source{
Beis D, Holzwarth K, Flinders M, Bader M, Wohr M, Alenina N., (2015) "Brain serotonin deficiency leads to social communication deficits in mice," Biol. Lett. 11:20150057. \cr
http://dx.doi.org/10.1098/rsbl.2015.0057

Once you go to the above link, to get the data, click on the "Figures and Data" tab.  Then click on the "Juvenile SocInter Behavior Data" link to download a hairy data file that needs to be cleaned a great deal to get our data.

}
\keyword{datasets}
