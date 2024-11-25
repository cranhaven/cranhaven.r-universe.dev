\name{PHharm_Info}
\alias{PHharm_Info}
\title{
Information on performance of soil pH (water) harmonization models
}
\description{
Information index for relative predictive performance of soil pH harmonization models
}
\usage{
PHharm_Info(solution)
}
\arguments{
  \item{solution}{
solution for measuring soil pH
}
}
\details{
Solution for measuring soil pH. It’s given in quotation marks. Current models consider “cacl2” and “kcl” ratios. Default solution is “cacl2”
}
\value{
Graphical display of the predictive performance index for the harmonization models in different regions of the world: Africa, Asia, Near East and North Africa (NENA), Latin America and Caribbean (LAC), north America, and Europe. The performance index ranges between 0 (poor) to 1 (best).
}
\author{
Christian Thine Omuto
}
\note{
The function currently works for cacl2 and kcl. These solutions must be entered in quotation marks.Due to periodic update,internet connectivity is needed for the function to work.
}
\seealso{
\code{\link{ECharm_Info}}, \code{\link{SASdata_densityInfo}}
}
\examples{
PHharm_Info("kcl")
}
