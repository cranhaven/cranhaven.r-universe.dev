\name{ECharm_Info}
\alias{ECharm_Info}
\title{
Information on performance of soil electrical conductivity (EC) harmonization models
}
\description{
Information index for relative predictive performance of soil EC harmonization models
}
\usage{
ECharm_Info(solution)
}
\arguments{
  \item{solution}{
ratio of soil-water solution for the extract used in measuring electrical conductivity.
}
}
\details{
Ratio in text format for the soil-water solution for the extract used in measuring EC. It’s given in quotation marks. Current models consider “1:2”, “1:2.5”, and “1:5” ratios. Default ratio is “1:2”
}
\value{
Graphical display of the predictive performance index for the harmonization models in different regions of the world: Africa, Asia, Near East and North Africa (NENA), Latin America and Caribbean (LAC), north America, and Europe. The performance index ranges between 0 (poor) to 1 (best).
}
\author{
Christian Thine Omuto
}
\note{
The function currently works for 1:2, 1:2.5, and 1:5. These ratios must be entered in quotation marks.Due to periodic update,internet connectivity is needed for the function to work.
}
\seealso{
\code{\link{PHharm_Info}}, \code{\link{SASdata_densityInfo}}
}
\examples{
ECharm_Info("1:2")
}
