\name{moid_traits}
\alias{moid_traits}
\encoding{UTF-8}
\docType{data}

\title{Trait data on 57 species of extant musteloids.}

\description{Morphological and life history data on mean male body mass (kg), mean male body mass (kg), sexual size dimorphism (male mass/female mass), mean male body length (mm), mean female body length (mm), mean litter size (number of offspring), social class (Solitary, Pairs, Variable Groups, Groups), and diet (Carnivorous, Piscivorous, Insectivorous, Omnivorous, Frugivorous/Herbivorous) for 57 species of extant musteloids.}

\usage{data("moid_traits")}

\format{ An object of class "data.frame".}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\references{
Noonan, M. J., Johnson, P. J., Kitchener, A. C., Harrington, L. A., Newman, C., and Macdonald, D. W. (2016). Sexual size dimorphism in musteloids: An anomalous allometric pattern is explained by feeding ecology. Ecology and evolution, 6(23), 8495-8501. \doi{10.1002/ece3.2480}.
Noonan, M. J., Fagan, W. F., and Fleming C. H. (2021) ``A semi-variance approach to visualising phylogenetic autocorrelation''. Methods in Ecology and Evolution, in press.
}

\seealso{ \code{\link{musteloids}}. }

\examples{
# Load package and data
library(ctpm)
data("moid_traits")

# Plot male mass versus female mass
plot(moid_traits$Mass.M ~ moid_traits$Mass.F)
}

\keyword{datasets}
