\name{MASSunite}
\alias{MASSunite}
\title{Body mass comparisons, projecting reconstructions onto an extant standard}
\description{
The main function for projecting a body mass estimate from a reconstruction onto the limb circumference standard of accuracy, as presented by Campione & Evans (2020).
}
\usage{
MASSunite(HFC = NULL, HC, FC, BM, ES.type = "QE", gait, VD.par = NULL, line = FALSE, ...)
}
\arguments{
  \item{HFC}{a numeric value or vector representing the combined humeral and femoral circumferences (in mm)}
  \item{HC}{a numeric value or vector representing humeral circumference (in mm)}
  \item{FC}{a numeric value or vector representing femoral circumference (in mm)}
  \item{BM}{a numeric value or vector associated with the limb circumferences, ideally generates by a reconstruction approach (in g)}
  \item{ES.type}{a character specifying whether a linear or quadratic equation should be used as the extant limb scaling standard, (default is linear \code{ES.type = "QE"})}
  \item{gait}{a character specifying whether the reconstruction is \code{gait = "biped"} or \code{gait = "quadruped"}}
  \item{VD.par}{a list of parameters to be used when plotting. Options include: \code{pch}, an integer specifying a symbol or a single character to be used to project a reconstruction, \code{col}, a character specifying the colour to be used for the prejected recontruction(s), \code{names}, an optional character vector equal in length to \code{BM} specifying whether names are to be included in the plot (should be specified as \code{names = NULL}, if unwanted)}
  \item{line}{a logical value specifying whether a linear model should be run for the reconstruction (if \code{length(BM) > 1})}
  \item{...}{parameter arguments passed to scaling standard plot (e.g., \code{\link{mod_OLS}}.}
}
\details{
This function will take a set of body masses estimated via volumetric-density (VD) approaches, their accociated limb circumference data (such as that of the humerus and femur for a quadruped), and project them onto the extant standard of accuracy (Campione & Evans, 2010, Campione et al. 2014, Campione & Evans 2020). In addition, there is an option to fit a linear model to the VD reconstructions, allowing visual comparison to the extant relationship (see \code{line = TRUE}).

Both the linear and quadratic extant models can be used (see Campione, 2017) and residual deviations are calculated in accordance with the specified model. Residual plot, however, will depict both the upper and lower extremes of the prediction intervals as these can vary along the regression lines, especially is \code{ES.type = "qQE"}.
}
\value{
  A list of variable length depending on the parameters specified. Permanent values include:
  \item{Residual}{a numeric value or vector with the residual deviation between VD reconstructions and the ES standard}
  \item{mean.Residual}{a numeric value of the average residual value}
  \item{PPE}{a list with output of the \code{\link{ppe}} function}
  Optional values include:
  \item{Inferred.Line}{object of class \code{lm} with the output from the linear model, if \code{line = TRUE}}
  \item{Outliers}{an optional vector specifying VD resconstruction data points that occur outside the prediction intervals of the extant standard}
  \item{Caution}{if \code{length(Outliers) > 0}, then a warning message will be generated}
}
\references{
  Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
  
  Campione, N. E., Evans, D. C., Brown, C. M. and Carrano, M. T. (2014) Body mass estimation in non-avian bipeds using a theoretical conversion to quadruped stylopodial proportions. Methods in Ecology and Evolution, 5(9), 913-923.
	
  Campione, N. E. (2017) Extrapolation of a universal scaling relationship for estimating body masses in extinct terrestrial vertebrates. Paleobiology, 43 (4), 693-699.
  
  Campione, N. E. and Evans, D. C. (2020) The accuracy and precisions of body mass estimation in non-avian dinosaurs. Biological Reviews.
}
\author{
  Nicolas E. Campione
}
\seealso{
  \code{\link{QE}}
  \code{\link{cQE}}
  \code{\link{lm}}
}
\examples{
# metadata from Campione & Evans (2020) 
data(dino_comparisons)

# using linear standard and quadupedal dinosaurs w/ VD reconstruction line
MASSunite(dino_comparisons$HCFC, BM = dino_comparisons$Recon.BM, gait = "quadruped", 
VD.par = list(pch = 19, col = "pink", names = NULL), line = TRUE)

# using quadratic standard and bipedal dinsoaurs w/ no line
MASSunite(FC = dino_comparisons$FC, BM = dino_comparisons$Recon.BM, ES.type = "qQE", gait = "biped", 
VD.par = list(pch = 19, col = "pink", names = NULL))
}