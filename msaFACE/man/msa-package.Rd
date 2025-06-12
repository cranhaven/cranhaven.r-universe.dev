\name{msaFACE-package}
\alias{msaFACE-package}
\docType{package}
\title{
Moving Subset Analysis GiFACE
}
\description{ 
% The \pkg{msaFACE} contains the dataset and method used to analyse the CO2 fertilization effect dependent on various environmental conditions within a longterm free air carbon enrichment experiment. In general, the method can be used to derive the influence of a third (accompanying) variable on the relation between a dependent and an independent variable.
  The package \pkg{msaFACE} provides a new methodology called "\pkg{m}oving \pkg{s}ubset \pkg{a}nalysis" to investigate the effect of environmental conditions on the CO2 fertilization effect within longterm free air carbon enrichment experiments. More generally, the functionality is applicable to derive the influence of a third variable (forcing experiment-support variable)  on the relation between a dependent and an independent variable.
}
\details{
  The main part of the package is the function \code{\link{moving_subset_analysis}} which derives the CO2-fertilization effect (CFE) and relates it to the variable environmental conditions. The   CFE is calculated by the regression between a dependent variable such as total aboveground biomass and the CO2 concentration in the air. Please note that the value of the function \code{\link{moving_subset_analysis}} is of class '\code{MSA_coef}' for which several methods are available to plot and print the results.

  
  Each experiment-support variable can be used as forcing variable, for which the influence on the CFE is analyzed through the following steps:
\enumerate{
 \item{}{In the first step, the dataset is rearranged in ascending order of the forcing experiment-support variable. The total dataset is then partitioned into subsets where each contains observations featuring similar environmental characteristics. Taking the rainfall as an example, the first subset will encompass the driest years. For the second one, the year with the lowest rainfall sum is dropped and replaced by the year with the next lowest rainfall sum. This is repeated until the last subset is reached which encompasses the wettest years. For the GiFACE dataset with 16 years (1998-2013) of available data, 12 subsets are created for a window size of 30 observations (each subset encompassing six rings and five years).}

 \item{}{The CFE is then derived as the slope of the regression model between the CO2 concentration of the air and the selected dependent variable, which is calculated separately within each of the subsets defined above.}

 \item{}{By presenting the slope and its significance against the average of the forcing experiment-support variable in the subset, the influence of the respective variable on the CFE is revealed. However, situated in natural environments, most of the experiment-support variables will be highly correlated and, thus, the influence of the forcing variable has to be interpreted carefully. Therefore, the \pkg{msaFACE} contains the possibility to plot the averages of the accompanying experiment-support variables in the subsets, enabling a comprehensive picture of the prevailing environmental conditions. All this can easily be achieved by the function \code{\link{plot.MSA_coef}}.}
}
%   Within the experiment, a permanent temperate grassland is treated with elevated CO2 (~20\% during daylight hours). In three rings the air is enriched with CO2 while other three rings act under atmospheric CO2 (control). The late-summer yields as well as the CO2 concentrations (averaged in the three months before harvest) were measured ring-wise and can be used as dependent and independent variables for the Moving Subset Analysis. Other variables describing the environmental conditions were aggregated to their average in the three months preceding harvest (respectively sum for solar radiation and precipitation). These environmental variables can be used to derive the subsets (where regression within the moving subset is performed) and be plotted as accompanying variables to get a clear picture of the environmental conditions within the respective subset. 
  
  To see the preferable citation of the package, type
  \code{citation("msaFACE")}.  
}
\seealso{
  \code{\link{moving_subset_analysis}}, \code{\link{GiFACE}}, \code{\link{plot.MSA_coef}}, \code{\link{print.MSA_coef}},
  
  \code{\link{summary.MSA_coef}}
}
\references{
  W. A. Obermeier, L. W. Lehnert, C. I. Kammann, C. Mueller, L.
  Gruenhage, J. Luterbacher, M. Erbs, G. Moser, R. Seibert, N. Yuan,
  J. Bendix (under review) Reduced CO2 fertilization in temperate C3 grasslands
  under more extreme weather conditions. Nature Climate Change
}
\author{
  Wolfgang A. Obermeier, Lukas W. Lehnert, Joerg Bendix
}
% \examples{
% ## Some basic examples
% }

\keyword{package}
 
