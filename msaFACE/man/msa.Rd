\name{moving_subset_analysis}
\alias{moving_subset_analysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving subset analysis
}
\description{
The moving subset analysis quantifies the influence of a third variable (forcing experiment-support variable) on the relation between a dependent and an independent variable.
}
\usage{
moving_subset_analysis(x, treatment_var, response_var, window_size,
                       group = 1:nrow(x), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Matrix or dataframe with dependent, independent and any experiment-support variables
}
  \item{treatment_var}{
    Name of treatment variable. All but treatment and response variable are defined as experiment-support variables.
}
  \item{response_var}{
    Name of response variable
  }
  
  \item{window_size}{
    Number of observations within one subset. Must be a multiple of the number of observations within one group.
  }
  \item{group}{
    Group factor identifying independent observations. Note that the number of observations within each group must be identical.
  }
    \item{...}{
    Further arguments (currently ignored)
  }
}
\author{
Wolfgang A. Obermeier, Lukas W. Lehnert, Joerg Bendix
}
\value{
  Object of class \code{MSA_coef}. Internally stored as a list. This object contains one element for each experiment-support variable. Those elements are data.frames with the subset-wise outputs:
   \itemize{
     \item {CFE:}{ Slope of linear model in subset}
     \item {Pval:}{ Significance value of linear model in subset}
     \item {}{ Mean of the dependent variable in subset}
     \item {MEAN_"i-th_exp_var":}{ Mean of the i-th experiment-support variable in subset}
     \item {...}{}
     \item {Min_"i-th_var":}{ Minimum of the forcing variable in subset}
     \item {Max_"i-th_var":}{ Maximum of the forcing variable in subset}
     }
}
\seealso{
  \code{\link{plot.MSA_coef}}, \code{\link{GiFACE}}, \code{\link{summary.MSA_coef}}
}
\references{
  W. A. Obermeier, L. W. Lehnert, C. I. Kammann, C. Mueller, L.
  Gruenhage, J. Luterbacher, M. Erbs, G. Moser, R. Seibert, N. Yuan,
  J. Bendix (under review) Reduced CO2 fertilization in temperate C3 grasslands
  under more extreme weather conditions. Nature Climate Change
}
\details{
  The function derives the CO2-fertilization effect (CFE) and relates it to the variable environmental conditions. The CFE is calculated by the regression between a dependent variable such as total aboveground biomass and the CO2 concentration in the air. Please note the the value of the function \code{\link{moving_subset_analysis}} is of class '\code{MSA_coef}' for which several methods are available to plot and print the results.
  
  Each experiment-support variable can be used as forcing variable, for which the influence on the CFE is analyzed through the following steps:
\enumerate{
 \item{}{In the first step, the dataset is rearranged in ascending order of the forcing experiment-support variable. The total dataset is then partitioned into subsets where each contains observations featuring similar environmental characteristics. Taking the rainfall as an example, the first subset will encompass the driest years. For the second one, the year with the lowest rainfall sum is dropped and replaced by the year with the next lowest rainfall sum. This is repeated until the last subset is reached which encompasses the wettest years. For the GiFACE dataset with 16 years (1998-2013) of available data, 12 subsets are created for a window size of 30 observations (each subset encompassing six rings and five years).}

 \item{}{The CFE is then derived as the slope of the regression model between the CO2 concentration of the air and the selected dependent variable, which is calculated separately within each of the subsets defined above.}

 \item{}{By presenting the slope and its significance against the average of the forcing experiment-support variable in the subset, the influence of the respective variable on the CFE is revealed. However, situated in natural environments, most of the experiment-support variables will be highly correlated and, thus, the influence of the forcing variable has to be interpreted carefully. Therefore, the \pkg{msaFACE} contains the possibility to plot the averages of the accompanying experiment-support variables in the subsets, enabling a comprehensive picture of the prevailing environmental conditions. All this can easily be achieved by the function \code{\link{plot.MSA_coef}}.}
}
}

\examples{
data(GiFACE)

msa_data <- moving_subset_analysis(GiFACE$data, "CO2A_Mean", "BYT", 30, group = GiFACE$year)

}
\keyword{ utilities }
