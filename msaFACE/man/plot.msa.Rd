\name{plot.MSA_coef}
\alias{plot.MSA_coef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot function for moving subset analysis
}
\description{
Create main or accompanying plot(s) of the moving subset analysis
}
\usage{
\method{plot}{MSA_coef}(x, i_var = 1:length(x), main_plot = TRUE, i_acc = 1, 
     axis.param = list(), label.param = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
    Object of class \code{MSA_coef}. Output of \code{\link{moving_subset_analysis}}.
}
\item{i_var}{
    Column number or name of the forcing variable used to define the subset which will be plotted. Default is to plot all available variables.
}
\item{main_plot}{
    Flag to determine whether to plot the main plot(s) (default) or the accompanying plot(s)
}
\item{i_acc}{
    If main_plot = FALSE, column number(s) of the experiment-support variable(s) to be plotted as accompanying variable(s)
}
\item{axis.param}{
    List containing the axis parameters (e.g. lty, col, tck). By default for main plots, first entry represents x-axis (forcing variable), second entry represents first y-axis (regression result), and third entry the second y-axis (dependent variable). See example section.
}
\item{label.param}{
    List containing the label parameters (e.g. line, cex). For order of the list refer to axis.param. See example section.
}   
\item{...}{
    Parameters passed to generic plot function
}


%   The example performs the moving subset analysis on the GiFACE dataset. Biomass (BYT) is the dependent variable and the CO2 Concentration (three month average, CO2A_Mean) is the independent variable. The years are used to group the observations used for regression
}
\author{
Wolfgang A. Obermeier, Lukas W. Lehnert, Joerg Bendix
}

\seealso{
  \code{\link{moving_subset_analysis}}, \code{\link{GiFACE}}, \code{\link{summary.MSA_coef}}
}

\examples{
data(GiFACE)

### Perform Moving Subset Analysis
msa_data <- moving_subset_analysis(GiFACE$data, "CO2A_Mean", "BYT", 30, group = GiFACE$year)

## Plot all final plots
plot(msa_data)

\dontrun{
## Example to produce similar plots as in article
pdf("Plot_NCC_1.pdf", width = 15, height = 20)
par(mfcol = c(9,4), mar = c(4,4,1,4), las = 0)
for(i in 1:4){
  plot(msa_data, i_var = i, ylim = list(c(0,2.5), c(200,350)), 
       legend = i == 1)
  plot(msa_data, i_var = i, main_plot = F, i_acc = 1:8)
}
dev.off()
}

\dontrun{
## Example to adjust axis and label parameters
par(mar = c(3.2,3.5,1,3), mfrow = c(2,1))
## Plot default for comparison
plot(msa_data, i_var = 2)     

## Change default axis (all parameters are passed to internal function "axis")
## Define x-axis specifications
xaxt_spec  <- list(tck = -0.02, padj = -0.3, cex.axis = 1.5, lwd = 2)
## Define 1st y-axis specifications 
yaxt1_spec <- list(at = c(0,1,2), tck = -0.02, padj = 0.5, cex.axis = 1.5, lwd = 2)
## Define 2nd y-axis specifications 
yaxt2_spec <- list(tck = -0.02, padj = -0.3, col = "gray50", col.axis = "gray50",
                   cex.axis = 1.5, lwd = 2)

## Change default labeling (all parameters are passed to internal function "mtext")
## Define x-axis label specifications 
xlab_spec <- list(side = 1, line = 2, cex = 1.5)
## Define 1st y-axis label specifications
ylab1_spec <- list(side = 2, line = 2, cex = 1.5)
## Define 2nd y-axis label specifications 
ylab2_spec <- list(side = 4, line = 2, cex = 1.5, col = "gray50")
                   
                   
## Define variable to be plotted, y-axis limits
plot(msa_data, i_var = 2, ylim = list(c(0,2.5), c(200,350)),                                     
     axis.param = list(xaxt_spec, yaxt1_spec, yaxt2_spec),
     label.param = list(xlab_spec, ylab1_spec, ylab2_spec),
     ## Define line width for horizontal lines (1) and the dependent variable (2)
     lwd = c(1,2),       
     ## Define point symbol
     pch = 20)                                                                                   
dev.off()
}
}
\keyword{ utilities }
