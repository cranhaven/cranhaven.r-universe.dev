\name{impulsivity}
\alias{impulsivity}
\docType{data}
\title{
Event-Related Potentials data from a study conducted by Shen et al. (2014) to investigate neural correlates of
impulsive behavior. }
\description{
In the study, adult males who scored at the top 25\% and bottom 25\% on a scale measuring trait impulsivity were recruited to participate in an ERP experiment.
There were twelve subjects in each impulsivity trait group (high or low).
Specifically, the influence of trait impulsivity in response inhibition was examined in a two-choice discrimination task using the stop-signal paradigm 
(Logan et al, 1984). Participants were asked to withhold their responses when a 'stop' signal appeared after the 'go' stimulus. ERP amplitudes 
from 32 scalp locations (channels) were recorded from 200 ms before signal onset till 1000 ms after with one reading per 2 ms.
Two of the 32 channels were excluded in the analysis by the authors. The response inhibition is a within-subject factor with two levels (successful or failed).
}
\usage{data("impulsivity")}
\format{
A data frame with 144 observations on the following 505 variables. The first four variables are categorical: \code{Channel} with levels \code{CPZ} \code{CZ} \code{FCZ} giving
the electrode position, \code{Subject} with levels \code{S1} \code{S2} ... to identify the subjects, \code{Group} with levels \code{High} \code{Low} for the membership to 
an impulsivity group and \code{Condition} with levels \code{Failure} \code{Success} for the response inhibition condition. The remaining 501 variables are the ERP values at
time 0 (onset), 2ms, 4ms, ..., 1000ms.
}
\source{

Shen, I., Lee, D., and Chen, C. (2014), The role of trait impulsivity in response inhibition: event-related
potentials in a stop-signal task. International Journal of Psychophysiology, 91(2).

}
\references{

Causeur, D., Sheu, C.-F., Perthame, E. and Rufini, F. (2018). A functional generalized F-test for signal detection with applications to event-related potentials significance analysis. Submitted.

Logan, G. D., Cowan, W., and Davis, K. (1984), On the ability to inhibit simple and choice reaction
time responses: a model and a method. Journal of Experimental Psychology: Human Perception
and Performance, 10(2), 276-291.

}
\examples{

data(impulsivity)

head(impulsivity[,1:10])  # each row of impulsivity contains an ERP curve (from column 5 to 505)
                  # Column 1: electrode position (channel)
                  # Column 2: subject id
                  # Column 3: impulsivity group (High/Low)
                  # Column 4: response inhibition condition (Success/Failure)  

time_pt = seq(0, 1000, 2)     # sequence of time points (1 time point every 2ms in [0,1000])
erpdta = impulsivity[,5:505]          # erpdta contains the ERP curves
T = length(time_pt)           # number of time points

covariates = impulsivity[,1:4]        # contains the experimental covariates of interest

# Impulsivity ERP design

with(data=covariates,table(Channel,Condition,Group)) 
   # 48 ERP curves for each of the 3 channels
   # Within each channel, 12 ERP curves in each Condition x Group
   # Within each channel, 12 subjects in group High, 12 in group Low

}
\keyword{datasets}

