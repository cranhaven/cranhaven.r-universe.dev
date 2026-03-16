\name{cdlm_robot_twostate}
\alias{cdlm_robot_twostate}
\title{
Shiny app for simulation of 1D robot movement with CDLM and two states.
}
\description{
Shiny app for simulation of 1D robot movement with CDLM and two states.
}
\usage{cdlm_robot_twostate()}
\details{
See \code{cdlm_robot} for explanation of the basic concepts.  This function is similar except there are two behavioral states (1 and 2, "slow"/"fast") to model, as well as the switching probabilities between them.

The means of the velocities of the two behaviors are simulated by a normal distribution with two means alpha ("unknown true mean of velocity", types 1 and 2).  The variance in each case is the same and known, as before. The prior means and variances of the velocities are assigned as before. 

The transition probabilities between the behaviors are given by "transition probability between type 1 and 2" and "2 and 1".  If box "are transition probabilities known?" is checked, then they are known.  Otherwise, the transition probabilities will be estimated by a Dirichlet prior (vector "Dirichlet prior values" of form 1->1, 1->2, 2->1, 2->2 of positive numbers, which should roughly correspond to the true probabilities in ratio).  Note that the predictions in this simulation are unlikely to be as good as in the prior 1-D example since there are more parameters to learn and only a limited number of timesteps or particles.

Panel 1 shows the particles' distributions of the velocities for each behavior.  This simulation works best if the distributions are well-separated.

Panel 2 shows the location predictions for each behavior.  The black dot indicates the mean predicted location, and the 
rectangle width is the width of the 95\% confidence interval.  The rectangle for behavior 1 is solid, for behavior 2 it has crosshatches.

Panel 3 shows the overall resampling weights for the particles, as well as the behavior-conditional ones.  The higher the behavior-conditional weight bar is, the better the
particle's prediction at that behavior matches what was observed.  The overall weight (top row) is the average of the 
conditional weight values, weighted by the transition probability into that behavior.

Panel 4 shows the resampled particles, along with their prediction of location and behavior type.  Ideally, the resampled rectangles should be centered around the observed point.  
It is not necessarily true that the resampled (most likely) rectangles will be the narrowest, since the likelihood of the behavior predicting the observed location is a combination of
both the density of that location at the prediction distribution (closeness to the center), as well as the likelihood (transition probability) of having that behavior, given the previous one.
In panel 4, the particle predictions are shown one at a time as that particle is resampled; the weight bar in panel 3 should be in bold as that particle is selected.

Panel 5 shows convergence over time of the means of the particle distributions of velocity (panel 1) to the true value (vertical dashed line). Ideally these should converge to the true value. 

Panel 6 shows the history of predicted locations over time, by vertical lines representing each particle's predictions.  Ideally the particle predictions should both converge to the observed locations
and should also be grouped closer together, as the estimated velocity distribution standard deviation decreases.

Panel 7 shows the estimated distributions of the behavior switching probabilities (if they are not known).  The true probabilities are shown by a vertical line, and ideally the mean
of the estimated distribution should be around there.

Panel 8 shows the accuracy of particle predictions of the behavior.  The color (1=black, 2=gray) is the true behavior type, and the height of the bar is the fraction of particles correctly predicting it.  Ideally, all bars should be high.

}
\references{
Ackerman, Samuel.  "A Probabilistic Characterization of Shark Movement Using Location Tracking Data."  Temple University doctoral thesis, 2018.  \url{https://digital.library.temple.edu/digital/collection/p245801coll10/id/499150}

Carvalho, Carlos M., Johannes, Michael S., Lopes, Hedibert F., and Nicholas G. Polson.  "Particle learning and smoothing."  Statistical Science, 2010.

}
\note{
Video explanation of simulation applet by author: \url{https://youtu.be/4XR8eB89z7E}
}
