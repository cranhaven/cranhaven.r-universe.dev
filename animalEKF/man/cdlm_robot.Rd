\name{cdlm_robot}
\alias{cdlm_robot}
\title{
Shiny app for 1D simulation of robot movement with CDLM.
}
\description{
Shiny app for 1D simulation of robot movement with CDLM.
}
\usage{cdlm_robot()}
\details{
This shiny app illustrates a 1-D robot movement model.  Here, T ("maximum number of iterations") steps are simulated for a robot moving along a 1-D line.  Each of the T steps represents a length of time represented by "time step (sec)" seconds; the longer the interval, the more location uncertainty there is between steps.  
At each step, the robot moves with velocity (v_t) modeled by a normal distribution with mean alpha ("unknown true mean of velocity") and variance "known true variance." For simplicity, we will only attempt to model the mean velocity while sequentially observing only the locations, since we assume the variance is known. 
The particle filter learns the movement parameters through N ("number of particles") particles, or independent simulations.  At each point in time, the filter simulates N draws of the velocity from the prior distribution, a normal distribution with mean mu ("prior mean on velocity mean") and variance sigma ("prior variance on velocity mean"). These distributions are shown in color panel 1.  Ideally, over time the colored distributions should converge to the true one (thick black curve).

Panel 1 shows the particles' distributions of the velocities.  Ideally the means of these distributions should converge to the true value (vertical line).
Note: this simulation works best if the distribution of true velocity is either clearly negative or positive.  If the distribution straddles v_t=0 with significant probability, movement will be more difficult to visualize.  In the above case, the location should be the one more in the direction of the sign of velocity (i.e. if velocity distribution >0, then the robot should be consistently moving to the right).

Panel 2 shows each particle's prediction of the location (black dot) and the 95\% confidence interval of this prediction (width of colored rectangle).  The true observed location and the previous one are the two dashed vertical lines.  Particles whose dots fall closer to the vertical line have better prediction.

Panel 3 shows the weights of the particles as calculated by the closeness of their location prediction to the true one.  Closer particles in panel 2 should have higher weights (the colors correspond).

Panel 4 shows the predictions and confidence intervals of particles being resampled by their weights (with replacement).  More of the predictions should be closer to the truth here than in panel 2.

Panel 5 shows convergence over time of the means of the particle distributions of velocity (panel 1) to the true value (vertical dashed line). Ideally these should converge to the true value. 

Panel 6 shows the history of predicted locations over time, by vertical lines representing each particle's predictions.  Ideally the particle predictions should both converge to the observed locations
and should also be grouped closer together, as the estimated velocity distribution standard deviation decreases.

The particle filter models the true value of location and the true velocity.  It is reasonable to assume that these are independent, which is why the covariance matrices given are diagonal. 
}

\references{
Ackerman, Samuel.  "A Probabilistic Characterization of Shark Movement Using Location Tracking Data."  Temple University doctoral thesis, 2018.  \url{https://digital.library.temple.edu/digital/collection/p245801coll10/id/499150}

Carvalho, Carlos M., Johannes, Michael S., Lopes, Hedibert F., and Nicholas G. Polson.  "Particle learning and smoothing."  Statistical Science, 2010.

}
\note{
Video explanation of simulation applet by author: \url{https://youtu.be/iVG_bCU0jCA}

}

