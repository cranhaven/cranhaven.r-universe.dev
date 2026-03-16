\name{shark_vis_longlat}
\alias{shark_vis_longlat}
\docType{data}
\title{
Shiny app for visualizing observed shark movement.
}
\description{
Shiny app for visualizing observed shark movement.
}
\usage{shark_vis_longlat()}

\details{
This shiny app visually illustrates movement of sharks in the dataset referenced in the paper below.  The observations to be visualized are selected in "Range of observed steps".  They are then linearly interpolated with step size "seconds to interpolate".  In our paper, we model the impact of other sharks' behaviors in a spatial-temporal neighborhood.  If desired, a spatial neighborhood of desired size "spatial radius" will appear around each shark in the presence of other sharks.  It will flash red if another shark enters in that radius (i.e., they are neighbors).
}
\source{
Espinoza, Mario, Farrugia, Thomas J., and Christopher G. Lowe. Habitat use, movements
and site fidelity of the gray smooth-hound shark in a newly restored Southern
California estuary. Journal of Experimental Marine Biology and Ecology, 2011.
}

\keyword{datasets}
