\name{ctpbw}
\alias{ctpbw}
\docType{data} 
\title{
The clinical trial design for weak biomarker effect
}
\description{
The dataset contains the 3-D rotable figure for the simulated nested two-subset design(weak biomarker effect), from which the optimal choice of each population's proportion and the optimal alpha split can be abtianed. 
}
\format{
  The format is:
List of 5
 $ plot_power     :plotly object
 $ plot_alpha     :plotly object
 $ opt_r_split    : numeric vector
 $ opt_power      : numeric 
 $ opt_alpha_split: numeric vector 
\describe{
   \item{plot_power}{the 3-d rotatable plot of optimal power versus r2 and r3.}
   \item{plot_alpha}{the 3-d rotatable plot of optimal alpha versus r2 and r3}
   \item{opt_r_split}{The optimal cutoffs for the 3 dimensional clinical design}
   \item{opt_power}{The optimal power values corresponding to the optimal r split}
   \item{opt_alpha_split}{The optimal alpha split corresponding to the optimal r split}
}
}

\keyword{datasets}
