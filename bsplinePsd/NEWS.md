## bsplinePsd 0.6.0

Have included a scaling/rescaling factor within the code to remove numerical errors when data is close to machine precision.

Have optimised the starting values for the Dirichlet process parameters.

## bsplinePsd 0.5.0

Previous versions only allowed the user to use cubic B-spline densities.  This version allows the user to choose any degree.  Normalising the  B-splines now uses a trivial integral formula.

The function gibbs_bspline can now handle odd length time series.

An S3 plot method has been included so the user can easily plot their PSD estimate.

## bsplinePsd 0.2.0

Added an argument called k1 in the gibbs_bspline function.  This allows the user to specify the starting value for parameter k.  If well-chosen, this can speed up convergence significantly.  The default is set to 20, which works well on all of the cases I have come across.  If missing (NA), then a random integer between degree + 2 and kmax will be selected as the starting value for k.
