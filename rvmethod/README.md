The **rvmethod** package is designed to implement the Radial Velocity (RV) method for detecting exoplanets through the periodic motion of their host star. Particularly, this uses the methodology from Holzer et. al (2020) that introduces the Hermite-Gaussian Radial Velocity (HGRV) approach that simplifies the process to simple linear regression.

The process of implementing this method through this package is the following sequence of steps:

* feed a time-series of normalized stellar spectra into the function *estimate_template( )* to estimate the template spectrum
* apply the absorption feature finding algorithm to the estimated template by feeding it into the *findabsorptionfeatures( )* function
* fit Gaussians to the absorption features found in the template spectrum with the *gaussfit( )* function
* estimate the RV in each stellar spectrum by feeding the estimated template spectrum and Gaussian fit parameters into the *hgrv( )* function

To install the package, run the following command in R: install.packages("rvmethod") 

Along with the functions mentioned in the sequence above, there are some example data sets that are used in examples given in the documentation.