# iemisc

R package that contains Irucka Embry&#8217;s miscellaneous functions:  

* statistical analysis
    + RMS,
    + coefficient of variation (CV),
    + approximate and relative error,
    + range,
    + harmonic mean,
    + geometric mean,
    + maximum mean relative error (MAXRE),
    + and mean relative error (MRE)];
* engineering economics
    + benefit-cost,
    + future value,
    + present value,
    + annual value,
    + gradients,
    + interest,
    + periods,
    + etc.;
* geometry
    + polygon area,
    + sphere volume,
    + and right triangle validation;
* civil & environmental/water resources engineering
    + Concrete Mix Design for Normal Strength & Structural Lightweight Concrete,
    + Construction Measurements with and without Fractions,
    + Engineering Surveying Calculations,
    + Air Stripping,
    + Saturated Vapor Pressure for Ice and Water,
    + Density of Water,
    + Dynamic and Kinematic Viscosity of Water,
    + Specific Gravity and Volume,
    + Unit Weight,
    + Surface Tension of Water,
    + Weighted C Factor,
    + Weighted Curve Number,
    + Darcy friction factor (f),
    + the Reynolds number,
    + Manning's n,
    + Gauckler-Manning-Strickler equations for open channel flow,
    + Modified Rational Method Equation,
    + Linear Surface Area calculation,
    + Rain Garden sizing based on driveways;
* convert a fraction (or mixed number) to a decimal (numeric vector);
* negation of chin from data.table and negation of in from base R (%notchin%);
* quick search (%qsin%);
* return character vectors in order (%inorder%);
* mortality rate calculations;
* proportion solver;
* sum of all digits in a vector to a single integer;
* string manipulation (splitcomma and splitremove);
* a version of linear interpolation for use with NAs;
* Python compatible floor division function (%//%);
* GNU Octave/MATLAB compatible trigonometric functions in degrees; and
* GNU Octave/MATLAB compatible
    + section properties (secprop),
    + remainder (Rem),
    + modulus (Mod_octave),
    + number of dimensions (ndims),
    + row vector (isrow) and column vector (iscolumn) tests,
    + fractional differences (fractdiff),
    + size,
    + numel,
    + and length functions.


This package can be used in academia by students and faulty alike and/or in professional settings.  



# Installation of iemisc from CRAN

```R
install.packages("iemisc")
```


# Help

With credit due to the `matlab` package, for a complete list of functions and the package DESCRIPTION file, use:  

```R
library(help = "iemisc")
```


With credit due to [Getting Help with R](https://www.r-project.org/help.html) for the following 2 methods of help:  


```R
help(shm, package = "iemisc") # sample harmonic mean function help

help(package = "iemisc") # help for the iemisc package
```


# Vignette Viewing

If you wish to browse the `iemisc` Vignettes, please copy-and-paste the following code into R:  

```R
library(iemisc)

utils::browseVignettes("iemisc")
```


# Examples (see more examples in the vignettes and in the functions)

```R

# 1)

install.load::load_package("iemisc", "rando")

set_n(200) # makes the example reproducible

samp <- r_norm(.seed = 200) # sample

# Calculate the sample harmonic mean (SHM) of the 200 values
# Using the default value of na.rm = FALSE
# using a matrix of the numeric vector obs1

samp1 <- matrix(data = samp, nrow = length(samp), ncol = 1, byrow = FALSE,
dimnames = list(c(rep("", length(samp))), "Sample"))

shm(samp1)



# 2)

install.load::load_package("iemisc", "rando")

# Compute the relative error of the 210 values
set_n(210) # makes the example reproducible

true <- r_norm(.seed = 210) # true

approx <- r_norm(.seed = 210) # approximation

relerror(true, approx)



# 3)

library(iemisc)

# Are any of the following right triangles?

righttri(2, 7) # a = 2, b = 7

righttri(a = 4, c = 11)

righttri(b = 4, c = 5)



# 4)

library(iemisc)

# What is the future worth of $2,390.90 in the present 13 years from now with a
# 0.25% interest rate compounded annually?

FgivenP(2390.90, 13, 0.25, frequency = "annual") # the interest rate is 0.25%
```


# Disclaimer

This software is provided &ldquo;AS IS&rdquo;. See the GPL License for more information.  



# License

`iemisc` is distributed under the GPL-3 (or later) license, as stated in the DESCRIPTION file. For more info, see the [GNU General Public License (GPL) page](https://www.gnu.org/licenses/gpl-3.0.html).  



# Citation

Please refer to the CITATION file for the correct way to cite `iemisc`.  



# Donations Accepted

If you want to support the continued development of this and my other R packages, feel free to:  

<p><script src="https://liberapay.com/iaembry/widgets/button.js"></script>
<noscript><a href="https://liberapay.com/iaembry/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a></noscript></p>
