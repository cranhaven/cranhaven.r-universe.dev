# DesignCTPB

**USER NEED TO KNOW: NVIDIA GPU CARD IS A MUST FOR RUNNING OUR PACKAGE AND BEFORE YOU USE OUR PACKAGE, PLEASE CHECK CUDA AND CUDATOOLKIT ARE WELL INSTALLED. AS FOR THE INSTALLATION OF CUDA DRIVER, PLEASE REFER TO: https://www.nvidia.com/Download/index.aspx**

This is the beta version of R package for designing clinical trial with potential biomarker effect. 
  
For a given setting of input parameters, this package can solve up to 5-dimension alpha-split problems. This can also be expended to handle higher dimension problems. But in practice, we do not suggest consider too high dimensions, since considering too many subpopulation leads to too much loss in power, and not being the optimal choice.\
This package can also guide the choice of size of nested populations, i.e. find optimal r-values. The function visualizes and optimizes r-values, but only supports 3-dimension. The optimization of r-values in more than 3-dimension is trivial, but visualization can be too hard.

We implemented it with GPU computing and smoothing method(thin plate spline). 

## How to install in R:

devtools::install_github("ubcxzhang/DesignCTPB")

## How to run in R:

### Auto-Setting Python environment and loading package
library(DesignCTPB)

### Calculating optimal alpha-split for a given setting of input parameters
alpha_split(r=c(1,0.5,0.3),N3=2000,sd_full=1/sqrt(20),delta_linear_bd = c(0.2,0.8))

### Calculating optimal alpha-split for many settings of r values (i.e. size of nested subpopulations), and visualize their results and calculate optimal choice of r values
res <- design_ctpb(m=24, n_dim=3, N3=2000, sd_full=1/sqrt(20),delta_linear_bd=c(0.2,0.8))\
res$plot_alpha # *<font face = "Times New Roman">to see the 3-d rotatable plot of optimal alpha versus r2 and r3.</font>*\
res$plot_power # *<font face = "Times New Roman">to see the 3-d rotatable plot of optimal power versus r2 and r3.</font>*\
res$opt_r_split\
res$opt_alpha_split\
res$opt_power

**The default inputs give the results of the strong biomarker effect in our paper. Users can change the values of input parameters to generate plot and obtain the optimal alpha and r values. <!--In practice, we never design nested-population clinical trails where there is no biomarker effect. So we do not suggest users to use our settings as "the no biomarker effect" example in our paper, which is only for demostrate when no biomarker effect, our method make the correct choice of put all alpha- values on full population. -->

In our package, the user can specify the standard deviation of each population by giving SIGMA as input, and the harzard reduction rate DELTA for each population. Just give input values to SIGMA and DELTA, but note that the entered matrix should coincides with the matrix of r-split setting. \
  *<font face = "Times New Roman">(e.g. if m=24 and n_dim=3, which means we are going to have 276 r-split setting(like our default setting), so each row of the SIGMA(DELTA) matrix should coincides with the corresponding row of r-split setting).</font>*\
For obtaining the r-split setting, user can specify it personalized or follow our r_setting(m,n_dim) function. 

#### Note for selection of N3
We are developing a better selection of N3, than presented in our paper, which should consider the proportions of each subset. This feature will be in the production version of this package.

## R Dependencies:

R/4.0.2\
reticulate(Package to interface python in R)\
mnormt/fields/plotly/dply

## Python Dependencies:

Python >=3.6.3
numba >=0.46.0 
scipy/numpy/pandas

## GPU and other Dependency 


gcc/7.3.0\
CUDA Tookit >=9.0 



