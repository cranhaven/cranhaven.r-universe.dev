# elfgen <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/elfgen)](https://cran.r-project.org/package=elfgen)
[![R-CMD-check](https://github.com/HARPgroup/elfgen/workflows/R-CMD-check/badge.svg)](https://github.com/HARPgroup/elfgen/actions)
[![codecov](https://codecov.io/gh/HARPgroup/elfgen/branch/master/graph/badge.svg?token=K9sGaBh54U)](https://app.codecov.io/gh/HARPgroup/elfgen)
<!-- badges: end -->

# Overview
 
elfgen is an instream flow framework for rapid generation and optimization of flow-ecology relations. The elfgen framework centers on the generation of Ecological Limit Function models (ELFs) describing the relation between maximum species richness and stream size characteristic (streamflow or drainage area). The elfgen framework was developed (By DEQ in cooperation with USGS and Virginia Tech) in response to a need for better environmental flow metrics for assesment of potential impacts of water use. For the first time this new framework may allow quantification of potential species loss resulting from flow change, and may offer an improved understanding of aquatic life risk variability due to geographic location, stream size and local scale.

Supporting manuscripts have been published in the Journal of the American Water Resources Association (JAWRA):
- *elfgen: A New Instream Flow Framework for Rapid Generation and Optimization of Flow–Ecology Relations.*
   - Kleiner et al. (2020): https://onlinelibrary.wiley.com/doi/full/10.1111/1752-1688.12876
- *Application of a New Species-Richness Based Flow Ecology Framework for Assessing Flow Reduction Effects on Aquatic Communities*
   - Rapp et al. (2020): https://onlinelibrary.wiley.com/doi/full/10.1111/1752-1688.12877

# Installation

``` r
# Install the current released version from CRAN:
install.packages("elfgen")

# Or install the development version from Github:
# install.packages("devtools")
devtools::install_github("HARPgroup/elfgen")
```

# Usage
An introductory example of how elfgen works is supplied below. You start 
by either supplying a dataset with flow and richness data, or by supplying 
a HUC code of interest. When supplying a HUC code: `elfdata()` will 
retieve IchthyMaps data from USGS ScienceBase using the `sbtools` package and automatically derive fish species richness at the NHDPlusV2 segment scale. Mean annual flow data is then automatically retrieved for each NHDPlusV2 segment using the `nhdplusTools` package.

A breakpoint in the flow-ecology relation is determined using a fixed user-defined
value, or identified using the functions `bkpt_pwit()` or `bkpt_ymax()`. The ELF
model is then generated and plotted using `elfgen()` with ELF model statistics
returned.

Additional richness change analyses may be performed using the functions `richness_change()` and `elfchange()` (See below)


## Example
### Load package and data.

``` r
library(elfgen)

# Retrieve dataset of interest
# You may enter either a 6, 8, 10, or 12-digit HUC code
# *Notes: 
#    By default the ichthy dataset is downloaded to a temp directory, however this may be overridden by 
#      supplying a local path of interest using the input parameter "ichthy.localpath"
#    6-digit HUCs like the following example may take a few minutes to process with elfdata() due to the
#      large number of contained IchthyMaps historical stream fish distribution data
watershed.df <- elfdata(watershed.code = '02080201', ichthy.localpath = tempdir())
```

``` r
# Alternatively, utilize a user-supplied dataset in the following format:
watershed.df <- data.frame(flow=c(28.257, 20.254, 22.825, ...), 
			   richness=c(2, 10, 12, ...),
			   watershed='02080201',
			   stringsAsFactors=FALSE) 
```



### Identify breakpoint in flow-ecology relation using 1 of 3 methods.
* **Fixed Method**: This approach utilizes a user specified breakpoint value. This "fixed breakpoint" is typically 
	determined through visual inspection of the flow-ecology relation by the user.
* **Piecewise Iterative Method**: This approach uses an iterative algorithm to identify shifts in the relation between maximum richness 
	and stream size. A user specifies a `"quantile"` for isolating an upper subset of the data. A user also 
	identifies a bounding range between two x-values (`"blo"` = “bound low”, `"bhi"` = “bound high”) in which the 
	upper subest of data is believed to contain a breakpoint. (Note: Small datasets may not contain a breakpoint)
* **Ymax Method**: This approach treats the maximum observed species richness value as the breakpoint. This function begins 
	by locating the point with the highest y-value in the full dataset, then utilizing the associated x-value 
	as the breakpoint.

``` r
# Fixed Method
breakpt <- 500

# Piecewise Iterative Method
breakpt <- bkpt_pwit("watershed.df" = watershed.df, "quantile" = 0.95, "blo" = 200, "bhi" = 500)  
#> [1] "Breakpoint identified at 310.815"
		
# Ymax Method		
breakpt <- bkpt_ymax("watershed.df" = watershed.df)			   
#> [1] "Breakpoint identified at 142.989"
```

### Plot flow-ecology relation and generate ELF model.	
* A user specifies a `"quantile"` for isolating the upper subset of the data. The breakpoint `"breakpt"` determined using one of the 
	above methods is passed in. `"yaxis_thresh"` is used to customize the maximum y-axis limit. Custom x-axis and y-axis plot labels can 
	be specified using `"xlabel"` and `"ylabel"` respectively.

	
``` r				   
elf <- elfgen("watershed.df" = watershed.df,
	      "quantile" = 0.95,
	      "breakpt" = breakpt,
	      "yaxis_thresh" = 53, 
	      "xlabel" = "Mean Annual Flow (ft3/s)",
	      "ylabel" = "Fish Species Richness")
```

``` r
elf$plot
```

![](man/figures/README-example-1.png)<!-- -->

``` r
elf$stats
#>     watershed breakpt quantile    m    b rsquared rsquared_adj p n_total n_subset n_subset_upper
#> 1	02080201 142.989     0.95 2.34 9.19    0.806          0.8 0     861      705             35
```

## Richness Change
* **Calculate absolute richness change** (resulting from flow reduction): Supply the elf stats derived above to the function `richness_change()` and input the percent reduction in flow `"pctchg"` (10 = 10% reduction in flow)
* **Calculate percent richness change** at a specific stream size (resulting from flow reduction): When an `"xval"` parameter is supplied, the function will calculate the percent change in richness 
at a specific stream size (For this example 50 = a stream size with mean annual flow of 50 cfs) 

``` r
# Calculate absolute richness change (resulting from flow reduction)
richness_change(elf$stats, "pctchg" = 10)
#> [1] "Absolute Richness Change:"
#> [1] -0.2465436

# Calculate percent richness change at a specific stream size
richness_change(elf$stats, "pctchg" = 10, "xval" = 50)
#> [1] "Percent Richness Change at x = 50:"
#> [1] -1.343992
```

* **Plot percent richness change for various percent flow reductions**: Supply the elf stats to the function `elfchange()`. `"yaxis_thresh"` is 
used to customize the maximum y-axis limit. Custom x-axis and y-axis plot labels can 
be specified using `"xlabel"` and `"ylabel"` respectively.

``` r
# Generate plot of percent richness change for various percent flow reductions
elfchange(elf$stats, "yaxis_thresh" = 25)
```

![](man/figures/README-example-2.png)<!-- -->
