
<!-- README.md is generated from README.Rmd. Please edit that file -->

# QuAnTeTrack

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/QuAnTeTrack)](https://CRAN.R-project.org/package=QuAnTeTrack)
[![R-CMD-check](https://github.com/MacroFunUV/QuAnTeTrack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MacroFunUV/QuAnTeTrack/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/MacroFunUV/QuAnTeTrack/graph/badge.svg)](https://app.codecov.io/gh/MacroFunUV/QuAnTeTrack)
<!-- badges: end -->

Understanding the movement and behavior of extinct tetrapods is a
fundamental aspect of palaeobiology, offering a glimpse into how these
organisms interacted with their environment and each other. Fossil
trackways provide a dynamic record of locomotor patterns, ecological
interactions, and even potential social behavior. However, extracting
meaningful information from these ancient tracks requires robust
analytical tools capable of processing complex datasets.

Introducing **QuAnTeTrack** (**Qu**antitative **An**alysis of
**Te**trapod **Track**ways), an integrated R package specifically
designed to facilitate the semi-automated extraction of palaeobiological
insights from fossil trackways. This versatile tool allows researchers
to seamlessly convert digitized footprint data into analytical objects
and apply a range of statistical and graphical methods to explore
locomotor and ecological hypotheses.

The QuAnTeTrack workflow begins with **data digitization**, where
footprint coordinates are recorded and saved in `.TPS` files using tools
like **tpsUtil** and **tpsDig**. These files are then converted into
structured R objects using the `tps_to_track()` function, transforming
raw coordinates into well-organized datasets that can be easily
manipulated and analyzed.

Once the data is properly structured, **exploratory analyses** can be
conducted to assess fundamental movement parameters. Functions like
`track_param()` provide detailed information on turning angles, track
distances, step lengths, sinuosity, and straightness. Simultaneously,
the `velocity_track()` function allows users to estimate locomotor speed
and relative stride length, providing crucial insights into gait and
locomotor performance. Visualizing these results is made simple through
functions like `plot_track()` and `plot_velocity()`, which generate
high-quality, publication-ready graphs.

Beyond exploratory analysis, **QuAnTeTrack** offers powerful tools for
**statistical testing and hypothesis evaluation**. Functions like
`test_direction()` and `test_velocity()` allow users to test for
directional consistency and velocity differences among tracks, while
`mode_velocity()` assesses whether trackmakers were accelerating,
decelerating, or maintaining steady speed along their paths.

A central aspect of the package is its ability to **simulate tracks
under different movement models** (`simulate_track()`). These models are
informed by geological and environmental constraints, allowing
researchers to evaluate how landscape features or resource availability
may have influenced ancient trackmakers’ paths. The `plot_sim()`
function provides an intuitive way to compare simulated tracks against
the original dataset.

Once simulated tracks are generated, **QuAnTeTrack** provides robust
tools to **test ecological and ethological hypotheses**. Trajectory
similarity can be assessed through **Dynamic Time Warping (DTW)** and
**Fréchet distance metrics** (`simil_DTW_metric()` and
`simil_Frechet_metric()`), while potential interactions between
individuals can be quantified using the `track_intersection()` function.
By comparing these metrics against null models generated from
simulations, researchers can assess whether trackways display patterns
suggestive of coordinated behavior, pursuit, or other ecologically
significant interactions.

Additionally, **QuAnTeTrack** supports combining multiple metrics into
comprehensive tests of hypothesis robustness using the `combined_prob()`
function. This allows researchers to aggregate the results of similarity
metrics, intersection counts, and other statistics into a single overall
measure of similarity or interaction significance.

The package also includes functionality to **cluster tracks based on
movement parameters** (`cluster_track()`). This tool is particularly
useful for detecting distinct behavioral modes within a dataset or for
grouping tracks that share similar movement characteristics prior to
further analysis.

Throughout the workflow, **QuAnTeTrack** offers flexibility in
visualizing, testing, and comparing tracks. The use of R’s powerful
visualization tools ensures that all results can be effectively
communicated and further refined as necessary.

By integrating data processing, statistical testing, simulation
modeling, and visualization into a single, user-friendly package,
**QuAnTeTrack** provides a comprehensive framework for analyzing
tetrapod trackways and testing complex ecological and behavioral
hypotheses.

## Installation

To install the **QuAnTeTrack** package, you can choose between
installing the **stable version from CRAN** (recommended) or the
**development version from GitHub**.

### **From CRAN (recommended)**

To install the stable version from CRAN, use:

``` r
install.packages("QuAnTeTrack")
```

### **From GitHub (development version)**

If you want the latest development version, you will need to use the
`devtools` package. If you haven’t installed `devtools` yet, you can do
so with the following command:

``` r
install.packages("devtools")
```

Once `devtools` is installed, you can install **QuAnTeTrack** using:

``` r
devtools::install_github("MacroFunUV/QuAnTeTrack")
```

If you have already installed **QuAnTeTrack** and want to ensure you
have the latest version, you can update it with:

``` r
devtools::install_github("MacroFunUV/QuAnTeTrack", force = TRUE)
```

## Usage Details & Functionality

For a detailed description of the package functionalities, including
usage examples and explanations of key functions, a detailed vignette is
available
[online](https://macrofunuv.github.io/QuAnTeTrack/articles/QuAnTeTrack.html).
