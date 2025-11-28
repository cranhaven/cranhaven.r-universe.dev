# pipenostics

[![License: GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/omega1x/pipenostics/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/omega1x/pipenostics/actions/workflows/R-CMD-check.yml)
[![pages-build-deployment](https://github.com/omega1x/pipenostics/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/omega1x/pipenostics/actions/workflows/pages/pages-build-deployment)
[![pipenostics status badge](https://omega1x.r-universe.dev/badges/pipenostics)](https://omega1x.r-universe.dev)
[![CodeFactor](https://www.codefactor.io/repository/github/omega1x/pipenostics/badge)](https://www.codefactor.io/repository/github/omega1x/pipenostics)
[![codecov](https://codecov.io/gh/omega1x/pipenostics/branch/master/graph/badge.svg?token=LMVLTBPAY5)](https://app.codecov.io/gh/omega1x/pipenostics)

[R-package](https://cran.r-project.org/package=pipenostics) for
diagnostics, reliability and predictive maintenance of pipeline systems.

------------------------------------------------------------------------

## Intro

The package aggregates to some extent the separate knowledge concerning
engineering, reliability, diagnostics and predictive maintenance of pipeline
systems. For the present time the package contains utilities for processing
corrosion data commonly gathered by *inline inspection*, as well as
empirical models for calculations of local thermal-hydraulic regimes of
district heating network.

## Installation

For the latest version leverage [r-universe](https://omega1x.r-universe.dev/pipenostics):

```R
install.packages("pipenostics", repos = "https://omega1x.r-universe.dev")
```

> &#9888; Starting from version 0.1.8 the package is not supported on [CRAN](https://cran.r-project.org/) due to its resource limitations of checking parallel algorithms

## Usage examples

### Corrosion diagnostics

By using of `b31crvl()` simply imitate the output of *CRVL.BAS* which is the honored software for determining the allowable length and maximum
allowable working pressure presented in [ASME B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf):

```R
library(pipenostics)
    
b31crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)
```

```txt
-- Calculated data --
Intermediate factor (A) = 1.847
Design pressure = 1093 PSI; Safe pressure = 1093 PSI
Pipe may be operated safely at MAOP, 910 PSI
With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000
```

### Probability of failure

Let's consider a pipe in district heating network with

```R
diameter           <- 762         # [mm]
wall_thickness     <-  10         # [mm]
UTS                <- 434.3697    # [MPa]
```

which transfers heat-carrier (water) at

```R
operating_pressure <-   0.588399  # [MPa]
temperature        <-  95         # [°C]
```

During *inline inspection* four corroded areas (defects) are detected with:

```R
depth  <- c(2.45,  7.86,   7.93,   8.15)  # [mm]
```

whereas the length of all defects is not greater 200 mm:

```R
length <- rep(200, 4)  # [mm]
print(length)
```

```R
[1] 200 200 200 200
```

Corrosion rates in radial and in longitudinal directions are not well-known and
may vary in range `.01` - `.30` mm/year:

```R
rar = function(n) stats::runif(n, .01, .30) / 365
ral = function(n) stats::runif(n, .01, .30) / 365
```

Then probabilities of failure (POFs) related to each corroded area are near:

```R
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv")
```

```txt
pipenostics::mepof: process case [4/4] - 100 % . All done, thanks!                 
```

```R
print(pof)
```

```R
[1] 0.000000 0.252935 0.368741 0.771299
```

So, the POF of the pipe is near

```R
print(max(pof))
```

```R
[1] 0.771299
```

The value of POF changes in time. So, in a year after *inline inspection* of
the pipe we can get something near

```R
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = 365)
```

```txt
pipenostics::mepof: process case [4/4] - 100 % . All done, thanks!             
```

```R
print(pof)
```

```R
[1] 0.000000 0.526646 0.647422 0.928825
```

For entire pipe we get something near:

```R
print(max(pof))
```

```R
[1] 0.928825
```

Two years ago before *inline inspection* the pipe state was rather good:

```R
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = -2 * 365)
```

```txt
pipenostics::mepof: process case [4/4] - 100 % . All done, thanks!
```

```R
print(pof)
```

```R
[1] 0.000000 0.040849 0.072734 0.272358
```

For entire pipe we get something near:

```R
print(max(pof))
```

```R
[1] 0.272358
```

### Regime tracing

Let's consider the next 4-segment tracing path:

![](https://raw.githubusercontent.com/omega1x/pipenostics/a26e1171f05d3cd4f2c25a71ccde9947a095409f/.src/svg-graphics/m325regtrace.svg)

Suppose we have the next sensor readings for *forward tracing*:

```R
t_fw <- 130         # [°C]
p_fw <-   0.588399  # [MPa]
g_fw <- 250         # [ton/hour]
```

Let's discharges to network for each pipeline segment are somehow determined as

```R
discharges <- seq(0, 30, 10)  # [ton/hour]
print(discharges)
```

```R
[1]  0 10 20 30
```

Then the calculated regime (red squares) for forward tracing is

```R
regime_fw <- m325traceline(t_fw, p_fw, g_fw, discharges, forward = TRUE)
print(regime_fw)
```

```R
$temperature
[1] 129.1799 128.4269 127.9628 127.3367

$pressure
[1] 0.5878607 0.5874226 0.5872143 0.5870330

$flow_rate
[1] 250 240 220 190
```

> &#8505; Read article [Concepts and useful notes](https://omega1x.github.io/pipenostics/articles/Concepts.html) for a deeper dive into the topic.
