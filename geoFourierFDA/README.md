geoFourierFDA
=============

In this paper, we have observsed *n* curves
*χ*<sub>**s**<sub>1</sub></sub>(*t*), …, *χ*<sub>**s**<sub>*n*</sub></sub>(*t*)
in a region, where
**s**<sub>*i*</sub> = (*θ*<sub>*i*</sub>, *η*<sub>*i*</sub>), *i* = 1, …, *n*,
and *θ*<sub>*i*</sub> is the latitude and *η*<sub>*i*</sub> is the
longitude where the curve *χ*<sub>**s**<sub>*i*</sub></sub> was sampled.
The goal of this package is to estimate an unsampled curve
*χ*<sub>**s**<sub>0</sub></sub>(*t*) at
**s**<sub>0</sub> ∉ {**s**<sub>1</sub>, …, **s**<sub>*n*</sub>}. The
Ideia proposed by [Giraldo
(2011)](https://doi.org/10.1007/s10651-010-0143-y) was simple: the curve
*χ*<sub>**s**<sub>0</sub></sub>(*t*) is a linear combination of all
curves
*χ*<sub>**s**<sub>1</sub></sub>(*t*), …, *χ*<sub>**s**<sub>*n*</sub></sub>(*t*),
i.e.,
$\\widehat{\\chi\_{\\mathbf{s}\_0}}(t) = \\lambda\_1 \\chi\_{\\mathbf{s}\_1}(t) + \\lambda\_2 \\chi\_{\\mathbf{s}\_2}(t) + \\dots + \\lambda\_n \\chi\_{\\mathbf{s}\_1}(t)$
where *λ*<sub>1</sub>, …, *λ*<sub>*n*</sub> is solution of the linear
system given by

<img src="man/figures/README-linear-system.png" width="100%" />

where *μ* is an constant from the method of Lagrange’s multipliers and
the function *γ*(*h*) = ∫*γ*(*h*; *t*)*d**t* is called the
trace-variogram, where, for each *t*, *γ*(*h*; *t*) is the semivariogram
for the process
*χ*<sub>**s**<sub>1</sub></sub>(*t*), …, *χ*<sub>**s**<sub>*n*</sub></sub>(*t*).
More precisely, for each *t*, a weakly and isotropic spatial process is
assumed for
*χ*<sub>**s**<sub>1</sub></sub>(*t*), …, *χ*<sub>**s**<sub>*n*</sub></sub>(*t*)
and the integration of the semivariogram is carried out. Usually, the
integration in the equation (1) is approximated using a modified version
of the empirical semivariogram. In this pcackage, we have used the
Legendre-Gauss quadrature, which is simple and it explicitly used the
definition of the semivariogram.

Installation
------------

This package can be installed using the `devtools` package.

    devtools::install_github("gilberto-sassi/geoFourierFDA")

Examples
--------

In this package, we have used the temperature dataset present in the
package [`fda`](https://CRAN.R-project.org/package=fda) and in the
package [`geofd`](https://CRAN.R-project.org/package=geofd). This
dataset has temperature measurements from 35 weather stations from
Canada. This data can be downloaded at
[weather.gov.ca](https://weather.gc.ca). For illustration, we have
separated the time series at *The Pas* station and used all others
stations to estimate the curve temperature at *The Pas*.

### How to interpolate a curve at an unmonitored location

    # interpolating curve at Halifax using all remaining curves in the functional dataset
    data(canada)

    # Estimating the temperature at The Pas
    geo_fda(canada$m_data, canada$m_coord, canada$ThePas_coord)

### Coefficients of smoothing using Fourier series polynomial

    # Coefficients of smoothing using Fourier series polynomial
    # Coefficients of smoothing at The Pas
    data(canada)

    coefs <- coef_fourier(canada$ThePas_ts)

### Smoothed curve using Fourier series

    # Coefficients of smoothing using Fourier series polynomial
    # Coefficients of smoothing at The Pas
    data(canada)


    # coefficients of Fourier series polynomial
    coefs <- coef_fourier(canada$ThePas_ts, m)

    # points to evaluate curve at interval [-pi, pi]
    x <- seq(from = -pi, to = pi, by = 0.01)

    # smoothed curve at some points x
    y_est <- fourier_b(coefs, x)

References
----------

Giraldo, R, P Delicado, and J Mateu. 2011. “Ordinary Kriging for
Function-Valued Spatial Data.” *Environmental and Ecological Statistics*
18 (3): 411–26.
