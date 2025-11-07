solaR2
=====
[![CRAN](https://www.r-pkg.org/badges/version/solaR2)](https://www.r-pkg.org/pkg/solaR2)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/solaR2)](https://www.r-pkg.org/pkg/solaR2)


The `solaR2` package allows for reproducible research both for photovoltaics (PV) systems performance and solar radiation. It includes a set of classes, methods, and functions to calculate the sun geometry and the solar radiation incident on a photovoltaic generator, as well as to simulate the performance of various photovoltaic energy applications. This package performs the entire calculation procedure from both daily and intradaily global horizontal irradiation to the final productivity of grid-connected PV systems and water pumping PV systems.

It is designed using a set of S4 classes that handle multivariate time series efficiently and are optimized for high-performance data manipulation. The classes share a variety of methods to access the information and several visualization methods. Additionally, the package provides tools for the visual statistical analysis of the performance of large PV plants composed of multiple systems.

Although `solaR2` is primarily designed for time series associated with a location defined by its latitude/longitude values and temperature and irradiation conditions, it can be easily combined with spatial packages for space-time analysis.

# Software #

The stable version of solaR is hosted at
[CRAN](https://cran.r-project.org/package=solaR2). The development
version is available at
[GitHub](https://github.com/solarization/solaR2).

Install the stable version with:

    install.packages('solaR2')

# Citation #

If you use `solaR2`, please cite it in any publication reporting results obtained with this software:

    Delgado López, Francisco y Perpiñán Lamigueiro, Oscar (2024).
    solaR2: Radiation and Photovoltaic Systems with R version 2.
    R package version 0.10.
    Disponible en: https://solarization.github.io/solaR2/

A BibTeX entry for LaTeX users is

    @Manual{,
      title     = {solaR2: Radiation and Photovoltaic Systems with R version 2},
      author    = {Francisco Delgado L{\'o}pez and Oscar Perpi{\~n}{\'a}n Lamigueiro},
      year      = {2024},
      url       = {https://solarization.github.io/solaR2/},
      note      = {R package version 0.10},
    }
