[![Generic badge](https://cranlogs.r-pkg.org/badges/grand-total/SPARSEMODr)](https://cran.r-project.org/package=SPARSEMODr)


SPARSE-MOD: Overview 
-------------------------------------

SPARSE-MOD stands for **SPA**tial **R**esolution-**SE**nsitive **M**odels of **O**utbreak **D**ynamics. Our goal with this R package is to offer a framework for simulating the dynamics of stochastic and spatially-explicit models of infectious disease. As we develop the package, our goal is to add more model structures and more user-control of the model dynamics. Our SPARSEMODr package offers several key features that should make it particularly relevant for pedogogical and practical use. See [our COVID-19 model vignette](https://github.com/NAU-CCL/SPARSEMODr/blob/main/vignettes/covid-19-model.Rmd) and [our SEIR model vignette](https://github.com/NAU-CCL/SPARSEMODr/blob/main/vignettes/seir-model.Rmd)for detailed walk-throughs of how to run the model(s), to plot the output, and to simulate customized time-windows.

-  **Spatially explicit models** that allow user-defined meta-population characteristics and a customizable dispersal kernel.

-   **Customizable process time-windows**: The user controls how model parameters can vary over time, such as the transmission rate, or parameters that define the host migration processes. We have created `time_window objects` that allow users to simulate, for example, time periods over which public health or conservation interventions are implemented that can affect the contact rates between hosts or the movement of hosts among populations.

-   **Demographic stochasticity** is built-in using a tau-leaping
    algorithm. This captures the random transmission processes that
    are important early in outbreaks and especially in small host
    populations.

-   **Stochastic transmission** is also built-in, allowing daily
    fluctuations in the transmission rate, which can help account for
    dynamics like super-spreading or super-shedding.

-   The transmission process can be simulated as **frequency-dependent**
    (i.e., contact rates are invariable to population density) or
    **density-dependent** (i.e., contact rates depend on population
    density). For density-dependent transmission, we allow the user to
    custom-define a (non-)linear relationship between local host density
    and the transmission rate (see below).

-   Models are **coded in C++** and take advantage of Rcpp for rapid
    simulation of stochastic model trajectories across many focal
    populations.


