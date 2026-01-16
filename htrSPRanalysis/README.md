# High-Throughput SPR Kinetics Analysis Software

This R package provides functions and a pipeline to analyze SPR data. It takes as input the sample information and the Carterra time series and produces a pdf with sensorgrams, fits, rate constants, Rmax, plots of residuals and standard errors of parameter estimates. It also produces a .csv file with the relevant parameters and standard errors.

Current features include:

- 1:1 binding model
- Options for local or global Rmax fitting
- Automated concentration range determination
- Automated dissociation window approximation
- Automated bulkshift determination

The motivations for developing this package are 

- to provide an open-source tool for analyzing high-throughput SPR kinetic data
- to automate some of the steps in analysis, such as determining the length of dissociation window and concentration range to include in fits and whether or not to include bulk-shift in fitting.
- to provide a platform for further development of multivalent binding kinetics models, such as bivalent analyte, parallel binding and more. These models are currently not an option in commercial software when using non-regenerative SPR data.
