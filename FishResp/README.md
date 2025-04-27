# FishResp: R package for Aquatic Respirometry

## Description
The R package 'FishResp' is designed to calculate metabolic rate of aquatic organisms measured using an intermittent-flow respirometry approach. Raw respirometry data can be imported from:

* PumpResp (open-source pump controller by _FishResp_)
* SensResp (open-source DO meter by _FishResp_).
* AutoResp (by _LoligoSystems_)
* Pyro Oxygen Logger (by _PyroScience_)
* OxyView (by _PreSens_)
* AquaResp (free software)
* Q-box Aqua (by _QubitSystems_)

The idea behind FishResp, an analysis pipeline and case studies are described in the [academic publication](https://doi.org/10.1093/conphys/coz003) by Morozov et al. (2019). Technical information about R functions and demo data can be found in [Reference Manual](https://cran.r-project.org/package=FishResp). Brief descriptions of the R package 'FishResp' and other open-source tools for aquatic respirometry are located at the website [fishresp.org](https://fishresp.org).

## Installation
The stable version of the ‘FishResp’ package can be installed from CRAN:<br/>`install.packages("FishResp")`. 

Alternativelly, download the latest release from GitHub and install using the following command:<br/>
`devtools::install_github("embedded-sergey/FishResp-Rpackage@*release")`. 

If you are not familiar with the R language, please check out the graphical implementation of the R package: [FishRespGUI](https://fishresp.org/gui-application/).

## Citation
To cite FishResp please refer to: "Morozov, S., McCairns, R.J.S., Merila, J. (2019) FishResp: R package and GUI application for analysis of aquatic respirometry data. Conserv Physiol 7(1): coz003; <https://doi.org/10.1093/conphys/coz003>".

## Changelog
To see the list of changes made in various versions of the R package, read the file ['NEWS.md'](https://github.com/embedded-sergey/FishResp-Rpackage/blob/main/NEWS.md).

## Bug Reporting
If you find a bug, please report about it on the [FishResp forum](https://fishresp.org/forum/) or through the [GitHub repository](https://github.com/embedded-sergey/FishResp-Rpackage).

## Acknowledgements
The individuals and organizations who developed, contributed to, or funded the FishResp project are acknowledged [here](https://fishresp.org/people/).
