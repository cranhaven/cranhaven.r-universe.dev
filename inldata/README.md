# inldata <img src="man/figures/logo.svg" alt="inldata" align="right" width="152px" />

[![cran-version-image](https://www.r-pkg.org/badges/version/inldata)](https://CRAN.R-project.org/package=inldata)
[![pipeline-status-image](https://code.usgs.gov/inl/inldata/badges/main/pipeline.svg)](https://code.usgs.gov/inl/inldata/-/commits/main)
[![coverage-status-image](https://code.usgs.gov/inl/inldata/badges/main/coverage.svg)](https://code.usgs.gov/inl/inldata/-/commits/main)

## Description

The **inldata** package is a practical resource that consolidates
datasets for the groundwater and surface-water monitoring networks of
the U.S. Geological Survey - Idaho National Laboratory (USGS-INL).
Managed by the [USGS-INL Project
Office](https://www.usgs.gov/centers/idaho-water-science-center/science/idaho-national-laboratory-project-office)
in collaboration with the U.S. Department of Energy, it provides data
from the Idaho National Laboratory and surrounding areas. This data is
crucial in understanding the effects of waste disposal on the eastern
Snake River Plain aquifer in southeastern Idaho and assessing the
long-term availability of water.

The package compiles historical water-quality samples from 1949 to 2024,
groundwater-level measurements from 1922 to 2024, and surface-water
measurements from 1959 to 2024. Geospatial data, which outlines the
areas of sample collection or observation, is also part of the package.

By integrating this data into a single [R](https://www.r-project.org/)
package, the process of data handling is simplified for researchers. It
also offers a standard format for data distribution along with its
documentation. The geospatial datasets are provided in a common
projection and datum, and the geohydrologic data is structured to
facilitate analysis. Overall, the **inldata** package serves as a
convenient tool for those studying the impact of waste disposal on water
quality and availability.

## Installation

To install the current release of the package from
[CRAN](https://CRAN.R-project.org/package=inldata), you can use the
following command in R:

``` r
install.packages("inldata")
```

To install the package along with its dependencies, which are required
to run examples in the package help documentation, build package
datasets, and create a USGS data release, run:

``` r
install.packages("inldata", dependencies = TRUE)
```

To install the development version of the package, you need to clone the
repository and build from source, or run the following commands:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_gitlab("inl/inldata@develop",
  auth_token = Sys.getenv("GITLAB_PAT"),
  host = "code.usgs.gov",
  dependencies = TRUE,
  build_vignettes = TRUE
)
```

## Usage

Detailed descriptions of the datasets, along with practical examples
illustrating their application, are readily accessible in the help pages
of the package. To access this documentation, simply run the following
command:

``` r
library("inldata")
help(package = "inldata")
```

## Authors

- Jason C. Fisher (ORCID iD
  [0000-0001-9032-8912](https://orcid.org/0000-0001-9032-8912))
- Allison R. Trcka (ORCID iD
  [0000-0001-8498-4737](https://orcid.org/0000-0001-8498-4737))
- Kerri C. Treinen (ORCID iD
  [0000-0003-0645-6810](https://orcid.org/0000-0003-0645-6810))

## Point of Contact

Jason C. Fisher (<jfisher@usgs.gov>)

## Suggested Citations

To cite **inldata** in publications, please use:

Fisher, J.C., Trcka, A.R., Treinen, K.C., 2024, inldata—Collection of
datasets for the U.S. Geological Survey - Idaho National Laboratory
groundwater and surface-water monitoring networks, v1.1: U.S. Geological
Survey software release, R package, <https://doi.org/10.5066/P9IAKQOR>.

Fisher, J.C., Trcka, A.R., Treinen, K.C., 2024, Datasets for the U.S.
Geological Survey - Idaho National Laboratory groundwater and
surface-water monitoring networks, v1.1: U.S. Geological Survey data
release, <https://doi.org/10.5066/P9UWRYR4>.

## Contributing

We value your contributions and suggestions on how to make these
materials more useful to the community. Please feel free to share your
thoughts by commenting on the [issue
tracker](https://code.usgs.gov/inl/inldata/-/issues) or opening a [merge
request](https://code.usgs.gov/inl/inldata/-/merge_requests) to
contribute.

## Code of Conduct

All contributions to- and interactions surrounding- this project will
abide by the [USGS Code of Scientific
Conduct](https://www.usgs.gov/office-of-science-quality-and-integrity/fundamental-science-practices).

<!-- Embedded References -->

## Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.

Any use of trade, product, or firm names is for descriptive purposes
only and does not imply endorsement by the U.S. Government.

## License

Unless otherwise noted, this project is in the public domain in the
United States because it contains materials that originally came from
the United States Geological Survey, an agency of the United States
Department of Interior. For more information, see the official USGS
copyright policy at
[copyrights-and-credits](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits).

Additionally, we waive copyright and related rights in the work
worldwide through the CC0 1.0 Universal public domain dedication.

#### CC0 1.0 Universal Summary

This is a human-readable summary of the [Legal Code (read the full
text)](https://creativecommons.org/publicdomain/zero/1.0/legalcode).

##### No Copyright

The person who associated a work with this deed has dedicated the work
to the public domain by waiving all of his or her rights to the work
worldwide under copyright law, including all related and neighboring
rights, to the extent allowed by law.

You can copy, modify, distribute and perform the work, even for
commercial purposes, all without asking permission.

##### Other Information

In no way are the patent or trademark rights of any person affected by
CC0, nor are the rights that other persons may have in the work or in
how the work is used, such as publicity or privacy rights.

Unless expressly stated otherwise, the person who associated a work with
this deed makes no warranties about the work, and disclaims liability
for all uses of the work, to the fullest extent permitted by applicable
law. When using or citing the work, you should not imply endorsement by
the author or the affirmer.

<!-- Embedded References -->

## Support

The Idaho National Laboratory Project Office of the USGS supports the
development and maintenance of **inldata**. Resources are available
primarily for maintenance and responding to user questions, and the
development team determines the priorities for the development of new
features.

## Additional Publication Details

This table contains additional metadata about this publication that is
not found in other parts of the page.

<!--html_preserve-->
<table>
<tbody>
<tr>
<th scope="row">
Publication type
</th>
<td>
Formal R language package
</td>
</tr>
<tr>
<th scope="row">
DOI
</th>
<td>
10.5066/P9IAKQOR
</td>
</tr>
<tr>
<th scope="row">
Year published
</th>
<td>
2024
</td>
</tr>
<tr>
<th scope="row">
Version
</th>
<td>
<a href='https://code.usgs.gov/inl/inldata/-/tree/v1.1.1'>1.1.1</a>
</td>
</tr>
<tr>
<th scope="row">
IPDS
</th>
<td>
IP-159621
</td>
</tr>
</tbody>
</table>

<cr><!--/html_preserve-->

<!-- Embedded References -->
