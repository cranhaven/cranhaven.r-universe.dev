# vortexRdata

`vortexRdata` is the auxiliary data package for [vortexR](https://github.com/carlopacioni/vortexR).

`vortexRdata` provides data from two publications, Campbell et al (2016) and 
Pacioni et al. (2017) both as original output from the population viability 
analysis software [Vortex](http://vortex10.org/) and as R objects.

The R package `vortexR` uses the raw data provided here to illustrate its
functionality of parsing raw Vortex output into R objects.

## Install

The stable versions of `vortexRdata` and `vortexR` can be installed with:

```R
install.packages("vortexR", dependencies = TRUE)
```

The latest development versions of `vortexRdata` and `vortexR` can be installed with:

```R
# install.packages("devtools")
devtools::install_github("carlopacioni/vortexRdata")
devtools::install_github("carlopacioni/vortexR")
```

See the [vortexR README](https://github.com/carlopacioni/vortexR/blob/master/README.md)
for detailed help with the installation of Java-based packages.

## Use

### Compiled data

You can use the data provided by vortexRdata as binary R objects as follows:

```R
require(vortexRdata)
data("pac.clas")
head(pac.clas)
```
The full list of provided compiled data can be found in the package's 
documentation: `help(package = "vortexRdata")`.

### Raw data

If you wish to test `vortexR`'s data handling functions on actual Vortex output files,
you can use the raw data here as a starting point that is guaranteed to work.

```R
pac.dir <- system.file("extdata", "pacioni", package="vortexRdata")
cam.dir <- system.file("extdata", "campbell", package="vortexRdata")
f <- file.path(pac.dir, "Pacioni_et_al_ST_Classic(Base).stdat")
require(vortexR)
one.st.classic <- collate_one_dat(f, 3)
head(one.st.classic)
```

The two folders of raw data are named "campbell" and "pacioni", respectively.

Using example data allows users of `vortexR` to trouble-shoot and validate their
workflow before using their own Vortex output.

## Cite

If you use data provided by `vortexRdata`, please use the citation generated from
`citation("vortexRdata")`.

## Contribute

We are happy to receive feedback and contributions through bug reports and pull 
requests through the main package [`vortexR`](https://github.com/carlopacioni/vortexR/issues).

## Future development

We are in the process of releasing `vortexR` and `vortexRdata` to CRAN.
In addition, a paper describing `vortexR` is in press.
Once both packages are available through CRAN, installation, citation, and usage 
of `vortexR` and `vortexRdata` are likely to change.
