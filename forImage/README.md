
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forImage

The goal of `forImage` is to facilitate foraminiferal test measurement
and biomass estimation.

The package is proposed as an alternative to manual and/or
multi-software dependent methods for biometry and biomass estimation.

## Installation

The package **requires** Python &gt;= 3.5.

You can install `forImage` from github with:

``` r
remotes::install_github("ThaiseRF/forImage", build_vignettes = TRUE)
# Using `build_vignettes = TRUE` will slow down the install, but is necessary if 
# you want to read the vignette, which is recommended
```

Use `install_measure()` to install all Python modules dependencies
(`scipy`, `imutils`, `numpy`, `pandas`, `Pillow` and `OpenCV`) at once
in a virtual environment. This function is a wrapper of [reticulate
py\_install()](https://rstudio.github.io/reticulate/reference/py_install.html)
and requires a conda installation on Windows.

To install the Python modules via pip on Ubuntu:

``` bash
sudo apt-get install python-pip
sudo pip install pip --upgrade
sudo pip install scipy
```

On Windows:

``` bash
python -m pip install --upgrade pip
pip install scipy
```

On macOs:

``` bash
python3 -m pip install --upgrade pip
pip3 install scipy
```
