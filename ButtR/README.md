# 🦋 ButtR 🦋

[![R-CMD-check](https://github.com/DiogoJackson/ButtR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DiogoJackson/ButtR/actions/workflows/R-CMD-check.yaml)

## Overview

`ButtR` is an R package designed to simplify the download and extraction
of the **OzButterflies Database**, a large dataset containing
comprehensive records of reflectance spectra, calibrated photographs and CO1 sequences
of Australian butterflies.

The **OzButterflies database** can be downloaded manually from [Dryad](https://blog.datadryad.org), however it is simpler to
use `ButtR` to do so. The database is quite large, so if the entire
database is not required, `ButtR` provides an efficient mechanism to
download and install only the desired parts of the database. It is also much simpler to install
the entire database using `ButtR` than to download and extract all of the zip files.

If you use the OzButterflies database, please cite the paper: 

📌 **[Citation to be added]**

## Why Use `ButtR`?

✅ **Easy Download** – Quickly access butterfly data with a single
command .\
✅ **Selective Filtering** – Download only specific species, families, or
locations, saving disk space and processing time.

## 🛠 Functions

The `ButtR` package provides the following core functions:

| Function | Description |
|---------------------------------|---------------------------------------|
| `get_Oz_butterflies()` | Download and install the database, optionally filtered by species, family, or location |

## 📥 Installation

You can install the stable version from CRAN:

``` r
#install the package 
install.packages("ButtR")

#load the package 
library("ButtR") 
```

Or install the latest development version from GitHub. In this case, you
need to have the `devtools` package installed:

``` r
# Install devtools if not already installed 
install.packages("devtools") 
library("devtools")

# Install ButtR from GitHub 
devtools::install_github("DiogoJackson/ButtR") 
library("ButtR")
```

## 🚀 Examples of use

After installation and activation, you can use the
*get_Oz_butterflies()* function from `ButtR` to download the entire
database or filter specific subsets.

Download the entire database:

``` r
# Download the full OzButterflies Database
get_Oz_butterflies()
```

Download data for a specific species:

``` r
# Get data only for Delias aganippe
get_Oz_butterflies(species = "Delias aganippe")
```

Download data for a specific genus:

``` r
# Get data for all species of the genus Delias
get_Oz_butterflies(genus = "Delias")
```

Download data for a specific family:

``` r
# Get all species within the Nymphalidae family
get_Oz_butterflies(family = "Nymphalidae")
```

Download data by site:

``` r
# Get all butterfly species from Cairns Botanic Gardens ("BG")
get_Oz_butterflies(site = "BG")
```

Download data for male *Delias aganippe* and *Delias mysis* from all sites with standardised collections in Brisbane:

``` r
# Get data with multiple filters 
get_Oz_butterflies(sex = "male", 
    species = c("Delias aganippe", "Delias mysis"), 
    site = c("BBG", "CC", "OC", "LSP"))
```

------------------------------------------------------------------------

## 📑 Summary of database content

### Folder structure
The OzButterflies Database has five folders for butterfly families (Papilionidae, Nymphalidae, Lycaenidae, Hesperiidae, Pieridae). Each family folder contains subfolders for each butterfly species. Each species subfolder contains subfolders for each butterfly specimen. Each specimen subfolder contains data and image files for that specimen, as shown in the schema below:

-   📁 Pieridae
    -   📁 Eurema_hecabe
        -   📁 1
            -   📄 1-v-RGB.dng (Ventral wing surfaces photographed in
                visible light)
            -   📄 1-d-RGB.dng (Dorsal wing surfaces photographed in
                visible light)
            -   📄 1-v-UV.dng (Ventral wing surfaces photographed in
                ultraviolet light)
            -   📄 1-d-UV.dng (Dorsal wing surfaces photographed in
                ultraviolet light)
            -   📄 1-RGB.dng (Dorsal and ventral wing surfaces
                photographed in visible light - non-pinned)
            -   📄 1-UV.dng (Dorsal and ventral wing surfaces
                photographed in ultraviolet light - non-pinned)
            -   📄 1-f.ab1 (CO1 gene sequenced forward - DNA)
            -   📄 1-d.jpeg (Reference image for speced patch
                numbers - dorsal)
            -   📄 1-v.jpeg (Reference image for speced patch
                numbers - ventral)
            -   📄 1-d-1-s.procspec (Spectra measured at 45 degrees -
                dorsal, patch 1)
            -   📄 1-d-1-a.procspec (Spectra measured at 60 degrees -
                dorsal, patch 1)
            -   📄 1-v-2-s.procspec (Spectra measured at 45 degrees -
                ventral, patch 2)
            -   📄 1.csv (CSV file containing all procspec measures)
            -   📄 1.png (Graph of the spectra measured at 45
                degrees)
        -   📁 2
        -   📁 3
    -   📁 Lampides_boeticus
        -   📁 4
        -   📁 5

## 💡 Citation

If you use this database in a publication, please cite it as follows:

📌 **[Citation to be added]**

------------------------------------------------------------------------

## 🤝 Helping us to improve

We welcome contributions! If you’d like to improve `ButtR`, feel free
to open an **[issue](https://github.com/DiogoJackson/ButtR/issues)** for bug reports or feature requests.

------------------------------------------------------------------------
