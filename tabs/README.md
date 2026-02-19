
# Temporal Altitudinal Biogeographic Shifts <a href="https://gitlab.com/uva_ibed_piac/tabs/-/blob/master/man/figures/logo.png"><img src="man/figures/logo.png" align="right" height="132" alt="tabs website" /></a>

The aim of [tabs](https://uva_ibed_piac.gitlab.io/tabs/) (Temporal Altitudinal Biogeographic Shifts) is to provide a standardized workflow to reconstruct spatial configurations of altitude-bounded biogeographic systems over time. For example, 'tabs' can model how island archipelagos expand or contract with changing sea levels or how alpine biomes shift in response to tree line movements. It provides functionality to account for various geophysical processes such as crustal deformation and other tectonic changes, allowing for a more accurate representation of biogeographic system dynamics. Additionally, tabs can project future configurations under climate change scenarios. For more information see De Groeve et al. ([2025](https://preprints.arphahub.com/article/151900/)).

![**Figure 1.** Example reconstruction](./man/figures/example.gif)

## Datasets 

TABS embeds state-of-the-art datasets including a topo, labeling dataset and a spatial-explicit sea level curve. In addition various other widely used curves are integrated in the R-packages as default datasets. **Topo** is a tiled version of the General Bathymetric Chart of the Oceans ([GEBCO](https://www.gebco.net/data-products/gridded-bathymetry-data)). **Labs** includes two Geopackages, one including the Global Shoreline Vector ([Sayre et al. 2018](https://doi.org/10.1080/1755876X.2018.1529714)), bioclimatic and physical characterization of the worlds islands ([Weigelt et al. 2013](https://doi.org/10.1073/pnas.1306309110)), tectonic plates and orogens ([Bird et al. 2003](https://doi.org/10.1029/2001GC000252)) and an open source reference labeling points dataset for many types of features ([geoNames](https://download.geonames.org/export/dump/)). The second geopackage includes the GMBA Mountain Inventory v2 ([Snethlage et al. 2022](https://doi.org/10.1038/s41597-022-01256-y)). **Curve** includes a Global spatial-explixit past sea level curve (st_curve, 0-26 kyBP / 0.5 ky, [De Groeve et al. 2022](https://doi.org/10.1111/geb.13573)), two global mean past sea level curves (Lambeck , 0-35 kyBP / 1 ky, [Lambeck et al. 2014](https://doi.org/10.1073/pnas.1411762111); Cutler, 0-140 kyBP / 1 ky, [Cutler et al. 2003](https://doi.org/10.1016/S0012-821X(02)01107-X)), one N-America mean past sea level curve (Bintanja, 0-3000 kyBP / 1 ky, [Bintanja & van de Wal 2008](https://doi.org/10.1038/nature07158)) and four global mean future sea level IPCC scenarios (ssp1, ssp2, ssp3, ssp5, [IPCC Scenarios](https://interactive-atlas.ipcc.ch/regional-information)). 

The three main processed datasets (topo, labs, st_curve) are published on Figshare ([Table 1](#TABLE1)) and need to be downloaded which can be conveniently done with the [setup](#download-and-installation-global-datasets) function. 

**Table 1.** TABS processed default datasets <a id="TABLE1"></a>

| Type      |    Dataset               |  Description                          |   
|:----------|:-------------------------|:--------------------------------------|
| topo      | [https://doi.org/10.21942/uva.23943957](https://doi.org/10.21942/uva.23943957) | Tiled version of General Bathymetric Chart of the Oceans |
| labs | [https://doi.org/10.21942/uva.22788464](https://doi.org/10.21942/uva.22788464) | GPKGs with GMBA, adjusted GSV and geoNames reference labeling polygons and points |
| curve     | [https://doi.org/10.21942/uva.20029991](https://doi.org/10.21942/uva.20029991) | Global spatial-explicit past sea level curve 0-26 kyBP / 0.5 ky | 

## Installation

Once available on CRAN it will also be possible to install TABS directly with `install.packages('tabs')`

```r
install.packages('tabs') 
```

You can also install the development version of `tabs` like so:

Download [ZIP](https://gitlab.com/uva_ibed_piac/tabs/-/archive/master/tabs-master.zip)
```r
# install.packages('devtools') 
devtools::install_local('~/Downloads/tabs-master.zip') # specify the correct path 
```

## Usage 

In this section basic examples are provided of the functionality of TABS using built-in datasets of the Sporades archipelago. Once you have evaluated TABS basic functionality we strongly recommend to install the default global datasets used by the package with the [setup](#download-and-installation-global-datasets) function below.

### Packages 

load tabs and terra. The latter is the main additional package to be used besides tabs. 

```r
library(tabs)
library(terra)
```

### Sample datasets 

With the built-in sample data of the Sporades archipelago it is possible to test the functionality of TABS. Sporades includes topographic model, a labeling dataset, a spatio-temporal sea level curve and a correction grid cropped to the extent of the archipelago. Various past and future temporal sea level curves are built-in including Cutler, Lambeck and Bintanja for the past and IPCC_global_mean (ssp1,ssp2,ssp3,ssp4) for the future, as well as a sample forest tree line curve.

```r
# Sporades (Greece) 
sporades <- sporades() 

# vector-layer

## labs 
labs <- sporades$labs

# raster layers 

## topo
topo <- sporades$topo

## correction grid
correction <- sporades$correction

## spatial-explicit sea level curve
curve <- sporades$curve

## plot datasets 

par(mfrow=c(3,2))
terra::plot(topo,main='topo',mar=c(2,3,2,3.5),box=F)
terra::plot(labs,add=T)
terra::plot(correction,main='correction',mar=c(2,3,2,3.5),box=F)
terra::plot(labs,add=T)
lapply(seq(1,terra::nlyr(curve),4), function(x) 
   {
   terra::plot(curve[[x]],main=names(curve)[x],mar=c(2,3,2,3.5),box=F)
   terra::plot(labs,add=T)
   })

dev.off()
terra::plot(curve[[14]]) 
terra::plot(labs,add=T)

period <- '-20000'
terra::plot(curve[[period]], main=period) 
terra::plot(labs,add=T)

# temporal relative sea level curves 

## in-build data sets:
lambeck
cutler
bintanja
IPCC_global_mean
data(lambeck)
data(cutler)
data(bintanja)
data(IPCC_global_mean)

# past sea level curves 
par(mfrow=c(3,1))
plot(x=as.numeric(names(lambeck)),y=lambeck,
     type='l',main='Lambeck [35000-0BP]',lwd=1.5)
plot(x=as.numeric(names(cutler)),y=cutler,
     type='l',main='Cutler [140000-0BP]',lwd=1.5)
plot(x=as.numeric(names(bintanja)),y=bintanja,
     type='l',main='Bintanja [3000000-0BP]',lwd=1.5)

# IPCC global mean curves 
par(mfrow=c(4,1))
plot(x=as.numeric(names(IPCC_global_mean$ssp1)),
     y=IPCC_global_mean$ssp1,type='l',
     main='IPCC SSP1',lwd=1.5,ylim=c(0,0.7))
plot(x=as.numeric(names(IPCC_global_mean$ssp2)),
     y=IPCC_global_mean$ssp2,type='l',
     main='IPCC SSP2',lwd=1.5,ylim=c(0,0.7))
plot(x=as.numeric(names(IPCC_global_mean$ssp3)),
     y=IPCC_global_mean$ssp3,type='l',
     main='IPCC SSP3',lwd=1.5,ylim=c(0,0.7))
plot(x=as.numeric(names(IPCC_global_mean$ssp5)),
     y=IPCC_global_mean$ssp5,type='l',
     main='IPCC SSP5',lwd=1.5,ylim=c(0,0.7))
     
# Upper forest line curve (Funza)
funza
data(funza)

```

### Basic functionality

The main function of tabs is `r reconstruct()` which reconstructs the changes in area and shape configurations over time based on (1) a selected region, (2) a sea-level curve, (3) a topographic/bathymetric model and (4) correction grid. Other important functions are `r export()` and  `r explore()` to respectively save and visualize outputs of reconstruct and `r import()` to import the reconstruction back in R. See below a basic example of the usage of the functions using the in-built dataset of the Sporades archipelago. For detailed examples, check the [vignettes](https://uva_ibed_piac.gitlab.io/tabs/articles/00-tabs-get-started.html) and the function help, ?reconstruct(). 


```r
# load data samples
data <- sporades() 
topo <- data$topo
labs <- data$labs
curve <- data$curve
correction <- data$correction

# reconstruct
# to use region as reference layer, specify the labeling column using the names argument
sporades <- reconstruct(topo=topo,
                        region=labs,
                        curve=curve,
                        correction=correction,
                        reclabs='name')
# export
dir <- tempdir()
export(sporades, paste0(dir,'/sporades')) # directory tree
export(sporades, paste0(dir,'/sporades.zip')) # zipped directory tree
export(sporades, paste0(dir,'/sporades.qs2')) # rds native R format
export(sporades, paste0(dir,'/sporades.rds')) # qs2 native R format

# reconstruct and export by defining filename
sporades <- reconstruct(topo=topo,
                        region=labs,
                        curve=curve,
                        correction=correction,
                        filename=paste0(dir,'/sporades.qs2')
                        )

# explore
explore(sporades) # present
explore(sporades,timelapse=0) # timelapse
explore('sporades.qs2')

# import
sporades <- import(paste0(dir,'/sporades'))
sporades <- import(paste0(dir,'/sporades.qs2'))
sporades <- import(paste0(dir,'/sporades.rds'))
```

## Download and installation global datasets 

Before using TABS, it is recommended to download and store the underlying default global TABS datasets. This requires 15 GB of storage and a stable internet connection. The download can take 10 minutes up to 3 hours depending on your internet connection. There are two approaches to installing the underlying datasets (labs, topo, curve): automatically or manually.

### Automatic [recommended]

To automatically download the datasets you can use the `r setup()` function:

```r
setup() # it will request to install in a default location, or you can specify a root-directory

# check default directory where datasets are installed
options()$tabs.datasetDefaultPath
options()$tabs.datasetPath
```

### Manual

To manually download the datasets, use the following **three** steps: 

1. Download datasets (see [Table 1](#TABLE1) for download links) 

2. Extract ZIP-files and store using the following structure in a directory of preference `<PATH/TO/DATASETS>/TABS`. Make sure to store the datasets into a root-directory named `TABS`.

```
TABS
├── topo
├── curve
│   ├── RSL_mosaic
│   └── RSL_tiles
├── labs
│   ├── mountains.gpkg
│   └── reference.gpkg
```

* <PATH/TO/DATASETS>/TABS/topo
* <PATH/TO/DATASETS>/TABS/curve
* <PATH/TO/DATASETS>/TABS/labs

3. Run `r setup()` function and define the root directory `<PATH/TO/DATASETS>` where datasets are stored. 

Once you have run the setup function, the path of the data sources will be added to the package options files and data will be available for usage. 

## TABS 2.0. 

We are continuously working on improving TABS and welcome all contributions to improve our package. Check out [issue 19](https://gitlab.com/uva_ibed_piac/tabs/-/issues/19) for ideas on future developments and let us know by creating an **issue** if you have any suggestions for future improvements! Please make sure to select the right label for each reported issue (e.g. bug, documentation, duplicate, enhancement). 


