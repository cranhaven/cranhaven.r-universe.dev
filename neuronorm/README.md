
# NeuroNorm <img src="img/neuro_sticker.png" align="right" width="160" /> 

[![R-CMD-check](https://github.com/DavidPayares/neuronorm/workflows/R-CMD-check/badge.svg)](https://github.com/DavidPayares/neuronorm/actions)
[![Build Status](https://travis-ci.com/DavidPayares/neuronorm.svg?branch=main)](https://app.travis-ci.com/github/DavidPayares/neuronorm)
[![](https://www.r-pkg.org/badges/version/neuronorm?color=blue)](https://cran.r-project.org/package=neuronorm)


NeuroNorm is an R package to preprocess structural magnetic resonance imaging (MRI) from multiple patients, diseases, scanners, and sites. NeuroNorm transforms multiple raw T1-w, T2-w, and FLAIR images in the NIfTI format into preprocessed images comparable across patients, sites, and diseases. Neuronorm performs inhomogeneity correction, spatial registration to a template, skull stripping, spatially informed MRI scan (brain segmentation) generation, intensity normalization, and intensity adjustment. NeuroNorm comes up as a standard procedure to compare and analyze multiple MRI scans of different brain disorders.

This package extends the master thesis **Detection and Classification of Neurodegenerative Diseases: A Spatially Informed Bayesian neural Network**, which conducts a population-level analysis of MRI scans of patients with neurodegenerative diseases.

## Background

After acquiring an MRI scan, due to the nature of its data, it needs to be processed before any statistical analysis, especially if the study involves multiple sources, multiple scans, and multiple subjects. The collection of transformations from the data is called imaging preprocessing. There are numerous steps in imaging preprocessing used usually to reduce noise, adjust and standardize the data. The steps' order and relevance depend on the study aim and the neurologist criteria. 

The `NeuroNorm` package presents a preprocessing pipeline to transform raw images into images ready for statistical analysis. First, the `NeuroNorm` package performs inhomogeneity correction using the N4 correction. Then it applies a non-linear registration to the MNI152 template using a diffeomorphism algorithm. It also only extracts the brain tissue using a brain mask derivated from the MNI atlas. The brain extraction is followed by a brain segmentation using Hidden Markov Random Fields (HMRF). The segmented image is considered a spatially informed scan given the HMRF model properties. A control voxel mask image is obtained for applying the RAVEL intensity normalization. Finally, the intensities are normalized by using the RAVEL algorithm.

The methods and algorithms selected of `NeuroNorm` are state-of-the-art methods in the brain imaging literature of neurodegeneration. `NeuroNorm` proposes a straightforward preprocessing pipeline for integrating images from numerous neurodegenerative processes.

<p align="center">

<img src="img/flow.png" width="1001"/>

</p>



## Installation

`Neuronorm` is now available on [CRAN!](https://CRAN.R-project.org/package=neuronorm).
You can install it directly from R.

``` r
install.packages("neuronorm")
```

Or you can install it from GitHub using `devtools`.

``` r
# install.packages("devtools")
devtools::install_github("DavidPayares/neuronorm@main")
```

`NeuroNorm` relies on many neuroimaging packages: `fslr`, `ANTsr`, `ITKR`, `extrantsr`, `MNITemplate` and `RAVEL`.
The package `fslr` is available on CRAN, and requires FSL to be installed on
your machine; see the [FSL website](http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/) for installation. 
For `ANTsR`, `ITKR`, `extrantsr` and `RAVEL`, it is recommended to install the latest stable versionS available at the [ANTsR](https://github.com/ANTsX/ANTsR/releases), [ITKR](https://github.com/stnava/ITKR), [extrantsr](https://github.com/muschellij2/extrantsr/releases/) and [RAVEL](https://github.com/Jfortin1/RAVEL) GitHub pages, respectively. 
For the template space, the MNI152 atlas with an isomorphic voxel size of 1mm is used. It is included in the `MNITemplate` package, available on GitHub at <https://github.com/Jfortin1/MNITemplate>. 

You can also install all the stable and compatible versions of the packages directly from [this](https://github.com/DavidPayares/drat) repository (Recommended).

***`NeuroNorm` is only available for Linux OS***

## Usage

### Data extructure

For using `NeuroNorm`, data must follow a specific structure. This makes the loading of input MRI scans more effortless and intuitive and the organizing of output MRI files. MRI images must be in `NiFTI` format. 
Currently, `NeuroNorm` only supports T1-w, T2-w, and FLAIR sequence scans. However, other modalities will be implemented in future versions. It is recommended to store your data in the following structure:

```r
├── General_folder              # main folder
│   ├── disease01_patient01     # patient-level folder
│   │   ├── T1-w                # image in NiFTI format
│   │   ├── T2-w
│   ├── disease01_patient02
│   │   ├── T1-w
│   │   ├── T2-w
│   ├── disease02_patient01
│   │   ├── T1-w
│   │   ├── T2-w
│   │   ├── FLAIR
│   ├── disease02_patient02
└── │   ├── T1-w
```

### Data Loading

`NeuroNorm` only requires two parameters. The first one refers to the folder containing the data (see Data structure). The second parameter corresponds to the covariates of interest needed to perform the RAVEL intensity normalization. Covariates should be associated with the patient's scans. The `NeuroNorm` package does not come with sample data. However, `NeuroData` an additional package located [here](https://github.com/DavidPayares/drat) includes NifTI images, covariates, and folder structure. Make sure you installed `NeuroData` to reproduce the following examples.

```r

library('neuronorm')
library('neurodata')

# Get folder with patients' folders
folder <- system.file("extdata", package = "neurodata")
# Get clinical covariates for RAVEL normalization
covariates <- system.file("covariates.txt", package = "neurodata")
# Read covariates information
clinical_info <- read.csv(file = covariates, sep = ';')

```


### Preprocessing 

The function `preprocess_patients` takes as input the folder containing the raw images and the patients' covariates, applies the preprocessing pipeline to the input images, and creates preprocessed images for each process.

| Parameter            | Description                                                                                                                               |
| ---------------------| ----------------------------------------------------------------------------------------------------------------------------------------- |
| `patients.folder`    | `folder` containing folders per patient with raw T1-w images.                                                                             |
| `clinical.covariates`| `data.frame` of covariates associated with the MRI scans. The number of rows should be equal to the number of images.                           |


The primary purpose of the function `preprocess_patients` is to create preprocessed images just by having the raw images and some covariates of interest.

```r

paths_preprocess_patients <- neuronorm::preprocess_patients(folder, clinical_info)

```

### Preprocessed images

After executing the `preprocess_patients`, a `list` of paths is created. The list contains the relatives paths to each of the preprocessed images organized by patient folder.

```r

library('oro.nifti')

img <- oro.nifti::readNIfTI(file.path(paths_preprocess_patients$patient01$ravel))
orthographic(img)

```
The `orthographic` function from the `oro.nifti` package can be used to visualize the image. The image corresponds to a fully preprocessed MRI scan ready to use in further analysis.

<p align="center">

<img src="img/preprocessing_neuronorm.jpg" width="600"/>

</p>

Reproducible script and sample data can be found in the [example folder](https://github.com/DavidPayares/neuronorm/tree/main/example) or the documentation of `NeuroNorm'available on [CRAN](https://CRAN.R-project.org/package=neuronorm/neuronorm.pdf).

## References


| Method | Step | Citation | Paper Link |
| ------ |----- | ---------|------------|
| N4     | Imhomogeneity Correction |  Nicholas J. Tustison, Brian B. Avants, Philip A. Cook, Yuanjie Zheng, Alexander Egan, Paul A. Yushkevich, and James C. Gee. **N4ITK: Improved N3 Bias Correction**. IEEE Trans Med Imaging, 29:1310–1320, 2010. | [Link](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3071855/) |
| SyN     | Spatial Registration |  B. B. Avants, C. L. Epstein, M Grossman, J. C. Gee **Symmetric diffeomorphic image registration with cross-correlation: evaluating automated labeling of elderly and neurodegenerative brain**. Medical Image Analysis, 12:1310–1320, 2008.| [Link](https://www.sciencedirect.com/science/article/pii/S1361841507000606?via%3Dihub) |
| MNI152     | Population atlas | Evans, A.C., Fox, P.T., Lancaster, J., Zilles, K., Woods, R., Paus, T., Simpson, G., Pike, B., Holmes, C., Collins, D.L., Thompson, P., MacDonald, D., Iacoboni, et al. **A probabilistic atlas and reference system for the human brain: International Consortium for Brain Mapping (ICBM)**. Philos. Trans. R. Soc. London B Biol, 356:1293-1322, 2001.| [Link](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1088516/) |
| HMRF     | Spatially Informed Brain Segmentation | Yongyue Zhang, J. Michael Brady, Stephen Smith **Hidden Markov random field model for segmentation of brain MR image**. Medical Imaging 2000: Image Processing, 2000.| [Link](https://www.spiedigitallibrary.org/conference-proceedings-of-spie/3979/1/Hidden-Markov-random-field-model-for-segmentation-of--brain/10.1117/12.387617.short) |
| RAVEL       | Intensity Normalization |Jean-Philippe Fortin, Elizabeth M Sweeney, John Muschelli, Ciprian M Crainiceanu, Russell T Shinohara, Alzheimer's Disease Neuroimaging Initiative, et al. **Removing inter-subject technical variability in magnetic resonance imaging studies**. NeuroImage, 132:198–212 , 2016.                                                                                                                        | [Link](https://www.sciencedirect.com/science/article/pii/S1053811916001452) |
