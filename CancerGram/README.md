[![published in: Pharmaceutics](https://img.shields.io/badge/published%20in-Pharmaceutics-green.svg)](https://doi.org/10.3390/pharmaceutics12111045)
[![R build status](https://github.com/BioGenies/CancerGram/workflows/R-CMD-check/badge.svg)](https://github.com/BioGenies/CancerGram/actions)


Identify anticancer peptides
-------------------------

CancerGram identifies anticancer peptides using n-gram encoding and random forests. It can be also accessed as a web-based service http://biongram.biotech.uni.wroc.pl/CancerGram/. 

How to cite?
-------------------------
Please use: Burdukiewicz, M., Sidorczuk, K., Rafacz, D., Pietluch, F., Bąkała, M., Słowik, J., and Gagat, P. (2020). CancerGram: An Effective Classifier for Differentiating Anticancer from Antimicrobial Peptides. Pharmaceutics 12, 1045, https://doi.org/10.3390/pharmaceutics12111045.

Local instance of CancerGram
------------------------

You can install the latest development version of the package:

```R
source("https://raw.githubusercontent.com/r-lib/remotes/master/install-github.R")$value("BioGenies/CancerGram")
```

After installation GUI can be accessed locally:

```R
library(CancerGram)
CancerGram_gui()
```


Installing dependency: CancerGramModel
------------------------
To be able to use CancerGram properly, you should have installed the 'CancerGramModel' package available via GitHub. 
CancerGramModel contains stacked random forest model and informative n-grams required for prediction of anticancer peptides.
Due to the large size of a model, it needs to be stored in the external repository, as CRAN do not allow upload of files
larger than 5 MB. 

You can install CancerGramModel using the install_CancerGramModel function:

```R
install_CancerGramModel()
```

Anticancer peptides might be also identified in the batch mode:

```R
library(CancerGram)
library(CancerGramModel)
# if you do not have CancerGramModel use:
# install_CancerGramModel()
sequences <- read_txt(system.file("CancerGram/prots.txt", package = "CancerGram"))
predict(CancerGram_model, sequences)
```

Unix/macOS: curl
------------------------

The curl library is one of the dependencies of the CancerGram package and requires additional, non-R software. If you encounter an error concerning curl, please follow instructions below to install curl (adapted from https://github.com/jeroen/curl).

Binary packages for OS-X or Windows can be installed directly from CRAN:

```r
install.packages("curl")
```

Installation from source on Linux requires `libcurl`. On Debian or Ubuntu use libcurl4-openssl-dev:

```bash
sudo apt-get install -y libcurl-dev
```

On Fedora, CentOS or RHEL use libcurl-devel:

```bash
sudo yum install libcurl-devel
```

On OS-X libcurl is included with the system so nothing extra is needed. However if you want to build against the most recent version of libcurl, install and force-link curl from homebrew:

```bash
brew install curl
brew link --force curl
```

Note that on OS-X you must recompile the R package from source after force-linking curl, otherwise you get a version conflict with the system version of libcurl.

Funding
------------------------
This work was supported by National Science Centre grants 2017/26/D/NZ8/00444, 2018/31/N/NZ2/01338 and 2019/35/N/NZ8/03366 to FP.
