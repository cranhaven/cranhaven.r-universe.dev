[![Build Status](https://api.travis-ci.org/michbur/AmpGram.png)](https://travis-ci.org/michbur/AmpGram)

<img src="https://github.com/michbur/AmpGram/blob/master/inst/AmpGram/AMP_log.png" alt="AmpGram" style="height: 200px;"/>

Identify antimicrobial peptides
-------------------------

AmpGram identifies antimicrobial peptides using n-gram encoding and random forests. It can be also accessed as a web-based service http://www.smorfland.uni.wroc.pl/shiny/AmpGram/. 

Local instance of AmpGram
------------------------

You can install the latest development version of the package:

```R
source("https://raw.githubusercontent.com/r-lib/remotes/master/install-github.R")$value("michbur/AmpGram")
```

After installation GUI can be accessed locally:

```R
library(AmpGram)
AmpGram_gui()
```


Installing dependency: AmpGramModel
------------------------
To be able to use AmpGram properly, you should have installed the 'AmpGramModel' package available via GitHub. 
AmpGramModel contains stacked random forest model and informative n-grams required for prediction of antimicrobial peptides.
Due to the large size of a model, it needs to be stored in the external repository, as CRAN do not allow upload of files
larger than 5 MB. 

You can install AmpGramModel using the install_AmpGramModel function:

```R
install_AmpGramModel()
```

Antimicrobial peptides might be also identified in the batch mode:

```R
library(AmpGram)
library(AmpGramModel)
# if you do not have AmpGramModel use:
# install_AmpGramModel()
sequences <- read_txt(system.file("AmpGram/prots.txt", package = "AmpGram"))
predict(AmpGram_model, sequences)
```
Unix/macOS: curl
------------------------

The curl library is one of the dependencies of the AmpGram package and requires additional, non-R software. If you encounter an error concerning curl, please follow instructions below to install curl (adapted from https://github.com/jeroen/curl).

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

