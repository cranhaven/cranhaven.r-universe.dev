# bayesassurance R package

This R package offers a constructive set of simulation-based functions
used for determining sample size and assurance in various settings. 
We hope these functions will be useful for addressing a wide range of 
clinical trial study design problems. 

# Setup Instructions

To install the `bayesassurance` package in R, there are several ways to compile the package from
source as the package is not yet available on CRAN. 

## Directly From Github (Mac/Windows)
  1. Open R Studio.
  2. Make sure `devtools` is installed and loaded. If not, run `install.packages("devtools")` and 
  load the package using `library(devtools)` once installation is complete. 
  3. Install the bayesassurance package directly through Github by running
  `devtools::install_github("jpan928/bayesassurance_rpackage")`. 
  You may be asked to install [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/)
  on a Windows machine. 
  4. If prompted with "These packages have more recent versions available. It is recommended to
  update all of them. Which would you like to update?", type "1" and press Enter. 
  5. Load package using `library(bayesassurance)` and start using package normally. 


## Using `tar.gz` File

Alternatively, you can build the package using the tar.gz file.

### Mac

#### Within R Studio

  1. Download the `bayesassurance_0.1.0.tar.gz` file. 
  2. Open R Studio.
  3. In the R prompt, navigate to where this file is stored using `setwd("your/filepath/here")`. 
  4. Run `install.packages("bayesassurance_0.1.0.tar.gz", repos = NULL, type = "source")`. 
  5. Load package using `library(bayesassurance)` and start using package normally. 

  
### Windows

#### Within R Studio

  1. Download the `bayesassurance_0.1.0.tar.gz` file. 
  2. Open R Studio.
  3. In the R prompt, navigate to where this file is stored using `setwd("your/filepath/here")`. 
  4. Run `install.packages("bayesassurance_0.1.0.tar.gz", repos = NULL, type = "source")`.
  You may be asked to install [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/). 
  5. Load package using `library(bayesassurance)` and start using package normally. 
  
  
#### Within Command Prompt

  1. Download the `bayesassurance_0.1.0.tar.gz` file.
  2. Open command prompt.
  3. Identify path of the folder to where R is installed and run `PATH <your/filepath/here>`. 
  An example of this file path is C:\Program Files\R\R-4.1.3\bin\x64. 
  4. On the same command prompt, navigate to the directory containing `bayesassurance_0.1.0.tar.gz`. 
  5. Enter `R CMD INSTALL bayesassurance_0.1.0.tar.gz` to install the package. 
  6. Open R Studio and run `library(bayesassurance)` and start using package normally. 


# Replication Materials

For JSS reviewers, R scripts containing the necessary code to reproduce figures and examples
in the manuscript can be found under `Replication_Material`. Please refer to the main 
script file, `replication_script.R`, and work through the examples in chronological 
order. The script includes all worked out examples and figures in the order in which they 
appear in the manuscript. It will also point you to the supplementary R Markdown files 
(`fig7_replication.Rmd`, `fig9_replication.Rmd`, and `fig10_replication.Rmd`)
where appropriate. 
  
# Vignettes

Vignettes are currently undergoing revisions and will be available soon. 

