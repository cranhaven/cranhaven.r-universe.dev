# Cornerstone R Scripts

**CornerstoneR** package provides generic R scripts which enable you to use existing R routines in Cornerstone.

The desktop application [Cornerstone](https://www.camline.com/en/products/cornerstone/cornerstone-core.html) is a data analysis software provided by camLine that empowers engineering teams to find solutions even faster.
The engineers incorporate intensified hands-on statistics into their projects.
They benefit from an intuitive and uniquely designed graphical Workmap concept: you design experiments (DoE) and explore data, analyze dependencies, and find answers you can act upon, immediately, interactively, and without any programming.

While Cornerstone's interface to the statistical programming language [R](https://www.r-project.org/) has been available since version 6.0, the latest interface with R is even much more efficient.
Cornerstone release 7.1.1 allows you to integrate user defined R packages directly into the standard Cornerstone GUI.
Your engineering team stays in Cornerstone's graphical working environment and can apply R routines, immediately and without the need to deal with programming code.

Learn how to use R packages in Cornerstone 7.1.1 on [camLineTV YouTube](https://www.youtube.com/watch?v=HEQHwq_laXU) channel (available in German).


## Documentation

1. [Overview](https://camLine.gitlab.io/CornerstoneR/docs/)
1. [Function Reference](https://camLine.gitlab.io/CornerstoneR/docs/reference/)
1. [News](https://camLine.gitlab.io/CornerstoneR/docs/news/)


## Installation

The installation of 'CornerstoneR' is written from the perspective of a 'Cornerstone' user. A 
successful installation and licensing of 'Cornerstone' is required. No experience with 'R' is
required.

1. [Download 'R'](https://cran.r-project.org/), click on 'Download R for Windows' and 'base' on the
following page. You will then be taken to a page from which you can download the current version 
of 'R'. Execute the file and follow the installation instructions.
1. Start 'R'. A window with blue text and the title 'R Console' is displayed which is embedded in
a window with the title 'RGui'. This is the default environment of 'R' where you install packages
and execute commands directly in 'R'.
1. Now install 'CornerstoneR' using one of the options from the following sections. When asking for a
download mirror you can decide freely. The first 'cloud' link automatic redirects to servers
worldwide, and is currently sponsored by 'RStudio'.
1. When packages are installed for the first time, you are asked if you want to install them into
your private library. Answer this question with Yes.

### CRAN Version

Packages at CRAN (Comprehensive R Archive Network) are precompiled and checked by the R Team. The
package 'CornerstoneR' will be updated on CRAN when new versions are released.

Install the version from CRAN by copying and executing the following command in the R console.
```r
install.packages("CornerstoneR")
```

To update all packages from CRAN, first close 'Cornerstone' and 'R'. Afterwards open 'R' and run
the following command
```r
update.packages(ask=FALSE)
```


### Development Version

The latest features are available in the development version of 'CornerstoneR'. Your feedback is
integrated into the development process and can be tested in this version.

For the development version from GitLab we use [remotes](https://cran.r-project.org/package=remotes).
Install the development version by copying and executing the following two commands in the R console.
```r
install.packages("remotes")
remotes::install_gitlab("camLine/CornerstoneR", dependencies = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```
Sometimes it is necessary to install certain versions, for example from a specific branch.
To install a certain branch, commit, or tag, append the corresponding `name` after an `@` to the repository name:
```r
remotes::install_gitlab("camLine/CornerstoneR@name", dependencies = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

To update the development version, first close 'Cornerstone' and 'R'. Afterwards open 'R' and run
the installation command again.
