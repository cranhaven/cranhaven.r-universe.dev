
![CRAN_Version](http://www.r-pkg.org/badges/version/eyeRead)  
##### development version (in dev branch)
[![Dev Build](https://travis-ci.org/SanVerhavert/eyeRead.png?branch=dev)](https://travis-ci.org/SanVerhavert/eyeRead) [![Dev codecov](https://codecov.io/gh/SanVerhavert/eyeRead/branch/dev/graph/badge.svg)](https://codecov.io/gh/SanVerhavert/eyeRead)
##### Release version (in master branch)
[![Build](https://travis-ci.org/SanVerhavert/eyeRead.png?branch=master)](https://travis-ci.org/SanVerhavert/eyeRead) [![codecov](https://codecov.io/gh/SanVerhavert/eyeRead/branch/master/graph/badge.svg)](https://codecov.io/gh/SanVerhavert/eyeRead)

# eyeRead
`eyeRead` is an R package contains some functions to prepare and analyse eye 
tracking data of reading exercises. Essentially it identifies first pass and
second pass fixations and their respective total durations.  

This package is not yet on CRAN.  

## Install

The development version of the package can be downloaded after installing
`devtools` and by running  
```
devtools::install_github( "SanVerhavert/fixationDuration", ref = "dev" )
```   
It cannot be guaranteed this is a stable version. Check the dev build badge to see
if the development version is stable (passing).  

## Use

An example on how to use the package can be found in the demo file.  
You can run a demo after installing the package by running  
```
demo( "simulated data", package = "eyeRead" )
```


## Contribute

You can contribute to this package in two ways:  
1/ by submitting issues or requests  
2/ by suggesting changes through pull requests  

### Issues or requests
You can submit issues or feature requests through the issues tab on GitHub.  
Please provide a clear description of the issue: e.g. code that causes the issue,
possible error message, the data that causes the error (as .txt file [preferred]
or R code[also preferred] or described )

### Pull requests
If you directly want to contribute code or changes, please fork this repository.
You can then add your changes and create a pull request. Please document your
pull request, otherwise it will be rejected.  
This repository follows the [gitflow logic](https://nvie.com/posts/a-successful-git-branching-model/).
Please respect this logic and the branch structure in the following section.  
Mind that gitflow only uses non-fast-forward merges.

## Branch structure
This repository follows the [gitflow logic](https://nvie.com/posts/a-successful-git-branching-model/).  

### Branch logic
These are the main branches:
- master &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Branch containing stable versions; do not commit to this branch;
Only merge from release branch.
- release &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Branch for preparing releases; commits to this branch only contain
version bumps and generated documentation; only merge from dev branch; merge to
master and dev branches
- dev &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Branch for main development; all commits allowed; merge from master
and release branches  

Supporting branches [optional]:
- feature_*&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Branches for developing new features; can be used to keep dev
branch clean and the development build working; only branches from and merges to
the dev branch
- issue_*&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Branches for solving issues; name mostly referring to issue numbers;
can branch from every other branch except (!) from master; merges only with the
branch from witch it was branched

### Overview of this repository

.  
|_ master  
|\\_ release  
|\\_ dev  
| |\\_issueNA 
| |\\_issue_reread
| |\\_feat\_dur\_out  
