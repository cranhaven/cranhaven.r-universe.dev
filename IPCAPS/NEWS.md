
# IPCAPS 1.1.8

## Updates

* Updated the package according to R v4.0.2
* Reduce the sizes of example files

# IPCAPS 1.1.7

## Updates

* Updated the functions to support re-analysis when the result files are moved to another directory; top.discriminator, ipcaps.
* top.discriminator now accepts a percentile for filtering.


# IPCAPS 1.1.6

## Updates

* Corrected descripton of export.gorups


# IPCAPS 1.1.5

## Updates

* Added more details in roxygen part of ipcaps-package.R
* Fixed text according to comments from CRAN
* Updated README.md according to the changes above


# IPCAPS 1.1.4

## Updates

* Added references in the DESCRIPTION file
* Added single quotes for Software's names in the DESCRRIPTION file


# IPCAPS 1.1.3

## Fixed Text

* In DESCRIPTION
* In README.Rmd

# IPCAPS 1.1.2

## Fixed Bugs

* Removed all examples for roxygen2 for all internal functions
* Removed all "library(IPCAPS)" in unit tests and examples

## Changes

* All example files were changed from ```simSNP*``` to ```IPCAPS_example*``` 

## Added example file

* inst/extdata/IPCAPS_example.RData to used in the example of ipcaps()


# IPCAPS 1.1.1

## Added unit testing

* testthat/test_clusteringmode.R

## Changes

* moved the nested functions in pasre.categorical.data and preprocess to parallelization.R
* add roxygen2 code to pasre.categorical.data, replace.missing, do.glm

## Updates

* In roxygen part, changed from getwd() to tempdir()
* fixed errors according to check as cran

## Fixed functions

* preprocess(): changed filename.label to label.file

---

# IPCAPS 1.1.0

## Updated functions

* In check.stopping(), changed from 
eigen.fit = max(eigen.fit.vec[1:2]) to 
eigen.fit = max(eigen.fit.vec).
This change may affect clustering result.

---

# IPCAPS 1.0.7

## Added data

* data/IPCAPS_example.RData
* inst/extdata/IPCAPS_example.bed
* inst/extdata/IPCAPS_example.bim
* inst/extdata/IPCAPS_example.fam
* inst/extdata/IPCAPS_example.individuals.txt
* inst/extdata/IPCAPS_example_PC10.txt
* inst/extdata/IPCAPS_example_rowVar_colInd.txt

## Added R file to describe data files

* R/data.R

## Added unit testing

* testthat/test_caleigenfit.R
* testthat/test_checkstopping.R

---

# IPCAPS 1.0.6

## Fixed functions

Removed the old RD files and added roxygen script to create manual for these functions:

* ```save.plots.cluster.html``` 
* ```save.plots.label.html``` 
* ```save.plots``` 
* ```top.discriminator``` 

---

# IPCAPS 1.0.5

## Fixed functions

Removed the old RD files and added roxygen script to create manual for these functions:

* ```preprocess``` 
* ```process.each.node``` 
* ```save.eigenplots``` 
* ```save.html``` 

---

# IPCAPS 1.0.4.0

## Fixed functions and objects

Removed the old RD files and added roxygen script to create manual for these 
functions and objects:

* ```IPCAPS-package``` 
* ```ipcaps``` 
* ```output.template``` 
* ```postprocess``` 

---

# IPCAPS 1.0.3.0

## Fixed functions

Removed the old RD files and added roxygen script to create manual for these functions:

* ```diff.eigen.fit``` 
* ```diff.xy``` 
* ```export.groups``` 
* ```get.node.info``` 

---

# IPCAPS 1.0.2.0

## Fixed functions

Removed the old RD files and added roxygen script to create manual for these functions:

* ```cal.eigen.fit``` 
* ```check.stopping``` 
* ```clustering.mode``` 
* ```clustering``` 

---

# IPCAPS 1.0.1

## Fixed functions

Removed the old RD files and added roxygen script to create manual for these functions:

* ```cal.pc.linear``` 
* ```cal.pc.linear``` 
* ```cal.pc.linear``` 
* ```cal.pc.linear``` 

---

# IPCAPS 1.0.0.0

## Initial functions

* ```cal.pc.linear``` A function for linear principal component analysis (PCA)
* ```fst.each.snp.hudson``` A function for fixation index (Fst) calculation for 
all SNPs between two groups.
* ```fst.hudson``` A function for average fixation index (Fst) calculation 
between two groups.
* ```plot3views``` A function to create scatter plots in three views.
* ```read.bed``` Read the binary PLINK format (BED, BIM, and FAM)
* ```rubikclust``` A function for unsupervised clustering to detect rough 
structures and outliers.
* ```write.bed``` Write an list of SNP object to the binary PLINK format (BED, 
BIM, and FAM)
* ```xxt``` A function for calculating matrix multipication between a matrix and 
its transpose for large data.

## Initial R data 

* ```simsnp``` Synthetic dataset containing single nucleotide polymorphisms 
(SNP)
* ```sample_labels``` Synthetic dataset containing population labels for the 
dataset simsnp.

## Initial example files

* ```example_SNP.bed``` Synthetic dataset containing single nucleotide polymorphisms 
(SNP) in binary format
* ```example_SNP.bim``` Simulated SNP information
* ```example_SNP.fam``` Simulated sample information

## Updates

From the initial idea, some functions were changed their names:

* The name of function ```cal.PC.linear``` was changed to ```cal.pc.linear```.
* The name of function ```plot.3views``` was changed to ```plot3views```.
* The name of function ```rubikClust``` was changed to ```rubikclust```.
* The name of function ```XXt``` was changed to ```xxt```.
