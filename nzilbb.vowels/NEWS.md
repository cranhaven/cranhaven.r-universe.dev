# nzilbb.vowels 0.4.1

- Remove `VignetteBuilder` from `DRESCRIPTION`.
- Now `plot_procrustes_loadings()` correctly sets the y-axis label to 'Loading' 
or 'Index loading'.

# nzilbb.vowels 0.4.0

## Breaking changes 

* In `mds_test()`, first argument changed from `similarity_matrix` to 
`dissimilarity_matrix` to match the behaviour expected by users of 
`smacof::smacofSym()`

## New features

* New `pca_rotate_2d()` takes the output of `stats::prcomp()` or 
`stats::princomp()` and rotates two specified PCs clockwise by `angle` degrees.
Both scores and loadings are rotated and the variance explained by each 
component is updates.

* New `pca_rotate_procrustes()` allows Procrustes rotation of the output of
`prcomp()` or `princomp()` to match a target configuration of either scores
or loadings. Partial overlap of variables is enabled by `rotation_variables`.
Both scores and loadings are rotated and the variance explained by each 
component is updates.

* New `procrustes_loadings()` generates data to enable calculation of
confidence intervals of loadings, or confidence intervals and null distributions
of index loadings, by bootstrapping, permutation, and Procrustes rotation.

* New `plot_procrustes_loadings()` provided to plot the output of 
`procrustes_loadings()`.

* New articles on package website <https://nzilbb.github.io/nzilbb_vowels/> 
covering the 'model-to-PCA' workflow used at NZILBB and the use of rotation with 
PCA.

# nzilbb.vowels 0.3.1

* Documentation fixes to initial CRAN submission (`nzilbb.vowels 0.3`).

# nzilbb.vowels 0.3

* Initial submission to CRAN
* `mds_test()` and `plot_mds_test()` added to determine number of dimensions for 
MDS.
* `pc_flip()` added to reverse orientation of selected PCs in PCA analysis.

# nzilbb.vowels 0.2.1

* All functions required for [Wilson Black et al. (2022)]( https://doi.org/10.1111/lnc3.12479)
