[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2531640.svg)](https://doi.org/10.5281/zenodo.2531640)


#### DynaRankR

##### Overview
This package provides functions for inferring longitudinal dominance hierarchies, which describe dominance relationships and their dynamics in a single latent hierarchy over time.
The package includes functions to implement the multiple ranking methods and associated support functions.

`informed_matreorder` Implements Informed MatReorder method  
`informed_elo` Implements Informed Elo-rating method  
`informed_ds` Implements Informed David's Score method  
`get_dynamics` Calculates dynamics for a longitudinal hierarchy  
`plot_ranks` Visualize longitudinal hierarchy  
`dyadic_similarity` Compare two rank orders  
`get_edgelist` Convert data from matrix to edgelist  
`edgelist_to_matrix` Convert data from edgelist to matrix  
`C.crocuta.female` Example data from female spotted hyenas  
`C.crocuta.male` Example data from male spotted hyenas  

##### Usage
```R
data(C.crocuta.female)

informed_matreorder(contestants = C.crocuta.female$contestants, convention = 'mri', n = 5, shuffles = 10, require.corroboration = TRUE, initial.ranks = C.crocuta.female$initial.ranks, interactions = C.crocuta.female$interactions)

```