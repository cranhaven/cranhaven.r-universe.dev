NEWS
================
Shuowen Chen
3/21/2022

# SortedEffects 1.7.0.
Changes from SortedEffects 1.6.0. The previous version imports a package dummies. Upon receiving a warning from CRAN about reverse dependencies, we revise the code so that the package no longer has the dependency. 

Changes from SortedEffects 1.5.0. The previous version imports a package rlist. Upon receiving a warning from CRAN about reverse dependencies, we revise the code so that the package no longer relies on rlist. 

Changes from SortedEffects 1.4.0. Renormalize the weights for stable estimates using glm.  

Changes from SortedEffects 1.3.0. For nonparametric bootstrap, we use the multinomial weight resampling to produce more stable bootstrap estimates. 

Changes from SortedEffects 1.2.0. The spe function in the 1.2.0 version produces far wider nonparametric bootstrap confidence bands for some specifications. We fix that issue. 

Changes from SortedEffects 1.1.0. The 1.1.0 version has a check problem due to a recent upgrade of package 'tibble' to version 3.0. The package doesn't really need tibble package, so we change the data type. 
