# Examples 

We provide examples on how to perform *uncertainty quantification* with the ``uqsa`` R package. The examples consist of R scripts. For large systems (like AKAP79) we recommend to run the scripts on a cluster.

The example files are available at the following links.

* [AKAR4](https://github.com/icpm-kth/uqsa/blob/master/inst/extdata/AKAR4/runABCMCMC_AKAR4.R)
* [AKAP79](https://github.com/icpm-kth/uqsa/blob/master/inst/extdata/AKAP79/runABCMCMC_AKAP79.R)

The description of model AKAP79 and the data used for uncertainty quantification are available in the publication 
```
Church, Timothy W., et al. "AKAP79 enables calcineurin to directly suppress protein kinase A activity." 
Elife 10 (2021): e68164.
```

AKAR4 is a submodel obtained from AKAP79 and it does not require intensive computation effort. An example of ``uqsa`` applied to this smaller model (AKAR4) is also available in the Jupyter notebook [AKAR4](https://github.com/icpm-kth/uqsa/blob/master/UQ_AKAR4.ipynb).

