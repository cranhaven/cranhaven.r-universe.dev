# R package drugprepr

Belay B. Yimer, David A. Selby, Meghna Jani, Goran Nenadic, Mark Lunt, William G. Dixon

An algorithm for the transparent and efficient preparation of electronic prescription data into information on individuals’ drug use over time. 
The goal of the `drugprepr` package is to allow users to perform multiverse analyses in a concise and easily interpretable manner. The `drugprepr` package allows researchers to specify sets of defensible data processing options at each decision node (e.g., different ways of imputing missing quantity and daily dose, different ways of handling multiple prescriptions), implement them all, and then report the outcomes of all analyses resulting from all possible choice combinations. 
The package depends on the R package `doseminer` for extracting drug dosage information from freetext prescription data.

# Installation
You can install the latest development version from `GitHub`:

```
devtools::install_github("belayb/drugprepr")
```

# Contributors

Maintained by Belay Birlie Yimer and David Selby of the Centre for Musculoskeletal Research, University of Manchester, UK.
Pull requests and GitHub issues are welcomed.
