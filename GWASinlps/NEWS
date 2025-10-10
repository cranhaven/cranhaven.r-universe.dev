# GWASinlps v2.3

* Insignificant minor technical update.

# GWASinlps v2.2

* Added analysis for survival data using accelerated failure time models. Added new function nlpsAFTM and updated the main function GWASinlps accordingly.

* Added peMOM based analyses wherever available.

* Minor changes: Some language changes in manual files and description.

# GWASinlps v2.1

* A small change in Cpp code: replaced & with &&.

# GWASinlps v2.0

* The computational efficiency was considerably increased.

* A bug persistent in the previous versions and related to the exclusion of unselected variables from future iterations was fixed. The bug sometimes led to a smaller number of selected variables than what could optimally be selected with a given set of tuning parameters that determined the stopping rule.

* For binary data, a new method was added so that there are currently two available methods, "rigorous" and "quick" (used in earlier package versions). A new function nlpsGLM() was added. Also, the use of the deprecated function pmomPM() from the mombf package was replaced by the use of the function modelSelection() consistently with the latest mombf package version 3.1.4. 

* The printed information (if verbose=TRUE) and the GWASinlps() function output were modified to contain actual variable names (if given) rather than their positional indices. Also, iteration-wise selected variables were included in the output. 


# GWASinlps v1.2

* Extention to binary data analysis. Added in function GWASinlps a new argument 'family' which can be normal or binomial.

* The status of the reference paper Sanyal et al. (2018) is published. 

* The author/maintainer email was updated.


# GWASinlps v1.1

* We have included the option to use 'horseshoe' prior within our iterative structured screen-and-select framework. 

* We have included the option to output selected variables after every skipping, by introducing an argument 'skip.return' in the GWASinlps function.

* For reproducibility, we have included an argument 'seed' in the GWASinlps function, allowing the user to set the random seed at the beginning of GWASinlps procedure.


