Version 0.2.3
==============

* Allows more flexible treatment of values below the detection threshold.
* Fixes errors thrown with tibble 3.0.0 release.


Version 0.2.2 
==============

* Fixes errors thrown with dplyr 1.0.0 release.


Version 0.2.1
================

* Notation for the triphasic exponential model has been modified to more clearly relate to that of the biphasic model.


Version 0.2.0
================

* A triphasic exponential model has been added so that data from ART containing integrase inhibitors can also be fit (see ?ushr_triphasic)
* Users can now specify the range of initial observations from which the beginning of each individual trajectory is chosen (previously this was fixed to the first three observations)
* There is now added functionality to view pairwise correlation plots for all estimated parameters
* GGally has been added as a suggested package
* Fixed bug that threw an error when summarize_model() was used with model output that included either biphasic or single phase fits, but not both
* Fixed error (thrown with development version of R) that assumed length(class(obj)) == 1


Version 0.1.0
================

Initial release
