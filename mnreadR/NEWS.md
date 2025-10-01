# mnreadR 2.1.7
========

Overall update to use reframe() in place of summarize() when needed


# mnreadR 2.1.6
========

Fixes a major bug in curve_param_RT() and curve_param_RS() that appeared with version 2.1.5 


# mnreadR 2.1.5
========

A bug was fixed in the function mansfield_algo() in order to improve estimation of the Maximum Reading Speed (MRS). In previous versions, MRS was estimated as the mean of ALL the reading speed measures above the Critical Print Size (CPS). In this fixed version, MRS is now estimated by averaging all reading speed within the optimal plateau, therefore excluding potential slow reading speed measures occurring at the largest print sizes.


# mnreadR 2.1.4
========

The whole package was updated to run properly with the new version of tidyselect 1.0.0


# mnreadR 2.1.3
========

Follow-up updates to fix minor bugs 



# mnreadR 2.1.2
========

mnreadCurve was updated to fix a bug that prevented the MNREAD curve to be plotted in cases where noisy/incomplete/atypical data made MRS and CPS calculation impossible with the original algorithm described in Legge (2007).  

The whole package was updated to run properly with the new version of dplyr 0.8.0



# mnreadR 2.1.1
========

Functions that calculate the Reading Accessibility Index - accIndex() and mnreadParam() - have been updated to fix a major bug preventing the index calculation.



# mnreadR 2.1.0
========

The whole logic of the NLME fitting has been modified to allow for more flexible and transparent analysis. The package now provides separate functions to run the model, estimate the MNREAD parameters and plot the MNREAD curves.

1 main function has been added: 
*   nlmeModel() -> nonlinear mixed-effect (NLME) modeling of the MNREAD data. This function returns a list of two objects: a first object of class dataframe; a second object of class nlme that can be further explored using generic functions from the nlme package.

2 functions have been modified: 
*   nlmeCurve() ->  this function does not run the NLME model anymore!!! It now uses the NLME model created by nlmeModel() to plot the individual MNREAD curves. 
*   nlmeParam() -> this function does not run the NLME model anymore!!! It now uses the NLME model created by nlmeModel() to estimate the Maximum Reading Speed and Critical Print Size.

2 secondary functions have been added: 
*   nlmePredict_PS() -> this function uses results from the NLME model created by nlmeModel() to estimate the print size value required to achieve a given reading speed.
*   nlmePredict_RS() -> this function uses results from the NLME model created by nlmeModel() to estimate the reading speed achieved for a specific print size.

----

The mnreadCurve() function was also modified to display the last sentence presented during a test for which the reading speed was equal to 0 words/minute.



# mnreadR 2.0.0
========

2 functions have been added: 
*   nlmeParam() -> Maximum Reading Speed and Critical Print Size estimation using nonlinear mixed-effect (NLME) modeling supporting nested and grouped structures
*   nlmeCurve() -> NLME estimated curve plotting 



# mnreadR 1.2.0
========

Functions that calculate the Reading Accessibility Index - accIndex() and mnreadParam() - have been updated to:
*   fix a bug in the missing values handling
*   fix a bug in the format of the output dataframe

mnreadCurve() was modified to display the print size range encompassed by the Reading Accessibility Index calculation

A message has been added to the functions that estimate the Maximum Reading Speed and Critical Print Size - mnreadParam(), curveParam_RT() and curveParam_RS() - to remember the user to check the accuracy of the estimates by inspecting the MNREAD curve using mnreadCurve()




# mnreadR 1.1.0
========

1 function has been added: 
*   mnreadCurve() -> Individual MNREAD curve plotting

Package citation was updated 




# mnreadR 1.0.0
========

1 function has been added: 
*   curveParam_RS() -> Maximum Reading Speed and Critical Print Size estimation using reading speed measures (instead of reading time and errors)

2 functions have been renamed:
*   curveParam_RT() replaces curveParam()   
*   accIndex() replaces ACCcalc() 

Package and functions descriptions were updated




# mnreadR 0.1.0
========

Features 7 new functions to analyze MNREAD data: 

*   mnreadParam() -> MNREAD parameters estimation
*   curveParam() -> Maximum Reading Speed and Critical Print Size estimation using reading time and errros
*   readingAcuity() -> Reading Acuity calculation
*   ACCcalc() -> Reading Accessibility Index calculation 
*   readingSpeed() -> Reading speed calculation
*   readingSpeed_nonCorrected() -> Reading speed calculation (non corrected for errors)  
*   logMARcorrect() -> Print size correction for non-standard viewing distance
