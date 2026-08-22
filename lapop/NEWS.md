# lapop 2.1.7
* include lpr_na_attributes() for non-reponses .a .b .c

# lapop 2.1.6
* update lpr_ccm() an lapop_cmm() to plot 4-variables

# lapop 2.1.5

* include lpr_ci() for data playground design-based confidence intervals

# lapop 2.1.4

* update fonts to Nunito

# lapop 2.1.3

* include lapop_map()

# lapop 2.1.2

* include ci_type to lapop_ts()

# lapop 2.1.1

* include lpr_extract_ros()

# lapop 2.1.0

* include vignettes 
* create pkgdown website

# lapop 2.0.9

* include AB data for vignettes (bra23, cm23, ym23)

# lapop 2.0.8

* update lpr_data() to take different inputs

# lapop 2.0.7

* add xvar parameter for lapop_stack()
 
# lapop 2.0.6

* add interact parameter for lpr_coef()

# lapop 2.0.5
* add use_cat parameter for lpr_mline()

# lapop 2.0.4

* add lpr_set_attr()
* add lpr_set_ros()
* add lpr_extract_notes()

# lapop 2.0.3

* add lpr_resc()

# lapop 2.0.2

* update labels system for stack and mover
* add "by" and "over" options in lpr_stack() and lpr_mline()

# lapop 2.0.1

* add lpr_coef() for regressions

# lapop 2.0.0

* add lpr_data() processing functionality for all graph types

# lapop 1.3.14

* Create function lapop_cccomb()
* add drop_singles to lapop_dumb()
* allow CIs for lapop_mline() graphs

# lapop 1.3.13

* add french labels

# lapop 1.3.12

* add ability for multiple highlights in cc

# lapop 1.3.11

* fix glitch with mline() end label
* change default horizontal alignment of subtitles back to zero
 
# lapop 1.3.10

* add option legendnrow to stack() and mline()

# lapop 1.3.9

* change default colors
* titles, subtitles, and source info aligned with plot
*add country highlight option for ccm
* line space before and after x axis label


# lapop 1.3.8

* updated gridlines and borders in line with new report templates 

# lapop 1.3.7

*fixed bug in ccm sorting

# lapop 1.3.6

* fixed warnings in dependencies on documentation
* added option for text_nudge in lapop_dumb()

# lapop 1.3.5

* fix issue in mline where repeated labels became disorganized

# lapop 1.3.4

* update fonts and colors (new designer)

# lapop 1.3.3

* allow 6 lines in mline
* allow custom ordering of factors in mover

# lapop 1.3.2.4

* change default text to roboto

# lapop 1.3.2.3

* remove unused dependencies
* add all_labels argument to mline
* fix issues with lines in mline

# lapop 1.3.2.2

* remove showtext from lapop_font_design()
* change font in mline()

# lapop 1.3.2.1

* add lapop_font_design()
* specify default font as nunito in all graphs

# lapop 1.3.1 

* add x-axis label to ccm()

# lapop 1.3.0

* fixing spacing issue in subtitle for lapop_mover() and lapop_ccm()

# lapop 1.2.8 

* add fixed_aspect_ratio = TRUE option to stack()

# lapop 1.2.7

Changes to lapop_ccm including: 
* text placed directly above error bars (no offset).  Offset customizable with text_position argument.  
* fixed automatic alphabetical sorting of bars (now order of dataframe)
* new sorting options (by var2 and var3 possible)

# lapop 1.2.6

* Update lapop_ccm() to allow 3 bars.  Fixed spacing issue with subtitle.  

# lapop 1.2.5

* Update lapop_ccm() with fix for subtitle issue.  Add y-axis label. 
 
# lapop 1.2.4

* Add option to remove percentages in ts and mline

# lapop 1.2.3

* remove "AmericasBarometer" mentions from help files

# lapop 1.2.2

* remove "AmericasBarometer" from automatically appearing in source

# lapop 1.2.1

* Automatically convert varlabel to character in mline() and stack()

# lapop 1.2.0

* Added lapop_ccm() 

# lapop 1.1.2

* Fixed interpolation on beginning of series in lapop_mline()

# lapop 1.1.1

* Change all y-axis defaults to 0-100

# lapop 1.1.0

* Deprecate old function names to match Stata ado file names

# lapop 1.0.4

* Changed lapop_ts() to work with year instead of wave
* Fixed bug with labels in lapop_tsmulti()
* Reduced size of gap between bars in stacked bar
* Added repelling of labels that overlapping in lapop_sb()
* Made subtitle movable in lapop_tsmulti()
* lapop_coef() description header was wrong
* Time series help page: example at the bottom was incorrectly formatted
* Interpolation by group for lapop_tsmulti() now prevents interpolation of final data point
* Made sorting possible in lapop_sb()

# lapop 1.0.3

* updated interpolation method in lapop_ts()
* add multivariable time series lapop_tsmulti()

# lapop 1.0.2

* Explained acronyms in package description
* Updated return values in function descriptions
* Removed \dontrun{} from lapop_save example and changed file directory to temp

# lapop 1.0.1

* updated errors in description that were rejected by CRAN

# lapop 1.0.0

* Added a `NEWS.md` file to track changes to the package.
