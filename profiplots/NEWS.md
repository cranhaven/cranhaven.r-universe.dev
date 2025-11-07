# profiplots 0.2.3

* Extending the Plots gallery page significantly.
* Increasing number of colours in default palette to 9 (may caused warnings as there are only 6 colors in the `default` palette).
* Couple more patches to please the CRAN policy checker and prepare the package to be released.


# profiplots 0.2.2

* Mostly patches to please the CRAN policy checker and prepare the package to be released.

# profiplots 0.2.1

* Minor changes mainly Docs oriented.
* Better package site.
* Preparations to be released on CRAN.


# profiplots 0.2.0

* Name changed to align with python version [profi-plots](https://datascience.profinitservices.cz/sablony/profiplots-r/).
* Palettes changed. Now there are:
  * Diverging palettes: `blue-red` (Default - base R and ggplot mapping for continuous variables.) and `blue-white-red`
  * Sequential palettes: `blues`, `blues-dark`, `reds`, `reds-dark`, `greys`
  * Discrete palettes: `discrete` (Default for discrete ggplot mapping), `discrete-full`
* Added support for the exact palette color values. Helpful especially for `discrete` and `discrete-full` color scales when you'd like to avoid color interpolation.
  * The functionality is added to `profinit_pal()` as well as `scale_color_profinit`, `scale_fill_profinit` etc.
* Default settings is now stored on `set_theme`. On `unset_theme()`, there is an attempt to restore the original setting instead of deleting the setting completly.


# profiplots 0.1.0

* `set_theme()` and `unset_theme()` global configuration added.
* GitlabPages released.
