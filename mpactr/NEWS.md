# mpactr 0.3.0

* Major changes to `filter_cv()`. Old `filter_cv()` would calculate the coefficient of variance of the biological groups, calculate the average/median, and remove the feature if the average/mean was above the threshold. To account for some groups having high variation by default, the new `filter_cv()` will now zero out groups if they are above the threshold. If every group in the feature has been zeroed out, we will remove the feature. 

* `filter_cv()` has been optimized to use c++ code, so there should be significant speed improvements.

* You are also now able to import `data.frames` for peak table input in `import_data()`.

# mpactr 0.2.1

# mpactr 0.1.0

# mpactr 0.1.0

* Initial CRAN submission.

* Contains four main filters: `filter_mispicked_ions()`, `filter_group()`, `filter_cv()`, and `filter_insource_ions()` for the correction of tandem MS/MS peaks.


