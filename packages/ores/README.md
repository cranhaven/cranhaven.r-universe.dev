# ORES API client

[![Travis-CI Build Status](https://travis-ci.org/Ironholds/ores.svg?branch=master)](https://travis-ci.org/Ironholds/ores) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ores)](https://cran.r-project.org/package=ores
) ![downloads](http://cranlogs.r-pkg.org/badges/grand-total/ores)

`ores` provides an API client for the Objective Revision Evaluation Service; an AI system designed to identify whether edits to Wikimedia projects like Wikipedia are damaging, likely to be reverted, or made in good faith, and what class of quality the underlying article falls into.

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/Ironholds/ores/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.

## Use

```
library(ores)

# Check if an edit does damage
check_damaging("enwiki", 34854345)
#       edit project prediction false_prob true_prob
# 1 34854345  enwiki       TRUE  0.4381965 0.5618035

# Check edit quality
check_quality("enwiki", 34854345)
# edit    project prediction   stub_prob start_prob     c_prob   b_prob     ga_prob   fa_prob
# 34854345  enwiki         FA 0.001613388 0.01765724 0.02922814 0.2296085  0.009218907 0.7126738

```
## Installation

`ores` can be obtained from CRAN with:

> install.packages("ores")

The package also lives on GitHub; you can install it with:

> devtools::install_github("ironholds/ores")
