# VBTree package
[![CRAN Version](http://www.r-pkg.org/badges/version/VBTree)](https://cran.r-project.org/package=VBTree)
[![Total RStudio Cloud Downloads](http://cranlogs.r-pkg.org/badges/grand-total/VBTree?color=brightgreen)](https://cran.r-project.org/package=VBTree)
[![RStudio Cloud Downloads](http://cranlogs.r-pkg.org/badges/VBTree?color=brightgreen)](https://cran.r-project.org/package=VBTree)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Vector Binary Tree: a Database Technique for Efficient Data Group Processing

## Brief Introduction

The R-package name, **VBTree**, abbreviated from vector binary tree, is a data structure designed for providing an easy and efficient approach for data group processing.

As we know, for many tasks with group attribute such as data visualization for specific conditions, data workers in R need to do much preliminary operations from raw data. According to different needs, the raw data should be melted or reshaped into a data frame with the proper styel, which steps are usually frustrating and tedious. Especially, if the raw data is of a large scale, these operations will further increase its object size then make the project inefficient.

**VBTree** is aimed to build a logical mapping between your raw data and your projects, through correctly setting column names of your raw data. Now you can imagine a data frame with many but limited columns, and with millions of rows, all your management on that data can be implemented just by reading its column names. The imported column names are saved in forms of vector binary tree, double list and array (tensor), which three can be convertible mutually. For different task objectives, you can use the rules in **VBTree** to build your code for telling your program how to extract your desired subset from the raw data.

We assume there is a data with 10 variables and 2 levels for each variable, if all combinations will be listed fully into columns, it must be 1024 columns in your data frame. For data visualization, non-numeric varaibles will be melted which makes the data frame further increased in object size. While as for management methods in **VBTree**, most of your requirements in data processing will be achieved, by constructing a vector binary tree with the structure of only 10 layers and 2 levels in each layer.

## Installation

### Installation from CRAN:

```
install.packages("VBTree")
```

### Installation from github:

```
library(devtools)
devtools::install_github("CubicZebra/VBTree")
```

## Contact

Author: Chen ZHANG 

Mail: chen.zhang_06sept@foxmail.com
