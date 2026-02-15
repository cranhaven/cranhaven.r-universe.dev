# EventDetectGUI
## General Info
[EventDetectR](https://frehbach.github.io/EventDetectR/) is an R-package for detecting/classifiying events in time-series data.
It aims to combine multiple well-known R-packages like the forecast package to deliver an easily configurable tool for event detection. The EventDetectGUI package is a graphical user interface, created to make the access and use of the EventDetectR package even easier. 

The main documentation for the GUI will be collected on the EventDetectR [GitHub Site](https://github.com/frehbach/EventDetectR/) and the [Package Website](https://frehbach.github.io/EventDetectR/).


[![Build Status](https://travis-ci.org/frehbach/EventDetectGUI.svg?branch=master)](https://travis-ci.org/frehbach/EventDetectGUI)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/frehbach/EventDetectGUI?branch=master&svg=true)](https://ci.appveyor.com/project/frehbach/EventDetectGUI)
[![codecov](https://codecov.io/gh/frehbach/EventDetectGUI/branch/master/graph/badge.svg)](https://codecov.io/gh/frehbach/EventDetectGUI)


## Installation
The package can be installed directly from github:

```R
require(devtools)
install_github("frehbach/EventDetectGUI")
```

The package will soon be available on CRAN and can be installed:

```R
install.packages('EventDetectGUI')
```
## Usage
You need only one function to start and run the EventDetectGUI:

```R
runGUI()
```
