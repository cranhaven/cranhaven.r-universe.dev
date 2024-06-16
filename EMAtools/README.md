
<!-- README.md is generated from README.Rmd. Please edit that file -->
EMAtools
========

EMAtools is an early-release set of tools that researchers who use real-time monitoring/EMA/ESM/micro-longitudinal data will find useful.

What can you do now with EMAtools?
----------------------------------

There are currently three tools:

**pcenter**, which allows you to center momentary data on participant means (also called "centering within clusters" or "centering within groups").

**eventmerge**, which allows merging event-level data into momentary datasets where you need the events to correspond to the nearest datapoint in the momentary data. For example, if you have momentary data about affect reported throughout the day and have participants initiate a survey whenever they engage in a certain behavior and want to have a column indicating whether or not the behavior occured after each momentary prompt (and before the next one). Right now this only works with data from the Mobile EMA platform, however other platforms (e.g., Movisens) will be added in the future.

**ema.powercurve**, which allows you to create power curves for multi-level data. These curves can be very useful for feasibility sections of grant applications.

How do I install EMA tools?
---------------------------

`install.packages("EMAtools")`
