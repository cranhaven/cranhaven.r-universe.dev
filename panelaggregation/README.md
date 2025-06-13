panelaggregation
================

An R Package to aggregate Panel Data


Description
-----------
The term panel data refers to a type of dataset which contains multiple variables measured over time(See also: http://en.wikipedia.org/wiki/Panel_data). Panel datasets occur in fields of research that do longitudinal studies. Business cycle researchers for example conduct regular surveys among the same set of firms to draw intertemporal comparison and infer on the business cycle. 

Often one or multiple dimensions of the panel dataset are collapsed by aggregation during the analysis. 
Typically observations are summarized by computing means or shares over certain groups. Assume a set of firms is summarized to different sectors of trade: e.g. lots of firms belonging to the service sector are aggregated to a mean number of employees in the service sector over time. 

This package provides useful functions to aggregate panel data and convert aggregation results into standard R time series. The **panelaggregation** package relies on the **data.table** and can be regarded as a more intuitive convenience layer around **data.table**. 

Install the package
-------------------
We're not CRAN ready yet. So the easiest way to go is the **install_github** function from the **devtools** package.

```
library(devtools)
install_github('panelaggregation','mbannert')
```



Example
-------------------
The following section shows a step-by-step example from reading a random example dataset to aggregation by groups and time to storing the results in a PostgreSQL time series database using the **timeseriesdb** R package.
If you installed the package you could also run the example by running **demo(agggregation)** on your R console. 

```
# load library and dataset
library(panelaggregation)
data(btsdemo)
head(btsdemo)
# adapt the levels to positive, equal and negative
# in order to suit the naming defaults. other levels work too, 
# but you'd need to specify multipliers in computeBalance then
levels(btsdemo$question_1) <- c("pos","eq","neg")

# compute the weighted shares and display store in wide format 
# to get a basis for further steps
level1 <- computeShares(btsdemo,"question_1","weight",by = c("date_qtrly","group"))

# compute balance, don't have to do much here, because
# 3 items is the default
level1_wbalance <- computeBalance(level1)

# Select a particular grouping combination and a timeseries that 
# should be extracted from the data_table
ts1 <- extractTimeSeries(level1_wbalance,
                  "date_qtrly",
                  list(group = "C"),
                  freq = 4,
                  item = "balance",
                  variable = "question_1")
ts1
# Plot a standard R ts using the plot method for ts
plot(ts1,main = attributes(ts1)$ts_key)

# DON'T RUN
# Optionally you can store the series into a time series db using the
# corresponding package... 
# Note that you can use the attribute to specify a timeseries key dynamically ! 
# library(timeseriesdb)
# con <- dbConnect(PostgreSQL(),user = "yourname",dbname="yourdb",host = "yourhost")
# storeTimeseries("ts1",attributes(ts1)$ts_key,"con")
```



