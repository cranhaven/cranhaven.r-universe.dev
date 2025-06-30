### R code from vignette source 'vignette.Rnw'

###################################################
### code chunk number 1: vignette.Rnw:41-53
###################################################
options(width = 65)
# load necessary packages, assuming they have already been installed on the system
library(waterData)
library(survival)
library(seawaveQ)
# load example data that comes with the package
data(swData)
# show first few rows of water-quality data for Missouri River at Omaha, Nebr.
head(qwMoRivOmaha)
# get a description of the data including definitions of the columns
# by viewing the help documentation
?qwMoRivOmaha


###################################################
### code chunk number 2: vignette.Rnw:62-64
###################################################
# scatter plot showing quantified, estimated, and censored  values
cenScatPlot(qwMoRivOmaha, pname = "04035")


###################################################
### code chunk number 3: vignette.Rnw:71-86
###################################################
# scatter plot with many additional plotting arguments
# these options provide a plot closer to the plotting standards
# of the U.S. Geological Survey, however, these plots may not 
# meet all U.S. Geological Survey publication requirements
par(las = 1, tcl = 0.5)
cenScatPlot(qwMoRivOmaha, pname = "04035",
            site = "06610000 Missouri River at Omaha, Nebraska",
            ylabel = "Simazine concentration, in micrograms per liter",
            legcex = 0.7, qwcols = c("R", "P"), ylim = c(0,0.1), yaxs = "i", 
            cex.lab = 0.9, cex.axis = 0.9, xaxs = "i", xaxt = "n",
            xlim = c(as.Date("1996-01-01"), as.Date("2004-01-01")))
axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", "2002-01-01", 
             "2004-01-01")
axis(1, as.Date(axdates), labels = c("1996", "1998", "2000", "2002", "2004"), 
     cex.axis = 0.9)


###################################################
### code chunk number 4: vignette.Rnw:93-99
###################################################
data(swData)
# show last few rows of water-quality data for Missouri River at Omaha, Nebr.
tail(cqwMoRivOmaha)
# get a description of the data including definitions of the columns
# by viewing the help documentation
?cqwMoRivOmaha


###################################################
### code chunk number 5: vignette.Rnw:108-113
###################################################
data(swData)
MoRivOmaha <- combineData(qwdat = qwMoRivOmaha, cqwdat = cqwMoRivOmaha,
qwcols = c("staid", "dates", "R", "P"))
# view combined data set
head(MoRivOmaha)


###################################################
### code chunk number 6: vignette.Rnw:121-135
###################################################
data(swData)

# associate continuous water-quality data with each sample
# combineData does this for you
modMoRivOmaha <- combineData(qwdat=qwMoRivOmaha, cqwdat=cqwMoRivOmaha)

# then fit model(s)
myfitLinearTrend <- fitswavecav(cdat = modMoRivOmaha, cavdat = cqwMoRivOmaha, 
                                tanm = "myfitLinearTrend", 
                                pnames = c("04035", "04037", "04041"), 
                                yrstart = 1995, yrend = 2003, tndbeg = 1995, 
                                tndend = 2003, iwcav = c("flowa30", "flowa1"), 
                                dcol = "dates", qwcols = c("R", "P"), mclass = 1,
                                plotfile = FALSE)


###################################################
### code chunk number 7: vignette.Rnw:142-161
###################################################
# get the first element of the list for each model/constituent combination
# the data frame with information about each model/constituent combination
myfitLinearTrend[[1]]

# get the second element of the list for each model/constituent combination
# the survival regression summary for each model/constituent combination
myfitLinearTrend[[2]]

# get the first few lines of the third element of the list
head(myfitLinearTrend[[3]])

# get the first few lines of the fourth element of the list
head(myfitLinearTrend[[4]])

# get the summary of predicted concentrations
myfitLinearTrend[[5]]

# get summary of trend results
myfitLinearTrend[[6]]


###################################################
### code chunk number 8: vignette.Rnw:168-172
###################################################

attributes(myfitLinearTrend[[2]][[1]])
myfitLinearTrend[[2]][[1]]$n
myfitLinearTrend[[2]][[1]]$table


###################################################
### code chunk number 9: vignette.Rnw:201-214
###################################################
data(swData)

# associate continuous water-quality data with each sample
# combineData does this for you
modMoRivOmaha <- combineData(qwdat = qwMoRivOmaha, cqwdat = cqwMoRivOmaha)

# then fit model
myfitRCS <- fitswavecav(cdat = modMoRivOmaha, cavdat = cqwMoRivOmaha, 
                        tanm = "myfitRCS", pnames = c("04035"), yrstart = 1995, 
                        yrend = 2003, tndbeg = 1995, tndend = 2003, 
                        iwcav = c("flowa30", "flowa1"), dcol = "dates", 
                        qwcols = c("R", "P"), mclass = 2, numk = 4, 
                        plotfile = FALSE)


###################################################
### code chunk number 10: vignette.Rnw:219-238
###################################################
# get the first element of the list for each model/constituent combination
# the data frame with information about each model/constituent combination
myfitRCS[[1]]

# get the second element of the list for each model/constituent combination
# the survival regression summary for each model/constituent combination
myfitRCS[[2]]

# get the first few lines of the third element of the list
head(myfitRCS[[3]])

# get the first few lines of the fourth element of the list
head(myfitRCS[[4]])

# get the summary of predicted concentrations
myfitRCS[[5]]

# get summary of trend results
myfitRCS[[6]]


###################################################
### code chunk number 11: vignette.Rnw:242-245
###################################################
attributes(myfitRCS[[2]][[1]])
myfitRCS[[2]][[1]]$n
myfitRCS[[2]][[1]]$table


###################################################
### code chunk number 12: vignette.Rnw:262-269
###################################################
# myfitRCSTrend <- fitswavecav(cdat = modMoRivOmaha, cavdat = cqwMoRivOmaha,
#                             tanm = "myfitRCSTrend", 
#                             pnames = c("04035", "04037", "04041"), 
#                             yrstart = 1995, yrend = 2003, tndbeg = 1995, 
#                             tndend = 2003, iwcav = c("flowa30", "flowa1"), 
#                             dcol = "dates", qwcols = c("R", "P"), mclass = 2, 
#                             numk = 4, bootRCS = TRUE, nboot = 1000, plotfile=FALSE)


