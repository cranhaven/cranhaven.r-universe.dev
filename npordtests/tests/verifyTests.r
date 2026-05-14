## The codes to verify the calculated results for npordtests package

library(npordtests)

## This is the code to verify the calculated results for AtTest by using
## the example of Lehmann (1975) to determine whether a certain diagnostic
## test can be interpreted successfully without much psychological training.

data("lehmann")
AtTest(Values~Group,lehmann)

## Results
# Test : Adaptive Test
# data : Values and Group
#
# Statistic = 851
# Mean = 583.1944
# Variance = 6570.726
# Z = 3.303794
# Asymp. p-value = 0.0004769302
#
# Result : Null hypothesis is rejected.
## The results are the same as in Beier and Buning (1997).

# ----------------------------------------------------------------------------------

## The data could not be obtained from the authors.

data("jdata")
FtmTest(Y~X,jdata)

## Results
# ---------------------------------------------------------
# Test : Ferdhiana, Terpstra and Magel Test
# data : Y and X
#
# Statistic = 122.6667
# Mean = 0
# Variance = 3261.63
# Z = 2.147876
# Asymp. p-value = 0.0158618
#
# Result : Null hypothesis is rejected.
# ---------------------------------------------------------
## The results are consistent with our manual solutions.


# ----------------------------------------------------------------------------------

## This is the code to verify the calculated results for JtTest by using
## the fictive example data set of Neuhauser (1998).

data("neuhauser")
JtTest(value~group,neuhauser)

## Results
# Test : Jonckheere-Terpstra Test
# data : value and group
#
# Statistic = 442
# Mean = 300
# Variance = 1716.667
# Z = 3.427247
# Asymp. p-value = 0.0003048675
#
# Result : Null hypothesis is rejected.
## The results are the same as in Neuhauser (1998).

# ----------------------------------------------------------------------------------

## This is the code to verify the calculated results for MjtTest by using
## the fictive example data set of Neuhauser (1998).

MjtTest(value~group,neuhauser)

## Results
# Test : Modified Jonckheere-Terpstra Test
# data : value and group
#
# Statistic = 784
# Mean = 500
# Variance = 6833.333
# Z = 3.435596
# Asymp. p-value = 0.0002956264
#
# Result : Null hypothesis is rejected.
## The results are the same as in Neuhauser (1998).

# ----------------------------------------------------------------------------------

## This is the code to verify the calculated results for TmTest by using
## the fictive example data set of Neuhauser (1998).

TmTest(value~group,neuhauser)

## Results
# Test : Terpstra-Magel Test
# data : value and group
#
# Statistic = 1274
# Mean = 416.6667
# Variance = 84595.73
# Z = 2.947647
# Asymp. p-value = 0.001601012
#
# Result : Null hypothesis is rejected.
## The results are the same as in Terpstra et al. (2003).

# ----------------------------------------------------------------------------------

## This is the code to verify the calculated results for KtpTest by using
## the data is taken from Terpstra et al. (2011).

data("hvwi")
KtpTest(Values~Group,hvwi)

## Results
# Test : KTP Test
# data : Values and Group
#
# Statistic = 56108.28
# Mean = 0
# Variance = 362485627
# Z = 2.94701
# Asymp. p-value = 0.001604316
#
# Result : Null hypothesis is rejected.
## The results are the same as in Terpstra et al. (2011).

# ----------------------------------------------------------------------------------

## This is the code to verify the calculated results for SsTest by using
## the data is taken from Shan et al. (2014).

data("hypertension")
SsTest(rdbp~doseLevel,hypertension)

## Results
#   Test : Shan's S test
#   data : rdbp and doseLevel
#
#   Statistic = 28868
#   Mean = 19929.5
#   Variance = 2585436
#   Z = 5.559014
#   Asymp. p-value = 1.356515e-08
#
#   Result : Null hypothesis is rejected.
## The results are the same as in Shan et al. (2014).

# ----------------------------------------------------------------------------------

## There is no numerical example for GcTest in literature.

GcTest(Y~X,jdata)

## Results
#   Test : Gaur's Gc Test
#   data : Y and X
#
#   Statistic = 0.375
#   Mean = 0
#   Variance = 0.06746032
#   Z = 1.4438
#   Asymp. p-value = 0.0743976
#
#   Result : Null hypothesis is not rejected.
## The results are consistent with our manual solutions.

