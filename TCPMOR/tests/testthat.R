# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(TCPMOR)

# Generate simulated data dataC
dataC <- createData(200)

# Fit the semi-parametric model
spm.fit <- fitSemiParamModel(dataC)
plot(spm.fit,ylab = "lnOR",xlab = "age",shade = FALSE)
summary(spm.fit)

# Find two cut-off points
dataC <- findCutoffs(spm.fit, dataC)

# Calculate the two cut-off points after limiting sensitivity se and specificity sp
result <- calculateCutoffs(dataC)
cutoffs <- result$cutoffs
dataC2 <- result$filteredData
print(cutoffs)

# Discretize age variable based on the two cutoffs
dataC2 <- discretizeAge(dataC2, cutoffs)

# Fitting logistic regression models and obtaining OR values and 95% confidence intervals
OR_Results <- fitLogisticRegression(dataC2)
print(round(OR_Results, 3))
