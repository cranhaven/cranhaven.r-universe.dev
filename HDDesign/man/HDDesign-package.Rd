\name{HDDesign-package}
\alias{HDDesign-package}
\alias{HDDesign}
\docType{package}
\title{
Sample Size Calculation for High Dimensional Classification Study

}
\description{
This package facilitates the design of studies employing high dimensional features for binary classification.  
The package assumes that the study will build a linear classifier by first screening features and selecting those that appear important for classification, and then forming a linear predictor based on the selected features.  
The package implements functions to 
1) determine the asymptotic feasibility of the classification problem; 
2) compute the upper bounds of the PCC for any linear classifiers; 
3) estimate the PCC of three design methods given design assumptions; 
4) determine the sample size requirement to achieve the target PCC for various design methods. 

}
\details{
\tabular{ll}{
Package: \tab HDDesign\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2016-04-26\cr
License: \tab GPL-2\cr
}
Design of high-dimensional classification studies involves several aspects. Firstly, we need to consider the feasibility of the classification. 
For high dimensional classification, the important signals are sometimes
so sparse and weak that the performance of any linear classifiers is no better than a random assignment classifier.
We implement functions to determine the asymptotic feasibility of the classification problem based on the theory 
of the rare and weak model (Donoho and Jin 2009). If the classification is feasible, we need to determine the appropriate target PCC to calibrate the sample size calculation.
The lower bound of the PCC corresponds to the worst case scenario where
the classifier performs as poor as the random assignment classifier. 
Then the lower bound of the PCC is the prevalence of the dominating group.
The upper bound of the PCC corresponds to the best case scenario where 
the classifier performs as good as the ideal classifier which uses full knowledge
of the data generating mechanism by Dobbin and Simon (2007). 
We implement a function to compute the PCC of the ideal classifier. 
Then  the target PCC can be chosen between the lower and upper bounds, depending
on the budget and other constraints of the study. Furthermore, we need to incorporate the uncertainty in feature selection into our sample size calculations in the design stage to ensure the target PCC is achieved at the analysis stage. 
We have implemented the following feature selection methods: 
a procedure proposed by Dobbin and Simon (2007), 
thresholding by cross validation,
and Higher Criticism Threshold (HCT) by Donoho and Jin (2009). We implement an efficient algorithm to calculate the sample size required when features are iid, for the case when HCT
is used to build the classifier. The CV method is more computationally intensive. We also adapt the approaches for the cases when features are correlated, however the resulting sample sizes will often be conservative.


In this package we use the following assumptions and notations. We assume features have the same variance across groups, and for simplicity assume all variances equal 1. 
If the differences between the means of a feature between the groups is zero,
then this feature does not differentially express between
the groups and it is therefore not important for the purpose of classification. 
If, however, the difference is non zero, this feature is important and its effect size 
is half of the absolute value of the difference. We denote the effect size by mu0, the total
number of features by p; the number of important features by m; and the total sample size for
two groups by n; the prevalence of "group 1" in the population by p1 and the prevalence of "group -1" by 1-p1. Finally, pvalues for all pairwise associations are derived from the t-distribution instead of a normal distribution to take into account the fact that some studies may have a small sample size. 

In the example below, we will illustrate how to use the functions implemented in this
package to address the sample size calculation problem for studies employing high dimensional 
features for classification.


}
\author{
Meihua Wu <meihuawu@umich.edu>
Brisa N. Sanchez <brisa@umich.edu>
Peter X.K. Song <pxsong@umich.edu> 
Raymond Luu <raluu@umich.edu>
Wen Wang <wangwen@umich.edu>
}
\references{
Donoho, D, and Jin, J. (2009). "Feature Selection by Higher Criticism Thresholding Achieves the Optimal Phase Diagram." Philosophical Transactions. Series A, Mathematical, Physical, and Engineering Sciences 367 (1906) : 4449-4470.

Dobbin, K.K., and Simon R.M. (2007). "Sample Size Planning for Developing Classifiers Using High-dimensional DNA Microarray Data." Biostatistics 8 (1): 101-117. 

Sanchez, B.N., Wu, M., Song, P.X.K., and Wang W. (2016). "Study design in high-dimensional classification analysis." Biostatistics, in press.
}
\keyword{ package }

\examples{
# Consider the following design scenario:
# Prevalence of Group 1
p1=0.5
# Effect size
mu0=0.4
# Total number of features
p=500
# The number of important features
m=10


# Step 1: Feasibility of the classification for a study with about 100 individuals
which.region(mu0=mu0, p=p, m=m, n=100)
# return 4, indicating the classification belongs to the feasible region. 

# Step 2: Upper bound of the PCC
ideal_pcc(mu0=mu0, m=m, p1=p1)
# return 0.8970484, 
# So the target PCC can be chosen between 0.5 and 0.8970484.

# Step 3: Obtain the sample requirement for target PCC equal to 0.8
#Use method proposed by Dobbin and Simon (2007)
set.seed(1)
samplesize(target=0.8, nmin=20, nmax=100, ds_method, mu0=0.4, p=500, m=10)
#return sample size n=66

#Use cross validation(commented due to long waiting time)
#set.seed(1)
#samplesize(target=0.8, nmin=20, nmax=100, cv_method, mu0=0.4, p=500, m=10, 
#alpha_list=10^((-10):(-2)), nrep=100) 
#alpha_list should be a dense list of p-value cutoffs; 
#here we only use a few values to ease computation of the example.
#return sample size n=78.

#Use HCT 
set.seed(1)
samplesize(target=0.8, nmin=20, nmax=100, hct_method, mu0=0.4, p=500, m=10, 
hct=hct_beta, alpha0=0.5, nrep=100) 
#return sample size n=78.

}
