\name{rec}
\alias{rec}
\title{Reverse engineering censored and decoupled data}
\usage{
rec(X, Y, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, 
    seed.matrix, seed.estimation.method)
}


\arguments{
\item{X}{Argument can be an average, a univariate frequency table, or a censored contingency table.  The average value should be a numeric class while a data.frame or matrix are acceptable table classes.  Y defaults to NULL if X argument is a censored contingency table.  See Details section below formatting.}

\item{Y}{Same description as X but this argument is for the Y variable.  X defaults to NULL if Y argument is a censored contingency table.}

\item{Xlowerbound}{A numeric class value to represent the left bound for X (row in contingency table).  The value must strictly be a non-negative integer and cannot be greater than the lowest category/average value provided for X (e.g. the lower bound cannot be 6 if a table has '< 5' as a X or row category).} 

\item{Xupperbound}{A numeric class value to represent the right bound for X (row in contingency table).  The value cannot be less than the highest category/average value provided for X (e.g. the upper bound cannot be 90 if a table has '> 100' as a X or row category).}

\item{Ylowerbound}{Same description as Xlowerbound but this argument is for Y (column in contingency table).} 

\item{Yupperbound}{Same description as Xupperbound but this argument is for Y (column in contingency table).} 

\item{seed.matrix}{An initial probability matrix to be updated.  If decoupled variables is provided the default is a Xlowerbound:Xupperbound by Ylowerbound:Yupperbound matrix with interior cells of 1, which are then converted to probabilities.  If a censored contingency table is provided the default is the seedmatrix()\$Probabilities output.}

\item{seed.estimation.method}{A character string indicating which method is used for updating the seed.matrix. The choices are: "ipfp", "ml", "chi2", or "lsq". Default is "ipfp".}  

}

\value{
The output is a list containing an uncensored contingency table of probabilities (rows range from Xlowerbound:Xupperbound and the columns range from Ylowerbound:Yupperbound) as well as the row X and column Y parameters used in making the margins for the mipfp R package. 
}

\description{
rec is a statistical approach that estimates what "true" uncensored bivariate table could have been given summarized information.  Note, there are assumptions used in this function.  First, rec relies on a Poisson distribution when a user only provides an average, which is assuming the variance and average of that variable are equal.  A more descriptive input variable(s), such as a decoupled univariate table(s) or a censored frequency table, can account for dispersion found in data.  However, independence between decoupled variables still has to be assumed when there is no external information about the joint distribution.  Because of these assumptions, rec also provides two options for sensitivity analysis: seed matrix the method used in updating the seed matrix are both arbitrary inputs.  For more information it is recommended for a user to read the Details section below and more information can be found in the vignettes.

}


\details{


\strong{Overview:}\cr
The rec function handles four cases.

\itemize{

\item \emph{\strong{Case I.}} When provided an average for both X and Y, the averages represent lambda values.  These lambdas create truncated Poisson X and Y probability densities for uncensored vectors ranging from Xlowerbound:Xupperbound and Ylowerbound:Yupperbound, respectively.  The Xlowerbound:Xupperbound vector with its corresponding density values represents the new row marginal.  The Ylowerbound:Yupperbound vector with its corresponding density values represents the new column marginal.  This is a decoupled case, and thus the seed (initial cross tabulation matrix to be updated) defaults to (a matrix of ones)/(length(Xlowerbound:Xupperbound) * length(Ylowerbound:Yupperbound)).  The mipfp R package then estimates cross tabulations with a selected seed estimation method, new uncensored marginals, and seed matrix.  The final result is an uncensored contingency table of probabilities. 

\item \emph{\strong{Case II.}} When provided an univariate frequency table for both X and Y, the negative binomial average and dispersion parameters are estimated with a customized maximum likelihood function.  These parameters then create truncated negative binomial X and Y probability densities for uncensored vectors, Xlowerbound:Xupperbound and Ylowerbound:Yupperbound, respectively.  The methods listed in Case I are then implemented.    

\item \emph{\strong{Case III.}} When provided a combination of an average and frequency table (X and Y could be either), the same methods stated in Case I and II are implemented. 

\item \emph{\strong{Case IV.}} When provided a censored X*Y contingency table, the row marginals create a univariate X frequency table while the column marginals create a univariate Y frequency table.  Both tables estimate there corresponding negative binomial average and dispersion parameters.  These parameters then create truncated negative binomial X and Y probability densities for uncensored vectors Xlowerbound:Xupperbound and Ylowerbound:Yupperbound, respectively.  The Xlowerbound:Xupperbound vector with its corresponding density values represents the new row marginal.  The Ylowerbound:Yupperbound vector with its corresponding density values represents the new column marginal.  This is not a decoupled case, and thus the default seed repeats the probability cells, which corresponding to the censored contingency table, for the newly created and compatible uncensored cross tabulations and then makes the matrix equal 1 (i.e. for each cell value j in the seed: seed[j]/sum(seed)).  The mipfp R package then estimates cross tabulations with a selected seed estimation method, new uncensored marginals, and seed matrix.  The final result is an uncensored contingency table of probabilities.  

}

\strong{Table Format:}\cr
The table(s) for Case II and III has restrictions. The univariate frequency table, which can be a data.frame or matrix class, must have two columns and n number of rows.  The categories must be in the first column with frequencies or probabilities in the second column.  Row names should never be placed in this table (the default row names should always be 1:n).  Column names can be any character string.  The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G).  Also, calculations use closed intervals.    

\itemize{
\item left censoring: <, L, <=, LE
\item interval censoring: - or I (symbol has to be placed in the middle of the two category values)
\item right censoring: >, >=, +, G, GE
\item uncensored: no symbol (only provide category value)
}

Below are three correctly formatted tables.  

\tabular{cc}{
    Category\tab Frequency\cr
    <=6\tab 11800 \cr
    7-12\tab 57100 \cr
    13-19\tab 14800 \cr
    20+\tab 3900 \cr
  }
  
  \tabular{cc}{
    Category\tab Frequency\cr
    LE6\tab 11800 \cr
    7I12\tab 57100 \cr
    13I19\tab 14800 \cr
    GE20\tab 3900 \cr
  }
  
    \tabular{cc}{
    Category\tab Frequency\cr
    <7\tab 11800 \cr
    7I12\tab 57100 \cr
    13-19\tab 14800 \cr
    >=20\tab 3900 \cr
  }
  
  
The table for Case IV also has restrictions.  The censored symbols should follow the requirements listed above.  The table's class can be a data.frame or a matrix.  The column names should be the Y category values. The first column should be the X category values and the row names can be arbitrary.  The inside of the table are X * Y frequency values, which are either nonnegative frequencies or probabilities if seed_estimation_method is "ipfp" or strictly positive when method is "ml", "lsq" or "chi2".  The row and column marginal totals corresponding to their X and Y category values need to be placed in this table. The top left, top right, and bottom left corners of the table should be NA or blank.  The bottom right corner can be a total cross tabulation sum value, NA, or blank. The table below is a formatted example.

  \tabular{ccccc}{
    NA \tab <20 \tab 20-30 \tab >30\tab NA\cr
    <5 \tab 18 \tab 19 \tab 8\tab 45\cr
    5-9 \tab 13 \tab 8 \tab 12\tab 33\cr
    >=10 \tab 7 \tab 5 \tab 10\tab 21\cr
    NA \tab 38 \tab 32 \tab 31\tab NA\cr
  }


\strong{Bounds:}\cr
Ideally, the four bounds should be chosen based off prior knowledge and expert elicitation, but they can also be selected intuitively with a brute force method.  If rec outputs a final contingency table with higher probabilities near the edge(s) of the table, then it would make sense to increase the range of the bound(s).  For both the X and Y variables, this would just involve making the lower bound less, making the upper bound more, or doing a combination of the two.  The opposite holds true as well.  If the final contingency table has very low probabilities near the edge(s) of the table, then a user should decrease the range of the particular bound(s).  


\strong{Seed Estimation Methods:} \cr
This function implements the \emph{mipfp} R package, which offers four methods to estimate cross tabulations when provided fixed marginals.  

\tabular{cc}{
  
\emph{\strong{Method}} \tab \emph{\strong{Abbreviation}} \cr

Iterative proportional fitting procedure \tab ipfp \cr

Maximum likelihood method\tab ml \cr

Minimum chi-squared\tab chi2 \cr 

Weighted least squares \tab lsq \cr
}

For a summary and understanding of all methods please refer to the vignettes and/or the papers by Little et al. (1991) and Suesse et al. (2017).  
}

\examples{
  # provide two averages
  # seed.matrix defaults to a matrix of ones
  # seed.estimation.method defaults to ipfp
  twoaverages.results<-rec(
     X= 4.4,
     Y = 571.3,
     Xlowerbound = 1,
     Xupperbound = 20,
     Ylowerbound = 520,
     Yupperbound = 620)
  
  
  # provide one average and one table
  # create a censored univariate table
  # seed.matrix defaults to a matrix of ones
  # seed.estimation.method defaults to ipfp
  Y.table = cbind(as.character(c("<7", "7-12", "13-19", ">19")), 
    c(11800,57100,14800,3900))
  combo.results<-rec(X= 2.3,
     Y = Y.table,
     Xlowerbound = 1,
     Xupperbound = 15,
     Ylowerbound = 1,
     Yupperbound = 30)
   
   
  # provide a censored contingency table 
  contingencytable<-matrix(c(6185,9797,16809,11126,6156,3637,908,147,69,4,
                         5408,12748,26506,21486,14018,9165,2658,567,196,78,
                         7403,20444,44370,36285,23576,15750,4715,994,364,136,
                         4793,17376,44065,40751,28900,20404,6557,1296,555,228,
                         2354,11143,32837,33910,26203,19301,6835,1438,618,245,
                         1060,6038,19256,21298,17774,13864,4656,1039,430,178,
                         273,2521,9110,11188,9626,7433,2608,578,196,112,
                         119,1130,4183,5566,5053,3938,1367,318,119,66,
                         33,388,1707,2367,2328,1972,719,171,68,37,
                         38,178,1047,1672,1740,1666,757,193,158,164),
                           nrow=10,ncol=10, byrow=TRUE)
  rowmarginal<-apply(contingencytable,1,sum)
  contingencytable<-cbind(contingencytable, rowmarginal)
  colmarginal<-apply(contingencytable,2,sum)
  contingencytable<-rbind(contingencytable, colmarginal)
  row.names(contingencytable)[row.names(contingencytable)=="colmarginal"]<-""
  contingencytable<-data.frame(c("1","2","3","4","5","6", "7", "8","9","10+", NA),
    contingencytable)
  colnames(contingencytable)<-c(NA,"<20","20-29","30-39","40-49","50-69","70-99",
                                "100-149","150-199","200-299","300+", NA)

  # the contingencytable input could be put in X or Y (opposing argument = NULL)
  # X = rows and Y = columns 
  # seed.matrix default = repeating the cross tabulations in the censored contingency
  ## table for the newly created and compatible uncensored cross tabulations
  # seed.estimation.method defaults to ipfp
  contingencytable.results<-rec(
     X= contingencytable,
     Xlowerbound = 1,
     Xupperbound = 15,
     Ylowerbound = 10,
     Yupperbound = 310)
}

\keyword{Poisson}
\keyword{negative binomial}
\keyword{univariate table}
\keyword{frequency table}
\keyword{count data}
\keyword{censored}
\keyword{dispersion}
\keyword{truncated}
\keyword{contingency}


\references{

Frederick Novomestky and Saralees Nadarajah (2016). truncdist: Truncated Random Variables. R package
version 1.0-2. https://CRAN.R-project.org/package=truncdist

Johan Barthelemy and Thomas Suesse (2018). mipfp: Multidimensional Iterative Proportional Fitting and Alternative Models. R package version 3.2. https://CRAN.R-project.org/package=mipfp 
  
Little, R. J., Wu, M. M. (1991) Models for contingency tables with known margins when target and sampled populations differ. \emph{Journal of the American Statistical Association}, 86(413): 87-95. doi: https://doi.org/10.2307/2289718

Suesse, T., Namazi-Rad, M., Mokhtarian, P., & Barthelemy, J. (2017). Estimating Cross-Classified Population Counts of Multidimensional Tables: An Application to Regional Australia to Obtain Pseudo-Census Counts, \emph{Journal of Official Statistics}, 33(4), 1021-1050. doi: https://doi.org/10.1515/jos-2017-0048
}
