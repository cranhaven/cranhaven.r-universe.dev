\name{VariableSelection}
\alias{VariableSelection}
\title{Selecting the best set of relevant environmental variables along
  with the optimal regularization multiplier for Maxent Niche Modeling}

\description{This is the core function of the package in which a set of
  environmental variables is reduced in a stepwise fashion in order to
  avoid overfitting the model to the occurrence records. This can be
  done for a range of regularization multipliers. The best performing
  model, based on AICc values (Akaike, 1974) or AUC.Test values (Fielding
  and Bell, 1997), identifies then the most-important uncorrelated
  environmental variables along with the optimal regularization
  multiplier.
}
\usage{

VariableSelection(maxent, outdir, gridfolder, occurrencelocations,
backgroundlocations, additionalargs, contributionthreshold,
correlationthreshold, betamultiplier)

}
\arguments{
  \item{maxent}{String specifying the filepath to the maxent.jar file (download from here: https://www.cs.princeton.edu/~schapire/maxent/). The package was tested with maxent.jar version 3.3.3k.
  }
  \item{outdir}{String specifying the path to the output directory to
    which all the result files will be written.Please don't put
    important files in this folder as all files but the output files of
    the VariableSelection function will be deleted from this folder.
  }
  \item{gridfolder}{String specifying the path to the directory that holds all the ASCII grids (in ESRI's
    .asc format) of environmental variables. All variables must have the same extent and resolution.
  }
  \item{occurrencelocations}{String specifying the filepath to the csv
    file with occurrence records. Please find the exact specifications of
    the SWD file format in the details section below.
  }
  \item{backgroundlocations}{String specifying the filepath to the
    csv file with background/pseudoabsence data. Please find the exact
    specifications of the SWD file format in the details section below.
  }
  \item{additionalargs}{String specifying additional maxent
    arguments. Please see in the details section below. 
  }
  \item{betamultiplier}{Vector of beta (regularization
    multipliers) (positive numerical values). The smaller this value, the
    more closely will the projected distribution fit to the training data
    set. Overfitted models are poorly transferable to novel environments
    and, thus, not appropriate to project distribution changes under
    environmental change. The model performance will be compared between
    models created with the beta values given in this \code{betamultiplier}
    vector. Thus, providing a range of beta values from 1 (the default in
    Maxent) to 15 or so, will help you to spot the optimal beta multiplier
    for your specific model.
  }
  \item{correlationthreshold}{Numerical value (between 0
    and 1) that sets the threshold of Pearson's correlation coefficient above
    which environmental variables are regarded to be correlated (based on
    values at all background locations). Of the
    correlated variables, only the variable with the highest
    contribution score will be kept, all other correlated
    variables will be excluded from the Maxent model.
    Correlated variables should be removed because they may reflect the
    same environmental conditions, and can lead to overly complex or
    overpredicted models. Also, models comiled with correlated variables might give wrong predictions in scenarios where the
    correlations between the variables differ.
  }
  \item{contributionthreshold}{Numerical value
    (between 0 and 100) that sets the threshold of model contributions
    below which environmental variables are excluded from the Maxent
    model. Model contributions reflect the importance of
    environmental variables in limiting the distribution of
    the target species.
  }
}

\details{

  For further details on the model selection process and the variable
  settings, please have a look at the vignette that comes with this
  package. 
}

\value{
  The following result files are saved in the directory specified with the \code{outdir}
  argument. 
  \item{ModelPerformance.txt}{A table listing the performance indicators
    of all created Maxent models
    \describe{
      \item{\code{Model}}{Unique model number}
      \item{\code{betamultiplier}}{Maxent's regularization multiplier}
      \item{\code{variables}}{Number of environmental variables}
      \item{\code{samples}}{Number of occurrence sites}
      \item{\code{parameters}}{Number of parameters estimated from the model}
      \item{\code{loglikelihood}}{log likelihood value}
      \item{\code{AIC}}{Akaike Information Criterion}
      \item{\code{AICc}}{sample size corrected AIC}
      \item{\code{BIC}}{Bayesian information criterion}
      \item{\code{AUC.Test}}{Area under the receiver operating
	characteristic fro test data}
      \item{\code{AUC.Train}}{Area under the receiver operating
	characteristic fro training data}
      \item{\code{AUC.Diff}}{Difference between AUC.Test and AUC.Train}
    }
    The information criteria (AIC, AICc, and BIC) are set to 'x' if the
    number of parameters is lower than the number of variables in the model.
  }
  
  
  \item{ModelSelectionAICc_MarkedMaxAUCTest.png}{
    A figure showing the AICc
    values of all models, which are ordered along the x-axis based on the applied
    beta-multiplier. The number of environmental variables included in
    each model is coded by dot color and size. The model with highest
    AUC.Test value is marked in red.
  }
  
  \item{ModelSelectionAICc_MarkedMinAICc.png}{ 
    A figure showing the AICc
    values of all models, which are ordered along the x-axis based on the applied
    beta-multiplier. The number of environmental variables included in
    each model is coded by dot color and size. The model with highest
    minimum AICc value is marked in red. 
  }
  
  \item{ModelSelectionAUCTest_MarkedMaxAUCTest.png}{
    A figure showing the AUC.Test
    values of all models, which are ordered along the x-axis based on the applied
    beta-multiplier. The number of environmental variables included in
    each model is coded by dot color and size. The model with highest
    AUC.Test value is marked in red.
  }
  
  \item{ModelSelectionAUCTest_MarkedMinAICc.png}{
    A figure showing the AUC.Test
    values of all models, which are ordered along the x-axis based on the applied
    beta-multiplier. The number of environmental variables included in
    each model is coded by dot color and size. The model with highest
    minimum AICc value is marked in red.
  }
  
  \item{ModelWithMaxAUCTest.txt}{
    Subset of the table \code{ModelPerformance.txt}, which shows only the
    model with the highest AUC.Test value.
  }
  
  \item{ModelWithMinAICc.txt}{
    Subset of the table
    \code{ModelPerformance.txt}, which shows only the model with the
    lowest AICc value.
  }
  
  
  \item{VariableSelectionProcess.txt}{Table listing model contributions for
    and correlations between each of the environmental variables for all
    created Maxent models. The numbers of the models refer to the unique
    model numbers in the table \code{ModelPerformance.txt} (see above).
    The following entries describe the content row-wise (not column-wise)
    \describe{
      \item{\code{Test}}{Either 'Contributions' or 'Correlation. Informs if the numbers for each of the
	environmental variables refers to model contribution coefficients
	or to correlation coefficients.
      }
      \item{\code{Model}}{The unique model number (the same unique model
	number as in \code{ModelPerformance.txt.)}
      }
      \item{\code{betamultiplier}}{The (regularization
	multipliers) used to compile the respective model.
      }
      \item{\code{X}}{'X' stands here for the name of an environmental
	variable. The \code{Test} row above informs whether the values in this row refer to the model
	contribution of this environmental variable or to its coefficient
	of correlation with another environmental variable. The variable
	to which it is compared is recognizable by a correlation
	coefficient of 1. If this environmental variable was excluded from
	the model, the value in this row is 'NA', which stands for 'Not
	Available'.'
      }
    }
  }
  \item{VariableSelectionMaxAUCTest.txt}{Subset of
    \code{VariableSelectionProcess.txt} that shows only those models which
    lead directly to the model with the  highest AUC.Test value.
  }
  
  \item{VariableSelectionMinAICc.txt}{Subset of
    \code{VariableSelectionProcess.txt} that shows only those models which
    lead directly to the model with the lowest AICc value.
  }
  
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}

\references{

Akaike H (1974)\cr
A new look at the statistical model identification\cr
\emph{IEEE Transactions on Automatic Control} \bold{19}:6 716--723.

Fielding AH and Bell JF (1997)\cr
A review of methods for the assessment of prediction\cr
errors in conservation presence/absence models\cr
\emph{Environmental Conservation} \bold{24}:1 38--49.

}



\author{Alexander Jueterbock, \email{Alexander-Jueterbock@web.de}\cr
%%  ~~who you are~~
}

\section{Warning}{Depending on the number of environmental variables and
  the range of different betamultipliers you want to test, variable
  selection can take several hours so that you might want to run the
  analysis over night.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
# Please find a workflow tutorial in the vignette of this package. It
# will guide you through the settings and usage of the
# 'VariableSelection' function, the core function of this package.
}
\donttest{
VariableSelection(
maxent="C:/.../maxent.jar",
outdir="OutputDirectory",
gridfolder="BioORACLEVariables",
occurrencelocations=system.file("extdata", "Occurrencedata.csv", package="MaxentVariableSelection"),
backgroundlocations=system.file("extdata", "Backgrounddata.csv", package="MaxentVariableSelection"),
additionalargs="nolinear noquadratic noproduct nothreshold noautofeature",
contributionthreshold=5,
correlationthreshold=0.9,
betamultiplier=seq(2,6,0.5)
)
}
}
