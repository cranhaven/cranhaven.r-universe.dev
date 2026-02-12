\name{pmmlTransformations-package}
\alias{pmmlTransformations-package}
\alias{pmmlTransformations}
\docType{package}
\title{
Data Transformations for PMML output
}
\description{
\bold{NOTE}: The functions in this package have been merged into the package \pkg{pmml}, starting with \pkg{pmml} 2.0.0. \pkg{pmmlTransformations} still works with \pkg{pmml} up to and including version 1.5.7, but will not receive any more updates. The examples throughout this package have (commented-out) calls to functions from \pkg{pmml}; if using \pkg{pmmlTransformations}, please use \pkg{pmml} 1.5.7 or older.

This package reads in raw data and allows the user to perform various transformations 
on the input data. The user can then use the derived data to construct models 
and output the model together with data transformations in the Predictive Model Markup
Language (PMML) format through the use of the \bold{pmml} package. 

PMML is an XML-based language which provides a way for applications to define machine learning, statistical and data mining models and to share models between PMML compliant applications. More information about the PMML industry standard and the Data Mining Group can be found at <http://www.dmg.org>. The generated PMML can be imported into any PMML consuming application, such as Zementis Predictive Analytics products, which integrate with web services, relational database systems and deploy natively on Hadoop in conjunction with Hive, Spark or Storm, as well as allow predictive analytics to be executed for IBM z Systems mainframe applications and real-time, streaming analytics platforms.
}
\details{
The general methodology to use this package is to first wrap the data with the \bold{WrapData} function 
and then perform all desired transformations. The model, in PMML format, including the information 
on the transformations executed, can then be output by calling the \bold{pmml} function of the pmml package. 
The \bold{pmml} function in this case has to be given an additional parameter, \bold{transform}, as shown in the 
example below.

This package can also be used as a transformation generator; output just the transformations information
instead of the whole pmml model. To do so, one has to call the pmml function with the WrapData output but
pass in a null value as the model name. An example can be seen in the documentation for the \bold{WrapData} function.

This package does not support boolean dataTypes; only numeric and string data are supported.  
}
\author{
Tridivesh Jena, Zementis, Inc.
}
\references{
  \href{http://www.dmg.org}{PMML home page}
  
  \href{http://dmg.org/pmml/v4-3/Transformations.html}{PMML page describing various possible data transformations}
  
  A. Guazzelli, W. Lin, T. Jena (2012), \emph{PMML in Action: Unleashing the Power  
     of Open Standards for Data Mining and Predictive Analytics}. CreativeSpace 
     (Second Edition) - \href{http://www.amazon.com/dp/1470003244}{Available on Amazon.com}

  T. Jena, A. Guazzelli, W. Lin, M. Zeller (2013). The R pmmlTransformations Package. 
     In \emph{Proceedings of the 19th ACM SIGKDD Conference on Knowledge Discovery 
     and Data Mining}.
}
\keyword{ pmml }
\examples{
# Load the standard iris dataset, already available in the base R package
   data(iris)

# First create the wrapper object
   irisBox <- WrapData(iris)

# Perform a simple z-transformation on the first variable of the dataset: 
# Sepal.Length. By default, the name of the transformed variable is 
# "derived_Sepal.Length". The information of the transformation is added 
# back to the wrapped data object.
   irisBox <- ZScoreXform(irisBox,"1")

# Build a simple lm model 
   fit <- lm(Sepal.Width ~ derived_Sepal.Length + Petal.Length, 
            data=irisBox$data)

# One may now output the model in PMML format using the command below.
# The PMML file will now include the data transformations as well as 
# the model. 
   # library(pmml)
   # fit_pmml <- pmml(fit, transform=irisBox)
}
