\name{synthetic}
\alias{synthetic}
\title{Synthetic Experimental Data function}
\usage{
synthetic(parm,sigma)}
\arguments{
  \item{parm}{Values for each parameter needed to solve the Direct Problem.}
  \item{sigma}{Standard Deviation of the synthetic data produced using the Direct Problem solution.}
}
\value{The result will be saved in the result file.}
\description{
The Direct Problem will be solved and the result will be corrupted to simulate experimental error. The user can inform the Standard Deviation error that will be added to the exact solution. Then, the synthetic function will return the data obtained using the equation:

Simulated_Results_with_Error <- Direct_Problem_Results * (1 + sigma).

{\if{html}{\figure{Inverse_Problem_R_Solver_Synt.png}{options: style="width: 25" alt="Figure: Inverse_Problem_R_Solver_Synt.png"}}}
}

\examples{
#
# Configure the name of the folder where each file with the parameter to be changed.
# If the folder is 'wd' then it will not be changed.
# You can change it to another location where your parameter files are.
auxi <- system.file(package = "gripp")
folder_name <- c(auxi,auxi)
#
# Configure the name of each file to be changed in the Direct Problem Solver.
file_name <- c('f1.R','f1.R')
#
# Configure the name of each parameter to be changed.
parm_name <- c('A','B')
#
# Configure alternative method using line number to enter each parameter
# When line_number is zero, then the input method uses keywords.
line_number <-c(0,0)
#
# Configure each parameter type, where:
#   1 means a numeric variable informed as a string, such as parm <- "1.38"
#   2 means a numeric vector informed as a string of numbers separated with a space character.
#     For example, parm <- "1.25 3.4". You must tell which one will be the variable considered.
parm_type <- c(1,1)
#
# Informe the position of the variable to be considered in the problem in the vector.
# If the parameter is a numeric variable, then its position is zero.
parm_vector <- c(0,0)
#
# Configure the name of the folder where the command must be called.
# If the folder is 'wd' then it will not be changed.
# You can change it to another location where your parameter files are.
command_folder <- auxi
#
# This variable means that the Direct Problem is solved using R or outside
# if this is FALSE, the results are to be read from a file.
# When it is TRUE, results will be passed inside R using the variable "results"
isitR <- TRUE
#
# Configure the command to be used to call the Direct Problem Solver.
command <- 'f1.R'
#
# Configure a string to be used to attrib values in the Direct Problem file.
attrib_str <- '<-'
#
# Configure the name of the file with the results obtained by the Direct Problem.
# It must be a single column of values.
# The results are changed at each run by the Direct Problem Solver.
result <- 'result.dat'
#
# Configure the name of the folder where the results can be found after each run.
# If the folder is 'wd' then it will not be changed.
# You can change it to another location where your result file is.
result_folder <- tempdir()
#
parm<- rep(5,3)
synthetic(parm,0.03)
}
