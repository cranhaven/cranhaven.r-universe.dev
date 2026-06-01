\name{invprob}
\alias{invprob}
\title{Inverse Problem Solver!}
\usage{invprob(parm_init)}
\arguments{
  \item{parm_init}{Set of values to be considered as initial parameters for the Inverse Problem solution}

}
\value{A vector with the quadratic residue calculated, the parameters estimated and the number of function call needed to solve the inverse problem.}
\description{
This sotfware will minimize a cost function and estimate a set of parameters using a Inverse Problem Solver.
{\if{html}{\figure{Inverse_Problem_R_Solver_IP.png}{options: style="width:25" alt="Figure: Inverse_Problem_R_Solver_IP.png"}}}
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
# Configure the smallest value for each parameter
parm_min <- c(0,0)
#
# Configure the larger value for each parameter
parm_max <- c(2,5)
#
# Configure the name of the folder where the command must be called.
# If the folder is 'wd' then it will not be changed.
# You can change it to another location where your parameter files are.
command_folder <- system.file(package = "gripp")
#
# This variable means that the Direct Problem is solved using R or outside
# if this is FALSE, the results are to be read from a file.
# When it is TRUE, results will be passed inside R using the variable "result"
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
# Configure the name of the file to be used as a target by the cost function.
# The file was built using parm <- c(1,3).
# It is also a single column of values.
target <- 'alvo.dat'
#
# Configure the name of the folder where the target file can be found.
# If the folder is 'wd' then it will not be changed.
# You can change it to another location where your target file is.
target_folder <- auxi
#
# Configure the Inverse Problem Solver to be used:
# 1) GRIPP can use the solver 'GenSA'.
# Then configure the maximum number of function evaluation for the Inverse Problem Solution
# using control <- list(max.time=60)
#
# 2) GRIPP can use the solver 'GA'
# The configure the maximum number of function evaluation for the Inverse Problem Solution
# control <- list(popSize = 50, maxiter = 1000, run = 100)
solver <- 'GenSA'
control <- list(max.time=1)
# Configure the initial value for each parameter
parm <- c(1.5,4)
out<-invprob(parm)
out
}
