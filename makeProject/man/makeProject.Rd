\name{makeProject}
\alias{makeProject}
\title{Make Project LCFD Framework}
\description{This package was inspired by a stackoverflow.com comment made by Josh Reich.  This package creates an empty framework of files and directories for the "Load, Clean, Func, Do" structure described by Josh.  The concept is simple.  The "main.R" script is called by the user.  It sets global variables and calls all the other scipts necessary.
}
\usage{
makeProject(name = "myProject", path = getwd(), force = FALSE, 
author = "Your Name", email = "yourfault@somewhere.net")
}

\arguments{
  \item{name}{The name of your project}
  \item{path}{The path of where to create the project framework.  A directory with the name of the project will be created here.}
  \item{force}{Force creation of the project, even if files already exist}
  \item{author}{The author's name}
  \item{email}{The author's email}
}
\value{
This function will create:
\item{Parent Directory}{The parent project directory}
\item{main.R}{This is where you set global variables, and call the other scripts.}
\item{data}{An empty data directory to hold your data files.}
\item{code}{A code directory containing the other code files.}
\item{load.R}{This is where you insert code to load in libraries and data files.}
\item{clean.R}{This is where you perform any messy data cleanup.}
\item{func.R}{This is where you write all the functions that you need.}
\item{do.R}{This is where the actual work is done.}
}
\author{
Noah Silverman
\cr
Department of Statistics, UCLA
\cr 
email{noahsilverman@ucla.edu}
}
\keyword{ project } 