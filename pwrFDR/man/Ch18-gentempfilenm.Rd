\name{gentempfilenm}
\alias{gentempfilenm}
\title{
  Generate a tempfile name
}
\description{
  Generates a tempfile name with an optional user specified prefix and suffix
  Result is a character string
}
\usage{
  gentempfilenm(prfx = "temp", sfx = ".txt")
}
\arguments{
  \item{prfx}{
  prefix for the file name, e.g. "temp"
  }
  \item{sfx}{
  suffix (file extension) for the file name, e.g. ".txt"
  }
}
\value{
  a character string containing the randomly generated name of the
  tempfile.
}
\author{
  Grant Izmirlian izmirlig at mail dot nih dot gov
}
