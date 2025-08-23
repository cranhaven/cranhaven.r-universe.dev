\name{plot_prop_NA}
\alias{plot_prop_NA}
\title{
Visual representation of NAs proportion over space and time}
\description{
The function allows to visualize spatial proportion of NAs and temporal proportion of NAs.
}
\usage{
plot_prop_NA(data, variable, time = FALSE, interactive = FALSE, bar = FALSE, map = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataset of class 'data.frame'. Specify the dataset obtained from the retrieving and the \code{merge_data} functions.}

  \item{variable}{
character. Specify the name of the variable for which to obtain the NAs proportion.}

  \item{time}{
Logical. By default set to \code{FALSE}. If the temporal proportion of NAs is required set the argument to \code{TRUE}.}

  \item{interactive}{
Logical. By default set to \code{FALSE}. If interactive plot set the argument to \code{TRUE}.
}

  \item{bar}{
Logical. By default set to \code{FALSE}. If set to \code{TRUE} a bar plot will be showed.}

  \item{map}{
Logical. By default set to \code{FALSE}. If set to \code{TRUE} a map will be showed.}

}
\value{
Object of class \code{gg}, \code{ggplot}
}

\author{
Lorena Ricciotti
}
