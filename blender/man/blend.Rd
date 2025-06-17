\name{blend}
\alias{blend}
\alias{blend.list}
\alias{blend.character}
\alias{print.blended.landscape}
\alias{plot.blended.landscape}
\alias{print.blended.landscape.bundle}
\alias{plot.blended.landscape.bundle}
\alias{blended.landscape}
\alias{blended.landscape.bundle}

\title{
  Calculate biotic homogenization using Jaccard's index and an approximation 
}
\description{
  \code{blend} finds native landscape similarity and exotic species' 
  contributions to landscape homogeneity using average pairwise Jaccard 
  similarity (J.Bar) and an analytical approximation (J.Star) described
  in Harris et al. (2011, "Occupancy is nine-tenths of the law," The American
  Naturalist) and in \link{blender.basics}.
  
  \code{blend} can be called on a \code{list} of \code{data.frame}s or on a
  character vector pointing to a directory containing data as .csv files.
  
  If \code{blend} is called multiple landscapes, it will analyze each one
  individually and then combine the results together in a
  \code{blended.landscape.bundle}, which has its own method for plotting.
}
\usage{
  blend(x, warn = FALSE)
}


\arguments{
\item{x}{
  Either the file path to your landscapes as .csv files (character vector)
  or a \code{list} of \code{data.frames}.  The files or \code{data.frames}
  must be named and structured as described below. On Windows, the
  directory must use either double backslashes or single forward slashes
  to separate directories (e.g., R cannot read "c:\\Users\\Dave\\Data").
}
\item{warn}{
  Logical.  Should \code{blender} warn you if it encounters problems when
  smoothing your data?  Defaults to \code{FALSE}.  
}
}
\details{
  J.Bar, J.Star, and P.Star are defined in Harris et al. (2011) and in 
  the documentation for \link{blender.basics}.
  
  \code{blend} expects a character vector pointing to \code{.csv} files
  on your hard drive or a \code{list} of \code{data.frame}s. 
  
  The files or 
  \code{data.frames} must be named to include a landscape ID (e.g. 
  "Iowa" or "Region 7") before the word "native" or "exotic", separated 
  by a space, as in the included \code{\link{PLANTS}} data set. \code{blend} 
  needs these names to match for the native and
  exotic landscape in order to compare them. Any
  landscapes that do not have a counterpart will not be included in the
  output. If \code{blend} cannot find
  any matching native-exotic landscape pairs, it will return an error. 
  
  \code{blend} expects sites as columns and species as rows. In .csv files,
  the first row must be site names and the first column must be species names.
  If you input data as \code{data.frame}s, these attributes should be 
  included as \code{dimnames} instead. The column names, corresponding to 
  site names, must match between the native and exotic landscapes.
  
  The body of your files or \code{data.frames} should be \code{1}s
  indicating species presence at a given site, or \code{0}s
  for absences.
}
\value{
\code{blend} returns a \code{blended.landscape} object if called on a
single landscape or a \code{blended.landscape.bundle} if called on more
than one.  A bundle includes all of the below for each landscape, 
plus a \code{summary} \code{data.frame}.

\code{blended.landscape} objects contain:

\item{name}{
  The name of the landscape analyzed (e.g. "Nebraska" if the contents of
  \code{x} included "Nebraska native table" and "Nebraska exotic table")
}
\item{J.Bar, J.Star}{
  \code{J.Bar} is the average Jaccard similarity among sites in the native 
  landscape (i.e. the average ratio of shared species to total species 
  among pairs of sites).

  \code{J.Star} is the approximation from Harris et al: average number of 
  species shared between each pair of sites divided by the average number
  of species present at least once among pairs in the native landscape.
}
\item{delta.J.Bar, delta.J.Star}{
  \code{delta.J.Bar} is the increase or decrease in average Jaccard 
  similarity observed when incorporating the full complement of exotic 
  species
  
  \code{delta.J.Star} is the corresponding increase or decrease in 
  \code{J.Star}.
}
\item{R2}{
  The proportion of variance in single-species changes in J.Bar explained
  by variance in single-species changes in J.Star.
} 
\item{threshold}{
  The proportion of sites that must be occupied by an exotic species for
  it to have no net effect on \code{J.Bar}, Calculated by smoothing the 
  observed \code{delta.J.Bars} in \code{species.delta.table} using the 
  \code{\link{loess}} function. Will return \code{NA} if data cannot be
  smoothed near this point.
}
\item{p.Star}{
  The proportion of sites that must be occupied by an exotic 
  species for it to have no effect on mean similarity, according to 
  the effective occupancy equation in Harris et al.(which is based on
  J.Star)
}
\item{nadir}{
  The level of exotic occupancy for which mean similarity is minimized.
  Calculated by smoothing the 
  observed \code{delta.J.Bars} in \code{species.delta.table} using the 
  \code{\link{loess}} function. Will return \code{NA} if data cannot be
  smoothed near this point.
}
\item{results.table}{
  A summary \code{data.frame} containing all the above information 
  (except \code{name}).
}
\item{species.delta.table}{
  A \code{data.frame} containing \code{delta.J.Bars} and 
  \code{delta.J.Stars} attributed to each of the exotic species in the
  exotic data table individually.
} 
\item{scoop}{
  A set of points used for plotting the "scoop"-shaped model predictions   
}
\item{native, exotic}{
  The original imported landscapes
}
  
If called on more than one landscape, \code{blend} produces a
\code{blended.landscape.bundle}, which includes
one \code{blended.landscape} for each landscape included,
as well as a \code{data.frame} called \code{summary} that includes all
the information from each landscape's \code{resuls.table}.

}
\references{

  Harris, D. J., K. G. Smith, and P. J. Hanly. 2011.
  "Occupancy is nine-tenths of the law: 
  Occupancy rates determine the homogenizing and differentiating effects
  of exotic species" The American Naturalist.

  
}
\author{
  David Jay Harris \code{<DavHarris@UCDavis.edu>}
}
\seealso{
  \link{blender.basics}
}
\examples{
data(PLANTS)

wy.results = blend(PLANTS[c("WY native table", "WY exotic table")])

# print a summary of the results 
wy.results

# plot contributions of individual exotic species to mean similarity
plot(wy.results)

# blend a set of five landscapes simultaneously
five.results = blend(PLANTS[1:(5 * 2)])

\dontrun{
  # Alternative method of calling blend using a directory
  five.results = blend("Users/Dave/Documents/similarity stuff/state matrices")
}

# print a summary of the results across all landscapes
five.results

# plot predictions vs. observations across all landscapes
plot(five.results)

# plot contributions of individual exotic species to mean similarity in 
# the first landscape
plot(five.results[[1]])}

