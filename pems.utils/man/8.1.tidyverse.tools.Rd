\name{8.1.pems.tidyverse.tools}
\alias{8.1.pems.tidyverse.tools}
\alias{pems.tidyverse}
\alias{fortify.pems}
\alias{fortify}
\alias{select_.pems}
\alias{select.pems}
\alias{select}
\alias{rename_.pems}
\alias{rename.pems}
\alias{rename}
\alias{filter_.pems}
\alias{filter.pems}
\alias{filter}
\alias{arrange_.pems}
\alias{arrange.pems}
\alias{arrange}
\alias{slice_.pems}
\alias{slice.pems}
\alias{slice}
\alias{mutate_.pems}
\alias{mutate.pems}
\alias{mutate}
\alias{group_by.pems}
\alias{group_by_.pems}
\alias{group_by}
\alias{groups.pems}
\alias{groups}
\alias{ungroup.pems}
\alias{ungroup}
\alias{group_size.pems}
\alias{group_size}
\alias{n_groups.pems}
\alias{n_groups}
\alias{summarise_.pems}
\alias{summarise.pems}
\alias{summarise}
\alias{pull.pems}
\alias{pull}
\alias{left_join.pems}
\alias{left_join}
\alias{inner_join.pems}
\alias{inner_join}
\alias{right_join.pems}
\alias{right_join}
\alias{full_join.pems}
\alias{full_join}
\alias{semi_join.pems}
\alias{semi_join}
\alias{anti_join.pems}
\alias{anti_join}


\title{
Functions to use tidyverse code with pems.utils outputs  
}

\description{
Various codes and methods.
}

\usage{

#ggplot2 

\method{fortify}{pems}(model, data, ...)

#dplyr (1) standard methods

\method{select}{pems}(.data, ...)
\method{rename}{pems}(.data, ...)
\method{filter}{pems}(.data, ...)
\method{arrange}{pems}(.data, ...)
\method{slice}{pems}(.data, ...)
\method{mutate}{pems}(.data, ..., units=NULL, warn=TRUE)
\method{group_by}{pems}(.data, ..., .add=FALSE)
\method{groups}{pems}(x)
\method{ungroup}{pems}(x, ...)
\method{group_size}{pems}(x)
\method{n_groups}{pems}(x)
\method{summarise}{pems}(.data, ...)
\method{pull}{pems}(.data, ...)

#dplyr (2) related underscore methods

\method{select_}{pems}(.data, ..., warn=TRUE)
\method{rename_}{pems}(.data, ..., warn=TRUE)
\method{filter_}{pems}(.data, ..., warn=TRUE)
\method{arrange_}{pems}(.data, ..., warn=TRUE)
\method{slice_}{pems}(.data, ..., warn=TRUE)
\method{mutate_}{pems}(.data, ..., units=NULL, warn=TRUE)
\method{group_by_}{pems}(.data, ..., .add=FALSE, warn=TRUE)             
\method{summarise_}{pems}(.data, ..., warn=TRUE)

#dplyr (3) joining methods
\method{inner_join}{pems}(x, y, by = NULL, copy = FALSE, ...)
\method{left_join}{pems}(x, y, by = NULL, copy = FALSE, ...)
\method{right_join}{pems}(x, y, by = NULL, copy = FALSE, ...)
\method{full_join}{pems}(x, y, by = NULL, copy = FALSE, ...)
\method{semi_join}{pems}(x, y, by = NULL, copy = FALSE, ...)
\method{anti_join}{pems}(x, y, by = NULL, copy = FALSE, ...)

}


\arguments{

  \item{model,data}{
  (pems.object) In \code{fortify}, the \code{pems} 
  object to be used as a data source when plotting 
  using \code{ggplot2} code. The method is rotuinely 
  applied by \code{ggplot2}, so users can typically 
  ignore this. See below.
}
  \item{\dots}{
  (Optional) Other arguments, typically passed on to
  equivalent tidyverse function or method.
}

  \item{.data}{
  (pems.object) For \code{dplyr} functions, the pems 
  object to be used with, e.g. dplyr code. 
} 
  \item{warn}{
  (Optional) Give warnings? For an underscore methods: 
  a warning that an underscore method was used 
  (See Below). For mutate: if new elements are 
  generated without unit assignments.
}
  \item{units}{
  (Character) In mutate, the units to assign to 
  new elements created by call. See Below.
}
  \item{x,y}{
   (Various) For \code{group...} functions, \code{x} 
   is the \code{pems} dataset to be grouped. 
   For \code{...join} functions, 
   \code{x} and \code{y} are the two datasets 
   (\code{pems}, \code{data.frame}, etc) to 
   be joined together.
}
  \item{.add}{
   (Optional) Argument used by \code{group_by} and 
   related \code{dplyr} grouping functions. 
}
  \item{by,copy}{
   (Various) For \code{...join} functions, consistent 
   with \code{dplyr}, \code{by} and \code{copy} are 
   optional arguments. See Below.   
}

}

\details{

  \code{fortify} is used by \code{ggplot2} functions 
  when these are used to plot data in a \code{pems} 
  dataset. Most users will never have to use this 
  directly. 

  The pems object methods \code{select}, 
  \code{rename}, \code{filter}, 
  \code{arrange}, \code{slice}, \code{mutate}, 
  \code{group_by} and  \code{summarise} are similar 
  to \code{data.frame} methods of the same 
  names in \code{dplyr}, but (hopefully) they 
  also track units, etc, like a pems object. 
  Work in progress. See below, especially Note.

  Equivalent underscore methods (\code{select_}, etc) 
  are also provided, although it should be noted that 
  they are probably going when \code{dplyr} drops 
  these.

  Data joining methods include \code{inner_join}, 
  \code{left_join}, \code{right_join}, \code{full_join}, 
  \code{semi_join} and \code{anti_join}. Like above 
  these are similar \code{data.frame} equivalents in 
  \code{dplyr}, but (hopefully) also track units, etc, 
  like a pems object. Same 'work in progress' caveat. 
  See Note.
   
}


\value{

  \code{select} returns the requested part of the 
  supplied pems object, e.g.:
  \code{select(pems.1, velocity)} returns the velocity 
  element of pems.1 as a single column pems.object, 
  consistent with the data.frame handling of 
  \code{select.data.frame}.

  \code{rename} returns the supplied pems object with 
  the requested name change, e.g.: 
  \code{rename(pems.1, speed=velocity)} returns pems.1 
  with the velocity column renamed speed.

  \code{filter} returns the supplied pems object after 
  the requested filter operation has been applied, 
  e.g.: \code{filter(pems.1, velocity>0.5)} returns 
  pems.1 after excluding all rows where the velocity 
  value was less than or equal to 0.5.

  \code{arrange} returns the supplied pems object 
  reordered based on order of values in an identified 
  element, e.g.: \code{arrange(pems.1, velocity)} 
  returns pems.1 with its row reordered lowest to 
  highest velocity entry.

  \code{slice} returns requested rows of the supplied 
  pems object, e.g.: \code{slice(pems.1, 1:10)} returns 
  rows 1 to 10 of pems.1 as a new pems object.

  \code{mutate} returns the supplied pems object with 
  extra elements calculated as requested, e.g.: 
  \code{mutate(pems.1, new=velocity*2)} returns the 
  pems object with additional column, called new, which 
  is twice the values in the velocity column. The units 
  of the new column can be set using the additional 
  argument units, e.g. 
  \code{mutate(pems.1, new=velocity*2, units="ick")}.

  \code{group_by} returns a \code{grouped_df} object, 
  which allowed by-group handling in subsequent 
  \code{dplyr} code. 
  
  \code{summarise} works like 
  \code{summarise(data.frame, ...)}  and allows 
  dataset calculations, e.g. 
  \code{summarise(pems, mean(velocity))} calculates 
  the mean of the \code{velocity} of a supplied 
  \code{pems} object. Units cannot be tracked 
  during such calls and outputs are returned as 
  a \code{tibble} as with \code{summarise.data.frame}. 

  The \code{..._join} joining methods, join two 
  supplied datasets. The first, \code{x}, 
  must be a \code{pems} to employ \code{..._join.pems} 
  but the second, \code{y} can be e.g. a 
  \code{data.frame}, etc. 
  
}

\references{
  Generics in general:

  H. Wickham. Advanced R. CRC Press, 2014.

  (Not yet fully implemented within this package.)

  ggplot2:

  H. Wickham. ggplot2: elegant graphics for data 
  analysis. Springer New
  York, 2009.

  (See Chapter 9, section 9.3, pages 169-175, 
  for discussion of fortify)
  
  dplyr:
  
  Hadley Wickham, Romain Francois, Lionel Henry and
  Kirill Muller (2020). dplyr: A Grammar of Data
  Manipulation. R package version 1.0.2.
  https://CRAN.R-project.org/package=dplyr

}

\author{
  Karl Ropkins
}

\note{
  Currently not sure what I think about tidyverse, but 
  it is always interesting, and ideas like 
  \code{fortify} are nice.  
  
  The \code{fortify} method was developed by 
  Hadley Wickham to simplify the integration of 
  \code{ggplot2} functions and special object classes.
    
  It is a really nice idea for multiple reasons, the 
  main one being that package users will probably never
  have to worry about it. However, packaging it means 
  you can use a \code{pems} object directly 
  as the data argument with \code{ggplot2} code.
  
  
}

\section{Warning }{
  This currently work in progress - handle with care.
}

%\seealso{
%  [to do] 
%}

%\examples{

%###########
%##example 1 
%###########

%}

\keyword{ methods }
