\name{3.1.generic.pems.handlers}
\alias{3.1.generic.pems.handlers}
\alias{generic.pems.handlers}
\alias{pems.generics}
\alias{as.data.frame.pems}
\alias{as.data.frame}
\alias{dim.pems}
\alias{dim}
%\alias{nrow.pems}
%\alias{ncol.pems}
\alias{$.pems}
\alias{$}
\alias{$<-.pems}
\alias{$<-}
\alias{[.pems}
\alias{[}
\alias{[<-.pems}
\alias{[<-}
\alias{[[.pems}
\alias{[[}
\alias{[[<-.pems}
\alias{[[<-}
\alias{with.pems}
\alias{with}
\alias{subset.pems}
\alias{subset}
\alias{head.pems}
\alias{head}
\alias{tail.pems}
\alias{tail}
\alias{names.pems}
\alias{names}
\alias{names<-.pems}
\alias{names<-}
\alias{print.pems}
\alias{print}
\alias{plot.pems}
\alias{summary.pems}
\alias{na.omit.pems}
\alias{na.omit}
\alias{units.pems}
\alias{units}
\alias{units<-.pems}
\alias{units<-}

\title{ Generic handling of pems objects }

\description{
  pems objects can be manipulated using generic functions like print, plot and 
  summary in a similar fashion to objects of other R classes. 
}

\usage{

\method{as.data.frame}{pems}(x,...)

\method{dim}{pems} (x, ...)

\method{$}{pems} (x, name, ...)

\method{$}{pems} (x, name, ...) <- value

\method{[}{pems} (x, i, j, ..., force = FALSE, simplify = TRUE)

\method{[}{pems} (x, i, j, ..., force = FALSE) <- value

\method{[[}{pems} (x, k, ...)

\method{[[}{pems} (x, k, ...) <- value

\method{with}{pems} (data, expr, ...)

\method{subset}{pems} (x, ...)

\method{names}{pems}(x, ...)

\method{names}{pems}(x, ...) <- value

\method{print}{pems}(x,..., rows=NULL, cols=NULL, width=NULL)

\method{plot}{pems}(x, id = NULL, ignore = "time.stamp", n = 3, ...)

\method{head}{pems}(x, n = 6, ...)

\method{tail}{pems}(x, n = 6, ...)

\method{summary}{pems}(object, ...)

\method{na.omit}{pems}(object, ...)

\method{units}{pems}(x)

\method{units}{pems}(x) <- value

}

\arguments{

  \item{x, object, data}{ 
  (An Object of pems class). For direct use with \code{print}, \code{plot}, \code{summary}, 
  etc. NOTE: Object naming (i.e., \code{x} or \code{object}) is determined in parent or 
  base function in R, so naming can vary by method.
}

  \item{name}{ 
  Element name, which operates in a similar fashion to \code{data.frame} names, e.g. 
  \code{pems$name} extracts the element \code{name} from the pems object \code{pems}.
}

  \item{i, j}{ 
  Row and column (elements) indices, which operate as a stricter version of 
  \code{data.frame} indices. See Note below.
}

  \item{k}{ 
  Structural indices. See Note below.
}

  \item{expr}{
   (Expression) For \code{with(pems)}, an expression to be evaluated inside 
   the supplied \code{pems} object.  
}

  \item{value}{
   (vector, data.frame or pems) An object to be inserted into a \code{pems} object in e.g. the form 
   \code{pems[i,j] <- value} or \code{pems$name <- value}. 
}

  \item{...}{ 
  Addition options, typically passed to associated default method(s).
}

  \item{force}{ 
  (Logical or character) Data element handling options. \code{force} provides various options to 
  forces data to fit it destination. This can either be set as a logical (\code{TRUE}/\code{FALSE} 
  force/don't force) or one or more character strings to specify particular types of forcing to try 
  when e.g. fitting \code{value} into \code{pems[i,j]}: \code{'omit.err.cases'}, equivalent to 
  \code{TRUE}, remove any unknown/unfound \code{i} or \code{j} terms; \code{'na.pad.insert'} expand 
  the \code{value} to fit larger \code{pems[i,j]}, placing \code{NAs} in any holes generated; 
  \code{'na.pad.target'} like previous but expanding \code{pems[i,j]} to fit larger \code{value}; 
  \code{'fill.insert'} like \code{'na.pad.inert'} but holes are filled by wrapping the supplied 
  \code{value} within elements and then by element. 
}

  \item{simplify}{ 
  (Logical) \code{simplify} returns a \code{pems.element} rather than a \code{pems} object 
  if possible. 
}
   
  \item{id, ignore}{ 
  (local plot parameters). \code{id} identifies which data series to plot; \code{ignore} identifies 
  which data series to ignore when leaving the choice of \code{id} to the function; and, \code{n} gives 
  the maximum number of data series to plot when leaving the choice of \code{id} to the function.
}
  \item{rows, cols, width}{
  (numerics, optional). For \code{print}, if supplied, these reset the number of rows and columns 
  to table when printing a \code{pems} object, and character width to print across.   
}
  \item{n}{ 
  (various). For \code{plot}, sets the maximum number of data series to plot when leaving the choice of 
  \code{id} to the function. For \code{head} or \code{tail}, sets the number of rows. 
}

}

\value{
  Generic functions provide appropriate (conventional) handling of objects of 
  \code{'pems'} class: 

  \code{as.data.frame(pems)} extracts the \code{data.frame} component of a 
  pems object.

  \code{dim(pems)} extracts the dimensions, row count and column count, 
   respectively, of the \code{data.frame} component of a pems object. The 
   function also allows \code{nrow(pems)} and \code{ncol(pems)}.  

  \code{pems$name} extracts the named element from a pems objects 
  in a similar fashion to \code{data.frame$name}. Likewise, 
  \code{pems$name <- value} inserts \code{value} into a pems objects 
  in a similar fashion to \code{data.frame$name <- value}.      

  \code{pems.object[i, j]} extracts the [i,j] elements of the data held in a 
  \code{pems} object. This is returned as either a \code{pems} or \code{pems.element} 
  object depending on the dimension of the extracted data and the \code{simplify} 
  setting.

  \code{pems.object[i, j]<-} insert \code{value} into the [i,j] region of the supplied  
  \code{pems} object. By default this action is strict and mismatching \code{pems[i, j]} 
  and \code{value} dimension produce an error, although mismatching insertions may be 
  forced using the \code{force} argument.  

  \code{pems.object[[k]]} extracts structural elements of a pems object: 
  \code{data}, the \code{data.frame}; \code{units} the unit table, etc.  

  \code{with(pems.object, expression)} evaluates the supplied \code{expression} 
  using the elements of the supplied \code{pems.object}. 

  \code{subset(pems.object, expression)} behaves like 
  \code{subset(data.frame, expression)}.

  \code{print(pems.object)} provides a (to console) description of a \code{pems} 
  object. This forshortens large datasets in a similar fashion to a tibble. 

  \code{plot(pems.object)} generates a standard R plot using selected data series in 
  a \code{pems} object.
 
  \code{names(pems.object)} returns a vector of the names of data series held in a 
  \code{pems} object when used in the form \code{names(pems)} or resets names when 
  used in the form \code{names(pems) <- new.names}. 

  \code{na.omit(pems.object)} returns the supplied \code{pems} object with all rows 
  including NAs removed. 

  \code{summary(pems.object)} generates a summary report for data series held in a 
  \code{pems} object. 

  \code{units(pems.object)} extracts the units from a supplied \code{pems} object 
  when used in the form \code{units(pems)} or sets/resets units when used in the form 
  \code{units(pems) <- new.units}. 

}

\author{ Karl Ropkins }

\note{

  The \code{pems} object is intended to be a stricter version of a standard R 
  \code{data.frame}. Unless the user specifically forces the operation, a \code{pems[]} 
  or \code{pems[]<-} call is not allowed unless it fits exactly. So, for example by default 
  the call \code{pems[,1]<-10} will not place 10 in every row of column one in the 
  same fashion as \code{data.frame[,1]<-10}. 

  The logic behind this is that columns (elements) of \code{pems} objects are time-series. 
  So, users would want to place these exactly and avoid any unintended wrapping. The 
  \code{force} argument should be used in cases where data padding or wrapping operations 
  are required. 

  \code{pems$name} and \code{pems$name<-} are not are rigorously managed, so behave more 
  like \code{data.frame$name} and \code{data.frame$name<-} calls, although even these do 
  wrap by default.

  \code{pems[[]]} provides access to structural components of the \code{pems} object, 
   e.g. \code{pems[["data"]]} extracts the \code{data.frame} component of the \code{pems} 
   object, \code{pems[["units"]]} extracts the \code{units} component, etc. See also 
   \code{\link{pems.structure}}.
 
}


\references{
  
  generics in general:

  H. Wickham. Advanced R. CRC Press, 2014.

  (Not yet fully implemented within this package.)

}



\examples{

##example 1
##basics pems handling

#extract a subset pems object from pems.1
a <- pems.1[1:4, 1:5]
a      

#indices work like data.frame
#a[x] and a[,x] recovers element/column number x
#a[x,] recovers row number x
#a["name"] and a[,"name"] recovers element/column named "name"
#a[4:5, "name"] recovers rows 4 to 5 of element/column named "name"
#a[x,y] <- z inserts z into a at row x, element y 
#etc

#insert 10 in at element 3, row 2
a[2,3] <- 10
a

#replace element conc.co2 with conc.co
a["conc.co2"] <- a$conc.co
a

#Note: by default pems objects subsetting and inserting is 
#more rigorous than data.frame subsetting/insertion
#for example, a[1:2, "conc.hc"] <- 8 would generate error
#because the target, a[1:2], and insert, 8, dimensions do not 
#match exactly: target 2 x 1; insert 1 x 1 

#By default no wrapping is applied.  

#the force argument allows the user to control how mismatching 
#targets and insertions are handled

#na pad target for larger insert
a[1:2, "conc.hc", force="na.pad.target"] <- 1:5
a

#Note here when the target is padded existing enteries are NOT 
#overwritten if they are not declared in a[], and the next 
#previously unassigned cells are used for any extra cases in
#the insert.

#wrap insert to fill hole made by a[i,j]  
a[1:2, "conc.hc", force="fill.insert"] <- 8
a

#pems$name <- value is equivalent to 
#pems[name, force=c("na.pad.target", "na.pad.insert")]
a$new <- 1:4
a

}

\keyword{methods}
