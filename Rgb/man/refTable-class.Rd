\name{refTable-class}
\Rdversion{1.1}
\docType{class}
\alias{refTable-class}

\title{Class \code{"refTable"}}
\description{
  This class is similar to the \code{data.frame} standard R class, following the object-oriented paradigm. The use of Reference Class system allows significative memory and time saving, making this class more suitable than \code{data.frame} to handle large tabular data.
  
  Objects can be created by two distincts means :
  \itemize{
    \item{The \code{\link{refTable}} constructor, similar to the \code{data.frame} constructor. It imports a single \code{data.frame} or a collection of vectors into the object, and check immediatly for validity.}
    \item{The \code{new} function (standard behavior for S4 and reference classes), which produces an empty object and do NOT check for validity. You can provide as arguments the values to use as the new object fields, if you know what you are doing.}
  }
}
\author{
  Sylvain Mareschal
}
\examples{
  # New empty refTable
  tab <- new("refTable")
  tab$addColumn(1:5, "C1")
  tab$addColumn(letters[1:5], "C2")
  tab$setRowNames(LETTERS[11:15])
  
  # New filled refTable (same content)
  tab <- refTable(C1=1:5, C2=letters[1:5], row.names=LETTERS[11:15])
  
  # Whole table print
  print(tab$extract())
  
  # Data update
  tab$fill(c(2,4), 2, c("B","D"))
  
  # Data extraction
  print(tab$extract(1:3))
  print(tab$extract(c(TRUE, FALSE)))
  print(tab$extract("K", "C1"))
  
  # Expression-based extraction
  expr <- expression(C1 \%\% 2 == 1)
  print(tab$extract(expr))
  
  # Table extension
  tab$addEmptyRows(5L, LETTERS[1:5])
  tab$fill(6:10, "C1", 6:10)
  print(tab$extract())
  
  # Filling from R objects
  tab <- new("refTable")
  print(tab$extract())
  tab$addVectors(C1=1:5, C2=letters[1:5])
  print(tab$extract())
  tab$addList(list(C1=6:8, C3=LETTERS[6:8]))
  print(tab$extract())
  
  # Beware of recycling !
  tab$addVectors(C1=9:15, C3=LETTERS[9:10])
  print(tab$extract())
}

\keyword{classes}
\section{Extends}{
  All reference classes extend and inherit methods from \code{\linkS4class{envRefClass}}.
}
\section{Implementation}{
  \subsection{Data storage}{
    refTable objects store data into their \code{values} field as vectors. As \code{values} is an environment, manipulating a refTable implies a pass-by-reference paradigm rather than the standard R pass-by-copy, i.e. data duplication (and so time and memory wasting) is widely reduced. As an example, updating a single cell in a \code{data.frame} leads to the duplication of the whole table in memory ('before' and 'after' versions), while in a refTable the duplication is limited to the involved column. 
  }
  \subsection{Column names}{
    To facilitate column renaming, the vectors in \code{values} are not named according to the user-level column names, but according to references stored in the \code{colReferences} field (integers greater than 0 converted to characters). Rename a column only updates the \code{colNames} field and leave the \code{values} one alone, as the column reference does not change.
  }
  \subsection{Data extraction}{
    Data extracted from refTable are usually returned as \code{data.frame}, for a more comfortable R usage. The extraction mechanism handles \code{data.frame} extraction mechanisms, and relies on the \code{indexes} method to handle the others.
	
	\itemize{
	  \item{Rows and columns may be selected by a numeric vector, as for R \code{data.frame} and vectors.}
	  \item{They also may be selected by a logical vector, defining for each row / column if it is to be selected (\code{TRUE}) or not (\code{FALSE}). Such vectors are recycled if not long enough to cover all the rows / columns.}
	  \item{A character vector defining the names of the rows / columns to select may also be used to extract data.}
	  \item{The \code{NULL} value may be used to select all rows / columns.}
	  \item{An unevaluated expression, as returned by \code{expression} or \code{parse} may be used to select rows in the table environment. See 'examples'.}
	}
  }
}
\section{Fields}{
  \describe{
    \item{\code{colCount}:}{Single \code{integer} value, the amount of rows in the table.}
    \item{\code{colIterator}:}{Single \code{integer} value, last column reference used.}
    \item{\code{colNames}:}{\code{Character} vector, the names of all rows (may be empty).}
    \item{\code{colReferences}:}{\code{Character} vector, the column names in the \code{values} environment.}
    \item{\code{rowCount}:}{Single \code{integer} value, the amount of rows in the table.}
    \item{\code{rowNamed}:}{Single \code{logical} value, whether row names should be considered or not.}
    \item{\code{rowNames}:}{\code{Character} vector, the names of all rows (may be empty).}
    \item{\code{values}:}{An \code{environment} storing the columns as vectors.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{addColumn(content, name, after = )}:}{
      Adds a column in the table\cr
      - \strong{content}   : values to fill in the new column, as a vector.\cr
      - \strong{name}      : name of the new column, as character.\cr
      - \strong{after}     : where to add the column, as the index (numeric) or name (character) of the column on its left
    }
    \item{\code{addDataFrame(dataFrame)}:}{
      Adds a data.frame content to the refTable\cr
      - \strong{dataFrame}   : the data to add.
    }
    \item{\code{addEmptyRows(amount, newNames)}:}{
      Add rows filled with NA at the bottom of the table.\cr
      - \strong{amount}     : single integer value, the amount of rows to add.\cr
      - \strong{newNames}   : character vector, the names of the new rows. Ignored if the table is not row named.
    }
    \item{\code{addList(dataList, row.names)}:}{
      Adds a list content to the refTable\cr
      - \strong{dataList}    : the data to add.\cr
      - \strong{row.names}   : character vector with the names of the enw rows.
    }
    \item{\code{addVectors(..., row.names)}:}{
      Adds vectors to the refTable\cr
      - \strong{...}   : named vectors to add.
    }
    \item{\code{check(warn = )}:}{
      Raises an error if the object is not valid, else returns TRUE
    }
    \item{\code{coerce(j = , class, levels, ...)}:}{
      Coerces a single column to a different class\cr
      - \strong{j}        : column index (numeric) or name (character).\cr
      - \strong{class}    : name of the class to coerce 'j' to.\cr
      - \strong{levels}   : if 'class' is factor, the levels to use.\cr
      - \strong{...}      : further arguments to be passed to the 'as' method (for atomics) or function (for other classes).
    }
    \item{\code{colOrder(newOrder, na.last = , decreasing = )}:}{
      Reorder the columns of the tables (duplication / subsetting are NOT handled)\cr
      - \strong{newOrder}   : new order to apply, as an integer vector of character vector of column names.
    }
    \item{\code{delColumns(targets)}:}{
      Deletes a column from the table\cr
      - \strong{targets}   : character vector, the name(s) of the column(s) to delete.
    }
    \item{\code{erase()}:}{
      Remove all the rows and columns in the table.
    }
    \item{\code{extract(i = , j = , drop = , asObject = )}:}{
      Extracts values into a data.frame or vector\cr
      - \strong{i}          : row selection, see indexes() for further details.\cr
      - \strong{j}          : column selection, see indexes() for further details.\cr
      - \strong{drop}       : if TRUE and querying a single column, will return a vector instead of a data.frame.\cr
      - \strong{asObject}   : if TRUE results will be served in the same class as the current object.
    }
    \item{\code{fill(i = , j = , newValues)}:}{
      Replaces values in a single column\cr
      - \strong{i}           : row indexes (numeric) or names (character). NULL or missing for all rows.\cr
      - \strong{j}           : column index (numeric) or name (character).\cr
      - \strong{newValues}   : vector of values to put in the object
    }
    \item{\code{getColCount()}:}{
      'colCount' field accessor.
    }
    \item{\code{getColNames()}:}{
      'colNames' field accessor.
    }
    \item{\code{getLevels(j = )}:}{
      Get levels of a factor column\cr
      - \strong{j}   : column index (numeric) or name (character).
    }
    \item{\code{getRowCount()}:}{
      'rowCount' field accessor.
    }
    \item{\code{getRowNames()}:}{
      'rowNames' field accessor.
    }
    \item{\code{indexes(i, type = )}:}{
      Checks row or column references and return numeric indexes\cr
      - \strong{i}   : reference to the rows or columns to select (NA not allowed), as :\cr
      - missing or NULL (all rows or columns)\cr
      - vector of numeric indexes to select\cr
      - vector of character indexes to select (if the dimension is named)\cr
      - vector of logical with TRUE on each value to select, FALSE otherwise\cr
      - expression object (as returned by e(...)), to be evaluated in the 'values' environment
    }
    \item{\code{initialize(rowCount = , rowNames = , rowNamed = , colCount = , colNames = , colReferences = , colIterator = , values = , ...)}:}{}
    \item{\code{metaFields()}:}{
      Returns a character vector of fields that do not directly depend on the tabular content, for clonage.
    }
    \item{\code{rowOrder(newOrder, na.last = , decreasing = )}:}{
      Reorder the rows of the tables (duplication / subsetting are handled)\cr
      - \strong{newOrder}     : new order to apply, as an integer vector of row indexes or a character vector of column names.\cr
      - \strong{na.last}      : to be passed to order(), if 'newOrder' is a column name vector.\cr
      - \strong{decreasing}   : to be passed to order(), if 'newOrder' is a column name vector.
    }
    \item{\code{setColNames(j, value)}:}{
      Replaces one or many column names.\cr
      - \strong{j}       : subset of columns to rename.\cr
      - \strong{value}   : new column names to use, as a character vector.
    }
    \item{\code{setLevels(j = , newLevels)}:}{
      Get or replace levels of a factor column\cr
      - \strong{j}           : column index (numeric) or name (character).\cr
      - \strong{newLevels}   : new levels to use, as a character vector.
    }
    \item{\code{setRowNames(value)}:}{
      Replaces the entire row names set.\cr
      - \strong{value}   : new row names to use in the table, as a character vector. NULL will disable row naming.
    }
    \item{\code{types(j = )}:}{
      Returns classes of selected columns\cr
      - \strong{j}   : column indexes or names (NULL for all columns)
    }
  }
  The following methods are inherited (from the corresponding class):
  \itemize{
    \item callSuper (\linkS4class{envRefClass})
    \item copy (\linkS4class{envRefClass}, overloaded)
    \item export (\linkS4class{envRefClass})
    \item field (\linkS4class{envRefClass})
    \item getClass (\linkS4class{envRefClass})
    \item getRefClass (\linkS4class{envRefClass})
    \item import (\linkS4class{envRefClass})
    \item initFields (\linkS4class{envRefClass})
    \item show (\linkS4class{envRefClass}, overloaded)
    \item trace (\linkS4class{envRefClass})
    \item untrace (\linkS4class{envRefClass})
    \item usingMethods (\linkS4class{envRefClass})
  }
}
