\name{symbolic.object}
\alias{symbolic.object}
\title{
  Symbolic data table Object 
}
\description{
These are objects representing symbolic data table structure
}
\section{Structure}{
The following components must be included in a legitimate \code{symbolic} object. 
}
\value{

\item{individuals}{
data frame with one row for each row in symbolic data table with following columns:

\code{num} - symbolic object (described by symbolic data table row) ordering number , usually from 1 to numebr of symbolic objects;

\code{name} - short name of symbolic object with no spaces;

\code{label} - full descriptive name of symbolic object.
}
\item{variables}{
data frame with one row for each column in symbolic data table with following columns:

\code{num} - symbolic variable (adequate to symbolic data table column) ordering number, usually from 1 to number of symbolic variables;

\code{name} - short name of symbolic variable with no spaces;

\code{label} - full descriptive name of symbolic variable;

\code{type} - type of symbolic variable: 
\command{IC} (InterContinous) - Symbolic interval variable type, every realization of symbolic variable of this type on symbolic object takes form of numerical interval;
\command{C} (Continous) - Symbolic interval variable type, every realization of symbolic variable of this type on symbolic object takes form of numerical interval for which begging is equal to end (equivalent to simple "numeric" value);
\command{MN} (MultiNominal) - every realization of multi nominal symbolic variable on symbolic objects takes form of set of nominal values;
\command{NM} ((Multi) Nominal Modif) - every realization of nominal symbolic variable on symbolic objects takes form of distribution of probabilities (set of nominal values with weights summing to one)
\command{N}  (Nominal) - every realization of nominal symbolic variable on symbolic objects is one value (or N.A.)

\code{details} - id of this variable in details table apropriate for this kind of variable (\emph{detailsN} for nominal and multi nominal variables, \emph{detailsIC} for symbolic interval variables, 
\emph{detailsC} for continous (metric single-valued) variables, \emph{detailsNM} of multi nominal with weights variables).
}
\item{detailsC}{
data frame describing symbolic continous (metric, single-valued) variables details with following columns:

\code{na} - number of N.A. (not available) variables realization;

\code{nu} - not used, left for compatibility with ASSO-XML specification;

\code{min} - beginning of interval representing symbolic interval variable domain (minimal value of all realizations of this variable on all symbolic objects);

\code{max} - end of interval representing symbolic interval variable domain (maximal value of all realizations of this variable on all symbolic objects).
}
\item{detailsIC}{
data frame describing symbolic inter-continous (symbolic interval) variables details with following columns:

\code{na} - number of N.A. (not available) variables realizations;

\code{nu} - not used, left for compatibility with ASSO-XML specification;

\code{min} - beginning of interval representing symbolic interval variable domain (minimal value of all beginnings of interval realizations of this variable on all symbolic objects);

\code{max} - end of interval representing symbolic interval variable domain (maximal value of all ends of interval realizations of this variable on all symbolic objects).
}
\item{detailsN}{
data frame describing symbolic nominal and multi nominal variables details with following columns:

\code{na} - number of N.A. variables realizations;

\code{nu} - not used, left for compatibility with ASSO-XML specification;

\code{modals} - number of categories in symbolic variable domain. Each categorie is described in \emph{detailsListNom}.
}
\item{detailsListNom}{
data frame describing every category of symbolic nominal and multi nominal variables, with following columns:

\code{details_no} - number of variable in \emph{detailsN} to which domain belongs category;

\code{num} - number of category within variable domain;

\code{name} - category short name

\code{label} - category full name
}
\item{detailsNM}{
data frame describing symbolic multi nominal modiff (categories sets with weights) variables details with following columns:

\code{na} number of N.A. (not available) variables realizations.

\code{nu} not used, left for compatibility with ASSO-XML specification

\code{modals} number of categories in symbolic variable domain. Each categorie is described in \emph{detailsListNomModiff}
}

\item{detailsListNomModif}{
data frame describing every category of symbolic multi nominal modiff variables, with following columns

\code{details_no} - number of variable in \emph{detailsNM} to which domain belongs category 

\code{num} - number of category within variable domain

\code{name} - category short name

\code{label} - category full name
}
\item{indivIC}{
array of symbolic interval variables realizations, with dimensions nr_of_objects X nr_of_variables X 2 containing beginnings and ends of intervals for given object and variable. For values different type than symbolic interval array contains zeros
}
\item{indivC}{
array of symbolic continues variables realizations, with dimensions nr_of_objects X nr_of_variables X 1 containing single values - realizations of variable on symbolic object. For values different type than symbolic continous array contains zeros
}
\item{indivN}{
data frame describing symbolic nominal and multi nonimal variables realizations with folowing columns:

\code{indiv} - id of symbolic object from \emph{individuals}; 

\code{variable} - id of symbolic object from \emph{variables};

\code{value} - id of category object from \emph{detailsListNom};

When this data frame contains line \emph{i,j,k} it means that category \emph{k} belongs to set that is realization of \emph{j}-th symbolic variable on \emph{i}-th symbolic object.
}
\item{indivNM}{
data frame describing symbolic multi nonimal modiff variables realizations with folowing columns:

\code{indiv} - id of symbolic object from \emph{individuals};

\code{variable} - id of symbolic object from \emph{variables};

\code{value} - id of category object from \emph{detailsListNom};

\code{frequency} - wiught of category;

When this data frame contains line \emph{i,j,k,w} it means that category \emph{k} belongs to set that is realization of \emph{j}-th symbolic variable on \emph{i}-th symbolic object with weight(probability) \emph{w}.

}

}
\details{
For all fields symbol N.A. means not available value.

For futher details see \url{../doc/SDA.pdf}
}
\seealso{
\code{\link{dist_SDA}}.
}
\keyword{symbolic}
\concept{SDA}
