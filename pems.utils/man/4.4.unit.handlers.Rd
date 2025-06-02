\name{4.4.unit.handlers}
\alias{4.4.unit.handlers}
\alias{pems.units}
\alias{getUnits}
\alias{setUnits}
\alias{convertUnits}
\alias{addUnitConversion}
\alias{addUnitAlias}
\alias{listUnitConversions}

\title{
 data unit handlers 
}

\description{
Various pems.utils functions for the management of data units.
}

\usage{

getUnits(input = NULL, data = NULL, ..., 
         if.missing = c("stop", "warning", "return"))

setUnits(input = NULL, units = NULL, data = NULL, ..., 
         if.missing = c("stop", "warning", "return"), 
         output = c("input", "data.frame", "pems", "special"),
         force = FALSE, overwrite = FALSE)

convertUnits(input = NULL, to = NULL, from = NULL, data = NULL, ..., 
         if.missing = c("stop", "warning", "return"), 
         output = c("input", "data.frame", "pems", "special"), 
         unit.conversions = NULL, force = FALSE, overwrite = FALSE)

#local unit.conversion method handling 

addUnitConversion(to = NULL, from = NULL, conversion = NULL, 
         tag = "undocumented",
         unit.conversions = ref.unit.conversions, ...,
         overwrite = FALSE)

addUnitAlias(ref = NULL, alias = NULL, 
         unit.conversions = ref.unit.conversions, ...)

listUnitConversions(unit.conversions = ref.unit.conversions, ...,
         verbose = FALSE, to = NULL, from = NULL)

}


\arguments{
  \item{input}{
  (vector, object or object element) An input, e.g. a vector of speed measurements. 
} 
  \item{data}{
  (data.frame, pems object) If supplied, the assumed source for an \code{input}. This 
  can currently be a standard \code{data.frame} or a \code{'pems' object}. Note: if 
  an \code{input} is not found in \code{data}, the parent environment is then also 
  checked before returning an error message.
}
\item{units, to, from, ref, alias, tag}{
  (Character vectors). Unit ids. \code{units} sets the units of \code{input} in \code{setUnits}. 
  \code{to} sets the units to convert \code{input} to when using \code{convertUnits}. The additional 
  arguments \code{from} can be used to apply unit conversions to \code{inputs} with un-defined or 
  mismatched units, but requires the extra argument \code{force = TRUE} to confirm action. When working 
  with local unit conversions \code{to} and \code{from} should be used to identify specific conversions, 
  e.g. when using \code{addUnitConversion} to add a new unit conversion method, and \code{ref} and 
  \code{alias} should be used to identify a current unit id and new alias, respectively, when using 
  \code{addUnitAlias}. \code{tag} is an optional more detailed conversion description, intended for 
  use in method documentation. (See Below for further details.) 

}
  \item{\dots}{
  (Optional) Other arguments, currently ignored.
}
\item{if.missing}{
  (Optional character vector) What the function should do if things do not go as expected. Current 
  options include: \code{"stop"} to stop the function with an error message; \code{"warning"} to 
  warn users that expected information was missing but to continue running the parent code; or 
  \code{"return"} to continue running the parent code without any warnings.
}
  \item{output}{
  (Character vector) Output mode for function results. Options currently include: \code{special}, 
  \code{input}, \code{data.frame}, and \code{pems}. See \code{force}, \code{overwrite} and Values 
  below for further details. 
}

  \item{force}{
   (Logical) Should a unit change to attempted even if checking indicates a mismatch, e.g. an attempt to 
   set the units of an \code{input} that already has units assigned.  
}
  \item{overwrite}{
   (Logical) If 'same name' cases are encountered when packing/repacking an \code{output} into a 
   \code{data.frame} or \code{pems} object, should the function overwrite the case in the target 
   \code{data.frame} or \code{pems} object with the modified \code{input}? (If \code{FALSE}, a 
   new element is generated with a unique name in the form \code{[input.name].number}.)
}

  \item{unit.conversions}{
   (Optional list) If supplied, \code{unit.conversions} is a 'look up' table for unit conversion 
   methods. By default, functions in \code{pems.utils} use the reference 
   \code{\link{ref.unit.conversions}}, but this can be copied to the workspace and updated to provide 
   the user with a means of updating and expanding the method set. 
}
   \item{conversion}{
   (Numeric or function) When adding or updating a conversion method using \code{addUnitConversion}, the 
   conversion method. This can be a numeric, in which case it is assumed to be a multiplication factor (and 
   converted to a function in the form \code{function(x) x * conversion}) or a function to be applied directly 
   to an \code{input}.    
}
   \item{verbose}{
   (Logical) For \code{listUnitConversions}. Should \code{unit.conversions} be reported in detail? By default 
   (\code{verbose = FALSE}) only unit conversion \code{tags} are reported.       
}


}

\details{

  \code{getUnits} returns the units of an \code{input}.

  \code{setUnits} sets/resets the units of an \code{input}.

  \code{convertUnits} converts the units of an \code{input}.     

  \code{addUnitConversion} adds a conversion method to a local version 
  of the unit conversion look-up table. Methods should be supplied as 
  \code{to} and \code{from} unit ids and an associated \code{conversion}.
  A \code{tag} can also be supplied to provide a more detailed description 
  of the conversion for use in documentation.   

  \code{addUnitAlias} adds an alias for an existing unit id in a local version 
  of the unit conversion look-up table. The existing unit id should be identified 
  using \code{ref} and the new alias should be assinged using \code{alias}. The 
  \code{alias} is added to all \code{to} and \code{from} elements containing 
  \code{ref} to allow users to work with alternative unit abbreviations.

  \code{listUnitConversions} lists the methods a supplied unit conversion look-up table. 
  If \code{to} and/or \code{from} arguments are also supplied, these are used to 
  subsample relevant methods. 
 
}


\value{

  \code{getUnits} returns the units of an \code{input} as a character vector if available, 
  else it returns \code{NULL}. 

  \code{setUnits} sets the units of an \code{input} to a supplied value, \code{units}, if they have 
  not already be set or if \code{force = TRUE}. The result is returned as the modified \code{input} alone, 
  the modified \code{input} as an element in a \code{data.frame}, or the modifed \code{input} as 
  an element in a \code{pems} object (depending on \code{output} setting). If either a 
  \code{data.frame} or \code{pems} object is supplied as \code{data}, this is used as the target 
  when repacking the \code{output}. (Note: \code{output = "special"} is a special case which allows the 
  function to select the \code{output} mode based on the type of \code{data} supplied.

  \code{convertUnits} converts the units of an \code{input}. Typically, this is done by setting the 
  required new units, using \code{to}, and letting the function select a suitable conversion method. However, 
  conversions can be forced by setting \code{from} and \code{force = TRUE} to apply a specifc 
  \code{to}/\code{from} method to an \code{input} regardless of the actual \code{units} of \code{input}. 
  As with \code{setUnits}, results can be \code{output} as \code{input}, \code{data.frame} or \code{pems} 
  objects.

  \code{addUnitConversion} returns a supplied unit conversion look-up table (or in its absence the reference 
  \code{ref.unit.conversions}) subject to the requested addition or update. Note: modifications that change 
  exist information require the extra argument \code{overwrite = TRUE} as confirmation. 

  \code{addUnitAlias} returns a supplied unit conversion look-up table (or in its absence the reference 
  \code{ref.unit.conversions}) subject to the requested alias addition. 

  \code{listUnitConversions} returns summary descriptions of methods in the supplied unit conversion look-up 
  table (or in its absence the reference \code{ref.unit.conversions}). Additional arguments, \code{to} and 
  \code{from}, can be used to select unit conversions of particular relevance.  
   
}
\references{
  References in preparation
}
\author{
  Karl Ropkins
}
\note{
  This set of functions is intended to provide a flexible framework for the routine handling of 
  data units. 
 
}

%\section{Warning }{
%  None currently
%}

\seealso{
  \code{\link{pems.element}}
}
\examples{

###########
##example 1 
###########

#work with data units

#getting units (where assigned)
getUnits(velocity, pems.1) #km/h

#setting units
a <- 1:10
a <- setUnits(a, "km/h") #add unit 

#alternaltive 
#using pems.element
#a <- pems.element(a, units="km/h", name = "a") 

#changing units
convertUnits(a, "mi/h")

# [1] 0.6213712 1.2427424 1.8641136 2.4854848 3.1068560 3.7282272 4.3495983
# [8] 4.9709695 5.5923407 6.2137119
# pems.element; [unnamed] [mi/h] [n = 10]

###########
##example 2
###########

#working with local unit conversions
#adding/updating unit conversion methods

#make a local reference
ref.list <- ref.unit.conversions 

#add a miles/hour alias to mi/h
ref.list <- addUnitAlias("mi/h", "miles/hour", ref.list)

#add a new conversion
ref.list <- addUnitConversion(to = "silly", from = "km/h", 
                              conversion = function(x) 12 + (21 * x), 
                              tag = "kilometers/hour to some silly scale",
                              unit.conversions = ref.list)

#use these
convertUnits(a, "miles/hour", unit.conversions = ref.list)

# [1] 0.6213712 1.2427424 1.8641136 2.4854848 3.1068560 3.7282272 4.3495983
# [8] 4.9709695 5.5923407 6.2137119
# units: "miles/hour" (as above but using your unit abbreviations)

convertUnits(a, "silly", unit.conversions = ref.list)

# [1]  33  54  75  96 117 138 159 180 201 222
# units: "silly" (well, you get what you ask for)

}

\keyword{ methods }
