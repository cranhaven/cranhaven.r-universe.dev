\name{6.2.common.check.functions}
\alias{6.2.common.check.functions}
\alias{check...}
\alias{checkOption}
\alias{checkPEMS}
\alias{checkUnits}
\alias{checkOutput}
\alias{checkIfMissing}

\title{
common check... functions 
}

\description{
Various pems.utils workhorse functions for input checking and routine data handling.
}

\usage{
checkOption(option=NULL, allowed.options=NULL,
           option.name = "option", allowed.options.name = "allowed.options",
           partial.match=TRUE, fun.name = "checkOption", 
           if.missing = c("stop", "warning", "return"),
           output = c("option", "test.result"), ...)

checkPEMS(data = NULL, fun.name = "checkPEMS",
           if.missing = c("return", "warning", "stop"),
           output = c("pems", "data.frame", "test.result"),
           ...) 

checkUnits(input = NULL, units = NULL, data = NULL, 
           input.name = NULL, fun.name = "checkUnits", 
           if.missing = c("stop", "warning", "return"),
           output = c("special", "units", "input", "test.result"),
           ..., unit.conversions = NULL)

checkOutput(input = NULL, data = NULL,
           input.name = NULL, fun.name = "checkOutput", 
           if.missing = c("stop", "warning", "return"),
           output = c("pems", "data.frame", "input", "test.result"),
           overwrite = FALSE, ...)

checkIfMissing(..., if.missing = c("stop", "warning", "return"), 
           reply = NULL, suggest = NULL, if.warning = NULL, 
           fun.name = NULL)

}


\arguments{
  \item{input}{
  (vector, object or object element) An input to be tested or recovered for 
  subsequent use by another function, e.g. a speed measurement from a \code{pems} 
  object. 
} 
  \item{data}{
  (data.frame, pems object) If supplied, the assumed source for an \code{input}. This 
  can currently be a standard \code{data.frame} or a \code{'pems' object}. Note: if an 
  \code{input} is not found in \code{data}, the parent environment is then also 
  checked before returning an error message.
}
  \item{input.name, option.name}{
  (Optional character vectors) If a \code{check...} function is used as a workhorse by 
  another function, the name it is given in any associated error messaging. See Note below.   
}
\item{fun.name}{
  (Optional character vector) If a \code{check...} function is used as a workhorse routine 
  within another function, the name of that other function to be used in any associated error 
  messaging. See Note below.
}
\item{if.missing}{
  (Optional character vector) How to handle an input, option, etc, if missing, not supplied or \code{NULL}. 
  Current options include: \code{"stop"} to stop the \code{check...} function and any parent function
  using it with an error message; \code{"warning"} to warn users that expected information was missing 
  but to continue running the parent code; or \code{"return"} to continue running the parent code 
  without any warnings.
}
  \item{output}{
  (Character vector) Output mode for \code{check...} function results. Options typically include the 
  \code{check type} and \code{"test.results"}. See Value below.

}
  \item{\dots}{
  (Optional) Other arguments, currently ignored by all \code{check...} functions expect \code{checkIfMissing}.
}
  \item{option, allowed.options, allowed.options.name}{
   (Character vectors) For \code{checkOption}, \code{option} and \code{allowed.options} are the supplied option, 
   and the allowed options it should be one of, respectively, and \code{allowed.options.name} if way these allowed 
   options should be identified in any associated error messaging. See Note below.  
}
  \item{partial.match}{
   (Logical) For \code{checkOption}, should partial matching be used when comparing \code{option} and 
   \code{allowed.options}.
}
  \item{units}{
   (Character vector) For \code{checkUnits}, the units to return \code{input} in, if requested 
   (\code{output = "input"}). Note: The default, \code{output = "special"}, is a special case which 
   allows \code{checkUnits} to return either the units if they are not set in the call (equivalent to 
   \code{output = "units"}) or the \code{input} in the requested units if they are set in the call 
   (equivalent to \code{output = "input"}). 
}
  \item{unit.conversions}{
   (List) For \code{checkUnits}, the conversion method source. See \code{\link{ref.unit.conversions}} and 
   \code{\link{convertUnits}} for further details. 
}

  \item{overwrite}{
   (Logical) For \code{checkOutput}, when packing/repacking a \code{data.frame} or \code{pems} object, 
   should 'same name' cases be overwritten? If \code{FALSE} and 'same names' are encountered, e.g. when 
   modifying an existing \code{data.frame} or \code{pems} element, a new element if generated with a 
   unique name in the form \code{[name].[number]}. 
}

  \item{reply, suggest, if.warning}{
   (Character vectors) For \code{checkIfMissing}, when generating error or warning messages, the main 
   reply/problem description, any suggestions what users can try to fix this, and the action taken by the 
   function if just warning (e.g. setting the missing value to \code{NULL}), respectively. All are options. 
}


}

\details{

  The \code{check...} functions are intended as a means of future-proofing \code{pems.utils} data 
  handling. They provide routine error/warning messaging and consistent 'front-of-house' handling 
  of function arguments regardless of any underlying changes in the structure of the \code{pems} 
  objects and/or \code{pems.utils} code. This means third-party function developed using these 
  functions should be highly stable.

  \code{checkOption} checks a supplied option against a set of allowed options, and then if present or 
  matchable returns the assigned option. It is intended as a workhorse for handling optional function 
  arguments.  

  \code{checkPEMS} checks a supplied data source and provides a short-cut for converting this 
  to and from \code{data.frames} and \code{pems} object classes. It is intended as a 'best-of-both-worlds' 
  mechanism, so users can supply data in various different formats, but function developers only 
  have to work with that data in one (known) format. 

  \code{checkUnits} checks the units of a previously recovered \code{input}, and then, depending on the 
  \code{output} setting, returns either the \code{units} of the \code{input} or the \code{input} in the 
  required \code{units} (assuming the associated conversion is known).

  \code{checkOutput} packs/repacks a previously recovered \code{input}. Depending on the \code{output} setting, this 
  can be as the (standalone) \code{input}, an element of a \code{data.frame} or an element of a \code{pems} object.  

  \code{checkIfMissing} if a workhorse function for the \code{if.missing} argument. If any of the supplied 
  additional arguments are \code{NULL}, it stops, warns and continues or continues a parent function according to 
  the \code{if.missing} argument. If supplied, \code{reply}, \code{suggest} and \code{if.warning} arguments are 
  used to generate the associated error or warning message.
 
}


\value{
  All \code{check...} functions return a logical if \code{output = "test.result"}, \code{TRUE} 
  if the \code{input}, \code{option}, etc., is suitable for use in that fashion or \code{FALSE} if not.      

  Otherwise,

  \code{checkOption} return the \code{option} argument if valid (on the basis of \code{if.missing}) or an 
  error, warning and/or \code{NULL} (on the basis of \code{if.missing}) if not. If \code{partial.match = TRUE} 
  and partial matching is possible this is in the full form given in \code{allowed.options} regardless of 
  the degree of abbreviation used by the user.

  \code{checkPEMS} returns the \code{data} argument if valid or an error, warning and/or \code{NULL} (on the basis 
  of \code{if.missing}) if not. Depending on \code{output} setting, the valid return is either a \code{data.frame} 
  or \code{pems} object.
  
  \code{checkUnits} returns the units of the \code{input} argument if no other information is supplied and 
  \code{units} have previously been assigned to that \code{input}. If \code{units} are assigned in the call 
  or \code{output} is forced (\code{output = "input"}), the \code{input} is returned in the requested 
  \code{units}. If this action is not possible (e.g. \code{pems.utils} does not know the conversion), the 
  function returns an error, a warning and the unchanged \code{input} or the unchanged \code{input} alone 
  depending on \code{if.missing} setting.

  Depening on \code{if.missing} argument, \code{checkIfMissing} either stops all parent functions with an error 
  message, warns of a problem but allows parent functions to continue running, or allows parent functions to 
  continue without informing the user. 

}
\references{
  [TO DO]
}
\author{
  Karl Ropkins
}
\note{
  The \code{...name} arguments allow the \code{check...} functions to be used silently. 
  If a parent function is identified as \code{fun.name} and the check case (\code{input}, 
  \code{option}, etc.) is identified with the associated \code{...name} argument these are 
  used in any associated error messaging.
 
}

%\section{Warning }{
%  None currently
%}

\seealso{
  See \code{\link{ref.unit.conversions}} and \code{\link{convertUnits}} for general unit handling. 
}
%\examples{
%
%###########
%##example 1 
%###########
%
%#TO DO
%}

\keyword{ methods }
