#' \code{airdas_dfr} class
#'
#' @description The \code{airdas_dfr} class is a subclass of \code{\link[base]{data.frame}},
#'   created to provide a concise and robust way to ensure that the input 
#'   to \code{\link{airdas_process}}adheres to certain requirements.
#'   Specifically, objects of class \code{airdas_dfr} are data frames with specific column names and classes,
#'   as detailed in the 'Properties of \code{airdas_dfr}' section. 
#'   Objects of class \code{airdas_dfr} are created by
#'   \code{\link{airdas_read}} or \code{\link{as_airdas_dfr}}, 
#'   and are intended to be passed directly to \code{\link{airdas_process}}.
#'
#'   Subsetting or otherwise altering an object of class \code{airdas_dfr} will cause 
#'   the object to drop its \code{airdas_dfr} class attribute, although note that combining
#'   two \code{airdas_dfr} objects using \code{\link[base:cbind]{rbind}} 
#'   will return an object with a \code{airdas_dfr} class attribute.
#'   \code{\link{airdas_process}} will then try to coerce the object to a
#'   \code{airdas_dfr} object. It is \bold{strongly} recommended to pass an object of class \code{airdas_dfr} to
#'   \code{\link{airdas_process}} before subsetting, e.g. for events from a certain date range.
#'
#' @section Properties of \code{airdas_dfr} objects:
#'
#'   Objects of class \code{airdas_dfr} have a class attribute of \code{c("airdas_dfr", "data.frame")}.
#'   They must have a column \code{file_type} where all values are one of: 
#'   "turtle", "caretta", "survey", or "phocoena" 
#'   (case sensitive; see \code{\link{airdas_read}} for more details). 
#'   \code{airdas_dfr} objects also must not have any \code{NA} event codes.
#'   
#'   In addition, they must have the following column names and classes:
#'   \tabular{ll}{
#'     \emph{Column name} \tab \emph{Column class}\cr
#'     Event     \tab "character"\cr
#'     EffortDot \tab "logical"\cr
#'     DateTime  \tab c("POSIXct", "POSIXt")\cr
#'     Lat       \tab "numeric"\cr
#'     Lon       \tab "numeric"\cr
#'     Data1 \tab "character"\cr
#'     Data2 \tab "character"\cr
#'     Data3 \tab "character"\cr
#'     Data4 \tab "character"\cr
#'     Data5 \tab "character"\cr
#'     Data6 \tab "character"\cr
#'     Data7 \tab "character"\cr
#'     EventNum  \tab "character"\cr
#'     file_das  \tab "character"\cr
#'     line_num  \tab "integer"\cr
#'     file_type \tab "character"\cr
#'   }
#'
#' @name airdas_dfr-class
#' @aliases airdas_dfr airdas_dfr-class
#' @seealso \code{\link{as_airdas_dfr}}
NULL



#' \code{airdas_df} class
#'
#' @description The \code{airdas_df} class is a subclass of \code{\link[base]{data.frame}},
#'   created to provide a concise and robust way to ensure that the input to 
#'   downstream AirDAS processing functions, such as \code{\link{airdas_sight}}, 
#'   adheres to certain requirements.
#'   Specifically, objects of class \code{airdas_df} are data frames with specific column names and classes,
#'   as detailed in the 'Properties of \code{airdas_df}' section. 
#'   In addition, \code{airdas_df} objects have no \code{NA} values 
#'   in the 'Lat' 'Lon', or 'DateTime' columns.
#'   Objects of class \code{airdas_df} are created by \code{\link{airdas_process}} 
#'   or \code{\link{as_airdas_df}}, and are intended to be passed directly to
#'   DAS processing functions such as \code{\link{airdas_sight}}.
#'
#'   Subsetting, say for a specific date or transect number, 
#'   or otherwise altering an object of class \code{airdas_df}
#'   will cause the object to drop its \code{airdas_df} class attribute, although note 
#'   that combining two \code{airdas_df} objects using \code{\link[base:cbind]{rbind}} 
#'   will return an object with a \code{airdas_df} class attribute.
#'   If this object is then passed to a DAS processing function such as \code{\link{airdas_sight}}, 
#'   the function will try to coerce the object to a \code{airdas_df} object.
#'
#' @section Properties of \code{airdas_df} objects:
#'
#'   Objects of class \code{airdas_df} have a class attribute of \code{c("airdas_df", "data.frame")}.
#'   All values in the OnEffort column must be \code{TRUE} or \code{FALSE} (no \code{NA} values).
#'   All on effort events must have non-\code{NA} Lat/Lon/DateTime values, and 
#'   there must be no events with a "#" event code (deleted event). 
#'   Like \code{airdas_dfr} events, there must be a \code{file_type} column where 
#'   all values are one of: "turtle", "caretta", "survey", or "phocoena" 
#'   (case sensitive; see \code{\link{airdas_read}} for more details about file types). 
#'   
#'   In addition, \code{airdas_df} objects must have the following column names and classes:
#'   \tabular{ll}{
#'     \emph{Column name} \tab \emph{Column class}\cr
#'     Event     \tab "character"\cr
#'     DateTime  \tab c("POSIXct", "POSIXt")\cr
#'     Lat       \tab "numeric"\cr
#'     Lon       \tab "numeric"\cr
#'     OnEffort  \tab "logical"\cr
#'     Trans    \tab "character"\cr
#'     Bft      \tab "numeric"\cr
#'     CCover   \tab "numeric"\cr
#'     Jelly    \tab "numeric"\cr
#'     HorizSun \tab "numeric"\cr
#'     VertSun  \tab "numeric"\cr
#'     HKR      \tab "character"\cr
#'     Haze     \tab "logical"\cr
#'     Kelp     \tab "logical"\cr
#'     Red tide \tab "logical"\cr
#'     AltFt  \tab "numeric"\cr
#'     SpKnot \tab "numeric"\cr
#'     ObsL   \tab "character"\cr
#'     ObsB   \tab "character"\cr
#'     ObsR   \tab "character"\cr
#'     Rec    \tab "character"\cr
#'     VLI    \tab "character"\cr
#'     VLO    \tab "character"\cr
#'     VB     \tab "character"\cr
#'     VRI    \tab "character"\cr
#'     VRO    \tab "character"\cr
#'     Data1 \tab "character"\cr
#'     Data2 \tab "character"\cr
#'     Data3 \tab "character"\cr
#'     Data4 \tab "character"\cr
#'     Data5 \tab "character"\cr
#'     Data6 \tab "character"\cr
#'     Data7 \tab "character"\cr
#'     EffortDot \tab "logical"\cr
#'     EventNum  \tab "character"\cr
#'     file_das  \tab "character"\cr
#'     line_num  \tab "integer"\cr
#'     file_type \tab "character"\cr
#'   }
#'
#' @name airdas_df-class
#' @aliases airdas_df airdas_df-class
#' @seealso \code{\link{as_airdas_df}}
NULL
