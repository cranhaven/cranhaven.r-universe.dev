#' \code{das_dfr} class
#'
#' @description The \code{das_dfr} class is a subclass of \code{\link[base]{data.frame}},
#'   created to provide a concise and robust way to ensure that the input to \code{\link{das_process}}
#'   adheres to certain requirements.
#'   Specifically, objects of class \code{das_dfr} are data frames with specific column names and classes,
#'   as detailed in the 'Properties of \code{das_dfr}' section. Objects of class \code{das_dfr} are created by
#'   \code{\link{das_read}} or \code{\link{as_das_dfr}}, and are intended to be passed directly to
#'   \code{\link{das_process}}.
#'
#'   Subsetting or otherwise altering an object of class \code{das_dfr} will cause the object to drop its
#'   \code{das_dfr} class attribute. \code{\link{das_process}} will then try to coerce the object to a
#'   \code{das_dfr} object. It is \bold{strongly} recommended to pass an object of class \code{das_dfr} to
#'   \code{\link{das_process}} before subsetting, e.g. for events from a certain date range.
#'
#' @section Properties of \code{das_dfr} objects:
#'
#'   Objects of class \code{das_dfr} have a class attribute of \code{c("das_dfr", "data.frame")}.
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
#'     Data8 \tab "character"\cr
#'     Data9 \tab "character"\cr
#'     Data10 \tab "character"\cr
#'     Data11 \tab "character"\cr
#'     Data12 \tab "character"\cr
#'     EventNum \tab "integer"\cr
#'     file_das \tab "character"\cr
#'     line_num \tab "integer"\cr
#'   }
#'
#' @name das_dfr-class
#' @aliases das_dfr das_dfr-class
#' @seealso \code{\link{as_das_dfr}}
NULL



#' \code{das_df} class
#'
#' @description The \code{das_df} class is a subclass of \code{\link[base]{data.frame}},
#'   created to provide a concise and robust way to ensure that the input to downstream DAS processing functions,
#'   such as \code{\link{das_sight}}, adheres to certain requirements.
#'   Specifically, objects of class \code{das_df} are data frames with specific column names and classes,
#'   as detailed in the 'Properties of \code{das_df}' section. Objects of class \code{das_df} are created by
#'   \code{\link{das_process}} or \code{\link{as_das_df}}, and are intended to be passed directly to
#'   DAS processing functions such as \code{\link{das_sight}}.
#'
#'   Subsetting, say for a specific date or cruise number, or otherwise altering an object of class \code{das_df}
#'   will cause the object to drop its \code{das_df} class attribute.
#'   If this object is then passed to a DAS processing function such as \code{\link{das_sight}}, the function
#'   will try to coerce the object to a \code{das_df} object.
#'
#' @section Properties of \code{das_df} objects:
#'
#'   All values in the Event column must not be \code{NA}.
#'
#'   Objects of class \code{das_df} have a class attribute of \code{c("das_df", "data.frame")}.
#'   In addition, they must have the following column names and classes:
#'   \tabular{ll}{
#'     \emph{Column name} \tab \emph{Column class}\cr
#'     Event    \tab "character"\cr
#'     DateTime \tab c("POSIXct", "POSIXt")\cr
#'     Lat      \tab "numeric"\cr
#'     Lon      \tab "numeric"\cr
#'     OnEffort  \tab "logical"\cr
#'     Cruise    \tab "numeric"\cr
#'     Mode      \tab "character"\cr
#'     EffType   \tab "character"\cr
#'     Course    \tab "numeric"\cr
#'     SpdKt     \tab "numeric"\cr
#'     Bft       \tab "numeric"\cr
#'     SwellHght \tab "numeric"\cr
#'     WindSpdKt \tab "numeric"\cr
#'     RainFog   \tab "numeric"\cr
#'     HorizSun  \tab "numeric"\cr
#'     VertSun   \tab "numeric"\cr
#'     Glare     \tab "logical"\cr
#'     Vis       \tab "numeric"\cr
#'     ObsL      \tab "character"\cr
#'     Rec       \tab "character"\cr
#'     ObsR      \tab "character"\cr
#'     ObsInd    \tab "character"\cr
#'     Data1 \tab "character"\cr
#'     Data2 \tab "character"\cr
#'     Data3 \tab "character"\cr
#'     Data4 \tab "character"\cr
#'     Data5 \tab "character"\cr
#'     Data6 \tab "character"\cr
#'     Data7 \tab "character"\cr
#'     Data8 \tab "character"\cr
#'     Data9 \tab "character"\cr
#'     Data10 \tab "character"\cr
#'     Data11 \tab "character"\cr
#'     Data12 \tab "character"\cr
#'     EffortDot \tab "logical"\cr
#'     EventNum \tab "integer"\cr
#'     file_das \tab "character"\cr
#'     line_num \tab "integer"\cr
#'   }
#'
#' @name das_df-class
#' @aliases das_df das_df-class
#' @seealso \code{\link{as_das_df}}
NULL
