#' @import TableContainer 
#' @import osfr
#' @import methods
#' @importFrom ramify pprint
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom stats sd
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_line aes labs scale_y_continuous theme element_text
#' @importFrom ggtext element_markdown
#' @importFrom gsignal resample
#' @importFrom stats reshape
#' @export nrow
#' @export ncol
#' @export rownames
#' @export colnames
#' @export rowData
#' @export colData
#' @export metaData
#' @export tblData
#' @export tblData<-
#' @export rowData<-
#' @export colData<-
#' @export metaData<-
#' 
NULL

pkg_global <- new.env(parent = emptyenv())

# Default project list as fallback
.default_project_list <- list(
    fragility = "7rnft"
)

.config_version <- "v1"

# Global project list that will be updated
pkg_global$.project_list <- .default_project_list

# Flag to track if we've attempted to fetch remote config
pkg_global$.config_fetched <- FALSE