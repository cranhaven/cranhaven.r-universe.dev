#' Add biocViews Field to Description File
#'
#' @param value package names
#' @param overwrite logical, TRUE is defaulted
#' @importFrom usethis ui_done ui_field ui_value proj_get
#' @export
#'
add_biocViews <- function(value='',overwrite=TRUE){
    value <- as.character(value)
    curr <- desc::desc_get('biocViews', file = proj_get())[[1]]
    curr <- gsub("^\\s*|\\s*$", "", curr)
    if (nchar(value)[1] == 0){
        ui_done("Setting {ui_field('biocViews')} field in DESCRIPTION")
    }else{
        ui_done("Setting {ui_field('biocViews')} field in DESCRIPTION to {ui_value(value)}")
    }
    desc::desc_set('biocViews', value, file = proj_get())
    invisible()
}
