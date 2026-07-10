#' Write data to Excel file
#' write or append one or more data into one Excel file in each sheet.
#' @param ... one or more data
#' @param file Excel file name
#' @param sheet sheet names
#' @param col.names logical, whether to write out column names
#' @param row.names logical, whether to write out row names
#' @param overwrite logical, whether to overwrite an existing file
#' @param append logical, whether to add data to an existing file
#'
#' @return write one or more data into one Excel file
#' @export
#'
#' @examples
#' \donttest{
#' mtcars2 = mtcars
#' # write_xlsx(mtcars,mtcars2,file='mtcars')
#' }
write_xlsx <- function(...,file,sheet,col.names=TRUE,row.names=FALSE,overwrite=FALSE,append=FALSE){
    if (!right_equal(file,'.xlsx')) file <- fmt('/ .xlsx',file)
    msg <- ifelse(cnOS(),
                  tmcn::toUTF8(fmt("/ \u5DF2\u5B58\u5728",file)),
                  fmt('/ already exists.'))
    if (file.exists(file) & all(!overwrite, !append)) stop(msg)
    if (missing(sheet)) sheet <- get_names(...)
    data <- list(...)
    if (append){
        overwrite=TRUE
        wb <- openxlsx::loadWorkbook(file = file)
    }else{
        wb <- openxlsx::createWorkbook()
    }
    for(i in seq_range(sheet)){
        openxlsx::addWorksheet(wb = wb, sheetName = sheet[i])
        openxlsx::writeData(wb = wb, sheet = sheet[i], x = data[[i]],
                            colNames = col.names,rowNames  = row.names)
    }
    openxlsx::saveWorkbook(wb = wb,file = file,overwrite = overwrite)
}


