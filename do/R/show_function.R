
#' Show function command line in new script
#' script will be store in your temporary directory
#' @param f one function
#' @param file file name
#' @return command line in new script
#' @export
#'
show_function <- function(f,file=NULL){
    args <- formals(f,envir = .GlobalEnv)
    argl <- lapply(args, function(i) deparse(i))
    argl[sapply(argl, function(i) nchar(i))>0] <- paste('=',argl[sapply(argl, function(i) nchar(i))>0])
    argc <- paste(names(argl),argl)
    name <- deparse(substitute(f))
    argcc <- c('',name,'',
               '######## arguments #########','',
               argc,
               '',
               '######## body #########',
               '')

    argb <- c(argcc,deparse(body(f)))
    dirshow <- paste0(do::Replace0(tempdir(),'Rtmp.*'),'do-show_function')
    if (!dir.exists(dirshow)) dir.create(dirshow)
    fl <- paste0(dirshow,'/',name) |>
        gsub(pattern = ':{2,}',replacement = '-') |>
        paste0('.R')
    fl
    i=1
    while(file.exists(fl)){
        if (i == 1){
            fl <- paste0(do::knife_right(fl,2),'-i','.R')
        }else{
            fl <- paste0(do::knife_right(fl,2),'i','.R')
        }

        i <- i+1
    }
    if (is.null(file)) file <- fl
    if (tolower(do::right(file,2)) != '.r') file=paste0(file,'.R')
    writeLines(argb,file)
    usethis::edit_file(file)
}




