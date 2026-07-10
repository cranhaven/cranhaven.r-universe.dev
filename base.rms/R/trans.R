#' @importFrom rms rcs asis pol catg scored matrx strat
#' @importFrom stats poly
#' @importFrom survival strata
trans.base2rms <- function(call.new){
    call.new=gsub('I\\(','asis(',call.new)
    call.new=gsub('ns\\(','rcs(',call.new)
    call.new=gsub('poly\\(','pol(',call.new)
    call.new=gsub('factor\\(','catg(',call.new)
    call.new=gsub('ordered\\(','scored(',call.new)
    call.new=gsub('matrix\\(','matrx(',call.new)
    call.new=gsub('strata\\(','strat(',call.new)
    call.new
}
trans.rms2base <- function(call.new){
    if (grepl('lsp\\(',call.new)) stop('We can not trans lsp() in rms to base function')
    call.new=gsub('asis\\(','I(',call.new)
    call.new=gsub('rcs\\(','ns(',call.new)
    call.new=gsub('pol\\(','poly(',call.new)
    call.new=gsub('catg\\(','factor(',call.new)
    call.new=gsub('scored\\(','ordered(',call.new)
    call.new=gsub('matrx\\(','matrix(',call.new)
    call.new=gsub('strat\\(','strata(',call.new)
    call.new
}
