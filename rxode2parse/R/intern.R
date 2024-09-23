.codeLoaded <- function() {
  .Call(`_rxode2parse_codeLoaded`)
}

.codegen <- function(c_file, prefix, libname, pMd5, timeId, lastMv, goodFun) {
  .Call(`_rxode2parse_codegen`, c_file, prefix, libname, pMd5, timeId, lastMv, goodFun)
}

.parseModel <- function(type) {
  .Call(`_rxode2parse_parseModel`, type)
}

.isLinCmt <- function() {
  .Call(`_rxode2parse_isLinCmt`)
}

.trans <- function(parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn) {
  .Call(`_rxode2parse_trans`,
        parse_file, prefix, model_md5, parseStr, isEscIn, inME, goodFuns, fullPrintIn)
}

.linCmtParse <- function(vars, inStr, verbose) {
  .Call(`_rxode2parse_linCmtParse`, vars, inStr, verbose)
}

.linCmtGen <- function(linCmt, vars, linCmtSens, verbose) {
  .Call(`_rxode2parse_linCmtGen`, linCmt, vars, linCmtSens, verbose)
}

.parseFreeSexp  <- function(last) {
  .Call(`_rxode2parse_parseFreeSexp`, last)
}

.calcDerived <- function(ncmtSXP, transSXP, inp, sigdigSXP) {
  .Call(`_rxode2parse_calcDerived`, ncmtSXP, transSXP, inp, sigdigSXP)
}
