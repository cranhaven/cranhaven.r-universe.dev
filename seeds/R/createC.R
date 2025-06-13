createCFile <- function(parameters, inputs, Eq, bden, nnStates) {

  if (missing(bden)) {
    bden <- FALSE
  }

  trimSpace <- function(x) gsub("\\s", "", x)

  StringC <- '#include <R.h>'
  StringC = append(StringC, '#include <math.h>')


  # define parameters and inputs
  if (bden) {
    paraStr <- paste0('static double parms[', length(parameters) + inputs, '];')
  } else {
    paraStr <- paste0('static double parms[', length(parameters), '];')
  }


  inpStr <- paste0('static double forc[', inputs + 1, '];')
  StringC = append(StringC, values = c('', paraStr, inpStr))

  #define parameters
  if (is.null(names(parameters))) {
    para <- paste0('#define ', Eq@parameters, ' parms[', 1:length(Eq@parameters) - 1, ']')
    paraStringList <- Eq@parameters
  } else {
    para <- paste0('#define ', names(parameters), ' parms[', 1:length(parameters) - 1, ']')
    paraStringList <- names(parameters)
  }

  if (bden) {
    bdent0 = paste0("#define t0 parms[", length(parameters), "]")
    bdenwt0 = paste0("#define w", 1:(inputs - 1), 't0 parms[', (length(parameters) + 1):(length(parameters) + inputs - 1), ']')
    bdenPara = append(bdent0, bdenwt0)
    para = append(para, bdenPara)
  }

  #define inputs
  defInputU <- paste0('#define u forc[', 0, ']')
  defInputs <- paste0('#define w', 1:(inputs - 1), ' forc[', 1:(inputs - 1), ']')
  StringC = append(StringC, values = c('', para, '', defInputU, defInputs))


  tStr <- rep("", 5)
  # format the functions
  tStr[1] = "void parmsc(void (* odeparms)(int *, double *))"
  tStr[2] = "{"
  if (bden) {
    tStr[3] = paste0("\tint N=", length(parameters) + inputs, ';')
  } else {
    tStr[3] = paste0("\tint N=", length(parameters), ';')
  }
  tStr[4] = "\todeparms(&N, parms);"
  tStr[5] = "}"

  StringC = append(StringC, values = c('', tStr))

  tStr[1] = "void forcc(void (* odeforcs)(int *, double *))"
  tStr[3] = paste0("\tint N=", inputs, ';')
  tStr[4] = "    odeforcs(&N, forc);"

  StringC = append(StringC, values = c('', tStr))

  # the ode function
  startStr <- "void derivsc(int *neq, double *t, double *x, double *dx, double *yout, int *ip)\n{"
  if (bden) {
    conBden = "\tif(*t == t0){"
    t0Bden = paste0('\t\tw', 1:(inputs - 1), ' = w', 1:(inputs - 1), 't0;')
    t0Bden = append(t0Bden, "\t}")
    conBden = append(conBden, t0Bden)
    startStr = append(startStr, conBden)
  }

  formatIndeces <- function(eqC) {
    eqC = gsub(pattern = "(\\[[0-9]*)", replacement = "\\1 -1", eqC)
    eqC = unlist(lapply(X = eqC, FUN = Deriv::Simplify))
    eqC = gsub(pattern = "(x*\\[[0-9]*\\])\\^([a-z]+[0-9]+)", replacement = "pow(\\1,\\2)", eqC)
    eqC = gsub(pattern = "(x*\\[[0-9]*\\])\\^([0-9]+)+", replacement = "pow(\\1,\\2)", eqC)
    eqC = gsub(pattern = "(x*\\[[0-9]*\\])\\^([a-z]+)", replacement = "pow(\\1,\\2)", eqC)
    eqC = gsub(pattern = "([a-zA-Z]*[1-9]*)\\^([a-z]+[0-9]+)", replacement = "pow(\\1,\\2)", eqC)
    # add cases for exponent
    eqC = gsub(pattern = "exp\\((x\\[[0-9]*\\])\\)\\^([a-z]+[0-9])+", replacement = "pow(exp(\\1),\\2)", eqC)
    eqC = gsub(pattern = "exp\\((x\\[[0-9]*\\])\\)\\^([0-9]+)+", replacement = "pow(exp(\\1),\\2)", eqC)
    eqC = gsub(pattern = "exp\\((x\\[[0-9]*\\])\\)\\^([a-z]+)", replacement = "pow(exp(\\1),\\2)", eqC)

    eqC = gsub("([\\+\\-\\*/])", " \\1 ", eqC)
    eqC = gsub("([^a-z1-9\\s]+)(t)([^a-zA-Z1-9]+)", "\\1*\\2\\3", eqC)

    return(eqC)
  }

  if (length(Eq@cond) > 0) {
    reformCond <- function(x) {
      Str <- c(x[1], paste0(x[-1]), '}\n')
      return(Str)
    }

    conditions <- unlist(lapply(Eq@cond, function(x) reformCond(x)))
    # conditions = gsub(pattern = "(\\[[0-9]*)", replacement = "\\1 -1", conditions)
    ifCond <- conditions[grepl(pattern = 'if', conditions)]
    ifId <- which(conditions %in% ifCond)


    # reformat the if statement
    ifCond = gsub(pattern = "if", replacement = "", x = ifCond)
    ifCond = formatIndeces(ifCond)
    ifCond = paste0('if(', ifCond, '){')

    # reformat the rest
    consq <- conditions[!grepl(pattern = 'if', conditions)]
    consq = consq[!grepl(pattern = "\\}", consq)]
    consqId <- which(conditions %in% consq)

    consq = formatIndeces(consq)
    consq = paste0('\t', consq, ';')

    conditions[ifId] = ifCond
    conditions[consqId] = consq
    conditions = paste0('\t', conditions)

  }
  
  eqC <- gsub(pattern = "^(d*[x])([0-9]+)", replacement = "\\1[\\2]", Eq@origEq)
  eqC <- gsub(pattern = "([^a-zA-Z0-9]d*[x])([0-9]+)", replacement = "\\1[\\2]", eqC)

  eqC = formatIndeces(eqC)
  eqC = paste0("\t", eqC, "+w", 1:length(eqC), ";")

  if (length(Eq@cond) > 0) {
    StringC = append(StringC, values = c(startStr, conditions, " ", eqC, '}'))
  } else {
    StringC = append(StringC, values = c(startStr, eqC, '}'))
  }
  
  # print(StringC)

  if (sum(nnStates) > 0) {
    rootFuncStr = createCLangRoot(nnStates)

    StringC = append(StringC, rootFuncStr)
  }


  writeFileC <- function(string) {
    if (.Platform$OS.type != "windows"){
      temp_file_path <- paste0(tempdir(),'/','model.c')
    } else {
      temp_file_path <- paste0(tempdir(),'\\','model.c')
    }
    file.create(temp_file_path)
    fileC <- file(temp_file_path)
    writeLines(string, fileC)
    close(fileC)
  }

  writeFileC(StringC)
}

#' Create compilable c-code of a model
#' 
#' Writes a c file that can be compiled for faster solution with the \code{\link[deSolve]{ode}} solver.
#' The file created is formatted to be used with the dynamic elastic net. A hidden input is 
#' added to every component of the state vector.
#' 
#' @note On the usage of compiled code in conjunction with \pkg{deSolve} take a look into the vignette 'R
#' Package deSolve, Writing Code in Compiled Languages' of the package.
#' 
#' @param modelFunc a R-function that can be solved with deSolve. External input of the system should
#'                  be declared with 'u'. To ensure that the function is working use the most general
#'                  state-space representation.
#'
#' @param parameters a vector describing the parameters of the system. If names are missing the function
#'                   tries to extract the declared parameters from the model function. 
#'                   
#' @param bden a boolean that indicates if the c-file is used for the mcmc algorithm, default value is 'FALSE'
#'
#' @param nnStates a bit vector indicating the states that should be non negative
#'
#' @return None
createCompModel <- function(modelFunc, parameters, bden, nnStates) {


  odeEq <- new("odeEquations")
  odeEq <- createModelEqClass(odeEq, modelFunc)

  if (missing(nnStates) || sum(nnStates) == 0) {
    nnStates <- rep(0, length(odeEq@origEq))
  }

  numInputs = length(odeEq@origEq) + 1

  createCFile(parameters = parameters, inputs = numInputs, odeEq, bden, nnStates)
}
