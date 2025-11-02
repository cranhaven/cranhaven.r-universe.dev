#' Installs the needed packages in JULIA to run the EM algorithm for rHMM.
#'
#' @param JULIA_HOME the file folder which contains julia binary, if not set, JuliaCall will look at the global option JULIA_HOME, if the global option is not set, JuliaCall will then look at the environmental variable JULIA_HOME, if still not found, JuliaCall will try to use the julia in path.
#'
#' @return empty
#' @usage setupJulia(JULIA_HOME = NULL)
#'
#' @export setupJulia
setupJulia = function(JULIA_HOME = NULL){
  if(!is.null(JULIA_HOME)) julia_setup(JULIA_HOME = JULIA_HOME)
  v = julia_eval("string(VERSION)")
  # v = unlist(strsplit(v, split = "[.]"))
  # v = as.numeric(paste(v[1:2], collapse = "."))
  if(v != "1.0.5") cat("WARNING: Your Julia version is different than 1.0.5.\nWe recommend to use 1.0.5 to improve speeed. Using other versions might give problems or do not work on higher speed.")
  julia_install_package_if_needed("Optim")
  julia_install_package_if_needed("Distributions")
  julia_install_package_if_needed("LinearAlgebra")
  julia_install_package_if_needed("CSV")
  julia_install_package_if_needed("DelimitedFiles")
  julia_install_package_if_needed("DataFrames")
  # julia_install_package_if_needed("Plots")
}

#' Function needed before using RTIGER() function. It loads the scripts in Julia that fit the rHMM.
#'
#' @return empty
#' @export sourceJulia
#'

sourceJulia = function(){
  julia_source(paste(system.file("julia", package = "RTIGER"),"/AuxilaryFunctions.jl", sep = ""))
  julia_source(paste(system.file("julia", package = "RTIGER"), "/rHMM_methods.jl", sep =""))
  # julia_source(paste(system.file("julia", package = "RTIGER"), "/rHMM_methods_old.jl", sep =""))

}
