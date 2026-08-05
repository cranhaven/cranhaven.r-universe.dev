.onLoad <- function(libname, pkgname){

  libRuntimeName = system.file("redengine", "", package = pkgname);

  invisible(.Call(`_minired_redatam_init_`, libRuntimeName))

  assign('outputs', new.env(), envir = parent.env(environment()))
}

.onUnload<-function(libpath) {
  invisible(.Call(`_minired_redatam_destroy_`))
}

.onAttach<-function(libname, pkgname) {
  packageStartupMessage( paste("Using: ", redatam_version()) )
  packageStartupMessage(
        "WARNING: This package is deprecated and may be removed in future releases. Please use [redatamx] instead."
    )
}
