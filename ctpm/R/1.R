# Universal stuff that needs to be run first
methods::setOldClass("vg")

new.vg <- methods::setClass("vg",representation=methods::representation("data.frame",info="list"),
                                   prototype=methods::prototype(data.frame(),info=list()) )

#Get unexported functions from ctmm
PDsolve <- get("PDsolve",envir=environment(ctmm::ctmm))
unit.ctmm <- get("unit.ctmm",envir=environment(ctmm::ctmm))
#plot.variogram <- get("plot.variogram",envir=environment(ctmm::ctmm))
new.variogram <- get("new.variogram",envir=environment(ctmm::ctmm))