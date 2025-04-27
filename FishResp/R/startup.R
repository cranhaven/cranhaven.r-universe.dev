# Startup message
#
# Startup text message linked to the publication describing the 'FishResp' package
#' @importFrom utils packageDescription

.onAttach<- function (lib, pkg){
  if(interactive()){
    pkg.version = packageDescription("FishResp", fields = "Version")
    startup.txt = paste("
    ==========================              o         #########      #
    FishResp", pkg.version, "is loaded             o        ##########       ##
    ==========================            o     ###  #########   ###
                                            o  ####################
    For a description of its performance         #############    ##
    & to cite FishResp please refer to:              # #           #

    Morozov, S., McCairns, R.J.S., Merila, J. (2019) FishResp: R package
    and GUI application for analysis of aquatic respirometry data.
    Conserv Physiol 7(1): coz003; https://doi.org/10.1093/conphys/coz003\n
    ======================================================================\n")
    packageStartupMessage(startup.txt)
  }
}
