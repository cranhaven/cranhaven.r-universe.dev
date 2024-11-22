
# fill in the gml structure
merlin_setup <- function(model,intmethod,ip,data,timevar,
                         family,link,covariance,userf,from,levels,
                         debug,sweights){

    #initialise
    gml             <- list()
    gml$data        <- data
    gml$Nmodels     <- Nmodels <- length(model)
    gml$levels      <- levels
    gml$Nlevels     <- Nlevels <- length(levels)
    gml$family      <- family
    gml$modelind    <- gml$modtouse <- 1                                #gets updated
    gml$fixedonly   <- FALSE
    gml$Nap         <- rep_len(0,Nmodels)
    gml$hasbh       <- rep_len(0,Nmodels)
    gml$bhazard     <- rep_len("",Nmodels)
    gml$NotNull     <- rep_len(1,Nmodels)
    gml$hasexposure <- rep_len(0,Nmodels)
    gml$exposure    <- rep_len("",Nmodels)

    gml = merlin_setup_get_ys(gml,model)
    gml = merlin_setup_links(gml,link)
    gml = merlin_setup_dap(gml)
    # gml = merlin_get_cluster_varnames(gml,model) -> needed if auto detecting level variables

    Ntimevars  = length(timevar)
    if (Ntimevars > 0) {
        if (Ntimevars < Nmodels) gml$timevar <- rep_len(timevar,Nmodels)
        else                     gml$timevar <- timevar
    }

    gml = merlin_setup_touses(gml,data,model)
    gml = merlin_setup_sweights(gml,sweights)
    if (gml$Nlevels) {
        gml = merlin_parse_unique_latents1(gml,model)
        gml = merlin_parse_unique_latents2(gml,model)
        gml = merlin_setup_panelvars(gml)
    }
    gml = merlin_setup_cp(gml,model)
    gml = merlin_setup_ch_ip(gml)

    if (Nlevels) {
        qind = vector()                                              #gets updated
        gml  = merlin_setup_vcv(gml,covariance)
        if (Nlevels>1 & length(intmethod)==1) intmethod = rep_len(intmethod,Nlevels)
        gml$intmethod = intmethod
        gml           = merlin_setup_ip(gml,ip)
    }

    gml = merlin_setup_labels(gml)
    gml = merlin_setup_userf(gml,userf)
    gml = merlin_setup_logl(gml)

    # done
    return(gml)
}

merlin_get_GK <- function()
{
    gq <- list()
    gq[[1]]	<- matrix(c(0.991455371120813,-0.991455371120813,0.949107912342759,-0.949107912342759,0.864864423359769,-0.864864423359769,0.741531185599394,-0.741531185599394,0.586087235467691,-0.586087235467691,0.405845151377397,-0.405845151377397,0.207784955007898,-0.207784955007898,0),nrow=1)
    gq[[2]] <- matrix(c(0.022935322010529,0.022935322010529,0.063092092629979,0.063092092629979,0.104790010322250,0.104790010322250,0.140653259715525,0.140653259715525,0.169004726639267,0.169004726639267,0.190350578064785,0.190350578064785,0.204432940075298,0.204432940075298,0.209482141084728),nrow=1)
    gq
}
