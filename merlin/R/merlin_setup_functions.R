
merlin_setup_get_ys <- function(gml,model)
{
    y <- list()
    for (k in 1:gml$Nmodels) {
        if (gml$family[k]=="null") {
            gml$NotNull[k] = 0
        }
        else {
            modk <- strsplit(as.character(model[[k]]),split='~', fixed=TRUE)
            if (gml$family[k] == "gaussian" | gml$family[k] == "bernoulli"
                | gml$family[k] == "poisson" | gml$family[k] == "user"
                | gml$family[k] == "beta" | gml$family[k] == "negbinomial") {
                y[[k]] <- merlin_trim(unlist(modk)[2])
            }
            else {  #survival outcomes
                y[[k]] <- c(as.character(model[[k]][[2]][[2]]),as.character(model[[k]][[2]][[3]]))
            }
        }
    }
    gml$y = y
    return(gml)
}

merlin_setup_links <- function(gml,link)
{
    Nlinks   <- length(link)
    links    <- rep_len("identity",gml$Nmodels)
    invlinks <- rep_len("merlin_identity",gml$Nmodels)
    if (!Nlinks) {
        for (m in 1:gml$Nmodels) {
            if (gml$family[m]=="bernoulli" | gml$family[m]=="beta") {
                links[m]    <- "logit"
                invlinks[m] <- "merlin_invlogit"
            }
            else if (gml$family[m]=="poisson" | gml$family[m]=="negbinomial") {
              links[m]    <- "log"
              invlinks[m] <- "merlin_exp"
            }
        }
    }
    else {
        if (Nlinks>0 & Nlinks!=gml$Nmodels) stop("link must be specified for every model")

        for (l in 1:Nlinks) {
            if      (link[l]=="identity") {
            }
            else if (link[l]=="log") {
                links[m]    <- "log"
                invlinks[m] <- "merlin_exp"
            }
            else if (link[l]=="logit") {
                links[m]    <- "logit"
                invlinks[m] <- "merlin_invlogit"
            }
            else stop("Unknown link function")
        }
    }

    gml$links    <- links
    gml$invlinks <- invlinks
    return(gml)
}

merlin_setup_dap <- function(gml)
{
    Ndap = rep(0,gml$Nmodels)
    for (m in 1:gml$Nmodels) {
        if (gml$family[m] == "gaussian" | gml$family[m] == "weibull"
            | gml$family[m] == "gompertz" | gml$family[m] == "beta"
            | gml$family[m] == "negbinomial") {
            Ndap[m] = 1
        }
    }
    gml$Ndap = Ndap
    return(gml)
}

# merlin_get_cluster_varnames <- function(gml,model)
# {
#
#     # get unique cluster spec's within []
#     gml$levelvars1 = ""											#first row contains level name
#     gml$levelvars2 = ""                     #second row contains M/MV#[] as well
#
#     #get any level specs within [] - includes pipes
#     idspec = NULL
#     for (m in 1:gml$Nmodels) {
#
#         rhs   = merlin_setup_get_rhs(model[[m]])
#         cmps  = merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))
#         Ncmps = length(cmps)
#         for (c in 1:Ncmps) {
#           dv 		= merlin_trim(cmps[c])
#           pos1 	= merlin_strpos("[",dv)
#           pos2  = merlin_strpos("]",dv)
#           check = merlin_strpos("XB",dv) | merlin_strpos("EV",dv) | merlin_strpos("rcs(",dv) | merlin_strpos("fp(",dv)
#           if (pos1 & !check) {
#               idspec = c(idspec,substr(dv,pos1+1,pos2-1))
#           }
#         }
#     }
#
#     return(gml)
# }

merlin_setup_touses <- function(gml,data,model)
{
    # work out the complete case data for each model
    gml$datause = list()
    gml$Nobs    = vector()
    for (m in 1:gml$Nmodels) {
        vars             = merlin_get_variables(gml,model[m])
        if (gml$NotNull[m]) vars = c(vars,gml$y[[m]])
        vars             = unique(c(vars,gml$timevar[m],gml$levels,gml$sweights))
        gml$datause[[m]] = stats::complete.cases(data[,vars])
        gml$Nobs[m]      = length(data[gml$datause[[m]],1])
    }

    return(gml)
}

merlin_get_variables <- function(gml,model)
{
    vars = NULL
    rhs   = merlin_setup_get_rhs(model)
    cmps  = merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))
    Ncmps = length(cmps)
    for (c in 1:Ncmps) {
      dv 		= merlin_trim(cmps[c])
      pos 	= merlin_strpos("[",dv)
      v2 = NULL
      if (pos) v2 = substr(dv,pos+1,merlin_strpos("]",dv)-1)
      else if (merlin_strpos("rcs(",dv) | merlin_strpos("fp(",dv) | merlin_strpos("srcs(",dv)) {
          if (merlin_strpos("var=",dv) | merlin_strpos("var =",dv)) {
              v2 = substr(dv,merlin_strpos("=",dv)+1,merlin_strpos(",",dv)-1)
          }
          else v2 = substr(dv,merlin_strpos("(",dv)+1,merlin_strpos(",",dv)-1)
      }
      vars = c(vars,v2)
    }
    return(vars)
}

# merlin_setup_levels <- function(gml,model)
# {
#     # unique latents at each level
#     latlevs = list()
#     Nres    = rep_len(0,gml$Nlevels)
#
#     for (l in 1:gml$Nlevels) {
#         # lats = J(0,1,"")
#         # go through everything for each level
#         for (m in 1:gml$Nmodels) {
#             rhs   = merlin_setup_get_rhs(model[[m]])
#             cmps  = merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))
#             Ncmps = length(cmps)
#             for (c in 1:Ncmps) {
#                 dv = merlin_trim(cmps[c])
#                 pos = 1
#                 while (pos) {
#                     pos = merlin_strpos(dv,":")
#                     if (pos) {
#                         dv2 = substr(dv,1,pos-1)
#                         dv = substr(dv,start=pos+1)
#                     }
#                     else dv2 = dv
#                     posid = strpos(dv2,gml$levelvars[2,i])
#                     if (posid) lats = lats\substr(dv2,1,posid-1)
#                 }
#             }
#         }
#         lats 		= uniqrows(lats)	#they could appear in multiple models and multiple times in same model
#         gml.Nres[i] = rows(uniqrows(lats))
#         asarray(gml.latlevs,i,lats)
#     }
#
#
#
#   Ncmps   = rep(0,gml$Nmodels)
#   cmps    = list()               #components for each model
#   Nels    = list()               #number of elements in each component, by model
#   els     = list()               #elements for each model and component
#   eltype  = list()
#   hascons = rep(1,gml$Nmodels)
#   gml$elinfo  = list()
#
#   for (m in 1:gml$Nmodels) {
#
#     rhs = merlin_setup_get_rhs(model[[m]])
#
#     # split into components
#     cmps[[m]]   = merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))
#     Ncmps[m]    = length(cmps[[m]])
#     Nels_m      = rep_len(0,Ncmps[m])
#     els[[m]]    = list()
#     eltype[[m]] = list()
#     gml$elinfo[[m]] = list()
#
#     for (c in 1:Ncmps[m]) {
#       # split into elements
#       els[[m]][[c]]         = unlist(strsplit(cmps[[m]][c],split=":",fixed=TRUE))
#       mcels                 = els[[m]][[c]]
#       Nels_m[c]             = length(els[[m]][c])
#       eltype[[m]][[c]]      = rep_len(0,Nels_m[c])
#       gml$elinfo[[m]][[c]]  = list()
#       for (e in 1:Nels_m[c]) {
#
#         if (mcels[e]=="-1") {
#           eltype[[m]][[c]][e] = 99
#           hascons[m] = 0
#         }
#         else gml = merlin_build_el(gml,mcels[e],m,c,e)
#
#       }
#     }
#     Nels[[m]] = Nels_m
#
#
#   }
#
#   gml$Ncmps = Ncmps
#   gml$Nels  = Nels
#   gml$hascons = hascons
#
#   return(gml)
# }

merlin_setup_vcv <- function(gml,covariance)
{
    Ncovars = length(covariance)
    if (Ncovars==0) {
      gml$covariance = matrix(c(1,0,0),nrow = gml$Nlevels,ncol = 3)
    }
    else {
        if (Ncovars>1 & Ncovars!=gml$Nlevels) {
            stop("Error in covariance")
        }
        if (Ncovars==1) {
            covflag = as.numeric(covariance == c("diagonal","unstructured","exchangeable"))
            gml$covariance = matrix(covflag,nrow = gml$Nlevels,ncol = 3)
        }
        else {
          gml$covariance = as.numeric(matrix(covariance,nrow = gml$Nlevels ,ncol = gml$Nlevels) == matrix(c("diagonal","exchangeable","unstructured"),nrow = gml$Nlevels ,ncol = gml$Nlevels))
        }
    }

    gml$vcvbindex = vector()

    for (l in 1:gml$Nlevels) {

        if (gml$covariance[l,1]) {
            for (r in 1:gml$Nres[l]) {
                gml$vcvbindex = cbind(gml$vcvbindex,c(gml$eqn,gml$eqn))
                gml$eqn = gml$eqn + 1
            }
        }
        else if (gml$covariance[l,2]) {
            for (r in 1:gml$Nres[l]) {
                gml$vcvbindex = cbind(gml$vcvbindex,c(gml$eqn,gml$eqn))
                gml$eqn = gml$eqn + 1
            }
            for (r in 1:gml$Nres[l]) { # now do off-diagonals
              ind2 <- 1
              while (ind2 < r) {
                  gml$vcvbindex = cbind(gml$vcvbindex,c(gml$eqn,gml$eqn))
                  gml$eqn = gml$eqn + 1
                  ind2 = ind2 + 1
              }
            }

        }
        else if (gml$covariance[l,3]) {
            gml$vcvbindex = cbind(gml$vcvbindex,c(gml$eqn,gml$eqn))
            gml$eqn = gml$eqn + 1
            gml$vcvbindex = cbind(gml$vcvbindex,c(gml$eqn,gml$eqn))
        }
        else {
            gml$vcvbindex = cbind(gml$vcvbindex,c(gml$eqn,gml$eqn))
            gml$eqn = gml$eqn + 1
        }

    }
    return(gml)
}

merlin_parse_unique_latents1 <- function(gml,model)
{
    # notes; not effected by @'s
    # 	     not affected by EV[]

    #unique latents at each level
    gml$latlevs = list()
    gml$Nres    = rep_len(0,gml$Nlevels)

    for (i in 1:gml$Nlevels) {
        lats = NULL
        for (m in 1:gml$Nmodels) {
            rhs   = merlin_setup_get_rhs(model[[m]])
            cmps  = merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))
            Ncmps = length(cmps)
            for (c in 1:Ncmps) {
                dv 		= merlin_trim(cmps[c])
                pos = 1
                while (pos) {
                    pos = merlin_strpos(":",dv)
                    if (pos) {
                        dv2 = substr(dv,1,pos-1)
                        dv = substr(dv,start=pos+1,stop=nchar(dv))
                    }
                    else dv2 = dv
                    posid = merlin_strpos(gml$levels[i],dv2)
                    if (posid) lats = c(lats,substr(dv2,1,merlin_strpos("[",dv2)-1))
                }
            }
        }
        lats 		= unique(lats)			#they could appear in multiple models and multiple times in same model
        gml$Nres[i] = length(lats)
        gml$latlevs[[i]] = lats
    }

    return(gml)
}

merlin_parse_unique_latents2 <- function(gml, model)
{
    #all latents at each level in each model
    latslevmod = list()
    #indicator; any REs at each level in each model
    gml$lev_mod_latind = matrix(0,gml$Nlevels,gml$Nmodels)
    for (l in 1:gml$Nlevels) {
        latslevmod[[l]] <- list()
        for (m in 1:gml$Nmodels) {
            lats = NULL
            rhs   = merlin_setup_get_rhs(model[[m]])
            cmps  = merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))
            Ncmps = length(cmps)
            for (c in 1:Ncmps) {
                dv 		= merlin_trim(cmps[c])
                pos = 1
                while (pos) {
                    pos = merlin_strpos(":",dv)
                    if (pos) {
                        dv2 = substr(dv,1,pos-1)
                        dv = substr(dv,start=pos+1,stop=nchar(dv))
                    }
                    else dv2 = dv
                    posid = merlin_strpos(gml$levels[l],dv2)
                    if (posid) lats = c(lats,substr(dv2,1,merlin_strpos("[",dv2)-1))
                }
            }
            latslevmod[[l]][[m]] = lats
            gml$lev_mod_latind[l,m] = as.numeric(!is.null(lats))
        }
    }
    gml$latslevmod = latslevmod
    return(gml)
}

merlin_setup_cp <- function(gml,model)
{

    Ncmps         = rep(0,gml$Nmodels)
    gml$cmps      = list()               #components for each model
    Nels          = list()               #number of elements in each component, by model
    els           = list()               #elements for each model and component
    gml$eltype    = list()
    hascons       = rep(1,gml$Nmodels)
    gml$elinfo    = list()

    gml$bindex    = list()
    gml$apbindex  = list()
    gml$dapbindex = list()
    gml$eqn       = 1

    gml$cmphasb   = list()

    gml$Nvars     = list()

    for (m in 1:gml$Nmodels) {

        cind = 1
        bindex = vector()
        rhs    = merlin_setup_get_rhs(model[[m]])

        gml$modelind = m
        gml$modtouse = m
        hasb = vector()

        # split into components
        gml             = merlin_get_cmps(gml,m,rhs)
        if (length(gml$cmps[[m]])==1) {
            if (gml$cmps[[m]] =="null") Ncmps[m] = 0
            else                        Ncmps[m] = 1
        }
        else                            Ncmps[m] = length(gml$cmps[[m]])
        Nels_m          = rep_len(0,Ncmps[m])
        els[[m]]        = list()
        gml$eltype[[m]] = list()
        gml$elinfo[[m]] = list()
        gml$Nvars[[m]]  = list()

        if (Ncmps[m]) {

            for (c in 1:Ncmps[m]) {

                hasb = c(hasb,!merlin_strpos("*",gml$cmps[[m]][c]))
                if (hasb[length(hasb)]) {
                    bindex   = cbind(bindex,c(gml$eqn,0))
                }

                # split into elements
                els[[m]][[c]]         = unlist(strsplit(gml$cmps[[m]][c],split=":",fixed=TRUE))
                mcels                 = els[[m]][[c]]
                Nels_m[c]             = length(els[[m]][[c]])
                gml$eltype[[m]][[c]]  = rep_len(0,Nels_m[c])
                gml$elinfo[[m]][[c]]  = list()
                for (e in 1:Nels_m[c]) {

                    if (mcels[e]=="-1") {
                        gml$eltype[[m]][[c]][e] = 99
                        hascons[m] = 0
                    }
                    else {
                        gml = merlin_build_el(gml,mcels[e],m,c,e)
                    }

                }

                gml = merlin_build_variables(gml,m,c,cind,hasb[length(hasb)])
                if (hasb[length(hasb)]) {
                    bindex[2,c] = gml$eqn - 1
                    cind = cind + 1
                }

            }
        }

        gml$cmphasb[[m]] = hasb
        Nels[[m]] = Nels_m

        if (hascons[m]) {
            bindex = cbind(bindex,c(gml$eqn,gml$eqn))
            gml$eqn = gml$eqn + 1
        }

        #dap eqns
        if (gml$Ndap[m]) {
            dapbindex = vector()
            for (dap in 1:gml$Ndap[m]) {
                dapbindex = cbind(dapbindex,c(gml$eqn,gml$eqn))
                gml$eqn = gml$eqn + 1
            }
            gml$dapbindex[[m]] = dapbindex
        }

        #ap eqns
        if (gml$Nap[m]) {
            apbindex = vector()
            for (ap in 1:gml$Nap[m]) {
                apbindex = cbind(apbindex,c(gml$eqn,gml$eqn))
                gml$eqn = gml$eqn + 1
            }
            gml$apbindex[[m]] = apbindex
        }

        gml$bindex[[m]]    = bindex
    }

    gml$Ncmps   = Ncmps
    gml$Nels    = Nels
    gml$hascons = hascons

    return(gml)
}

merlin_get_cmps <- function(gml,m,rhs)
{

    if (rhs[1]!="1" | length(rhs)>1) {

        cmps <- merlin_trim(unlist(strsplit(rhs, split='+', fixed=TRUE)))

        #parse and strip out special elements
        newcmps = vector()
        for (c in 1:length(cmps)) {
            if (merlin_strpos("ap(",cmps[c])) {
                if (gml$Nap[m]) stop("ap() cannot be specified twice in one submodel")
                gml$Nap[m] = as.numeric(substr(cmps[c],merlin_strpos("(",cmps[c])+1,merlin_strpos(")",cmps[c])-1))
            }
            else if (merlin_strpos("bhazard(",cmps[c])) {
                if (gml$hasbh[m]) stop("bhazard() cannot be specified twice in one submodel")
                gml$hasbh[m] = 1
                gml$bhazard[m] = substr(cmps[c],merlin_strpos("(",cmps[c])+1,merlin_strpos(")",cmps[c])-1)
            }
            else if (merlin_strpos("exposure(",cmps[c])) {
                if (gml$hasexposure[m]) stop("exposure() cannot be specified twice in one submodel")
                gml$hasexposure[m] = 1
                gml$exposure[m] = substr(cmps[c],merlin_strpos("(",cmps[c])+1,merlin_strpos(")",cmps[c])-1)
            }
            else {
                newcmps = c(newcmps,cmps[c])
            }
        }
        gml$cmps[[m]] = as.character(newcmps)
    }
    else gml$cmps[[m]] = "null"    #no components

    return(gml)
}

merlin_setup_labels <- function(gml,m)
{
    label = vector()
    for (m in 1:gml$Nmodels) {

        # split into components
        Ncmps 	  = gml$Ncmps[m]			# of components
        Nels 	    = gml$Nels[[m]]			# els per component

        hasb      = gml$cmphasb[[m]]

        if (Ncmps) {

            for (c in 1:Ncmps) {

                if (hasb[c]) {
                    eltype = gml$eltype[[m]][[c]]
                    stub   = vector()

                    for (e in 1:Nels[c]) {

                        if (e>1) stub = paste0(stub,":")

                        if      (eltype[e]==1) stub = paste0(stub,gml$elinfo[[m]][[c]][[e]])
                        else if (eltype[e]==2) {
                            info = gml$elinfo[[m]][[c]][[e]]
                            stub = paste0(stub,gml$latlevs[[info[1]]][info[2]])
                        }
                        else if (eltype[e]==3)  stub = paste0(stub,"rcs()")
                        else if (eltype[e]==4)  stub = paste0(stub,"fp()")
                        else if (eltype[e]==5)  stub = paste0(stub,"EV[]")
                        else if (eltype[e]==6)  stub = paste0(stub,"iEV[]")
                        else if (eltype[e]==7)  stub = paste0(stub,"dEV[]")
                        else if (eltype[e]==8)  stub = paste0(stub,"d2EV[]")
                        else if (eltype[e]==10) stub = paste0(stub,"XB[]")
                        else if (eltype[e]==11) stub = paste0(stub,"iXB[]")
                        else if (eltype[e]==12) stub = paste0(stub,"dXB[]")
                        else if (eltype[e]==13) stub = paste0(stub,"d2XB[]")

                    }

                    Nvars = gml$Nvars[[m]][[c]]
                    if (Nvars>1) {
                        for (v in 1:Nvars) {
                            label = c(label,paste0(stub,":",as.character(v)))
                        }
                    }
                    else label = c(label,stub)
                }

            }

        }

        if (gml$hascons[m]) {
            label = c(label,"_cons")
        }

        if (gml$Ndap[m]) {
            if      (gml$family[m]=="gaussian") label = c(label,"log_sd(resid.)")
            else if (gml$family[m]=="weibull")  label = c(label,"log(gamma)")
            else if (gml$family[m]=="gompertz") label = c(label,"gamma")
            else {
                for (ap in 1:gml$Ndap[m]) {
                  label = c(label,paste0("_dap",as.character(ap)))
                }
            }
        }

        if (gml$Nap[m]) {
            for (ap in 1:gml$Nap[m]) {
                label = c(label,paste0("_ap",as.character(ap)))
            }
        }

    }

    labelo = label

    if (gml$Nlevels) {
        for (l in 1:gml$Nlevels) {
            latents = gml$latlevs[[l]]
            if (gml$covariance[l,1] | gml$covariance[l,2]) {
                for (r in 1:gml$Nres[l]) {
                    label = c(label,paste0("sd(",latents[r],")"))
                    labelo = c(labelo,paste0("log_sd(",latents[r],")"))
                }
                if (gml$covariance[l,2]) {
                    for (r in 1:gml$Nres[l]) {
                        ind2 <- 1
                        while (ind2 < r) {
                            label = c(label,paste0("corr(",latents[r],",",latents[ind2],")"))
                            labelo = c(labelo,paste0("atanh_corr(",latents[r],",",latents[ind2],")"))
                            ind2 = ind2 + 1

                        }
                    }
                }
            }
            else if (gml$covariance[l,3]) {
                label = c(label,paste0("sd()"))
                label = c(label,paste0("corr()"))
                labelo = c(labelo,paste0("log_sd()"))
                labelo = c(labelo,paste0("atanh_corr()"))
            }
            else {
                label = c(label,paste0("sd()"))
                labelo = c(labelo,paste0("log_sd()"))
            }
        }
    }
    gml$labels = label
    gml$labelso = labelo
    return(gml)
}


merlin_setup_get_rhs <- function(modelrhs)
{
    rhs = strsplit(as.character(modelrhs),split='~', fixed=TRUE)
    return(unlist(rhs[length(rhs)]))
}

merlin_setup_sweights <- function(gml,sweights)
{
    # number of weights (if specified should be the same length as levels + 1 for ob level)
    # swsts[[model]][[level]]
    Nsweights   = length(sweights)
    hassweights = Nsweights>0
    swts        = list()
    if (hassweights) {
        if (Nsweights != (gml$Nlevels + 1)) {
          stop("Weights must be specified for each level, with additional weights for the individual observations, if there is no weighting at a particular level then the weight is 1 for all observations",call. = FALSE)
        }
        for (k in 1:gml$Nmodels) {
          swtsk <- list()
          for (s in 1:Nsweights) {
              if (s == Nsweights) swtsk[[s]] <- gml$data[gml$datause[[k]],sweights[s]]
              else swtsk[[s]] <- stats::aggregate(gml$data[gml$datause[[k]],sweights[s]],by=list(gml$sind[[k]][[s]]), mean)[,2]
          }
          swts[[k]] <- swtsk
        }
    }
    gml$hassweights = hassweights
    if (hassweights) gml$swts = swts
    return(gml)
}

merlin_build_el <- function(gml,el,m,c,e)
{
    el          = merlin_trim(el)
    hassquareb	= grepl("[",el,fixed=T)
    hasroundb	  = grepl("(",el,fixed=T)

    if (hassquareb & !hasroundb) {

        if (merlin_strpos("EV",el) | merlin_strpos("XB",el)) {	#EV XB

            kpos 	= merlin_strpos("[",el)	                  #get response varname
            kpos2 = merlin_strpos("]",el)
            y 		= substr(el,kpos+1,kpos2-1)

            for (k in 1:gml$Nmodels) {                      #get response varname index and store
                if (gml$y[[k]][1]==y | as.character(k)==y) {
                    gml$elinfo[[m]][[c]][[e]] = k           #store element info
                }
            }

            if      (substr(el,1,2)=="XB")	   gml$eltype[[m]][[c]][e] = 10
            else if (merlin_strpos("iXB",el))  gml$eltype[[m]][[c]][e] = 11
            else if (merlin_strpos("dXB",el))  gml$eltype[[m]][[c]][e] = 12
            else if (merlin_strpos("d2XB",el)) gml$eltype[[m]][[c]][e] = 13
            else if (substr(el,1,2)=="EV")	   gml$eltype[[m]][[c]][e] = 5
            else if (merlin_strpos("iEV",el))  gml$eltype[[m]][[c]][e] = 6
            else if (merlin_strpos("dEV",el))  gml$eltype[[m]][[c]][e] = 7
            else if (merlin_strpos("d2EV",el)) gml$eltype[[m]][[c]][e] = 8

        }
        else 	{
            gml$eltype[[m]][[c]][e] = 2
            gml = merlin_setup_re(gml,m,c,e,el)	 #random effect
        }

    }
    else {

        if (merlin_strpos("rcs(",el)) {            #rcs function
              gml$eltype[[m]][[c]][e] = 3
              gml = merlin_setup_rcs(gml,m,c,e,el)
        }
        else if (merlin_strpos("fp(",el)) {        #fp function
            gml$eltype[[m]][[c]][e] = 4
            gml = merlin_setup_fp(gml,m,c,e,el)
        }
        else {                                     #varname
            gml$eltype[[m]][[c]][e] = 1
            gml = merlin_setup_var(gml,m,c,e,el)
        }

        # if 		  (merlin_strpos("mf(",el))  return(merlin_setup_mf(gml,mod,i,Nels,el))	 #user-defined function
        # else if (merlin_strpos("rcs(",el)) return(merlin_setup_rcs(gml,mod,i,Nels,el)) #rcs function
        # else if (merlin_strpos("fp(",el))  return(merlin_setup_fp(gml,mod,i,Nels,el))  #fp function
        # else


    }

    return(gml)
}
merlin_strpos <- function(s,x)
{
    res = regexpr(s,x,fixed=T)[[1]]
    if (res==-1) res = 0
    return(res)
}

merlin_setup_var <- function(gml,m,c,e,el)
{
    gml$elinfo[[m]][[c]][[e]] = el
    return(gml)
}

merlin_setup_re <- function(gml,m,c,e,el)
{
    name = substr(el,1,merlin_strpos("[",el)-1)		#get name of random effect
    info = rep_len(0,2)														#get level and dimension index

    for (l in 1:gml$Nlevels) {
        if (name %in% gml$latlevs[[l]]) {
            info[1] = l														#level
        }
    }
    info[2] = sum((1:gml$Nres[info[1]]) * as.numeric(name==gml$latlevs[[info[1]]]))	#dimensionindex
    gml$elinfo[[m]][[c]][[e]] = info					#store info
    return(gml)
}

merlin_setup_rcs <- function(gml,m,c,e,el)
{
    #add gml etc.
    if (merlin_strpos("srcs(",el)) {
        call = substr(el,merlin_strpos("(",el)+1,nchar(el)-1)
        call = paste0("rcs(gml,",call,",log=T,event=T)")
    }
    else {
        call = substr(el,merlin_strpos("(",el)+1,nchar(el))
        call = paste0("rcs(gml,",call)
    }
    gml$elinfo[[m]][[c]][[e]] = eval(parse(text=call))
    return(gml)
}

merlin_setup_fp <- function(gml,m,c,e,el)
{
    #add gml
    call = substr(el,merlin_strpos("(",el)+1,nchar(el))
    call = paste0("fp(gml,",call)
    gml$elinfo[[m]][[c]][[e]] = eval(parse(text=call))
    return(gml)
}

merlin_build_variables <- function(gml,m,c,cind,hasb)
{
    #build core variables to post to dataset
    eltype  = gml$eltype[[m]][[c]]
    Nels 	  = length(eltype)
    Nobs	  = gml$Nobs[m]
    newvars = matrix(1,nrow = Nobs,ncol = 1)

    for (e in 1:Nels) {

        if 		(eltype[e]==1)	{       #variable
            varname  = gml$elinfo[[m]][[c]][[e]]
            nextvars = as.matrix(gml$data[gml$datause[[m]],varname])
        }
        else if (eltype[e]==2) {			#random effect
            nextvars = matrix(1,nrow = Nobs,ncol = 1)
        }
        else if (eltype[e]==5 | eltype[e]==6 | eltype[e]==7 | eltype[e]==8) {	#?EV
            nextvars = matrix(1,nrow = Nobs,ncol = 1)
        }
        else if (eltype[e]==10 | eltype[e]==11 | eltype[e]==12 | eltype[e]==13) {	#?XB
            nextvars = matrix(1,nrow = Nobs,ncol = 1)
        }
        else if (eltype[e]==3) {												#rcs()
            nextvars = merlin_xz_rcs(gml,m,c,e)
        }
        else if (eltype[e]==4) {												#fp()
            nextvars = merlin_xz_fp(gml,m,c,e)
        }

        #rebuild
        Nold = ncol(newvars)
        Nnew = ncol(nextvars)

        copyold = newvars
        newvars = NULL
        for (j in 1:Nnew) {
            newvars = cbind(newvars,sweep(copyold,1,nextvars[,j],"*"))
        }

    }

    Nnew = ncol(newvars)
    if (hasb) {
      for (j in 1:Nnew) {
        gml$eqn = gml$eqn + 1
      }
    }

    gml$Nvars[[m]][[c]] = Nnew

    return(gml)
}

merlin_setup_ip <- function(gml,ip)
{
    g    = list()
    w    = list()
    Ndim = rep_len(0,gml$Nlevels)

    if (length(ip)==1) {
        ip = rep_len(ip,gml$Nlevels)
    }

    if (length(ip)==0) {
        ip <- rep_len(0,gml$Nlevels)
        for (l in 1:gml$Nlevels) {
            if (gml$intmethod[l] == "ghermite") ip[l] <- 7
            else ip[l] <- 100
        }
    }

    for (l in 1:gml$Nlevels) {
        # level specific integration points
        if (gml$intmethod[l] == "ghermite") {
            gq      <- statmod::gauss.quad(ip[l],kind="hermite")
            gh      <- ghq_expand_matrix(g=gq$nodes,w=gq$weights,nr=gml$Nres[l])
            g[[l]]  <- gh$nodes
            w[[l]]  <- apply(gh$wts, 2, prod)
            Ndim[l] <- ncol(gh$nodes)
        }
        else if (gml$intmethod[l] == "halton"){
            g[[l]]    <- randtoolbox::halton(ip[l],dim = gml$Nres[l], normal=TRUE)
            Ndim[l]   <- ip[l]
        }
        else if (gml$intmethod[l] == "halton") {
            g[[l]]    <- randtoolbox::sobol(ip[l], dim = gml$Nres[l], normal = TRUE, scrambling = 1)
            Ndim[l]   <- ip[l]
        }
        else {
            g[[l]]    <- MASS::mvrnorm(ip[l],rep(0,gml$Nres[l]),diag(gml$Nres[l]))
            Ndim[l]   <- ip[l]
        }
    }
    gml$g = g
    gml$w = w
    gml$Ndim = Ndim
    gml$ip <- ip
    return(gml)
}

# setup ip for numerical integration of cumulative hazard function
merlin_setup_ch_ip <- function(gml) {

  # integrating cumulative hazard -> filled up below
  gml$NI = rep_len(0,gml$Nmodels)
  gml$hazNnodes  <- rep_len(NA,gml$Nmodels)
  gml$haznodes   <- list()
  gml$hazweights <- list()

  for (i in 1:gml$Nmodels) {
      gml$NI[i] = !is.null(gml$timevar[i])
      if (gml$family[i]=="loghazard") gml$NI[i] = 1
      if (gml$NI[i]) {
        # gml$hazNnodes[i] <- 15
        # gq <- merlin_get_GK()
        gml$hazNnodes[i] <- 30
        gq <- statmod::gauss.quad(gml$hazNnodes[i],kind="legendre")
        ys <- matrix(gml$data[gml$datause[[i]],unlist(gml$y[[i]])[1]],ncol=1)
        gml$haznodes[[i]]   <- sweep((ys %*% (gq[[1]]/2)),1,(ys/2),"+")
        gml$hazweights[[i]] <- (ys %*% (gq[[2]]/2))
      }
      else {
        gml$haznodes[[i]]   <- NA
        gml$hazweights[[i]] <- NA
      }
  }
  return(gml)
}

merlin_setup_panelvars <- function(gml)
{
    panelid = list()
    Npanels = list()
    panelid = list()
    # get the level indicators
    for (m in 1:gml$Nmodels) {
        sindk <- list()
        for (l in 1:gml$Nlevels) {
          l2 = gml$Nlevels - l + 1
          if (l2 == gml$Nlevels) { # observation level
              sindkl <- gml$data[gml$datause[[m]],gml$levels[[l2]]]
          }
          else { # at a higher level
              sindkl <- gml$data[gml$datause[[m]],gml$levels[l2]]
              sindkl <- sindkl[!duplicated(sindk[[(l2+1)]])]
          }
          sindk[[l2]]   <- sindkl
          Npanels[[l2]] <- length(unique(sindkl)) # THIS WILL NEED CHANGING WHEN MISSING DATA IS DEALT WITH
        }
        panelid[[m]] <- sindk
    }
    gml$Npanels = Npanels
    gml$panelid = panelid
    return(gml)
}

merlin_setup_logl <- function(gml)
{
    logl <- vector()

    for (m in 1:gml$Nmodels) {

        if      (gml$family[m] == "gaussian")     logl <- c(logl,"merlin_logl_gaussian(gml)")
        else if (gml$family[m] == "weibull")      logl <- c(logl,"merlin_logl_weibull(gml)")
        else if (gml$family[m] == "exponential")  logl <- c(logl,"merlin_logl_exponential(gml)")
        else if (gml$family[m] == "gompertz")     logl <- c(logl,"merlin_logl_gompertz(gml)")
        else if (gml$family[m] == "bernoulli")    logl <- c(logl,"merlin_logl_bernoulli(gml)")
        else if (gml$family[m] == "poisson")      logl <- c(logl,"merlin_logl_poisson(gml)")
        else if (gml$family[m] == "beta")         logl <- c(logl,"merlin_logl_beta(gml)")
        else if (gml$family[m] == "negbinomial")  logl <- c(logl,"merlin_logl_negbinomial(gml)")
        else if (gml$family[m] == "rp")           logl <- c(logl,"merlin_logl_rp(gml)")
        else if (gml$family[m] == "loghazard")    logl <- c(logl,"merlin_logl_logh(gml)")
        else if (gml$family[m] == "user")         logl <- c(logl,paste0(gml$userf[m],"(gml)"))

    }
    gml$logl <- logl
    return(gml)
}


merlin_setup_userf <- function(gml,userf)
{
    gml$userf = rep_len("",gml$Nmodels)

    ind = 1
    for (m in 1:gml$Nmodels) {
        if (gml$family[m]=="user") {
          gml$userf[m] = userf[ind]
          ind = ind + 1
        }
    }

    return(gml)
}




