
merlin_initial_values <- function(gml,mod)
{

    if (gml$Nlevels) {
        init = vector()

        for (m in 1:gml$Nmodels) {
            mi = mod[[m]]
            #get response syntax
            modk = strsplit(as.character(mi),split='~', fixed=TRUE)
            y    = modk[[2]]
            mi   = modk[[3]]
            #strip out [] elements and fit fixed effect model
            mi = merlin_parse_init(gml,m,mi)
            if (is.null(mi)) mi = "1"
            refit = paste0(y,"~",mi)
            refit = parse(text=refit)
            res = merlin(model=list(eval(refit)), family=gml$family[m],data=gml$data)
            init = c(init,res$par)
        }
        init = merlin_rebuild_init(gml,init,mod)
        init = c(init,rep_len(0,ncol(gml$vcvbindex)))
    }
    else {
        if (gml$Nap[gml$Nmodels]) init = rep_len(0,max(gml$apbindex[[gml$Nmodels]]))
        else {
          if (gml$Ndap[gml$Nmodels]) init = rep_len(0,max(gml$dapbindex[[gml$Nmodels]]))
          else init = rep_len(0,max(gml$bindex[[gml$Nmodels]]))
        }

        # If any Gompertz, replace all starting 0's with 0.1's
        if ("gompertz" %in% gml$family) {
          init[init == 0] <- 0.1
        }

        if ("rp" %in% gml$family) {
            init = merlin_rcs_init(gml,init,mod)
        }
    }

    return(init)
}

merlin_parse_init <- function(gml,m,mi)
{
    Ncmps = gml$Ncmps[m]			# of components
    Nels 	= gml$Nels[[m]]			# els per component

    hasb  = gml$cmphasb[[m]]

    cmps = merlin_trim(unlist(strsplit(as.character(mi), split='+', fixed=TRUE)))

    xb   = NULL

    for (c in 1:Ncmps) {

        if (hasb[c]) {
            if (!merlin_strpos("[",cmps[c])) {
              if (length(xb)) xb = paste0(xb,"+")
              xb = paste0(xb,cmps[c])
            }
        }

    }
    return(xb)
}

merlin_rebuild_init <- function(gml,inits,mod)
{

  newinits = vector()

  index = 1
  for (m in 1:gml$Nmodels) {

      mi = mod[[m]]
      #get response syntax
      modk = strsplit(as.character(mi),split='~', fixed=TRUE)
      mi   = modk[[3]]

      Ncmps = gml$Ncmps[m]			# of components
      Nels 	= gml$Nels[[m]]			# els per component

      hasb  = gml$cmphasb[[m]]

      cmps = merlin_trim(unlist(strsplit(as.character(mi), split='+', fixed=TRUE)))

      xb   = NULL

      for (c in 1:Ncmps) {

          Nvars = gml$Nvars[[m]][[c]]

          if (hasb[c]) {

              newc = NULL
              if (merlin_strpos("[",cmps[c])) {  #add in zeros
                  newinits = c(newinits,rep_len(0,Nvars))
              }
              else {
                  for (n in 1:Nvars) {
                      newinits = c(newinits,inits[index])
                      index = index + 1
                  }
              }
              xb = paste0(xb,newc)
          }

      }

      if (gml$hascons[m]) {
          newinits = c(newinits,inits[index])
          index = index + 1
      }

      if (gml$Ndap[m]) {
        for (d in 1:gml$Ndap[m]) {
            newinits = c(newinits,inits[index])
            index = index + 1
        }
      }

      if (gml$Nap[m]) {
        for (d in 1:gml$Nap[m]) {
          newinits = c(newinits,inits[index])
          index = index + 1
        }
      }
  }
  return(newinits)
}

merlin_rcs_init <- function(gml,init,mod)
{
  index = 1
  for (m in 1:gml$Nmodels) {

    mi = mod[[m]]
    #get response syntax
    modk = strsplit(as.character(mi),split='~', fixed=TRUE)
    mi   = modk[[3]]

    Ncmps = gml$Ncmps[m]			# of components
    hasb  = gml$cmphasb[[m]]
    cmps = merlin_trim(unlist(strsplit(as.character(mi), split='+', fixed=TRUE)))

    for (c in 1:Ncmps) {
        Nvars = gml$Nvars[[m]][[c]]
        if (hasb[c]) {
            if (merlin_strpos("rcs(",cmps[c])) {  #add in zeros
              init[index] = 1
            }
            index = index + Nvars
        }
    }

    if (gml$hascons[m]) index = index + 1
    if (gml$Ndap[m])    index = index + gml$Ndap[m]
    if (gml$Nap[m])    index = index + gml$Nap[m]
  }
  return(init)
}






