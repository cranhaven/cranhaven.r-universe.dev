get_Sigma <- function(draws, m, settings, g = NA, howsave = "data.frame",
                      Sigmacols, InvSigmacols, sdSigmacols, corSigmacols){
  
  howsave <- match.arg(howsave, c("data.frame", "list"))
  totnran <- settings["Sigma", "d1"]
  
  if(howsave == "data.frame"){
  
    if(settings["InvSigma", "gspec"]){
      if(missing(g) || is.na(g) || !is.element(g, 1:settings["InvSigma", "G"])){
        warning("Cluster g not given, g=1 used.")
        g <- 1
      }
      if(missing(Sigmacols)){Sigmacols <- grep(paste0("^Sigma\\(", g, "\\)"), colnames(draws))}
      if(missing(InvSigmacols)){InvSigmacols <- grep(paste0("^InvSigma\\(", g, "\\)"), colnames(draws))}
      if(missing(sdSigmacols)){sdSigmacols <- grep(paste0("^sdSigma\\(", g, "\\)"), colnames(draws))}
      if(missing(corSigmacols)){corSigmacols <- grep(paste0("^corSigma\\(", g, "\\)"), colnames(draws))}
    }else{
      if(missing(Sigmacols)){Sigmacols <- grep("^Sigma", colnames(draws))}
      if(missing(Sigmacols)){InvSigmacols <- grep("^InvSigma", colnames(draws))}
      if(missing(Sigmacols)){sdSigmacols <- grep("^sdSigma", colnames(draws))}
      if(missing(Sigmacols)){corSigmacols <- grep("^corSigma", colnames(draws))}
    }
      
    if(settings["Sigma", "save"]){
      values <- unlist(draws[m, Sigmacols, drop = FALSE])
      Sigma <- matrix(0, totnran, totnran)
      Sigma[upper.tri(Sigma, diag = TRUE)] <- values
      Sigma <- t(Sigma)
      Sigma[upper.tri(Sigma, diag = TRUE)] <- values
      # InvSigma <- solve(Sigma)
      # sdSigma <- sqrt(diag(Sigma))
      # corSigma <- diag(1/sdSigma) %*% Sigma %*% diag(1/sdSigma)
    }else{
      if(settings["InvSigma", "save"]){
        values <- unlist(draws[m, InvSigmacols, drop = FALSE])
        InvSigma <- matrix(0, totnran, totnran)
        InvSigma[upper.tri(InvSigma, diag = TRUE)] <- values
        InvSigma <- t(InvSigma)
        InvSigma[upper.tri(InvSigma, diag = TRUE)] <- values
        Sigma <- chol2inv(chol(InvSigma))
        # sdSigma <- sqrt(diag(Sigma))
        # corSigma <- diag(1/sdSigma) %*% Sigma %*% diag(1/sdSigma)
      }else{
        if(settings["sdSigma", "save"] & settings["corSigma", "save"]){
          sdSigma <- unlist(draws[m, sdSigmacols, drop = FALSE])
          cors <- unlist(draws[m, corSigmacols, drop = FALSE])
          corSigma <- matrix(1, totnran, totnran)
          corSigma[upper.tri(corSigma, diag = FALSE)] <- cors
          corSigma <- t(corSigma)
          corSigma[upper.tri(corSigma, diag = FALSE)] <- cors
          Sigma <- diag(sdSigma, nrow = totnran) %*% corSigma %*% diag(sdSigma, nrow = totnran)
          # InvSigma <- solve(Sigma)
        }else{
          warning("None of InvSigma, Sigma, (sdSigma, corSigma) is saved.")
          Sigma <- diag(totnran)
        }
      }
    }
    
  }
  
  if(howsave == "list"){
    if(settings["InvSigma", "gspec"]){
      if(settings["Sigma", "save"]){
        Sigma <- draws$Sigma[[g]][m,,]
        # InvSigma <- solve(Sigma)
        # sdSigma <- sqrt(diag(Sigma))
        # corSigma <- diag(1/sdSigma) %*% Sigma %*% diag(1/sdSigma)
      }else{
        if(settings["InvSigma", "save"]){
          InvSigma <- draws$InvSigma[[g]][m,,]
          Sigma <- solve(InvSigma)
          # sdSigma <- sqrt(diag(Sigma))
          # corSigma <- diag(1/sdSigma) %*% Sigma %*% diag(1/sdSigma)
        }else{
          if(settings["sdSigma", "save"] & settings["corSigma", "save"]){
            sdSigma <- draws$sdSigma[[g]][m,]
            corSigma <- draws$corSigma[[g]][m,,]
            Sigma <- diag(sdSigma, nrow=totnran) %*% corSigma %*% diag(sdSigma, nrow=totnran)
            # InvSigma <- solve(Sigma)
          }else{
            warning("None of InvSigma, Sigma, (sdSigma, corSigma) is saved.")
            Sigma <- diag(totnran)
          }
        }
      }
    }else{
      if(settings["Sigma", "save"]){
        Sigma <- draws$Sigma[m,,]
        # InvSigma <- solve(Sigma)
        # sdSigma <- sqrt(diag(Sigma))
        # corSigma <- diag(1/sdSigma) %*% Sigma %*% diag(1/sdSigma)
      }else{
        if(settings["InvSigma", "save"]){
          InvSigma <- draws$InvSigma[m,,]
          Sigma <- solve(InvSigma)
          # sdSigma <- sqrt(diag(Sigma))
          # corSigma <- diag(1/sdSigma) %*% Sigma %*% diag(1/sdSigma)
        }else{
          if(settings["sdSigma", "save"] & settings["corSigma", "save"]){
            sdSigma <- draws$sdSigma[m,]
            corSigma <- draws$corSigma[m,,]
            Sigma <- diag(sdSigma, nrow=totnran) %*% corSigma %*% diag(sdSigma, nrow=totnran)
            # InvSigma <- solve(Sigma)
          }else{
            warning("None of InvSigma, Sigma, (sdSigma, corSigma) is saved.")
            Sigma <- diag(totnran)
          }
        }
      }
    }
  }
  
  return(Sigma)
}
