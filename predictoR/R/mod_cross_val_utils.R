# Obtener moda
moda <- function(x) {
  names(which.max(table(x)))
}

# Obtiene todas las precisiones globales promedio.
calc_cross_pg <-  function(x, cat = "Global") {
  res <- data.frame()
  for (k in names(x)) {
    for (vc in names(x[[k]])) {
      if(vc %in% c("categoria", "corte")) {
        next
      }
      mc <- 0
      for (g in names(x[[k]][[vc]])) {
        mc <- mc + x[[k]][[vc]][[g]]$mc
      }
      indices <- indices.generales(mc)
      if(cat == "Global") {
        res[k, vc] <- indices$precision.global
      } else {
        res[k, vc] <- indices$precision.clase[cat]
      }
    }
  }
  
  return(res)
}

# Obtiene todas los indices promedio.
calc_cross_indices <-  function(x, i, tt = T) {
  res <- data.frame()
  
  for (k in names(x)) {
    if(tt) {
      mc   <- x[[k]][[i]]$mc
    } else {
      mc <- 0
      if(is.numeric(i)) {
        vc <- paste0("VC", i)
        for (g in names(x[[k]][[vc]])) {
          mc <- mc + x[[k]][[vc]][[g]]$mc
        }
      } else {
        n <- 0
        for (vc in names(x[[k]])) {
          if(vc %in% c("categoria", "corte")) {
            next
          }
          n <- n + 1
          for (g in names(x[[k]][[vc]])) {
            mc <- mc + x[[k]][[vc]][[g]]$mc
          }
        }
        mc <- round(mc / n)
      }
    }
    
    indices <- indices.generales(mc)
    if(is.null(x[[k]]$corte)) {
      nuevo <- data.frame(Modelo = k, PG = round(indices$precision.global, 2))
    } else {
      corte <- paste0(x[[k]]$categoria, " >= ", x[[k]]$corte)
      nuevo <- data.frame(Modelo = k, Corte = corte, PG = round(indices$precision.global, 2))
    }
    
    for (categoria in names(indices$precision.clase)) {
      nuevo[[categoria]] <- round(indices$precision.clase[categoria], 2)
    }
    res <- rbind(res, nuevo)
  }
  
  return(res)
}

# Obtiene la matriz de confusión.
calc_g_mc <-  function(x) {
  res <- 0
  for (g in names(x)) {
    res <- res + x[[g]]$mc
  }
  
  return(res)
}

# Obtiene la matriz de confusión promedio.
calc_cross_mc <-  function(x) {
  n <- 0
  res <- 0
  for (vc in names(x)) {
    if(vc %in% c("categoria", "corte")) {
      next
    }
    n <- n + 1
    for (g in names(x[[vc]])) {
      res <- res + x[[vc]][[g]]$mc
    }
  }
  
  return(round(res / n))
}

# Obtiene la probabilidad.
calc_g_prob <-  function(x, grupos) {
  probs <- data.frame()
  preds <- data.frame()
  
  for (g in names(x)) {
    gi     <- as.numeric(gsub("G", "", g))
    gprobs <- data.frame(x[[g]]$prob$prediction, row.names = grupos[[gi]])
    probs  <- rbind(probs, gprobs)
    gpreds <- data.frame(x[[g]]$pred, row.names = grupos[[gi]])
    preds  <- rbind(preds, gpreds)
  }
  
  probs <- probs[order(as.numeric(row.names(probs))), ]
  preds <- preds[order(as.numeric(row.names(preds))), ]
  
  return(list(probs, preds))
}

# Obtiene la probabilidad promedio.
calc_cross_prob <-  function(x, grupos) {
  probs <- 0
  preds <- NULL
  n <- 0
  for (vc in names(x)) {
    if(vc %in% c("categoria", "corte")) {
      next
    }
    n <- n + 1
    vcprobs <- data.frame()
    vcpreds <- data.frame()
    for (g in names(x[[vc]])) {
      vci <- as.numeric(gsub("VC", "", vc))
      gi  <- as.numeric(gsub("G", "", g))
      gprobs  <- data.frame(x[[vc]][[g]]$prob$prediction, row.names = grupos[[vci]][[gi]])
      vcprobs <- rbind(vcprobs, gprobs)
      gpreds  <- data.frame(x[[vc]][[g]]$pred, row.names = grupos[[vci]][[gi]])
      vcpreds <- rbind(vcpreds, gpreds)
    }
    vcprobs <- vcprobs[order(as.numeric(row.names(vcprobs))), ]
    vcpreds <- vcpreds[order(as.numeric(row.names(vcpreds))), , drop = FALSE]
    probs <- probs + vcprobs
    if(is.null(preds)) {
      preds <- vcpreds
    } else {
      preds <- cbind(preds, vcpreds)
    }
  }
  
  probs <- probs / n
  preds <- apply(preds, 1, moda)
  return(list(probs, preds))
}

# Obtiene las matrices de confusión por corte.
calc_g_cortes <-  function(x, grupos, prueba, categorias, positiva, paso = 0.5) {
  negativa <- categorias[categorias != positiva]
  
  for (corte in seq(0, 1, by = paso)) {
    probs <- data.frame()
    for (g in names(x)) {
      gi  <- as.numeric(gsub("G", "", g))
      gprobs  <- data.frame(x[[g]]$prob$prediction, row.names = grupos[[gi]])
      probs <- rbind(probs, gprobs)
    }
    
    probs <- probs[order(as.numeric(row.names(probs))), ]
    pred <- ifelse(probs[, positiva] >= corte, positiva, negativa)
    
    MC  <- table(prueba, Pred = factor(pred, levels = categorias))
    cat("\nCorte usado para la Probabilidad = ")
    cat(corte)
    cat("\n")
    print(general.indexes(mc = MC))
    cat("\n========================================")
  }
}

# Obtiene las matrices de confusión por corte.
calc_cross_cortes <- function(x, grupos, prueba, categorias, positiva, paso = 0.5) {
  negativa <- categorias[categorias != positiva]
  
  for (corte in seq(0, 1, by = paso)) {
    MC <- 0
    n  <- 0
    for (vc in names(x)) {
      if(vc %in% c("categoria", "corte")) {
        next
      }
      n <- n + 1
      vcprobs <- data.frame()
      for (g in names(x[[vc]])) {
        vci <- as.numeric(gsub("VC", "", vc))
        gi  <- as.numeric(gsub("G", "", g))
        gprobs  <- data.frame(x[[vc]][[g]]$prob$prediction, row.names = grupos[[vci]][[gi]])
        vcprobs <- rbind(vcprobs, gprobs)
      }
      vcprobs <- vcprobs[order(as.numeric(row.names(vcprobs))), ]
      pred <- ifelse(vcprobs[, positiva] >= corte, positiva, negativa)
      MCI  <- table(prueba, Pred = factor(pred, levels = categorias))
      MC <- MC + MCI
    }
    
    MC <- round(MC/n)
    cat("\nCorte usado para la Probabilidad = ")
    cat(corte)
    cat("\n")
    print(general.indexes(mc = MC))
    cat("\n========================================")
  }
}
