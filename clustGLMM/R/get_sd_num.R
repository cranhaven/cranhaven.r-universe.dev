get_sd_num <- function(draws, m, settings, y = NA, howsave = "data.frame") {
  howsave <- match.arg(howsave, c("data.frame", "list"))
  
  if (howsave == "data.frame") {
    if (settings["prec_num", "gspec"]) {
      if (settings["sd_num", "save"]) {
        sd_num <- draws[m, paste0("sd_num_", y, "(", 1:settings["sd_num", "G"], ")")]
      } else if (settings["prec_num", "save"]) {
        sd_num <- 1 / sqrt(draws[m, paste0("prec_num_", y, "(", 1:settings["prec_num", "G"], ")")])
      } else if (settings["var_num", "save"]) {
        sd_num <- sqrt(draws[m, paste0("var_num_", y, "(", 1:settings["var_num", "G"], ")")])
      } else {
          stop("Neither of sd_num, prec_num, var_num is saved in draws!")
      }
    } else {
      if (settings["sd_num", "save"]) {
        sd_num <- draws[m, paste0("sd_num_", y)]
      } else if (settings["prec_num", "save"]) {
          sd_num <- 1 / sqrt(draws[m, paste0("prec_num_", y)])
      } else if (settings["var_num", "save"]) {
          sd_num <- sqrt(draws[m, paste0("var_num_", y)])
      } else {
          stop("Neither of sd_num, prec_num, var_num is saved in draws!")
      }
    }
  }
  
  if (howsave == "list") {
    if (settings["prec_num", "gspec"]) {
      sd_num <- numeric(settings["sd_num", "G"])
      if (settings["sd_num", "save"]) {
        for(g in 1:settings["sd_num", "G"]) {
          sd_num[g] <- draws$sd_num[[g]][m, y]
        }
      } else if (settings["prec_num", "save"]) {
        for(g in 1:settings["prec_num", "G"]) {
          sd_num[g] <- 1 / sqrt(draws$prec_num[[g]][m, y])
        }
      } else if (settings["var_num", "save"]) {
        for(g in 1:settings["var_num", "G"]) {
          sd_num[g] <- sqrt(draws$var_num[[g]][m, y])
        }
      } else {
          stop("Neither of sd_num, prec_num, var_num is saved in draws!")
      }
    } else {
      if (settings["sd_num", "save"]) {
        sd_num <- draws$sd_num[m, y]
      } else if (settings["prec_num", "save"]) {
        sd_num <- 1 / sqrt(draws$prec_num[m, y])
      } else if (settings["var_num", "save"]) {
        sd_num <- sqrt(draws$var_num[m, y])
      } else {
        stop("Neither of sd_num, prec_num, var_num is saved in draws!")
      }
    }
  }
  
  return(sd_num)
}
