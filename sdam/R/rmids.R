
## 
## FUNCTION rmids() for restricted multiply-imputed data subsets
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.6 (29-08-2022)
##
## PARAMETERS
## x         (data set to impute, dataframe or lists of dataframes)
## vars      (attribute variables in x, optional vector)
## collapse  (collapse list of dataframes?, optional and logical)


rmids <-
function (x, vars, collapse, pool, type = c("1", "2")) 
{
    if (is.null(x) == TRUE) 
        stop("'x' is NULL")
    ifelse(missing(vars) == TRUE, vars <- c("not_before", "not_after"), 
        NA)
    ifelse(missing(collapse) == FALSE && isTRUE(collapse == TRUE) == 
        TRUE, collapse <- TRUE, collapse <- FALSE)
    ifelse(missing(pool) == FALSE && isTRUE(pool == TRUE) == 
        TRUE, pool <- TRUE, pool <- FALSE)
    ocl <- class(x)
    xc <- x
    if (isTRUE(is.data.frame(x) == TRUE) == TRUE) {
        vrs <- c(which(colnames(x) == vars[1]), which(colnames(x) == 
            vars[2]))
        if (any(is.na(as.vector(x[, vrs]))) == TRUE) {
            if (!(exists("rpd"))) {
                utils::data("rpd", package = "sdam", envir = environment())
                rpd <- get("rpd", envir = environment())
            }
            else {
                invisible(NA)
            }
        }
        if (all(is.na(as.vector(x[, vrs]))) == TRUE) {
            for (i in seq_len(length(rpd))) {
                if (any(attr(rpd[[i]], "class") == as.vector(x$id)[1]) == 
                  TRUE) {
                  message("avg taken from province.")
                  avg <- rpd[[i]][1]
                  break
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        else {
            avg <- round(mean(stats::na.omit(as.numeric(as.vector(unlist(x[, 
                vrs]))))))
        }
        if (all(is.na(as.vector(x[, vrs[1]]))) == TRUE) {
            for (i in seq_len(length(rpd))) {
                if (any(attr(rpd[[i]], "class") == as.vector(x$id)[1]) == 
                  TRUE) {
                  message("min TAQ taken from province.")
                  mtaq <- rpd[[i]][2]
                  break
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        else {
            mtaq <- min(stats::na.omit(as.numeric(as.vector(unlist(x[, 
                vrs])))))
        }
        if (all(is.na(as.vector(x[, vrs[2]]))) == TRUE) {
            for (i in seq_len(length(rpd))) {
                if (any(attr(rpd[[i]], "class") == as.vector(x$id)[1]) == 
                  TRUE) {
                  message("max TPQ taken from province.")
                  mtpq <- rpd[[i]][3]
                  break
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        else {
            mtpq <- max(stats::na.omit(as.numeric(as.vector(unlist(x[, 
                vrs])))))
        }
        if (all(is.na(x[, vrs[1]])) == FALSE && all(is.na(x[, 
            vrs[2]])) == FALSE) {
            avgts <- (round(mean(as.numeric(as.vector(stats::na.omit(x[, 
                vrs[2]]))))) - round(mean(as.numeric(as.vector(stats::na.omit(x[, 
                vrs[1]]))))))
        }
        else {
            for (i in seq_len(length(rpd))) {
                if (any(attr(rpd[[i]], "class") == as.vector(x$id)[1]) == 
                  TRUE) {
                  message("avg len TS taken from province.")
                  avgts <- rpd[[i]][4]
                  break
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        compl <- list()
        compln <- vector()
        if (isTRUE(pool == TRUE) == TRUE) {
            pl <- list()
            pln <- vector()
        }
        for (j in seq_len(nrow(x))) {
            if (any(is.na(x[j, vrs]) == TRUE) == FALSE) {
                compl[[length(compl) + 1L]] <- x[j, ]
                compln <- append(compln, "complete")
            }
            else {
                if (all(is.na(x[j, vrs])) == TRUE) {
                  temp <- x[j, ][rep(row.names(x[j, ]), 5), ]
                  temp[, vrs] <- factor(temp[, vrs], levels = NULL)
                  temp[2, vrs] <- c(mtaq, mtpq)
                  temp[3, vrs] <- c(mtaq, (mtaq + avgts))
                  temp[4, vrs] <- c((mtpq - avgts), mtpq)
                  temp[5, vrs] <- rep(avg, 2)
                  compl[[length(compl) + 1L]] <- temp[2:5, ]
                  compln <- append(compln, "NA-NA")
                  if (isTRUE(pool == TRUE) == TRUE) {
                    if (match.arg(type) == "1") {
                      temp[1, vrs] <- c(min(as.numeric(unlist(temp[2:5, 
                        vrs[1]]))), max(as.numeric(unlist(temp[2:5, 
                        vrs[2]]))))
                    }
                    else if (match.arg(type) == "2") {
                      ifelse(isTRUE(max(as.numeric(unlist(temp[2:5, 
                        vrs[1]]))) > min(as.numeric(unlist(temp[2:5, 
                        vrs[2]])))) == TRUE, temp[1, vrs] <- rev(c(max(as.numeric(unlist(temp[2:5, 
                        vrs[1]]))), min(as.numeric(unlist(temp[2:5, 
                        vrs[2]]))))), temp[1, vrs] <- c(max(as.numeric(unlist(temp[2:5, 
                        vrs[1]]))), min(as.numeric(unlist(temp[2:5, 
                        vrs[2]])))))
                    }
                    pl[[length(pl) + 1L]] <- temp[1, ]
                    pln <- append(pln, "NA-NA")
                  }
                }
                else {
                  temp <- x[j, ][rep(row.names(x[j, ]), 3), ]
                  temptaq <- temp[, vrs[1]]
                  temptpq <- temp[, vrs[2]]
                  tpqlvls <- c(levels(factor(temptaq)), levels(factor(temptpq)), 
                    mtpq, (as.numeric(as.vector(unlist(x[j, vrs[1]]))) + 
                      avgts))
                  taqlvls <- c(levels(factor(temptaq)), levels(factor(temptpq)), 
                    mtaq, (as.numeric(as.vector(unlist(x[j, vrs[2]]))) - 
                      avgts))
                }
                if (is.na(x[j, vrs[1]]) == FALSE && is.na(x[j, 
                  vrs[2]]) == TRUE) {
                  temp[, vrs] <- factor(temp[, vrs], levels = tpqlvls)
                  temp[, vrs[1]] <- temptaq
                  temp[2, vrs[2]] <- mtpq
                  temp[3, vrs[2]] <- (as.numeric(as.vector(unlist(x[j, 
                    vrs[1]]))) + avgts)
                  compl[[length(compl) + 1L]] <- temp[2:3, ]
                  compln <- append(compln, "taq-NA")
                  if (isTRUE(pool == TRUE) == TRUE) {
                    if (match.arg(type) == "1") {
                      temp[1, vrs[2]] <- max(as.numeric(unlist(temp[2:3, 
                        vrs])))
                    }
                    else if (match.arg(type) == "2") {
                      temp[1, vrs[2]] <- min(as.numeric(unlist(temp[2:3, 
                        vrs[2]])))
                    }
                    pl[[length(pl) + 1L]] <- temp[1, ]
                    pln <- append(pln, "taq-NA")
                  }
                }
                else if (is.na(x[j, vrs[1]]) == TRUE && is.na(x[j, 
                  vrs[2]]) == FALSE) {
                  temp[, vrs] <- factor(temp[, vrs], levels = taqlvls)
                  temp[, vrs[2]] <- temptpq
                  temp[2, vrs[1]] <- mtaq
                  temp[3, vrs[1]] <- (as.numeric(as.vector(unlist(x[j, 
                    vrs[2]]))) - avgts)
                  compl[[length(compl) + 1L]] <- temp[2:3, ]
                  compln <- append(compln, "NA-tpq")
                  if (isTRUE(pool == TRUE) == TRUE) {
                    temp[1, vrs[1]] <- min(as.numeric(unlist(temp[2:3, 
                      vrs])))
                    pl[[length(pl) + 1L]] <- temp[1, ]
                    pln <- append(pln, "NA-tpq")
                  }
                }
            }
        }
        rm(j)
        names(compl) <- compln
        if (isTRUE(pool == TRUE) == TRUE) {
            names(pl) <- pln
        }
        if (isTRUE(pool == TRUE) == TRUE) {
            if (isTRUE(collapse == TRUE) == TRUE) {
                plc <- do.call("rbind", pl)
                return(plc)
            }
            else {
                return(pl)
            }
        }
        else {
            if (isTRUE(collapse == FALSE) == TRUE) {
                xc <- compl
                class(xc) <- noquote(length(compl))
            }
            else {
                xc <- do.call("rbind", compl)
                class(xc) <- noquote(c(ocl, nrow(xc)))
            }
            return(xc)
        }
    }
    else {
        cnt <- vector()
        for (k in seq_len(length(x))) {
            if (is.null(dim(x[[k]][[1]])) == FALSE) {
                vrs <- c(which(colnames(x[[1]][[1]]) == vars[1]), 
                  which(colnames(x[[1]][[1]]) == vars[2]))
                if (!(exists("rpd"))) {
                  utils::data("rpd", package = "sdam", envir = environment())
                  rpd <- get("rpd", envir = environment())
                }
                else {
                  invisible(NA)
                }
                for (i in seq_len(length(x[[k]]))) {
                  if (any(is.na(x[[k]][[i]][, vrs])) == TRUE) {
                    if (all(is.na(x[[k]][[i]][, vrs])) == FALSE) {
                      avg <- round(mean(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][[i]][, 
                        vrs]))))))
                    }
                    else {
                      flgna <- FALSE
                      for (l in seq_len(length(rpd))) {
                        if (any(attr(rpd[[l]], "class") == as.vector(x[[k]][[i]]$id)[1]) == 
                          TRUE) {
                          warning("avg taken from province.")
                          flgna <- TRUE
                          avg <- rpd[[l]][1]
                          break
                        }
                        else {
                          NA
                        }
                      }
                      rm(l)
                      ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                        avg <- suppressWarnings(round(mean(as.vector(stats::na.omit(as.numeric(unlist(lapply(x, 
                          `[`, vrs), use.names = FALSE))))))))
                    }
                    if (all(is.na(x[[k]][[i]][, vrs[1]])) == 
                      FALSE) {
                      mtaq <- min(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][[i]][, 
                        vrs])))))
                    }
                    else {
                      flgna <- FALSE
                      for (l in seq_len(length(rpd))) {
                        if (any(attr(rpd[[l]], "class") == as.vector(x[[k]][[i]]$id)[1]) == 
                          TRUE) {
                          warning("min TAQ taken from province.")
                          flgna <- TRUE
                          mtaq <- rpd[[l]][2]
                          break
                        }
                        else {
                          NA
                        }
                      }
                      rm(l)
                      ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                        mtaq <- suppressWarnings(min(as.vector(stats::na.omit(as.numeric(unlist(lapply(x, 
                          `[`, vrs), use.names = FALSE)))))))
                    }
                    if (all(is.na(x[[k]][[i]][, vrs[2]])) == 
                      FALSE) {
                      mtpq <- max(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][[i]][, 
                        vrs])))))
                    }
                    else {
                      flgna <- FALSE
                      for (l in seq_len(length(rpd))) {
                        if (any(attr(rpd[[l]], "class") == as.vector(x[[k]][[i]]$id)[1]) == 
                          TRUE) {
                          message("max TPQ taken from province.")
                          flgna <- TRUE
                          mtpq <- rpd[[l]][3]
                          break
                        }
                        else {
                          NA
                        }
                      }
                      rm(l)
                      ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                        mtpq <- suppressWarnings(max(as.vector(stats::na.omit(as.numeric(unlist(lapply(x, 
                          `[`, vrs), use.names = FALSE)))))))
                    }
                    if (all(is.na(x[[k]][[i]][, vrs[1]])) == 
                      FALSE && all(is.na(x[[k]][[i]][, vrs[2]])) == 
                      FALSE) {
                      avgts <- round(mean(as.vector(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][[i]][, 
                        vrs[2]]))) - as.numeric(as.vector(unlist(x[[k]][[i]][, 
                        vrs[1]])))))))
                    }
                    else {
                      flgna <- FALSE
                      for (l in seq_len(length(rpd))) {
                        if (any(attr(rpd[[l]], "class") == as.vector(x[[k]][[i]]$id)[1]) == 
                          TRUE) {
                          message("avg len TS taken from province.")
                          flgna <- TRUE
                          avgts <- rpd[[l]][4]
                          break
                        }
                        else {
                          NA
                        }
                      }
                      rm(l)
                      ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                        avgts <- 177)
                    }
                    compl <- list()
                    compln <- vector()
                    if (isTRUE(pool == TRUE) == TRUE) {
                      pl <- list()
                      pln <- vector()
                    }
                    for (j in seq_len(nrow(x[[k]][[i]]))) {
                      if (any(is.na(x[[k]][[i]][j, vrs]) == TRUE) == 
                        FALSE) {
                        compl[[length(compl) + 1L]] <- x[[k]][[i]][j, 
                          ]
                        compln <- append(compln, "complete")
                      }
                      else {
                        if (all(is.na(x[[k]][[i]][j, vrs])) == 
                          TRUE) {
                          temp <- x[[k]][[i]][j, ][rep(row.names(x[[k]][[i]][j, 
                            ]), 5), ]
                          temp[, vrs] <- factor(temp[, vrs], 
                            levels = NULL)
                          temp[2, vrs] <- c(mtaq, mtpq)
                          temp[3, vrs] <- c(mtaq, (mtaq + avgts))
                          temp[4, vrs] <- c((mtpq - avgts), mtpq)
                          temp[5, vrs] <- rep(avg, 2)
                          compl[[length(compl) + 1L]] <- temp[2:5, 
                            ]
                          compln <- append(compln, "NA-NA")
                          if (isTRUE(pool == TRUE) == TRUE) {
                            if (match.arg(type) == "1") {
                              temp[1, vrs] <- c(min(as.numeric(unlist(temp[2:5, 
                                vrs[1]]))), max(as.numeric(unlist(temp[2:5, 
                                vrs[2]]))))
                            }
                            else if (match.arg(type) == "2") {
                              ifelse(isTRUE(max(as.numeric(unlist(temp[2:5, 
                                vrs[1]]))) > min(as.numeric(unlist(temp[2:5, 
                                vrs[2]])))) == TRUE, temp[1, 
                                vrs] <- rev(c(max(as.numeric(unlist(temp[2:5, 
                                vrs[1]]))), min(as.numeric(unlist(temp[2:5, 
                                vrs[2]]))))), temp[1, vrs] <- c(max(as.numeric(unlist(temp[2:5, 
                                vrs[1]]))), min(as.numeric(unlist(temp[2:5, 
                                vrs[2]])))))
                            }
                            pl[[length(pl) + 1L]] <- temp[1, 
                              ]
                            pln <- append(pln, "NA-NA")
                          }
                        }
                        else {
                          temp <- x[[k]][[i]][j, ][rep(row.names(x[[k]][[i]][j, 
                            ]), 3), ]
                          temptaq <- temp[, vrs[1]]
                          temptpq <- temp[, vrs[2]]
                          tpqlvls <- c(levels(factor(temptaq)), 
                            levels(factor(temptpq)), mtpq, (as.numeric(x[[k]][[i]][j, 
                              vrs[1]]) + avgts))
                          tpqlvls <- tpqlvls[which(is.na(tpqlvls) == 
                            FALSE)]
                          taqlvls <- c(levels(factor(temptaq)), 
                            levels(factor(temptpq)), mtaq, (as.numeric(x[[k]][[i]][j, 
                              vrs[2]]) - avgts))
                          taqlvls <- taqlvls[which(is.na(taqlvls) == 
                            FALSE)]
                        }
                        if (is.na(x[[k]][[i]][j, vrs[1]]) == 
                          FALSE && is.na(x[[k]][[i]][j, vrs[2]]) == 
                          TRUE) {
                          temp[, vrs] <- factor(temp[, vrs], 
                            levels = tpqlvls)
                          temp[, vrs[1]] <- temptaq
                          temp[2, vrs[2]] <- mtpq
                          temp[3, vrs[2]] <- (as.numeric(x[[k]][[i]][j, 
                            vrs[1]]) + avgts)
                          compl[[length(compl) + 1L]] <- temp[2:3, 
                            ]
                          compln <- append(compln, "taq-NA")
                          if (isTRUE(pool == TRUE) == TRUE) {
                            if (match.arg(type) == "1") {
                              temp[1, vrs[2]] <- max(as.numeric(unlist(temp[2:3, 
                                vrs])))
                            }
                            else if (match.arg(type) == "2") {
                              temp[1, vrs[2]] <- min(as.numeric(unlist(temp[2:3, 
                                vrs[2]])))
                            }
                            pl[[length(pl) + 1L]] <- temp[1, 
                              ]
                            pln <- append(pln, "taq-NA")
                          }
                        }
                        else if (is.na(x[[k]][[i]][j, vrs[1]]) == 
                          TRUE && is.na(x[[k]][[i]][j, vrs[2]]) == 
                          FALSE) {
                          temp[, vrs] <- factor(temp[, vrs], 
                            levels = taqlvls)
                          temp[, vrs[2]] <- temptpq
                          temp[2, vrs[1]] <- mtaq
                          temp[3, vrs[1]] <- (as.numeric(x[[k]][[i]][j, 
                            vrs[2]]) - avgts)
                          compl[[length(compl) + 1L]] <- temp[2:3, 
                            ]
                          compln <- append(compln, "NA-tpq")
                          if (isTRUE(pool == TRUE) == TRUE) {
                            temp[1, vrs[1]] <- min(as.numeric(unlist(temp[2:3, 
                              vrs])))
                            pl[[length(pl) + 1L]] <- temp[1, 
                              ]
                            pln <- append(pln, "NA-tpq")
                          }
                        }
                      }
                    }
                    rm(j)
                    names(compl) <- compln
                    if (isTRUE(pool == TRUE) == TRUE) {
                      names(pl) <- pln
                    }
                    ifelse(isTRUE(collapse == TRUE) == TRUE && 
                      isTRUE(is.data.frame(compl) == FALSE) == 
                        TRUE, xc[[k]][[i]] <- do.call("rbind", 
                      compl), xc[[k]][[i]] <- compl)
                  }
                  else if (any(is.na(x[[k]][[i]][, vrs])) == 
                    FALSE) {
                    NA
                  }
                }
                rm(i)
            }
            else if (is.null(dim(x[[k]][[1]])) == TRUE) {
                vrs <- c(which(colnames(x[[k]]) == vars[1]), 
                  which(colnames(x[[k]]) == vars[2]))
                if (any(is.na(x[[k]][, vrs])) == TRUE) {
                  if (!(exists("rpd"))) {
                    utils::data("rpd", package = "sdam", envir = environment())
                    rpd <- get("rpd", envir = environment())
                  }
                  else {
                    invisible(NA)
                  }
                  if (all(is.na(x[[k]][, vrs])) == FALSE) {
                    avg <- round(mean(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][, 
                      vrs]))))))
                  }
                  else {
                    flgna <- FALSE
                    for (i in seq_len(length(rpd))) {
                      if (any(attr(rpd[[i]], "class") == as.vector(x[[k]]$id)[1]) == 
                        TRUE) {
                        message("avg taken from province.")
                        flgna <- TRUE
                        avg <- rpd[[i]][1]
                        break
                      }
                      else {
                        NA
                      }
                    }
                    rm(i)
                    ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                      avg <- round(mean(as.vector(stats::na.omit(as.numeric(unlist(lapply(x, 
                        `[`, vrs), use.names = FALSE)))))))
                  }
                  if (all(is.na(x[[k]][, vrs[1]])) == FALSE) {
                    mtaq <- min(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][, 
                      vrs])))))
                  }
                  else {
                    flgna <- FALSE
                    for (i in seq_len(length(rpd))) {
                      if (any(attr(rpd[[i]], "class") == as.vector(x[[k]]$id)[1]) == 
                        TRUE) {
                        message("min TAQ taken from province.")
                        flgna <- TRUE
                        mtaq <- rpd[[i]][2]
                        break
                      }
                      else {
                        NA
                      }
                    }
                    rm(i)
                    ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                      mtaq <- min(as.vector(stats::na.omit(as.numeric(unlist(lapply(x, 
                        `[`, vrs), use.names = FALSE))))))
                  }
                  if (all(is.na(x[[k]][, vrs[2]])) == FALSE) {
                    mtpq <- max(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][, 
                      vrs])))))
                  }
                  else {
                    flgna <- FALSE
                    for (i in seq_len(length(rpd))) {
                      if (any(attr(rpd[[i]], "class") == as.vector(x[[k]]$id)[1]) == 
                        TRUE) {
                        message("max TPQ taken from province.")
                        flgna <- TRUE
                        mtpq <- rpd[[i]][3]
                        break
                      }
                      else {
                        NA
                      }
                    }
                    rm(i)
                    ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                      mtpq <- max(as.vector(stats::na.omit(as.numeric(unlist(lapply(x, 
                        `[`, vrs), use.names = FALSE))))))
                  }
                  if (all(is.na(x[[k]][, vrs[1]])) == FALSE && 
                    all(is.na(x[[k]][, vrs[2]])) == FALSE) {
                    avgts <- round(mean(as.vector(stats::na.omit(as.numeric(as.vector(unlist(x[[k]][, 
                      vrs[2]]))) - as.numeric(as.vector(unlist(x[[k]][, 
                      vrs[1]])))))))
                  }
                  else {
                    flgna <- FALSE
                    for (i in seq_len(length(rpd))) {
                      if (any(attr(rpd[[i]], "class") == as.vector(x[[k]]$id)[1]) == 
                        TRUE) {
                        message("avg len TS taken from province.")
                        flgna <- TRUE
                        avgts <- rpd[[i]][4]
                        break
                      }
                      else {
                        NA
                      }
                    }
                    rm(i)
                    ifelse(isTRUE(flgna == TRUE) == TRUE, NA, 
                      avgts <- 177)
                  }
                  compl <- list()
                  compln <- vector()
                  if (isTRUE(pool == TRUE) == TRUE) {
                    pl <- list()
                    pln <- vector()
                  }
                  for (j in seq_len(nrow(x[[k]]))) {
                    if (any(is.na(x[[k]][j, vrs]) == TRUE) == 
                      FALSE) {
                      compl[[length(compl) + 1L]] <- x[[k]][j, 
                        ]
                      compln <- append(compln, "complete")
                    }
                    else {
                      if (all(is.na(x[[k]][j, vrs])) == TRUE) {
                        temp <- x[[k]][j, ][rep(row.names(x[[k]][j, 
                          ]), 5), ]
                        temp[, vrs] <- factor(temp[, vrs], levels = NULL)
                        temp[2, vrs] <- c(mtaq, mtpq)
                        temp[3, vrs] <- c(mtaq, (mtaq + avgts))
                        temp[4, vrs] <- c((mtpq - avgts), mtpq)
                        temp[5, vrs] <- rep(avg, 2)
                        compl[[length(compl) + 1L]] <- temp[2:5, 
                          ]
                        compln <- append(compln, "NA-NA")
                        if (isTRUE(pool == TRUE) == TRUE) {
                          if (match.arg(type) == "1") {
                            temp[1, vrs] <- c(min(as.numeric(unlist(temp[2:5, 
                              vrs[1]]))), max(as.numeric(unlist(temp[2:5, 
                              vrs[2]]))))
                          }
                          else if (match.arg(type) == "2") {
                            ifelse(isTRUE(max(as.numeric(unlist(temp[2:5, 
                              vrs[1]]))) > min(as.numeric(unlist(temp[2:5, 
                              vrs[2]])))) == TRUE, temp[1, vrs] <- rev(c(max(as.numeric(unlist(temp[2:5, 
                              vrs[1]]))), min(as.numeric(unlist(temp[2:5, 
                              vrs[2]]))))), temp[1, vrs] <- c(max(as.numeric(unlist(temp[2:5, 
                              vrs[1]]))), min(as.numeric(unlist(temp[2:5, 
                              vrs[2]])))))
                          }
                          pl[[length(pl) + 1L]] <- temp[1, ]
                          pln <- append(pln, "NA-NA")
                        }
                      }
                      else {
                        temp <- x[[k]][j, ][rep(row.names(x[[k]][j, 
                          ]), 3), ]
                        temptaq <- temp[, vrs[1]]
                        temptpq <- temp[, vrs[2]]
                        tpqlvls <- c(levels(factor(temptaq)), 
                          levels(factor(temptpq)), mtpq, (as.numeric(x[[k]][j, 
                            vrs[1]]) + avgts))
                        taqlvls <- c(levels(factor(temptaq)), 
                          levels(factor(temptpq)), mtaq, (as.numeric(x[[k]][j, 
                            vrs[2]]) - avgts))
                        if (is.na(x[[k]][j, vrs[1]]) == FALSE && 
                          is.na(x[[k]][j, vrs[2]]) == TRUE) {
                          temp[, vrs] <- factor(temp[, vrs], 
                            levels = tpqlvls)
                          temp[, vrs[1]] <- temptaq
                          temp[2, vrs[2]] <- mtpq
                          temp[3, vrs[2]] <- (as.numeric(x[[k]][j, 
                            vrs[1]]) + avgts)
                          compl[[length(compl) + 1L]] <- temp[2:3, 
                            ]
                          compln <- append(compln, "taq-NA")
                          if (isTRUE(pool == TRUE) == TRUE) {
                            if (match.arg(type) == "1") {
                              temp[1, vrs[2]] <- max(as.numeric(unlist(temp[2:3, 
                                vrs])))
                            }
                            else if (match.arg(type) == "2") {
                              temp[1, vrs[2]] <- min(as.numeric(unlist(temp[2:3, 
                                vrs[2]])))
                            }
                            pl[[length(pl) + 1L]] <- temp[1, 
                              ]
                            pln <- append(pln, "taq-NA")
                          }
                        }
                        else if (is.na(x[[k]][j, vrs[1]]) == 
                          TRUE && is.na(x[[k]][j, vrs[2]]) == 
                          FALSE) {
                          temp[, vrs] <- factor(temp[, vrs], 
                            levels = taqlvls)
                          temp[, vrs[2]] <- temptpq
                          temp[2, vrs[1]] <- mtaq
                          temp[3, vrs[1]] <- (as.numeric(x[[k]][j, 
                            vrs[2]]) - avgts)
                          compl[[length(compl) + 1L]] <- temp[2:3, 
                            ]
                          compln <- append(compln, "NA-tpq")
                          if (isTRUE(pool == TRUE) == TRUE) {
                            temp[1, vrs[1]] <- min(as.numeric(unlist(temp[2:3, 
                              vrs])))
                            pl[[length(pl) + 1L]] <- temp[1, 
                              ]
                            pln <- append(pln, "NA-tpq")
                          }
                        }
                      }
                    }
                  }
                  rm(j)
                  names(compl) <- compln
                  if (isTRUE(pool == TRUE) == TRUE) {
                    names(pl) <- pln
                  }
                }
                else if (any(is.na(x[[k]][, vrs])) == FALSE) {
                  compl <- x[[k]]
                  compln <- append(compln, "complete")
                }
                ifelse(isTRUE(collapse == TRUE) == TRUE && isTRUE(is.data.frame(compl) == 
                  FALSE) == TRUE, xc[[k]] <- do.call("rbind", 
                  compl), xc[[k]] <- compl)
                cnt <- append(nrow(xc[[k]]), cnt)
            }
            cnt <- suppressWarnings(append(sum(as.numeric(summary(xc[[k]])[, 
                1])), cnt))
        }
        rm(k)
    }
    if (isTRUE(pool == TRUE) == TRUE) {
        if (isTRUE(collapse == TRUE) == TRUE) {
            plc <- do.call("rbind", pl)
            return(plc)
        }
        else {
            return(pl)
        }
    }
    else {
        if (isTRUE(collapse == TRUE) == TRUE) {
            xcc <- vector("list", length = length(x))
            for (k in seq_len(length(xc))) {
                if (isTRUE(is.data.frame(xc[[k]]) == FALSE) == 
                  TRUE) {
                  xcc[[k]] <- do.call("rbind", xc[[k]])
                }
                else {
                  xcc[[k]] <- xc[[k]]
                }
            }
            rm(k)
            class(xcc) <- noquote(c(ocl, apply(do.call("rbind", 
                lapply(xcc, nrow)), 2, sum)))
            xcc
        }
        else {
            class(xc) <- noquote(c(ocl, sum(cnt)))
            xc
        }
    }
}
