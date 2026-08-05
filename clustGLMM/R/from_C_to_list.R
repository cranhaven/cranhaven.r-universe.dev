from_C_to_list <-
function(values,
         p,
         settings,
         yspecd1=NULL,
         yspecd2=NULL,
         d2spec=NULL,
         family)        # parameter to be listed
{
  # data frame settings consists of needed parameters:
  # save, gspec, G, isy, ynums, yords, ybins, iter, 
  # d1, d2, BYROW, sym, diag, diagval, D
  v <- c(settings[p, ])
  niter <- length(values) / v$dimswithG
  Ys <- c()
  if(v$ynums){Ys <- c(Ys, names(family)[is.element(family, c("num", "gaussian"))])}
  if(v$ypois){Ys <- c(Ys, names(family)[is.element(family, c("poi", "poisson"))])}
  if(v$ybins){Ys <- c(Ys, names(family)[is.element(family, c("bin", "bernoulli"))])}
  if(v$yords){Ys <- c(Ys, names(family)[is.element(family, c("ord", "cumulative"))])}
  if(v$ycats){Ys <- c(Ys, names(family)[is.element(family, c("cat", "categorical"))])}
  
  sumd1 <- ifelse(v$ydepd1, sum(yspecd1), v$d1*length(Ys))
  #sumd1 <- v$d1*length(Ys)
  
  if(v$dimswithG > 0){
    if(v$gspec){
      RET <- list()
      if(v$isy){
        
        if(v$D==0){
          for(k in 1:v$G){
            ## option with [[y]]
            #RET[[k]] <- list()
            #for(y in 1:length(Ys)){
            #RET[[k]][[Ys[y]]] <- values[seq(from = (k-1)*length(Ys)+y, 
            #                                to = G*length(Ys)*iter, 
            #                                by = G*length(Ys))]
            # }
            
            ## better --> no need for [[y]] --> can be stored in matrix with columns names Ys
            RET[[k]] <- matrix(NA, nrow = niter, ncol = length(Ys))
            colnames(RET[[k]]) <- Ys
            for(y in 1:length(Ys)){
              RET[[k]][, Ys[y]] = values[seq(from = (k-1)*length(Ys)+y, 
                                             to = v$G*length(Ys)*niter, 
                                             by = v$G*length(Ys))]
            }
            
          }
        } # end of D==0
        
        if(v$D==1){
          for(k in 1:v$G){
            RET[[k]] <- list()
            dimy = 0
            for(y in 1:length(Ys)){
              d1 = ifelse(v$ydepd1, yspecd1[Ys[y]], v$d1)
              #d1 = v$d1
              RET[[k]][[Ys[y]]] <- matrix(NA, nrow = niter, ncol = d1)
              if(d1 > 0){
                for(m in 1:niter){
                  RET[[k]][[Ys[y]]][m, ] <- values[(m-1)*v$G*sumd1 + (k-1)*sumd1 + dimy + 1:d1]
                }
                dimy = dimy + d1
              }
            }
          }
        } # end of D==1
        
        if(v$D==2){
          for(k in 1:v$G){
            RET[[k]] <- list()
            if(v$sym){
              if(v$diag){
                dimtot = ifelse(v$ydepd1, sum(yspecd1*(yspecd1+1)/2), length(Ys)*v$d1*(v$d1+1)/2)
              }else{
                dimtot = ifelse(v$ydepd1, sum((yspecd1-1)*yspecd1/2), length(Ys)*(v$d1-1)*v$d1/2)
              }
            }else{
              dimtot = ifelse(v$ydepd1|v$ydepd2, sum(yspecd1*yspecd2), length(Ys)*v$d1*v$d2)
            }
            dimy = 0
            for(y in 1:length(Ys)){
              d1 = ifelse(v$ydepd1, yspecd1[Ys[y]], v$d1)
              d2 = ifelse(v$ydepd2, yspecd2[Ys[y]], v$d2)
              RET[[k]][[Ys[y]]] <- array(NA, dim = c(niter, d1, d2))
              for(m in 1:niter){
                if(v$sym){
                  # matrix is symmetrical and only upper-right triangle is stored
                  # d1 = d2
                  if(v$diag){
                    dd = d1*(d1+1)/2 # number of elements 
                    for(row in 1:d1){
                      for(col in row:d1){
                        RET[[k]][[Ys[y]]][m, row, col] <- RET[[k]][[Ys[y]]][m, col, row] <-
                          values[(m-1)*v$G*dimtot + (k-1)*dimtot + dimy + row + (col-1)*col/2]
                      }
                    }
                  }else{
                    dd = (d1-1)*d1/2 # number of elements
                    # non-diagonal elements
                    for(row in 1:(d1-1)){
                      for(col in (row+1):d1){
                        RET[[k]][[Ys[y]]][m, row, col] <- RET[[k]][[Ys[y]]][m, col, row] <-
                          values[(m-1)*v$G*dimtot + (k-1)*dimtot + dimy + row + (col-2)*(col-1)/2]
                      }
                    }
                    # diagonal elements
                    for(row in 1:d1){
                      RET[[k]][[Ys[y]]][m, row, row] <- v$diagval
                    }
                  } # end of else of diag
                }else{
                  # matrix is general rectangular --> all is stored
                  dd = d1*d2
                  RET[[k]][[Ys[y]]][m,,] <- matrix(values[(m-1)*v$G*dimtot + (k-1)*dimtot + dimy + 1:dd],
                                                   nrow = d1,
                                                   ncol = d2,
                                                   byrow = v$BYROW)
                } # end of else of sym
              }
              dimy = dimy + dd
            }
          }
        } # end of D==2
        
      }else{
        # is class-specific, however not y-specific
        
        if(v$D==0){
          RET <- matrix(values, nrow = niter, ncol = v$G, byrow = TRUE)
          # RET <- matrix(NA, nrow = niter, ncol = v$G)
          # for(k in 1:v$G){
          #   # no need to do [[k]] --> can be stored in matrix
          #   # column corresponds to k
          #   RET[, k] = values[seq(from = k,
          #                         to = v$G*niter,
          #                         by = v$G)]
          #   
          # }
        } # end of D==0
        
        if(v$D==1){
          for(k in 1:v$G){
            RET[[k]] <- matrix(NA, nrow = niter, ncol = v$d1)
            for(m in 1:niter){
              RET[[k]][m, ] <- values[(m-1)*v$G*v$d1 + (k-1)*v$d1 + 1:v$d1]          
            }
          }
        } # end of D==1
        
        if(v$D==2){
          for(k in 1:v$G){
            RET[[k]] <- array(NA, dim = c(niter, v$d1, v$d2))
            for(m in 1:niter){
              if(v$sym){
                # matrix is symmetrical and only upper-right triangle is stored
                # d1 = d2
                if(v$diag){
                  dd = v$d1*(v$d1+1)/2 # number of elements 
                  for(row in 1:v$d1){
                    for(col in row:v$d1){
                      RET[[k]][m, row, col] <- RET[[k]][m, col, row] <-
                        values[(m-1)*v$G*dd + (k-1)*dd + row + (col-1)*col/2]
                    }
                  }
                }else{
                  dd = (v$d1-1)*v$d1/2 # number of elements
                  # non-diagonal elements
                  for(row in 1:(v$d1-1)){
                    for(col in (row+1):v$d1){
                      RET[[k]][m, row, col] <- RET[[k]][m, col, row] <-
                        values[(m-1)*v$G*dd + (k-1)*dd + row + (col-2)*(col-1)/2]
                    }
                  }
                  # diagonal elements
                  for(row in 1:v$d1){
                    RET[[k]][m, row, row] <- v$diagval
                  }
                } # end of else of diag
              }else{
                # matrix is general rectangular --> all is stored
                if(v$d2spec){
                  cumsumni <- 0
                  for(i in 1:v$d1){
                    RET[[k]][m,i,seq_len(yspecd2[i])] <- values[(m-1)*v$dimswithG + (k-1)*v$dims + cumsumni + seq_len(yspecd2[i])]
                    cumsumni <- cumsumni + yspecd2[i]
                  }
                }else{
                  RET[[k]][m,,] <- matrix(values[(m-1)*v$G*v$d1*v$d2 + (k-1)*v$d1*v$d2 + 1:(v$d1*v$d2)],
                                          nrow = v$d1,
                                          ncol = v$d2,
                                          byrow = v$BYROW)
                }
              } # end of else of sym
            }
          }
        } # end of D==2
        
      } # end of else of isy
    }else{
      # not class-specific
      if(v$isy){
        # not class-specific, but still y-specific
        if(v$D==0){
          ## option with [[y]]
          #RET <- list()
          #for(y in 1:length(Ys)){
          #RET[[Ys[y]]] <- values[seq(from = y, 
          #                                to = length(Ys)*iter, 
          #                                by = length(Ys))]
          # }
          
          ## better --> no need for [[y]] --> can be stored in matrix with columns names Ys
          RET <- matrix(0, nrow = niter, ncol = length(Ys))
          colnames(RET) <- Ys
          for(y in 1:length(Ys)){
            RET[, Ys[y]] = values[seq(from = y,
                                      to = length(Ys)*niter,
                                      by = length(Ys))]
          }
          
        } # end of D==0
        
        if(v$D==1){
          RET <- list()
          dimtot = ifelse(v$ydepd1, sum(yspecd1), length(Ys)*v$d1)
          dimy = 0
          for(y in 1:length(Ys)){
            d1 = ifelse(v$ydepd1, yspecd1[Ys[y]], v$d1)
            RET[[Ys[y]]] <- matrix(NA, nrow = niter, ncol = d1)
            if(d1 > 0){
              for(m in 1:niter){
                RET[[Ys[y]]][m, ] <- values[(m-1)*dimtot + dimy + 1:d1]
              }
              dimy = dimy + d1
            }
          }
        } # end of D==1
        
        if(v$D==2){
          RET <- list()
          if(v$sym){
            if(v$diag){
              dimtot = ifelse(v$ydepd1, sum(yspecd1*(yspecd1+1)/2), length(Ys)*v$d1*(v$d1+1)/2)
            }else{
              dimtot = ifelse(v$ydepd1, sum((yspecd1-1)*yspecd1/2), length(Ys)*(v$d1-1)*v$d1/2)
            }
          }else{
            dimtot = ifelse(v$ydepd1|v$ydepd2, sum(yspecd1*yspecd2), length(Ys)*v$d1*v$d2)
          }
          dimy = 0
          for(y in 1:length(Ys)){
            d1 = ifelse(v$ydepd1, yspecd1[Ys[y]], v$d1)
            d2 = ifelse(v$ydepd2, yspecd2[Ys[y]], v$d2)
            RET[[Ys[y]]] <- array(NA, dim = c(niter, d1, d2))
            for(m in 1:niter){
              if(v$sym){
                # matrix is symmetrical and only upper-right triangle is stored
                # d1 = d2
                if(v$diag){
                  dd = d1*(d1+1)/2 # number of elements 
                  for(row in 1:d1){
                    for(col in row:d1){
                      RET[[Ys[y]]][m, row, col] <- RET[[Ys[y]]][m, col, row] <-
                        values[(m-1)*dimtot + dimy + row + (col-1)*col/2]
                    }
                  }
                }else{
                  dd = (d1-1)*d1/2 # number of elements
                  # non-diagonal elements
                  for(row in 1:(d1-1)){
                    for(col in (row+1):d1){
                      RET[[Ys[y]]][m, row, col] <- RET[[Ys[y]]][m, col, row] <-
                        values[(m-1)*dimtot  + dimy + row + (col-2)*(col-1)/2]
                    }
                  }
                  # diagonal elements
                  for(row in 1:d1){
                    RET[[Ys[y]]][m, row, row] <- v$diagval
                  }
                } # end of else of diag
              }else{
                # matrix is general rectangular --> all is stored
                dd = d1 * d2
                RET[[Ys[y]]][m,,] <- matrix(values[(m-1)*dimtot + dimy + 1:dd],
                                            nrow = d1,
                                            ncol = d2,
                                            byrow = v$BYROW)
              } # end of else of sym
            }
            dimy = dimy + dd
          }
        } # end of D==2
        
      }else{
        # is NEITHER class-specific, nor y-specific
        
        if(v$D==0){
          RET <- values
        } # end of D==0
        
        if(v$D==1){
          RET <-  matrix(values, nrow = niter, ncol = v$d1, byrow = v$BYROW)
        } # end of D==1
        
        if(v$D==2){
          RET <- array(NA, dim = c(niter, v$d1, v$d2))
          for(m in 1:niter){
            if(v$sym){
              # matrix is symmetrical and only upper-right triangle is stored
              # d1 = d2
              if(v$diag){
                dd = v$d1*(v$d1+1)/2 # number of elements 
                for(row in 1:v$d1){
                  for(col in row:v$d1){
                    RET[m, row, col] <- RET[m, col, row] <-
                      values[(m-1)*dd + row + (col-1)*col/2]
                  }
                }
              }else{
                dd = (v$d1-1)*v$d1/2 # number of elements
                # non-diagonal elements
                for(row in 1:(v$d1-1)){
                  for(col in (row+1):v$d1){
                    RET[m, row, col] <- RET[m, col, row] <-
                      values[(m-1)*dd + row + (col-2)*(col-1)/2]
                  }
                }
                # diagonal elements
                for(row in 1:v$d1){
                  RET[m, row, row] <- v$diagval
                }
              } # end of else of diag
            }else{
              # matrix is general rectangular --> all is stored
              if(v$d2spec){
                cumsumni <- 0
                for(i in 1:v$d1){
                  RET[m,i,seq_len(yspecd2[i])] <- values[(m-1)*v$dims + cumsumni + seq_len(yspecd2[i])]
                  cumsumni <- cumsumni + yspecd2[i]
                }
              }else{
                RET[m,,] <- matrix(values[(m-1)*v$d1*v$d2 + 1:(v$d1*v$d2)],
                                   nrow = v$d1,
                                   ncol = v$d2,
                                   byrow = v$BYROW)
              }
            } # end of else of sym
          }
        } # end of D==2
      } # end of else of isy
    } # end of else of gspec
  }else{
    # parameter has no dimension --> return empty vector = NULL
    RET <- c()
  }
  
  return(RET)
}
