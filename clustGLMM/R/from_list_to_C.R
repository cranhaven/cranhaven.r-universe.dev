from_list_to_C <-
function(draws,
         iterations,
         p,
         settings,
         yspecd1=NULL,
         yspecd2=NULL,
         d2spec=NULL,
         family)
{
  v <- c(settings[p, ])
  Ys <- c()
  if(v$ynums){Ys <- c(Ys, names(family)[is.element(family, c("num", "gaussian"))])}
  if(v$ypois){Ys <- c(Ys, names(family)[is.element(family, c("poi", "poisson"))])}
  if(v$ybins){Ys <- c(Ys, names(family)[is.element(family, c("bin", "bernoulli"))])}
  if(v$yords){Ys <- c(Ys, names(family)[is.element(family, c("ord", "cumulative"))])}
  if(v$ycats){Ys <- c(Ys, names(family)[is.element(family, c("cat", "categorical"))])}
  
  if(missing(iterations)){iterations <- draws$m}
  ind_iterations <- is.element(draws$m, iterations)  
  
  sumd1 <- ifelse(v$ydepd1, sum(yspecd1), ifelse(v$d1==0,1,v$d1)*length(Ys))
  RET <- c()
  
  if(v$dimswithG > 0){
    if(v$gspec){
      if(v$isy){
        if(length(Ys) > 0){
          if(v$D==0){
            for(i in iterations){
              di = which(draws$m==i)
              for(k in 1:v$G){
                for(y in 1:length(Ys)){
                  RET <- c(RET, draws[[p]][[k]][di, Ys[y]])
                }
              }
            }
          } # end of D==0
          
          if(v$D==1){
            for(i in iterations){
              di = which(draws$m==i)
              for(k in 1:v$G){
                for(y in 1:length(Ys)){
                  RET <- c(RET, draws[[p]][[k]][[Ys[y]]][di, ])
                }
              }
            }
          } # end of D==1
          
          if(v$D==2){
            for(i in iterations){
              di = which(draws$m==i)
              for(k in 1:v$G){
                for(y in 1:length(Ys)){
                  d1 = ifelse(v$ydepd1, yspecd1[Ys[y]], v$d1)
                  d2 = ifelse(v$ydepd2, yspecd2[Ys[y]], v$d2)
                  if(v$sym){
                    # matrix is symmetrical and only upper-right triangle is stored
                    # d1 = d2
                    if(v$diag){
                      for(col in 1:d1){
                        for(row in 1:col){
                          RET <- c(RET, draws[[p]][[k]][[Ys[y]]][di, row, col])
                        }
                      }
                    }else{
                      for(col in 2:d1){
                        for(row in 1:(col-1)){
                          RET <- c(RET, draws[[p]][[k]][[Ys[y]]][di, row, col])
                        }
                      }
                    } # end of else of diag
                  }else{
                    # matrix is general rectangular --> all is stored
                    if(v$BYROW){
                      RET <- c(RET, t(draws[[p]][[k]][[Ys[y]]][di,,]))
                    }else{
                      RET <- c(RET, draws[[p]][[k]][[Ys[y]]][di,,])
                    }
                  } # end of else of sym
                }
              }
            }
          } # end of D==2
        }
      }else{
        # is class-specific, however not y-specific
  
        if(v$D==0){
          RET <- c(t(draws[[p]][is.element(draws$m, iterations), ]))
        } # end of D==0
        
        if(v$D==1){
          for(i in iterations){
            di = which(draws$m==i)
            for(k in 1:v$G){
              RET <- c(RET, draws[[p]][[k]][di,])
            }
          }
        } # end of D==1
        
        if(v$D==2){
          for(i in iterations){
            di = which(draws$m==i)
            for(k in 1:v$G){
              if(v$sym){
                # matrix is symmetrical and only upper-right triangle is stored
                # d1 = d2
                if(v$diag){
                  for(col in 1:v$d1){
                    for(row in 1:col){
                      RET <- c(RET, draws[[p]][[k]][di, row, col])
                    }
                  }
                }else{
                  for(col in 2:v$d1){
                    for(row in 1:(col-1)){
                      RET <- c(RET, draws[[p]][[k]][di, row, col])
                    }
                  }
                } # end of else of diag
              }else{
                # matrix is general rectangular --> all is stored
                if(v$BYROW){
                  if(v$d2spec){
                    for(i in 1:v$d1){
                      RET <- c(RET, draws[[p]][[k]][di,i,1:d2spec[i]])
                    }
                  }else{
                    RET <- c(RET, t(draws[[p]][[k]][di,,]))
                  }
                }else{
                  RET <- c(RET, draws[[p]][[k]][di,,])
                }
              } # end of else of sym
            }
          }
        } # end of D==2
        
      } # end of else of isy
    }else{
      # not class-specific
      if(v$isy){
        if(length(Ys) > 0){
          # not class-specific, but still y-specific
          if(v$D==0){
            RET <- c(t(draws[[p]][is.element(draws$m, iterations), ]))
          } # end of D==0
        
          if(v$D==1){
            for(i in iterations){
              di = which(draws$m==i)
              for(y in 1:length(Ys)){
                RET <- c(RET, draws[[p]][[Ys[y]]][di, ])
              }
            }
          } # end of D==1
          
          if(v$D==2){
            for(i in iterations){
              di = which(draws$m==i)
              for(y in 1:length(Ys)){
                d1 = ifelse(v$ydepd1, yspecd1[Ys[y]], v$d1)
                d2 = ifelse(v$ydepd2, yspecd2[Ys[y]], v$d2)
                if(v$sym){
                  # matrix is symmetrical and only upper-right triangle is stored
                  # d1 = d2
                  if(v$diag){
                    for(col in 1:d1){
                      for(row in 1:col){
                        RET <- c(RET, draws[[p]][[Ys[y]]][di, row, col])
                      }
                    }
                  }else{
                    for(col in 2:d1){
                      for(row in 1:(col-1)){
                        RET <- c(RET, draws[[p]][[Ys[y]]][di, row, col])
                      }
                    }
                  } # end of else of diag
                }else{
                  # matrix is general rectangular --> all is stored
                  if(v$BYROW){
                    RET <- c(RET, t(draws[[p]][[Ys[y]]][di,,]))
                  }else{
                    RET <- c(RET, draws[[p]][[Ys[y]]][di,,])
                  }
                } # end of else of sym
              }
            }
          } # end of D==2
        }
      }else{
        # is NEITHER class-specific, nor y-specific
        
        if(v$D==0){
          RET <- draws[[p]][is.element(draws$m, iterations)]
        } # end of D==0
        
        if(v$D==1){
          RET <- c(t(draws[[p]][is.element(draws$m, iterations),]))
        } # end of D==1
        
        if(v$D==2){
          for(i in iterations){
            di = which(draws$m==i)
            if(v$sym){
              # matrix is symmetrical and only upper-right triangle is stored
              # d1 = d2
              if(v$diag){
                for(col in 1:v$d1){
                  for(row in 1:col){
                    RET <- c(RET, draws[[p]][di, row, col])
                  }
                }
              }else{
                for(col in 2:v$d1){
                  for(row in 1:(col-1)){
                    RET <- c(RET, draws[[p]][di, row, col])
                  }
                }
              } # end of else of diag
            }else{
              # matrix is general rectangular --> all is stored
              if(v$BYROW){
                if(v$d2spec){
                  for(i in 1:v$d1){
                    RET <- c(RET, draws[[p]][di,i,1:d2spec[i]])
                  }
                }else{
                  RET <- c(RET, t(draws[[p]][di,,]))
                }
              }else{
                RET <- c(RET, draws[[p]][di,,])
              }
            } # end of else of sym
          }
        } # end of D==2
      } # end of else of isy
    } # end of else of gspec
  }else{
    # the dimension is zero --> return empty set
  }
  
  return(RET)

}
