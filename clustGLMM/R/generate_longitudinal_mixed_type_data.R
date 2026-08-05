generate_longitudinal_mixed_type_data <-
function(
    n, n_i = 4,
    G = 2, timepar = "parallel",
    catpar = "no", catsubjfix = TRUE,
    xin1 = 1,
    Sigma, 
    Kord = 5, Kcat = 4,
    kspec_bi_cat = FALSE){
  # require(mvtnorm)
  # require(splines)
  
  ### Some functions to be used
  softmax <- function(x){
    exp_x <- exp(x - max(x))
    return(as.numeric(exp_x / sum(exp_x)))
  }
  
  ### Preparation
  gSigmatrue <- is(Sigma, "list")
  if(gSigmatrue){
    totnran <- dim(Sigma[[1]])[1]
  }else{
    totnran <- dim(Sigma)[1]
  }
  
  degree <- 2
  knots <- c(0, 0.25, 0.5, 0.75, 1)
  inner <- knots[-c(1, length(knots))]
  bound <- knots[c(1, length(knots))]
  xgrid <- seq(0,1,by=0.01)
  splB <- bs(xgrid, knots = inner, Boundary.knots = bound, 
             degree = degree, intercept = FALSE)
  spldim <- dim(splB)[2]
  
  #
  sd_num <- list()
  sd_num[[2]] <- c(0.5, 0.8)
  sd_num[[3]] <- c(0.5, 0.75, 1)
  
  
  beta_num <- beta_poi <- beta_bin <- beta_ord <- c_ord <- beta_cat <- list()
  if(G == 2){
    ### beta_num
    # intercept
    beta_num[[1]] <- c(-1)/ifelse(timepar=="spline",2,1)
    names(beta_num[[1]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = 2, 
                      spline = c(-1.3, -0.2, -0.4, 0.9, 0.2),
                      mix = c(-1.3, -0.2, -0.4, 0.9, 0.2))
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_num[[1]] <- c(beta_num[[1]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
    names(addbeta) <- "cat"
    beta_num[[1]] <- c(beta_num[[1]], addbeta)
    
    # intercept
    beta_num[[2]] <- c(1)/ifelse(timepar=="spline",2,1)
    names(beta_num[[2]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = -2, 
                      spline = c(0.3, 0.3, 0.3, -1, -1.3),
                      mix = c(0.3, 0.3, 0.3, -1, -1.3))
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_num[[2]] <- c(beta_num[[2]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
    names(addbeta) <- "cat"
    beta_num[[2]] <- c(beta_num[[2]], addbeta)
    
    ### beta_poi
    # intercept
    beta_poi[[1]] <- c(-1)/ifelse(timepar=="spline",2,1)
    names(beta_poi[[1]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = 2, 
                      spline = c(-0.8, -1.1, -1.0, 0.0, 2.7),
                      mix = 2)
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_poi[[1]] <- c(beta_poi[[1]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
    names(addbeta) <- "cat"
    beta_poi[[1]] <- c(beta_poi[[1]], addbeta)
    
    # intercept
    beta_poi[[2]] <- c(1)/ifelse(timepar=="spline",2,1)
    names(beta_poi[[2]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = -2, 
                      spline = c(-0.2, -0.4, 0.8, -0.4, 0.5),
                      mix = -2)
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_poi[[2]] <- c(beta_poi[[2]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
    names(addbeta) <- "cat"
    beta_poi[[2]] <- c(beta_poi[[2]], addbeta)
    
    ### beta_bin
    # intercept
    beta_bin[[1]] <- c(-1)/ifelse(timepar=="spline",2,1)
    names(beta_bin[[1]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = 2, 
                      spline = c(1.2, 1.6, -0.1, -0.3, -1.3),
                      mix = 2)
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_bin[[1]] <- c(beta_bin[[1]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
    names(addbeta) <- "cat"
    beta_bin[[1]] <- c(beta_bin[[1]], addbeta)
    
    # intercept
    beta_bin[[2]] <- c(1)/ifelse(timepar=="spline",2,1)
    names(beta_bin[[2]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = -2, 
                      spline = c(-1, 0.6, -0.3, -1.9, 0.9),
                      mix = 2)
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_bin[[2]] <- c(beta_bin[[2]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
    names(addbeta) <- "cat"
    beta_bin[[2]] <- c(beta_bin[[2]], addbeta)
    
    ### beta_ord
    # intercept
    beta_ord[[1]] <- c(0)
    names(beta_ord[[1]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = 2, 
                      spline = c(1, -0.1, -0.1, 1, 0.3),
                      mix = c(1, -0.1, -0.1, 1, 0.3))
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_ord[[1]] <- c(beta_ord[[1]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
    names(addbeta) <- "cat"
    beta_ord[[1]] <- c(beta_ord[[1]], addbeta)
    
    # intercept
    beta_ord[[2]] <- c(0)
    names(beta_ord[[2]]) <- "(Intercept)"
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = 2, 
                      cross = -2, 
                      spline = c(0.2, -0.2, 0.9, -0.1, 0.9),
                      mix = c(0.2, -0.2, 0.9, -0.1, 0.9))
    names(addbeta) <- paste0("time", 1:length(addbeta))
    beta_ord[[2]] <- c(beta_ord[[2]], addbeta)
    addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
    names(addbeta) <- "cat"
    beta_ord[[2]] <- c(beta_ord[[2]], addbeta)
    
    ### c_ord
    c_ord[[1]] <- c(-Inf, -0.5, 0.5, 1.5, 2.5, Inf) + ifelse(timepar=="parallel", 0, -1) + ifelse(timepar=="cross", 0.5, 0)
    c_ord[[2]] <- c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf) + ifelse(timepar=="parallel", 0, -1) + ifelse(timepar=="cross", -0.5, 0)
    
    ### beta_cat
    # intercept
    beta_cat[[1]] <- switch(timepar,
                            parallel = c(-1,-1,-1,0),
                            cross = c(-1,1,0,0),
                            spline = c(-0.5,-0.5,-0.5,0),
                            mix = c(-1,1,0,0))
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = c(2,-2,0,0), 
                      cross = c(2,-2,0,0), 
                      spline = c(-0.9, -0.5, 0.7, 0.8, -1.8,
                                 -1, 0.2, 0.5, 1.6, -1.7,
                                 -1.4, 0.5, -0.1, 0.9, -1.7,
                                 0,0,0,0,0),
                      mix = c(2,-2,0,0))
    addbetacat <- switch(catpar, no = c(0,0,0,0), fixed = c(0.7,-0.3,0.4,0), group = c(0.7,-0.3,0.4,0))
    beta_cat[[1]] <- matrix(c(beta_cat[[1]], addbeta, addbetacat), nrow = Kcat)
    colnames(beta_cat[[1]]) <- c("(Intercept)", 
                                 paste0("time", 1:(length(addbeta)/Kcat)),
                                 "cat")
    
    # intercept
    beta_cat[[2]] <- switch(timepar,
                            parallel = c(1,1,1,0),
                            cross = c(1,-1,0,0),
                            spline = c(0.5,0.5,0.5,0),
                            mix = c(1,-1,0,0))
    # time parametrization parameters
    addbeta <- switch(timepar, 
                      parallel = c(2,-2,0,0), 
                      cross = c(-2,2,0,0), 
                      spline = c(0.3, 0.6, 1.4, -0.8, 0.8,
                                 -0.8, -1.3, -0.2, -2.1, -0.1, 
                                 -1.2, 0.4, 0, 0.6, -0.6,
                                 0,0,0,0,0),
                      mix = c(-2,2,0,0))
    addbetacat <- switch(catpar, no = c(0,0,0,0), fixed = c(0.7,-0.3,0.4,0), group = c(-0.7,0.3,-0.4,0))
    beta_cat[[2]] <- matrix(c(beta_cat[[2]], addbeta, addbetacat), nrow = Kcat)
    colnames(beta_cat[[2]]) <- c("(Intercept)", 
                                 paste0("time", 1:(length(addbeta)/Kcat)),
                                 "cat")
  }else{
    if(G == 3){
      ### beta_num
      # intercept
      beta_num[[1]] <- c(-1)/ifelse(timepar=="spline",2,1)
      names(beta_num[[1]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 2, 
                        spline = c(-1.5, 0.7, -0.8, 0.1, -0.1),
                        mix = c(-1.5, 0.7, -0.8, 0.1, -0.1))
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_num[[1]] <- c(beta_num[[1]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
      names(addbeta) <- "cat"
      beta_num[[1]] <- c(beta_num[[1]], addbeta)
      
      # intercept
      beta_num[[2]] <- c(0)/ifelse(timepar=="spline",2,1)
      names(beta_num[[2]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 0, 
                        spline = c(0.8, -0.3, -0.8, -1, -0.1),
                        mix = c(0.8, -0.3, -0.8, -1, -0.1))
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_num[[2]] <- c(beta_num[[2]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
      names(addbeta) <- "cat"
      beta_num[[2]] <- c(beta_num[[2]], addbeta)
      
      # intercept
      beta_num[[3]] <- c(1)/ifelse(timepar=="spline",2,1)
      names(beta_num[[3]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = -2, 
                        spline = c(-0.3, -0.4, -0.3, 0.7, 0),
                        mix = c(-0.3, -0.4, -0.3, 0.7, 0))
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_num[[3]] <- c(beta_num[[3]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0)
      names(addbeta) <- "cat"
      beta_num[[3]] <- c(beta_num[[3]], addbeta)
      
      ### beta_poi
      # intercept
      beta_poi[[1]] <- c(-1)/ifelse(timepar=="spline",2,1)
      names(beta_poi[[1]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 2, 
                        spline = c(0.2, 0.0, -1.6, -1.0, 0.4),
                        mix = 2)
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_poi[[1]] <- c(beta_poi[[1]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
      names(addbeta) <- "cat"
      beta_poi[[1]] <- c(beta_poi[[1]], addbeta)
      
      # intercept
      beta_poi[[2]] <- c(0)/ifelse(timepar=="spline",2,1)
      names(beta_poi[[2]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 0, 
                        spline = c(-0.9, -0.6, 0.3, 0.1, 1.5),
                        mix = 0)
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_poi[[2]] <- c(beta_poi[[2]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
      names(addbeta) <- "cat"
      beta_poi[[2]] <- c(beta_poi[[2]], addbeta)
      
      # intercept
      beta_poi[[3]] <- c(1)/ifelse(timepar=="spline",2,1)
      names(beta_poi[[3]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = -2, 
                        spline = c(0.0, -0.6, 0.5, 0.6, -1.3),
                        mix = -2)
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_poi[[3]] <- c(beta_poi[[3]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0)
      names(addbeta) <- "cat"
      beta_poi[[3]] <- c(beta_poi[[3]], addbeta)
      
      ### beta_bin
      # intercept
      beta_bin[[1]] <- c(-1)/ifelse(timepar=="spline",2,1)
      names(beta_bin[[1]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 2, 
                        spline = c(0, -0.2, -1, 0.3, 0.5),
                        mix = 2)
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_bin[[1]] <- c(beta_bin[[1]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
      names(addbeta) <- "cat"
      beta_bin[[1]] <- c(beta_bin[[1]], addbeta)
      
      # intercept
      beta_bin[[2]] <- c(0)/ifelse(timepar=="spline",2,1)
      names(beta_bin[[2]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 0, 
                        spline = c(1.1, 0.5, 0.6, -0.6, -1),
                        mix = 2)
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_bin[[2]] <- c(beta_bin[[2]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
      names(addbeta) <- "cat"
      beta_bin[[2]] <- c(beta_bin[[2]], addbeta)
      
      # intercept
      beta_bin[[3]] <- c(1)/ifelse(timepar=="spline",2,1)
      names(beta_bin[[3]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = -2, 
                        spline = c(1.4, 0.3, -1.6, -0.2, 2),
                        mix = 2)
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_bin[[3]] <- c(beta_bin[[3]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0)
      names(addbeta) <- "cat"
      beta_bin[[3]] <- c(beta_bin[[3]], addbeta)
      
      
      ### beta_ord
      # intercept
      beta_ord[[1]] <- c(0)
      names(beta_ord[[1]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 2, 
                        spline = c(-1.4, -0.6, 0.6, 0, -2),
                        mix =  c(-1.4, -0.6, 0.6, 0, -2))
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_ord[[1]] <- c(beta_ord[[1]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = -0.7)
      names(addbeta) <- "cat"
      beta_ord[[1]] <- c(beta_ord[[1]], addbeta)
      
      # intercept
      beta_ord[[2]] <- c(0)
      names(beta_ord[[2]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = 0, 
                        spline = c(1, 0.2, -0.7, 1, -0.2),
                        mix = c(1, 0.2, -0.7, 1, -0.2))
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_ord[[2]] <- c(beta_ord[[2]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0.7)
      names(addbeta) <- "cat"
      beta_ord[[2]] <- c(beta_ord[[2]], addbeta)
      
      # intercept
      beta_ord[[3]] <- c(0)
      names(beta_ord[[3]]) <- "(Intercept)"
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = 2, 
                        cross = -2, 
                        spline = c(0.5, -0.5, -0.1, -1.1, -1.1),
                        mix = c(0.5, -0.5, -0.1, -1.1, -1.1))
      names(addbeta) <- paste0("time", 1:length(addbeta))
      beta_ord[[3]] <- c(beta_ord[[3]], addbeta)
      addbeta <- switch(catpar, no = 0, fixed = 0.7, group = 0)
      names(addbeta) <- "cat"
      beta_ord[[3]] <- c(beta_ord[[3]], addbeta)
      
      ### c_ord
      c_ord[[1]] <- c(-Inf, 0, 1, 2, 3, Inf)
      c_ord[[2]] <- c(-Inf, -1, 0, 1, 2, Inf)
      c_ord[[3]] <- c(-Inf, -2, -1, 0, 1, Inf)
      
      ### beta_cat
      # intercept
      beta_cat[[1]] <- switch(timepar,
                              parallel = c(-1,-1,-1,0),
                              cross = c(-1,1,0,0),
                              spline = c(-0.5,-0.5,-0.5,0),
                              mix = c(-1,1,0,0))
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = c(2,-2,0,0), 
                        cross = c(2, -2, 2, 0), 
                        spline = c(1.8, -1.4, 1, 1.4, -0.5,
                                   0.9, -0.6, -0.5, 0.2, 0.9,
                                   -0.9, -0.5, 1, 0.1, -0.2,
                                   0,0,0,0,0),
                        mix = c(2, -2, 2, 0))
      addbetacat <- switch(catpar, no = c(0,0,0,0), fixed = c(0.7,-0.3,0.4,0), group = c(0.7,-0.3,0.4,0))
      beta_cat[[1]] <- matrix(c(beta_cat[[1]], addbeta, addbetacat), nrow = Kcat)
      colnames(beta_cat[[1]]) <- c("(Intercept)", 
                                   paste0("time", 1:(length(addbeta)/Kcat)),
                                   "cat")
      
      # intercept
      beta_cat[[2]] <- switch(timepar,
                              parallel = c(0,0,0,0),
                              cross = c(1,-1,0,0),
                              spline = c(0.5,0.5,0.5,0),
                              mix = c(1,-1,0,0))
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = c(2,-2,0,0), 
                        cross = c(-2, 2, -2, 0), 
                        spline = c(-0.3, 1.5, -0.4, 1.3, 0.9, 
                                   0.3, 1.4, -1.2, -1.3, -1.3,
                                   0.3, -1.6, 0.4, -0.8, -1,
                                   0,0,0,0,0),
                        mix = c(-2, 2, -2, 0))
      addbetacat <- switch(catpar, no = c(0,0,0,0), fixed = c(0.7,-0.3,0.4,0), group = c(-0.7,0.3,-0.4,0))
      beta_cat[[2]] <- matrix(c(beta_cat[[2]], addbeta, addbetacat), nrow = Kcat)
      colnames(beta_cat[[2]]) <- c("(Intercept)", 
                                   paste0("time", 1:(length(addbeta)/Kcat)),
                                   "cat")
      
      # intercept
      beta_cat[[3]] <- switch(timepar,
                              parallel = c(1,1,1,0),
                              cross = c(-1,1,0,0),
                              spline = c(-0.5,-0.5,-0.5,0),
                              mix = c(-1,1,0,0))
      # time parametrization parameters
      addbeta <- switch(timepar, 
                        parallel = c(2,-2,0,0), 
                        cross = c(0,0,0,0), 
                        spline = c(0.1, 0.1, -0.8, 0.5, 1.1,
                                   -0.1, 0.2, -0.7, -0.5, 1.8,
                                   2, -0.9, 0, 1.2, 0.4,
                                   0,0,0,0,0),
                        mix = c(0,0,0,0))
      addbetacat <- switch(catpar, no = c(0,0,0,0), fixed = c(0.7,-0.3,0.4,0), group = c(0.3,0.4,0.7,0))
      beta_cat[[3]] <- matrix(c(beta_cat[[3]], addbeta, addbetacat), nrow = Kcat)
      colnames(beta_cat[[3]]) <- c("(Intercept)", 
                                   paste0("time", 1:(length(addbeta)/Kcat)),
                                   "cat")
    }else{
      stop("G is not 2 or 3!")
    }
  }
  
  # beta_num
  # beta_bin
  # beta_ord
  # c_ord
  # beta_cat
  
  N <- n_i * n
  x_range <- 1/xin1
  xmid <- x_range/2
  data <- data.frame()
  
  # sample by subjects - covariate + group + random effects
  for(i in 1:n){
    datai <- data.frame(i = i,
                        x = runif(n_i, min = -xmid, max = xmid) + xmid*sample(1:(2*xin1-1), size = 1))
    datai <- datai[order(datai$x),]
    datai$j <- 1:n_i
    # spline base
    splx <- bs(datai$x, knots = inner, Boundary.knots = bound, 
               degree = degree, intercept = FALSE)
    for(j in 1:spldim){
      datai[, paste0("bs",j)] <- splx[,j]
    }
    # categorical covariate
    if(catsubjfix){
      datai$f <- rep(sample(0:1, size = 1), n_i)
    }else{
      datai$f <- sample(0:1, size = n_i, replace = TRUE)
    }
    # group - uniformly
    datai$g <- rep(sample(1:G, 1, prob = rep(1/G,G)), n_i)
    # random intercepts
    if(gSigmatrue){
      bi <- rmvnorm(1, mean = rep(0,totnran), sigma = Sigma[[datai$g[1]]])
    }else{
      bi <- rmvnorm(1, mean = rep(0,totnran), sigma = Sigma)
    }
    datai$b_num <- rep(bi[1], n_i)
    datai$b_poi <- rep(bi[2], n_i)
    datai$b_bin <- rep(bi[3], n_i)
    datai$b_ord <- rep(bi[4], n_i)
    datai$b_cat <- rep(bi[5], n_i)
    
    data <- rbind(data, datai)
  }
  rownames(data) <- 1:N
  
  # calculate predictors
  for(i in 1:N){
    g <- data$g[i]
    # Numeric
    timecols <- grep("time", names(beta_num[[g]]))
    if(length(timecols)>1){
      data$pred_num[i] <- beta_num[[g]][1] + sum(beta_num[[g]][timecols] * data[i,paste0("bs",1:spldim)]) + beta_num[[g]]["cat"] * data$f[i] + data$b_num[i]
    }else{
      data$pred_num[i] <- beta_num[[g]][1] + beta_num[[g]][2] * data$x[i] + beta_num[[g]]["cat"] * data$f[i] + data$b_num[i]
    }
    # Poisson count
    timecols <- grep("time", names(beta_poi[[g]]))
    if(length(timecols)>1){
      data$pred_poi[i] <- beta_poi[[g]][1] + sum(beta_poi[[g]][timecols] * data[i,paste0("bs",1:spldim)]) + beta_poi[[g]]["cat"] * data$f[i] + data$b_poi[i]
    }else{
      data$pred_poi[i] <- beta_poi[[g]][1] + beta_poi[[g]][2] * data$x[i] + beta_poi[[g]]["cat"] * data$f[i] + data$b_poi[i]
    }
    # Binary
    timecols <- grep("time", names(beta_bin[[g]]))
    if(length(timecols)>1){
      data$pred_bin[i] <- beta_bin[[g]][1] + sum(beta_bin[[g]][timecols] * data[i,paste0("bs",1:spldim)]) + beta_bin[[g]]["cat"] * data$f[i] + data$b_bin[i]
    }else{
      data$pred_bin[i] <- beta_bin[[g]][1] + beta_bin[[g]][2] * data$x[i] + beta_bin[[g]]["cat"] * data$f[i] + data$b_bin[i]
    }
    # Ord
    timecols <- grep("time", names(beta_ord[[g]]))
    if(length(timecols)>1){
      data$pred_ord[i] <- beta_ord[[g]][1] + sum(beta_ord[[g]][timecols] * data[i,paste0("bs",1:spldim)]) + beta_ord[[g]]["cat"] * data$f[i] + data$b_ord[i]
    }else{
      data$pred_ord[i] <- beta_ord[[g]][1] + beta_ord[[g]][2] * data$x[i] + beta_ord[[g]]["cat"] * data$f[i] + data$b_ord[i]
    }
    
    data$c_ord1[i] <- c_ord[[g]][2]
    data$c_ord2[i] <- c_ord[[g]][3]
    data$c_ord3[i] <- c_ord[[g]][4]
    data$c_ord4[i] <- c_ord[[g]][5]
    
    # Cat
    timecols <- grep("time", colnames(beta_cat[[g]]))
    if(length(timecols)>1){
      data$pred_cat1[i] <- 0
      data$pred_cat2[i] <- beta_cat[[g]][1,1] + sum(beta_cat[[g]][1,timecols] * data[i,paste0("bs",1:spldim)]) + beta_cat[[g]][1,"cat"] * data$f[i] + data$b_cat[i]
      data$pred_cat3[i] <- beta_cat[[g]][2,1] + sum(beta_cat[[g]][2,timecols] * data[i,paste0("bs",1:spldim)]) + beta_cat[[g]][2,"cat"] * data$f[i] + data$b_cat[i]
      data$pred_cat4[i] <- beta_cat[[g]][3,1] + sum(beta_cat[[g]][3,timecols] * data[i,paste0("bs",1:spldim)]) + beta_cat[[g]][3,"cat"] * data$f[i] + data$b_cat[i]
      
    }else{
      data$pred_cat1[i] <- 0
      data$pred_cat2[i] <- beta_cat[[g]][1,1] + beta_cat[[g]][1,2] * data$x[i] + beta_cat[[g]][1,"cat"] * data$f[i] + data$b_cat[i]
      data$pred_cat3[i] <- beta_cat[[g]][2,1] + beta_cat[[g]][2,2] * data$x[i] + beta_cat[[g]][2,"cat"] * data$f[i] + data$b_cat[i]
      data$pred_cat4[i] <- beta_cat[[g]][3,1] + beta_cat[[g]][3,2] * data$x[i] + beta_cat[[g]][3,"cat"] * data$f[i] + data$b_cat[i]
      
    }

  }
  
  data$f <- factor(data$f)
  
  ## sample outcomes
  # Numeric
  data$ynum <- rnorm(N, mean = data$pred_num, sd = sd_num[[G]][data$g])
  
  # Poisson count
  data$ypoi <- rpois(N, lambda = exp(data$pred_poi))
  
  # Binary
  data$ybin <- rbinom(N, size = 1,
                      prob = plogis(data$pred_bin))
  # Ordinal
  probs <- matrix(0, nrow = N, ncol = Kord)
  for(i in 1:N){
    g <- data$g[i]
    # response sampled by probabilities
    loginvs <- plogis(data$pred_ord[i]-c_ord[[g]])
    probs[i,] <- loginvs[1:Kord] - loginvs[2:(Kord+1)]
    data$yord[i] <- sample(0:(Kord-1), 1, prob = probs[i,])
  }
  data$yord <- factor(data$yord, levels = 0:(Kord-1), ordered = TRUE)
  
  # Categorical
  probs <- matrix(0, nrow = N, ncol = Kcat)
  for(i in 1:N){
    probs[i,] <- softmax(data[i,paste0("pred_cat", 1:Kcat)])
    data$ycat[i] <- sample(0:(Kcat-1), 1, prob = probs[i,])
  }
  data$ycat <- factor(data$ycat, levels = 0:(Kcat-1))
  
  return(list(data = data, 
              sd_num = sd_num, 
              beta_num = beta_num, 
              beta_poi = beta_poi, 
              beta_bin = beta_bin, 
              beta_ord = beta_ord, 
              c_ord = c_ord, 
              beta_cat = beta_cat))
  

}
