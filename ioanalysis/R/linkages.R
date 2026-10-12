linkages <- function(io, ES = NULL, regions = "all", sectors = "all", type = c("total"), normalize = FALSE, intra.inter = FALSE){
  # Note the easy select functionality has been added ex post, so it is not as neat.
  # pre checks
  if(!"direct" %in% type & !"total" %in% type) stop("type must be a combination of 'direct' or 'total'")
  if(!"all" %in% regions & class(regions) == "character"){
    for(k in 1:length(regions)){
      if(!regions[k] %in% io$RS_label[, 1]) stop(paste(regions[k], "is not a region in RS_label"))
    }
  }
  if(!"all" %in% sectors & class(sectors) == "character"){
    for(k in 1:length(sectors)){
      if(!sectors[k] %in% io$RS_label[, 2]) stop(paste(sectors[k], "is not a sector in RS_label"))
    }
  }
  # Grabbing the objects we will need
  if("InputOutput" %in% class(io)){
    # Grabbing and calculating the matrices we need
    n <- dim(io$Z)[1]
    A <- io$A                                # Input technical coefficients
    B <- io$B                                # Output Technical coefficients
    X <- io$X                                # Total production
    L <- io$L                                # Leontief Inverse
    G <- io$G            # Ghoshian Inverse
    if("all" %in% regions){
      region <- unique(io$RS_label[, 1]) # Region
    } else if(class(regions) == "character"){
      region <- regions
    } else if(class(regions) == "numeric" | class(regions) == "integer"){
      region <- unique(io$RS_label[, 1])[regions]
    }
    if(!is.null(ES)){
      region <- unique(ES[, 2])
    }
    RS_label <- io$RS_label                  # Region Sector Label
  } else if(!"InputOutput" %in% class(io)) stop('io must be an "InputOuput" object. See ?as.inputoutput')


  # Creating an object to store important things
  R <- length(region)
  linkage <- vector("list", R)
  names(linkage) <- region
  link <- NULL
  ##########################
  ##########################
  ## Single Region System ##
  ##########################
  ##########################
  if(length(unique(region)) == 1 & setequal(region, unique(io$RS_label[, 1]))){
    n <- dim(A)[1]
    one <- rep(1, n)
    if("all" %in% sectors){
      sector <- RS_label[, 2]
    }
    ############
    ## Direct ##
    ############
    if("direct" %in% type){
      if(normalize == TRUE){
        BL.dir <- t(  n * (t(one) %*% A) / as.numeric(t(one) %*% A %*% one)  )
        FL.dir <-     n * (   B %*% one) / as.numeric(t(one) %*% B %*% one)
      } else if(normalize == FALSE){
        BL.dir <- t(  (t(one) %*% L)  )
        FL.dir <-     (   G %*% one)
      }
      link.dir <- cbind(BL.dir, FL.dir)
      colnames(link.dir) <- c("BL.dir", "FL.dir")
      rownames(link.dir) <- sector
      link <- cbind(link, link.dir)
    }
    ###########
    ## Total ##
    ###########
    if("total" %in% type){
      if(normalize == TRUE){
        BL.tot <- t(  n * (t(one) %*% L) / as.numeric(t(one) %*% L %*% one)  )
        FL.tot <-     n * (   G %*% one) / as.numeric(t(one) %*% G %*% one)
      } else if(normalize == FALSE){
        BL.tot <- t(  (t(one) %*% L)  )
        FL.tot <-     (   G %*% one)
      }
      link.tot <- cbind(BL.tot, FL.tot)
      colnames(link.tot) <- c("BL.tot", "FL.tot")
      rownames(link.tot) <- sector
      link <- cbind(link, link.tot)
    }
    linkage[[1]] <- link
  }
  ###########################
  ###########################
  ## Multi Region Analysis ##
  ###########################
  ###########################
  else {
    ####################
    ## Just Aggregate ##
    ####################
    if(intra.inter == FALSE){
      n <- dim(A)[1]
      one <- matrix(rep(1,n))
      # Deliberately checking types each loop
      # This allows for more logical construction of the linkage matrix
      for(k in 1:length(region)){
        r <- which(RS_label[, 1] == region[k])
        one.r <- matrix(rep(1, length(r)))
        ## ------
        # Checking sectors each time. This allows for multi-regional systems with different sectors
        ## ----
        if("all" %in% sectors){
          sector <- RS_label[r, 2]
          z <- 1:length(sector)
        } else if(!"all" %in% sectors & class(sectors) == "character"){
          z <- which(RS_label[r, 2] %in% sectors)
          sector <- RS_label[r, 2][z]
        } else if(class(sectors) == "numeric" | class(sectors) == "integer"){
          z <- which(1:length(RS_label[r, 2]) %in% sectors)
          sector <- RS_label[r, 2][z]
        } else if(!is.null(ES)){
          z <- which(ES[, 2] %in% region[k])
          sector <- ES[z, 3]
          z <- which(RS_label[, 1] %in% region[k] & RS_label[, 2] %in% sector)
        }
        else {
          stop("Unable to assign sectors. Requrie either 'Easy.Select' object, sectors='all', class(sectors)='character', class(sectors)='numeric', or class(sectors)='integer'")
        }
        ############
        ## Direct ##
        ############
        if("direct" %in% type){
          A.r <- A[, r]
          Br. <- B[r, ]
          if(normalize == TRUE){
            BL.dir <- t(  n * (t(one) %*% A.r) / as.numeric(t(one) %*% A.r %*% one.r)  )
            FL.dir <-     n * (   Br. %*% one) / as.numeric(t(one.r) %*% Br. %*% one)
          } else if(normalize == FALSE){
            BL.dir <- t(  (t(one) %*% A.r)   )
            FL.dir <-     (   Br. %*% one)
          }
          link.dir <- cbind(BL.dir[z], FL.dir[z])
          colnames(link.dir) <- c(  paste( "BL.dir", sep = "."),  paste( "FL.dir", sep = "."))
          rownames(link.dir) <- sector
          link <- cbind(link, link.dir)
        }
        ###########
        ## Total ##
        ###########
        if("total" %in% type){
          L.r <- L[, r]
          Gr. <- G[r, ]
          if(normalize == TRUE){
            BL.tot <- t(  n * (t(one) %*% L.r) / as.numeric(t(one) %*% L.r %*% one.r)  )
            FL.tot <-     n * (   Gr. %*% one) / as.numeric(t(one.r) %*% Gr. %*% one)
          } else if(normalize == FALSE){
            BL.tot <- t(  (t(one) %*% L.r)   )
            FL.tot <-     (   Gr. %*% one)
          }
          link.tot <- cbind(BL.tot[z], FL.tot[z])
          colnames(link.tot) <- c(  paste( "BL.tot", sep = "."),  paste( "FL.tot", sep = "."))
          rownames(link.tot) <- sector
          link <- cbind(link, link.tot)
        }
        linkage[[k]] <- link
        link <- NULL
      }
    }
    ##############################################
    ## Intraregional and Interregional Linkages ##
    ##############################################
    if(intra.inter == TRUE){
      n <- dim(A)[1]
      i <- 1:n
      one <- matrix(rep(1,n))
      for(k in 1:length(region)){
        r <- which(RS_label[, 1] == region[k])
        one.r <- matrix(rep(1, length(r)))
        s <- setdiff(i, r)
        one.s <- matrix(rep(1, length(s)))
        ## ------
        # Checking sectors each time. This allows for multi-regional systems with different sectors
        ## ----
        if("all" %in% sectors){
          sector <- RS_label[r, 2]
          z <- 1:length(sector)
        } else if(!"all" %in% sectors & class(sectors) == "character"){
          z <- which(RS_label[r, 2] %in% sectors)
          sector <- RS_label[r, 2][z]
        } else if(class(sectors) == "numeric" | class(sectors) == "integer"){
          z <- which(1:length(RS_label[r, 2]) %in% sectors)
          sector <- RS_label[r, 2][z]
        } else if(!is.null(ES)){
          z <- which(ES[, 2] %in% region[k])
          sector <- ES[z, 3]
          z <- which(RS_label[, 1] %in% region[k] & RS_label[, 2] %in% sector)
        }
        else {
          stop("Unable to assign sectors. Requrie either 'Easy.Select' object, sectors='all', class(sectors)='character', class(sectors)='numeric', or class(sectors)='integer'")
        }
        ############
        ## Direct ##
        ############
        if("direct" %in% type){
          Arr <- A[r, r]
          Asr <- A[s, r]
          Brr <- B[r, r]
          Brs <- B[r, s]
          A.r <- A[, r]
          Br. <- B[r, ]
          # Intraregional
          if(normalize == TRUE){
            BL.intra.dir <- t(  n * (t(one.r) %*% Arr) / as.numeric(t(one.r) %*% Arr %*% one.r)  )
            FL.intra.dir <-     n * (   Brr %*% one.r) / as.numeric(t(one.r) %*% Brr %*% one.r)
          }
          else if(normalize == FALSE){
            BL.intra.dir <- t(  (t(one.r) %*% Arr)   )
            FL.intra.dir <-     (   Brr %*% one.r)
          }
          link.dir <- cbind(BL.intra.dir[z], FL.intra.dir[z])
          colnames(link.dir) <- c(  paste( "BL.intra.dir", sep = "."),  paste( "FL.intra.dir", sep = "."))
          rownames(link.dir) <- sector
          link <- cbind(link, link.dir)
          # Interregional
          if(normalize == TRUE){
            BL.inter.dir <- t(  n * (t(one.s) %*% Asr) / as.numeric(t(one.s) %*% Asr %*% one.r)  )
            FL.inter.dir <-     n * (   Brs %*% one.s) / as.numeric(t(one.r) %*% Brs %*% one.s)
          }
          if(normalize == FALSE){
            BL.inter.dir <- t(  (t(one.s) %*% Asr)    )
            FL.inter.dir <-     (   Brs %*% one.s)
          }
          link.dir <- cbind(BL.inter.dir[z], FL.inter.dir[z])
          colnames(link.dir) <- c(  paste( "BL.inter.dir", sep = "."),  paste( "FL.inter.dir", sep = "."))
          rownames(link.dir) <- sector
          link <- cbind(link, link.dir)
          # Aggregate
          if(normalize == TRUE){
            BL.agg.dir <- t(  n * (t(one) %*% A.r) / as.numeric(t(one) %*% A.r %*% one.r)  )
            FL.agg.dir <-     n * (   Br. %*% one) / as.numeric(t(one.r) %*% Br. %*% one)
          } else if(normalize == FALSE){
            BL.agg.dir <- t(  (t(one) %*% A.r)   )
            FL.agg.dir <-     (   Br. %*% one)
          }
          link.dir <- cbind(BL.agg.dir[z], FL.agg.dir[z])
          colnames(link.dir) <- c(  paste( "BL.agg.dir", sep = "."),  paste( "FL.agg.dir", sep = "."))
          rownames(link.dir) <- sector
          link <- cbind(link, link.dir)
        }
        ###########
        ## Total ##
        ###########
        if("total" %in% type){
          Lrr <- L[r, r]
          Lsr <- L[s, r]
          Grr <- G[r, r]
          Grs <- G[r, s]
          L.r <- L[, r]
          Gr. <- G[r, ]
          # Intraregional
          if(normalize == TRUE){
            BL.intra.tot <- t(  n * (t(one.r) %*% Lrr) / as.numeric(t(one.r) %*% Lrr %*% one.r)  )
            FL.intra.tot <-     n * (   Grr %*% one.r) / as.numeric(t(one.r) %*% Grr %*% one.r)
          }
          else if(normalize == FALSE){
            BL.intra.tot <- t(  (t(one.r) %*% Lrr)   )
            FL.intra.tot <-     (   Grr %*% one.r)
          }
          link.tot <- cbind(BL.intra.tot[z], FL.intra.tot[z])
          colnames(link.tot) <- c(  paste( "BL.intra.tot", sep = "."),  paste( "FL.intra.tot", sep = "."))
          rownames(link.tot) <- sector
          link <- cbind(link, link.tot)
          # Interregional
          if(normalize == TRUE){
            BL.inter.tot <- t(  n * (t(one.s) %*% Lsr) / as.numeric(t(one.s) %*% Lsr %*% one.r)  )
            FL.inter.tot <-     n * (   Grs %*% one.s) / as.numeric(t(one.r) %*% Grs %*% one.s)
          }
          if(normalize == FALSE){
            BL.inter.tot <- t(  (t(one.s) %*% Lsr)    )
            FL.inter.tot <-     (   Grs %*% one.s)
          }
          link.tot <- cbind(BL.inter.tot[z], FL.inter.tot[z])
          colnames(link.tot) <- c(  paste( "BL.inter.tot", sep = "."),  paste( "FL.inter.tot", sep = "."))
          rownames(link.tot) <- sector
          link <- cbind(link, link.tot)
          # Aggregate
          if(normalize == TRUE){
            BL.agg.tot <- t(  n * (t(one) %*% L.r) / as.numeric(t(one) %*% L.r %*% one.r)  )
            FL.agg.tot <-     n * (   Gr. %*% one) / as.numeric(t(one.r) %*% Gr. %*% one)
          } else if(normalize == FALSE){
            BL.agg.tot <- t(  (t(one) %*% L.r)   )
            FL.agg.tot <-     (   Gr. %*% one)
          }
          link.tot <- cbind(BL.agg.tot[z], FL.agg.tot[z])
          colnames(link.tot) <- c(  paste( "BL.agg.tot", sep = "."),  paste( "FL.agg.tot", sep = "."))
          rownames(link.tot) <- sector
          link <- cbind(link, link.tot)
        }
        linkage[[k]] <- link
        link <- NULL
      }
    }
  }
#  if(length(linkage) == 1){
#    linkage <- linkage[[1]]
#  }
  linkage
}

