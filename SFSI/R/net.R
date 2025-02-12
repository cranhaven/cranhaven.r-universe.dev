#====================================================================
# Plot the top 2 PCs of the K matrix showing tst and trn points
#====================================================================

net <- function(object, K = NULL,
                nsup = NULL, p.radius = 1.7,
                delta = .Machine$double.eps)
{
  # object=fm5; K=G0; i=i0; nsup=nsup0

  if(!is.null(K)){
    if(length(dim(K)) != 2L | (length(K) != nrow(K)^2)) {
      stop("Input 'K' must be a squared matrix")
    }
    if(has_names(K)){
      stopifnot(all(rownames(K)==colnames(K)))
      namesK <- rownames(K)
    }
  }

  isSGP <- FALSE
  if(inherits(object, "SGP")){
    X <- NULL
    isSGP <- TRUE
  }else{
    if(length(dim(object)) == 2L){
      X <- object
      rm(object)
    }else{
      stop("The input object is not of the class 'SGP' or a matrix")
    }
  }

  if(isSGP){
    n <- object$n
    q <- object$ntraits
    if(is.null(nsup)) nsup <- summary.SGP(object)$optCOR['nsup']
    if(0 > nsup | nsup > range(object$nsup)[2]){
      stop("Parameter 'nsup' must be greater than zero and no greater than 'trn' size")
    }
    X <- as.matrix(coef.SGP(object, nsup=nsup))
    symmetric <- FALSE
    xxx <- object$trn
    yyy <- object$tst
    MAP <- map_set(i=object$ID_geno, j=object$ID_trait, n=n, x=xxx, y=yyy, labels=object$labels)

    if(!is.null(K)){
      if(has_names(K) & !is.null(object$labels)){
        flag1 <- any(MAP$label[xxx] != namesK[MAP$i[xxx]])
        flag2 <- any(MAP$label[yyy] != namesK[MAP$i[yyy]])
        if(flag1 | flag2){ # Only if names does not match
          if(any(!c(MAP$label[xxx],MAP$label[yyy]) %in% namesK)){
            stop("Some row/column names of 'object' were not found in names of 'K'")
          }
          n <- nrow(K)
          tmp <- apply(expand.grid(namesK, seq(q)),1,paste0,collapse="_")
          yyy <- match(MAP$label_j[yyy], tmp)
          xxx <- match(MAP$label_j[xxx], tmp)
          MAP <- map_set(i=object$ID_geno, j=object$ID_trait, n=n, x=xxx, y=yyy, labels=namesK)
        }
      }else{
        if(nrow(K) != n){
          stop("Input 'object' couldn't be matched to 'K' through row/column names")
        }
      }
    }

  }else{
    symmetric <- isSymmetric(X, tol=1E-6)
    q <- 1
    if(symmetric){
      if(has_names(X)){
        stopifnot(rownames(X) == colnames(X))
      }
      if(is.null(K)){
        n <- ncol(X)
        xxx <- yyy <- 1:n
        if(has_names(X)){
          labels0 <- rownames(X)
        }else{
          labels0 <- seq(n)
        }
      }else{
        if(has_names(K) & has_names(X)){
          if(any(!c(rownames(X),colnames(X)) %in% namesK)){
            stop("Some row/column names of 'object' were not found in names of 'K'")
          }
          n <- nrow(K)
          xxx <- yyy <- match(colnames(X),namesK)
          labels0 <- namesK
        }else{
          stop("Input 'object' couldn't be matched to 'K' through row/column names")
        }
      }
    }else{
      if(is.null(K)){
        n <- nrow(X)+ncol(X)
        yyy <- seq(nrow(X))
        xxx <- nrow(X) + seq(ncol(X))
        if(has_names(X)){
          labels0 <- c(rownames(X), colnames(X))
        }else{
          labels0 <- rep("",n)
          labels0[yyy] <- paste0("R",seq_along(yyy))
          labels0[xxx] <- paste0("C",seq_along(xxx))
        }

      }else{
        if(has_names(K) & has_names(X)){
          if(any(!c(rownames(X),colnames(X)) %in% namesK)){
            stop("Some row/column names of 'object' were not found in names of 'K'")
          }
          n <- nrow(K)
          yyy <- match(rownames(X),namesK)
          xxx <- match(colnames(X),namesK)
          labels0 <- namesK
        }else{
          stop("Input 'object' couldn't be matched to 'K' through row/column names")
        }
      }
    }
    MAP <- map_set(n=n, x=xxx, y=yyy, labels=labels0)
  }

  out <- get_net(X=X, MAP=MAP, symmetric=symmetric,
                 K=K, p.radius=p.radius, delta=delta)
  stopifnot(nrow(out) == nrow(MAP))

  out$label <- MAP$label
  out$isEigen <- !is.null(K)
  out$isSGP <- isSGP
  out$symmetric <- symmetric

  class(out) <- c("net")

  return(out)
}
