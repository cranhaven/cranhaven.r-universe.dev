#----------------------------------------------------------------
#' Preprocessing to link strategies and designs.
#' @title Strategy generation
#' @param n.s scalar or vector. If scalar, total number of strategies (to be divided equally among players),
#'  otherwise number of strategies per player.
#' @param d number of variables
#' @param nobj number of objectives (or players)
#' @param x.to.obj vector allocating variables to objectives. If not provided, default is \code{1:nobj}, assuming that \code{d=nobj}
#' @param gridtype either "\code{cartesian}" or "\code{lhs}", or a vector to define a different type for each player.
#' @param equilibrium either "\code{NE}", "\code{KSE}", "\code{CKSE}" or "\code{NKSE}"
#' @param lb,ub vectors specifying the bounds of the design space, by default \code{[0,1]^d}
#' @param include.obs Boolean, if TRUE observations given in \code{model@X} are added to the integration points (only for \code{KSE} and \code{CKSE})
#' @param model optional list of \code{km} models (used if \code{include.obs=TRUE})
#' @param seed random seed used by \code{\link[DiceDesign]{lhsDesign}}
#' @param init_set large grid to subsample from
#' @param include_set grid to be included in the larger one generated
#' @return A list containing two matrices, \code{integ.pts} the design of experiments and \code{expanded.indices}
#' the corresponding indices (for \code{NE}), and the vector \code{n.s}
#' @export
#' @examples
#' \donttest{
#' ##############################################
#' ### 4 variables, 2 players, no filter
#' ##############################################
#'
#' # Create a 11x8 lattice based on 2 LHS designs
#' n.s <- c(11,8)
#' gridtype = "lhs"
#' # 4D space is split in 2
#' x.to.obj <- c(1,1,2,2)
#' integcontrol <- generate_integ_pts(n.s=n.s, d=4, nobj=2, x.to.obj = x.to.obj, gridtype=gridtype)
#' pairs(integcontrol$integ.pts)
#' 
#' # Create a simple 11x11 grid
#' integcontrol <- generate_integ_pts(n.s=11^2, d=2, nobj=2, gridtype="cartesian")
#' pairs(integcontrol$integ.pts)
#' }

generate_integ_pts <- function(n.s, d, nobj, x.to.obj=NULL, gridtype="cartesian", equilibrium="NE", lb=rep(0,d), ub=rep(1,d), 
                               include.obs=FALSE, model=NULL, init_set=NULL, include_set=NULL, seed=42) {

  # Set x.to.obj to 1:nobj (one variable for each player) if missing
  # Necessary for NE or KSE with a cartesian grid
  if (is.null(x.to.obj) && (gridtype=="cartesian" || !(equilibrium %in% c("KSE", "CKSE")))) {
    if (d!=nobj) {
      stop("x.to.obj must be given if d!=nobj")
    }
    x.to.obj <- 1:nobj
  }
  
  if (is.null(model) && include.obs) {
    warning("a list of models must be given if include.obs=TRUE \n")
    include.obs <- FALSE
  }

  # Set n.s as vector unless LHS design for KSE is sought
  if (!(equilibrium %in% c("KSE", "CKSE") && gridtype %in% c("lhs", "subset"))) {
    if (length(n.s)==1) n.s <- rep(round((n.s)^(1/nobj)), nobj)
  }

  if (equilibrium %in% c("KSE", "CKSE") && length(gridtype)==1 && gridtype[1] %in% c("lhs", "subset")) {
    
    if (include.obs) n.lhs <- max(0, n.s - model[[1]]@n) else n.lhs <- n.s
    if (!is.null(include_set)) n.lhs <- max(0, n.lhs - nrow(include_set))
        
    if (gridtype[1] == "subset" && !is.null(init_set)) {
      I <- sample.int(nrow(init_set), n.lhs, replace = FALSE)
      test.grid <- init_set[I,,drop=FALSE] 
    } else {
      # KSE & LHS: standard LHS is generated
      test.grid <- lhsDesign(n=n.lhs, dimension=d, seed = seed)$design
      
      # Scaling
      test.grid <- test.grid*matrix((ub - lb), nrow=nrow(test.grid), ncol=ncol(test.grid), byrow=TRUE) +
        matrix(lb, nrow=nrow(test.grid), ncol=ncol(test.grid), byrow=TRUE)
    }

    if (include.obs) test.grid <- rbind(model[[1]]@X, test.grid)
    if (!is.null(include_set)) test.grid <- rbind(include_set, test.grid)
    
    test.grid <- unique(test.grid)
    n.s <- nrow(test.grid)
    
    expanded.indices <- matrix(rep(seq(1, n.s), d), ncol=d)
  } else {
    # Otherwise: grid structure
    if (length(gridtype)==1) gridtype <- rep(gridtype, nobj)
    subspace.doe <- vector("list", nobj)
    indices <-  vector("list", nobj)

    for (i in 1:nobj) {
      if (gridtype[i]=="cartesian") {
        n.grid   <- round((n.s[i])^(1/length(which(x.to.obj==i))))
        vec.grid <- rep(list(seq(0, 1, length.out = n.grid)), length(which(x.to.obj==i)))
        subspace.doe[[i]] <- as.matrix(expand.grid(vec.grid))
      } else if (gridtype[i]=="lhs") {
        subspace.doe[[i]] <- lhsDesign(n=n.s[i], dimension=length(which(x.to.obj==i)), seed = seed)$design
      }
      indices[[i]] <- seq(1, nrow(subspace.doe[[i]]))
    }
    expanded.indices <- expand.grid(indices)

    test.grid <- c()
    for (i in 1:nobj) {
      test.grid <- cbind(test.grid, subspace.doe[[i]][expanded.indices[,i],])
    }
    
    # Scaling
    test.grid <- test.grid*matrix((ub - lb), nrow=nrow(test.grid), ncol=ncol(test.grid), byrow=TRUE) +
      matrix(lb, nrow=nrow(test.grid), ncol=ncol(test.grid), byrow=TRUE)
  }

  return(list(integ.pts=test.grid, expanded.indices=as.matrix(expanded.indices), n.s=n.s,
              x.to.obj=NULL, gridtype=gridtype, equilibrium=equilibrium, lb=lb, ub=ub, 
              include.obs=include.obs, model=model, init_set=init_set))
}
