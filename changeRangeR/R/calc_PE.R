# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# calc_PE.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the changeRangeR R package.
#
# changeRangeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# changeRangeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with changeRangeR. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
#' @name calc_PE
#' @title Calculate phylogenetic endemism
#' @description Calculates phylogenetic endemism from a tree and a sites by tips (species) matrix.
#' @param phylo.tree class phylo object of phylogenetic tree. The names on tree's tip labels need to match the column names on the matrix
#' @param sites_x_tips class data.frame object. Rows should be locations and columns should be species. The sites should represent equal areas (presumably grid cells). There is no need to include unoccupied grid cells.  One easy way to get this, is to simply round the coordinates to the appropriate grid resolution, and then group occurrences at the same rounded location together using aggregate or dplyr
#' @param presence character string of either: "presence", "abundance", or "probability".
#' presence specifies what the values in the matrix cells mean, and how to calculate PE
#' abundance is an amount (could be number of individuals, proportion of cell occupied etc).  With abundance, PE is equivalent to Caddotte & Davies BED)
#' probability is a value from 0 to 1, for example from an SDM.  Probability then propagates to internal branches at the probability that any of the descendent branches are present.  This method is described in a paper of mine in, which is being pending minor revisions.
#' @import phylobase
#' @return dataframe showing the PE at each site
#' @examples
#'## Convert raster stack to points
#' r1 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r1)<- runif(n = (108*108))
#' r1[r1 < 0.5] <- NA
#' r1[r1 > 0.5] <- 1
#' r2 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r2)<- runif(n = (108*108))
#' r2[r2 < 0.5] <- NA
#' r2[r2 > 0.5] <- 1
#' r3 <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
#' raster::values(r3)<- runif(n = (108*108))
#' r3[r3 < 0.5] <- NA
#' r3[r3 > 0.5] <- 1
#' rStack <- raster::stack(r1, r2, r3)
#' Allxy <- raster::rasterToPoints(rStack)
#' # Drop first 2 columns (lat/long)
#' sites <- Allxy[,2:ncol(Allxy)]
#' sites[is.na(sites)] <- 0
#' library(ape)
#' tree <- rtree(n = 3)
#' tree$tip.label <- names(rStack)
#' calc_PE(phylo.tree = tree, sites_x_tips = sites, presence = "presence")
#' @export

##################################################
##  A script to calculate phylogenetic endemism ##
##  Dan Rosauer                                 ##
##  dan.rosauer@anu.edu.au                      ##
##  revised March 2015                          ##
##################################################

## the calc_PE() function in this script calculates phylogenetic endemism from a
## tree and a sites by tips (species) matrix.

##   tree                 - the names on tree's tip labels need to match the column names on the matrix
##   sites x tips matrix  - the sites should represent equal areas (presumably grid cells).   There is no need to include unoccupied grid cells.  One easy way to get this, is to simply round the coordinates to the appropriate grid resolution, and then group occurrences at the same rounded location together using aggregate or dplyr
##   presence             - specifies what the values in the matrix cells mean, and how to calculate PE
##                             presence uses 0 or 1 for presence / absence.  This is PE exactly as in Rosauer et al 2009
##                             abundance is an amount (could be number of individuals, proportion of cell occupied etc).  With abundance, PE is equivalent to Caddotte & Davies BED)
##                             probability is a value from 0 to 1, for example from an SDM.  Probability then propagates to internal branches at the probability that any of the descendent branches are present.  This method is described in a paper of mine in, which is being pending minor revisions.

calc_PE <- function(phylo.tree, sites_x_tips, presence=NULL){
  # c("presence","abundance","probability")

  #require(phylobase)

  parent.prob <- function(probabilities) {
    # probabilities is a vector of values between 0 and 1
    # add code to check values are of correct type!
    parent.prob <- 1 - prod(1-probabilities)
    return(parent.prob)
  }

  scale.to <- function(vec,vec.sum) {
    #mat is a vector
    #this function rescales each the vector values to sum to 'vec.sum'
    vec.tot <- sum(vec,na.rm=TRUE)
    if (vec.tot > 0) {
      vec.out <- vec.sum*vec/vec.tot
    } else {
      vec.out <- rep(0,times=length(vec))  #should columns for tips / branches with no occurrences be removed?
    }
    return(vec.out)
  }

  # add code to check that the values are correct for the presence type:
  # 0 or 1 for presence - this calculates PE (Rosauer et al 2009)
  # from 0 to 1 for probability - this calculates model weighted PE (Rosauer, in prep)
  # any value for abundance - this calculation is equivalent to BED (Cadotte & Davies 2010)

  #default value for presence
  if (is.na(presence)) {presence="presence"}

  # change to a phylobase phylo4 object
  #if (class(tree) == "phylo") {tree <- phylo4(tree)}
  tree <- phylobase::phylo4(phylo.tree)

  sites_x_branches <- data.frame(cbind(rep(0,nrow(sites_x_tips))))

  for (i in 1:phylobase::nTips(tree)) {
    sites_x_branches[,i] <- sites_x_tips[,which(phylobase::labels(tree)[i]==names(sites_x_tips))]
    #names(sites_x_branches)[i] <- phylobase::labels(tree)[i]

    #names(sites_x_branches)[i] <- tree@label[i]
  }
  colnames(sites_x_branches) <- as.character(tree@label)

  rm(sites_x_tips); gc()
  branch.labels <- as.character(labels(tree))
  branch.count <- length(labels(tree))

  # add names and occupancy columns for internal branches
  for (i in (phylobase::nTips(tree)+1):branch.count) {
    branch.labels[i] <- paste("b",i,sep="")
    desc <- as.integer(phylobase::descendants(tree,i, type="tips"))
    if (presence=="abundance") {
      branch_col <- as.numeric(apply(sites_x_branches[,desc],MARGIN=1,FUN=sum))
    } else if (presence=="presence") {
      branch_col <- as.numeric(apply(sites_x_branches[,desc],MARGIN=1,FUN=max))
    } else if (presence=="probability") {
      branch_col <- as.numeric(apply(sites_x_branches[,desc],MARGIN=1,FUN=parent.prob))
    }
    sites_x_branches[,i] <- branch_col
    names(sites_x_branches[i]) <- branch.labels[i]
    #cat(i,branch.labels[i],length(desc),"\n")
    gc(verbose=F)
  }

  #scale columns (branches) to sum to 1
  sites_x_branches <- apply(sites_x_branches,MARGIN=2,FUN=scale.to,1)

  #now scale branches to sum to their length
  branch.lengths <- as.numeric(phylobase::edgeLength(tree,1:branch.count))
  branch.lengths[is.na(branch.lengths)] <- 0
  for (i in 1:length(branch.lengths)) {
    sites_x_branches[,i] <- sites_x_branches[,i] * branch.lengths[i]
  }

  PE.vec <- apply(sites_x_branches,MARGIN=1,FUN=sum,na.rm=T)
  PE <- data.frame(cbind(1:nrow(sites_x_branches),PE.vec))
  names(PE) <- c("site","PE")
  return(PE)
}
