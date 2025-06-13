#### Date: 02/24/2020
#### Modified : 04/15/2021
#### Author: Seungyong Hwang
## Tracking and Tractography algorithm from Raymond et al. (2016)

#~~Tracking Function
## v.track: depend on the following internal functions
### get.fdis
### fiber.track
#### dist.line
#### fiber.in.out
#### get.out.vox
#### project.proceed
##### fiber.in.out
##### proceed

#~~Tractography Function
## tractography: depend on the following internal functions
## plot.fib
### get.fib.col

# vorient: if the voxel orientation does not match the physical orientation, make it -1

#' Deterministic tracking algorithm -- DiST
#'
#' v.track is used to apply the deterministic tracking algorithm -- DiST (Wong et al 2017)
#' It can be used to carry out the neuronal fiber reconstruction based on the peak detection results with local fiber estimation.
#' Peak detection algorithm can be found in example_HCP_analysis.py from \href{https://github.com/vic-dragon/BJS/tree/master/example_scripts/}{github-repository}
#'
#' @param v.obj An list type object which contains the following components:
#' \itemize{
#'  \item{vec:} A matrix containing the estimated peak directions.
#'  \item{loc:} A matrix containing the 'braingrid' coordinates of the corresponding estimated peak direction.
#'  \item{map:} A vector containing the voxel indicator of corresponding estimated peak direction.
#'  \item{rmap:} A vector specifying the location in 'map' of each voxel.
#'  \item{n.fiber:} A vector specifying the number of peaks at each voxel.
#'  \item{n.fiber2:} A vector specifying the number of peaks corresponding to 'map'.
#'  \item{braingrid:} A array specifying the normalized voxel coordinates.
#'  \item{xgrid.sp,ygrid.sp,zgrid.sp: } A numeric value specifying the voxel size (mm) in x, y, z-axis, respectively. (e.g. Voxel size in HCP dMRI: 1.25mm * 1.25mm * 1.25mm)
#'  \item Example can be found in main page of \href{https://github.com/vic-dragon/dmri.tracking}{github-repository}
#' }
#' @param max.line A integer value specifying the maximum number of voxels that the reconstructed fibers can go through. The value can depend on the size of ROI.
#' @param nproj A integer value specifying the number of neighborhood voxels if the algorithm cannot find any viable direction nearby.
#' @param elim logical. If TRUE, 'sorted.update.ind' returns whether the reconstructed fiber is greater than 'elim.thres'
#' @param elim.thres  A numeric value specifying the lower limit length of reconstructed fibers.
#' @param thres.ang A numeric value specifying the threshold to determine whether the destination voxel have a viable direction (default value: pi/6). i.e.,
#' the algorithm will be proceeded, if the angular difference of the diffusion direction between the previous voxel and the destination voxel is smaller than 'thres.ang'.
#'
#' @return
#'
#'  Result of deterministic tracking algorithm
#' \itemize{
#'  \item{v.obj:} Input of \code{\link{v.track}}
#'  \item{track1, track2:} A list containing the reconstructed fiber information
#'  \itemize{
#'   \item{inloc:} The voxel coordinates that the reconstructed fiber went through
#'   \item{dir:} The diffusion direction that used to reconstruct fiber
#'   \item{iinds:} The indicator of voxel that the reconstructed fiber went through
#'   \item{change:} logical, If TRUE, that voxel pass the angular difference threshold (thres.ang)
#'   \item{pvox:} The passed voxel
#'   \item{pdir:} The passed direction
#'   \item{pdis:} The length of fiber between the passed information and the previous one.
#'   \item{ppdis:} The distance of the voxel between the passed one and the previous one.
#'  }
#'  \item{n.iinds:} Number of voxels the reconstructed fiber went through
#'  \item{n.use.iind:} Number of times that each estimated direction is used.
#'  \item{lens:} The reconstructed fiber lengths.
#'  \item{sorted.iinds:} The ordered reconstructed fibers
#'  \item{sorted.update.ind:} Whether the reconstructed fiber is greater than elim.thres
#' }
#' @seealso \code{\link{tractography}} for plotting tractography based on the tracking result from \code{\link{v.track}} in \code{\link{dmri.tracking}} package.
#' @author Raymond Wong, Seungyong Hwang (Maintainer: \email{syhwang@@ucdavis.edu})
#' @references
#' R. K. W. Wong, T. C. M. Lee, D. Paul, J. Peng and for the Alzheimer's Disease Neuroimaging Initiative. (2016)
#' "Fiber Direction Estimation, Smoothing and Tracking in Diffusion MRI". The Annals of Applied Statistics, 10(3), 1137-1156.
#' @examples
#' #Load an example output from the peak detection algorithm
#' load(system.file("extdata", "peakresult.rda", package = "dmri.tracking"))
#'
#' str(peak.result)  #Output from the peak detection algorithm
#'
#' #Apply Tracking algorithm
#' result = v.track(v.obj = peak.result, max.line=500)
#'
#'
#' #Plot tracking result.
#' \donttest{
#' library(rgl)
#' open3d()
#' for (iind in (result$sorted.iinds[result$sorted.update.ind])){
#'   cat(iind,"\n")
#'   tractography(result$tracks1[[iind]]$inloc, result$tracks1[[iind]]$dir)
#'   tractography(result$tracks2[[iind]]$inloc, result$tracks2[[iind]]$dir)
#' }
#' }
#' #An example to prepare v.obj is available in https://github.com/vic-dragon/dmri.tracking
#'
#' @export
v.track <- function(v.obj, max.line=100, nproj=1, elim=T, elim.thres=1, thres.ang=0.5235988){

  xgrid.sp = v.obj$xgrid.sp
  ygrid.sp = v.obj$ygrid.sp
  zgrid.sp = v.obj$zgrid.sp

  braingrid = v.obj$braingrid

  vorient=c(1,1,1)

  tracks1 <- list()
  tracks2 <- list()

  all.pvox <- NULL
  all.pdir <- NULL
  all.pdis <- NULL
  all.ppdis <- NULL
  n.use.iind <- array(0, dim=length(v.obj$n.fiber2))
  n.iinds <- array(0,dim=length(v.obj$n.fiber2))
  lens <- array(0, dim=length(v.obj$n.fiber2))

  for (iind in which(v.obj$n.fiber2>0)){
    cat(iind,"\n")
    tracks1[[iind]] <- fiber.track(iind=iind, eig=v.obj$vec, loc=v.obj$loc,
                                   map=v.obj$map, rmap=v.obj$rmap,
                                   n.fiber=v.obj$n.fiber, xgrid.sp=xgrid.sp,
                                   ygrid.sp=ygrid.sp, zgrid.sp=zgrid.sp, braingrid=braingrid,
                                   max.line=max.line, nproj=nproj, thres.ang=thres.ang,
                                   vorient=vorient)

    tracks2[[iind]] <- fiber.track(iind=iind, eig=-v.obj$vec, loc=v.obj$loc,
                                   map=v.obj$map, rmap=v.obj$rmap,
                                   n.fiber=v.obj$n.fiber, xgrid.sp=xgrid.sp,
                                   braingrid=braingrid,
                                   ygrid.sp=ygrid.sp, zgrid.sp=zgrid.sp,
                                   max.line=max.line, nproj=nproj, thres.ang=thres.ang,
                                   vorient=vorient)

    n.use.iind[tracks1[[iind]]$iinds] <- n.use.iind[tracks1[[iind]]$iinds] + 1
    n.use.iind[tracks2[[iind]]$iinds] <- n.use.iind[tracks2[[iind]]$iinds] + 1
    n.use.iind[iind] <- n.use.iind[iind] - 1
    n.iinds[iind] <- length(union(tracks1[[iind]]$iinds, tracks2[[iind]]$iinds))
    lens[iind] <- get.fdis(tracks1[[iind]]$inloc) + get.fdis(tracks2[[iind]]$inloc)

    if (length(all.pdis)!=length(all.pvox)){
      break
    }
  }

  len.ord <- order(lens, decreasing=T)
  if (max(lens[n.iinds<=1])> elim.thres){
    cat("elim.thres is too small: it should be set at least", max(lens[n.iinds<=1]),"\n")
  }

  if (elim){
    update.ind <- rep(T, length(v.obj$n.fiber2))
    update.ind[as.logical((v.obj$n.fiber2==0)+(lens<=elim.thres))] <- F
    nv.obj <- update_v.obj(v.obj, list(vec=v.obj$vec, update.ind=update.ind))$obj
  } else {
    nv.obj <- v.obj
    update.ind <- rep(T, length(v.obj$n.fiber2))
    update.ind[as.logical((v.obj$n.fiber2==0)+(lens<=elim.thres))] <- F
  }
  sorted.iinds <- (1:length(v.obj$n.fiber2))[len.ord]
  sorted.update.ind <- update.ind[len.ord]

  return(list(v.obj=nv.obj, tracks1=tracks1, tracks2=tracks2, n.iinds=n.iinds,
              n.use.iind=n.use.iind, update.ind=update.ind, sorted.iinds=sorted.iinds,
              sorted.update.ind=sorted.update.ind, lens=lens))
}

#' Update number of fiber directions after v.smooth.bw
#'
#' @param pre an object from v.est
#' @param res an object from v.smooth.bw or v.smooth or any list(vec, update.ind)
#' @param ... Other parameters
#' @return updated object for fiber directions
#' @noRd

update_v.obj <- function(pre, res, ...){
  iinds <- which(((!res$update.ind)*(pre$n.fiber2>0))==1)
  if (length(iinds)>0){
    nvox <- length(pre$rmap)
    tt <- tabulate(pre$map[iinds],nvox)
    voxs <- which(tt>0)
    nvoxs <- tt[voxs]


    n.fiber <- pre$n.fiber
    n.fiber[voxs] <- n.fiber[voxs] - nvoxs
    n.fiber2 <- pre$n.fiber2

    rmap <- pre$rmap
    for (i in (1:length(voxs))){
      vox <- voxs[i]
      tiind <- pre$rmap[vox]
      ii <- tiind:(tiind + max(0,pre$n.fiber[vox]-1))
      n.fiber2[ii] <- n.fiber2[ii] - nvoxs[i]
      if ((vox+1) <= nvox){
        if (n.fiber[vox]!=0){
          rmap[(vox+1):nvox] <- rmap[(vox+1):nvox] - nvoxs[i]
        } else {
          rmap[(vox+1):nvox] <- rmap[(vox+1):nvox] - nvoxs[i] + 1
        }
      }
    }

    iinds1 <- pre$rmap[voxs[n.fiber[voxs]==0]]
    iinds2 <- setdiff(iinds, iinds1)

    n.fiber2 <- n.fiber2[-iinds2]

    map <- pre$map[-iinds2]
    vec <- res$vec
    vec[iinds1,] <- NA
    vec <- vec[-iinds2,] ###
    loc <- pre$loc[-iinds2,]

    obj <- list(map=map, rmap=rmap, n.fiber2=n.fiber2, n.fiber=n.fiber,
                vec=vec, loc=loc)
    return(list(obj=obj, up=T, remove.iinds=iinds))
  } else {
    obj <- list(map=pre$map, rmap=pre$rmap, n.fiber2=pre$n.fiber2,
                n.fiber=pre$n.fiber, vec=res$vec, loc=pre$loc)
    return(list(obj=obj, up=F, remove.iinds=NULL))
  }
}

#' Get fiber distance
#' @param inloc inloc
#' @noRd
get.fdis <- function(inloc){
  if (nrow(inloc)==2){
    return(sum((apply(inloc,2,diff))^2))
  } else {
    return(sum(apply(apply(inloc,2,diff),1,function(x){sum(x^2)})))
  }
}

#' fiber tracking function
#' @param iind iind
#' @param eig eig
#' @param loc loc
#' @param map map
#' @param rmap rmap
#' @param n.fiber n.fiber
#' @param xgrid.sp xgrid.sp
#' @param ygrid.sp ygrid.sp
#' @param zgrid.sp zgrid.sp
#' @param braingrid braingrid
#' @param max.line max.line
#' @param nproj nprog
#' @param thres.ang thres.ang
#' @param vorient vorient
#' @noRd
fiber.track <- function(iind, eig, loc, map, rmap, n.fiber, xgrid.sp, ygrid.sp,
                        zgrid.sp, braingrid, max.line=1000, nproj=1, thres.ang=0.5235988,
                        vorient=c(1,1,1))
{

  braindim <- dim(braingrid)[-1]
  nvox <- prod(braindim)
  dimens <- c(xgrid.sp, ygrid.sp, zgrid.sp)

  path.voxel <- array(dim=max.line)
  path.dir <- array(dim=c(max.line, 3))
  path.in <- array(dim=c(max.line, 3))
  path.change <- array(dim=max.line)
  path.iind <- array(dim=max.line)
  pass.vox <- NULL
  pass.dir <- NULL
  pass.dis <- NULL
  pass.pdis <- NULL # perpendicular distance


  # initialization of the tract
  path.voxel[1] <- map[iind]  ## tract starting voxel
  path.dir[1,] <- eig[iind,]  ## tract starting direction
  path.in[1,] <- loc[iind,]   ## tract starting location
  path.change[1] <- T
  path.iind[1] <- iind  ## tract starting index (index of all fiber directions in the ROI)

  ii <- 1
  while ((ii<max.line)){

    fio <- fiber.in.out(inc=path.in[ii,]-loc[path.iind[ii],], direct=path.dir[ii,], dimens=dimens)
    path.in[ii+1,] <- fio$outc + loc[path.iind[ii],]

    # for previous pass.dis and pass.pdis, using the previous "change"
    if ((!path.change[ii])&&(n.fiber[path.voxel[ii]]>0)){
      pass.pdis <- c(pass.pdis, dist.line(loc[path.iind[ii],], path.in[ii,], path.in[ii+1,]))
      pass.dis <- c(pass.dis, sqrt(sum((path.in[ii,]-path.in[ii+1,])^2)))
    }

    # determine which voxel it is going to
    next.vox <- get.out.vox(fio$index, path.voxel[ii], braindim=braindim, vorient=vorient)

    if (is.na(next.vox)){
      break
    }

    # determine if we should stop
    pro.res <- project.proceed(inc0=path.in[ii+1,], vox0=next.vox,
                               dir0=path.dir[ii,], loc, eig, rmap, n.fiber,
                               braindim, dimens, nproj=nproj,
                               thres.ang=thres.ang, vorient=vorient)
    change <- pro.res$first
    good <- pro.res$last

    if (!good){
      break
    }

    # update voxel
    path.voxel[ii+1] <- next.vox

    # update dir, iind and change
    if (n.fiber[next.vox]<=1){
      path.iind[ii+1] <- rmap[next.vox]
      path.change[ii+1] <- change
      if (change){
        path.dir[ii+1,] <- eig[path.iind[ii+1],]
      } else {
        path.dir[ii+1,] <- path.dir[ii,]
        if (n.fiber[next.vox]==1){
          pass.vox <- c(pass.vox,next.vox)
          pass.dir <- rbind(pass.dir, path.dir[ii,])
        }
      }
    } else {
      # thresholding rule -> determine stop or not, and within the thresholding rule, choose the closest

      if (change){
        # decide which directions
        tiind <- rmap[next.vox]
        chosen <- which.max(abs(eig[tiind+(0:(n.fiber[next.vox]-1)),]%*%path.dir[ii,]))
        path.iind[ii+1] <- tiind+chosen-1
        path.dir[ii+1,] <- eig[path.iind[ii+1],]
        path.change[ii+1] <- T
      } else {
        path.iind[ii+1] <- rmap[next.vox]
        path.change[ii+1] <- F
        path.dir[ii+1,] <- path.dir[ii,]
        pass.vox <- c(pass.vox,next.vox)
        pass.dir <- rbind(pass.dir, path.dir[ii,])
      }
    }

    # align directions
    path.dir[ii+1,] <- sign(sum(path.dir[ii+1,]*path.dir[ii,]))*path.dir[ii+1,]

    ii <- ii+1
  }

  if (ii<max.line){
    path.in <- path.in[1:(ii+1),]
    path.iind <- path.iind[1:ii]
    path.dir <- path.dir[1:ii,]
    path.change <- path.change[1:ii]
  }
  return(list(inloc=path.in, dir=path.dir, iinds=path.iind, change=path.change, pvox=pass.vox, pdir=pass.dir, pdis=pass.dis, ppdis=pass.pdis))
}

#' get the distance between x and line(y,z)
#' @param x x
#' @param y y
#' @param z z
#' @noRd
dist.line <- function(x, y, z){

  v <- y-z; v <- v/sqrt(sum(v^2))
  w <- y-x
  return(sqrt(sum((w - sum(w*v)*v)^2)))
}

#' assuming inc, outc are coordinates with the center of the voxel being (0,0,0)
#' compute the distance of the current fiber directon to each face of the current voxel
#' create a box with x,y,z dimens and divide by travel speed(direct)
#' @param inc inc
#' @param direct direct
#' @param dimens dimens
#' @noRd
fiber.in.out <- function(inc, direct, dimens){
  # assuming inc, outc are coordinates with the center of the voxel being (0,0,0)
  if (sum(dimens==0)){
    stop("directions has zero component, not yet supported! Please modify fiber.in.out\n")
  }
  # compute the distance of the current fiber directon to each face of the current voxel
  # create a box with x,y,z dimens and divide by travel speed(direct)
  tempdiff <- (round(cbind(dimens/2-inc,-inc-dimens/2),5)/direct)  ## Hao: add round5

  # Hao
  # tempdiff[tempdiff==Inf]=1e10
  tempdiff[tempdiff==-Inf]=Inf
  # tempdiff[is.nan(tempdiff)]=1e10
  # tempdiff[tempdiff==Inf]=1e10

  # get which axis is the current fiber direction hitting face of the current voxel first
  # 1:x  2:y  3:z
  index1 <- which.min(diag(tempdiff[,2-(direct>=0)]))  # Hao change direct>0 to direct>=0
  # which direction it is hitting 1:positive  2:negative
  index <- c(index1, (2-(direct>0))[index1])
  const <- tempdiff[index[1],index[2]]
  outc <- round(inc + const*direct,5)     ## Hao: add round5

  return(list(outc=outc, index=as.vector(index)))
}

#' convert current voxel vector index to the 3d index in ROI
#' @param index index
#' @param cvox cvox
#' @param braindim braindim
#' @param vorient vorient
#' @noRd
get.out.vox <- function(index, cvox, braindim, vorient){
  # convert current voxel vector index to the 3d index in ROI
  cvoxindex <- as.vector(arrayInd(cvox, braindim))
  if (index[2]==1){
    # positive sides
    cvoxindex[index[1]] <- cvoxindex[index[1]] + vorient[index[1]]
  } else {
    # negative sides
    cvoxindex[index[1]] <- cvoxindex[index[1]] - vorient[index[1]]
  }
  if ((cvoxindex[index[1]]<1)||(cvoxindex[index[1]]>braindim[index[1]])){
    return(NA)
  } else {
    return(ArrayIndex(braindim, cvoxindex[1], cvoxindex[2], cvoxindex[3]))
  }
}

#' project proceed
#' @param inc0 inc0
#' @param vox0 vox0
#' @param dir0 dir0
#' @param loc loc
#' @param eig eig
#' @param rmap rmap
#' @param n.fiber number fiber
#' @param braindim braindim
#' @param dimens dimens
#' @param nproj nproj
#' @param thres.ang thres angular
#' @param vorient vorient
#' @noRd
project.proceed <- function(inc0, vox0, dir0, loc, eig, rmap, n.fiber,
                            braindim, dimens, nproj=2, thres.ang=0.5235988,
                            vorient=c(1,1,1)){
  # first
  first <- proceed(vox0, dir0, eig, rmap, n.fiber, thres.ang)

  cont <- !first
  num <- 1
  tinc <- inc0
  vox <- vox0
  last <- first
  iind <- rmap[vox]
  while (cont && (num <= nproj)){
    fio <- fiber.in.out(inc=tinc-loc[iind,], direct=dir0, dimens=dimens)
    tinc <- fio$outc + loc[iind,]

    # determine which voxel it is going to
    vox <- get.out.vox(fio$index, vox, braindim=braindim, vorient=vorient)
    if (is.na(vox)){
      last <- F
      break
    }
    iind <- rmap[vox]
    last <- proceed(vox, dir0, eig, rmap, n.fiber, thres.ang)
    if (last){
      break
    }
    num <- num + 1
  }
  return(list(first=first, last=last))
}

#' proceed
#' @param vox0 vox0
#' @param dir0 dir0
#' @param eig eig
#' @param rmap rmap
#' @param n.fiber number of fiber
#' @param thres.ang threshold of angular
#' @noRd
proceed <- function(vox0, dir0, eig, rmap, n.fiber, thres.ang=0.5235988){
  good <- T
  if (n.fiber[vox0]==0){
    good <- F
  } else if (n.fiber[vox0]==1) {
    good <- acos(min(abs(eig[rmap[vox0],]%*%dir0),1))<thres.ang
  } else {
    good <- as.logical(sum(as.vector(acos(pmin(abs(eig[rmap[vox0]+(0:(n.fiber[vox0]-1)),]%*%dir0),1)))<thres.ang))
  }
  return(good)
}


#' Tractography
#'
#' Visualize the result from v.track.
#' r package \code{\link{rgl}} is required.
#'
#' @param loc Voxel coordinates that the reconstructed fiber go through
#' @param vec Diffusion direction that used to reconstruct the fiber
#'
#' @seealso The tracking result from \code{\link{v.track}} can be used for \code{\link{tractography}} in \code{\link{dmri.tracking}} package.
#'
#' @author Raymond Wong, Seungyong Hwang (Maintainer: \email{syhwang@@ucdavis.edu})
#'
#' @return
#' No return value. The reconstructed fiber is visualized on the opened rgl window.
#'
#' @references
#' R. K. W. Wong, T. C. M. Lee, D. Paul, J. Peng and for the Alzheimer's Disease Neuroimaging Initiative. (2016)
#' "Fiber Direction Estimation, Smoothing and Tracking in Diffusion MRI". The Annals of Applied Statistics, 10(3), 1137-1156.
#'
#' @examples
#' #Load an example output from the peak detection algorithm
#' load(system.file("extdata", "peakresult.rda", package = "dmri.tracking"))
#'
#' str(peak.result)  #Output from the peak detection algorithm
#'
#' #Apply Tracking algorithm
#' result = v.track(v.obj = peak.result, max.line=500)
#'
#'
#' #Plot tracking result.
#' \donttest{
#' library(rgl)
#' open3d()
#' for (iind in (result$sorted.iinds[result$sorted.update.ind])){
#'   cat(iind,"\n")
#'   tractography(result$tracks1[[iind]]$inloc, result$tracks1[[iind]]$dir)
#'   tractography(result$tracks2[[iind]]$inloc, result$tracks2[[iind]]$dir)
#' }
#' }
#' #An example to prepare v.obj is available in https://github.com/vic-dragon/dmri.tracking
#'
#' @export
#'
tractography <- function(loc, vec)
{
  x_lim=NULL
  y_lim=NULL
  z_lim=NULL

  if(is.null(x_lim)&&is.null(y_lim)&&is.null(z_lim)){
    # repeat location for get color for each line segment
    loc1 <- matrix(rep(loc, each=2), ncol=3)
    loc1 <- loc1[-c(1, nrow(loc1)), ]

    # get color
    col1 <- get.fib.col(vec)
    col1 <- rep(col1, each=2)

    rgl::lines3d(loc1, lwd=1, col=col1)
  }else{
    epsi = 1e-5
    index_t = 1:dim(loc)[1]
    dele_t = which((((loc[,2]>y_lim[1]-epsi)&(loc[,2]<y_lim[2]+epsi))==0)|(((loc[,1]>x_lim[1]-epsi)&(loc[,1]<x_lim[2]+epsi))==0)|(((loc[,3]>z_lim[1]-epsi)&(loc[,3]<z_lim[2]+epsi))==0))
    if(length(dele_t)==length(index_t)){

    }else if((length(dele_t)>0)&&(length(index_t)-length(dele_t)>1)){
      index_u = index_t[-dele_t]
      loc = loc[index_u,]

      index_v = integer(0)
      for(i in 1:(length(index_u)-1)){
        if((index_u[i+1]-index_u[i])==1){
          index_v = c(index_v,index_u[i])
        }
      }
      vec = vec[index_v,]
      # repeat location for get color for each line segment
      loc1 <- matrix(rep(loc, each=2), ncol=3)
      loc1 <- loc1[-c(1, nrow(loc1)), ]

      # get color
      col1 <- get.fib.col(vec)
      col1 <- rep(col1, each=2)

      rgl::lines3d(loc1, lwd=1, col=col1)
    } else if(length(dele_t)==0) {
      loc1 <- matrix(rep(loc, each=2), ncol=3)
      loc1 <- loc1[-c(1, nrow(loc1)), ]

      # get color
      col1 <- get.fib.col(vec)
      col1 <- rep(col1, each=2)

      rgl::lines3d(loc1, lwd=1, col=col1)
    }
  }
}

#' Fiber coloring
#' @param vecs vecs
#' @noRd
get.fib.col <- function(vecs)
{
  vecs <- abs(vecs)
  if (length(vecs)==3){
    vecs <- matrix(vecs, ncol=3)
  }
  return(grDevices::rgb(red=vecs[,1], green=vecs[,2], blue=vecs[,3], alpha=1))
}


#### wrapper function of dwi.fit.l1.internal_C ####
#dwi.fit.l1.internal <- function(lasso.lam, betas0, y, S, gam, betas.w,
#                                sigma, Nmiddle.max=100, Ninner=25, tol=1e-8,
#                                beta.max=10)
#{
#  betas <- .Call("dwi_fit_l1_internal_C", lasso.lam, betas0, y, S, gam,
#                 betas.w, sigma, Nmiddle.max, Ninner, tol, beta.max,
#                 PACKAGE="dmri.tracking")
#  return(list(betas=betas,df=sum(betas!=0),gam=gam,betas.w=betas.w))
#}

#' wrapper function of ArrayIndex_C
#' @param dims dims
#' @param xrange xrange
#' @param yrange yrange
#' @param zrange zrange
#'
#' @useDynLib dmri.tracking ArrayIndex_C
#' @noRd
ArrayIndex <- function(dims, xrange, yrange, zrange)
{
  return(.Call("ArrayIndex_C", dims, xrange, yrange, zrange, PACKAGE="dmri.tracking"))
}

