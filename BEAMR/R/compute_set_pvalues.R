#' Compute BEAMR p-values for sets
#'
#' @param beam.stats A beam.stats object from compute_beam_stats function
#' @param peel Logical indicating whether to peel in p-value calculation
#' @param z Logical indicating whether to z-scale each vector of one coefficient estimate across bootstraps before analysis
#' @param alpha Maximum depth to peel (reduces computing time); default 0.1.
#' @param mess.freq Message frequency; default 25.
#'
#' @returns A list with a data.frame of set p-values from BEAMR analysis, a data.frame of summary row p-values, and a data frame of set matching.
#' @importFrom stats prcomp
#' @importFrom stats p.adjust
#' @importFrom stats predict
#' @importFrom stats sd
#' @export
#'
#' @examples
#' data(beam_stats_sm)
#' test.pvals <- compute_set_pvalues(beam.stats=beam_stats_sm)
compute_set_pvalues=function(beam.stats, peel=FALSE, z=TRUE, alpha=0.1, mess.freq=25)
{
  ######################################################
  # Identify the matrices with analyses specified
  beam.specs=beam.stats$beam.specs
  specs.mtx=beam.specs[,"mtx"]
  stat.ids=names(beam.stats$beam.stats)

  #################################################
  # Extract the set data with analyses specified
  message(paste0("Preparing bootstrap results for calculating feature set p-values: ",date()))
  set.data=beam.stats$beam.data$set.data

  keep.set.data=is.element(set.data$mtx.id,specs.mtx)
  set.data=set.data[keep.set.data,]


  ord=order(set.data$mtx.id)
  set.data=set.data[ord,]
  a=nrow(set.data)
  new.sect=which((set.data$mtx.id[-1]!=set.data$mtx.id[-a]))
  row.start=c(1,new.sect+1)
  row.end=c(new.sect,a)
  mtx.index=cbind.data.frame(row.start=row.start,
                             row.end=row.end,
                             mtx.id=set.data$mtx.id[row.end])

  ###############################################
  # Merge set data with analysis stat matrices

  message(paste0("Finding stats for each data matrix:",date()))
  set.mtch=NULL
  for (i in 1:nrow(mtx.index))
  {
    message(paste0("  Finding stats for data matrix ",mtx.index$mtx.id[i],": ",date()))
    spec.rows=which(specs.mtx==mtx.index$mtx.id[i])
    for (j in spec.rows)
    {
      message(paste0("   Finding features with with ",stat.ids[j]," stats: ",date()))
      row.ids=row.names(beam.stats$beam.stats[[j]])
      stat.rows=cbind(mtx.row=paste0(mtx.index$mtx.id[i],"_",row.ids),
                      stat.id=stat.ids[j],
                      row.id=row.ids)
      set.mtch=rbind.data.frame(set.mtch,stat.rows)
    }
  }
  message(paste0("Found ",nrow(set.mtch)," rows of stats: ",date()))

  message(paste0("Merging stats with feature-sets: ",date()))
  set.data$mtx.row=paste0(set.data$mtx.id,"_",set.data$row.id)
  set.mtch=merge(set.data[c("mtx.row","set.id")],
                 set.mtch,
                 by="mtx.row")
  message(paste0("Merged feature-set stat rows: ",nrow(set.mtch)))

  ##########################################
  # Index feature-set stat rows
  message(paste0("Ordering and indexing feature sets: ",date()))
  ord=order(set.mtch$set.id,
            set.mtch$stat.id)
  set.mtch=set.mtch[ord,]

  m=nrow(set.mtch)
  new.sect=which((set.mtch$set.id[-1]!=set.mtch$set.id[-m])|
                   (set.mtch$stat.id[-1]!=set.mtch$stat.id[-m]))
  row.start=c(1,new.sect+1)
  row.end=c(new.sect,m)

  set.stat.index=cbind.data.frame(row.start=row.start,
                                  row.end=row.end,
                                  set.id=set.mtch$set.id[row.start],
                                  stat.id=set.mtch$stat.id[row.start])

  m=nrow(set.stat.index)
  new.sect=which(set.stat.index$set.id[-1]!=set.stat.index$set.id[-m])
  row.start=c(1,new.sect+1)
  row.end=c(new.sect,m)
  set.index=cbind.data.frame(row.start=row.start,
                             row.end=row.end,
                             set.id=set.stat.index$set.id[row.start])

  ########################################
  # clean up bootstrap coefficient matrices
  message(paste0("  Cleaning up beam.stat matrices:",date()))
  mtx.list=names(beam.stats$beam.stats)
  n.mtx=length(mtx.list)
  for (i in 1:n.mtx)
  {
    message(paste0("   Working on matrix ",i," of ",n.mtx,": ",date()))
    B.mtx=beam.stats$beam.stats[[i]]
    B.cln=clean_Bmtx(B.mtx)
    beam.stats$beam.stats[[i]]=B.cln
  }

  ##################################################
  # Compute summary statistics of bootstrap results

  # message(paste0("  Computing summary stats of bootstrap results for each feature: ",date()))
  smry.mtx=NULL
  # for (i in 1:n.mtx)
  # {
  #   message(paste0("   Working on matrix ",i," of ",n.mtx,": ",date()))
  #   B.mtx=beam.stats$beam.stats[[i]]
  #   tmp.stats=apply(B.mtx,1,smry.stats)
  #   tmp.stats=t(tmp.stats)
  #   rownames(tmp.stats)=paste0(mtx.list[i],"_",
  #                              rownames(tmp.stats))
  #   smry.mtx=rbind(smry.mtx,tmp.stats)
  # }
  #
  # message(paste0("  Merging feature-level bootstrap summaries with set definitions: ",date()))
  # set.mtch$stat.row=paste0(set.mtch$stat.id,"_",set.mtch$row.id)
  # smry.mtx=merge(set.mtch,smry.mtx,by.x="stat.row",by.y="rownames",all=TRUE)

  #print(head(set.index))
  #print(head(set.stat.index))
  #print(head(set.mtch))

  ##############################################
  # Extract bootstrap coefs and compute p-value for each set
  r=nrow(set.index)
  p.set=rep(NA,r)
  features=rep("",r)
  origin.to.center=rep(NA,r)
  mean.from.center=rep(NA,r)
  distance.ratio=rep(NA,r)

  for (i in 1:r)
  {
    if (((i-1)%%mess.freq)==0)
    {
      message(paste0("Computing p-value for feature set ",i," of ",r,": ",date()))
      message(set.index[i,])
    }

    ind1.start=set.index$row.start[i]
    ind1.end=set.index$row.end[i]
    ind2.start=set.stat.index$row.start[ind1.start]
    ind2.end=set.stat.index$row.end[ind1.end]
    ft.str=paste(set.stat.index$row.end[ind1.start:ind1.end]-
                   set.stat.index$row.start[ind1.start:ind1.end]+1,
                 set.stat.index$stat.id[ind1.start:ind1.end])
    features[i]=paste(ft.str,collapse=" | ")
    m=ind2.end-ind2.start+1
    B=NULL
    #obs.vec <- NULL
    for (j in ind1.start:ind1.end)
    {
      stat.id=set.stat.index$stat.id[j]
      row.start=set.stat.index$row.start[j]
      row.end=set.stat.index$row.end[j]
      row.ids=set.mtch$row.id[row.start:row.end]
      #b.mtx=beam.stats$beam.stats[[stat.id]][row.ids,-1]
      #obs.vec <- c(obs.vec, beam.stats$beam.stats[[stat.id]][row.ids, 1])
      b.mtx=beam.stats$beam.stats[[stat.id]][row.ids,] # keep observed value as first column
      B=rbind(B,b.mtx)
    }
    B=t(B)                          # transpose for distance calculations
    #clm.sd=apply(B,2,sd)
    #clm.sd=apply(B[-1,],2,sd)
    #if (((i-1)%%mess.freq)==0) message(summary(clm.sd))
    res.list <- beam.pvalue(B, peel=peel, z=z, alpha=alpha)
    p.set[i] <- res.list$p
    origin.to.center[i] <- res.list$dist.to.null
    mean.from.center[i] <- res.list$mean.from.obs
    distance.ratio[i] <- res.list$dist.to.null/res.list$mean.from.obs
  }
  message(paste0("Finished computing p-values at: ",date()))
  pi0.hat=min(1,2*mean(p.set,na.rm=TRUE))
  q.set=pi0.hat*stats::p.adjust(p.set,method="fdr")
  message(paste0("Minimum q-value is ", min(q.set)))

  message("Creating set p-value data frame.")
  set.pvals=cbind.data.frame(set.id=set.index$set.id,
                             features=features,
                             mean.from.center=mean.from.center,
                             origin.to.center=origin.to.center,
                             distance.ratio=distance.ratio,
                             p.set=p.set,
                             q.set=q.set)
  message("Done creating data frame.")

  set.anns=beam.stats$beam.data$set.anns
  if (!is.null(set.anns))
  {
    ann.pvals=merge(set.anns,set.pvals,
                    by="set.id",all=TRUE)
    set.pvals=ann.pvals
  }

  message("Creating list for output.")
  res=list(set.pvals=set.pvals,
           row.pvals=smry.mtx,
           set.mtch=set.mtch)

  return(res)
}


beam.pvalue=function(B.mtx,     # bootstrap matrix with observed result in row 1
                     peel=FALSE,    # indicates whether or not to peel
                     z=TRUE,       # indicates whether z-scale each vector of one coefficient estimate across bootstraps before analysis
                     alpha=0.1) # maximum depth to peel (reduce computing time)

{
  cent=B.mtx[1,,drop=FALSE]                             # observed result is first row, it will be the center for PCA calculations
  B0=B.mtx[-1,,drop=FALSE]                              # remove observed result
  B.mtx=B0                                   # bootstrap association matrix
  b0=b=nrow(B.mtx)                           # number of bootstraps

  # Check column variability in B.mtx
  col.sd <- apply(B.mtx, 2, stats::sd)
  # If there is a constant column, scale the other columns by hand to avoid prcomp error
  if(any(col.sd==0)&z){
    B.mtx.scale <- B.mtx
    for(i in 1:ncol(B.mtx.scale)){
      if(col.sd[i]!=0){B.mtx.scale[,i] <- (B.mtx[,i]-cent[i])/f(B.mtx[,i]-cent[i])}
    }
    pca.res=stats::prcomp(B.mtx.scale,
                          center=FALSE,
                          scale.=FALSE)
  }
  else{
    pca.res=stats::prcomp(B.mtx,                      # principal components for all bootstraps
                          center=cent,                # use the observed result as the center
                          scale.=z)               # user option for rescaling PCs before computing distance

  }

  null=matrix(0,1,ncol(B.mtx))               # define vector for null (origin: all coefs = 0)
  colnames(null)=colnames(B.mtx)             # assign column names
  pca.null=stats::predict(pca.res,null)             # project original null into PC space
  pca.mtx=rbind(pca.res$x,pca.null)          # matrix of PCs (bootstraps and null)

  pca.dist=rowSums(pca.mtx^2)                # distance of null and each bootstrap from observed
  p.indx=which(pca.dist[-(b+1)]>=pca.dist[(b+1)])    # index of bootstraps farther from observed than null
  dist.to.null <- pca.dist[(b+1)]
  mean.from.obs <- mean(pca.dist[-(b+1)])


  if (peel)   # if requested, use peeling to compute p-values
  {
    p=0                                      # initialize p-value
    indx=1:b                                 # index of bootstraps
    drop.indx=NULL                           # index of those farther from center than null
    cont=TRUE                                   # initialize indicator on whether to continue looping

    while(cont)                         # loop over iterative peeling
    {
      max.indx=which.max(pca.dist)              # index of most remote bootstrap point
      hit.null=(max.indx==length(pca.dist))     # is this the index of the null?
      p=p+(!hit.null)                           # increment the p-value if it's not the null
      cont=(p<(alpha*b0))&&(!hit.null)          # determine whether to continue

      if (cont)                              # if need to continue do the following
      {
        drop.indx=c(drop.indx,indx[max.indx])     # capture index of this bootstrap point
        indx=indx[-max.indx]                      # remove from remaining bootstrap index list

        B.mtx=B.mtx[-max.indx,]
        b=nrow(B.mtx)                           # number of remaining bootstraps
        # Check column variability in B.mtx
        col.sd <- apply(B.mtx, 2, stats::sd)
        # If there is a constant column, scale the other columns by hand to avoid prcomp error
        if(any(col.sd==0)&z){
          B.mtx.scale <- B.mtx
          for(i in 1:ncol(B.mtx.scale)){
            if(col.sd[i]!=0){B.mtx.scale[,i] <- (B.mtx[,i]-cent[i])/f(B.mtx[,i]-cent[i])}
          }
          pca.res=stats::prcomp(B.mtx.scale,
                                center=FALSE,
                                scale.=FALSE)
        }
        else{
          pca.res=stats::prcomp(B.mtx,                      # principal components for all bootstraps
                                center=cent,                # use the observed result as the center
                                scale.=z)               # user option for rescaling PCs before computing distance

        }

        null=matrix(0,1,ncol(B.mtx))            # define vector for null (origin: all coefs = 0)
        colnames(null)=colnames(B.mtx)          # assign column names
        pca.null=stats::predict(pca.res,null)          # project original null into PC space
        pca.mtx=rbind(pca.res$x,pca.null)       # matrix of pcas

        pca.dist=rowSums(pca.mtx^2)             # distance of null and each bootstrap from observed

      }                                     # end if need to continue section
    }                                  # end loop over iterative peeling

    if ((p>=(alpha*b0))&&(alpha<1))     # if p-value reaches alpha, use simple distance for remaining points
    {
      p.indx=which(pca.dist[1:b]>=pca.dist[b+1])  # indices of points farther from center than null
      p=p/b0+(1-p/b0)*length(p.indx)/b                  # p-value is mixture of peeling and simple distance for remaining points
      p.indx=c(drop.indx,indx[p.indx])
    }
  }       # end section of using peeling to compute p-values

  p=length(p.indx)/b0

  # use a simple interpolation from 1/b to zero for p < 1/b
  if (p==0)
  {
    p=max(pca.dist[1:b])/(b*pca.dist[b+1])
  }

  output.list <- list(p=p, dist.to.null=dist.to.null, mean.from.obs=mean.from.obs)
  return(output.list)
}

# scale in scale R command
f <- function(v) {
  v <- v[!is.na(v)]
  sqrt(sum(v^2) / max(1, length(v) - 1L))
}
