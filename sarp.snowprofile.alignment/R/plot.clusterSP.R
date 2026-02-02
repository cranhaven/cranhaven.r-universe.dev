#' Plot clustered snow profiles
#'
#' Plot the snowprofileSet sorted and divided by clusters with vertical lines. If available also plot
#' the centroid or medoid profiles beneath the snowprofileSets.
#'
#' @param x a clusterSP object output by [clusterSP]
#' @param SPx a [sarp.snowprofile::snowprofileSet] to be clustered
#' @param centers plot either `centroids`, `medoids` or `none` profiles underneath each set of clustered profiles
#' @param include plot `rta` or `p_unstable` distributions next to centroids
#' @param hardnessResidualSPx Value within (0, 1) to control the minimum horizontal space of each layer that will be
#' e colored irrespective of the layer's hardness. A value of 1 corresponds to no hardness being shown
#' @param SortMethod sort profiles within each cluster by either snow depth `hs` (default) or `unsorted`
#' @param ... Additional parameters passed to [sarp.snowprofile::plot.snowprofileSet]
#' @importFrom graphics abline legend title
#' @seealso [clusterSP]
#' @author fherla shorton
#' @export
plot.clusterSP <- function(x,
                           SPx = NULL,
                           centers = c('centroids', 'medoids', 'n')[1],
                           include = c(NA, 'rta_dist', 'pu_dist')[1],
                           hardnessResidualSPx = 1,
                           SortMethod = c('hs', 'unsorted'),
                           ...) {

  ## --- Initialization ----
  if (!is.null(SPx)) x$SPx <- SPx
  if (!'SPx' %in% names(x)) stop('Cluster object contains no SPx, provide SPx as separate parameter!')
  if (!'id.med' %in% names(x)) x$id.med <- NULL

  restore <- FALSE
  plot_centroids <- FALSE

  if (centers == 'medoids') x$centroids <- x$medoids
  if (centers == 'n') x$centroids <- x$id.med <- NULL
  if ('centroids' %in% names(x)) {
    opar <- par(mfrow = c(2, 1))
    on.exit(par(opar))
    restore <- TRUE
    plot_centroids <- TRUE
  }

  if (!plot_centroids & !all(is.na(include))) warning('Can not include any distributions because your clusterSP object contains no centroids!')

  if (SortMethod[1] == 'hs') {
    meta <- data.frame(clustering = x$clustering,
                       hs = summary(x$SPx)$hs)
    medianHS <- sapply(unique(sort(meta$clustering)), function(cli) median(meta$hs[meta$clustering == cli], na.rm = TRUE))
    meta$medianHS <- medianHS[meta$clustering]
    kSPx <- order(meta$medianHS, meta$clustering, meta$hs)
    kcent <- unique(na.omit(meta$clustering[kSPx]))
  } else {
    kSPx <- order(x$clustering)
    kcent <- unique(x$clustering[kSPx])
  }
  kcent_nSP <- sapply(kcent, function(k) length(which(x$clustering == k)))

  ## --- plotting ----

  ## plot cluster profiles
  plot(x$SPx, SortMethod = 'presorted', k = kSPx, hardnessResidual = hardnessResidualSPx, xticklabels = 'originalIndices',
       xPadding = 0, yPadding = 0, OutlineProfile = x$id.med, ...)
  title(paste0('Clustered profiles (', x$call$type, ')'))
  diffvec <- diff(x$clustering[kSPx])
  abline(v = seq_along(diffvec)[-which(diffvec == 0)] + 0.5, lwd = 2)


  ## plot centroids
  if (plot_centroids) {
    if (any(include %in% 'rta_dist')) {
      plot(x$centroids, SortMethod = 'presorted', k = kcent, hardnessResidual = 0, hardnessScale = 0.4, hardnessOffset = 0,
           xticklabels = 'originalIndices', xlab = paste('Cluster', centers), box = FALSE)
      if ('rta' %in% names(x$centroids[[1]]$layers)) {
        for (c in kcent) {
          bt <- backtrackLayers(x$centroids[[c]], profileSet = x$SPx[which(x$clustering == c)], computationByHeight = TRUE)
          rta_Ngeq6 <- sapply(bt, function(bti) sum(bti$rta > 0.65)/ length(bti$rta))
          rta_Ngeq8 <- sapply(bt, function(bti) sum(bti$rta > 0.8)/ length(bti$rta))
          # rta_median <- sapply(bt, function(bti) median(bti$rta_layer))
          # cols <- colorRampPalette(c(rgb(1,0,0,0.02), rgb(1,0,0,1)), alpha = TRUE)(3)

          ## plot lines
          # points(c+0.5*rta_Ngeq6-0.5,as.numeric(names(bt)), pch = 20, cex = 0.5, col = 'black')
          # points(c+0.5*rta_Ngeq8-0.5,as.numeric(names(bt)), pch = 20, col = 'red')
          # lines(which(kcent == c)+0.4*rta_Ngeq8-0.4,as.numeric(names(bt)), lwd = 3, col = rgb(1, 0, 0, 0.8))
          # lines(which(kcent == c)+0.4*rta_Ngeq6-0.4,as.numeric(names(bt)), lwd = 2.5, col = rgb(0, 0, 0, 0.5))

          ## color points according to median value
          # rta_colidx <- findInterval(rta_median, c(0, 0.6, 0.8))
          # points(c+0.9*rta_NgeqT-0.5, as.numeric(names(bt)), pch = 20, col = cols[rta_colidx], cex = 1)

          ## plot all individual points:
          # df <- data.frame(x = do.call('c', lapply(bt, function(bti) bti$rta_layer)),
          #                  y = do.call('c', lapply(as.numeric(names(bt)), function(bti) rep(bti, times = sum(x$clustering == c)))))
          # df$colidx <- findInterval(df$x, c(0, 0.6, 0.8))
          # points(c+0.625*df$x-0.5, df$y, pch = 20, col = cols[df$colidx], cex = 0.5)

          ## plot horizontal stacked histogram for RTA
          xoffset <- min(0.4, 0.4*kcent_nSP[kcent == c]/max(kcent_nSP))
          x0 <- which(kcent == c) - xoffset - 0.01
          ygrid <- as.numeric(names(bt))
          cols <- c(grDevices::rgb(1, 0, 0, 0.05),
                    grDevices::rgb(1, 0, 0, 0.2),
                    grDevices::rgb(1, 0, 0, 1))
          stackedhist <- rbind(data.frame(xleft = 0, xright = 1-rta_Ngeq6, ytop = ygrid, ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[1]),
                               data.frame(xleft = 1-rta_Ngeq6, xright = (1-rta_Ngeq6) + rta_Ngeq6, ytop = ygrid, ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[2]),
                               data.frame(xleft = 1-rta_Ngeq8, xright = 1, ytop = ygrid, ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[3]))
          rect(xleft = x0+xoffset*stackedhist$xleft, ybottom = stackedhist$ybottom, xright = x0+xoffset*stackedhist$xright, ytop = stackedhist$ytop, col = stackedhist$col, border = NA)
        }
        # legend('right', title = '% of members with', c('RTA > 0.8', 'RTA > 0.6'), col = c(rgb(1, 0, 0, 0.8), rgb(0, 0, 0, 0.5)), lty = 'solid', box.lty = 0, bty = 'n', cex = 0.8)
        legend('topleft', c('poor', 'transitional', 'good'), horiz = TRUE,
               title = 'Stability of cluster members (RTA)', fill = rev(cols), border = 'transparent',
               box.lty = 0, bty = 'n', cex = 0.8, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5))
      } else {
        warning('Seems like there is no RTA stored in your profiles!')
      }
    } else if (any(include %in% 'pu_dist')) {
      plot(x$centroids, SortMethod = 'presorted', k = kcent, hardnessResidual = 0, hardnessScale = 0.4, hardnessOffset = 0,
           xticklabels = 'originalIndices', xlab = paste('Cluster', centers), box = FALSE)
      if ('p_unstable' %in% names(x$centroids[[1]]$layers)) {
        for (c in kcent) {
          bt <- backtrackLayers(x$centroids[[c]], profileSet = x$SPx[which(x$clustering == c)], computationByHeight = TRUE)
          trans <- sapply(bt, function(bti) sum(bti$p_unstable >= 0.7)/ length(bti$p_unstable))
          poor <- sapply(bt, function(bti) sum(bti$p_unstable >= 0.77)/ length(bti$p_unstable))
          ## plot horizontal stacked histogram for p_unstable
          xoffset <- min(0.4, 0.4*kcent_nSP[kcent == c]/max(kcent_nSP))
          x0 <- which(kcent == c) - xoffset - 0.01
          ygrid <- as.numeric(names(bt))
          cols <- c(grDevices::rgb(1, 0, 0, 0.05),
                    grDevices::rgb(1, 0, 0, 0.2),
                    grDevices::rgb(1, 0, 0, 1))
          stackedhist <- rbind(data.frame(xleft = 0, xright = 1-trans, ytop = ygrid, ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[1]),
                               data.frame(xleft = 1-trans, xright = (1-trans) + trans, ytop = ygrid, ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[2]),
                               data.frame(xleft = 1-poor, xright = 1, ytop = ygrid, ybottom = c(0, ygrid[1:(length(ygrid)-1)]), col = cols[3]))
          rect(xleft = x0+xoffset*stackedhist$xleft, ybottom = stackedhist$ybottom, xright = x0+xoffset*stackedhist$xright, ytop = stackedhist$ytop, col = stackedhist$col, border = NA)
        }
        legend('topleft', c('poor', 'transitional', 'good'), horiz = TRUE,
               title = 'Stability of cluster members (p_unstable)', fill = rev(cols),
               border = 'transparent', box.lty = 0, bty = 'n', cex = 0.8, x.intersp = 1,
               y.intersp = 1, adj = c(0, 0.5))
      } else {
        warning('Seems like there is no p_unstable stored in your profiles!')
      }
    } else {  # END IF p_unstable_distribution
      plot(x$centroids, SortMethod = 'presorted', k = kcent, hardnessResidual = 0.1, hardnessScale = 0.95,
           xticklabels = 'originalIndices', xlab = paste('Cluster', centers), box = FALSE)
    }
  }  # END plot_centroids


  if (restore) par(opar)
}
