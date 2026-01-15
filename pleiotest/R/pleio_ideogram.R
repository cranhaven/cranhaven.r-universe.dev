#' @title Plot ideogram from pleio_test results
#' @description Plots genomic segments that contain significant pleiotropic SNPs using results of pleio_test(). It also returns a dataframe with segment information.
#' @param pleio_res list returned by pleio_test().
#' @param alpha numeric threshold for significance level (Bonferroni correction by default).
#' @param n_traits integer indicating the level of pleiotropy to test (a.k.a. number of traits).
#' @param bp_positions dataframe with colnames 'chr' and 'pos' indicating the chromosome and position for each SNP. Rownames must contain SNP names matching results of pleio_test.
#' @param window_size numeric value indicating the minimum size (in base pairs) of the genomic region that contains significant SNPs.
#' @param centromeres string 'human' or dataframe (or matrix) with chromosome and position (in mbp) of the centromeres in the first and second columns. If NULL (default) does not plot the centromeres.
#' @param color_bias number for bias of the color scale. See help(colorRampPalette). By default color_bias = 1
#' @param set_plot logical indicating whether to plot the ideogram (TRUE by default).
#' @param set_legend logical indicating whether to plot a legend (TRUE by default).
#' @param set_ylim_prop numeric proportion of upper margin to fit the legend (no margin by default). 1 = no margin, 1.1 = 10% left for margin, etc.
#' @param ... more plot arguments.
#' @return Ideogram plot and a dataframe with genomic segments information.
#' @seealso \code{\link{pleio_plot}}
#' @export
pleio_ideogram <- function(pleio_res, alpha = 'bonferroni05', n_traits = 2, bp_positions, window_size = 1e6, centromeres = NULL, color_bias = 1, set_plot = T, set_legend = T, set_ylim_prop = 1.1, ...){

  total_n_traits <- ncol(pleio_res[[1]])
  if (alpha == 'bonferroni05')
    alpha <- 0.05 / length(nrow(pleio_res[[2]]))

  p_values <- apply(pleio_res[[1]][, 1:n_traits, drop = F], 1, max)
  if (!n_traits > ncol(pleio_res[[2]])){
    indices <- as.character(pleio_res[[2]][, n_traits])
  } else {
    indices <- rep('', length(p_values))
  }

  p_values[p_values == 0] <- 1e-300
  p_notna <- !is.na(p_values)
  p_values <- p_values[p_notna]
  indices <- indices[p_notna]

  if (class(bp_positions) != 'data.frame'){
    warning ('bp_positions must be a data frame')
    bp_positions <- as.data.frame(bp_positions)
  }

  if (is.null(rownames(bp_positions)))
    stop ('bp_positions must have names matching names with pleio_res')
  if (any(is.na(bp_positions)))
    stop('bp_positions cannot have NAs')

  chr_col <- grep('chr', colnames(bp_positions), ignore.case = T, value = T)[1]
  bpp_col <- grep('pos', colnames(bp_positions), ignore.case = T, value = T)[1]

  if(length(chr_col) == 0 | length(bpp_col) == 0)
    stop('bp_positions must have col names for chromosomes (chr) and positions (pos)')

  snp_names <- rownames(bp_positions)
  chr_integers <- as.integer(bp_positions[, chr_col])

  pos_integers <- as.integer(bp_positions[, bpp_col])
  chr_pos <- data.frame(chr_integers, pos_integers - min(pos_integers), row.names = snp_names)
  colnames(chr_pos) <- c('chr', 'pos')

  if (total_n_traits > length(unique(chr_pos$chr)))
    stop('There must be more chromosomes than total number of traits')

  data1 <- cbind(chr_pos[match(names(p_values), rownames(chr_pos)),],
                 pvalues = p_values,
                 index = indices)
  data1 <- data1[!is.na(data1$chr),]

  windows <- by(data1, data1$chr, function(chr_i){
    chr_i$sign <- chr_i$pvalues < alpha
    chr_i$window <- FALSE
    tmp_pos <- chr_i[chr_i$sign,'pos']

    for (ws in tmp_pos){
      chr_i$window[abs(chr_i$pos - ws) < window_size] <- TRUE
    }
    chr_i <- chr_i[order(chr_i$pos),]
    tmp1 <- rle(chr_i$window)
    if (tmp1$values[1]) {
      tmp1$lengths <- c(1, tmp1$lengths)
      tmp1$values <- c(F, tmp1$values)
    }
    true <- which(tmp1$values)
    window <- as.data.frame(matrix(NA, nrow = length(true), ncol = 7))
    colnames(window) <- c('chr', 'lower', 'upper', 'nSNP', 'min_pval', 'SNP', 'traits')
    if(any(tmp1$values)){
      for(i in seq_along(true)){
        win_i <- cumsum(tmp1$lengths)[c(true[i] - 1, true[i])]
        tmp <- chr_i[win_i[1]:win_i[2],]
        window$chr[i] <- tmp$chr[1]
        window$lower[i] <- min(tmp$pos)
        window$upper[i] <- max(tmp$pos)
        window$nSNP[i] <- sum(tmp$sign)
        window$min_pval[i] <- min(tmp$pvalues)
        window$SNP[i] <- rownames(tmp)[which.min(tmp$pvalues)]
        window$traits[i] <- names(which.max(table(factor(tmp[tmp$sign,'index']))))
      }
    }
    return(window)
  })
  windows <- do.call(rbind, windows)
  windows$lower <- windows$lower / 1e6
  windows$upper <- windows$upper / 1e6

  if (set_plot){

    white_limits <- by(data1, data1$chr, function(chr_i){
      pos_i <- sort(chr_i$pos)
      pos_diff <- diff(pos_i)
      if(max(pos_diff) > (window_size * 2)){
        wm <- which.max(pos_diff)
        pos_i[c(wm, wm + 1)]
      }
    })

    xlims <- c(min(data1$chr), max(data1$chr))
    ylims <- c(0, max(data1$pos / 1e6) * set_ylim_prop)

    graphics::plot(1, type = "n", xlab = "Chromosome", ylab = "Positions (Mbp)", xlim = xlims, ylim = ylims, bty = "n", xaxt = 'n', ...)
    graphics::axis(1, seq(xlims[1],xlims[2], 1))
    viridis <- c('#440154FF', '#482677FF', '#404788FF', '#33638DFF', '#287D8EFF', '#1F968BFF', '#29AF7FFF', '#55C667FF', '#95D840FF', '#DCE319FF', '#FDE725FF')
    colfunc <- grDevices::colorRampPalette(viridis, bias = color_bias)

    colors <- factor(windows$min_pval)
    levels(colors) <- rev(colfunc(length(levels(colors))))
    windows$colors <- as.character(colors)
    # traits shapes
    pchs <- rep(15:18, length.out = total_n_traits)
    # traits colors
    trait_cols <- rep(RColorBrewer::brewer.pal(total_n_traits, 'Dark2'), length.out = total_n_traits)
    bar_distance <- 0.2

    # notch
    notch_fun <- function(chr, notch_y,  a, ...){
      notch_x <- seq(0, 1, .1)
      notch <- function(x) 1 - (x * a) + (x^2) * abs(a)
      graphics::polygon(c(notch_x, rev(notch_x)) * bar_distance * 2 + (chr - bar_distance),
              c(notch(notch_x), - notch(notch_x)) + notch_y,
              col = 'white', ...)
    }

    for (i in xlims[1]:xlims[2]){
      x_min <- i - bar_distance
      x_max <- i + bar_distance
      y_min <- min(data1$pos[data1$chr == i]) / 1e6
      y_max <- max(data1$pos[data1$chr == i]) / 1e6
      graphics::rect(xleft = x_min, ybottom = y_min, xright = x_max, ytop = y_max, col = 'grey', border = NA)
      notch_fun(chr = i, notch_y = y_min, a = 3, border = 'white')
      notch_fun(chr = i, notch_y = y_max, a = 3, border = 'white')

      if (!is.null(white_limits[[i]])){
        tmp_lim <- white_limits[[i]] / 1e6
        graphics::rect(xleft = x_min, ybottom = tmp_lim[1], xright = x_max, ytop = tmp_lim[2], col = 'white', border = NA)
        notch_fun(chr = i, notch_y = tmp_lim[1], a = 3, border = 'white')
        notch_fun(chr = i, notch_y = tmp_lim[2], a = 3, border = 'white')
      }
      win_i <- windows[windows$chr == i,]
      if(nrow(win_i) > 0){
        for (j in 1:nrow(win_i)){
          graphics::rect(xleft = x_min, ybottom = win_i$lower[j], xright = x_max, ytop = win_i$upper[j], col = win_i$colors[j], border = NA)
          traits_i <- as.numeric(unlist(strsplit(win_i[j, 'traits'], "_")))
          traits_x_pos <- seq(x_max, x_min + 1, length.out = length(traits_i) + 2)
          traits_x_pos <- traits_x_pos[c(-1, -length(traits_x_pos))]
          for (w in traits_i) {
            graphics::points(traits_x_pos[traits_i == w], mean(c(win_i$lower[j], win_i$upper[j])), col = trait_cols[w], pch = pchs[w], cex = .6)
          }
        }
      }
    }
    # Add centromeres
    if (!is.null(centromeres)){
      if (is.character(centromeres)){
        data_c <- human_centromeres # from R/sysdata.rda
      } else {
        data_c <- centromeres
      }
      for (i in unique(data1$chr)) notch_fun(chr = data_c[i,1], notch_y = data_c[i,2], a = 3, border = 'white')
      for (i in unique(data1$chr)) graphics::points(data_c[i,1], data_c[i,2], pch = 1, cex = 0.7, col = 'red')
    }

    if(set_legend){
    # legend
    leg_x_pos <- seq(xlims[2] - total_n_traits, xlims[2] - 1, by = 1)
    leg_y_pos <- rep(ylims[2], length(leg_x_pos))

    graphics::points(leg_x_pos, leg_y_pos, pch = pchs, col = trait_cols)
    graphics::text(leg_x_pos, leg_y_pos - leg_y_pos * .05, labels = seq_len(total_n_traits), cex = .9)
    graphics::text(min(leg_x_pos) - 1, leg_y_pos[1], labels = 'Traits:', cex = .9)

    # scale
    scale_range <- range(leg_x_pos)
    scale_y_pos <- min(leg_y_pos) * .85

    scale_df <- windows[, c('colors', 'min_pval')]
    scale_df <- scale_df[order(windows$min_pval, decreasing = T),]
    scale_df$leg <- round(-log10(scale_df$min_pval))
    scale_df <- scale_df[!duplicated(scale_df$leg),]

    window_size_mb <- window_size / 1e6

    graphics::text(scale_range[1] - 1, scale_y_pos + window_size_mb * 1.5, labels = '-log10(p):', cex = .9)

    scale_x_pos <- seq(scale_range[1], scale_range[2], length.out = nrow(scale_df) + 1)
    scale_x_by <- scale_x_pos[2] - scale_x_pos[1]
    scale_text_bool <- rep(T, nrow(scale_df) + 1)

    if(nrow(scale_df) > 11){
      scale_text_bool <- rep(F, nrow(scale_df) + 1)
      scale_text_bool[c(1, 3, ceiling(length(scale_text_bool) / 2), length(scale_text_bool))] <- T
      scale_text_bool[length(scale_text_bool)] <- T
    }

    pol_height <- ylims[2] / 30

    graphics::polygon(c(0, scale_x_by, scale_x_by, 0) + scale_x_pos[1],
            c(0, 0, pol_height, pol_height) + scale_y_pos,
            col = 'grey', border = NA)

    graphics::text(scale_x_pos[1] + (scale_x_by / 2), scale_y_pos - pol_height * 1.4,
         labels = paste0('<', round(-log10(alpha))), cex = .7)

    for (i in 1:nrow(scale_df) + 1){
      graphics::polygon(c(0, scale_x_by, scale_x_by, 0) + scale_x_pos[i],
              c(0, 0, pol_height, pol_height) + scale_y_pos, col = scale_df$colors[i - 1], border = NA)
      if(scale_text_bool[i])
        graphics::text(scale_x_pos[i] + (scale_x_by / 2),
             scale_y_pos - pol_height * 1.4, labels = scale_df$leg[i - 1], cex = .7)
    }
    }
    windows$colors <- NULL
  }
  rownames(windows) <- NULL
  return(windows)
}
