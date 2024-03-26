#' Comparing sample L-moment ratios with L-spaces of various distributions on the L-moments ratio diagram
#'
#' @param sample for a single site, sample is a vector of observations, e.x. FLOW_AMAX. For multiple sites, sample is a dataframe consisting of multiple columns where each column has the data observed at one site; this dataframe should have column names as station names, e.x. FLOW_AMAX_MULT.
#' @param type the type of the sample. It can be "s" for single site, the default, or "m" for multiple sites.
#' @param Dist select the distribution to plot its L-space in the background. This can be "BrIII" for Burr Typr-III distribution, "BrXII" for Burr Typr-XII distribution, or "GG" for Generalized Gamma distribution. The default Dist is "BrIII".
#' @param color color of the L-point/s, default is "red".
#' @param shape shape of the L-point/s, default is 8.
#'
#' @return ggplot plot comparing sample/s L-point/s with L-space of a distribution on the L-moment ratio diagram
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#'
#' @examples
#' 
#' com_plot_BrIII <- com_sam_lspace(LMoFit::FLOW_AMAX, type = "s", Dist = "BrIII")
#' com_plot_BrXII <- com_sam_lspace(LMoFit::FLOW_AMAX, type = "s", Dist = "BrXII")
#' com_plot_GG <- com_sam_lspace(LMoFit::FLOW_AMAX, type = "s", Dist = "GG")
#' com_plot_BrIII <- com_sam_lspace(LMoFit::FLOW_AMAX_MULT, type = "m", Dist = "BrIII")
#' com_plot_BrXII <- com_sam_lspace(LMoFit::FLOW_AMAX_MULT, type = "m", Dist = "BrXII")
#' com_plot_GG <- com_sam_lspace(LMoFit::FLOW_AMAX_MULT, type = "m", Dist = "GG")
#' 
com_sam_lspace <- function(sample, type = "m", Dist = "BrIII", color = "red", shape = 8) {
  if (type == "s") { #condition of single site
    #sample <- FLOW_AMAX
    slmom <- get_sample_lmom(x = sample)
    if (Dist == "BrIII") {
      com_plot <- LMoFit::lspace_BrIII + geom_point(data = slmom, aes(x = st2, y = st3), color = color, shape = shape)
    } else if (Dist == "BrXII") {
      com_plot <- LMoFit::lspace_BrXII + geom_point(data = slmom, aes(x = st2, y = st3), color = color, shape = shape)
    } else if (Dist == "GG") {
      com_plot <- LMoFit::lspace_GG + geom_point(data = slmom, aes(x = st2, y = st3), color = color, shape = shape)
    } else {
      stop(paste0("make sure to select one of the three distributions that are ", "'BrIII'" , " 'BrXII'", " or 'GG'"))
    }
  } else if (type == "m") { #condition of multiple sites
    #sample <- FLOW_AMAX_MULT
    slmom <- NA
    for (i in 1:ncol(sample)) {
      #i <- 1
      st_i <- data.frame(st_i = t(get_sample_lmom(x = sample[,i])))
      slmom <- cbind(slmom, st_i)
    }
    slmom <- slmom[,2:ncol(slmom)]
    colnames(slmom) <- colnames(sample[,1:ncol(sample)])
    slmom <- data.frame(t(slmom))
    if (Dist == "BrIII") {
      com_plot <- LMoFit::lspace_BrIII + geom_point(data = slmom, aes(x = st2, y = st3), color = color, shape = shape)
    } else if (Dist == "BrXII") {
      com_plot <- LMoFit::lspace_BrXII + geom_point(data = slmom, aes(x = st2, y = st3), color = color, shape = shape)
    } else if (Dist == "GG") {
      com_plot <- LMoFit::lspace_GG + geom_point(data = slmom, aes(x = st2, y = st3), color = color, shape = shape)
    } else {
      stop(paste0("make sure to select one of the three distributions that are ", "'BrIII'" , " 'BrXII'", " or 'GG'"))
    }
  } else {
    stop(paste0("make sure to select one of the two types of samples ", "'s'" , " or 'm'"))
  }
  return(com_plot)
}
