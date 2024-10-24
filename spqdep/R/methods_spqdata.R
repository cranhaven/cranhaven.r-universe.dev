#' @name methods_spqtest
#' @title Methods for class spqtest
#' @description The \code{plot()} function allows the user obtain the plot of relative frequency
#' of each symbol (standard and equivalent) with the confidence interval.
#'  The \code{print()} function is used to get a list with the values of Q test
#'  for standard symbolization based on permutations and for equivalent symbolization
#'  based on combinations. \code{summary()} print a table with the output of the Q test.
#'
#' @param x a \code{spqtest} object created by \code{\link{Q.test}}.
#' @param ci confidence level for the intervals in \code{plot} method.
#'   Default \code{ci = 0.95}
#' @param ... further arguments passed to or from other methods
#' @return This functions does not return any value
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz, M., López, F., and Páez, A. (2021).
#'     A test for global and local homogeneity of categorical data based on spatial runs.
#'       \emph{working paper}.
#'   }
#' @export
#'
plot.spqtest <- function(x, ...,ci = 0.95) {
  z <- x
  if (!inherits(z, "spqtest")) stop("Argument must be a spqtest object")
  alpha_div_2 <- (1-ci)/2
  critval <- qnorm(alpha_div_2, lower.tail = FALSE)
  lplot_symb <- vector('list', length(z))
  for (i in 1:length(z)) {
    zi <- z[[i]]
    R <- zi$R
    if (zi$type == "standard-permutations") {
      qsymb <- zi$qp_symb
      efsymb <- zi$efp_symb
      Symb <- zi$PSymb
      efsymb_mc <- zi$efp_symb_mc
    } else if (zi$type == "equivalent-combinations") {
      qsymb <- zi$qc_symb
      efsymb <- zi$efc_symb
      Symb <- zi$CSymb
      efsymb_mc <- zi$efc_symb_mc
    } else stop("type must be standard-permutations or equivalent-combinations")
    # Expected frequency of standard permutation symbols under the null
    # REPASAR AQUÍ... NO ME CUADRAN LOS INTERVALOS BOOTSTRAP
    sf0 <- R * qsymb
    lb_int <- rep(0, length = length(sf0))
    ub_int <- rep(0, length = length(sf0))
    names(lb_int) <- names(ub_int) <- names(efsymb)
     if (zi$distr == "asymptotic") {
      sep_symb <- critval*sqrt((sf0/R * (1 - sf0/R))/R)
      lb_int <- sf0/R - sep_symb
      ub_int <- sf0/R + sep_symb
    } else if (zi$distr == "mc") {
      for (j in 1:length(sf0)) {
        lb_int[j] <- quantile(efsymb_mc[j,],
                                alpha_div_2)
        ub_int[j] <- quantile(efsymb_mc[j,],
                               1 - alpha_div_2)
        }
      lb_int <- lb_int / R
      ub_int <- ub_int / R
    } else stop("distribution must be asymptotic or mc")
    # # Determine if empirical frequency is outside of intervals of
    # # confidence
    sigp_symb <- (-1 * (efsymb/R < lb_int) +
                    1 * (efsymb/R > ub_int))
    sigp_symb <- factor(sigp_symb)
    if (any(sigp_symb == -1)) {
      levels(sigp_symb)[levels(sigp_symb) == "-1"] <- "sig -"
    }
    if (any(sigp_symb == 0)) {
      levels(sigp_symb)[levels(sigp_symb) == "0"] <- "non-sig"
    }
    if (any(sigp_symb == 1)){
      levels(sigp_symb)[levels(sigp_symb) == "1"] <- "sig +"
    }
    sigp_symb <- factor(sigp_symb, levels = c("sig -","non-sig","sig +"))
    # Dataframe for plotting
    efsymbR <- efsymb/R
    sf0R <- sf0/R
    Symb.df <- data.frame(Symb, efsymbR, sf0R,
                          lb_int, ub_int, sigp_symb)
    colnames(Symb.df) <- c("Symb", "efsymbR",
                           "sf0R", "lb_int",
                           "ub_int", "sigp_symb")
    # # Create ggplot2 plot object
    if (zi$distr == "asymptotic") {
      subt <- paste("m:", zi$m," r: ", zi$r,
                    " distance: ", zi$distance,
                    " intervals: ", zi$distr)
    } else if (zi$distr == "mc") {
      subt <- paste("m:", zi$m,
                    " distance: ", zi$distance,
                    " intervals: ", zi$distr)
    } else stop("Distribution must be asymptotic or mc")
    lplot_symb[[i]] <- ggplot(Symb.df) +
      geom_bar(aes(x = Symb, y = efsymbR,
                            fill = sigp_symb),
                        stat = "identity",
                        color = "black")  +
      scale_fill_manual(values = c("sig -" = "blue",
                                            "non-sig" = "grey77",
                                            "sig +" = "red")) +
      geom_errorbar(aes(x = Symb, y = sf0R,
                                 ymin = lb_int,
                                 ymax = ub_int,
                                 color = "black"),
                             width = 0.5) +
      labs(title = paste("Symbolization for variable:",
                                  zi$var.name),
           subtitle = subt,
            x = paste("Symbol:", zi$type), y = "Frequency",
           color = "Significance") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0,
                                        size = 6),
                      legend.position = "bottom")
  }
  # if (viewplot
  #     ) lapply(lplot_symb, print)
  lplot_symb
}

# # Como no tengo coordenadas uso igraph
# if (inherits(lsrq$listw), "knn")){
#   W <- nb2mat(knn2nb(lsrq$listw))
#   W <- (W>0)*1
# }
# if (inherits(lsrq$listw, "nb")){
#   W <- nb2mat(lsrq$listw)
#   W <- (W>0)*1
# }
# if (inherits(lsrq$listw[1], "matrix")){
#   W <- (lsrq$listw>0)*1
# }
# g1 = igraph::graph.adjacency(W)
# plot(g1, axes =TRUE,vertex.size=6,vertex.label="",edge.color='black',edge.arrow.mode=0)


# if (!is.null(sf)){
#   if (is.null(lsrq$nsim)){
#     # if (sum(is.na(lsrq$SRQlocal$`z-value`))==0){
#     a <- as.factor((lsrq$SRQlocal$p.menor < sig)*1+(lsrq$SRQlocal$p.mayor < sig)*2)
#     sf$Signif <- addNA(a)
#     mylevel <- levels(sf$Signif)
#     mylevel[mylevel==0]="non-sig"
#     mylevel[mylevel==1]="sig +"
#     mylevel[mylevel==2]="sig -"
#     mylevel[mylevel=="NA"]="NA"
#     mycolor[a==0]="gray"
#     mycolor[a==1]="red"
#     mycolor[a==2]="blue"
#     sf$Signif <- mycolor
#     levels(sf$Signif) <- mylevel
#     ggplot(sf) +
#     geom_sf(aes(fill = Signif, color = Signif), size=3,color = mycolor) +
#     theme_bw() +
#     theme(axis.text.x=element_blank(),axis.text.y=element_blank()) +
#     xlab(paste0("Significance p-value = ", sig)) +
#     scale_fill_manual(values = c("blue","gray"))
#
#     # }
#   }}
