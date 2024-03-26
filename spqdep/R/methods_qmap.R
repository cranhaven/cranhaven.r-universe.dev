#' @name methods_qmap
#' @title Method for class qmap
#' @description A function to plot the difference in frequencies of symbols of each map.
#'  The \code{plot()} function to obtain the plot.
#'  The argument \code{ci} select the confidence level. \cr
#' @param x object of class \emph{qmap}
#' @param ci confidence level for the difference of probabilities of symbols in `plot` method.
#'   Default \code{ci = 0.95}
#' @param ... further arguments passed to or from other methods.
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#' @references
#'   \itemize{
#'     \item Ruiz M, López FA and A Páez (2011).
#'     Comparison of Thematic Maps Using Symbolic Entropy.
#'       \emph{International Journal of Geographical Information Science},  26, 413-439.
#'     \item Ruiz, M., López, F., and Páez, A. (2021).
#'     A test for global and local homogeneity of categorical data based on spatial runs.
#'       \emph{working paper}.
#'   }
#' @examples
#' # Example 1:
#' N <- 100
#' cx <- runif(N)
#' cy <- runif(N)
#' coor <- cbind(cx,cy)
#' p <- c(1/6,3/6,2/6)
#' rho = 0.5
#' listw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(cbind(cx,cy), k = 4)))
#' fx <- dgp.spq(list = listw, p = p, rho = rho)
#' q.test <- Q.test(fx = fx, coor = coor, m = 3, r = 1)
#' plot(q.test)

NULL

#' @export
plot.qmap <- function(x,ci = 0.95,...){
  qmap <- x
  alpha_div_2 <- (1-ci)/2
  critval <- qnorm(alpha_div_2, lower.tail = FALSE)
  R <- sum(qmap[[1]]$nsk[,2])
  ps <- qmap[[1]]$nsk[,1]/R
  qs <- qmap[[1]]$nsk[,2]/R
  e <- critval*((ps*(1-ps)+qs*(1-qs))/R)^.5
  lb_int <- 0 - e
  ub_int <- 0 + e
  fr <- (qmap[[1]]$nsk[,1]-qmap[[1]]$nsk[,2])/R
  sigp_symb <- (-1 * (fr < lb_int) +
                  1 * (fr > ub_int))
  sigp_symb <- factor(sigp_symb, levels = c("-1","0","1"))
  if (any(sigp_symb == -1)) {
    levels(sigp_symb)[levels(sigp_symb) == "-1"] <- "sig -"
  }
  if (any(sigp_symb == 0)) {
    levels(sigp_symb)[levels(sigp_symb) == "0"] <- "non-sig"
  }
  if (any(sigp_symb == 1)){
    levels(sigp_symb)[levels(sigp_symb) == "1"] <- "sig +"
  }


  data <- data.frame(symbols = apply(qmap[[1]]$symb,1,function(x){paste0(x,collapse = "")}),
                     fr = fr, lb_int = 0-e ,ub_int = 0+e)
  data$sigp_symb = factor(sigp_symb, levels=c("sig -", "non-sig", "sig +"))
  ggplot(data, aes(x = symbols, y = fr,
                            fill = sigp_symb)) +
    geom_bar(color = "black",
                      stat = "identity",
                      position="stack") +
    theme_bw() +
    scale_fill_manual(values = c("sig -" = "blue",
                                          "non sig" = "grey77",
                                          "sig +" = "red")) +
    geom_errorbar(aes(x = symbols, y = fr,
                      ymin = lb_int,
                      ymax = ub_int,
                      color = "red"),width = 0.2) +
    labs(title = paste("Bar charts of differences in symbol frequency"),
         subtitle = paste(qmap[[1]]$data.name),
         x = paste("Symbols", ""), y = "Difference in frequency Map",
         color = "Significance") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0,
                                     size = 6),
          legend.position = "bottom")
}




