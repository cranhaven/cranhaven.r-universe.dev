#' Given the immune compositions (ICs) of bulk-RNA samples, this function creates
#' a ternary plot similar to ALOT tube from EuroFlow analysis and Figure 1E of our paper.
#'
#' @param res seAMLess object.
#' @examples
#' library(seAMLess)
#'
#' data(minRes)
#' ternaryPlot(minRes)
#'
#' @return ggplot2 object
#' @export
ternaryPlot <- function(res) {

    # CRAN note check
    B.cells <- Myeloid.cells <- T.Cells <- NULL

    ics <- res$Deconvolution
    myeloid.cell.type <- c("CD14 Mono", "GMP", "LMPP",  "Early Eryth","EMP","Late Eryth","pDC",
                           "CLP","HSC", "cDC", "BaEoMa", "Prog Mk","pre-pDC" , "pre-mDC","CD16 Mono","ASDC")
    b.cells <- c( "pre B","B Cells","Plasma","pro B")

    ## Add TARGET AML P1
    data.gg <- data.frame(cbind(ics[,setdiff(colnames(ics),c(b.cells, myeloid.cell.type))],
                                B.cells = rowSums(ics[, b.cells]),
                                Myeloid.cells = rowSums(ics[, myeloid.cell.type])))


    plot.ternary <-
        ggtern::ggtern(data.gg, ggplot2::aes(x = T.Cells, y= Myeloid.cells, z= B.cells)) +
        ggplot2::geom_point(size  = 3) +
        ggplot2::labs(x = "T cells",y = "Myeloid cells",  z  = "B cells", color = "Primary Diagnosis")+
        ggplot2::theme_bw() +
        ggtern::theme_showarrows()

    return(plot.ternary)
}
