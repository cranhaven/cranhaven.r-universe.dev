# Plot ==========================================
setGeneric("ggvenn", function(venn = "Venn", slice = "all",
                              fill = c("gold", "dodgerblue3", "deeppink"),
                              alpha = 0.5, thickness = 1.5) {
  standardGeneric("ggvenn")
}
)

#' @export
#' @importFrom rlang .data
#' @rdname ggvenn
setMethod("ggvenn", c(venn = "Venn",
                      slice = "ANY",
                      fill = "ANY",
                      alpha = "ANY",
                      thickness = "ANY"),
          function(venn,
                   slice = "all",
                   fill = c("gold", "dodgerblue3", "deeppink"),
                   alpha = 0.5,
                   thickness = 1.5) {

            if (slice[1] == "all") {
              input = venn@sets
            } else {
              input = venn@sets[slice]
            }

            if (!(length(input) %in% c(2, 3))) {
              stop("Number of sets should be 2 or 3.")
            }

            if (!(0 <= alpha & alpha <= 0.5)) {
              stop("alpha should be between 0 and 1.")
            }

            if (length(input) == 3) {
              setA = input[[1]]
              setB = input[[2]]
              setC = input[[3]]

              lenAB = setA %>% intersect(setB) %>% setdiff(setC) %>% length
              lenBC = setB %>% intersect(setC) %>% setdiff(setA) %>% length
              lenAC = setA %>% intersect(setC) %>% setdiff(setB) %>% length
              lenABC = setA %>% intersect(intersect(setB, setC)) %>% length

              lenA = setA %>% setdiff(union(setB, setC)) %>% length
              lenB = setB %>% setdiff(union(setA, setC)) %>% length
              lenC = setC %>% setdiff(union(setA, setB)) %>% length

              df3 = data.frame(
                x = c(0.75, 2, 3.25),
                y = c(2, 0.25, 2),
                r = c(2, 2, 2),
                s = names(input)
              )

              ggplot2::ggplot() +
                ggforce::geom_circle(ggplot2::aes(x0 = .data$x, y0 = .data$y, r = .data$r, fill = .data$s),
                                     data = df3, alpha = alpha, size = thickness) +
                ggplot2:: scale_fill_manual(values = fill) +
                ggforce::geom_circle(ggplot2::aes(x0 = .data$x, y0 = .data$y, r = .data$r),
                                     data = df3, size = thickness) +
                ggplot2::annotate(geom = "text",
                                  x = c(0, 1, 2, 2, 2, 3, 4),
                                  y = c(2.5, 1, -0.5, 1.5, 2.75, 1, 2.5),
                                  label = c(lenA, lenAB, lenB, lenABC, lenAC, lenBC, lenC),
                                  size = 10) +
                ggplot2::annotate(geom = "text",
                                  x = c(-0.5, 2, 4.5),
                                  y = c(4, -2, 4),
                                  label = df3$s,
                                  size = 10) +
                ggplot2::theme(legend.position = "none",
                               axis.title = ggplot2::element_blank(),
                               axis.line = ggplot2::element_blank(),
                               axis.ticks = ggplot2::element_blank(),
                               axis.text = ggplot2::element_blank()) +
                ggplot2::coord_fixed()
            } else if (length(input) == 2) {
              setA = input[[1]]
              setB = input[[2]]

              lenI = intersect(setA, setB) %>% length
              lenA = setA %>% setdiff(setB) %>% length
              lenB = setB %>% setdiff(setA) %>% length

              df2 = data.frame(
                x = c(0.75, 3.25),
                y = c(2, 2),
                r = c(2, 2),
                s = names(input)
              )

              ggplot2::ggplot() +
                ggforce::geom_circle(ggplot2::aes(x0 = .data$x, y0 = .data$y, r = .data$r, fill = .data$s),
                                     data = df2, alpha = alpha, size = thickness) +
                ggplot2::scale_fill_manual(values = fill) +
                ggforce::geom_circle(ggplot2::aes(x0 = .data$x, y0 = .data$y, r = .data$r),
                                     data = df2, size = thickness) +
                ggplot2::annotate(geom = "text",
                                  x = c(0, 2, 4),
                                  y = c(2, 2, 2),
                                  label = c(lenA, lenI, lenB),
                                  size = 10) +
                ggplot2::annotate(geom = "text",
                                  x = c(-0.5, 4.5),
                                  y = c(4, 4),
                                  label = df2$s,
                                  size = 10) +
                ggplot2::theme(legend.position = "none",
                               axis.title = ggplot2::element_blank(),
                               axis.line = ggplot2::element_blank(),
                               axis.ticks = ggplot2::element_blank(),
                               axis.text = ggplot2::element_blank()) +
                ggplot2::coord_fixed()
            }
          }
)

