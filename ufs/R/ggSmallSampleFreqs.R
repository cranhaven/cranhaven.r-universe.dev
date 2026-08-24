# ggSmallSampleFreqs <- function(data,
#                                category,
#                                symbol = category,
#                                color = category,
#                                width = NULL,
#                                scale_color = ggplot2::scale_color_viridis_d(),
#                                scale_shape = ggplot2::scale_shape_manual(values=c('\u26c4', '\u26f2', '\u2614'))) {
#
#   ### Select columns and rows to use
#   dat <- data[, c(category, symbol, color)];
#   dat <- dat[complete.cases(dat), ];
#
#   ### Sort by category variable
#   dat <- dat[order(dat[, category]), ];
#
#   ### Set with, if width was not yet set
#   if (is.null(width)) {
#     possibleWidths <- (1:nrow(dat))[nrow(dat) %% 1:nrow(dat) == 0];
#     sqrtWidth <- sqrt(nrow(dat));
#     width <- possibleWidths[which(abs(possibleWidths - sqrtWidth) == min(abs(possibleWidths - sqrtWidth)))];
#   }
#
#   if (!is.factor(dat[, category])) {
#     dat[, category] <- as.factor(dat[, category]);
#   }
#   if (!is.factor(dat[, symbol])) {
#     dat[, symbol] <- as.factor(dat[, symbol]);
#   }
#   if (!is.factor(dat[, color])) {
#     dat[, color] <- as.factor(dat[, color]);
#   }
#
#   xVector <- rep(1:width,
#                  each=ceiling(nrow(dat)/width))[1:nrow(dat)];
#
#   if (is.null(colors)) {
#     colors <- viridis::viridis(length(unique(dat[, color])));
#   }
#
#   res <-
#     ggplot2::ggplot(dat,
#                     ggplot2::aes_string(x='xVector',
#                                         y=1,
#                                         group=category,
#                                         color=color,
#                                         shape=symbol)) +
#     ggplot2::geom_point(position=ggplot2::position_stack(reverse=TRUE),
#                         size=10) +
#     scale_shape +
#     scale_color +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(panel.grid = ggplot2::element_blank(),
#                    axis.text = ggplot2::element_blank(),
#                    axis.title = ggplot2::element_blank()) +
#     NULL;
#
#   return(res);
#
# }
#
# ### https://jrgraphix.net/r/Unicode/2600-26FF
#
# print(ggSmallSampleFreqs(data=mtcars,
#                    category='vs',
#                    color='cyl'));
#
