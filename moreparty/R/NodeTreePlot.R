#' @importFrom rlang .data
#' @importFrom stats weighted.mean

#' @export

NodeTreePlot <- function(ct) {
  
  response <- ct$fitted[,"(response)"]
  
  if(is.numeric(response)) {
    
      rule <- list.rules.party(ct)
      rule <- as.character(rule)
      rule <- gsub("%in%", "in", rule, fixed = TRUE)
      rule <- gsub("c(", "(", rule, fixed = TRUE)
      rule <- gsub('\"', "", rule, fixed = TRUE)
      rule <- gsub("NA, ", "", rule)
      rule <- gsub(", NA", "", rule)
      rule <- sapply(rule, simplify_rule)
      rule <- factor(rule)
      
      levels(rule) <- gsub(" & ", "\n", levels(rule))
      
      df <- ct$fitted
      names(df) <- c("node", "weights", "response")
      df$node <- factor(df$node)
      
      freq <- descriptio::weighted.table(df$node, weights = df$weights)
      freq <- data.frame(node = names(freq), freq = as.numeric(freq))
      
      rule <- data.frame(node = factor(levels(df$node)), rule)
      rule <- merge(rule, freq, sort = FALSE)
      rule$rule <- paste0(rule$rule, "\n(n=", rule$freq, ")")
      df <- merge(df, rule, by = "node")
      
      moy <- sapply(split(df, df$rule), function(x) stats::weighted.mean(x$response, x$weights, na.rm = TRUE))
      sorted_levs <- names(sort(moy))
      df$rule <- factor(df$rule, levels = sorted_levs)
      
      moy <- data.frame(rule = factor(names(moy)), moy)
      df <- merge(df, moy, by = "rule")
      
      colors <- c("#009392FF","#39B185FF","#9CCB86FF","#E9E29CFF","#EEB479FF","#E88471FF","#CF597EFF")
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$rule, y = .data$response, fill = .data$moy)) +
            ggplot2::geom_boxplot() +
            ggplot2::coord_flip() +
            ggplot2::ylab(names(ct$data[1])) +
            ggplot2::scale_fill_gradientn(colours = colors) +
            ggplot2:: theme_minimal() +
            ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                           panel.grid.minor.y = ggplot2::element_blank())
      
  } else if(is.factor(response)) {
    if(nlevels(response) == 2) {
      
      rule <- list.rules.party(ct)
      rule <- as.character(rule)
      rule <- gsub("%in%", "in", rule, fixed = TRUE)
      rule <- gsub("c(", "(", rule, fixed = TRUE)
      rule <- gsub('\"', "", rule, fixed = TRUE)
      rule <- gsub("NA, ", "", rule)
      rule <- gsub(", NA", "", rule)
      rule <- sapply(rule, simplify_rule)
    
      # rule <- sapply(strwrap(rule, width=40, simplify=FALSE), paste, collapse="\n")
      rule <- gsub(" & ", "\n", rule)
      
      df <- ct$fitted
      names(df) <- c("node", "weights", "response")
      df$node <- factor(df$node)
      df$response_num <- as.numeric(df$response==levels(df$response)[2])
      
      rule <- data.frame(node = factor(levels(df$node)), rule)
      df <- merge(df, rule, by = "node", sort = FALSE)
      
      moy <- sapply(split(df, df$rule), function(x) stats::weighted.mean(x$response_num, x$weights, na.rm = TRUE))
      moy <- data.frame(rule = names(moy), moy)
      moy$freq <- as.numeric(descriptio::weighted.table(df$rule, weights = df$weights))
      moy <- moy[order(moy$moy), ]
      moy$rule <- paste0(moy$rule, "\n(n=", moy$freq, ")")
      moy$rule <- factor(moy$rule, levels = moy$rule)
      
      colors <- c("#009392FF","#39B185FF","#9CCB86FF","#E9E29CFF","#EEB479FF","#E88471FF","#CF597EFF")
      
      p <- ggplot2::ggplot(moy, ggplot2::aes(x = .data$rule, y = .data$moy, color = .data$moy)) +
            ggplot2::geom_segment(ggplot2::aes(x = .data$rule, xend = .data$rule, y = 0, yend = .data$moy)) +
            ggplot2::geom_point(size = 4) +
            ggplot2::coord_flip() +
            ggplot2::ylab(names(ct$data[1])) +
            ggplot2::scale_color_gradientn(colours = colors) +
            ggplot2::theme_minimal() +
            ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                           panel.grid.major.y = ggplot2::element_blank(),
                           panel.grid.minor.y = ggplot2::element_blank())

    } else if(nlevels(response)>2) {
      rule <- list.rules.party(ct)
      rule <- as.character(rule)
      rule <- gsub("%in%", "in", rule, fixed = TRUE)
      rule <- gsub("c(", "(", rule, fixed = TRUE)
      rule <- gsub('\"', "", rule, fixed = TRUE)
      rule <- gsub("NA, ", "", rule)
      rule <- gsub(", NA", "", rule)
      rule <- sapply(rule, simplify_rule)
      
      # rule <- sapply(strwrap(rule, width=40, simplify=FALSE), paste, collapse="\n")
      rule <- gsub(" & ", "\n", rule)
      
      df <- ct$fitted
      names(df) <- c("node", "weights", "response")
      freq <- as.numeric(descriptio::weighted.table(df$node, weights = df$weights))
      rule <- paste0(rule, "\n(n=", freq, ")")
      df$node <- factor(df$node, labels = rule)
      temp <- MASS::corresp(weights ~ node + response, nf = 1, data = df)
      df$node <- factor(df$node, levels =  names(sort(temp$rscore)))
     
      dfby <- descriptio::agg.wtd.mean(GDAtools::dichotom(df$response),
                                       by = df$node,
                                       weights = df$weights)
      names(dfby) <- levels(df$response)
      dfby <- cbind(node = rep(rownames(dfby), nlevels(df$response)), utils::stack(dfby))
      
      p <- ggplot2::ggplot(dfby, ggplot2::aes(x = .data$ind,
                                         y = .data$values,
                                         fill = .data$ind)) +
        ggplot2::geom_col() +
        ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
        ggplot2::facet_grid(~ .data$node) +
        ggplot2::xlab(names(ct$data[1])) +
        ggplot2::ylab("Percents") +
        # ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                       panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       legend.position = "none")
    }
    
  } else if(is.data.frame(response)) {
    rule <- list.rules.party(ct)
    rule <- as.character(rule)
    rule <- gsub("%in%", "in", rule, fixed = TRUE)
    rule <- gsub("c(", "(", rule, fixed = TRUE)
    rule <- gsub('\"', "", rule, fixed = TRUE)
    rule <- gsub("NA, ", "", rule)
    rule <- gsub(", NA", "", rule)
    rule <- sapply(rule, simplify_rule)
    rule <- gsub(" & ", "\n", rule)
    
    node <- ct$fitted[,"(fitted)"]
    weights <- ct$fitted[,"(weights)"]
    
    freq <- as.numeric(descriptio::weighted.table(node, weights = weights))
    rule <- paste0(rule, "\n(n=", freq, ")")
    node <- factor(node, labels = rule)
    
    plist <- list()
    for(i in 1:ncol(response)) {
      temp <- data.frame(response = response[,i], node, weights)
      if(is.numeric(response[,i])) {
        pp <- 
          ggplot2::ggplot(temp, ggplot2::aes(y = .data$response,
                                           weight = .data$weights)) +
          ggplot2::geom_boxplot() +
          ggplot2::facet_grid(~ .data$node) +
          ggplot2::ylab(names(response)[i]) +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_blank(),
                         panel.grid.minor.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
      } else if(is.factor(response[,i])) {
        tempby <- descriptio::agg.wtd.mean(GDAtools::dichotom(temp$response),
                                           by = temp$node,
                                           weights = temp$weights)
        names(tempby) <- levels(temp$response)
        tempby <- cbind(node = rep(rownames(tempby), nlevels(temp$response)),
                        utils::stack(tempby))
        pp <- ggplot2::ggplot(tempby, ggplot2::aes(x = .data$ind,
                                                y = .data$values,
                                                fill = .data$ind)) +
          ggplot2::geom_col() +
          ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
          ggplot2::facet_grid(~ .data$node) +
          ggplot2::xlab(NULL) +
          ggplot2::ylab(names(response)[i]) +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_blank(),
                         panel.grid.minor.y = ggplot2::element_blank(),
                         legend.position = "none")
      }
      if(i>1) pp <- pp + ggplot2::theme(strip.text.x = ggplot2::element_blank())
      plist[[i]] <- pp
    }
    p <- patchwork::wrap_plots(plist, ncol = 1)
  }
  
  return(p)
}
