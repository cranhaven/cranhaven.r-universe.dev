#' VLDA Plot
#'
#'
#' Assists in producing a plot that more effectively expresses changes over time for two different types (long format and wide format) using a consistent calling scheme for longitudinal data. It provides the ability to projection supplementary information (supplementary objects and variables) that can often occur in longitudinal data to graphs, as well as provides a new interactive implementation to perform the additional interpretation, so it is also useful for longitudinal data visuals analysis.
#' 
#' 
#' Coordinates in opposite directions on each axis can be considered to be different groups. And if the distance between the coordinates is close, it indicates that the group has a similar tendency. Even if the explanatory variable is not significant, a small tendency can confirm because the coordinate is placed in consideration of the relative influence.
#'
#' @param fit An object returned by \code{vlda()} or \code{supplement()}
#' @param rename Rename a variable to another name
#' @param interactive Use the interactive graphical elements (default TRUE)
#' @param title Plot title. If NULL, the title is not shown (default NULL)
#' @param title.col Title color (default color is black)
#' @param title.size Title font size (default size = 15)
#' @param title.hjust Alignment of title (Number from 0 (left) to 1 (right): \code{left-aligned by default})
#' @param subtitle Subtitle for the plot which will be displayed below the title
#' @param sub.col Sub-title color (default color is black)
#' @param sub.size Sub-title font size (default size = 15)
#' @param sub.hjust Alignment of sub-title (Number from 0 (left) to 1 (right): \code{left-aligned by default})
#' @param labels Legend labels
#' @param lab.col Legend labels color
#' @param lab.size Legend labels size
#' @param lab.face Legend labels font c(\code{"plain", "bold", "italic", "bold.italic"}) default = "\code{plain}"
#' @param legend.position The position of legends (\code{"none", "left", "right", "bottom", "top"}, or two-element numeric vector) default is "\code{bottom}"
#' @param legend.justification Anchor point for positioning legend inside plot ("center" or two-element numeric vector) or the justification according to the plot area when positioned outside the plot
#' @param linetype Line types can be specified with: An integer or name: 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash, as shown below:
#' @param line.col Axis line color
#' @param font.size Font size (left-aligned by default size = 1.0)
#' @param var.size Variable coordinate point size of plot
#' @param obs.col Observation coordinate point color of plot
#' @param obs.size Observation coordinate point size on plot
#' @param add.obs.col Color of added observation coordinate points
#' @param arrow.col Arrow color (default color = "orange")
#' @param arrow.size Arrow size (default size = 0.5)
#' @param arrow.type One of "\code{open}" or "\code{closed}" indicating whether the arrow head should be a closed triangle
#'
#' @usage
#' vlda_plot(fit, rename = NULL, interactive = TRUE,
#'           title = NULL, title.col = NULL, title.size = 15, title.hjust = 0,
#'           subtitle = NULL, sub.col = NULL, sub.size = 15, sub.hjust = 0,
#'           labels = NULL, lab.col = NULL, lab.size = NULL, lab.face = NULL,
#'           legend.position = "bottom", legend.justification = NULL,
#'           linetype = 2, line.col = "red", font.size = 1.0, var.size = 2.5,
#'           obs.col = "darkgray", obs.size = 2.5, add.obs.col = "#666666",
#'           arrow.col = "orange", arrow.size = 0.5, arrow.type = "closed")
#'
#' @return
#'    \item{...}{Same as the result of vlda}
#'    \item{graphics}{As a result of vlda, it creates a two-dimensional graph. provides interactive graphics, so when the mouse cursor points to the observation coordinates, it provides a tooltip that displays observations of having the same coordinates and displays the row and column coordinate. In the case of long-form, the tooltip displays a time point, besides, coordinate having the same time point are filled with the yellow color on the graph, to make it easier to distinguish the same time points of observations with colors. In the case of a wide form, the combinations that the explanatory variables can have are grouped and the coordinates points of the corresponding observations are shown in yellow on the graph. changes in time points are indicated by orange arrows on the graph.}
#' @seealso \code{vlda}
#' @keywords Visualzation Plot
#' @examples
#' ### Long form ###
#' data(PTSD)
#' PTSD[,2:4] <- apply(PTSD[,2:4], 2, function(x) ifelse(x >= 3, 1, 0))
#' PTSD[,5] <-  ifelse(PTSD[,5] >= 6 , 1, 0)
#' PTSD <- data.frame(lapply(PTSD, function(x) as.factor(x)))
#' PTSD
#' str(PTSD)
#' head(PTSD, 10)
#' fit <- vlda(x = PTSD, object = "subject", time = "time", type = "long")
#' vlda_plot(fit)
#'
#'  ## row and column ##
#' data(PTSD_row)
#' data(PTSD_column)
#' PTSD_row <- as.matrix(PTSD_row)
#' PTSD_column <- as.matrix(PTSD_column)
#'
#' fit2 <- vlda_add(fit, add.row = PTSD_row, add.col = PTSD_column)
#' vlda_plot(fit2)
#'
#'
#' ### Wide form ###
#' data(Depression)
#' wide.fit <-
#' vlda(
#'   x = Depression,
#'   object = "Case",
#'   time = c("1week", "2weeks", "4weeks"),
#'   type = "wide"
#' )
#' vlda_plot(wide.fit)
#'
#'
#'
#' @import ggrepel
#' @import ggplot2
#' @import dplyr
#' @import ggiraph
#' @import ggsci
#' @importFrom utils combn globalVariables
#' @export vlda_plot
#'
vlda_plot = function(fit, rename = NULL, interactive = TRUE,
                     title = NULL, title.col = NULL, title.size = 15, title.hjust = 0,
                     subtitle = NULL, sub.col = NULL, sub.size = 15, sub.hjust = 0,
                     labels = NULL, lab.col = NULL, lab.size = NULL, lab.face = NULL,
                     legend.position = "bottom", legend.justification = NULL,
                     linetype = 2, line.col = "red", font.size = 1.0, var.size = 2.5,
                     obs.col = "darkgray", obs.size = 2.5, add.obs.col = "#666666",
                     arrow.col = "orange", arrow.size = 0.5, arrow.type = "closed"){


  obs.coordinate <- fit$obs.coordinate
  var.coordinate <- fit$var.coordinate
  svd.G <- fit$svd.G
  x <- fit$x
  object <- fit$object
  time <- fit$time
  type <- fit$type
  Eigen <- fit$Eigen
  GOF <- fit$GOF
  Ags <- fit$Ags
  ev.add.wide <- fit$ev.add.wide
  LAB <- fit$LAB




  nf <- length(table(x[time[1]]))

  tmp <- sapply( strsplit(rownames(var.coordinate), "\\."), function(x) x[1] )

  index.i <- lapply( unique(tmp), function(x) which(tmp %in% x) )

  names(index.i) <- unique(tmp)

  if( type == "long"  ){

    out.arrow.time <- index.a <- as.list(1 : (nf - 1))

    nchar.time <- nchar(time)

    wh.a <-

      substring(rownames(var.coordinate), 1, nchar.time) %>%

      {. == time } %>%

      which


    combn.a <-

      wh.a %>%

      as.factor %>%

      as.numeric %>%

      combn(2)

    index.a <-

      as.list(as.data.frame(combn.a[, apply(combn.a, 2, function(x) diff(x, 1)) == 1])) %>%

      lapply(function(index) wh.a[index])

    out.arrow.aaa <- as.list(1:length(index.a))

    count <- 0

    for (aaa in index.a) {

      count <- count + 1

      x0 <- aaa[1]

      x1 <- aaa[2]

      out.arrow.aaa[[count]] <-

        sapply(list(var.coordinate[x0, 1], var.coordinate[x0, 2], var.coordinate[x1, 1], var.coordinate[x1, 2]), as.numeric)

    }

    out.arrow.time <- do.call("rbind", out.arrow.aaa)

    arrow_coordinate <- data.frame(out.arrow.time)

  } else if( type == "wide" ) {

    out.arrow.time <- index.a <- as.list(1:nf)

    for ( timetime in 1 : nf ) {

      wh.a <- sapply(index.i[time], function(xx) xx[timetime])

      combn.a <-

        wh.a %>%

        as.factor %>%

        as.numeric %>%

        combn(2)

      index.a[[timetime]] <-

        as.list(as.data.frame(combn.a[, apply(combn.a, 2, function(x) diff(x, 1)) == 1])) %>%

        lapply(function(index) wh.a[index])

      out.arrow.aaa <- as.list(1:length(index.a))

      count <- 0
      for (aaa in index.a[[timetime]]) {

        count <- count + 1

        x0 <- aaa[1]

        x1 <- aaa[1] + length(table(aaa))

        out.arrow.aaa[[count]] <-

          sapply(list(var.coordinate[x0, 1], var.coordinate[x0, 2], var.coordinate[x1, 1], var.coordinate[x1, 2]), as.numeric)

      }

      out.arrow.time[[timetime]] <- do.call("rbind", out.arrow.aaa)

    }

    arrow_coordinate <- data.frame( do.call( "rbind", out.arrow.time ) )

  }


  tf.time <- rep(names(index.i), sapply( index.i, length )) %in% tmp[wh.a]

  labels.shape <- ifelse(tf.time, "Time", rep( names(index.i), sapply( index.i, length ) ))



  V <- data.frame(V = round(var.coordinate, 3), labels.shape)

  colnames(V) <- c("x", "y", "labels.shape")


  if (!is.null(rename)) {

    V$raw.name <- rownames(V)

    rownames(V) <- rename

  }

  V$var_tooltip <- paste0( rownames(V), "<br>x=", V$x, ", y=", V$y )



  suppressWarnings({

    p <- obs.coordinate

    i <- 1

    for ( i in 1 : nrow(p)){

      p$list[i] <-  paste0( p$obs_list[[i]], collapse = " " )

      p$list2[i] <- ifelse(length(p$obs_list[[i]]) >= 5,

                           paste0( paste( p$obs_list[[i]][1:5], collapse = " " ), "..." ),

                           paste0( p$obs_list[[i]], collapse = " " )  )


      if( type == "long"){

        p$obs_time[i] <- paste( time, "=", unique(sapply(strsplit(p$obs_list[[i]], "_"), function(x) x[2])), collapse = " " )

        p$obs_tooltip <- paste0( "Obs Group [", rownames(p), "]", "<br>", object, " (", p$obs_time, ")<br>", p$list2, "<br>x = ", p$x, ", y = ", p$y )

        }
      }
      if ( type == "wide" ){

        ## raw data ##
        wide.obs <- NULL
        for (i in 1:(length(p$obs_list))){
          wide.obs <- rbind(wide.obs, lapply(p$obs_list, function(x) x[1])[[i]])
        }
        ev <- x[-which(colnames(x) %in% c(object,time))]

        ev.wide <- ev[wide.obs,]

        ## supplement data ##
        if(!is.null(Ags)){
        ev.add.wide <- fit$ev.add.wide

        addobs <- NULL
        for(i in 1:(nrow(Ags))) {

          addobs <- rbind(addobs, lapply(Ags$obs_list, function(x) x[1])[[i]])

        }

        Ags$obs_time <- GetOrder(ev.add.wide[addobs,])[,3] + (GetOrder(ev.wide)[3] %>% unique %>% max)
        }



        p$obs_time <- GetOrder(ev.wide)[,3]

        p$obs_tooltip <- paste0( "Obs Group [", rownames(p), "]", "<br>", object, "<br>", p$list2, "<br>x = ", p$x, ", y = ", p$y )
      }

  })




  g <- ggplot(data = V, aes(x = x, y = y, color = labels.shape, tooltip = var_tooltip, data_id = labels.shape) ) +

    geom_point_interactive(size = var.size) +

    geom_point_interactive(data = p, aes(x = x, y = y, color = obs_time, tooltip = obs_tooltip, data_id = obs_time ),

                           size = obs.size, shape = 16, color = obs.col ) +

    guides(colour = guide_legend(labels) ) +

    geom_text_repel(aes(x = x, y = y, label = rownames(V)), size=5*font.size, show.legend = FALSE ) +

    xlab(LAB[1]) + ylab(LAB[2]) +

    geom_vline(xintercept = 0 , color = line.col, linetype = linetype ) +

    geom_hline(yintercept = 0 , color = line.col, linetype = linetype ) +

    geom_segment_interactive(data = arrow_coordinate, aes(x = arrow_coordinate[,1], y = arrow_coordinate[,2],

                                                          xend = arrow_coordinate[,3], yend = arrow_coordinate[,4], tooltip = NULL, data_id = NULL ),

                             arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = arrow.type ),

                             size = arrow.size, color = arrow.col ) +

    ggtitle(title, subtitle = subtitle) +

    theme_bw(base_size = 15 * font.size) +

    theme(plot.title = element_text(size = title.size, color = title.col, hjust = title.hjust),

          plot.subtitle = element_text(size = sub.size, color = sub.col, hjust = sub.hjust),

          legend.position = legend.position, legend.justification = legend.justification,

          legend.title = element_text(colour = lab.col, size = lab.size, face = lab.face)) +

    scale_color_d3()

  if (!is.null(Ags) == TRUE) {

    ifelse(add.obs.col == "equal", add.obs.col <- obs.col, add.obs.col)

    g <- g + geom_point_interactive(data = as.data.frame(Ags),
                                    aes(x = x, y = y, color = obs_time, tooltip = new_tooltip, data_id = obs_time),
                                    size = obs.size, color = add.obs.col)
  }

  if (interactive == TRUE){

    tooltip_css <- "background-color:lightgray;font-style:italic;padding:20px;border-radius:10px 20px 10px 20px;"

    hover_css <- "fill:orange;r:4px;cursor:pointer"

    viewer <- ggiraph(code = {print(g)}, width = 0.8, width_svg = 6, height_svg = 6, tooltip_extra_css = tooltip_css, hover_css = hover_css)


  } else if (interactive == FALSE) {

    viewer <- g

  }

  return(viewer)

}




























