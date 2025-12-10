#' Title Plotting to show the quality controlled data in environmental space.
#'
#' @param qcdata \code{dataframe} Data output from quality controlled function \code{\link{extract_clean_data}} and \code{\link{classify_data}}.
#' @param xvar \code{string} The variable to be on the x-axis.
#' @param yvar \code{string} The variable to be on the y-axis.
#' @param type \code{string} Its \code{1D}, \code{2D} for a two dimensional ggplot2 graph or \code{3D} for a 3-dimensional graph for multivariate data.
#' @param zvar \code{string} The variable to be on the z-axis only if the 3D plot type is selected..
#' @param labelvar \code{string} Column name in the quality controlled data that has the labels. This applies is the 3D plot is selected.
#' @param xlab,ylab,zlab \code{string} x-axis, y-axis, and z-axis label.
#' @param ncol,nrow \code{integer} If number of groups are greater than 1, then number of rows and columns can be set.
#'      Check ggplot2 facet parameters on how the columns are set.
#' @param scalecolor \code{string} The scale color themes supported are grey, manual, viridis. If \code{manual} is selected, then the
#'        \code{colorvalues} should be provided for the different colors for each data label.
#' @param colorvalues If \code{manual} is selected, then the
#'        \code{colorvalues} should be provided for the different colors for each data label. If 3D is selected and \code{colorvalues} is not
#'        \code{auto}, then colors should determined.
#' @param legend_position \code{string} Its either \code{bottom}, \code{top} or \code{inside}. If the \code{inside} is selected then the vector
#'        with graph coordinates should be provided to avoid the legend overlap with the graph contents.
#' @param legend_inside \code{vector} If the \code{inside} for legend position is selected then the vector
#'        with graph coordinates should be provided to avoid the legend overlap with the graph contents.
#' @param pointsize \code{decimal} The size of the points.
#' @param themebackground \code{string} Either \code{classic}, \code{bw} or \code{gray} to set the plot theme. This is based on ggplot2.
#' @param fontsize \code{integer} Indicates the sizes of fonts for the whole graph.
#' @param legtitle \code{string} Either \code{blank} or \code{TRUE} to set the legend title for the 2D plot.
#' @param ggxangle \code{integer} Indicates the angle of the x-axis text. The dafualt is 45 but depend on the data.
#' @param xhjust \code{numeric} Indicates the distance of the x-axis text from the x-axis line in a vertical direction.
#' @param xvjust \code{numeric} Indicates the distance of the x-axis text from the x-axis line in a horizontal direction.
#' @param main \code{string} Plot title
#' @param pch \code{string} Either \code{auto}: the point characters will be automatically set or different pch are set.
#' @param lpos3d \code{string} Indicates the legend position for the 3D graph. bottom, left, and right are accepted.
#' @param cexsym \code{numeric} The size of pch in the 3D plot.
#'
#' @return If "2D" or "1D" is the selected type, then a ggplot2 graph will be the output and a "3D" type will return a scatterplot3D plot.
#'
#' @export
#'
ggenvironmentalspace <- function(qcdata,
                                 xvar = NULL,
                                 yvar = NULL,
                                 zvar = NULL,
                                 labelvar = NULL,
                                 type = '2D',
                                 xlab = NULL,
                                 ylab = NULL,
                                 zlab = NULL,
                                 ncol = 2,
                                 nrow = 2,
                                 scalecolor = 'viridis',
                                 colorvalues = 'auto', #manual color values for ggplot2
                                 legend_position = 'right',
                                 legend_inside = NULL,
                                 pointsize = 1,
                                 themebackground ='bw',
                                 fontsize = 13,
                                 legtitle = 'blank',
                                 ggxangle = 1,
                                 xhjust = 0.5,
                                 xvjust = 1,
                                 main = NULL,
                                 pch = 'auto',
                                 lpos3d = 'left',
                                 cexsym= NULL
){

  # xc <- c(xvar, yvar)%in%colnames(qcdata)
  #
  # vardd <- c(xvar, yvar)[which(xc==FALSE)]
  #
  # if(length(which(xc==FALSE)>=1))stop('The variable indicated ', paste(vardd, collapse =' and '), ' are/is not in the quality controlled dataset ', deparse(substitute(qcdata)))

  ngroups <- length(unique(qcdata$groups))

  if(ngroups>20)warning('The facets may not appear properly as the groups are greater than 20.')

  if(!is.null(scalecolor) && scalecolor=='manual'){

    if(length(unique(qcdata$label))!= length(colorvalues)) stop('The of colors set should be equal to ', length(colorvalues))
  }

  #make sure type is 1d, 2d, 3d are accepted
  type <- toupper(type)

  if(type=="1D"){

    check_packages(pkgs = c("ggplot2"))

    #check if number of columns and rows equal to groups in the dataset

    totfacets <-  ncol*nrow

    if(totfacets<ngroups)stop("The total number of ncol and nrow are not enough to enable facets. Adjust ncol and nrow to a equal a product of ", ngroups, ".")

    label <- NULL

    plt <- ggplot2::ggplot(qcdata, ggplot2::aes(x= label, fill = label))+

      ggplot2::geom_bar()+

      {if(themebackground=='classic'){

        ggplot2::theme_classic()

      }else if(themebackground=='gray'){

        ggplot2::theme_gray()
      }else{
        ggplot2::theme_bw() +

          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank())
      }
      }+

      {if(ngroups>1) ggplot2::facet_wrap(~groups, scales = 'free', ncol = ncol, nrow = nrow)}+

      {if(scalecolor=='gray'){

        ggplot2::scale_fill_grey()

      }else if(scalecolor=='manual'){

        ggplot2::scale_fill_manual(values = colorvalues)

      } else{
        ggplot2::scale_fill_viridis_d(alpha = 1, direction = -1)
      }
      }+

      {if(legend_position=='bottom'){

        ggplot2::theme(legend.position = 'bottom')

      }else if(legend_position=='top'){

        ggplot2::theme(legend.position = 'top')

      }else if(legend_position=='blank'){

        ggplot2::theme(legend.position = 'none')

      }else if(legend_position=='inside') {

        ggplot2::theme(legend.position = 'inside',

                       legend.position.inside = legend_inside)
      }else{
        ggplot2::theme(legend.position = 'right')
      }
      }+

      ggplot2::theme(legend.background = ggplot2::element_blank())+

      ggplot2::theme(text = ggplot2::element_text(size = fontsize),
                     axis.text.x = ggplot2::element_text(angle = ggxangle,
                                                         hjust = xhjust,
                                                         vjust = xvjust))+

      {if(legtitle=='blank')ggplot2::theme(legend.title = ggplot2::element_blank())}+

      {if(is.null(xlab))ggplot2::xlab(label = xvar) else ggplot2::labs(x = xlab)}+

      {if(is.null(ylab))ggplot2::ylab(label = yvar) else ggplot2::labs(y = ylab)}+

      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+

      ggplot2::ggtitle(label = main)

    print(plt)

  }else if(type=='2D'){

    check_packages(pkgs = c("ggplot2"))

    #check if number of columns and rows equal to groups in the dataset

    totfacets <-  ncol*nrow

    if(totfacets<ngroups)stop("The total number of ncol and nrow are not enough to enable facets. Adjust ncol and nrow to a equal a product of ", ngroups, ".")

    label <- NULL

    plt <- ggplot2::ggplot(qcdata, ggplot2::aes(x= .data[[xvar]], y= .data[[yvar]], colour = label, shape = label))+

      ggplot2::geom_point(size = pointsize)+

      {if(themebackground=='classic'){

        ggplot2::theme_classic()

      }else if(themebackground=='gray'){

        ggplot2::theme_gray()
      }else{
        ggplot2::theme_bw() +

          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank())
      }
      }+

      {if(ngroups>1) ggplot2::facet_wrap(~groups, scales = 'free', ncol = ncol, nrow = nrow)}+

      {if(scalecolor=='gray'){

        ggplot2::scale_color_grey()

      }else if(scalecolor=='manual'){

        ggplot2::scale_color_manual(values = colorvalues)

      } else{
        ggplot2::scale_colour_viridis_d(alpha = 1, direction = -1)
      }
      }+

      {if(legend_position=='bottom'){
        ggplot2::theme(legend.position = 'bottom')
      }else if(legend_position=='top'){
        ggplot2::theme(legend.position = 'top')
      }else if(legend_position=='inside') {
        ggplot2::theme(legend.position = 'inside',
                       legend.position.inside = legend_inside)
      }else{
        ggplot2::theme(legend.position = 'right')
        }
        }+

      ggplot2::theme(legend.background = ggplot2::element_blank())+

      ggplot2::theme(text = ggplot2::element_text(size = fontsize),
                     axis.text.x = ggplot2::element_text(angle = ggxangle,
                                                         hjust = xhjust,
                                                         vjust = xvjust))+

      {if(legtitle=='blank')ggplot2::theme(legend.title = ggplot2::element_blank())}+

      {if(is.null(xlab))ggplot2::xlab(label = xvar) else ggplot2::labs(x = xlab)}+

      {if(is.null(ylab))ggplot2::ylab(label = yvar) else ggplot2::labs(y = ylab)}+

      ggplot2::ggtitle(label = main)

    print(plt)
}else if(type=="3D"){

  check_packages(pkgs = "scatterplot3d")

  if(is.null(zvar)) stop("For 3D plot provide a numeric z parameter.")

  if(is.null(labelvar)) stop("For 3D plot provide a labelvar name, which is the column name with data labels.")

  x1 <- qcdata[[xvar]]
  y1 <- qcdata[[yvar]]
  z1 <- qcdata[[zvar]]
  labels1 <- qcdata[[labelvar]]

  #check if x, y, z are numeric

  tf <- sapply(list(x1, y1, z1), is.numeric)

  if(all(tf==TRUE)==FALSE) stop("All xvar, yvar, and zvar parameters should be numeric")

  #change the pch
  if(length(pch)==1 && pch=='auto'){

    pp <- as.numeric(as.factor(unique(labels1)))

    ppd <- as.numeric(as.factor(labels1))

  }  else {
    pp <- pch

    tbv <- table(as.numeric(as.factor(labels1)))

    ppd <- rep(pch, tbv)
  }

  if(length(colorvalues)==1 && colorvalues=='auto'){

    cpp <- as.numeric(as.factor(unique(labels1)))

    cc <- as.numeric(as.factor(labels1))
  }else{

    cpp <- colorvalues

    #cc  <- colorvalues[as.factor(labels1)]

    tbv <- table(as.numeric(as.factor(labels1)))

    cc <- rep(cpp, tbv)
  }


  s3plot <- scatterplot3d::scatterplot3d(x    = x1,
                                         y    = y1,
                                         z    = z1,
                                         color= cc,
                                         pch  =   ppd,
                                         xlab = xlab,
                                         ylab = ylab,
                                         zlab = zlab,
                                         main = main,
                                         cex.symbols = cexsym)
  # Add a legend
  graphics::legend(x = lpos3d,
                   legend = unique(labels1),
                   y.intersp = 1.3,
                   x.intersp = 1.2,
                   bty = "n",
                   cex = 1,
                   col = cpp,
                   pch = pp)
}else{
  stop('Please for dimensions set either 2D or 3D')
}
}
utils::globalVariables(".data")
