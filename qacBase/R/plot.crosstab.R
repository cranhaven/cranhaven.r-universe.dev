#' @title Plot a crosstab object
#' @description This function plots the results of a calculated two-way
#' frequency table.
#' @param x An object of class \code{crosstab}
#' @param size numeric. Size of bar text labels.
#' @param ... no currently used.
#' @return a ggplot2 graph
#' @rdname plot.crosstab
#' @import ggplot2
#' @export
#' @examples
#' tbl <- crosstab(cars74, cyl, gear, type = "freq")
#' plot(tbl)
#'
#' tbl <- crosstab(cars74, cyl, gear, type = "colpercent")
#' plot(tbl)

plot.crosstab <- function(x, size=3.5, ...) {
  if(!inherits(x, "crosstab")) stop("Object must be of type crosstab")

  title <- paste(x$rowname, " by ", x$colname)
  plotdata <- as.data.frame(x$table)

  if (x$type == "freq"){

    plotdata$lbl <- as.character(plotdata$Freq)
    plotdata$lbl <- ifelse(plotdata$lbl == "0", "", plotdata$lbl)
    rowvar <- names(plotdata)[1]
    colvar <- names(plotdata)[2]
    p <- ggplot(plotdata,
                aes_string(x=rowvar, y="Freq", fill=colvar)) +
      geom_bar(stat="identity", position="stack") +
      geom_text(aes(label=.data[["lbl"]]), size=size,
                position=position_stack(vjust=0.5)) +
      labs(y="Frequency", title=title, subtitle="cell counts")

  }

  if (x$type == "percent"){

    plotdata$lbl <- paste(round(plotdata$Freq * 100), "%", sep="")
    plotdata$lbl <- ifelse(plotdata$lbl == "0%", "", plotdata$lbl)
    rowvar <- names(plotdata)[1]
    colvar <- names(plotdata)[2]
    p <- ggplot(plotdata,
                aes_string(x=rowvar, y="Freq", fill=colvar)) +
      geom_bar(stat="identity", position="stack") +
      geom_text(aes(label=.data[["lbl"]]), size=size,
                position=position_stack(vjust=0.5)) +
      labs(y="Percent", title=title, subtitle="cell percents")

  }

  if (x$type == "rowpercent"){

    plotdata$lbl <- paste(round(plotdata$Freq * 100), "%", sep="")
    plotdata$lbl <- ifelse(plotdata$lbl == "0%", "", plotdata$lbl)
    rowvar <- names(plotdata)[1]
    colvar <- names(plotdata)[2]
    p <- ggplot(plotdata,
                aes_string(x=rowvar, y="Freq", fill=colvar)) +
      geom_bar(stat="identity", position="fill") +
      geom_text(aes(label=.data[["lbl"]]), size=size,
                position=position_stack(vjust=0.5)) +
      scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%")) +
      labs(y="Percent", title=title, subtitle="row percents") +
      coord_flip()

  }

  if (x$type == "colpercent") {

    plotdata$lbl <- paste(round(plotdata$Freq * 100), "%", sep="")
    plotdata$lbl <- ifelse(plotdata$lbl == "0%", "", plotdata$lbl)
    rowvar <- names(plotdata)[2]
    colvar <- names(plotdata)[1]
    p <- ggplot(plotdata,
                aes_string(x=rowvar, y="Freq", fill=colvar)) +
      geom_bar(stat="identity", position="fill") +
      geom_text(aes(label=.data[["lbl"]]), size=size,
                position=position_stack(vjust=0.5)) +
      scale_y_continuous(labels=c("0%", "25%", "50%", "75%", "100%")) +
      labs(y="Percent", title=title, subtitle="column percents")

  }
  if (!is.null(x$chisquare)){
    p <- p + labs(caption=x$chisquare)
  }
  return(p)
}
