#' @title Julian Conversion
#'
#' @param x vector of input dates
#' @return vector of julian days
#'
julian_conv<-function(x){
  if(is.na(x)){
    return(NA)
  }
  else{j<-julian(x, origin=as.POSIXlt(paste0(format(x,"%Y"),'-01-01')))
  temp<-unclass(j)
  return(temp[1]+1)
  }
}

#' @title arrange columns
#'
#' @param data input dataframe
#' @param tomove which column(s) to move
#' @param where where to move them - e.g. "before", "after", "first", "last"
#' @param ba ??
#' @return Dataframe with new column order
#'
moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

#' @title label plots
#'
#' @param object leaflet plot
#' @param text text
#' @param color text color
#' @param fontSize font size
#' @param fontFamily font
#' @param leftPosition left position
#' @param topPosition top position
#' @return title on plots
#'
addTitle <- function(object,
                    text,
                    color = "black",
                    fontSize = "20px",
                    fontFamily = "Sans",
                    leftPosition = 50,
                    topPosition = 2){

  htmlwidgets::onRender(object, paste0("
                                       function(el,x){
                                       h1 = document.createElement('h1');
                                       h1.innerHTML = '", text ,"';
                                       h1.id='titleh1';
                                       h1.style.color = '", color ,"';
                                       h1.style.fontSize = '",fontSize,"';
                                       h1.style.fontFamily='",fontFamily,"';
                                       h1.style.position = 'fixed';
                                       h1.style['-webkit-transform']='translateX(-50%)';
                                       h1.style.left='",leftPosition ,"%';
                                       h1.style.top='",topPosition,"%';
                                       document.body.appendChild(h1);
                                       }"))
}

utils::globalVariables(c("AID", "clus_summary", "ClusID", "consec_locs", "dat", "Lat", "Long"))
