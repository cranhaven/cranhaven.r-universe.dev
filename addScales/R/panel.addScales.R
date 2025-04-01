panel.addScales <- function( scaleline = c(0, 0),
                             scaleType = c("line", "region"),
                             ndig.midline = c(h = 2, v = 2),
                             col.midline = "red",
                             adj.midline = c(-.1, -.25),
                             midline.aes = list(lwd =
                                    if(colCode == "n") 1.5 else 3),
                             midline.label.aes = list(
                                col = if(colCode == "n") col.midline
                                   else "black",
                                fontface = "bold", cex = .8),
                             scaleline.aes = list(lty = "dashed",
                                                  col = "purple"),
                             region.aes = list(fill = "tan",
                                               alpha = .2),
                             colCode = c("n","m","r"),
                             palette = hcl.colors(n = 100, "Viridis"),
                             ...)

{
    scaleType <- match.arg(scaleType)
    ## filter and adjust ndig.midline argument
    stopifnot(
        0 < length(ndig.midline),
        3 > length(ndig.midline),
        all(is.numeric(ndig.midline)),
        all(is.finite(ndig.midline))
    )
    ndig.midline <- round(ndig.midline,0)
    ndig.midline[ndig.midline < 1 | ndig.midline >15] <- 2L
    if(length(ndig.midline) == 1) ndig.midline <- rep(ndig.midline,2)
    ## ignore colCode argument if not of right form
    colCode <- match.arg(colCode)
    if(any(is.na(colCode) | !colCode %in% c("m","r"))) colCode <- "n"
    ## set up the variable names for calls
    hv <- c("h","v")
    cpl <- current.panel.limits()[2:1]
    names(cpl) <- hv
    names(ndig.midline) <- hv
    lims <- lapply(list(...)[["all.panel.limits"]], function(x){
                if(length(x) == 1 | !is.numeric(x[[1]])) NA
                else range(sapply(x,mean))
            })
    ####### use default aesthetic components if not explicitly given
    add.defaults <- function(list1,list2,to.what){
        list1 <- as.list(list1)[-1] ## convert call to list
        needed <- setdiff(names(list1),names(list2))
        if(length(needed)){
            list2 <- c(list2, list1[needed])
            assign(to.what,list2, inherits = TRUE)
        }
        NULL
    }
    wh <- c("midline.aes", "midline.label.aes", "scaleline.aes", "region.aes")
    mapply(add.defaults,formals()[wh], mget(wh), wh, SIMPLIFY = FALSE)
    ###################   filter scaleline argument
    stopifnot(length(scaleline) > 0, length(scaleline) < 3,
              is.numeric(scaleline), is.finite(scaleline))
    scaleline[is.na(scaleline)] <- 0 ## replace NA's by 0
    if(length(scaleline) == 1) scaleline <- c(scaleline,0)
    if(all(!scaleline)) stop("All scaleline values are 0")
    scaleline <- abs(scaleline)
    names(scaleline) <- hv
    ########################  build argument lists
    lapply(hv, function(x){
        if(!(scaleline[x])) return(NULL) else{
            ml <- list(mean(cpl[[x]]))
            sl <- list(ml[[1]] + c(-1,1)*scaleline[x])
            if(x == "h"){
                corners <- as.list(c(sl[[1]],cpl[[2]]))
            } else corners <- as.list(c(cpl[[1]],sl[[1]]))
            names(corners) <- c("ybottom","ytop","xleft","xright")
            names(sl) <- names(ml) <- x
            ndig <- ndig.midline[x]
            ml.lab <- zapsmall(signif(c(ml[[x]], sl[[x]]),digits = ndig),
                               digits = ndig)[1] ## just the midline

            ############# build panel components
            lmx <- lims[[x]]
            if((colCode != "n" ) && length(lmx) == 2){
                thiscol <- palette[max(1,round(length(palette)*(ml[[1]] - lmx[1])/diff(lmx)))]
                col.midline <- thiscol
                if(colCode == "r") region.aes$fill <- thiscol
            }
            if(scaleType == "line"){
                do.call(panel.refline,c(sl, scaleline.aes))
            } else {
                do.call(panel.rect,c(corners, region.aes))
            }
            do.call(panel.refline, c(ml, col.line = col.midline, midline.aes))
           ## get text location
            if(x == "h"){
                xloc <- cpl[["v"]][1]
                yloc <- ml[[1]]
            } else{
                xloc <- ml[[1]]
                yloc <- cpl[["h"]][1]
            }
            do.call(panel.text,
                    c(list(
                        x = xloc, y = yloc,
                        label = ml.lab,
                        adj = adj.midline),
                        midline.label.aes))
        }})
    invisible(NULL)
}
