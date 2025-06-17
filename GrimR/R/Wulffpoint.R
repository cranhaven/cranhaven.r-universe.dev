#' Function Wulffpoint
Wulffpoint <-
function(ES,S, col = 2, pch = 5, bg = "white", lab = ""){
    if (missing(col)) {
        col = "red"
    }
    if (missing(pch)) {
        pch = 5
    }
    if (missing(lab)) {
        lab = ""
    }

    phi<-ES*pi/180
    theta<-S*pi/180
    x<- cos(theta)/(1+sin(phi)*sin(theta))
    y<- cos(phi)*sin(theta)/(1+sin(phi)*sin(theta))
    points(x,y,pch=pch,col=col, bg=bg)
    if (!missing(lab)) {
        text(x, y,  lab, pos = 4)
    }
    return(list(x=x,y=y))
}
