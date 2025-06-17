#' Function Wulffplot
Wulffplot <-
function(x){
    data<-x$Wulffdat
    print("Wulffnet")
    par(omi=c(0,0,0,2))
    Wulffnet()
    Wulffpoint(data$sgrad,data$esgrad ,pch=23,col="red",bg=data$bg)
    Wulffpoint(data$sgrad,data$esgrad2,pch=23,col="red",bg=data$bg)
    Wulffpoint(data$sgrad,data$esexp  ,pch=3,col="blue")
    Wulffpoint(data$sgrad,data$esexp2 ,pch=3,col="blue")
    Wulffpoint(data$A1phi,data$A1theta,pch=16,col="blue",lab="OA1")
    Wulffpoint(data$A2phi,data$A2theta,pch=16,col="blue",lab="OA2")
    Wulffpoint(data$ABphi,data$ABtheta,pch=16,col="green",lab="AB")
    Wulffpoint(data$ONphi,data$ONtheta,pch=16,col="green",lab="ON")
    Wulffpoint(data$OBphi,data$OBtheta,pch=16,col="green",lab="OB")
   legend(1,1,c("obs. Extinction (S<180 deg)","obs. Extinction (S>180 deg)","calc. Extinction","Optical Axes", "Principal Axes"),pch=c(23,23,3,16,16),col=c("red","red","blue","blue","green"),pt.bg=c("white","red","white", "white","white"),xpd=NA)
}

