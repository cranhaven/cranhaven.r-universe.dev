ECharm_Info=function(solution="1:2"){
  inputInfo=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTRcGLaM3PyMDB0abd8r1VE0OW6TWQ5QzrINjW00oiwS_SYBXM2RIJhqw9hnAeP9ePATdix7HyDjgda/pub?output=csv")
 if(solution=="1:2") {
  df1=as.matrix(inputInfo[,2:11])
  row.names(df1)=c("Africa","N.America","Asia","Europe","LAC","NENA","Pacific")
  regioncol=c("blue","red","cyan","magenta","green","gray", "yellow")
  par(mgp=c(3,0.6,0),mar=c(5,4.5,1,1)+0.1)
  barplot(df1, las=2,beside = TRUE,xpd = F,col=regioncol,ylim=c(0,1.2),horiz = F, cex.names = 0.75,space = c(0.4, 4))
  grid (nx=NA, ny=NULL, lty = 6, col = "gray")
  legend("topleft", x.intersp=0.25,y.intersp=0,bg="transparent",text.width=11,legend = rownames(df1), fill = regioncol, box.lty = 0, cex = 0.6, horiz = TRUE)
  box(lty = 1, col = 'black')
  mtext(side=1, text="EC harmonization models", line=3.7)
  mtext(side=2, text="Performance index", line=2.5)}
  else if(solution=="1:2.5") {
    inputInfo=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRs69jpFKSO-t-iCJAL_5Y76H6N1s39XXBQcgTDx9cZ6N0chf1fI13FkxiaQYLX2n-34wEqOQSTTTTb/pub?output=csv")
    df1=as.matrix(inputInfo[,2:11])
    row.names(df1)=c("Africa","N.America","Asia","Europe","LAC","NENA","Pacific")
    regioncol=c("blue","red","cyan","magenta","green","gray", "yellow")
    par(mgp=c(3,0.6,0),mar=c(5,4.5,1,1)+0.1)
    barplot(df1, las=2,beside = TRUE,xpd = F,col=regioncol,ylim=c(0,1.2),horiz = F, cex.names = 0.75,space = c(0.4, 4))
    grid (nx=NA, ny=NULL, lty = 6, col = "gray")
    legend("topleft", x.intersp=0.25,y.intersp=0,bg="transparent",text.width=11,legend = rownames(df1), fill = regioncol, box.lty = 0, cex = 0.6, horiz = TRUE)
    box(lty = 1, col = 'black')
    mtext(side=1, text="EC harmonization models", line=3.7)
    mtext(side=2, text="Performance index", line=2.5)}
  else if(solution=="1:5") {
    inputInfo=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBDSv0JeMNwIevV4BgnhKCrsSG_EcseHxPxGCNAgMSAN1ikMg88mb7r05KCP7HEopna-J1Jkb3BjNO/pub?output=csv")
    df1=as.matrix(inputInfo[,2:22])
    row.names(df1)=c("Africa","Asia","Europe","LAC","NENA","N.America","Pacific")
    regioncol=c("blue","red","cyan","magenta","green","gray", "yellow")
    par(mgp=c(3,0.6,0),mar=c(5,4.5,1,1)+0.1)
    barplot(df1, las=2,beside = TRUE,xpd = F,col=regioncol,ylim=c(0,1.2),horiz = F, cex.names = 0.75,space = c(0.4, 4))
    grid (nx=NA, ny=NULL, lty = 6, col = "gray")
    legend("topleft", x.intersp=0.25,y.intersp=0,bg="transparent",text.width=11,legend = rownames(df1), fill = regioncol, box.lty = 0, cex = 0.6, horiz = TRUE)
    box(lty = 1, col = 'black')
    mtext(side=1, text="EC harmonization models", line=4.0)
    mtext(side=2, text="Performance index", line=2.5)}

}
