# ordEval.R
# 
# visualization of ordEval algorithm and data preparation for it
#
# Author: rmarko
###############################################################################
oeInst<-function(ord, noAttr, graphTitle = "", normalization=TRUE, instSelection=NULL, bw=FALSE)
{
    noStats = 8
    noMethods = 3
    noInst = length(ord)
    xInit <- c(0, 0)
    yInit <- c(1, noAttr+0.85)   
    ylabName = "" ## "attributes"
    subtitleName <- ""#"   impact"
    boxHeight = 1.0
    chExp = 1.0  ## char expansion for boxes
    if (is.null(instSelection))
        instSelection<-1:min(noInst,100)
    if (bw) {
        downColor <- gray(0.7)
        downOverColor <- gray(0.9)
        upColor <- gray(0.5)
        upOverColor <- gray(0.3)        
    }
    else {
        downColor <- "blue"
        downOverColor <- "lightblue"
        upColor <- "red"
        upOverColor <- "orange"
    }
    
    for (inst in instSelection){    
        plot(xInit, yInit, type = "n", xlim = c(-1.2, 1), ylim = c(0.9, noAttr+0.9), xlab="",
                ylab = ylabName, axes = FALSE)
        par(fig=c(0.2/2.2,1,0,1),new=TRUE) ## to make more space for labels on left
        mtext(text="downward      upward",side=1,line=2,adj=c(0.5,0.5))
        lines(xInit,yInit)
        ## plot title
        if (graphTitle=="")
            graphTitle <- "Impact on instance"
        subtitleName <- paste("   ", ord[[inst]]$className,"=",ord[[inst]]$classValue,sep="")
        title(main=graphTitle, sub=subtitleName)
        ## x axis
        axis(1, at = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(1, 0.8, 0.6, 0.4, 0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),cex.axis=0.8)
        ## left y axis, attribute values    
        leftLabel <- c()
        for(iA in 1:noAttr) {
            leftLabel <- c(leftLabel, paste(ord[[inst]]$attributeName[[iA]],"=",ord[[inst]]$valueName[[iA]],sep=""))
        }
        axis(2, at = boxHeight/4+c(1:noAttr), labels = leftLabel, las = 1, cex.axis = 0.6)
        
        for(iA in 1:noAttr) {
            xDown <-  - ord[[inst]]$reinfPos[iA]
            xUp <- ord[[inst]]$reinfNeg[iA]
            y <- iA
            if (xDown<0) {
                rect(xDown, y, 0.0, y+0.45*boxHeight, col=downColor)
            }  
            if (xUp>0) {
                rect(0.0, y, xUp, y+0.45*boxHeight, col=upColor)
            }     
            if (normalization) {
              ## box and whiskers for random down 
              boxwhiskers(ord[[inst]]$rndReinfNeg[[iA]], y+0.50*boxHeight, y+0.70*boxHeight)
              ## box and whiskers for random up
              boxwhiskers(-ord[[inst]]$rndReinfPos[[iA]], y+0.50*boxHeight, y+0.70*boxHeight)
          }
        }
    }
    invisible()
}


avNormBarObject<-function(oe, ciType=c("two.sided","upper","lower","none"), ciDisplay=c("box","color"),
        ciDecorate = NULL,
        graphTitle = NULL, ylabLeft = "attribute values", ylabRight="number of values" ,
        xlabel="reinforcement", attrIdx=0, equalUpDown=FALSE, colors=c("green","lightgreen","blue","lightblue")) 
{
    ciType<-match.arg(ciType)
    ciDisplay<-match.arg(ciDisplay)
    
    if (is.null(colors)) { # black and white
        downColor <- gray(0.7)
        downOverColor <- gray(0.9)
        upColor <- gray(0.5)
        upOverColor <- gray(0.3)        
    }
    else {
        downColor <- colors[1]
        downOverColor <- colors[2]
        upColor <- colors[3]
        upOverColor <- colors[4]
    }
    noStats <- length(getStatNames())
    
    if (is.null(ylabLeft))
        ylab <-"" 
    else  ylab <- ylabLeft
    if (is.null(xlabel)) {
        xlab <-""
        subtitleName <- ""        
    }
    else {
        subtitleName <- xlabel
        if (equalUpDown)
            xlab <- "decrease to       increase to"
        else
            xlab <- "downward        upward"
    }
    # if equalUpDown=TRUE upward and downward reinforcements are shown on the same level
    if (equalUpDown)
        equalUD=1
    else
        equalUD=0
    boxHeight <- 1.0
    chExp <- 1.0  ## char expansion for boxes
    ## for(iA in c(1,3,5,7)) {
    attrSelection <- 1:oe$noAttr
    if (attrIdx!=0)
        attrSelection<-c(attrIdx)
    for(iA in attrSelection) {
        noAttrValues <- length(oe$valueNames[[iA]])
        x <- c(0, 0)
        y <- c(1, noAttrValues)
		par(xpd=NA,mar=c(5.5,7,5,7))
        plot(x, y, type = "l", xlim = c(-1, 1), ylim = c(0.9, noAttrValues-equalUD+0.9), xlab = xlab,
                ylab = ylab, axes = FALSE)
        ## plot title
        if (is.null(graphTitle))
            titleName <- paste("", gsub('_', ' ', oe$attrNames[iA]))
        else if (graphTitle == "")
            titleName <- ""
        else
            titleName <-paste(gsub('_', ' ', oe$attrNames[iA]),"\neffect on ", graphTitle)
        title(main=titleName, sub=subtitleName)
        
        ## bottom x axis
        axis(1, at = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(1, 0.8, 0.6, 0.4, 0.2, 0, 0.2, 0.4, 0.6, 0.8, 1))
        
        # prepare left and right axis labels
        if (equalUpDown) {
            # left and right contain values
            leftLabels <- oe$valueNames[[iA]][-length(oe$valueNames[[iA]])]
            rightLabels <- oe$valueNames[[iA]][-1]
        }
        else {
            # values on the left, number of values on the right-hand side 
            leftLabels <- oe$valueNames[[iA]]
            rightLabels <- oe$noAV[iA,]        
        }
        las <- 1 ## horizontal
        cex.axis <- 1
        maxAVchars <- max(nchar(leftLabels))
        if (maxAVchars > 3) {
            las <- 0
            if (maxAVchars > 9)
                cex.axis <- 0.5    
        }   
        ## left y axis, attribute values    
        axis(2, at = boxHeight/4+c(1:(noAttrValues-equalUD)), labels = leftLabels, las = las, line = 1, cex.axis=cex.axis)
        
        ## right y axis, number of instances or attribute values
        axis(4, at = boxHeight/4+c(1:(noAttrValues-equalUD)), line = -0.8, labels = rightLabels, las = las)
        if (!is.null(ylabRight) && (equalUpDown==FALSE || (equalUpDown==TRUE  && ylabRight!="number of values"))) {
            mtext(ylabRight, side=4, line = 1.35,cex=1.0)
        }
        
        for(i in 1:(noAttrValues-equalUD)) {
            xUp <-   oe$reinfNegAV[iA,i]
            statsUp <- oe$rndReinfNegAV[iA,i,]
            xDown <-  oe$reinfPosAV[iA,i+equalUD]
            statsDown <- oe$rndReinfPosAV[iA,i+equalUD,]
            y <- i

            if (xDown>0) {
                rect(-xDown, y, 0.0, y+0.45*boxHeight, col=downColor)
            } 
            ## box and whiskesrs 
            if (statsDown[["highPercentile"]] > 0) {
                if (ciType=="lower")
                    statsDown[["highPercentile"]] <- 1
                if (ciType=="upper")
                    statsDown[["lowPercentile"]] <- 0
                if (ciDisplay == "box")
                   boxwhiskers(-statsDown, y+0.50*boxHeight, y+0.70*boxHeight, ciType)
                else if (ciType != "none") {
                    # change the color within upper limit of confidence interval 
                    rect(max(-xDown, -statsDown[["highPercentile"]]), y, 0.0, y+0.45*boxHeight, col=downOverColor)                    
                }  
                if (!is.null(ciDecorate) && abs(statsDown[["highPercentile"]]) < abs(xDown) ) {
                  segments(-statsDown[["highPercentile"]], y - 0.10,  -statsDown[["highPercentile"]], y + 0.55, lty="dashed")
                  draw.ellipse(x = -(statsDown[["highPercentile"]]+xDown)/2, y = y+0.225*boxHeight, a = max(0.05, 0.05+(xDown - statsDown[["highPercentile"]])/2), b= 0.05 + 0.225*boxHeight, border=ciDecorate, lwd=2)
                }
                
            }
            if (xUp>0) {
                rect(0.0, y, xUp, y+0.45*boxHeight, col=upColor)
             }                
            ## box and whiskesrs 
            if (statsUp[["highPercentile"]] > 0){
                if (ciType=="lower")
                    statsUp[["highPercentile"]]<-1
                if (ciType=="upper")
                    statsUp[["lowPercentile"]]<-0              
                if (ciDisplay == "box"){
                    boxwhiskers(statsUp, y+0.50*boxHeight, y+0.70*boxHeight,ciType)
                }
                else if (ciType != "none"){
                    # change the color within upper limit of confidence interval 
                    rect(min(xUp, statsUp[["highPercentile"]]), y, 0.0, y+0.45*boxHeight, col=upOverColor)                    
                }   
                if (!is.null(ciDecorate) && abs(statsUp[["highPercentile"]]) < abs(xUp) ) {
                  segments(statsUp[["highPercentile"]], y - 0.10,  statsUp[["highPercentile"]], y + 0.55, lty="dashed")
                  draw.ellipse(x = (statsUp[["highPercentile"]]+xUp)/2, y = y+0.225*boxHeight, a = max(0.05, 0.05+(xUp - statsUp[["highPercentile"]])/2), b = 0.05 + 0.225*boxHeight, border=ciDecorate, lwd = 2)
                }
            }
        }
        par(lwd = 1)
    }
    invisible()
}




avSlopeObject<- function(oe, ciType=c("two.sided","upper","lower","none"),attrIdx=0, graphTitle=NULL, xlabel = "attribute values", colors=c("green","lightgreen","blue","lightblue"))
{
    ciType<-match.arg(ciType)
    
    noAttr <- oe$noAttr
    ordVal <- oe$ordVal
    
    if (is.null(colors)) {
        downColor <- "black"
        downOverColor <- gray(0.9)
        upColor <- "black"
        upOverColor <- gray(0.3)     
    }
    else {
        downColor <- colors[1]
        downOverColor <- colors[2]
        upColor <- colors[3]
        upOverColor <- colors[4]
    }
	ciColor = gray(0.9)
	
    yU<-matrix(nrow=noAttr,ncol=ordVal)
    yUlow<-matrix(nrow=noAttr,ncol=ordVal)
    yUhigh<-matrix(nrow=noAttr,ncol=ordVal)
    xU <- c(1:ordVal)
    yD<-matrix(nrow=noAttr,ncol=ordVal)
    yDlow<-matrix(nrow=noAttr,ncol=ordVal)
    yDhigh<-matrix(nrow=noAttr,ncol=ordVal)
    xD <- c(1:ordVal)
    for(iA in 1:noAttr) {
        yU[iA,1] <- 0
        for(i in 2:ordVal) {
            ySlope <- oe$reinfPosAV[iA,i]
            stats <- oe$rndReinfPosAV[iA,i,]
            
            ySlopeLow <- stats[["lowPercentile"]]
            ySlopeHigh <- stats[["highPercentile"]]            
            
            yU[iA,i] <- yU[iA,i-1] + ySlope
            yUlow[iA,i] <- yU[iA,i-1] + ySlopeLow
            yUhigh[iA,i] <- yU[iA,i-1] + ySlopeHigh            
        }
        yD[iA,ordVal] <- 0
        for(i in (ordVal-1):1) {
            ySlope <- oe$reinfNegAV[iA,i]
            stats <- oe$rndReinfNegAV[iA,i,]

            ySlopeLow <- stats[["lowPercentile"]]
            ySlopeHigh <- stats[["highPercentile"]]                        
            
            yD[iA,i] <- yD[iA,i+1] - ySlope
            yDlow[iA,i] <- yD[iA,i+1] - ySlopeLow
            yDhigh[iA,i] <- yD[iA,i+1] - ySlopeHigh            
        }
    }
    yLimit <- c(min(yD,yU)-0.1, max(yU,yD) + 0.1)
    attrSelection <- 1:noAttr
    if (attrIdx!=0)
        attrSelection<-c(attrIdx)
    if (length(xlabel)==1) ## expand a single label to vector
        xlabel[2:length(attrSelection)] <- xlabel[1]
    
    for(iA in attrSelection) {
        par(lwd = 1)
        x <- c(0.8, ordVal+0.2)
        y <- c(0, 0)
        plot(x, y, xlim = c(1, ordVal), ylim = yLimit, xlab = "", ylab = "cumulative reinforcement", type = "n", axes = FALSE)
        
        if (is.null(graphTitle))
            titleName <- paste("", gsub('_', ' ', oe$attrNames[iA]))
        else if (graphTitle == "")
            titleName <- ""
        else
            titleName <-paste(gsub('_', ' ', oe$attrNames[iA]),"\neffect on ", graphTitle)
        title(main=titleName)

        text((ordVal+1)/2.0, yLimit[1]-0.1, label = xlabel[iA], adj=c(0.5,1),xpd=TRUE)
        lines(x, y, lwd = 2)
        x <- c(1:ordVal)
        y <- rep(-0.09, ordVal)
        av <- oe$valueNames[[iA]]
        text(x, y, label = av, adj = c(0.5,1))
        axis(2)
        lines(xU, yU[iA,], type = "o", pch = 15, col = upColor)
        lines(xD, yD[iA,], type = "o", pch = 15, col = downColor)
        for(i in 1:(ordVal - 1)) {

            if (ciType=="lower")
              polygon(c(xU[i], xU[i+1],xU[i+1]), c(yU[iA,i], yUlow[iA,i+1],yU[iA,i]), col = ciColor, border=NA)
            if (ciType=="upper")
              polygon(c(xU[i], xU[i+1],xU[i+1]), c(yU[iA,i], yUhigh[iA,i+1],yU[iA,i]), col = ciColor, border=NA)
            if (ciType=="two.sided")
                polygon(c(xU[i], xU[i+1],xU[i+1]), c(yU[iA,i], yUhigh[iA,i+1],yUlow[iA,i+1]), col = ciColor, border=NA)

            arrows(xU[i], yU[iA,i], xU[i+1], yU[iA,i+1], col = upColor, angle=9, length=0.12)
                       
            
            if (ciType=="lower")
                polygon(c(xD[i+1], xD[i],xD[i]), c(yD[iA,i+1], yD[iA,i+1],yDlow[iA,i]), col = ciColor, border=NA)
            if (ciType=="upper")
               polygon(c(xD[i+1], xD[i],xD[i]), c(yD[iA,i+1], yD[iA,i+1],yDhigh[iA,i]), col = ciColor, border=NA)
            if (ciType=="two.sided")
               polygon(c(xD[i+1], xD[i],xD[i]), c(yD[iA,i+1], yDhigh[iA,i],yDlow[iA,i]), col = ciColor, border=NA)
            
            arrows(xD[i + 1], yD[iA,i+1], xD[i], yD[iA,i], col = downColor, angle=9, length=0.12)
                    
        }
    }
    invisible()
    }


attrNormBarObject<-function(oe, graphTitle = "OrdEval for all attributes", 
        ciType=c("two.sided","upper","lower","none"),ciDisplay=c("box","color"), ciDecorate=NULL, colors=c("green","lightgreen","blue","lightblue"))
{
    ciType = match.arg(ciType)
    ciDisplay=match.arg(ciDisplay)
    noAttr <- oe$noAttr
    ordVal <- oe$ordVal
    noStats <- length(getStatNames())
    if (is.null(colors)) {
        downColor <- gray(0.7)
        downOverColor <- gray(0.9)
        upColor <- gray(0.5)
        upOverColor <- gray(0.3)        
    }
    else {
        downColor <- colors[1]
        downOverColor <- colors[2]
        upColor <- colors[3]
        upOverColor <- colors[4]
    }
    
    boxHeight <- 1.0
    chExp <- 1.0  ## char expansion for boxes
    
    x <- c(0, 0)
    y <- c(1, noAttr+0.85)   
    ylabName <- "" ## "attributes"
	par(xpd=NA,mgp=c(3,0.7,0),mar=c(5,12,4,1))
	plot(x, y, type = "l", xlim = c(-1, 1), ylim = c(0.9, noAttr+0.9), xlab = "downward            upward     ",
            ylab = ylabName, axes = FALSE)
    ## plot title
    subtitleName <- "reinforcement"
    title(main=graphTitle, sub=subtitleName)
    ## x axis
    axis(1, at = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), labels = c(1,0.8, 0.6, 0.4, 0.2, 0, 0.2, 0.4, 0.6, 0.8, 1))
    
	## left y axis, attributes
    attrSelection <- 1:noAttr
    attrNames <- oe$attrNames
    noAV <- oe$noAVattr
#     for(iA in attrSelection) {
#         if (nchar(attrNames[[iA]],type="chars") > 7) {
#             attrNames[[iA]] <-  paste(strsplit(attrNames[[iA]],split="_",fixed=TRUE)[[1]],sep="",collapse="\n")
#         }
# 	}
	cex.axisA = 1.0    
	maxCharsA = max(nchar(attrNames[attrSelection]))
	if (maxCharsA > 15) {
		 cex.axisA = 0.9    
		 if (maxCharsA > 20)
			 cex.axisA = 0.6    
	 }	
    axis(2, at = boxHeight/4+c(1:noAttr), labels = attrNames, las = 1, line = 1, cex.axis = cex.axisA)
    ## right y axis, number of instances
    #axis(4, at = boxHeight/4+c(1:noAttr), line = -0.8, labels = noAV, las = 1, cex.axis = 0.7)
    #axisLabel4 <- "number of values" 
    #mtext(axisLabel4, side=4, line = 1.3)
    
    for(iA in attrSelection) {
        xUp <-   oe$reinfNegAttr[iA]
        xDown <- -oe$reinfPosAttr[iA]
        y <- iA
        if (xDown<0) {
            rect(xDown, y, 0.0, y+0.45*boxHeight, col=downColor)
        }  
        ## box and whiskesrs for random down 
        if (oe$rndReinfNegAttr[iA,"highPercentile"] > 0) {
            stats <- oe$rndReinfNegAttr[iA,]
            if (ciType=="lower")
                stats[["highPercentile"]]<-1
            if (ciType=="upper")
                stats[["lowPercentile"]]<-0
            if (ciDisplay == "box"){                
              boxwhiskers(stats, y+0.50*boxHeight, y+0.70*boxHeight, ciType)
            }
            else if (ciType != "none") {
                # change the color within upper limit of confidence interval 
                rect(max(xDown, -stats[["highPercentile"]]), y, 0.0, y+0.45*boxHeight, col=downOverColor)                    
            }  
            if (!is.null(ciDecorate) && abs(stats[["highPercentile"]]) < abs(xDown) ) {
              segments(-stats[["highPercentile"]], y - 0.10,  -stats[["highPercentile"]], y + 0.55, lty="dashed")
              draw.ellipse(x = (-stats[["highPercentile"]]+xDown)/2, y = y+0.225*boxHeight, a = max(0.05, 0.05+(-xDown - stats[["highPercentile"]])/2), b= 0.05 + 0.225*boxHeight, border=ciDecorate, lwd=2)
            }
            
         }
        if (xUp>0) {
            rect(0.0, y, xUp, y+0.45*boxHeight, col=upColor)
         }     
        ## box and whiskesrs for random up
        if (oe$rndReinfPosAttr[iA,"highPercentile"] > 0){
            stats <- oe$rndReinfPosAttr[iA,]
            if (ciType=="lower")
                stats[["highPercentile"]]<-1
            if (ciType=="upper")
                stats[["lowPercentile"]]<-0
            
            if (ciDisplay == "box"){              
               boxwhiskers(-stats, y+0.50*boxHeight, y+0.70*boxHeight, ciType)
            }
            else if (ciType != "none") {
                # change the color within upper limit of confidence interval 
                rect(min(xUp, stats[["highPercentile"]]), y, 0.0, y+0.45*boxHeight, col=upOverColor)                    
            }      
            if (!is.null(ciDecorate) && abs(stats[["highPercentile"]]) < abs(xUp) ) {
              segments(stats[["highPercentile"]], y - 0.10,  stats[["highPercentile"]], y + 0.55, lty="dashed")
              draw.ellipse(x = (stats[["highPercentile"]]+xUp)/2, y = y+0.225*boxHeight, a = max(0.05, 0.05+(xUp - stats[["highPercentile"]])/2), b = 0.05 + 0.225*boxHeight, border=ciDecorate, lwd = 2)
            }
        
        }
      }
      invisible()
}

boxwhiskers<-function(stats, y1, y2, ciType="two.sided")
{
    # names(stats) contains: c("median", "Q1", "Q3", "lowPercentile", "highPercentile", "mean", "stdDev", "exp")) 
 
    if (ciType=="none")
        return()
    
    ## quartile box
    rect(stats[["Q1"]], y1, stats[["Q3"]],  y2, col="lightgrey")
    ## median
    segments(stats[["median"]], y1, stats[["median"]], y2, lwd=2)
    ##minimum line and whisker
    midY = (y1+y2)/2.0
    yLen = y2-y1
     if (ciType != "upper") {
       segments(stats[["Q1"]], midY, stats[["lowPercentile"]], midY,lty="solid")       
       segments(stats[["lowPercentile"]], y1+0.2*yLen, stats[["lowPercentile"]], y2-0.2*yLen)
   }
    else
        segments(stats[["Q1"]], midY, stats[["lowPercentile"]], midY,lty="solid")  
 
    ##maximum line and whisker    
    if (ciType != "lower") {
        segments(stats[["Q3"]], midY, stats[["highPercentile"]], midY, lty="solid")
        segments(stats[["highPercentile"]], y1+0.2*yLen, stats[["highPercentile"]], y2-0.2*yLen) 
    }
    else
        segments(stats[["Q3"]], midY, stats[["highPercentile"]], midY, lty="solid")
    
       
    #if (length(stats)==8) ## also expeceted is present
    #   points(stats[[8]],midY,pch=20) 
    
}

trimSpaces<-function(strng) {
    s1=sub('^ +', '', strng)  ## trailing spaces only
    s2=sub(' +$', '', s1)  ## trailing spaces only
    s2
}

