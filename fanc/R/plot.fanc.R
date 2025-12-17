##
## Factor analysis with lasso: GTK object
##
##  file name: plot.fanc.R
##  file contents:
##  name;YAMAMOTO, Michio
##  date created:2012.02.02.
##  date modified:2013.12.29.
##  comments: deleted the display of  "X", "F" (031012)
##            created the plot function that can be used (031012)
##           minus loading is red(031012)
##           gamma bar is added, plot for odd number of factors is modified(031912)
##           added the heatmap(100613)
##           resolved the scroll bar in the heatmap(111113)
##           added output button, and several indices for heatmap(131229)
##           modified the figures(131229)
##

##-----define the callback function-----##
##callback function for expose-event signal
##function for depicting figures


##-----variables for info.fanc-----##

##*Rad.Ellipse: Major axis of the ellipse of factor 
##  -> Minor axis is calculated by major axis
##*Len.Rec: Length of rectangular of variables
##*Window.Height: window height
##  -> window width is calculated by the number of variables and the number of factors
##*num.lambda: initial number of Lambda(Ordinal number of Lambda)
##*L: 3-dimensional array of Lambda
##  -> the dimension of Lambda corresponds to (variable, factor, lambda).
##*lambdas, *gammas: vectors of lambda, gamma.
##*lambda.current, *gamma.current: current values of lambda, gamma
##*N.var, *N.fac, *N.lambda, *N.gamma: the number of variables, the number of factors, and the number of uning parameters
##  -> calculated from L

##type in c("heatmap", "path")


##plot.fanc
##modified the cancidates of lambda as gamma varies
plot.fanc <- function (x, Window.Height=500, type=NULL, df.method="active", ...){

    ## check ellipse library
    #if(nchar(system.file(package="ellipse")) == 0){
    #    msg <- paste0("The package 'ellipse' is required to plot ",
    #        "the solution path.\n",
    #        "Do you want to install 'ellipse' now? (y/n)")
    #    answer <- readline(msg)
    #  if(answer=="y"){
    #    install.packages("ellipse")
    #    if (nchar(system.file(package="ellipse")) == 0) stop('The package "ellipse" was not able to be installed')
    #  } else {
    #    stop("The plot was terminated.")
    #  }
    #}
    #requireNamespace("ellipse", quietly=TRUE)
    ## check tcltk library
    #if(nchar(system.file(package="tcltk")) == 0){
    # answer <- readline("The package 'tcltk' is required to plot the solution path. \nDo you want to install 'tcltk' now?  (y/n)")
    #  if(answer=="y"){
    #      install.packages("tcltk")
    #      if (nchar(system.file(package="tcltk")) == 0) stop('The package "tcltk" was not able to be installed')
    #  }else{
    #      stop("The plot was terminated.")
    #  }
    #}
    #requireNamespace("tcltk", quietly=TRUE)
    

       old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
   on.exit(par(old.par))

    # check plot type
    fig.type <- ""
    if (identical(type, "path") ||
        (is.null(type) && dim(x$loadings[[1]][[1]])[1] < 50)) {
        if(dim(x$loadings[[1]][[1]])[1] > 100) {
            msg <- paste0("The number of variables must be less than or",
                   "equal to 100 to plot the solution path.")
            stop(msg)
        }
        if(Window.Height<250 || Window.Height>2000) {
            stop("'Window.Height' must be in [250,2000].")
        }
        #
        fig.type <- "path"
    } else if (identical(type, "heatmap") ||
               (is.null(type) && dim(x$loadings[[1]][[1]])[1] >= 50)) {
        fig.type <- "heatmap"
    } else {
        stop("Only 'path' and 'heatmap' are available for 'type'")
    }
    ##extract the factor pattern
    L <- x$loadings
    lambdas <- x$rho
    gammas <- x$gamma
    if(df.method=="reparametrization"){
        GFIs <- x$GFI
        AGFIs <- x$AGFI
	CFIs <- x$CFI
	RMSEAs <- x$RMSEA
	SRMRs <- x$SRMR
        AICs <- x$AIC
        BICs <- x$BIC
        CAICs <- x$CAIC
	EBICs <- x$EBIC
    }
    if(df.method=="active"){
        GFIs <- x$GFI
        AGFIs <- x$AGFI_dfnonzero
	CFIs <- x$CFI_dfnonzero
	RMSEAs <- x$RMSEA_dfnonzero
	SRMRs <- x$SRMR

        AICs <- x$AIC_dfnonzero
        BICs <- x$BIC_dfnonzero
        CAICs <- x$CAIC_dfnonzero
	EBICs <- x$EBIC_dfnonzero
    }
    info.fanc <- list("Rad.Ellipse"=50, "Len.Rec"=50,
		      "Window.Height"=Window.Height,
                      "N.var"=NULL, "N.fac"=NULL, "N.lambda"=NULL,
                      "L"=NULL, "lambdas"=NULL, "num.lambda"=1,
		      "num.gamma"=1,"num.GFI"=1)

    info.fanc$lambda.current <- lambdas[1,1]
    info.fanc$gamma.current <- gammas[1]
    info.fanc$GFI.current <- GFIs[1]
    info.fanc$AGFI.current <- AGFIs[1]
    info.fanc$CFI.current <- CFIs[1]
    info.fanc$RMSEA.current <- RMSEAs[1]
    info.fanc$SRMR.current <- SRMRs[1]

    info.fanc$AIC.current <- AICs[1]
    info.fanc$BIC.current <- BICs[1]
    info.fanc$CAIC.current <- CAICs[1]
    info.fanc$EBIC.current <- EBICs[1]
    info.fanc$L <- L
    info.fanc$lambdas <- lambdas
    info.fanc$gammas <- gammas
    info.fanc$num.lambda <- 1
    info.fanc$num.gamma <- 1
    info.fanc$GFIs <- GFIs
    info.fanc$AGFIs <- AGFIs
    info.fanc$CFIs <- CFIs
    info.fanc$RMSEAs <- RMSEAs
    info.fanc$SRMRs <- SRMRs

    info.fanc$AICs <- AICs
    info.fanc$BICs <- BICs
    info.fanc$CAICs <- CAICs
    info.fanc$EBICs <- EBICs

    info.fanc$N.var <- dim(info.fanc$L[[1]][[1]])[1]
    info.fanc$N.fac <- dim(info.fanc$L[[1]][[1]])[2]
    info.fanc$N.lambda <- length(info.fanc$L[[1]])
    info.fanc$N.gamma <- length(info.fanc$L)
    info.fanc$Window.Width <- max(((info.fanc$N.var+1) * info.fanc$Len.Rec + (info.fanc$N.var+2) * info.fanc$Len.Rec / 2), ((info.fanc$N.fac) * 1.5 * info.fanc$Rad.Ellipse),650)

    ##For image function
    n.col <- 256
    col.red <- rgb(red=1, green = (0:n.col)/n.col, blue = (0:n.col)/n.col,
                   names = paste("red", 0:n.col, sep = "."))
    col.black <- rgb(red=(0:n.col)/n.col, green = (0:n.col)/n.col,
        blue = (0:n.col)/n.col, names = paste("black", 0:n.col, sep = "."))
    col.all <- c(col.red,rev(col.black))
    max.col <- 0
    for(i in 1:length(x$loadings)){
      for(j in 1:length(x$loadings[[1]])){
        max.col <- max(max.col,max(abs(x$loadings[[i]][[j]])))
      }
    }
    #--
    isPDF <- FALSE
    PDFFileName <- ""
    pngFileName <- ""

    # ---------------------------------------------
    # definitions functions
    # ---------------------------------------------
    # generation of uniq filename
    uniqFilename <- function(suffix) {
        nlen <- 17
        st <- strtoi(charToRaw('a'), 16)
        fin <- strtoi(charToRaw('z'), 16)
        fname <- ""
        while (TRUE) {
            xraw <- as.raw(floor(runif(nlen, st, fin+1)))
            fname <- paste(rawToChar(xraw), '.',suffix,sep='',collapse='')
            if (!file.exists(fname)) {
                break
            }
        }
        return(fname)
    }
    # get appropriate font size
    # width: # of pixels of spcae width
    # nlen : # of characters
    # dpi  : dots per inche
    # font size in point
    minFontSize <- 10
    wscale <- 1.2
    win.dpi <- 72
    apprFontSize <- function( width, nlen, dpi=win.dpi ) {
        width0 <- floor(width * wscale)
        pts <- floor( 72*width0/(nlen*dpi) )
        len    <- nlen
        if (pts<minFontSize) {
            len <- floor( 72*width0/(minFontSize*dpi) )
            pts <- minFontSize
        }
        return( c(pts, len) )
    }
    ##callback function for expose-event signal of GtkWidget label
    ##-----for 'rho'-----
    cbExposeLabelLambda <- function () {
        text <- sprintf ("rho : %.6f", info.fanc$lambda.current)
	tcltk::tkconfigure(fr3.label, text=text, font=fontSmall)
    }

    ##-----for 'GFI'-----
    cbExposeLabelGFI <- function () {
	text <- sprintf("%5s: %5.3f", "GFI", info.fanc$GFI.current)
	tcltk::tkconfigure(fr1.gfi, text=text, font=fontSmall)
    }

    ##-----for 'AGFI'-----
    cbExposeLabelAGFI <- function () {
	text <- sprintf("%5s: %5.3f", "AGFI", info.fanc$AGFI.current)
	tcltk::tkconfigure(fr1.agfi, text=text, font=fontSmall)
    }

    ##-----for 'CFI'-----
    cbExposeLabelCFI <- function () {
	text <- sprintf("%5s: %5.3f", "CFI", info.fanc$CFI.current)
	tcltk::tkconfigure(fr1.cfi, text=text, font=fontSmall)
    }

    ##-----for 'RMSEA'-----
    cbExposeLabelRMSEA <- function () {
	text <- sprintf("%5s: %5.3f", "RMSEA", info.fanc$RMSEA.current)
	tcltk::tkconfigure(fr1.rmsea, text=text, font=fontSmall)
    }

    ##-----for 'SRMR'-----
    cbExposeLabelSRMR <- function () {
	text <- sprintf("%5s: %5.3f", "SRMR", info.fanc$SRMR.current)
	tcltk::tkconfigure(fr1.srmr, text=text, font=fontSmall)
    }

    ##-----for 'AIC'-----
    cbExposeLabelAIC <- function () {
	text <- sprintf("%5s: %.4f", "AIC", info.fanc$AIC.current)
	tcltk::tkconfigure(fr2.aic, text=text, font=fontSmall)
      }

    ##-----for 'BIC'-----
    cbExposeLabelBIC <- function () {
	text <- sprintf("%5s: %.4f", "BIC", info.fanc$BIC.current)
	tcltk::tkconfigure(fr2.bic, text=text, font=fontSmall)
    }

    ##-----for 'CAIC'-----
    cbExposeLabelCAIC <- function () {
	text <- sprintf("%5s: %.4f", "CAIC", info.fanc$CAIC.current)
	tcltk::tkconfigure(fr2.caic, text=text, font=fontSmall)
    }

    ##-----for 'EBIC'-----
    cbExposeLabelEBIC <- function () {
	text <- sprintf("%5s: %.4f", "EBIC", info.fanc$EBIC.current)
	tcltk::tkconfigure(fr2.ebic, text=text, font=fontSmall)
    }

    ##-----for 'gamma'-----
    cbExposeLabelGamma <- function () {
        text <- sprintf ("gam : %f", info.fanc$gamma.current)
	tcltk::tkconfigure(fr4.label, text=text, font=fontSmall)
    }

    #
    onClickLoadings <- function () {
        print(info.fanc$L[[info.fanc$num.gamma]][[info.fanc$num.lambda]])
    }
    #
    onClickPDF <- function() {
	filename <- tcltk::tclvalue(tcltk::tkgetSaveFile())
	if (nchar(filename)>0) {
	    isPDF <<- TRUE
	    PDFFileName <<- filename
	    cbExposeCanvas()
	    isPDF <<- FALSE
	}
    }
    #
    onClickOut <- function() {
	lambda.current <- info.fanc$lambda.current
	gamma.current <- info.fanc$gamma.current
	print(out(x, rho=lambda.current, gamma=gamma.current, df.method=df.method))
    }

    ##callback function for value-changed signal of GtkScale widget
    ##("canvas")
    cbExposePath <- function () {
        maxStrLen <- 6
        orgFontSize <- par("ps")
        N.var <- info.fanc$N.var
        N.fac <- info.fanc$N.fac
	Window.Width <- as.numeric(tcltk::tkwinfo("width", canvas))
	Window.Height <- as.numeric(tcltk::tkwinfo("height", canvas))
	scale.x <- Window.Width / info.fanc$Window.Width
	scale.y <- Window.Height / info.fanc$Window.Height
        Rad.Ellipse.x <- info.fanc$Rad.Ellipse * scale.x * 2
        Rad.Ellipse.y <- info.fanc$Rad.Ellipse * scale.y
        Len.Rec.x <- info.fanc$Len.Rec * scale.x
        Len.Rec.y <- info.fanc$Len.Rec * scale.y

	Sep.Ellipse   <- info.fanc$Rad.Ellipse * scale.x / 2
	Step.Ellipse  <- Rad.Ellipse.x + Sep.Ellipse
	Sep.Rec       <- Len.Rec.x/2
	Step.Rec      <- Len.Rec.x + Sep.Rec

        # calc. font size
        #   for factors
        len <- 1 + floor(log10(N.fac))
        fontParams1 <- apprFontSize(Rad.Ellipse.x, len)
        #   for variable
        if (length(colnames(x$x)) == 0 ) {
            len <- 1 + floor(log10(N.var)) + 1
            fontParams2 <- apprFontSize(Len.Rec.x, len)
        } else {
            len <- max(nchar(colnames(x$x)))
            if ( len > maxStrLen ) {
                len <- maxStrLen+2      # 2 characters for '..'
            }
            fontParams2 <- apprFontSize(Len.Rec.x, len)
        }
        fontSize <- min(c(fontParams1[1], fontParams2[1]))
        strLen <- fontParams2[2]
        if (strLen > maxStrLen) {
            strLen <- maxStrLen
        }
        HN.var <- N.var / 2
        HN.fac <- N.fac / 2

	par(plt=c(0,1,0,1))
	par(mai=c(0, 0, 0, 0))
	par(mar=c(0, 0, 0, 0))
	par(omd=c(0, 1, 0, 1))
	par(oma=c(0, 0, 0, 0))
	par(omi=c(0, 0, 0, 0))
	par(xpd=NA)
	#dev.off()
	if ( isPDF == TRUE ) {
	    aspect <- Window.Width / Window.Height
	    if ( aspect > 1.417 ) {
		paper.Width <- 8.5
		paper.Height <- 8.5 / aspect
	    } else {
		paper.Height <- 6
		paper.Width <- 6 * aspect
	    }
            # calc. font size for printing
            winWidthInch <- Window.Width / win.dpi
            printScale.x <- paper.Width / winWidthInch
            printFontSize <- floor(fontSize*printScale.x)
            # debug
            print(sprintf("printFontSize=%d", printFontSize))

	    pdf(file=PDFFileName, width=paper.Width, height=paper.Height,
	    family="Courier" )
	    #family="Courier", pointsize=printFontSize )
	} else {
            pngFileName <- uniqFilename('png')
	    png(filename=pngFileName, width=Window.Width,
		height=Window.Height)
	}
par(ask=F)
	plot(NULL, NULL, xlim=c(0,Window.Width), ylim=c(Window.Height,0),
	    axes=FALSE, ann=FALSE)
        ##-------------------
        ##   depict the Ellipse of factor
        ##-------------------
        Rem.N.fac <- N.fac %% 2
        LineWidth <- 2
	x0 <- Window.Width / 2
	y0 <- 0.1 * Window.Height
	y1 <- y0 - Rad.Ellipse.y / 2
	y2 <- y0 + Rad.Ellipse.y / 2

	offset.num <- 1
	Offset.Ellipse <- Sep.Ellipse / 2
	NN.fac <- HN.fac
	rad <- seq(-pi, pi, length=40)
	if (Rem.N.fac != 0) {
	  offset.num <- 2
	  Offset.Ellipse <- Rad.Ellipse.x/2 + Sep.Ellipse
	  NN.fac <- floor(HN.fac)
	}
	# for odd case
	if (Rem.N.fac != 0) {
          ##depict the center ellipse
	  x1 <- x0 - Rad.Ellipse.x/2
	  x2 <- x0 + Rad.Ellipse.x/2
	  lines(cos(rad)*Rad.Ellipse.x/2+x0, sin(rad)*Rad.Ellipse.y/2+y0,
		lwd=LineWidth )
	  ##depict the center label
          if (isPDF == TRUE) {
              par(ps=printFontSize)
          } else {
              par(ps=fontSize)
          }
	  text <- sprintf ("f%d", NN.fac + 1)
	  x1   <- x0
	  text(x1, y0, labels=text)
          par(ps=orgFontSize)
	}
	if (NN.fac>0) {
	  for (i in 1:NN.fac) {
            ii <- i - 1
            ##depict the ellipse
	    x1 <- x0 + Offset.Ellipse + ii*Step.Ellipse
	    x2 <- x1 + Rad.Ellipse.x
	    xc <- (x1+x2)/2
	    lines(cos(rad)*Rad.Ellipse.x/2+xc, sin(rad)*Rad.Ellipse.y/2+y0,
		    lwd=LineWidth)

	    x2 <- x0 - Offset.Ellipse - ii*Step.Ellipse
	    x1 <- x2 - Rad.Ellipse.x
	    xc <- (x1+x2)/2
	    lines(cos(rad)*Rad.Ellipse.x/2+xc, sin(rad)*Rad.Ellipse.y/2+y0,
		    lwd=LineWidth)
	    ##depict the label
            if (isPDF == TRUE) {
                par(ps=printFontSize)
            } else {
                par(ps=fontSize)
            }
	    text <- sprintf ("f%d", NN.fac + offset.num + ii)
	    x1 <- x0 + (Offset.Ellipse+Rad.Ellipse.x/2) + ii*Step.Ellipse
	    text(x1, y0, labels=text, ps=fontSize)

	    text <- sprintf ("f%d", NN.fac - ii)
	    x1 <- x0 - (Offset.Ellipse+Rad.Ellipse.x/2) - ii*Step.Ellipse
	    text(x1, y0, labels=text, ps=fontSize)
            par(ps=orgFontSize)
	  }
	}

        ##-------------------
        ##  depict rectangular of variables 
        ##-------------------
        ##Change the pattern when the number of variables is even (odd)
        Rem.N.var <- N.var %% 2
	LineWidth <- 2

	y0 <- 0.9 * Window.Height
	y1 <- y0 - Len.Rec.y / 2
	y2 <- y0 + Len.Rec.y / 2

	Offset.Rec <- Sep.Rec / 2
	offset.num <- 1
	NN.var <- HN.var
	if (Rem.N.var != 0) {
	  Offset.Rec <- Len.Rec.x/2 + Sep.Rec
	  offset.num <- 2
	  NN.var <- floor(HN.var)
	}
    #par(family="Hiragino Maru Gothic ProN W4") # for Japanese
	# for odd case
	if (Rem.N.var != 0) {
          ##depict the center rectangle
	  x1 <- x0 - Len.Rec.x/2
	  x2 <- x0 + Len.Rec.x/2
	  rect(x1, y1, x2, y2, lwd=LineWidth)
	  ##depict the center label
          if ( length(colnames(x$x)) == 0 ) {
              text <- sprintf ("x%d", NN.var + 1)
          } else {
              text <- colnames(x$x)[NN.var + 1]
              if (nchar(text) > maxStrLen ) {
                  text <- paste0(substring(text, 1, strLen), "..")
              }
          }
          if (isPDF == TRUE) {
              par(ps=printFontSize)
          } else {
              par(ps=fontSize)
          }
	  x1 <- x0
	  text(x1, y0, labels=text)
          par(ps=orgFontSize)
	}
	if (NN.var>0) {
	  for (i in 1:NN.var) {
            ii <- i - 1
            ##depict the rectangle
	    x1 <- x0 + Offset.Rec + ii*(3/2*Len.Rec.x)
	    x2 <- x1 + Len.Rec.x
	    rect(x1, y1, x2, y2, lwd=LineWidth)

	    x2 <- x0 - Offset.Rec - ii*(3/2*Len.Rec.x)
	    x1 <- x2 - Len.Rec.x
	    rect(x1, y1, x2, y2, lwd=LineWidth)
	    ##depict the label
	    if ( length(colnames(x$x)) == 0 ) {
		text1 <- sprintf ("x%d", NN.var + offset.num + ii)
		text2 <- sprintf ("x%d", NN.var - ii)
	    } else {
		text1 <- colnames(x$x)[NN.var + offset.num + ii]
		text2 <- colnames(x$x)[NN.var - ii]
                if (nchar(text1) > strLen ) {
                    text1 <- paste0(substring(text1, 1, strLen), "..")
                }
                if (nchar(text2) > strLen ) {
                    text2 <- paste0(substring(text2, 1, strLen), "..")
                }
	    }
            if (isPDF == TRUE) {
                par(ps=printFontSize)
            } else {
                par(ps=fontSize)
            }
	    x1 <- x0 + (Offset.Rec+Len.Rec.x/2) + ii*(3/2*Len.Rec.x)
	    text(x1, y0, labels=text1)

	    x1 <- x0 - (Offset.Rec+Len.Rec.x/2) - ii*(3/2*Len.Rec.x)
	    text(x1, y0, labels=text2)
            par(ps=orgFontSize)
	  }
	}

        ##---------------------------
        ##  depict the line between variables and factors
        ##---------------------------
	x0 <- Window.Width / 2
	y2 <- 0.1*Window.Height + Rad.Ellipse.y/2
	y1 <- 0.9*Window.Height - Len.Rec.y/2
	x0.fac <- x0-(Sep.Ellipse+Rad.Ellipse.x)/2-Step.Ellipse*(NN.fac-1)
	x0.var <- x0 - (Sep.Rec+Len.Rec.x)/2     - Step.Rec*(NN.var-1)
	cur.lambda <- info.fanc$num.lambda
	cur.gamma <- info.fanc$num.gamma
	if (Rem.N.fac != 0) {
	  x0.fac <- x0-(Sep.Ellipse+Rad.Ellipse.x)-Step.Ellipse*(NN.fac-1)
	}
	if (Rem.N.var != 0 ) {
	  x0.var <- x0 - (Sep.Rec+Len.Rec.x)     - Step.Rec*(NN.var-1)
	}
        for (i in 1:N.var) {
          ii <- i - 1
	  x1 <- x0.var + ii*Step.Rec
          for (j in 1:N.fac) {
            jj <- j - 1
	    x2 <- x0.fac + jj*Step.Ellipse

	    val <- info.fanc$L[[cur.gamma]][[cur.lambda]][i, j]
	    lw0 <- abs((val  * 10))
	    LineWidth <- 0
	    if (lw0 > 0.05 && lw0<1.5 ) {
		LineWidth <- 1
	    } else if (lw0 >=1.5) {
		LineWidth <- as.integer( lw0 + 0.5 )
	    }
	    color <- "black"
            if (info.fanc$L[[cur.gamma]][[cur.lambda]][i, j] < 0) {
              color <- "red"
            }
	    if (LineWidth > 0) {
		lines(c(x1, x2), c(y1, y2), col=color, lwd=LineWidth)
	    }
          }
        }
	dev.off()
	#graphics.off()
	if ( isPDF == FALSE ) {
	    image1 <- tcltk::tclVar()
	    tcltk::tkimage.create("photo", image1, file=pngFileName)
	    tcltk::tkdelete(canvas, tag="all")
	    tcltk::tkcreate(canvas, "image",0,0,image=image1,anchor="nw")
            file.remove(pngFileName)
	}
    }
    ## --
    cbExposeHeatmap <- function() {
        loadings <-
            info.fanc$L[[info.fanc$num.gamma]][[info.fanc$num.lambda]]
        loadings <- as.matrix(loadings)
        loadings  <- t(loadings)
        loadings <- fliplr.fanc(loadings)
        #
        Window.Width <- as.numeric(tcltk::tkwinfo("width", canvas))
        Window.Height <- as.numeric(tcltk::tkwinfo("height", canvas))
	# output image to temporary file
	if ( isPDF == FALSE ) {
            pngFileName <- uniqFilename('png')
	    png(pngFileName, width=Window.Width, height=Window.Height)
	} else {
	    pdf(file=PDFFileName, width=6, height=6)
	}
        image(loadings, col=col.all, zlim=c(-max.col, max.col),
		xlab="factors", ylab="variables")
	dev.off()
	# Convert to Tcl/Tk image and put it on the canvas
	if ( isPDF == FALSE ) {
	    image1 <- tcltk::tclVar()
	    tcltk::tkimage.create("photo", image1, file=pngFileName)
	    tcltk::tkdelete(canvas, tag="all")
	    tcltk::tkcreate(canvas, "image", 0, 0, image=image1,
                anchor="nw" )
            file.remove(pngFileName)
	}
    }

    cbExposeCanvas <- function() {
        if ( fig.type == "path" ) {
            cbExposePath()
        } else {
            cbExposeHeatmap()
        }
    }

    onChangeParam <- function (...) {
        # rho
        value <- as.numeric(tcltk::tclvalue(LambdaValue))
        num.lambda <- value + 1
        if (num.lambda < 1 ) {
            num.lambda <- 1
        }
        
        # gamma
        value <- as.numeric(tcltk::tclvalue(GammaValue))
        num.gamma <- value + 1
        if (num.gamma < 1) {
            num.gamma <- 1
        }
        

        info.fanc$num.lambda <<- num.lambda
        info.fanc$num.gamma  <<- num.gamma
        info.fanc$lambda.current<<-info.fanc$lambdas[num.lambda, num.gamma]
        info.fanc$gamma.current<<-info.fanc$gammas[num.gamma]
        info.fanc$GFI.current <<- info.fanc$GFIs[num.lambda, num.gamma]
        info.fanc$AGFI.current <<- info.fanc$AGFIs[num.lambda, num.gamma]
        info.fanc$CFI.current <<- info.fanc$CFIs[num.lambda, num.gamma]
        info.fanc$RMSEA.current <<- info.fanc$RMSEAs[num.lambda, num.gamma]
        info.fanc$SRMR.current <<- info.fanc$SRMRs[num.lambda, num.gamma]
    
        info.fanc$AIC.current <<- info.fanc$AICs[num.lambda, num.gamma]
        info.fanc$BIC.current <<- info.fanc$BICs[num.lambda, num.gamma]
        info.fanc$CAIC.current <<- info.fanc$CAICs[num.lambda, num.gamma]
        info.fanc$EBIC.current <<- info.fanc$EBICs[num.lambda, num.gamma]
        cbExposeCanvas()
        cbExposeLabelLambda()
        cbExposeLabelGFI()
        cbExposeLabelAGFI()
        cbExposeLabelCFI()
        cbExposeLabelRMSEA()
        cbExposeLabelSRMR()
        cbExposeLabelAIC()
        cbExposeLabelBIC()
        cbExposeLabelCAIC()
        cbExposeLabelEBIC()
        cbExposeLabelGamma()
    }

    onClickOverview <- function() {
	cbExposeSubPath <- function() {
            N.var <- info.fanc$N.var
            N.fac <- info.fanc$N.fac
	    window.width<-as.numeric(tcltk::tkwinfo("width", subCanvas))
	    window.height<-as.numeric(tcltk::tkwinfo("height",subCanvas))
	    canvas.height <- window.height/ndiv.lambda
	    canvas.width  <- window.width/ndiv.gamma
            #
	    scale.x <- canvas.width / info.fanc$Window.Width
	    scale.y <- canvas.height / info.fanc$Window.Height
	    Rad.Ellipse.x <- info.fanc$Rad.Ellipse * scale.x * 2
	    Rad.Ellipse.y <- info.fanc$Rad.Ellipse * scale.y
	    Len.Rec.x <- info.fanc$Len.Rec * scale.x
	    Len.Rec.y <- info.fanc$Len.Rec * scale.y

	    Sep.Ellipse   <- info.fanc$Rad.Ellipse * scale.x / 2
	    Step.Ellipse  <- Rad.Ellipse.x + Sep.Ellipse
	    Sep.Rec       <- Len.Rec.x/2
	    Step.Rec      <- Len.Rec.x + Sep.Rec

	    HN.var <- N.var / 2
	    HN.fac <- N.fac / 2

	    tcltk::tkdelete(subCanvas, tag="all")
	    for (i in 1:(ndiv.lambda-1)) {
		yy <- i * canvas.height
		tcltk::tkcreate(subCanvas, "line", 0, yy, window.width, yy,
			width=2, fill="#aaaaaa")
	    }
	    for (i in 1:(ndiv.gamma-1)) {
		xx <- i * canvas.width
		tcltk::tkcreate(subCanvas, "line", xx, 0,
			xx, window.height, width=2, fill="#aaaaaa")
	    }

	    Rem.N.fac <- N.fac %% 2
	    Rem.N.var <- N.var %% 2
	    NN.fac <- HN.fac
	    if (Rem.N.fac != 0) {
		NN.fac <- floor(HN.fac)
	    }
	    NN.var <- HN.var
	    if (Rem.N.var != 0) {
	        NN.var <- floor(HN.var)
	    }
	    ##-------------------
	    ##   depict the Ellipse of factor
	    ##-------------------
	    LineWidth <- 1
	    for ( i in 1:ndiv.gamma ) {
		x0 <- (i-1)*canvas.width + canvas.width/2
		for ( j in 1:ndiv.lambda ) {
		    y0 <- (j-1)*canvas.height + 0.2 * canvas.height
		    y1 <- y0 - Rad.Ellipse.y / 2
		    y2 <- y0 + Rad.Ellipse.y / 2
		    Offset.Ellipse <- Sep.Ellipse / 2
		    offset.num <- 1
		    if (Rem.N.fac != 0) {
			offset.num <- 2
			Offset.Ellipse <- Rad.Ellipse.x/2 + Sep.Ellipse
		    }
		    # for odd case
		    if (Rem.N.fac != 0) {
		      ##depict the center ellipse
		      x1 <- x0 - Rad.Ellipse.x/2
		      x2 <- x0 + Rad.Ellipse.x/2
		      tcltk::tkcreate(subCanvas, "oval", x1, y1, x2, y2,
			      width=LineWidth)
		      ##depict the center label
		      text <- sprintf ("f%d", NN.fac + 1)
		      x1 <- x0
		      tcltk::tkcreate(subCanvas, "text", x1, y0, text=text,
			      anchor="center", font=fontTiny)
		    }
		    if (NN.fac>0) {
		      for (ii in 1:NN.fac) {
			iii <- ii - 1
			##depict the ellipse
			x1 <- x0 + Offset.Ellipse + iii*Step.Ellipse
			x2 <- x1 + Rad.Ellipse.x
			tcltk::tkcreate(subCanvas, "oval", x1, y1, x2, y2,
				width=LineWidth)

			x2 <- x0 - Offset.Ellipse - iii*Step.Ellipse
			x1 <- x2 - Rad.Ellipse.x
			tcltk::tkcreate(subCanvas, "oval", x1, y1, x2, y2,
				width=LineWidth)
			##depict the label
			text <- sprintf ("f%d", NN.fac + offset.num + iii)
			x1 <- x0 + (Offset.Ellipse+Rad.Ellipse.x/2) + iii*Step.Ellipse
			tcltk::tkcreate(subCanvas, "text", x1, y0,
				text=text, anchor="center", font=fontTiny)

			text <- sprintf ("f%d", NN.fac - iii)
			x1 <- x0 - (Offset.Ellipse+Rad.Ellipse.x/2)	- iii*Step.Ellipse
			tcltk::tkcreate(subCanvas, "text", x1, y0,
				text=text, anchor="center", font=fontTiny)
		      }
		    }
		}
	    }
	    #
	    ##-------------------
	    ##  depict rectangular of variables 
	    ##-------------------
	    LineWidth <- 1
	    for ( i in 1:ndiv.gamma ) {
		x0 <- (i-1)*canvas.width + canvas.width/2
		for ( j in 1:ndiv.lambda ) {
		    y0 <- (j-1)*canvas.height + 0.9 * canvas.height
		    y1 <- y0 - Len.Rec.y / 2
		    y2 <- y0 + Len.Rec.y / 2
		    Offset.Rec <- Sep.Rec / 2
		    offset.num <- 1
		    if (Rem.N.var != 0) {
		      Offset.Rec <- Len.Rec.x/2 + Sep.Rec
		      offset.num <- 2
		    }
		    # for odd case
		    if (Rem.N.var != 0) {
		      ##depict the center rectangle
		      x1 <- x0 - Len.Rec.x/2
		      x2 <- x0 + Len.Rec.x/2
		      tcltk::tkcreate(subCanvas, "rectangle", x1, y1,
			      x2, y2, width=LineWidth)
		      ##depict the center label
		      text <- sprintf ("x%d", NN.var + 1)
		      x1 <- x0
		      tcltk::tkcreate(subCanvas, "text", x1, y0, text=text,
			      anchor="center", font=fontTiny)
		    }
		    if (NN.var>0) {
		      for (ii in 1:NN.var) {
			iii <- ii - 1
			##depict the rectangle
			x1 <- x0 + Offset.Rec + iii*Step.Rec
			x2 <- x1 + Len.Rec.x
			tcltk::tkcreate(subCanvas, "rectangle", x1, y1,
				x2, y2, width=LineWidth)

			x2 <- x0 - Offset.Rec - iii*Step.Rec
			x1 <- x2 - Len.Rec.x
			tcltk::tkcreate(subCanvas, "rectangle", x1, y1,
				x2, y2, width=LineWidth)
			##depict the label
			text <- sprintf ("x%d", NN.var + offset.num + iii)
			x1 <- x0 + Offset.Rec+Len.Rec.x/2 + iii*Step.Rec
			tcltk::tkcreate(subCanvas, "text", x1, y0,
				text=text, anchor="center", font=fontTiny)

			text <- sprintf ("x%d", NN.var - iii)
			x1 <- x0 - Offset.Rec - iii*Step.Rec - Len.Rec.x/2
			tcltk::tkcreate(subCanvas, "text", x1, y0,
				text=text, anchor="center", font=fontTiny)
		      }
		    }
		}
	    }
	    ##---------------------------
	    ##  depict the line between variables and factors
	    ##---------------------------
	    for ( i in 1:ndiv.gamma ) {
		x0 <- (i-1)*canvas.width + canvas.width/2
		for ( j in 1:ndiv.lambda ) {
		    y2 <- (j-1)*canvas.height + 0.2*canvas.height +
			    Rad.Ellipse.y/2
		    y1 <- (j-1)*canvas.height + 0.9*canvas.height -
			    Len.Rec.y/2
		    x0.fac <- x0 - Step.Ellipse/2 - Step.Ellipse*(NN.fac-1)
		    x0.var <- x0 - Step.Rec/2 - Step.Rec*(NN.var-1)
		    cur.lambda <- lambda.list[j]
		    cur.gamma <- gamma.list[i]
		    if (Rem.N.fac != 0) {
		      x0.fac <- x0 - Step.Ellipse*NN.fac
		    }
		    if (Rem.N.var != 0 ) {
		      x0.var <- x0 - Step.Rec*NN.var
		    }
		    for (ii in 1:N.var) {
		      x1 <- x0.var + (ii-1)*Step.Rec
		      for (jj in 1:N.fac) {
			x2 <- x0.fac + (jj-1)*Step.Ellipse
			val<-info.fanc$L[[cur.gamma]][[cur.lambda]][ii, jj]
			color <- "black"
			if (val < 0) {
			  color <- "red"
			}
			LineWidth <- abs(val * 5)
			if (LineWidth > 0.025) {
			    tcltk::tkcreate(subCanvas, "line", x1, y1,
				    x2, y2, fill=color, width=LineWidth)
			}
		      }
		    }
		}	# for (j)
	    }	# for (i)
	    ##---------------------------
	    ##  depict values of lambda and gamma
	    ##---------------------------
	    for ( i in 1:ndiv.gamma ) {
		x0 <- (i-1)*canvas.width + canvas.width/2
		for ( j in 1:ndiv.lambda ) {
		    y0 <- (j-1)*canvas.height + 0.05*canvas.height
		    cur.lambda <- lambda.list[j]
		    cur.gamma  <- gamma.list[i]
		    lambda <- info.fanc$lambdas[cur.lambda, cur.gamma]
		    gamma <- info.fanc$gammas[cur.gamma]
		    text <- sprintf("rho= %.3f  gamma= %.3f",
			lambda, gamma)
		    tcltk::tkcreate(subCanvas, "text", x0, y0, text=text,
			anchor="center", font=fontTiny )
		}
	    }
	}	# exposeCanvas
        cbExposeSubHeatmap <- function() {
            N.var <- info.fanc$N.var
            N.fac <- info.fanc$N.fac
	    window.width<-as.numeric(tcltk::tkwinfo("width", subCanvas))
	    window.height<-as.numeric(tcltk::tkwinfo("height",subCanvas))
	    canvas.height <- window.height/ndiv.lambda
	    canvas.width  <- window.width/ndiv.gamma
            tcltk::tkdelete(subCanvas, tag="all")
            for (i in 1:ndiv.gamma) {
                cur.gamma <- gamma.list[i]
                x.pos <- canvas.width * (i-1)
                gamma <- info.fanc$gammas[cur.gamma]
                for (j in 1:ndiv.lambda) {
                    cur.lambda <- lambda.list[j]
                    y.pos <- canvas.height * (j-1)
                    lambda <- info.fanc$lambdas[cur.lambda, cur.gamma]
                    # setting variables
                    loadings <- info.fanc$L[[cur.gamma]][[cur.lambda]]
                    loadings <- as.matrix(loadings)
                    loadings  <- t(loadings)
                    loadings <- fliplr.fanc(loadings)
                    # title
                    text <- sprintf("rho= %.3f  gamma= %.3f",
                                    lambda, gamma)
                    # get file name
                    tmpFile <- uniqFilename('png')
                    png(tmpFile, width=canvas.width,
                        height=canvas.height)
                    image(loadings, col=col.all, main=text,
                          zlim=c(-max.col, max.col),
                          xlab="factors", ylab="variables")
                    dev.off()
                    #
                    image1 <- tcltk::tclVar()
                    tcltk::tkimage.create("photo", image1, file=tmpFile)
                    tcltk::tkcreate(subCanvas, "image", x.pos, y.pos,
                                    image=image1, anchor="nw" )
                    file.remove(tmpFile)
                }
            }
        }
        cbExposeSubCanvas <- function() {
            if ( fig.type == "path" ) {
                cbExposeSubPath()
            } else {
                cbExposeSubHeatmap()
            }
        }
	onClickDiv <- function() {
	    if (ndiv.lambda == 5) {
		ndiv.lambda <<- 4
		ndiv.gamma <<- 4
		tcltk::tkconfigure(subFrm.div, text="5x5")
		tcltk::tktitle(subWin) <- "Overview (4x4)"
	    } else {
		ndiv.lambda <<- 5
		ndiv.gamma <<- 5
		tcltk::tkconfigure(subFrm.div, text="4x4")
		tcltk::tktitle(subWin) <- "Overview (5x5)"
	    }
            if (ndiv.lambda > N.lambda) {
                ndiv.lambda <<- N.lambda
            }
            if (ndiv.gamma > N.gamma) {
                ndiv.gamma <<- N.gamma
            }
            cbExposeSubCanvas()
	}
        #-- main program of sub canvas
	N.lambda <- info.fanc$N.lambda
	N.gamma  <- info.fanc$N.gamma
	ndiv.lambda <- 5
	ndiv.gamma  <- 5
        if (N.lambda < ndiv.lambda) {
            ndiv.lambda <- N.lambda
        }
        if (N.gamma < ndiv.gamma) {
            ndiv.gamma <- N.gamma
        }
	#
	lambda.list <- vector(length=ndiv.lambda)
	gamma.list  <- vector(length=ndiv.gamma)
        if (ndiv.lambda == 1) {
            lambda.list[1] <- 1
        } else {
            d.lambda <- (N.lambda-1) / (ndiv.lambda-1)
            for (i in 1:ndiv.lambda) {
                lambda.list[i] <- 1 + as.integer((i-1)*d.lambda+0.1)
            }
        }
        if (ndiv.gamma == 1) {
            gamma.list[1] <- 1
        } else {
            d.gamma  <- (N.gamma-1)  / (ndiv.gamma-1)
            for (i in 1:ndiv.gamma) {
                gamma.list[i] <- 1 + as.integer((i-1)*d.gamma+0.1)
            }
        }
        #-- create sub canvas
	subWin <- tcltk::tktoplevel()
	title <- sprintf("Overview (%dx%d)", ndiv.lambda, ndiv.gamma)
	tcltk::tktitle(subWin) <- title
	subCanvas <- tcltk::tkcanvas(subWin, background="#FFFFFF")
	tcltk::tkpack(subCanvas, expand=TRUE, fill="both")
	tcltk::tkbind(subCanvas, "<Configure>", cbExposeSubCanvas) 
	# some button
	subFrm <- tcltk::tkframe(subWin)
	subFrm.close <- tcltk::tkbutton(subFrm, text="Close",
		width=Text.Width, padx=10,
		command=function() tcltk::tkdestroy(subWin))
	subFrm.div <- tcltk::tkbutton(subFrm, text="4x4",
		width=Text.Width, padx=10, command=onClickDiv)
	tcltk::tkpack(subFrm.close, side="right")
	tcltk::tkpack(subFrm.div, side="right")
	tcltk::tkpack(subFrm, side="right")
	tcltk::tkwm.geometry(subWin, "1200x850")
    }


    ##-----main window to depict-----##
    ##"info" includes 'L' and 'lambdas'.
    ## 'L' is a p*m*r array of pattern matrices
    ## 'lambdas' is a r*1 vector of tuning parameter
    lambdas <- info.fanc$lambdas
    gammas <- info.fanc$gammas
    N.lambda <- info.fanc$N.lambda
    N.gamma  <- info.fanc$N.gamma
    ##the range of lambda, gamma are restricted in (0, 1)
    ##for 'rho'
    Min.lambda <- 0
    Max.lambda <- N.lambda - 1
    Step.lambda <- 1
    ##for 'gamma'
    Min.gamma <- 0
    Max.gamma <- N.gamma - 1
    Step.gamma <- 1

LambdaValue <- tcltk::tclVar(sprintf("%f", Min.lambda))
    GammaValue  <- tcltk::tclVar(sprintf("%f", Min.gamma))
    Items <- c()
    # font settings
    fontNormal <- tcltk::tkfont.create( family="Courier New", size=14)
    fontSmall <- tcltk::tkfont.create( family="Courier New", size=10)
    fontTiny <- tcltk::tkfont.create( family="Courier New", size=8)

    Window.Width <- sprintf("%d", info.fanc$Window.Width)
    Window.Height0 <- sprintf("%d", info.fanc$Window.Height + 200)
    Text.Width <- "16"

    top <- tcltk::tktoplevel(width=Window.Width, height=Window.Height0)
    ##-------------------
    ##    depict the Tk canvas
    ##-------------------
    canvas <- tcltk::tkcanvas(top, background="#FFFFFF")
    tcltk::tkpack(canvas, expand=TRUE, fill="both")
    tcltk::tkbind(canvas, "<Configure>", cbExposeCanvas) 

    ##-----------------------
    ##  depict the goodness of fit indices
    ##-----------------------
    ##-- create master frame
    frmAll <- tcltk::tkframe(top, width=Window.Width)
    ##-- create frame 1 --
    fr1 <- tcltk::tkframe(frmAll, width=Window.Width)
    fr1.gfi   <- tcltk::tklabel(fr1, width=Text.Width, anchor="w", text="")
    fr1.agfi  <- tcltk::tklabel(fr1, width=Text.Width, anchor="w", text="")
    fr1.cfi   <- tcltk::tklabel(fr1, width=Text.Width, anchor="w", text="")
    fr1.rmsea <- tcltk::tklabel(fr1, width=Text.Width, anchor="w", text="")
    fr1.srmr  <- tcltk::tklabel(fr1, width=Text.Width, anchor="w", text="")
    tcltk::tkpack(fr1.gfi,   side="left")
    tcltk::tkpack(fr1.agfi,  side="left")
    tcltk::tkpack(fr1.cfi,   side="left")
    tcltk::tkpack(fr1.rmsea, side="left")
    tcltk::tkpack(fr1.srmr,  side="left")
    tcltk::tkpack(fr1)
    ##-- create frame2 --
    fr2 <- tcltk::tkframe(frmAll, width=Window.Width)
    fr2.aic  <- tcltk::tklabel(fr2, width=Text.Width, anchor="w", text="" )
    fr2.bic  <- tcltk::tklabel(fr2, width=Text.Width, anchor="w", text="" )
    fr2.caic <- tcltk::tklabel(fr2, width=Text.Width, anchor="w", text="" )
    fr2.ebic <- tcltk::tklabel(fr2, width=Text.Width, anchor="w", text="" )
    fr2.dum  <- tcltk::tklabel(fr2, width=Text.Width, anchor="w", text="" )
    tcltk::tkpack(fr2.aic,  side="left")
    tcltk::tkpack(fr2.bic,  side="left")
    tcltk::tkpack(fr2.caic, side="left")
    tcltk::tkpack(fr2.ebic, side="left")
    tcltk::tkpack(fr2.dum,  side="left")
    tcltk::tkpack(fr2)

    ##-- rho slider --
    fr3 <- tcltk::tkframe(frmAll, width=Window.Width)
    fr3.label <- tcltk::tklabel(fr3, width=Text.Width, anchor="w", text="")
    fr3.scale <- tcltk::tkscale(fr3, length=250, from=Min.lambda,
	to=Max.lambda, resolution=Step.lambda, variable=LambdaValue,
	orient="horizontal", showvalue=0, command=onChangeParam )
    tcltk::tkpack(fr3.label, side="left")
    tcltk::tkpack(fr3.scale, side="left")
    tcltk::tkpack(fr3)

    ##-- gamma slider --
    fr4 <- tcltk::tkframe(frmAll, width=Window.Width)
    fr4.label <- tcltk::tklabel(fr4, width=Text.Width, anchor="w", text="")
    fr4.scale <- tcltk::tkscale(fr4, length=250, from=Min.gamma,
	to=Max.gamma, resolution=Step.gamma, variable=GammaValue,
	orient="horizontal", showvalue=0, command=onChangeParam )
    tcltk::tkpack(fr4.label, side="left")
    tcltk::tkpack(fr4.scale, side="left")
    tcltk::tkpack(fr4)

    ##-- some buttons --
    fr5 <- tcltk::tkframe(frmAll, width=Window.Width)
    fr5.overview <- tcltk::tkbutton(fr5, text="Overview", width=Text.Width,
	padx=10, command=onClickOverview)
    fr5.loadings <- tcltk::tkbutton(fr5, text="Output loadings",
	width=Text.Width, padx=10, command=onClickLoadings)
    fr5.out <- tcltk::tkbutton(fr5, text="Output params", width=Text.Width,
	    padx=10, command=onClickOut)
    fr5.pdf <- tcltk::tkbutton(fr5, text="PDF", width=Text.Width,
	    padx=20, command=onClickPDF)
    tcltk::tkpack(fr5.loadings, side="right")
    tcltk::tkpack(fr5.overview, side="right")
    tcltk::tkpack(fr5.out,      side="right")
    tcltk::tkpack(fr5.pdf,      side="right")
    tcltk::tkpack(fr5, side="right")

    tcltk::tkpack(frmAll, fill="x")
    tcltk::tkwm.geometry(top, "900x650")

    if(x$type == "MC" && x$model == "FA"){
    tcltk::tktitle(top) <- "Factor analysis with MC+"
    }
    if(x$type == "prenet" && x$model == "FA") {
    tcltk::tktitle(top) <- "Factor analysis with prenet"
    }
    if(x$type == "MC" && x$model == "PPCA"){
    tcltk::tktitle(top) <- "Probabilistic PCA with MC+"
    }
    if(x$type == "prenet" && x$model == "PPCA") {
    tcltk::tktitle(top) <- "Probabilistic PCA with prenet"
    }
}

fliplr.fanc <- function(x){
    m <- ncol(x)
    x[,m:1]
}
