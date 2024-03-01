#' @export
glm.pred.JGR = function(my.data, sampsize.name=NULL, subset1.name,
                         subset1.val, subset2.name, subset2.val,
                         site.name, site.val,
                         my.formula, my.family="gaussian",
                         label.name="",
                         iRmIntercept=FALSE,
                         sig.level=0.05,
                         browserResults=FALSE)
{
### fits a linear regression model returning estimates and diagnostics
  
### my.data      data.frame
### subset1.name column name of the first subsetting variable (panel)
### subset1.val  values of the first subsetting variable
### subset2.name column name of the second subsetting variable (group)
### subset2.val  values of the second subsetting variable
### site.name    column name of the variable indicating reference data
### site.val     values of site.name to include in the reference data
### my.formula   string giving the model formula
###                [If binomial family, assumes a comma-separated string
###                 giving: n,y,x1,x2,x3,... (with spaces)]
### my.family    error family
### label.name   column name containing the labels to plot for each row
### my.contrast  numeric vector giving the contrast of interest
### iRmIntercept indicates whether the model should be fit without an intercept
### sig.level    significance level -- used for flagging high t-values

  ## Get place to store results
  if (browserResults) resultLocation = genResultSpace()
  
  ## a function to strip leading/trailing white-space (shouldn't be
  ##   needed, but in case called with spaces in my.formula ...
  stripwhite = function(x){ gsub("[[:space:]]+$","",gsub("^[[:space:]]+","",x)) }

  # NA handling
  # use options to set na.action to omit NAs which is used by lm
  options(na.action = "na.omit")

  #find proper data subset
  ind = rep(TRUE, nrow(my.data))
  if (length(subset1.val)>0) ind = ind & my.data[,subset1.name]%in%subset1.val
  if (length(subset2.val)>0) ind = ind & my.data[,subset2.name]%in%subset2.val
  my.subset = my.data[ind,]

  ## Check for sensible binomial inputs and remove NA rows
  if( my.family == "binomial" ){
    vars = stripwhite( strsplit(my.formula,",")[[1]] )
    if( vars[1]=="1" ){
      nas = is.na(apply(my.subset[,vars[2:length(vars)]],1,sum))
      my.subset = my.subset[!nas,]
      nn = rep(1,nrow(my.subset))
    } else {
      nas = is.na(apply(my.subset[,vars],1,sum))
      my.subset = my.subset[!nas,]
      nn = my.subset[,vars[1]]
    }
    yname=vars[2]
    yy = my.subset[,yname]
    nna = !is.na(yy) & !is.na(nn)
    yyna = yy[nna]
    nnna = nn[nna]
    
    if( any( ( yyna < 0 ) || ( yyna != as.integer(yyna[1]) ) ) ){
      if (browserResults) {
        localJGRError("To fit data under the binomial distribution, the dependent variable must contain only non-negative integers.", resultLocation, geterr=FALSE)
      } else {
      	stop("To fit data under the binomial distribution, the dependent variable must contain only non-negative integers.", call.=FALSE)
      }
    }
    
    if( any( ( nnna < 0 ) || ( nnna != as.integer(nnna[1]) ) ) ){
      if (browserResults) {
        localJGRError("To fit data under the binomial distribution, the sample size variable must contain only non-negative integers.", resultLocation, geterr=FALSE)
      } else {
        stop("To fit data under the binomial distribution, the sample size variable must contain only non-negative integers.", call.=FALSE)
      }
    }
    
    if( any( ( yyna > nnna ) ) ){
      if (browserResults) {
      	localJGRError("To fit data under the binomial distribution, the dependent variable must be between 0 and the corresponding value for the sample size variable.", resultLocation, geterr=FALSE)
      } else {
      	stop("To fit data under the binomial distribution, the dependent variable must be between 0 and the corresponding value for the sample size variable.", call.=FALSE)
      }
    }
    
    my.formula=paste("cbind(", vars[2], ",", vars[1], "-", vars[2], ")~", paste(vars[3:length(vars)], collapse="+"), sep="")
  }
  else {
    vars = stripwhite(strsplit(my.formula,"~")[[1]])
    yname = vars[1]
    vars = c(yname,stripwhite(strsplit(vars[2],"[+]")[[1]]))
    imatch <- match("1", vars)
    if (! is.na(imatch)) {
      vars = vars[-match("1", vars)]
    }
    
    if (length(vars) > 1) {
      nas = is.na(apply(my.subset[,vars],1,sum))
    }
    else {
      nas = is.na(my.subset[, vars])
    }
    my.subset = my.subset[!nas,]
    if( my.family=="poisson" ){
      if( any( ( my.subset[,yname] < 0 ) | ( my.subset[,yname] != as.integer(my.subset[,yname]) ) ) ) {
		  if (browserResults) {
			localJGRError("To fit data under the Poisson distribution, the dependent variable must contain only non-negative integers.", resultLocation, geterr=FALSE )
		  } else {
			stop("To fit data under the Poisson distribution, the dependent variable must contain only non-negative integers.", call.=FALSE)
		  }
      }
    }
  }

  ## Get reference data
  in.ref = my.subset[,site.name] %in% site.val
  nref = sum(in.ref)
  if( nref == 0 ){
	  if (browserResults) {
		localJGRError("No reference data selected (0 rows after subsetting).", resultLocation, geterr=FALSE )
	  } else {
		stop("No reference data selected (0 rows after subsetting).", call.=FALSE)
	  }
  }
  if( nref == nrow(my.subset) ){
	  if (browserResults) {
		localJGRError("No non-reference data selected (0 rows after subsetting).", resultLocation, geterr=FALSE )
	  } else {
		stop("No non-reference data selected (0 rows after subsetting).", call.=FALSE)
	  }
  }
  refdata = my.subset[in.ref,]
  nrefdata = my.subset[!in.ref,]
  if( my.family=="binomial" ){
    nn.ref = nn[in.ref]
    nn.nref = nn[!in.ref]
  }

  ## fit the model
  if (iRmIntercept) my.formula = paste(my.formula, "-1")
  my.formula = as.formula(my.formula)
  my.fit = try( glm(my.formula, data=refdata, family=my.family, x=TRUE ) )
  if( inherits( my.fit, "try-error" ) ){
	  if (browserResults) {
		localJGRError("Error attempting to fit the generalized linear model.", resultLocation, geterr=TRUE )
	  } else {
		stop("Error attempting to fit the generalized linear model.", call.=TRUE)
	  }
  }
  
  ## predict on reference data
  fittedval = predict( my.fit, newdata=refdata, type="response" )

  ## predict on non-reference data
  predfit = predict( my.fit, newdata=nrefdata, se.fit=TRUE, type="response" )

  ## get standard errors for non-reference data
  if( my.family=="gaussian" ){
    stderrPred = sqrt( predfit$se.fit^2 + predfit$residual.scale^2 )
  } else if( my.family=="poisson" ){
    stderrPred = sqrt( predfit$se.fit^2 + predfit$fit )
    poisFlag = ifelse( predfit$fit < 5, "No", "Yes" )
  } else {
    stderrPred = sqrt( predfit$se.fit^2 + predfit$fit * ( 1 - predfit$fit ) / nn.nref )
    binomFlag = ifelse( predfit$fit * nn.nref < 5, "No",
      ifelse( ( 1 - predfit$fit ) * nn.nref < 5, "No", "Yes" ) )
  }    

  ## calculate t/z-values
  if( my.family=="binomial" ){
    tval = ( nrefdata[,yname] / nn.nref - predfit$fit ) / stderrPred
  } else {
    tval = ( nrefdata[,yname] - predfit$fit ) / stderrPred
  }
  if( my.family=="gaussian" ){
    pval = 2 * pt( -abs(tval), my.fit$df.residual )
  } else {
    pval = 2 * pnorm( -abs(tval) )
  }
  significance = ( pval < sig.level )
  significancelab = ifelse( significance, "Yes", "" )

  ## calculate high-leverage points
  nxmat = model.matrix(my.formula,data=nrefdata)
  vmat = nxmat %*% solve( t(my.fit$x) %*% my.fit$x ) %*% t(nxmat)
  hyplev = diag(vmat) / ( 1 - my.fit$df.residual / my.fit$df.null )
  highlev = ( hyplev > 2 )
  highlevlab = ifelse( hyplev > 5, "No", ifelse( hyplev > 2, "Borderline", "Yes" ) )
  
  ## set up output
  if( label.name=="" ){
    labelcol=data.frame(" "=1:nrow(nrefdata))
  } else {
    labelcol=nrefdata[,label.name]
  }
  if( my.family=="binomial" ){
    output = data.frame( labelcol, nn.nref, nrefdata[,yname] / nn.nref, predfit$fit,
      stderrPred, pval, significancelab, highlevlab, binomFlag )
    colnames(output)=c(ifelse(label.name==""," ",label.name),"Sample Size",
              "Observed","Predicted","Std. Error","p-value","Significant?",
              "In Range?","Normal Approx. OK?")
  } else if( my.family=="poisson" ){
    output = data.frame( labelcol, nrefdata[,yname], predfit$fit,
      stderrPred, pval, significancelab, highlevlab, poisFlag )
    colnames(output)=c(ifelse(label.name==""," ",label.name),
              "Observed","Predicted","Std. Error","p-value","Significant?",
              "In Range?","Normal Approx. OK?")
  } else {
    output = data.frame( labelcol, nrefdata[,yname], predfit$fit,
      stderrPred, pval, significancelab, highlevlab )
    colnames(output)=c(ifelse(label.name==""," ",label.name),
              "Observed","Predicted","Std. Error","p-value","Significant?",
              "In Range?")
  }
  
  ## Plot observed versus predicted
  if (browserResults) {
    png(filename=file.path(resultLocation, paste("Predicted vs Observed",".png",sep="")), width=600, height=600)
  } else {
  	  JavaGD(height=500,width=600)
  }
  par(mar=c(4,4,2,1))
  if( my.family=="binomial" ){
    rng = range( c( fittedval, predfit$fit, yyna / nnna ), na.rm=TRUE )
  } else {
    rng = range( c( fittedval, predfit$fit, my.subset[,yname] ), na.rm=TRUE )
  }
  buffer = diff(rng)*.15
  rng = rng + c(-1,1) * buffer
  if (my.family=="binomial") {
    plot( fittedval, refdata[,yname] / nn.ref, xlim=rng, ylim=rng, col="black", pch=1,
         xlab="Predicted", ylab="Observed", main="Predicted versus Observed" )
  } else {
    plot( fittedval, refdata[,yname], xlim=rng, ylim=rng, col="black", pch=1,
         xlab="Predicted", ylab="Observed", main="Predicted versus Observed" )
  }      
  abline( 0, 1, col="gray77", lty=2 )
  legend( "topleft",
         legend=c("Reference Point","In Model Range -- Not Significant",
           "In Model Range -- Significant", "Out of Model Range -- Not Significant",
           "Out of Model Range -- Significant"),
         col=c("black","green","red","green","red"), pch=c(1,3,3,2,2), cex=0.75 )
  if( my.family=="binomial" ){
    points( predfit$fit, nrefdata[,yname] / nn.nref, col=ifelse(significance,"red","green"),
           pch=ifelse(highlev,2,3) )
  } else {
    points( predfit$fit, nrefdata[,yname], col=ifelse(significance,"red","green"),
           pch=ifelse(highlev,2,3) )
  }
  if( sum(significance)>0 ){
    if( my.family=="binomial" ){
      text( predfit$fit[significance], nrefdata[significance,yname] / nn.nref[significance], 
           labels=as.character(output[significance,1]), pos=3, cex=0.75, col="red" )
    } else {
      text( predfit$fit[significance], nrefdata[significance,yname], 
           labels=as.character(output[significance,1]), pos=3, cex=0.75, col="red" )
    }
  }
  if(browserResults) dev.off()
  
  #output results table
  if(browserResults) {
    buildresultsXML(object=list(output), location=resultLocation, title="Regression Prediction Summary")
  } else {
  	print(output)
  }
  
  return(invisible())
}
