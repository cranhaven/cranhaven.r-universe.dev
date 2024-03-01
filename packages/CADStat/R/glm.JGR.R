#' @export
glm.JGR = function(my.data, sampsize.name=NULL, subset1.name=NULL, subset1.val=NULL, subset2.name=NULL, subset2.val=NULL,
                    my.formula, my.family="gaussian", my.contrast=NULL, iRmIntercept=FALSE, iCI=TRUE, conf.level=0.95,
                    iDiag.1=FALSE, iDiag.2=FALSE, iDiag.3=FALSE, iDiag.4=FALSE, iInfluence=FALSE, browserResults=FALSE, saveLMName="" )
{
# fits a linear regression model returning estimates and diagnostics
  
# my.data      data.frame
# subset1.name column name of the first subsetting variable (panel)
# subset1.val  values of the first subsetting variable
# subset2.name column name of the second subsetting variable (group)
# subset2.val  values of the second subsetting variable
# my.formula   string giving the model formula
#                [If binomial family, assumes a comma-separated string
#                 giving: n,y,x1,x2,x3,... (with spaces)]
# my.family    error family
# my.contrast  numeric vector giving the contrast of interest
# iRmIntercept indicates whether the model should be fit without an intercept
# iCI          indicates whether confidence intervals for model coefficients should be computed
# conf.level   confidence level
# iDiag.1      indicates whether diagnostic plot "Residual versus Fitted" should be created
# iDiag.2      indicates whether diagnostic plot "Normal Q-Q plot" should be created
# iDiag.3      indicates whether diagnostic plot "Scale-Location plot" should be created
# iDiag.4      indicates whether diagnostic plot "Cook's Distance plot" should be created
# iInfluence   indicates whether an influence plot should be created

  #get place to store results
  if (browserResults) resultLocation = genResultSpace()

  # Utility function
  stripwhite = function(x){ gsub("[[:space:]]+$","",gsub("^[[:space:]]+","",x)) }

  # NA handling
  # use options to set na.action to omit NAs which is used by lm
  options(na.action = "na.omit")

  #find proper data subset
  ind = rep(TRUE, nrow(my.data))
  if (length(subset1.val)>0) ind = ind & my.data[,subset1.name]%in%subset1.val
  if (length(subset2.val)>0) ind = ind & my.data[,subset2.name]%in%subset2.val
  my.subset = my.data[ind,]
  
  # Check for sensible binomial inputs
  if( my.family == "binomial" ) {
    vars = stripwhite(strsplit(my.formula,",")[[1]])
    if (vars[1]=="1"){
      nn = rep(1, nrow(my.subset))
    } else {
      nn = my.subset[,vars[1]]
    }
    yy = my.subset[,vars[2]]
    nna = !is.na(yy) & !is.na(nn)
    yy = yy[nna]
    nn = nn[nna]
    
    if (any((yy < 0) || (yy != as.integer(yy)))) {
      if (browserResults) {
        localJGRError("To fit data under the binomial distribution, the dependent variable must contain only non-negative integers.", resultLocation, geterr=FALSE)
      } else {
      	stop("To fit data under the binomial distribution, the dependent variable must contain only non-negative integers.", call.=FALSE)
      }
    }
    
    if (any((nn < 0 ) || (nn != as.integer(nn)))) {
      if (browserResults) {
        localJGRError("To fit data under the binomial distribution, the sample size variable must contain only non-negative integers.", resultLocation, geterr=FALSE)
      } else {
        stop("To fit data under the binomial distribution, the sample size variable must contain only non-negative integers.", call.=FALSE)
      }
    }
    
    if (any(yy > nn)) {
      if (browserResults) {
      	localJGRError("To fit data under the binomial distribution, the dependent variable must be between 0 and the corresponding value for the sample size variable.", resultLocation, geterr=FALSE)
      } else {
      	stop("To fit data under the binomial distribution, the dependent variable must be between 0 and the corresponding value for the sample size variable.", call.=FALSE)
      }
    }
    
    my.formula = paste("cbind(",vars[2],",",vars[1],"-",vars[2],")~", paste(vars[3:length(vars)],collapse="+"),sep="")
  }
  
  #check poisson inputs
  if (my.family=="poisson") {
    vars = stripwhite(strsplit(my.formula, "~")[[1]])
    yname = vars[1]
    tmpy = my.subset[,yname]
    tmpy = tmpy[!is.na(tmpy)]
    
    if (any((tmpy < 0) | (tmpy != as.integer(tmpy)))) {
      if (browserResults) {
      	localJGRError("To fit data under the Poisson distribution, the dependent variable must contain only non-negative integers.", resultLocation, geterr=FALSE )
      } else {
      	stop("To fit data under the Poisson distribution, the dependent variable must contain only non-negative integers.", call.=FALSE)
      }
    }
  }
  
  # NOTE: the remainder of the code groups subset1.val values together and
  # groups subset2.val values together rather than perform computations for
  # individual subsets

  # fit the model
  if (iRmIntercept) my.formula = paste(my.formula, "-1")
  my.fit = try( glm(as.formula(my.formula), data=my.subset, family=my.family ) )
  if (inherits(my.fit, "try-error")) {
    if (browserResults) {
      localJGRError("Error attempting to fit the generalized linear model.", resultLocation)
    } else {
      stop("Error attempting to fit the generalized linear model.", call.=FALSE)
    }
  }
  

  #Fix to set the environment and pass it in. Found in datamerge/bioinfer/glm/pca.fa
  pos <- 1
  envir = as.environment(pos)


  if (saveLMName != "" ) assign( saveLMName, my.fit, envir = envir)
  
  #output summary table
  print(summary(my.fit))
  
  #output coefficient CIs if necessary
  if (iCI) {
    # check conf.level
    if (!is.numeric(conf.level) | is.nan(conf.level)) {
      conf.level = 0.95
    } else if (conf.level <= 0 | conf.level >= 1) {
      conf.level = 0.95
    } 
    my.fit.ci = data.frame(confint(my.fit, level=conf.level))
    attr(my.fit.ci,"title") = "Model Coefficient Confidence Intervals"
    print("Model Coefficient Confidence Intervals")
    print(my.fit.ci)
  } else {
    my.fit.ci = NULL
  }
  
  # test contrast if necessary (requires gmodels)
#  if (any(my.contrast)!=0) {
#    my.fit.est = data.frame(estimable(my.fit, my.contrast))
#    attr(my.fit.est,"title") = "Test of Model Coefficient Contrast"
#    print("Test of Model Coefficient Contrast")
#    print(estimable(my.fit, my.contrast))
#  }else{
#    my.fit.est = NULL
#  }
  
  margins=c(5,6,2,2)

  #produce diagnostic plots if necessary
  iPlotLM = c(iDiag.1, iDiag.2, iDiag.3, iDiag.4)
  n.diagnostic = sum(c(iPlotLM, iInfluence))
  for (i in which(iPlotLM))
  {
    fig.name = c("Residuals vs Fitted", "Normal Q-Q", "Scale-Location", "Cooks distance", "Residuals vs Leverage", "Cooks distance vs Leverage")
    if (browserResults) {
      png(filename=file.path(resultLocation,paste(fig.name[i],".png",sep="")),width=600,height=600)
    } else {
      JavaGD(name=fig.name[i], width=500, height=400, ps=14)
    }
    par(mar=margins)
    plot(my.fit, which=i, caption=NULL, sub.caption="")
    if (browserResults) dev.off()
  }

  #produce influence plot if necessary (influencePlot requires car)
  if (iInfluence) {
  	if (browserResults) {
      png(filename=file.path(resultLocation,"Influence.png"),width=600,height=600)
    } else {
      JavaGD(name="Influence plot",width=500, height=400, ps=14)
    }
    par(mar=margins)
    influencePlot(my.fit, main="Influence Plot", labels=FALSE)
    #identify triggers warnings... auto by default possibly. Look Into using ellipses
    #influencePlot(my.fit, main="Influence Plot", labels=FALSE, identify = "auto") 
    if (browserResults) dev.off()
  }

  #create results file if browserResults
  ttl = paste(ifelse(my.family=="gaussian", "Linear", ifelse(my.family=="poisson", "Poisson", "Logistic") ), "Regression Summary")
  if (browserResults) buildresultsXML(object=list(my.fit, my.fit.ci), location=resultLocation, title=ttl)

  assign("my_glm.fit", my.fit, envir = envir)

  return(invisible())
}

