
# Missing value projection matrix
HadamardProjection4NA = function(DataM){
  PMM = 0*DataM+1
  PMM[is.na(PMM)]=0

  return(PMM = PMM)
}


# Scale
NormalizeMatrix = function(X,method=c("standard","robust","range","none"),type=c("global","rowwise","columnwise"),na.rm=TRUE){
  # get arguments
  type = match.arg(type)
  method = match.arg(method)

  # Scale with global number
  if(type=="global"){
    if(method == "standard"){
      shift = mean(X,na.rm=na.rm)
      scalar = sd(X,na.rm=na.rm)


    } else if(method == "robust"){
      shift = median(X,na.rm=na.rm)
      scalar = mad(X,constant = 1,na.rm=na.rm)

    }else if(method == "range"){
      rmax = max(X,na.rm=na.rm)
      rmin = min(X,na.rm=na.rm)
      shift = rmin
      scalar = rmax-rmin

    }else if(method == "none"){
      shift = 0
      scalar = 1
    }
    else{
      stop(paste("Method: ",method," not an option for NormalizeMatrix()",sep="",collapse=""))
    }
    
    # safely shift and scale
    scalar[scalar==0|is.na(scalar)|is.infinite(scalar)]=1
    X  = (X-shift)/scalar

  }
  else{
    if(type=="columnwise") {dm = 2}
    else if(type=="rowwise") {dm = 1}
    else if(type !="global") {stop("Invalid Scaling Type in NormalizeMatrix()")}

    if(method == "standard"){
      shift = apply(X,dm,mean,na.rm=na.rm)
      scalar = apply(X,dm,sd,na.rm=na.rm)


    } else if(method == "robust"){
      shift = apply(X,dm,median,na.rm=na.rm)
      scalar = apply(X,dm,mad,constant = 1,na.rm=na.rm)

    }else if(method == "range"){
      rmax = apply(X,dm,max,na.rm=na.rm)
      rmin = apply(X,dm,min,na.rm=na.rm)
      shift = rmin
      scalar = rmax-rmin

    }else if(method == "none"){
      shift[]=0
      scalar[]=1
    }
    
    else{
      stop(paste("Method: ",method," not an option for NormalizeMatrix()",sep="",collapse=""))
    }

    scalar[scalar==0|is.na(scalar)|is.infinite(scalar)]=1
    X = sweep(X, dm, shift, `-`)
    X = sweep(X, dm, scalar, `/`)
  }

  # return with attributes
  base::attr(X,"shift") = shift
  base::attr(X,"scale") = scalar
  base::attr(X,"scale_method") = method
  base::attr(X,"scale_type") = type
  return(X)

}

unScale = function(X,type="rowwise",Shift=0,Scale=1) {
  if(type=="columnwise") {dm = 2}
  else{dm = 1}
  X = sweep(X, dm, Scale, `*`)
  X =  sweep(X, dm, Shift, `+`)
  return(X)

}
