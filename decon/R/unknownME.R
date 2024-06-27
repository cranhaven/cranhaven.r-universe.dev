npdenest <- 
    function(w, e, bw, adjust = 1, n = 512, from, to, cut = 3, na.rm = FALSE,...)  
{
    CheckValidity <- function(y,na.rm){
        if (!is.numeric(y)) 
            stop("argument 'y' must be numeric")
        name <- deparse(substitute(y))
        y <- as.vector(y)
        y.na <- is.na(y)
        if (any(y.na)) {
            if (na.rm)  y <- y[!y.na]
            else stop("'y' contains missing values")
        }
        y.finite <- is.finite(y)
        if (any(!y.finite)) {
            y <- y[y.finite]
        }
        ny <- length(y)
        list(y=y,ny=ny,name=name);
    }
    if (length(list(...)) > 0) 
        warning("non-matched further arguments are disregarded")
  
    yout <- CheckValidity(w);
    y <- yout$y;
    ny <- yout$ny;
    name <- yout$name;
    N <- ny;

    eout <- CheckValidity(e);
    e <- eout$y;
    ne <- length(e)

    if(!is.numeric(n)|n<3)
        stop(paste("log2(n) must be within the range [2,21]." ));

    if(missing(bw)){
        bw <- bw.dboot2(y,sig=sd(e), error="normal")
        isearch <- 0
    }else{
        if(!is.numeric(bw)) stop("'bw' is not numeric")
        if(bw <=0) stop("Invalid 'bw'")
        isearch <- 0
    }
    
    if (missing(from)) 
        from <- min(y) - cut * bw
    if (missing(to)) 
        to <- max(y) + cut * bw
    if (!is.finite(from)) 
        stop("non-finite 'from'")
    if (!is.finite(to)) 
        stop("non-finite 'to'")
    if(from>=to){
        stop("'from' is not smaller than 'to'!");
    }else{
        x <- seq(from,to,length=n);
    };

    nx <- length(x); ny <- length(y);
    vmin <- var(y) - var(e);
    if(vmin <= 0.0)
        stop("Var('W') <= Var('E')")
    
    fhat <- .C("NPDESupport",
               as.double(y),as.integer(ny),
               as.double(e),as.integer(ne),
               y=as.double(x),as.integer(nx),
               bw=as.double(bw), as.integer(isearch),
               vhat = as.double(vmin),
               PACKAGE='decon')
    return(structure(list(x = x,y = fhat$y,bw = fhat$bw,n = N,Sn=fhat$vhat,
                          call = match.call(), data.name = name,
                          has.na = FALSE), class = "Decon"))
}


npreg <- 
    function(w, y, e, bw, adjust = 1, n = 512, from, to, cut = 0, na.rm = FALSE,...)  
{
    CheckValidity <- function(y,na.rm){
        if (!is.numeric(y)) 
            stop("argument 'y' must be numeric")
        name <- deparse(substitute(y))
        y <- as.vector(y)
        y.na <- is.na(y)
        if (any(y.na)) {
            if (na.rm)  y <- y[!y.na]
            else stop("'y' contains missing values")
        }
        y.finite <- is.finite(y)
        if (any(!y.finite)) {
            y <- y[y.finite]
        }
        ny <- length(y)
        list(y=y,ny=ny,name=name);
    }
    if (length(list(...)) > 0) 
        warning("non-matched further arguments are disregarded")
  
    yout <- CheckValidity(w);
    w <- yout$y;
    nw <- yout$ny;

    yout <- CheckValidity(y);
    y <- yout$y;
    ny <- yout$ny;
    name <- yout$name;

    stopifnot(nw == ny)
    
    N <- ny;

    eout <- CheckValidity(e);
    e <- eout$y;
    ne <- length(e)

    if(!is.numeric(n)|n<3)
        stop(paste("log2(n) must be within the range [2,21]." ));

    if(missing(bw)){
        bw <- bw.dboot2(y,sig=sd(e), error="normal")
    }else{
        if(!is.numeric(bw)) stop("'bw' is not numeric")
        if(bw <=0) stop("Invalid 'bw'")
    }
    
    if (missing(from)) 
        from <- 0.9*min(w) - cut * bw
    if (missing(to)) 
        to <- 0.9*max(w) + cut * bw
    if (!is.finite(from)) 
        stop("non-finite 'from'")
    if (!is.finite(to)) 
        stop("non-finite 'to'")
    if(from>=to){
        stop("'from' is not smaller than 'to'!");
    }else{
        x <- seq(from,to,length=n);
    };

    nx <- length(x); ny <- length(y);
    vmin <- var(y) - var(e);
    if(vmin <= 0.0)
        stop("Var('W') <= Var('E')")
    
    fhat <- .C("NPRegSupport",
               as.double(w),as.double(y),as.integer(ny),
               as.double(e),as.integer(ne),
               y=as.double(x),as.integer(nx),
               bw=as.double(bw),
               PACKAGE='decon')

    return(structure(list(x = x,y = fhat$y,bw = fhat$bw,n = N,Sn=NULL,
                          call = match.call(), data.name = name,
                          has.na = FALSE), class = "Decon"))
}

