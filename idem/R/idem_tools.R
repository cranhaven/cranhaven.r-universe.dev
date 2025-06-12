##------------------------------------------------------
##
##           FUNCTIONS
##
##------------------------------------------------------

##get coef from lm
get.coef <- function(reg.rst) {
    c(summary(reg.rst)$sigma, coef(reg.rst));
}

##get constants
get.const <- function(cname) {
    switch(cname,
           ORG.PREFIX  = "ORIG",
           TXT.ENDP    = "ENDP",
           IDEM.CLASS  = "IDEMDATA",
           ERR.CLASS   = "IDEMERROR",
           FIT.CLASS   = "IDEMFIT",
           BENCH.CLASS = "IDEMSINGLE",
           IMP.CLASS   = "IDEMIMP",
           IMP.MICE    = "IDEMIMPMICE",
           IMP.STAN    = "IDEMIMPSTAN",
           TEST.CLASS  = "IDEMINFER",
           SACE.CLASS  = "IDEMSACE",
           "default"
           )
}

##set options
set.option <- function(x, opt.x) {
    if (is.null(x)) {
        x <- opt.x;
    } else {
        x <- x[x %in% opt.x];
        if (0 == length(x))  {
            x <- opt.x;
        }
    }

    x
}

##get pvalues
get.pval <- function(m, s) {
    apply(cbind(m,s), 1,
          function(x) {
        2*min(pnorm(x[1], 0, x[2]), 1-pnorm(x[1], 0, x[2]));
    })
}


##parse endpoint formula
get.parsed.endfml <- function(endfml, data.name="d.frame") {
    if (is.null(endfml))
        return(NULL);

    parse(text=paste("with(", data.name, ", {",endfml,"})"));
}


##get parameters in a list to the current environment
get.para <- function(lst.var, env=environment()) {
    if (!is.environment(env))
        return();

    env$vtrt     <- lst.var$trt;
    env$voutcome <- lst.var$outcome;
    env$vy0      <- lst.var$y0;
    env$vcov     <- lst.var$cov;
    env$duration <- lst.var$duration;
    env$vsurv    <- lst.var$surv;
    env$endfml   <- lst.var$endfml;
    env$unitTime <- lst.var$unitTime;
    env$trt.len  <- lst.var$trt.label;
    env$bounds   <- lst.var$bounds;

    ##parsed endpoint formula
    env$parsed.endfml <- lst.var$parsed.endfml;
    if (is.null(env$parsed.endfml))
        env$parsed.endfml <- get.parsed.endfml(lst.var$endfml);

    if (is.null(env$unitTime))
        env$unitTime <- "Days";

    if (is.null(env$trt.len))
        env$trt.len <- c('Control', 'Intervention');

    if (!is.null(env$endfml)
        & !is.null(env$voutcome)) {
        tmp <- which(sapply(env$voutcome, grepl, env$endfml));
        if (length(tmp) > 0)
            env$eoutcome <- env$voutcome[tmp];
    }

}


##decompose numbers
get.decompose <- function(numbers, digits, base=2) {

    if (digits <=0 ) return(NULL);

    numbers <- as.matrix(numbers);

    if (1 == length(base)) {
        base <- base*array(1, digits-1);
    }

    ##bs order: [100 10 0]
    bs <- NULL;
    for (i in 1:(digits-1)) {
        bs <- c(bs, prod(base[1:(digits-i)]));
    }

    ngroup <- numbers;
    value  <- NULL;
    for (j in 1:dim(numbers)[1]) {
        cur.n      <- numbers[j];
        cur.value  <- NULL;

        if (digits >= 2) {
            for (i in 1:(digits-1)) {
                curw  <- bs[i];
                curg  <- floor(cur.n / curw);
                cur.n <- cur.n - curg * curw;
                cur.value <- c(cur.value, curg);
            }
        }
        cur.value <- c(cur.value, cur.n);
        value     <- rbind(value, cur.value);
    }

    value
}

##inverse of decompose
get.compose <- function(numbers, base=2) {

    numbers <- as.matrix(numbers);
    digits  <- ncol(numbers);

    ##bs order: [100 10 0]
    bs <- NULL;
    for (i in 1:digits) {
        bs <- c(2^(i-1), bs);
    }

    rst <- apply(numbers, 1, function(x){sum(x*bs)});
    rst;
}

##get trt groups
get.trt <- function(trts) {
    rst  <- sort(unique(trts));
}

##get missing pattern
##nt:number of time points
##mono.first: put monotone pattern at first
get.miss.pattern <- function(nt, mono.first=TRUE) {
    all.pattern <- get.decompose((2^nt-1):0, nt);
    rownames(all.pattern) <- 2^nt:1;

    if (mono.first & nt>1) {
        mono <- sapply(1:(nt-1), function(x){c(rep(1, nt-x), rep(0, x))})
        mono <- rbind(rep(1, nt), t(mono));
        inx.mono <- 2^nt - get.compose(mono);
        rst <- all.pattern[inx.mono,];
        rst <- rbind(rst, all.pattern[-inx.mono,]);
    } else {
        rst <- all.pattern;
    }

    rst
}

##read all data
get.all.data <- function(fname) {
    data.all        <- read.table(fname, header=TRUE);
    names(data.all) <- toupper(names(data.all));
    data.all$pid    <- 1:nrow(data.all);
    data.all
}


##transfer data to -inf to inf
get.transfer <- function(x, bounds) {

    if (is.null(bounds))
        return(x);

    ux  <- (x - bounds[1])/(bounds[2] - bounds[1]);
    rst <- log(ux/(1-ux));
    rst
}


##transfer back to original scale
get.inv.transfer <- function(x, bounds){
    if (is.null(bounds))
        return(x);

    ux  <- exp(x)/(1+exp(x));
    rst <- bounds[1] + ux * (bounds[2] - bounds[1]);
    rst
}


##get jacobian for data transfermation
get.jacob <- function(x, bounds) {
    if (is.null(bounds)) {
        dphi <- 1/(x-bounds[1]) + 1/(bounds[2]-x);
    } else {
        dphi <- 1;
    }
    rst  <- prod(dphi);
}

##transfer all data set
get.transfer.all <- function(data.all, lst.var, bounds=lst.var$bounds, inverse=FALSE) {
    if (is.null(bounds))
        return(data.all);

    ally <- lst.var$outcome;
    for (i in 1:length(ally)) {
        if (!inverse) {
            data.all[, ally[i]] <- get.transfer(data.all[, ally[i]], bounds);
        } else {
            data.all[, ally[i]] <- get.inv.transfer(data.all[, ally[i]], bounds);
        }
    }

    data.all
}


## Check if the \code{idem-parameters} are correctly specified and consistent
## with the data
chk.pars <- function(data.all, lst.var) {

    if (is.null(data.all)|is.null(lst.var)) {
        err.msg <- "Please provide a valid dataset with necessary specifications."
        class(err.msg) <- get.const()$ERR.CLASS;
        return(err.msg);
    }

    ep <- function(msg) {
        c(err.msg, msg);
    }

    err.msg  <- NULL;

    ##treatment
    if (0 == length(lst.var$trt)) {
        err.msg <- ep("No treatment specified");
    } else if (1 < length(lst.var$trt)) {
        err.msg <- ep("More than one treatment specified");
    } else {
        chk.trt <- data.all[, lst.var$trt];
        if (2 != length(unique(chk.trt)))
            err.msg <- ep("The treatment column has different than 2 levels.");
    }

    ## if (!is.null(lst.var$trt.label)) {
    ##     if (2 != lst.var$trt.label)
    ##         err.msg <- ep("The treatment label is not length 2.");
    ## }

    if (0 == length(lst.var$surv))
        err.msg <- ep("No survival time specified");
    if (1 < length(lst.var$surv))
        err.msg <- ep("More than one survival time specified");
    if (0 == length(lst.var$outcome))
        err.msg <- ep("No outcome specified");

    var.out <- c(lst.var$outcome, lst.var$y0);

    ##endpoints
    if (is.null(lst.var$endfml)) {
        err.msg <- ep("Please specify endpoint");
    } else if (0 == nchar(gsub("\\s","",lst.var$endfml))) {
        err.msg <- ep("Please specify endpoint");
    } else {
        chk.1 <- try({exp.end <- parse(text=lst.var$endfml);})
        if ("try-error" == class(chk.1)) {
            err.msg <- ep(paste("Endpoint error:", chk.1[1], sep=""));
        } else if (1 < length(exp.end)) {
            err.msg <- ep("Endpoint expression contains more than one line");
        } else {
            chk.end <- NULL;
            eval(parse(text=paste("chk.end <- try(with(data.all[, var.out, drop = FALSE],
                                  {",lst.var$endfml,"}), silent=T)"
                                  )
                       )
                 );
            if ("try-error" == class(chk.end))
                err.msg <- ep( paste("Endpoint formula error: ",
                                     gsub("[\r\n]", "", chk.end[1]), sep=""));
        }
    }

    ##duration
    if (is.null(lst.var$duration)) {
        err.msg <- ep("Study duration is not specified");
    } else if (is.na(lst.var$duration) | lst.var$duration <=0) {
        err.msg <- ep("Study duration is not a proper positive number");
    }

    ##boundary
    if (!is.null(lst.var$bounds)) {
        if (any(is.na(lst.var$bounds))) {
            err.msg <- ep("Lower or upper bound is not specified");
        } else {
            if (lst.var$bounds[1] > min(data.all[, var.out], na.rm=TRUE))
                err.msg <- ep("Lower bound is bigger than some observed outcomes");

            if (lst.var$bounds[2] < max(data.all[, var.out], na.rm=TRUE))
                err.msg <- ep("Upper bound is smaller than some observed outcomes");
        }
    }

    ##return
    if (!is.null(err.msg))
        class(err.msg) <- get.const("ERR.CLASS");
    err.msg;
}

##get missing frequency table
get.mis.table <- function(data.all, lst.var) {
    vtrt     <- NULL;
    voutcome <- NULL;
    eoutcome <- NULL;
    vsurv    <- NULL;
    duration <- NULL;
    endfml   <- NULL;
    tmp.endp <- NULL;
    vy0      <- NULL;
    bounds   <- NULL;

    get.para(lst.var, environment());

    data.all <- as.matrix(data.all);
    a.trt    <- get.trt(data.all[,vtrt]);
    mis.pat  <- get.miss.pattern(length(voutcome));

    mis.pat[mis.pat == 1] <- 'Observed';
    mis.pat[mis.pat == 0] <- 'Missing';

    if (is.null(trt.len)) {
        trt.len <- paste(toupper(vtrt), "=", a.trt, sep="");
    }

    rst <- NULL;
    for (i in 1:length(a.trt)) {
        subg      <- data.all[which(a.trt[i] == data.all[,vtrt]),
                              c(vsurv, voutcome)];
        nsub      <- nrow(subg);
        cur.alive <- subg[which(subg[,vsurv] > duration),voutcome];
        n.dead    <- nsub-nrow(cur.alive);

        cur.y <- !is.na(cur.alive);
        inx.y <- table(1+get.compose(cur.y));

        n.p               <- rep(0, nrow(mis.pat));
        names(n.p)        <- rownames(mis.pat);
        n.p[names(inx.y)] <- inx.y;

        char.rst <- sapply( c(n.dead, n.p), function(x) { sprintf("%i (%.0f%%)", x, 100*x/nsub)});
        char.rst <- c(char.rst, nsub);
        rst      <- cbind(rst, char.rst);
    }

    rst <- cbind(rbind("", mis.pat, ""), rst);
    colnames(rst) <- c(voutcome, trt.len);
    rownames(rst) <- c("Deaths on study",
                       paste("S=", 1:nrow(mis.pat), sep=""),
                       "Total");

    data.frame(rst)
}

## Get subjects that need imputation
get.needimp <- function(data.all, lst.var, endponly=FALSE) {

    voutcome <- NULL;
    eoutcome <- NULL;
    vsurv    <- NULL;
    duration <- NULL;
    endfml   <- NULL;
    tmp.endp <- NULL;

    ##get parameters in current enviroment
    get.para(lst.var, environment());

    ##chk subjects need imputation
    if (endponly) {
        vendp <- eoutcome;
    } else {
        vendp <- voutcome;
    }

    need.imp <- apply(data.all,
                      1,
                      function(x) {
        rst <- (x[vsurv] > duration) & any(is.na(x[vendp]));
    });

    need.imp <- which(need.imp);
    if (0 == length(need.imp)) {
        return(NULL);
    } else {
        need.imp;
    }
}

## get missing patter of a given dataset
get.dta.pattern <- function(data.outcome) {
    nt <- ncol(data.outcome);
    bs <- NULL;
    for (i in 1:nt) {
        bs <- c(2^(i-1), bs);
    }

    apply(is.na(data.outcome), 1, function(x) {
        sum(x * bs);
    })
}

##------------------------------------------------------
##
##           Multivariate Normal
##
##------------------------------------------------------

# Returns conditional mean and variance of x[req.ind]
# Given x[given.ind] = x.given
# where X is multivariate Normal with
# mean = mu and covariance = sigma
#
get.cond.Normal <- function(mu, sigma, vec.y) {
    ##missing
    req.ind <- which(is.na(vec.y));

    if (length(mu) == length(req.ind)) {
        rst <- list(mu=mu, sigma=sigma);
    } else if (0 == length(req.ind)) {
        rst <- NULL;
    } else {
        given.ind <- which(!is.na(vec.y));
        B         <- sigma[req.ind, req.ind];
        C         <- sigma[req.ind, given.ind, drop=FALSE];
        D         <- sigma[given.ind, given.ind];
        CDinv     <- C %*% solve(D);
        cMu       <- c(mu[req.ind] + CDinv %*% (vec.y[given.ind] - mu[given.ind]));
        cVar      <- B - CDinv %*% t(C);
        rst       <- list(mu=cMu, sigma=cVar);
    }

    ##return
    rst
}


##get joint normal from the sequential model fitting results
get.joint.Normal.sigma <- function(fit.trt) {
    cur.trt <- fit.trt;
    ny      <- length(cur.trt);
    sigma   <- array(NA, dim=c(ny, ny));

    for (i in 1:ny) {
        cur.coef   <- cur.trt[[i]]$coef;
        sigma[i,i] <- cur.coef[1]^2;
        if (1 == i) next;

        py <- cur.coef[3:(3+i-1)];

        for (j in 1:(i-1)) {
            sigma[i,i] <- sigma[i,i] + py[j]^2*sigma[j,j];
            sigma[i,j] <- sigma[j,i] <- sum(py * sigma[1:(i-1), j]);
        }
    }

    sigma;
}

##get joint normal mus
get.joint.Normal.mu <- function(cov.x, fit.trt) {
    cur.trt <- fit.trt;
    ny      <- length(cur.trt);
    mu      <- rep(NA, ny);

    for (i in 1:ny) {
        cur.coef <- cur.trt[[i]]$coef;
        if (1 == i) {
            prev.y <- NULL;
        } else {
            prev.y <- mu[1:(i-1)];
        }
        mu[i] <- sum(cur.coef[-1] * c(1, prev.y, cov.x));
    }
    mu;
}

##compute kernel density band
get.band.h <- function(res) {
    rst <- 1.06*sd(res)*(length(res)^(-0.2));
}

##kernel density pdf
c.kdpdf <- function(err, res, h=NULL, log.v=TRUE, ...) {

    if (is.null(h)) {
        h <- get.band.h(res);
    }

    tmp <- 0;
    rst.c <- .C("kdpdf",
                as.double(err), as.double(res),
                as.double(h),   as.integer(length(res)),
                as.double(tmp));
    rst <- rst.c[[5]];
    if (log.v) {
        rst <- log(rst);
    }

    rst
}



##------------------------------------------------------
##
##           Imputation
##
##------------------------------------------------------



##exponetial tilting
imp.exponential <- function(imp.single, deltas=0, n.imp=5, maxiter=1000) {
    stopifnot(class(imp.single) == get.const("BENCH.CLASS"));
    imp.single <- imp.single$complete;

    all.z <- imp.single[, get.const("TXT.ENDP")];
    n.z   <- length(all.z);

    rst <- NULL;
    for (i in 1:length(deltas)) {
        cur.bz  <- all.z * deltas[i];
        cur.M   <- max(cur.bz);
        cur.imp <- NULL;
        j       <- 1;
        while (length(cur.imp) < n.imp & j < (n.imp*maxiter)) {
            tmp.sub <- sample(1:n.z, 1);
            tmp.p   <- exp(cur.bz[tmp.sub] - cur.M);
            if (runif(1) < tmp.p) {
                cur.imp <- c(cur.imp, tmp.sub);
                j       <- j + 1;
            }
        }

        if (j > maxiter)
            stop("The MCMC chain is not long enough for the requested number of imputations.");

        ##add to rst
        rst <- rbind(rst,
                     cbind("DELTA" = deltas[i],
                           "IMP"   = 1:n.imp,
                           imp.single[cur.imp,]));
    }

    rst
}


##------------------------------------------------------
##
##           RANK ANALYSIS
##
##------------------------------------------------------
c.rankij <- function(val.i, val.j, duration, cut.surv=0, cut.z=0, ...) {
    tmp <- 0;
    rst.c <- .C("rankij",
                as.double(val.i[1]), as.double(val.i[2]),
                as.double(val.j[1]), as.double(val.j[2]),
                as.double(duration),
                as.double(cut.surv),
                as.double(cut.z),
                as.integer(tmp));
    rst <- rst.c[[8]];
    rst
}

##get the rank test statistic \theta
c.rankall <- function(val.trt1, val.trt2, duration, cut.surv=0, cut.z=0, ...) {
    tmp <- 0;
    rst.c <- .C("rankall",
                as.double(t(val.trt1)), as.double(t(val.trt2)),
                as.integer(nrow(val.trt1)), as.integer(nrow(val.trt2)),
                as.double(duration), as.double(cut.surv), as.double(cut.z),
                as.double(tmp));
    rst <- rst.c[[8]];
    rst
}

##bubble sort for ordering subjects from one trt to get median
c.bubblesort <- function(val.trt, duration, cut.surv=0, cut.z=0, ...) {

    if (1 == nrow(val.trt))
        return(c(val.trt,1));

    v <- cbind(val.trt, 1:nrow(val.trt));
    rst.c <- .C("bsort",
                as.double(t(v)),
                as.integer(nrow(v)),
                as.double(duration),
                as.double(cut.surv),
                as.double(cut.z));
    rst <- rst.c[[1]];
    dim(rst) <- c(3, nrow(val.trt));
    t(rst);
}


##get median
get.median <- function(val.trt, duration, effect.quantiles=0.5, ...) {
    sort.val <- c.bubblesort(val.trt, duration, ...);
    med.inx  <- quantile(1:nrow(sort.val), probs=effect.quantiles);
    cbind(effect.quantiles,
          sort.val[ceiling(med.inx), 1:2, drop=FALSE])
}

##get rank and median from complete (after imputation) dataset
get.data.ready <- function(data.full, lst.var, atrt) {

    if (0 == nrow(data.full))
        return(NULL);

    vtrt     <- lst.var$trt;
    duration <- lst.var$duration;
    vsurv    <- lst.var$surv;

    endp <- get.const("TXT.ENDP");

    ##deceased
    inx.d <- which(data.full[,vsurv] <= duration);
    if (length(inx.d) > 0) {
        data.dead <- data.full[inx.d, c(vtrt, vsurv)];
        ##this -1 is for c program sorting
        ##should not be changed to na
        data.dead[,as.character(endp)] <- -1;
    } else {
        data.dead <- NULL;
    }

    ##alive
    inx.a <- which(data.full[,vsurv] > duration);
    if (length(inx.a) > 0) {
        data.alive <- data.full[inx.a, c(vtrt, vsurv, endp)];
    } else {
        data.alive <- NULL;
    }

    ##
    dat.red <- rbind(data.dead, data.alive);
    rst     <- list(NULL);
    for (i in 1:length(atrt)) {
        rst[[i]] <- dat.red[which(atrt[i] == dat.red[,1]), 2:3];
    }
    rst
}

##get composite endpoint
get.comp <- function(y, surv, duration=NULL) {
    rst <- y;
    if (is.null(duration)) {
        inx <- which(!is.na(surv));
    } else {
        inx <- which(surv <= duration);
    }

    if (length(inx) > 0) {
        ##avoid inf when y is all NA
        if (all(is.na(y))) {
            min.y <- 0;
        } else {
            min.y <- min(y, na.rm=TRUE);
        }

        rst[inx] <-  min.y - max(surv, na.rm=TRUE) - 1000 + surv[inx];
    }
    rst
}

##treatment effect estimation
get.estimate <- function(imp.rst,
                         effect.quantiles=0.5,
                         ...) {

    if (is.null(imp.rst))
        return(NULL);

    stopifnot(any(class(imp.rst) == get.const("IMP.CLASS")));

    lst.var  <- imp.rst$lst.var;
    imp.data <- imp.rst$complete;
    deltas   <- imp.rst$deltas;
    n.imp    <- imp.rst$n.imp;

    vtrt     <- lst.var$trt;
    duration <- lst.var$duration;
    vsurv    <- lst.var$surv;

    ##trt arms
    atrt <- sort(unique(imp.data[, vtrt]));

    ##not imputed subjects
    sub.noimp   <- imp.data[is.na(imp.data$IMP),,drop=FALSE];
    ready.noimp <- get.data.ready(sub.noimp, lst.var, atrt);

    rst.median   <- NULL;
    rst.rank     <- NULL;
    for (i in 1:n.imp) {
        tmp.lst <- rep(list(NULL), length(deltas));
        for (j in 1:length(deltas)) {
            cur.data     <- subset(imp.data, imp.data$DELTA == deltas[j] & imp.data$IMP == i);
            cur.ready    <- get.data.ready(cur.data, lst.var, atrt);
            tmp.lst[[j]] <- rep(list(NULL), length(atrt));

            for (k in 1:length(atrt)) {
                tmp.lst[[j]][[k]] <- rbind(ready.noimp[[k]], cur.ready[[k]]);
                c.med             <- get.median(tmp.lst[[j]][[k]], duration,
                                                effect.quantiles=effect.quantiles, ...);
                rst.median        <- rbind(rst.median,
                                           cbind(i, deltas[j], atrt[k], c.med));
            }
        }

        for (t1 in 1:length(deltas)) {
            for (t2 in 1:length(deltas)) {
                cur.rank <- c.rankall(tmp.lst[[t1]][[1]],
                                      tmp.lst[[t2]][[2]],
                                      duration, ...);
                rst.rank <- rbind(rst.rank,
                                  c(i, deltas[t1], deltas[t2], cur.rank));
            }
        }
    }

    colnames(rst.median) <- c("Imputation", "Delta", "TRT", "Q", "QuantSurv", "QuantY");
    colnames(rst.rank)   <- c("Imputation", "Delta0", "Delta1", "Theta");

    ##median of median
    dfmedian       <- data.frame(rst.median);
    dfmedian$Quant <- get.comp(dfmedian$QuantY, dfmedian$QuantSurv, lst.var$duration);
    dfmedian       <- sqldf('select * from dfmedian order by Delta, Trt, Q, Quant');
    inx.median     <- ceiling(n.imp/2);
    median.median  <- dfmedian[seq(inx.median, nrow(dfmedian), by=n.imp),
                               c("Delta", "TRT", "Q", "QuantY", "QuantSurv")];

    inx.2 <- which(median.median$QuantSurv <= lst.var$duration);
    if (length(inx.2) > 0) {
        median.median[inx.2,  "QuantY"]    <- NA;
        median.median[-inx.2, "QuantSurv"] <- NA;
    }

    ##average rank
    dfrank  <- data.frame(rst.rank);
    avg.rank <- sqldf('select Delta0, Delta1, avg(Theta) as Theta
                       from dfrank group by Delta1, Delta0');

    ##survivor functional means
    dalive       <- imp.data[imp.data[,vsurv] > duration, , drop=FALSE];
    rst.survivor <- NULL;
    if (nrow(dalive) > 0) {
        ##there exist survivors
        endp    <- get.const("TXT.ENDP");
        txt.sql <- paste("select delta, trt, avg(endp) as Mean",
                         "from (select distinct delta, ", vtrt, " as trt, ID,",
                         "avg(", endp, ") as endp",
                         "from dalive group by delta, ID) group by delta, trt",
                         sep=" ");
        survtrt    <- sqldf(txt.sql);
        survtrt.t0 <- survtrt[seq(1, nrow(survtrt)-1, 2), c("delta", "Mean")];
        survtrt.t1 <- survtrt[seq(2, nrow(survtrt), 2), c("delta", "Mean")];
        ndelta     <- nrow(survtrt.t0);
        rst.survivor <- cbind(survtrt.t0[ rep(1:ndelta, each = ndelta),],
                              survtrt.t1[ rep(1:ndelta, ndelta),]);

        colnames(rst.survivor) <- c("Delta0", "Mean0", "Delta1", "Mean1");
        rst.survivor      <- data.frame(rst.survivor);
        rst.survivor$Diff <- rst.survivor$Mean1 - rst.survivor$Mean0;
    }

    rst <- list(lst.var  = lst.var,
                deltas   = imp.rst$deltas,
                imp.rst  = imp.rst,
                effect.quantiles=median.median,
                theta=avg.rank,
                survivor=rst.survivor,
                raw.theta=rst.rank,
                raw.quantiles=rst.median);

    rst
}


##------------------------------------------------------
##
##           BOOTSTRAP
##
##------------------------------------------------------

##get bootstrap sample
get.bs.sample <- function(data.all=NULL, lst.var, a.trt=NULL, bs=TRUE) {

    ## treatment name
    vtrt <- lst.var$trt;

    ##return original dataset
    if (!bs)
        rst <- 1:nrow(data.all);

    ##
    if (is.null(a.trt))
        a.trt <- sort(unique(data.all[,vtrt]));

    rst <- NULL;
    for (i in 1:length(a.trt)) {
        cur.inx <- which(a.trt[i] == data.all[,vtrt]);
        cur.smp <- sample(cur.inx, length(cur.inx), TRUE);
        rst     <- c(rst, cur.smp);
    }

    rst;
}

get.sum.rank <- function(mat.rank) {
    n.imp    <- max(mat.rank[,1]);
    n.each   <- nrow(mat.rank)/n.imp;
    m.r      <- mat.rank[,ncol(mat.rank)];
    dim(m.r) <- c(n.each, n.imp);
    rst      <- cbind(mat.rank[1:n.each, 2:3],
                      apply(m.r, 1, mean));
}

get.sum.median <- function(mat.median, offset=10000) {
    n.imp    <- max(mat.median[,1]);
    n.each   <- nrow(mat.median)/n.imp;

    m.r      <- apply(mat.median, 1, function(x) {
                      if (!is.na(x[5])) {
                          rst <- x[5];
                      } else {
                          rst <- x[4] - offset;
                      }
                      rst
                      });
    dim(m.r) <- c(n.each, n.imp);
    rst      <- cbind(mat.median[1:n.each, 2:3],
                      apply(m.r, 1, median));
}


##combine bootstrap results if using cluster
combine.boot.all <- function(n.boot, prefix="boot_rst", rst.name="rst.bs") {
    rst <- rep(list(NULL), n.boot);
    for (i in 1:n.boot) {
        cur.f <- paste(prefix, i, ".Rdata", sep="");
        load(cur.f);
        cur.rst  <- get(rst.name);
        rst[[i]] <- cur.rst[[1]];
    }
    rst
}

##bootstrap single case
get.boot.single <- function(data.all,
                            lst.var,
                            deltas = 0,
                            boot=TRUE,
                            n.imp=5,
                            normal=TRUE,
                            use_mice = FALSE,
                            imp.par = NULL,
                            ...) {
    goodImp <- FALSE;
    while (!goodImp) {
        rst <- tryCatch({
            if (boot) {
                smp.inx  <- get.bs.sample(data.all, lst.var);
                cur.smp  <- data.all[smp.inx,];
            } else {
                cur.smp <- data.all;
            }

            cur.data <- do.call(imData, c(list(cur.smp), lst.var))
            if (use_mice) {
                cur.full <- do.call(imImpAll_mice,
                                    c(list(im.data = cur.data,
                                           deltas  = deltas,
                                           n.imp   = n.imp),
                                      imp.par))
            } else {
                fit.rst  <- imFitModel(cur.data);
                cur.full <- do.call(imImpAll,
                                    c(list(fit.rst=fit.rst,
                                           normal=normal,
                                           n.imp=n.imp,
                                           deltas=deltas
                                           ),
                                      imp.par))
            }
            rst      <- get.estimate(cur.full, ...);
            goodImp  <- TRUE;
            rst
        }, error = function(e) {
            goodImp <- FALSE;
			     	print('Error in Imputation! Resampling bootstrap sample');
			     	print(e);
        })
    }
    rst
}

##hypothesis testing
get.tests <- function(rst.org, rst.boot, duration, quantiles=c(0.025,0.975)) {
    ##bootstrap
    meta  <- rep(list(NULL), 2);
    rst   <- rep(list(NULL), 2);

    for (i in 1:length(rst.boot)) {
        cur.rank <- rst.boot[[i]]$theta;
        cur.surv <- rst.boot[[i]]$survivor;

        if (1 == i) {
            meta[[1]] <- cur.rank[, -ncol(cur.rank)];
            meta[[2]] <- cur.surv[, -ncol(cur.surv)];
        }

        rst[[1]] <- cbind(rst[[1]], cur.rank[,ncol(cur.rank)]);
        rst[[2]] <- cbind(rst[[2]], cur.surv[,ncol(cur.surv)]);
    }
    rsd   <- cbind(meta[[1]], SD = apply(rst[[1]], 1, sd));
    rsurv <- cbind(meta[[2]], SD = apply(rst[[2]], 1, sd));

    rst.qs  <- array(NA, dim=c(nrow(rst.boot[[1]]$effect.quantiles),
                               length(rst.boot),
                               2));

    for (i in 1:length(rst.boot)) {
        cur.med     <- rst.boot[[i]]$effect.quantiles;
        rst.qs[,i,] <- as.matrix(cur.med[, c("QuantY", "QuantSurv")]);
    }

    rqs  <- NULL;
    inxs <- ceiling(quantile(1:length(rst.boot), quantiles));
    for (i in 1:nrow(rst.boot[[1]]$effect.quantiles)) {
        cur.qs  <- rst.qs[i,,];
        cur.qsc <- get.comp(cur.qs[,1], cur.qs[,2], duration);
        cur.ord <- order(cur.qsc);

        cur.q   <- NULL;
        cur.qi  <- NULL;
        for (j in 1:length(inxs)) {
            cur.i <- cur.ord[inxs[j]];
            if (is.na(cur.qs[cur.i,1])) {
                cur.q  <- c(cur.q,  cur.qs[cur.i, 2]);
                cur.qi <- c(cur.qi, 1);
            } else {
                cur.q  <- c(cur.q,  cur.qs[cur.i, 1]);
                cur.qi <- c(cur.qi, 0);
            }
        }
        names(cur.q)  <- paste("Q", quantiles*100, sep = "");
        names(cur.qi) <- paste(names(cur.q), "_Surv", sep = "");
        cur.rst       <- c(cur.q, cur.qi);
        rqs           <- rbind(rqs, cur.rst);
    }
    row.names(rqs) <- NULL;
    rqs            <- data.frame(rqs);


    ##original
    orank <- rst.org$theta;
    omed  <- rst.org$effect.quantiles;
    osurv <- rst.org$survivor;

    ##rank
    rstrank <- sqldf("select a.*, b.sd
                      from orank a
                      left join rsd b on (a.Delta0 = b.Delta0 and a.Delta1 = b.Delta1)");

    ##rank confidence interval
    for (j in 1:length(quantiles)) {
        cur.q            <- paste("Q", quantiles[j]*100, sep = "");
        rstrank[[cur.q]] <- rstrank[,"Theta"] + rstrank[,"SD"] * qnorm(quantiles[j]);
    }

    rstrank$PValue<- apply(rstrank,
                           1,
                           function(x) { 2*min(pnorm(x[3],0,x[4]),
                                               1-pnorm(x[3],0,x[4]))});
    ##quantiles
    rstquan  <- cbind(omed, rqs);

    ##survivors
    rstsurv <- sqldf("select a.delta0, a.delta1, a.diff, b.sd
                      from osurv a
                      left join rsurv b on (a.Delta0 = b.Delta0 and a.Delta1 = b.Delta1)");

    ##rstsurv$PValue<- apply(rstsurv,
    ##                       1,
    ##                       function(x) { 2*min(pnorm(x[3],0,x[4]),
    ##                                           1-pnorm(x[3],0,x[4]))});

    rtn <- list(theta=rstrank,
                effect.quantiles=rstquan,
                survivor=rstsurv);

    rtn
}

##print theta and quantiles for selected delta
get.theta.quant <- function(object, delta0 = NULL, delta1 = NULL) {
    cat("\nTreatment effect (theta) under different \nsensitivity parameters are: \n\n");

    dtheta <- object$theta;
    if (!is.null(delta0)) {
        dtheta <- subset(dtheta, dtheta$Delta0 %in% delta0);
    }

    if (!is.null(delta1) & 0 < nrow(dtheta)) {
        dtheta <- subset(dtheta, dtheta$Delta1 %in% delta1);
    }

    x <- as.matrix(dtheta);
    format(x, digits = max(3L, getOption("digits") - 3L));
    print(x);

    cat("\nTreatment effect (quantiles) under different \nsensitivity parameters are: \n\n");
    if (is.null(delta0)) {
        dquant <- object$effect.quantiles;
    } else {
        dquant <- subset(object$effect.quantiles, object$effect.quantiles$Delta %in% delta0);
    }

    x <- as.matrix(dquant);
    format(x, digits = max(3L, getOption("digits") - 3L));
    print(x);
    invisible(list(dtheta, dquant))
}

##------------------------------------------------------
##
##           Plot
##
##------------------------------------------------------
plot.survivor <- function(x,
                           lst.var,
                           fname=NULL,
                           ...) {

    data.all <- x;

    vsurv     <- NULL;
    duration  <- NULL;
    vy0       <- NULL;
    voutcome  <- NULL;
    vtrt      <- NULL;
    trt.len   <- NULL;

    get.para(lst.var, env=environment());

    ## change completers to survivors
    data.all <- as.matrix(data.all);
    data.all <- data.all[data.all[, vsurv] > duration,]

    all.y <- data.all[,c(vy0, voutcome)];
    ylims <- range(all.y, na.rm=TRUE);
    all.x <- seq(0, duration, length.out=length(c(vy0, voutcome)));
    a.trt <- sort(unique(data.all[,vtrt]));

    times <- as.numeric(substr(voutcome, 2, nchar(voutcome)));

    if (sum(!is.na(times)) == length(voutcome)) {
        if(is.null(vy0)) {
            all.x <- times;
        } else {
            all.x <- c(0, times);
        }
    }

    if (!is.null(fname))
        pdf(file=fname, ...);

    par(cex.lab=0.9,
        mfrow=c(1,length(a.trt)),
        mar=c(4,4,2,2),
        cex.axis=0.9);

    for (i in 1:length(a.trt)) {
        cur.data <- data.all[which(a.trt[i] == data.all[,vtrt]),
                             c(vy0,voutcome)];

        if (is.null(vy0)) {
            t0 <- NULL;
        } else {
            t0 <- 'y0';
        }

        plot(NULL,
             xlim=(range(all.x)), ylim=ylims,
             axes=F,
             xlab="Functional Outcome",
             ylab="Observed Value",
             main=trt.len[i]);

        axis(1, at=all.x, labels=c(t0, voutcome));
        axis(2, at=round(seq(ylims[1],ylims[2],length=5)))

        for (j in 1:nrow(cur.data)) {
            cur.y <- cur.data[j,];
            cur.x <- all.x
            cols <- 'black'
            if(sum(is.na(cur.y)) == length(all.x)) next
            if(any(is.na(cur.y))){
                cols <- 'purple'
                cur.x <- all.x[-which(is.na(cur.y))]
                cur.y <- cur.y[-which(is.na(cur.y))]
            }
            lines(cur.x, cur.y,type='b',pch=16, col = cols)

        }
        lines(all.x, colMeans(cur.data,na.rm=TRUE),col='red',type='b',lwd=5.5)
    }

    if (!is.null(fname))
        dev.off();

}

plot.mispattern <- function(x,
                            lst.var,
                            cols=c("blue", "gray"),
                            order.by = c("pattern", "amount"),
                            fname=NULL, ...) {
    
    data.all <- x;

    voutcome <- NULL;
    vtrt     <- NULL;

    order.by <- match.arg(order.by);

    get.para(lst.var, environment());

    n.time  <- length(voutcome);
    n.sub   <- max(table(data.all[,vtrt]));
    a.trt   <- sort(unique(data.all[,vtrt]));

    if (is.null(trt.len)) {
        trt.len <- paste(toupper(vtrt), "=", a.trt, sep="");
    }

    if (!is.null(fname))
        pdf(file=fname, ...);

    par(cex.lab=1, mfrow=c(1,length(a.trt)), mar=c(4,2,2,2), cex.axis=1);

    for (i in 1:length(a.trt)) {
        cur.data <- data.all[which(a.trt[i] == data.all[,vtrt]),
                             voutcome];

        ##order subjects
        o.inx <- switch(order.by,
                        amount  = order(apply(is.na(cur.data),1,sum)),
                        pattern = order(get.dta.pattern(cur.data)))

        cur.data <- cur.data[o.inx,];
        plot(NULL, NULL, xlim=c(0.5, n.time+0.5), ylim=c(0, n.sub+8), axes=FALSE,
             xlab="Outcomes", ylab="Subjects", main=trt.len[i]);
        axis(1, at=1:n.time, labels=voutcome);
        box();
        for (j in 1:nrow(cur.data)) {
            for (k in 1:n.time) {
                if (is.na(cur.data[j,k])) {
                    mis <- 1;
                } else {
                    mis <- 0;
                }

                rect(k-0.48, j-1, k+0.48, j, col=cols[mis+1],
                     border=FALSE);
            }
        }
    }
    if (!is.null(fname))
        dev.off();
}

plot.surv <- function(x,
                       lst.var,
                       cols=c("black", "blue"),
                       fname=NULL, ...) {

    data.all <- x;

    vsurv    <- NULL;
    vtrt     <- NULL;
    duration <- NULL;
    unitTime <- NULL;

    get.para(lst.var, environment());
    vtime    <- data.all[, vsurv];
    grp      <- data.all[,vtrt];

    vevent   <- rep(1, nrow(data.all));
    vevent[which(duration < vtime)] <- 0;
    vtime[which(duration < vtime)]  <- duration;

    ##by group
    sfit   <- survfit(Surv(vtime, vevent) ~ grp);
    sdif   <- survdiff(Surv(vtime, vevent) ~ grp);
    p.val  <- 1 - pchisq(sdif$chisq, length(sdif$n) - 1);

    if (!is.null(fname))
        pdf(fname, ...);

    par(cex.lab=1.3,las=1)
    ylims <- c(min(sfit$lower),1)
    plot(sfit,
         xlab=paste("Time (", unitTime,")", sep=''),
    		 ylab="Survival Probability",
         ylim=ylims,yaxt='n',
         cex=1, conf.int=F, lty=c(1,2), lwd=2, col=cols,
         mark.time=FALSE,
         main = 'Survival Curves');

    axis(2, at=round(seq(ylims[1],ylims[2],len=5),2),
         labels=100*round(seq(ylims[1],ylims[2],len=5),2))
    box(lty='solid')
    grid(5,5);

    if (is.null(trt.len)) {
        trt.len <- paste(vtrt, "=", levels(as.factor(grp)), sep="");
    }

    legend("topright",
           legend=c(trt.len, sprintf("p-value = %5.3f",p.val)),
           lty=c(1,2,0),
           col=c(cols,'black'),
           bty="n", cex=1.2);

    if (!is.null(fname))
        dev.off();
}

## Plot density of imputed values and the density of the observed outcomes
plot.imputed <- function(x,
                         fname=NULL,
                         deltas=0,
                         endp=FALSE,
                         adj=1.5,
                         cols=c("red","cyan","blue","green","brown"),
                         ltys=rep(1, 6),
                         xlim=NULL,
                         ylim=NULL,
                         mfrow=NULL,
                         to.plot=NULL,
                         ...) {

    imp.rst <- x;               

    if (is.null(imp.rst))
        return(NULL);

    stopifnot(any(class(imp.rst) == get.const("IMP.CLASS")));
    stopifnot(all(deltas %in% imp.rst$deltas));

    lst.var    <- imp.rst$lst.var;
    imp.data   <- imp.rst$complete;

    TXT.ENDP   <- get.const("TXT.ENDP");
    ORG.PREFIX <- get.const("ORG.PREFIX");
    vtrt       <- NULL;
    voutcome   <- NULL;
    endfml     <- NULL;
    get.para(lst.var, environment());

    ##get trt
    a.trt   <- sort(unique(imp.data[,vtrt]));
    n.trt   <- length(a.trt);
    n.delta <- length(deltas);

    if (endp) {
        to.plot <- TXT.ENDP;
    } else {
        if (is.null(to.plot))
            to.plot <- voutcome;
    }

    n.y <- length(to.plot);

    ##get densities
    lst.den <- rep(list(NULL), n.trt);
    lst.obs <- rep(list(NULL), n.trt);

    maxy <- 0;
    for (i in 1:n.trt) {
        ##observed
        lst.obs[[i]] <- list(NULL);
        inx          <- which(a.trt[i] == imp.data[,vtrt] & is.na(imp.data$IMP));
        lst.obs[[i]][[TXT.ENDP]] <- density(imp.data[inx, TXT.ENDP], adjust=adj, na.rm=TRUE);

        inx <- which(a.trt[i] == imp.data[,vtrt] &
                     0 == imp.data$DELTA);

        for (k in 1:length(voutcome)) {
            cur.y <- imp.data[inx, paste(ORG.PREFIX, voutcome[k], sep="")];
            cur.d <- density(cur.y, adjust=adj, na.rm=TRUE);
            lst.obs[[i]][[voutcome[k]]] <- cur.d;
        }

        ##imputed
        lst.den[[i]] <- rep(list(NULL), n.delta);
        for (j in 1:n.delta) {
            lst.den[[i]][[j]] <- rep(list(NULL), n.y);
            inx               <- which(a.trt[i]  == imp.data[,vtrt] &
                                       !is.na(imp.data$IMP) &
                                       deltas[j] == imp.data$DELTA);

            for (k in 1:n.y) {
                cur.y <- imp.data[inx, to.plot[k]];

                if (length(cur.y) == 0)
                    break ## AL

                cur.d <- density(cur.y, adjust = adj, na.rm = TRUE);
                maxy  <- max(maxy, cur.d$y)
                lst.den[[i]][[j]][[k]] <- cur.d
            }
        }
    }

    ##lims
    if (is.null(xlim)) {
        xlim <- range(imp.data[,to.plot], na.rm=TRUE);
    }

    if (is.null(ylim)) {
        ylim <- c(0, maxy*1.1);
    }

    if (is.null(trt.len)) {
        trt.len <- paste(toupper(lst.var$trt), "=", a.trt, sep="");
    }

    ##outputfile
    if (!is.null(fname)) {
        pdf(fname, ...);
    }

    if (is.null(mfrow)) {
        if (n.y > 1) {
            mfrow <- c(n.trt, n.y);
        } else {
            mfrow <- c(1, n.trt);
        }
    }

    par(mfrow=mfrow, las = 1);

    if (endp) {
        ## xlabs <- "Z (Imputed)";
        xlabs <- endfml; ## AL
    } else {
        xlabs <- to.plot;
    }

    for (i in 1:n.trt) {
        for (k in 1:n.y) {
            plot(NULL,
                 xlab = xlabs[k], ylab = "Density",
                 main = trt.len[i],
                 xlim=xlim, ylim=ylim);

            for (j in 1:n.delta) {
                lines(lst.den[[i]][[j]][[k]], col=cols[j], lty=ltys[j], lwd=2);
            }

            ##observed
            lines(lst.obs[[i]][[to.plot[k]]], col="gray", lty=2, lwd=2);

            ql <- as.expression(lapply(deltas,
                function(l) bquote(Delta==.(l))));

            legend("topleft",
                   c(ql, "Observed"),
                   lty=c(ltys[1:length(deltas)], 2),
                   col=c(cols[1:length(deltas)], "gray"),
                   bty="n");
        }
    }

    if (!is.null(fname))
        dev.off();

}

## Generate cumulative plot of the composite survival and functional outcome
plot.composite <- function(x,
                           delta=0,
                           fname=NULL,
                           buffer=0.05,
                           at.surv=NULL,
                           at.z=NULL,
                           p.death=NULL,
                           seg.lab=c("Survival", "Functional"),
                           cols=rep(c("cyan", "red"), 3),
                           ltys=rep(1, 6),
                           main="",
                           ...) {

    imp.rst <- x;

    if (is.null(imp.rst))
        return(NULL);

    stopifnot(any(class(imp.rst) == get.const("IMP.CLASS")));
    stopifnot(1 == length(delta));
    stopifnot(delta %in% imp.rst$deltas);

    lst.var    <- imp.rst$lst.var;
    imp.data   <- imp.rst$complete;

    f.y  <- function(x) {
        rst <- p.death+buffer + (1-p.death-buffer)*(x - range.y[1])/(range.y[2]-range.y[1]);
    }

    TXT.ENDP <- get.const("TXT.ENDP");
    vtrt     <- NULL;
    duration <- NULL;
    get.para(lst.var, environment());

    a.trt <- sort(unique(imp.data[,vtrt]));
    xlim  <- c(0,1);
    ylim  <- c(0,1);

    if (is.null(trt.len)) {
        trt.len <- paste("TRT=", a.trt, sep="");
    }

    ##convert xaxis
    delta.data <- subset(imp.data, imp.data$DELTA == delta | is.na(imp.data$DELTA));
    avg.data   <- sqldf(paste("select distinct ID, TRT, SURV, DELTA, avg(",
                              TXT.ENDP,
                              ")  as ENDP from 'delta.data' group by ID",
                              sep=""));

    inx.death  <- which(is.na(avg.data$ENDP));
    inx.alive  <- which(!is.na(avg.data$ENDP));
    if (is.null(p.death)) {
        p.death    <- length(inx.death)/nrow(avg.data);
    }
    ##add xaxis that combines surv and y
    avg.data$toplot <- NA;

    ##surv
    range.surv <- range(c(avg.data[inx.death,'SURV'], duration));
    f.surv <- function(x) {
        rst <- p.death * (x - range.surv[1])/(range.surv[2]-range.surv[1]);
    }
    avg.data$toplot[inx.death] <- f.surv(avg.data[inx.death,'SURV']);


    ##endp
    range.y <- range(avg.data[inx.alive, TXT.ENDP]);
    avg.data$toplot[inx.alive] <- f.y(avg.data[inx.alive,TXT.ENDP]);

    if (is.null(at.surv)) {
        at.surv <- range.surv;
    }

    at.z <- unique(c(at.z, round(range.y, 1)))

    ##outputfile
    if (!is.null(fname))
        pdf(fname, ...);

    par(mar=c(5.1,4.1,2.1,2.1),las=1)
    plot(NULL, xlab="", ylab="Percentile", xlim=xlim, ylim=ylim, main=main,
         axes=FALSE);

    box();
    ##axis(1, at=c(p.death/2, p.death+(1-p.death)/2), c("Survival","Functional"), tick=FALSE);
    axis(1, at = f.surv(at.surv), at.surv)
    axis(2, at = seq(0, 1, 0.25))
    axis(1, at = f.y(at.z), at.z)

    ## grid
    abline(v = c(f.surv(at.surv), f.y(at.z)),
           h = seq(0, 1, 0.25),
           col = "gray", lty = 3)

    text(c(p.death/2, p.death+(1-p.death)/2),
         c(0.5,0.5),
         seg.lab,
         col="gray",
         cex=1.2);

    lines(c(p.death,p.death), c(-1,2), lwd=2, lty=2, col="gray");

    for (i in 1:length(a.trt)) {
        cur.d  <- subset(avg.data, avg.data$TRT==a.trt[i]);
        toplot <- c(sort(cur.d$toplot), 1);
        y      <- c(0, 1:nrow(cur.d)/nrow(cur.d), 1);
        lines(stepfun(toplot, y), lwd=2, col=cols[i], lty=ltys[i], do.points=FALSE);
    }

    legend("topleft",
           trt.len,
           lty=ltys[1:length(a.trt)],
           col=cols[1:length(a.trt)],
           bty="n");

    if (!is.null(fname))
        dev.off();
}


##generate contour plot
plot.contour <- function(x, trt.len, col.var, con.v=0.05, nlevels=30, ...) {

    cur.data <- x;

    alphas   <- sort(unique(cur.data$Delta0));
    ql       <- as.expression(lapply(trt.len, function(l) bquote(.(l)~Delta)));
    nalpha   <- length(alphas);
    rst      <- matrix(NA, nalpha, nalpha);
    for (i in 1:nalpha) {
        for (j in 1:nalpha) {
            c.d      <- subset(cur.data, cur.data$Delta0 == alphas[i] & cur.data$Delta1 == alphas[j]);
            rst[i,j] <- c.d[1, col.var];
        }
    }

    par(oma=c(1,0,0,0));
    filled.contour(alphas,
                   alphas,
                   rst,
                   xlab = ql[1],
                   ylab = ql[2],
                   ...,
                   col=grey(seq(1,0,length=nlevels)),
                   plot.axes={axis(side=1, at=alphas);
                              axis(side=2, at=alphas);
                              grid(nx=length(alphas)-1, ny=length(alphas)-1, col="black");
                              contour(alphas, alphas, rst, levels=con.v,
                                      labcex=1.2, lwd=2, add=T, drawlabels=TRUE)
                                 });

}
