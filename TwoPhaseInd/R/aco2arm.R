aco2arm <- function (data, svtime, event, treatment, BaselineMarker, subcohort=NULL, esttype = NULL, weight=NULL, extra=NULL) {
  if (!is.data.frame(data)) {
    stop("Argument data must be a data.frame object.")
  } else {
    colNames <- colnames(data)
    if (!(svtime %in% colNames)) {
      stop("Survival time variable was not found in the data.")
    }
    if (!(treatment %in% colNames)) {
      stop("Treatment variable was not found in the data.")
    } else {
      if (any(levels(factor(data[, treatment])) != c("0","1"))) {
        warning("Treatment variable must be either 0 or 1 only.")
      }
    }
    if (!(BaselineMarker %in% colNames)) {
      stop("BaselineMarker variable was not found in the data.")
    }
    if (!is.null(extra)) {
      if (!any(extra %in% colNames)) {
        extraNotFound <- paste(extra[!(extra %in% colNames)], sep = "", collapse = ", ")
        stop(paste("Extra variable(s) was not found in the data:", extraNotFound))
      }
      tmp <- remove_rarevariants(data[,extra])
      if (any(tmp))
      {
        idx <- tmp==TRUE
        toremove=NULL
        for (i in 1:length(idx))
        {
          if (idx[i]) toremove <- c(toremove,extra[idx[i]])
        }
        warnings(paste0(paste(toremove,sep=", "), " were removed due to rare vairant"))
        extra <- extra[!idx]
        if (length(extra)==0) extra <- NULL
      }
    }
  }
  
  #check esttype
  if (is.null(esttype) && is.null(weight)) stop("esttype can't be null when the weight is not provided")
  
  numvacc <- nrow(data)
  
  #limit the data to the variables to be used
  data <- data[,c(svtime,event,treatment,BaselineMarker,subcohort,extra)]
  
  #Remove missing data
  dat1 <- remove_missingdata(data)$data
  n1 <- nrow(dat1)
  nx <- 1 + length(extra)
  if (is.null(weight)) ww <- rep(1, n1) else {
      ww <- dat1[,weight] 
      esttype <- 0
  }    
      
  #Biomarker variable should be transformed to numeric
  if (! is.numeric(dat1[, BaselineMarker]))
    dat1[, BaselineMarker] = char2num(dat1[, BaselineMarker])
  if (remove_rarevariants(dat1[, BaselineMarker])) {
    warnings("BaselineMarker variable is rare variant")
    tmpResult <- data.frame(beta=rep(NA,length(extra)+3), stder=rep(NA,length(extra)+3), pVal=rep(NA,length(extra)+3))
    rownames(tmpResult)[4:nrow(tmpResult)]=extra
  } else {
    cases <- dat1[dat1[, event] == 1, ]
    fit4 <- glm(cases[, treatment] ~ cases[, BaselineMarker],family = binomial, x = TRUE, y = TRUE)
    bread1 <- t(fit4$x) %*% (fit4$x * fit4$fitted * (1 - fit4$fitted))
    b23 <- fit4$coef
    var23 <- (summary(fit4)$coef[, 2])^2
    ofs <- cbind(dat1[, treatment], dat1[, treatment] * dat1[,BaselineMarker]) %*% fit4$coef
    if (!is.null(esttype)) {
      if(esttype == 1) {
      dat1$id <- 1:n1
      temp1 <- data.frame(dat1[dat1[, event] == 1, ], dummy = -100,group = 1, ofs = ofs[dat1[, event] == 1, ])
      temp2 <- data.frame(dat1[dat1[, subcohort] == 1, ], dummy = 0, group = 0, ofs = ofs[dat1[, subcohort] == 1, ])
      newdata <- rbind(temp1, temp2)
      if (!is.null(extra))
      {
        fmla <- as.formula(paste0("Surv(", svtime, ",group) ~ ",paste(paste(c(BaselineMarker, extra), collapse = "+"),"offset(dummy+ofs)+cluster(id)", sep = "+")))
      }else
      {
        fmla <- as.formula(paste0("Surv(", svtime, ",group) ~ ",BaselineMarker,"+offset(dummy+ofs)+cluster(id)"))
      }
      fit5 <- coxph(fmla, data = newdata)
      id.ss2 <- which(dat1[, subcohort] == 1)
      }else 
      {
        if (is.null(weight) & !is.null(subcohort)) ww[dat1[, event] == 0] <- (numvacc - sum(dat1[,event] ==  1))/(sum(dat1[,subcohort] == 1) - sum(dat1[,subcohort] == 1 & dat1[,event] == 1))
        if (!is.null(extra))
       {
         fmla <- as.formula(paste0("Surv(", svtime, ",", event,") ~ ", paste(paste(c(BaselineMarker, extra), collapse = "+"), "offset(ofs)", sep = "+")))
       }else 
       {
        fmla <- as.formula(paste0("Surv(", svtime, ",", event,") ~ ", BaselineMarker, "+offset(ofs)"))
       }
       fit5 <- coxph(fmla, weights = ww, data = dat1, robust = TRUE)
       id.ss2 <- 1:n1
     }
    }

    if (!is.null(extra))
    {
      xx0 <- cbind(dat1[, treatment], dat1[, treatment] * dat1[,BaselineMarker], dat1[, BaselineMarker], dat1[, extra])
    }else {
      xx0 <- cbind(dat1[, treatment], dat1[, treatment] * dat1[,BaselineMarker], dat1[, BaselineMarker])
    }
    
    xx0 <- as.matrix(xx0)
    nx <- dim(xx0)[2]
    xx <- xx0[id.ss2, ]
    yy <- dat1[id.ss2, svtime]
    n2 <- length(id.ss2)
    beta <- c(fit4$coef, fit5$coef)
    a <- exp(xx %*% beta) * ww[id.ss2]
    s0 <- rep(0, n1)
    s1 <- matrix(0, n1, nx)
    dd1 <- matrix(0, n1, nx)
    for (i in which(dat1[, event] == 1)) {
      b <- 1 * (yy >= dat1[i, svtime])
      if (sum(b) > 0) {
        s0[i] <- sum(b * a)
        s1[i, ] <- apply(matrix(b * a, nrow = n2,ncol = nx,byrow = FALSE) * xx, 2, sum)
        dd1[i, ] <- xx0[i, ] - s1[i, ]/s0[i]
      }
      else {
        dd1[i, ] <- xx0[i, ]
      }
    }
    dd2 <- matrix(0, n1, nx)
    for (i in id.ss2) {
      tmp <- matrix(0, n1, nx)
      for (j in which(dat1[, event] == 1)) {
        tmp[j, ] <- ww[i] * (dat1[i, svtime] >= dat1[j, svtime]) * as.numeric(exp(xx0[i, ] %*% beta)) * (xx0[i, ] - s1[j, ]/s0[j])/s0[j]
      }
      dd2[i, ] <- apply(tmp, 2, sum)
    }
    ss2 <- dd1 - dd2
    infmat <- matrix(0, nx, nx)
    for (i in which(dat1[, event] == 1)) {
      b <- yy >= dat1[i, svtime]
      s2 <- matrix(0, nx, nx)
      for (j in which(b)) {
        s2 <- s2 + as.matrix(a[j] * xx[j, ]) %*% t(as.matrix(xx[j,]))
      }
      temp <- s2/s0[i] - as.matrix(s1[i, ]) %*% t(as.matrix(s1[i,]))/(s0[i]^2)
      infmat <- infmat + temp
    }
    V2s <- drop((solve(infmat[-(1:2), -(1:2)]) %*% t(ss2[, -(1:2)]) %*% ss2[, -(1:2)] %*% solve(infmat[-(1:2), -(1:2)])))
    ss1 <- (fit4$y - fit4$fitted) * fit4$x
    V1 <- solve(t(ss1) %*% ss1)
    V21 <- solve(infmat[-(1:2), -(1:2)])
    C1 <- infmat[-(1:2), 1:2, drop=FALSE]
    R <- t(ss2[dat1[, event] == 1, -(1:2)]) %*% ss1
    var1 <- diag(V2s + V21 %*% (C1 %*% V1 %*% t(C1) - R %*% V1 %*% t(C1) - C1 %*% V1 %*% t(R)) %*% V21)
    
    vmat <- matrix(0,nx,nx)
    vmat[3:nx,3:nx] <- (V2s + V21 %*% (C1 %*% V1 %*% t(C1) - R %*% V1 %*% t(C1) - C1 %*% V1 %*% t(R)) %*% V21)
    vmat[1:2,1:2] <- solve(bread1) %*% t(fit4$x * (fit4$y - fit4$fitted)) %*% (fit4$x * (fit4$y - fit4$fitted)) %*% solve(bread1)
    
    cov1 <- solve(bread1) %*% t(fit4$x * (fit4$y - fit4$fitted)) %*% (ss2[dat1[,event]==1, -(1:2)]- ss1 %*% V1 %*% as.matrix(infmat[1:2,-(1:2)])) %*% solve(infmat[-(1:2),-(1:2)])
    
    vmat[1:2,-(1:2)] <- cov1
    vmat[-(1:2),(1:2)] <- t(cov1)
    
    outmat <- matrix(0,3,3)
    outmat[2:3,2:3] <- vmat[1:2,1:2]
    outmat[1:3,1] <- vmat[c(3,1,2),3]
    outmat[1,1:3] <- vmat[c(3,1,2),3]
    
    est.coef <- c(fit5$coef[1], b23, fit5$coef[-1])
    est.var <- diag(vmat)
    est.var <- c(est.var[3],est.var[-3])
    
    
    pVal <- 2*(1-pnorm(abs(est.coef/sqrt(est.var))))
    tmpResult <- data.frame(beta=round(est.coef, 4), stder=round(sqrt(est.var), 4), pVal=round(pVal,4))
    
  }
  
  rownames(tmpResult)[1] <- paste(BaselineMarker, "(Baseline Marker)")
  rownames(tmpResult)[2] <- paste(treatment, "(Treatment)")
  rownames(tmpResult)[3] <- "Marker-treatment interatcion"
  
  outmat <- data.frame(outmat)
  
  rownames(outmat)[1] <- BaselineMarker
  rownames(outmat)[2] <- treatment
  rownames(outmat)[3] <- "Interaction"
  
  colnames(outmat)[1] <- BaselineMarker
  colnames(outmat)[2] <- treatment
  colnames(outmat)[3] <- "Interaction"
  
  
  return(list(Estimate=tmpResult,Covariance=outmat))
  
}
