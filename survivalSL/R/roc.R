
roc <- function(times, failures, variable, confounders, data, pro.time, precision=seq(.01, .99, by=.01))
{

cut.off <- quantile(data[,variable], probs=precision, na.rm=TRUE)

if((max(precision)==1) | (min(precision)==0)){ stop("The cut-off values have to be different from the minimum or the maximum of the variable") }

data$temp <- data[,failures] + data[,variable] + data[,times]

form0 <- update.formula(confounders, temp ~ .)

if((length(data[,failures]) - summary(glm(form0, data=data))$df.null - 1) > 0) {stop("Error: missing values are not allowed")}

km.c <- summary(survfit(Surv(data[,times], data[,failures]==0)~1))
km.c <- data.frame(time=c(0, km.c$time), surv=c(1, km.c$surv))
km.c$surv[km.c$surv==0] <- 0.0001
s.c <- 1 / (sapply(data[,times], FUN=function(x) {km.c$surv[km.c$time<=x][sum(km.c$time<=x)]}))

.cox <- eval(parse(text=paste("coxph(Surv(",times ,", ",failures,")",paste(confounders[1],confounders[2]),", data=data)",sep="")))

if(confounders=="~1"){
	survfit.object <- survival::survfit(.cox,se.fit=FALSE,conf.int=FALSE)
	W <- summary(survfit.object,times=pro.time)$surv
}else{
	survfit.object <- survival::survfit(.cox, newdata=data,se.fit=FALSE,conf.int=FALSE)
	W <- summary(survfit.object, times=pro.time)$surv
}

se <- function(x) {
	sum(1 * ((data[, variable] > cut.off[x]) & (data[,
	times] <= pro.time)) * data[, failures] * s.c *
	pmin(1/(1 - W),1/(mean(1-W)^2*0.1)))/sum(1 * (data[, times] <= pro.time) *
	data[, failures] * s.c * pmin(1/(1 - W),1/(mean(1-W)^2*0.1)))
}

sp <- function(x) {
	sum(1 * ((data[, variable] <= cut.off[x]) & (data[,times] > pro.time)) * pmin(1/W,1/(mean(W)^2*0.1)))/sum(1 * (data[, times] > pro.time) * pmin(1/W,1/(mean(W)^2*0.1)))
}

temp.se <- sapply(1:length(cut.off), FUN = "se")
temp.sp <- sapply(1:length(cut.off), FUN = "sp")

.tab <-data.frame(cut.off = cut.off, se = temp.se, sp1 = 1-temp.sp)

.tab$se[.tab$se > 1] <- NA
.tab$sp1[.tab$sp1 > 1] <- NA
.tab.res.temp <- .tab[!is.na(.tab$sp1 + .tab$se), ]
.tab.res.temp <- .tab.res.temp[order(.tab.res.temp$sp1, .tab.res.temp$se), ]
.tab.res.temp <- rbind(c(NA, 0, 0), .tab.res.temp, c(NA, 1, 1))
colnames(.tab.res.temp) <- colnames(.tab)

.tab$sp <- 1 - .tab$sp1
.tab <- rbind(c(min(data[,variable]),1,1,0) ,.tab, c(max(data[,variable]),0,0,1))



auc.fun <- function(sens, spec)
{
  .tab.res <- data.frame(se=sens, sp=spec)
  .tab.res <- .tab.res[!is.na(.tab.res$sp + .tab.res$se),]
  .tab.res$sp1 <- 1-.tab.res$sp
  .tab.res <- .tab.res[order(.tab.res$sp1, .tab.res$se),]
  .tab.res <- rbind(c(0,1,0), .tab.res, c(1,0,1))

  return( sum((.tab.res$sp1[2:length(.tab.res$sp1)] -
                 .tab.res$sp1[1:(length(.tab.res$sp1)-1)]) * 0.5 *
                (.tab.res$se[2:length(.tab.res$se)]+.tab.res$se[1:length(.tab.res$se)-1])) )
}


if(dim(.tab.res.temp)[1]>2){
.auc <- auc.fun(.tab.res.temp$se, 1-.tab.res.temp$sp1)
}else{.auc<-NA}

.tab$J <- .tab$sp + .tab$se - 1

.obj <- list(table=.tab[,c("cut.off", "se", "sp", "J")], auc = .auc)

class(.obj) <- "rocrisca"

return(.obj)
}


