plot_results <-
function(results="",by="",plot_all=TRUE, var = "", save_plot=TRUE, plot_name="", bf= FALSE , save_qq = TRUE) 
{
    print("Arguments set:")
    print(paste("Data:",substitute(results),sep=" "))
    print(paste("Plot results by:",substitute(by),sep=" "))
    if(plot_name!=""){
    print(paste("Save plot as:",substitute(plot_name)),sep=" ")
}
if(plot_name==""){
print(paste("Save plot as (default):Results_genomicper_",substitute(by),sep=""))
}
    traits <- as.character(unique(results[,2]))
paths <- as.character(unique(results[,1]))
ntraits <- length(traits)
npaths <- length(paths)
onefile=TRUE
if(by != "trait"){
if(by != "set"){
stop("Argument \"by\" must be set to \"trait\" or \"set\"")
}
}
    if(by == "trait"){
if(npaths < 5){
stop("Number of sets for qq plot must be more than 5")
}
    }
    if(by == "set"){
if(ntraits < 5){
stop("Number of traits for qq plot must be more than 5")
}
    }
    if(plot_all == TRUE){
if(save_plot == FALSE){
stop("If plot_all is set to TRUE, the plots must be saved in working directory (\"save_plot=TRUE\")") 
}
}
    if(plot_all == FALSE){
    if(var==""){
    stop("Argument \"var\" missing. Select a name of a set or trait to plot")
    }
    if(var %in% traits == FALSE){
if(var %in% paths == FALSE){
stop("Variable \"var\" not found in results")
}
    }
}
    if(save_plot == TRUE){
if(missing(plot_name) == TRUE) {
plot_name <- "Results_genomicper"
}
    }
rp <- which(results[,5]== 0)
if(length(rp)!= 0){
results[rp,5] <- 0.000001
}
results[,1]<-as.character(results[,1])
results[,2]<-as.character(results[,2])
results[,5]<-as.numeric(as.character(results[,5]))
qq_data <- ""
if(by == "trait"){
if(plot_all == TRUE){
bf1 <- (0.05/npaths)
mxx <- -log10(.5/npaths)
mxy <- -log10(results[which.max(-log10(results[,5])),5])
fname <- paste(plot_name,"_trait.pdf",sep="")
pdf(file = ifelse(onefile, fname),bg="transparent",width=15,height=15)
par(las=1,cex=1,mfrow=c(4,4),lty=1,ps=15)
for(i in 1:ntraits){
x <- which(results[,2]==traits[i])
temp <- results[x,c(1,2,5)]
rnk <- c(1:npaths)
x <- order(temp[,3])
temp <- temp[x,]
empL <- -log10(temp[,3])
expL <- -log10((rnk -.5)/npaths)
temp <- cbind(temp,empL,expL)
colnames(temp)[4] <- "Log10_Observed"
colnames(temp)[5] <- "Log10_Expected"
if(save_qq==TRUE){
qq_data <-rbind(qq_data,temp) 
}
plot(expL,empL,xlim=c(0,mxx),ylim=c(0,mxy),main=traits[i],xlab="Expected(-log10)",ylab="Observed(-log10)",col="red")
lines(expL,expL,type="l")
if(bf==TRUE){
abline(h=-log10(bf1),col="blue",cex=1.2)
}
}
dev.off()
}
}
if(by == "trait"){
if(plot_all == FALSE){
bf1 <- (0.05/npaths)
mxx <- -log10(.5/npaths)
if(save_plot==TRUE){
fname <- paste(plot_name,"_trait.pdf",sep="")
pdf(file = ifelse(onefile, fname),bg="transparent",width=15,height=15)
}
x <- which(results[,2]== var)
mxy <- -log10(results[x[which.max(-log10(results[x,5]))],5])
if(mxy < mxx){
mxy <- mxx
}
temp <- results[x,c(1,2,5)]
rnk <- c(1:npaths)
x <- order(temp[,3])
temp <- temp[x,]
empL <- -log10(temp[,3])
expL <- -log10((rnk -.5)/npaths)
if(save_qq==TRUE){
qq_data <- cbind(temp,empL,expL)
colnames(qq_data)[4] <- "Log10_Observed"
colnames(qq_data)[5] <- "Log10_Expected"
}
par(las=1,cex=1,lty=1,ps=15)
plot(expL,empL,xlim=c(0,mxx),ylim=c(0,mxy),main=var,xlab="Expected(-log10)",ylab="Observed(-log10)",col="red")
lines(expL,expL,type="l")
if(bf==TRUE){
abline(h=-log10(bf1),col="blue",cex=1.2)
}
identify(expL,empL,labels=temp[,1])
if(save_plot==TRUE){
dev.off()
}
}
}
if(by == "set"){
if(plot_all == TRUE){
bf1 <- (0.05/ntraits)
mxx <- -log10(.5/ntraits)
mxy <- -log10(results[which.max(-log10(results[,5])),5])
fname <- paste(plot_name,"_set.pdf",sep="")
pdf(file = ifelse(onefile, fname),bg="transparent",width=15,height=15)
par(las=1,cex=1,mfrow=c(4,4),lty=1,ps=15)
for(i in 1:npaths){
x <- which(results[,1]==paths[i])
temp <- results[x,c(1,2,5)]
rnk <- c(1:ntraits)
x <- order(temp[,3])
temp <- temp[x,]
empL <- -log10(temp[,3])
expL <- -log10((rnk -.5)/ntraits)
temp <- cbind(temp,empL,expL)
colnames(temp)[4] <- "Log10_Observed"
colnames(temp)[5] <- "Log10_Expected"
if(save_qq==TRUE){
qq_data <- cbind(temp,empL,expL)
}
plot(expL,empL,xlim=c(0,mxx),ylim=c(0,mxy),main=paths[i],xlab="Expected(-log10)",ylab="Observed(-log10)",col="red")
lines(expL,expL,type="l")
if(bf==TRUE){
abline(h=-log10(bf1),col="blue",cex=1.2)
}
}
dev.off()
}
}
if(by == "set"){
if(plot_all == FALSE){
bf1 <- (0.05/ntraits)
mxx <- -log10(.5/ntraits)
if(save_plot==TRUE){
fname <- paste(plot_name,"_set.pdf",sep="")
pdf(file = ifelse(onefile, fname),bg="transparent",width=15,height=15)
}

x <- which(results[,1]== var)
mxy <- -log10(results[x[which.max(-log10(results[x,5]))],5])
if(mxy < mxx){
mxy <- mxx
}
temp <- results[x,c(1,2,5)]
rnk <- c(1:ntraits)
x <- order(temp[,3])
temp <- temp[x,]
empL <- -log10(temp[,3])
expL <- -log10((rnk -.5)/ntraits)
if(save_qq==TRUE){
qq_data <- cbind(temp,empL,expL)
colnames(qq_data)[4] <- "Log10_Observed"
colnames(qq_data)[5] <- "Log10_Expected"
}
par(las=1,cex=1,lty=1,ps=15)
plot(expL,empL,xlim=c(0,mxx),ylim=c(0,mxy),main=var,xlab="Expected(-log10)",ylab="Observed(-log10)",col="red")
lines(expL,expL,type="l")
if(bf==TRUE){
abline(h=-log10(bf1),col="blue",cex=1.2)
}
identify(expL,empL,labels=temp[,2])
if(save_plot==TRUE){
dev.off()
}
}
}
if(save_qq==TRUE){
return(qq_data)
}
}
