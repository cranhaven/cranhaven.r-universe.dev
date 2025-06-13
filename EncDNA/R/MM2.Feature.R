MM2.Feature <-
function(positive_class, test_seq){
if(class(positive_class)!="DNAStringSet"){stop("The positive_class must be of class DNAStringSet")}
if(length(unique(width(positive_class)))>1){stop("Each sequence of positive_class must of equal length")}

if(class(test_seq)!="DNAStringSet"){stop("The test_seq must be of class DNAStringSet")}
if(length(unique(width(test_seq)))>1){stop("Each sequence of test_seq must be of equal length")}
zz <- as.character(as.character(test_seq))

x <- as.matrix(positive_class)
x[x=="T" | x=="TRUE"]<-"X"

nl <- ncol(x)
nr <- nrow(x)
x1 <- x[,1:(nl-2)]
x2 <- x[,2:(nl-1)]
x3 <- x[,3:nl]
dp <- paste(x1, x2, sep="")
tp <- paste(x1, x2, x3, sep="")
mdp <- matrix(dp, nrow=nr, ncol=(nl-2))
mtp <- matrix(tp, nrow=nr, ncol=(nl-2))

countdi <- function(s, z) {
k <- sum((s==z)*1)
k
}

ind_di <- c("AA","AX","AG","AC", "XA","XX","XG","XC", "GA","GX","GG","GC", "CA","CX","CG","CC")
ind_tri <- c("AAA","AAX","AAG","AAC", "AXA","AXX","AXG","AXC", "AGA","AGX","AGG","AGC", "ACA","ACX","ACG","ACC"
         ,"XAA","XAX","XAG","XAC", "XXA","XXX","XXG","XXC", "XGA","XGX","XGG","XGC", "XCA","XCX","XCG","XCC"
		 ,"GAA","GAX","GAG","GAC", "GXA","GXX","GXG","GXC", "GGA","GGX","GGG","GGC", "GCA","GCX","GCG","GCC"
		 ,"CAA","CAX","CAG","CAC", "CXA","CXX","CXG","CXC", "CGA","CGX","CGG","CGC", "CCA","CCX","CCG","CCC")
n_tri <- t(sapply (ind_tri, function (z) apply(mtp, 2, function (s) countdi (s,z) )))
n_di <- t(sapply (ind_di, function (z) apply(mdp, 2, function (s) countdi (s,z) )))
dr <- apply(n_di, 2, function(r) rep(r,each=4))
mm1 <- round(n_tri/(dr+0.000003),3)

encode <- function(k){
s <- unlist(strsplit(k, split=""))
s[s=="T"|s=="TRUE"]<- "X"
les <- length(s)
z <- vector(mode="numeric", length=(les-2))

z1 <- s[1:(les-2)]
z2 <- s[2:(les-1)]
z3 <- s[3:les]
pz <- paste(z1, z2, z3, sep="")
#___________________________________________________A#
#############################
aaa <- which(pz=="AAA")
z[aaa] <- mm1[1,aaa]
aat <- which(pz=="AAX")
z[aat] <- mm1[2,aat]
aag <- which(pz=="AAG")
z[aag] <- mm1[3,aag]
aac <- which(pz=="AAC")
z[aac] <- mm1[4,aac]
########################
ata <- which(pz=="AXA")
z[ata] <- mm1[5,ata]
att <- which(pz=="AXX")
z[att] <- mm1[6,att]
atg <- which(pz=="AXG")
z[atg] <- mm1[7,atg]
atc <- which(pz=="AXC")
z[atc] <- mm1[8,atc]
###########################
aga <- which(pz=="AGA")
z[aga] <- mm1[9,aga]
agt <- which(pz=="AGX")
z[agt] <- mm1[10,agt]
agg <- which(pz=="AGG")
z[agg] <- mm1[11,agg]
agc <- which(pz=="AGC")
z[agc] <- mm1[12,agc]
###########################
aca <- which(pz=="ACA")
z[aca] <- mm1[13,aca]
act <- which(pz=="ACX")
z[act] <- mm1[14,act]
acg <- which(pz=="ACG")
z[acg] <- mm1[15,acg]
acc <- which(pz=="ACC")
z[acc] <- mm1[16,acc]
#############################
#______________________________________________________T#
#############################
taa <- which(pz=="XAA")
z[taa] <- mm1[17,taa]
tat <- which(pz=="XAX")
z[tat] <- mm1[18,tat]
tag <- which(pz=="XAG")
z[tag] <- mm1[19,tag]
tac <- which(pz=="XAC")
z[tac] <- mm1[20,tac]
########################
tta <- which(pz=="XXA")
z[tta] <- mm1[21,tta]
ttt <- which(pz=="XXX")
z[ttt] <- mm1[22,ttt]
ttg <- which(pz=="XXG")
z[ttg] <- mm1[23,ttg]
ttc <- which(pz=="XXC")
z[ttc] <- mm1[24,ttc]
###########################
tga <- which(pz=="XGA")
z[tga] <- mm1[25,tga]
tgt <- which(pz=="XGX")
z[tgt] <- mm1[26,tgt]
tgg <- which(pz=="XGG")
z[tgg] <- mm1[27,tgg]
tgc <- which(pz=="XGC")
z[tgc] <- mm1[28,tgc]
###########################
tca <- which(pz=="XCA")
z[tca] <- mm1[29,tca]
tct <- which(pz=="XCX")
z[tct] <- mm1[30,tct]
tcg <- which(pz=="XCG")
z[tcg] <- mm1[31,tcg]
tcc <- which(pz=="XCC")
z[tcc] <- mm1[32,tcc]
############################
#_______________________________________________G#
#############################
gaa <- which(pz=="GAA")
z[gaa] <- mm1[33,gaa]
gat <- which(pz=="GAX")
z[gat] <- mm1[34,gat]
gag <- which(pz=="GAG")
z[gag] <- mm1[35,gag]
gac <- which(pz=="GAC")
z[gac] <- mm1[36,gac]
########################
gta <- which(pz=="GXA")
z[gta] <- mm1[37,gta]
gtt <- which(pz=="GXX")
z[gtt] <- mm1[38,gtt]
gtg <- which(pz=="GXG")
z[gtg] <- mm1[39,gtg]
gtc <- which(pz=="GXC")
z[gtc] <- mm1[40,gtc]
###########################
gga <- which(pz=="GGA")
z[gga] <- mm1[41,gga]
ggt <- which(pz=="GGX")
z[ggt] <- mm1[42,ggt]
ggg <- which(pz=="GGG")
z[ggg] <- mm1[43,ggg]
ggc <- which(pz=="GGC")
z[ggc] <- mm1[44,ggc]
###########################
gca <- which(pz=="GCA")
z[gca] <- mm1[45,gca]
gct <- which(pz=="GCX")
z[gct] <- mm1[46,gct]
gcg <- which(pz=="GCG")
z[gcg] <- mm1[47,gcg]
gcc <- which(pz=="GCC")
z[gcc] <- mm1[48,gcc]
############################
#______________________________________________C#
############################
caa <- which(pz=="CAA")
z[caa] <- mm1[49,caa]
ccat <- which(pz=="CAX")
z[ccat] <- mm1[50,ccat]
cag <- which(pz=="CAG")
z[cag] <- mm1[51,cag]
cac <- which(pz=="CAC")
z[cac] <- mm1[52,cac]
########################
cta <- which(pz=="CXA")
z[cta] <- mm1[53,cta]
ctt <- which(pz=="CXX")
z[ctt] <- mm1[54,ctt]
ctg <- which(pz=="CXG")
z[ctg] <- mm1[55,ctg]
ctc <- which(pz=="CXC")
z[ctc] <- mm1[56,ctc]
###########################
cga <- which(pz=="CGA")
z[cga] <- mm1[57,cga]
cgt <- which(pz=="CGX")
z[cgt] <- mm1[58,cgt]
cgg <- which(pz=="CGG")
z[cgg] <- mm1[59,cgg]
cgc <- which(pz=="CGC")
z[cgc] <- mm1[60,cgc]
###########################
cca <- which(pz=="CCA")
z[cca] <- mm1[61,cca]
cct <- which(pz=="CCX")
z[cct] <- mm1[62,cct]
ccg <- which(pz=="CCG")
z[ccg] <- mm1[63,ccg]
ccc <- which(pz=="CCC")
z[ccc] <- mm1[64,ccc]
##########################
z
}

enc_mm2 <- t(sapply(zz, encode))
enc_mm2
}
