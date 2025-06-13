Trint.Dist.Feature <-
function(test_seq){

if(class(test_seq)!="DNAStringSet"){stop("The dataset must be of class DNAStringSet")}
if(length(test_seq)<2){stop("User must supply at leat two sequences")}
if(length(unique(width(test_seq)))>1){stop("Each sequence must be of equal length")}
zz <- as.character(as.character(test_seq))


ind <- c("AAA","AAX","AAG","AAC", "AXA","AXX","AXG","AXC", "AGA","AGX","AGG","AGC", "ACA","ACX","ACG","ACC"
         ,"XAA","XAX","XAG","XAC", "XXA","XXX","XXG","XXC", "XGA","XGX","XGG","XGC", "XCA","XCX","XCG","XCC"
		 ,"GAA","GAX","GAG","GAC", "GXA","GXX","GXG","GXC", "GGA","GGX","GGG","GGC", "GCA","GCX","GCG","GCC"
		 ,"CAA","CAX","CAG","CAC", "CXA","CXX","CXG","CXC", "CGA","CGX","CGG","CGC", "CCA","CCX","CCG","CCC")

expr <- function(k){
s <- unlist(strsplit(k, split=""))
s[s=="T"|s=="TRUE"]<- "X"
paste(s, collapse="")
}

bef <- as.character(sapply(zz, expr))
mm_bfor <- sapply(ind, function(z) sapply(bef,function(s)gregexpr(z,s)))


alph <- function(m)
 {
v <- unlist(m)
lv <- length(v)
if(v[1] < 0) {alpha_r <- 0} 
             else{
			 if(lv==1) {alpha_r <- 1}
			            else{
						     u1 <- v[1:(lv-1)]
			                 u2 <- v[2:lv]
						     du <- u2-u1
							 idu <- 1/du
							 ldu <- length(du)
							 ss <- rep(1:ldu, times=ldu:1)
							 duss <- idu[ss]
							 mdu <- matrix(0, nrow=ldu, ncol=ldu)
							 mdu[lower.tri(mdu, diag=T)] <- duss
							 amdu <- apply(mdu, 1, sum)
							 qi <- amdu/sum(amdu)
							 peq <- sum(qi*exp(1-qi))				 
						     alpha_r <- peq
						   }
			 
			     }
alpha_r			 
 }
 
PE_bfor <- apply(mm_bfor, c(1,2), alph) 
PE_bfor
}
