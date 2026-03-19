# This file is part of econetwork

# econetwork is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# econetwork is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with econetwork.  If not, see <http://www.gnu.org/licenses/>

cpness <- function(web, type=c("automatic","binary","integer","float"), plot=TRUE, fastplot=FALSE){
    spid1 <- rownames(web)
    spid2 <- colnames(web)
    if(missing(type)){
       type="automatic"
    }
    if(type=="automatic"){
        if(length(table(web))==2){
            type="binary"
        } else{
            if(length(table(as.integer(web))) == length(table(web))){                
                type="integer"
            } else{
                type="float"
            }
        }
    }
    if(type=="binary"){
        web[web>0] <- 1L
        my_model <- BM_bernoulli("LBM", web, explore_max=4, plotting="", verbosity=0, ncores=1)
    }
    if(type=="integer"){
        my_model <- BM_poisson("LBM", web, explore_max=4, plotting="", verbosity=0, ncores=1)
    }
    if(type=="float"){
        my_model <- BM_gaussian("LBM", web, explore_max=4, plotting="", verbosity=0)
    }
    capture.output(my_model$estimate())
    selected <- which.max(my_model$ICL)    
    rowmb <- apply(my_model$memberships[selected][[1]]$Z1,1,which.max)
    colmb <- apply(my_model$memberships[selected][[1]]$Z2,1,which.max)
    if(length(table(rowmb))==2){
        conn <- sapply(split(rowMeans(web),rowmb), mean)
        if(conn[1]<conn[2]){
            rowmb[which(rowmb==1)] <- -2
            rowmb[which(rowmb==2)] <- -1
            rowmb <- -rowmb
        }
    }
    if(length(table(colmb))==2){
        conn <- sapply(split(colMeans(web),colmb), mean)
        if(conn[1]<conn[2]){
            colmb[which(colmb==1)] <- -2
            colmb[which(colmb==2)] <- -1
            colmb <- -colmb
        }
    }
    if(plot & !fastplot){
        visweb((log(1+web))[order(rowmb, decreasing=T),order(colmb)], type="none")
        if(length(table(rowmb))==2){
            lines(c(0,length(colmb)), c(table(rowmb)[1],table(rowmb)[1]), col=4, lwd=3)
        }
        if(length(table(colmb))==2){
            lines(c(table(colmb)[1],table(colmb)[1]), c(0,length(rowmb)), col=4, lwd=3)
        }
    }
    if(fastplot){          
        image(1:length(colmb), 1:length(rowmb), t(log(1+web))[order(colmb),order(rowmb)],
              xaxt="n", yaxt="n", main="" , xlab=NA, ylab=NA, col=c(rev(gray.colors(12)),"black"))
        if(length(table(rowmb))==2){
            abline(h=table(rowmb)[1]+0.5, col=4, lwd=3)
        }
        if(length(table(colmb))==2){
            abline(v=table(colmb)[1]+0.5, col=4, lwd=3)
        }
    }
    if((length(table(rowmb))==2)&(length(table(colmb))==2)){        
        E11 <- sum(web[which(rowmb==1), which(colmb==1)])
        E12 <- sum(web[which(rowmb==1), which(colmb==2)])
        E21 <- sum(web[which(rowmb==2), which(colmb==1)])
        E22 <- sum(web[which(rowmb==2), which(colmb==2)])
        cpness <- (E11+E12+E21)/sum(web)
    } else{
        cpness <- NA        
    }
    names(rowmb) <- spid1
    names(colmb) <- spid2
    return(list(cpness=cpness, rowmembership=rowmb, colmembership=colmb))
}
