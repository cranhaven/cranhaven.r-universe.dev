utils::globalVariables(c('Lew', 'Lns', 'H'))

fSombra6<-function(angGen, distances, struct, prom=TRUE)
{
    stopifnot(is.list(struct),
              is.data.frame(distances))
    ##distances only has three distances, so I generate a grid
    if (dim(distances)[1] == 1){ 
        Red <- distances[, .(Lew = c(-Lew, 0, Lew, -Lew, Lew),
                             Lns = c(Lns, Lns, Lns, 0, 0),
                             H=H)]
    } else { #distances is an array, so there is no need to generate the grid
        Red<-distances[1:5,]} #I only need the first 5 rows...necessary in case a wrong data.frame is delivered

    ## I calculate the shadow due to each of the 5 followers
    SombraGrupo <- matrix(ncol=5,nrow=dim(angGen)[1]) ###VECTORIZE
    for (i in 1:5) {SombraGrupo[,i]<-fSombra2X(angGen,Red[i,],struct)}
    ##To calculate the Average Shadow, I need the number of followers in each position (distrib)
    distrib <- with(struct,c(1,Ncol-2,1,Nrow-1,(Ncol-2)*(Nrow-1),Nrow-1)) 
    vProm <- c(sum(distrib[c(5,6)]),
               sum(distrib[c(4,5,6)]),
               sum(distrib[c(4,5)]),
               sum(distrib[c(2,3,5,6)]),
               sum(distrib[c(1,2,4,5)]))
    Nseg <- sum(distrib) ##Total number of followers
    ##With the SWEEP function I multiply the Shadow Factor of each type (ShadowGroup columns) by the vProm result
    
    if (prom == TRUE){
        ## Average Shadow Factor in the group of SIX followers taking into account distribution
        FS <- rowSums(sweep(SombraGrupo,2,vProm,'*'))/Nseg
        FS[FS>1] <- 1
    } else {		
        ## Shadow factor on follower #5 due to the other 5 followers
        FS <- rowSums(SombraGrupo)
        FS[FS>1] <-1
    }
    return(FS)
}
