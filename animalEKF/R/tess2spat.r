tess2spat <- function(obj, idvec=NULL) {
	K <- nrow(obj$summary)
	if (is.null(idvec)) { idvec <- 1:K }
	partition <- vector(mode="list", length=K)
	xy <- lapply(tile.list(obj), "[", i=3:4)
	#form Polygons
	for (i in 1:length(xy)) {
		pcrds <- unique(cbind(xy[[i]]$x, xy[[i]]$y))
		pcrds <- rbind(pcrds, pcrds[ 1,])
		colnames(pcrds) <- c("X","Y")
   
		partition[[i]] <- sp::Polygons(list(sp::Polygon(pcrds)), ID=as.character(idvec[i]))
	}
	partition <- sp::SpatialPolygons(partition)
}