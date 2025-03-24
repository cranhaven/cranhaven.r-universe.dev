alleleSummary = function(x, ids, ibd.status=FALSE) {
    if(missing(ids)) ids = 1:length(x[[1]])
    allele.colnames = paste0(rep(ids, each=2), c("p","m"))
    
    each.chrom = lapply(x, function(y) {
        haplos = unlist(y[ids], recursive=FALSE)
        breaks = unlist(lapply(haplos, function(m) m[-1,1]))
        breaks = c(0, .sortDouble(breaks[!duplicated(breaks)]))
        alleles = vapply(haplos, pos2allele, posvec=breaks, FUN.VALUE=breaks)
        if(length(breaks)==1)
            # Ad hoc fix, since vapply simplifies if FUN.VALUE has length 1
            dim(alleles) = c(1, length(allele.colnames))
        colnames(alleles) = allele.colnames
        
        stops = c(breaks[-1], attr(y, 'length_Mb'))
        chrom = rep.int(attr(y, 'chromosome'), length(breaks))
        cbind(chrom=chrom, start=breaks, end=stops, length=stops-breaks, alleles) 
    })
    res = do.call(rbind, each.chrom)
    if(ibd.status) {
        stopifnot(length(ids)==2)
        ibd_pp = res[, 5] == res[, 7]
        ibd_pm = res[, 5] == res[, 8]
        ibd_mp = res[, 6] == res[, 7]
        ibd_mm = res[, 6] == res[, 8]
        ibd = (ibd_pp | ibd_pm) + (ibd_mp | ibd_mm) 
        
        res = cbind(res, ibd=ibd, ibd_pp=ibd_pp, ibd_pm=ibd_pm, ibd_mp=ibd_mp, ibd_mm=ibd_mm)
    }
    res
}
