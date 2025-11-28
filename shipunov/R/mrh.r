MRH <- function(hcl, dim=NULL, method="groups") {
method <- match.arg(method, choices=c("groups", "height", "branches", "cophenetic"))
h <- hcl$height
num <- length(h) + 1 # number of objects
if(method=="groups"){
if(is.null(dim)) dim <- (num-1):2 # margin k's give no information
res <- cutree(hcl, k=dim)
}
if(method=="height"){
if(is.null(dim)) dim <- 2*(num+1) # twice as number of objects plus 2 (to take out)
heights <- seq(max(h), min(h), length.out=(dim+2))[2:(dim+1)] # take out two marginal heights
res <- cutree(hcl, h=heights)
}
if(method=="branches"){
labs <- hcl$labels
res <- t(Hcl2mat(hcl)) # Hcl2mat() returns objects as columns
row.names(res) <- labs # and loses label names
}
if(method=="cophenetic"){
if(is.null(dim)) dim <- num-1 # 'dim' must be '<= n-1' for cmdscale()
res <- cmdscale(cophenetic(hcl), k=dim)
}
return(res)
}
