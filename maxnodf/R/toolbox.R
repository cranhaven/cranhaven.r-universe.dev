pmin2 <- function(xvec, yvec){
    #res <- pmin3(xvec, yvec)
    #print(c(length(xvec), length(yvec), length(res)))
    #return(res)
    return(((xvec + yvec) - (abs(xvec - yvec))) / 2)
}

# Computes the row marginal totals
marginal_total0 <- function(mtx){
    mt_0 <- rowSums(mtx, na.rm = FALSE, dims = 1)
    return(mt_0)
}

# Computes the column marginal totals
marginal_totalt <- function(mtx){
    mt_t <- colSums(mtx, na.rm = FALSE, dims = 1)
    return(mt_t)
}

# Assembles the list of marginal totals
compute_marginal_totals <- function(mtx){
    mt_0  <- marginal_total0(mtx)
    mt_t  <- marginal_totalt(mtx)
    return(list(mt_0, mt_t))
}

# Computes a list containing both Fill matrices
compute_fill_factors <- function(mtx){
    F0 <- mtx %*% t(mtx)
    Ft <- t(mtx) %*% mtx
    return(list(F0, Ft))
}

# Compute a list containing both degree matrices
compute_deg_mtx <- function (mtx) {
    NodesA <- nrow(mtx)
    NodesB <- ncol(mtx)
    mt_0 <- marginal_total0(mtx)
    mt_t <- marginal_totalt(mtx)
    deg_mtx0 <- matrix(mt_0, nrow=length(mt_0),ncol=length(mt_0),byrow=TRUE)
    deg_mtxt <- matrix(mt_t, nrow=length(mt_t),ncol=length(mt_t),byrow=TRUE)
    return(list(deg_mtx0, deg_mtxt))
}

# Compute a list containing both degree minima matrices
compute_deg_minima <- function(mtx){
    my_ans <- compute_deg_mtx(mtx)
    DM0 <- my_ans[[1]]
    DMt <- my_ans[[2]]
    deg_min0 <- pmin2(DM0, t(DM0))
    deg_mint <- pmin2(DMt, t(DMt))
    return(list(deg_min0, deg_mint))
}

# Compute a list containing both negative delta matrices
compute_neg_deltas <- function(mtx){
    mt_0 <- marginal_total0(mtx)
    mt_t <- marginal_totalt(mtx)
    neg_delta0 = outer(mt_0, mt_0, FUN = ">")
    neg_deltat = outer(mt_t, mt_t, FUN = ">")
    return(list(neg_delta0, neg_deltat))
}

# Compute a list containing both the row and column sum values
compute_sums <- function(mtx){
    my_res <- compute_fill_factors(mtx)
    F0 <- my_res[[1]]
    Ft <- my_res[[2]]
    my_res <- compute_deg_minima(mtx)
    DM0 <- my_res[[1]]
    DMt <- my_res[[2]]
    my_res <- compute_neg_deltas(mtx)
    ND0 <- my_res[[1]]
    NDt <- my_res[[2]]
    n_paris0 = F0[ND0] / (DM0[ND0])
    n_parist = Ft[NDt] / (DMt[NDt])
    sum0 = sum(n_paris0)
    sumt = sum(n_parist)
    return(c(sum0, sumt))
}

# Assembles the list containing all the additional data required
# for fast_nodf computations
init_nodf <- function(mtx){
    MT <- compute_marginal_totals(mtx)
    Fill <- compute_fill_factors(mtx)
    DM <- compute_deg_minima(mtx)
    ND <- compute_neg_deltas(mtx)
    S <- compute_sums(mtx)
    return(list(MT, Fill, DM, ND, S))
}

# Efficient way to compute the nodf value of a neighbor graph
nodf_neighbor2 <- function(mtx, oPos, zPos, mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S){
    xpos <- zPos[[1]]
    ypos <- zPos[[2]]
    my_nodf0<- nodf_one_link_added_cpp(mtx, xpos, ypos, mt_0, mt_t, F0, Ft,
                                        DM0, DMt, ND0, NDt, S)
    xpos <- oPos[[1]]
    ypos <- oPos[[2]]
    my_nodf <- nodf_one_link_removed_cpp(mtx, xpos, ypos, mt_0, mt_t, F0, Ft,
                                        DM0, DMt, ND0, NDt, S)
    return(my_nodf)
}