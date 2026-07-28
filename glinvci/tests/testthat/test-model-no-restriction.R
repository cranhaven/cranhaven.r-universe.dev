    ## THESE TESTS ARE GENERATED AUTOMATICALLY. DO NOT MODIFY.
    test_this("Without reparameterisation", {
        gen_data = function (seed, treesize, k, missing_ranges, lost_ranges) 
{
    RNGkind(sample.kind = "Rejection")
    set.seed(seed)
    tr = ape::rtree(treesize)
    x = matrix(rnorm(k * treesize), k, treesize)
    x0 = rnorm(k)
    H = matrix(rnorm(k * k), k)
    H[lower.tri(H, diag = F)] = H[lower.tri(H, diag = F)]/5
    H[upper.tri(H, diag = F)] = H[upper.tri(H, diag = F)]/5
    theta = rnorm(k)
    sig_x_frs = matrix(0, k, k)
    sig_x_frs[upper.tri(sig_x_frs, diag = T)] = rnorm(k * (k + 
        1)/2)
    diag(sig_x_frs) = sqrt(abs(diag(sig_x_frs))) + 1
    sig = sig_x_frs %*% t(sig_x_frs)
    tmp = t(chol(sig))
    diag(tmp) = log(diag(tmp))
    sig_x = tmp[lower.tri(sig, diag = T)]
    if (!is.null(missing_ranges)) 
        for (rgidx in seq_along(missing_ranges)) x[rgidx, missing_ranges[[rgidx]]] = NA
    if (!is.null(lost_ranges)) 
        for (rgidx in seq_along(lost_ranges)) {
            x[rgidx, lost_ranges[[rgidx]]] = NaN
        }
    list(tr = tr, k = k, x = x, x0 = x0, H = H, theta = theta, 
        sig_x = sig_x, sig_x_frs = sig_x_frs, sig = sig)
}
        par = list(list(treesize = 2, k = 1, miss = NULL, lost = NULL), list(
    treesize = 3, k = 1, miss = NULL, lost = NULL), list(treesize = 4, 
    k = 1, miss = NULL, lost = NULL), list(treesize = 20, k = 2, 
    miss = list(1:4, NULL), lost = list(NULL, 10:14)), list(treesize = 99, 
    k = 3, miss = list(3:10, 8, c(55, 60)), lost = list(80:89, 
        20:25, NULL)))
        ## ------ START TEST 01!
                
        i = 1
        D = gen_data(i*5201, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
        glinvmod = glinv(D$tr, D$x0, D$x,
                         parfns = list(ou_zaplost(oupar)),
                         pardims = list(nparams_ou(D$k)),
                         parjacs = list(dou_zaplost(oujac)),
                         parhess = list(hou_zaplost(ouhess)))
        lik.glinv = lik(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        grad.glinv = grad(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        varest.glinv = suppressWarnings(
            varest(glinvmod,unlist(list(D$H,D$theta,D$sig_x)),method="analytical"))
        expect_equal(lik.glinv, -7.7000309955, tol=1e-4)
        expect_equal(sum(grad.glinv), 10.2448835853, tol=1e-4)
        expect_equal(sum(varest.glinv$hessian), -21.9144507867, tol=1e-4)
        i = 2
        D = gen_data(i*5201, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
        glinvmod = glinv(D$tr, D$x0, D$x,
                         parfns = list(ou_zaplost(oupar)),
                         pardims = list(nparams_ou(D$k)),
                         parjacs = list(dou_zaplost(oujac)),
                         parhess = list(hou_zaplost(ouhess)))
        lik.glinv = lik(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        grad.glinv = grad(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        varest.glinv = suppressWarnings(
            varest(glinvmod,unlist(list(D$H,D$theta,D$sig_x)),method="analytical"))
        expect_equal(lik.glinv, -4.5857602372, tol=1e-4)
        expect_equal(sum(grad.glinv), -0.3746453914, tol=1e-4)
        expect_equal(sum(varest.glinv$hessian), 0.7919421307, tol=1e-4)
        i = 3
        D = gen_data(i*5201, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
        glinvmod = glinv(D$tr, D$x0, D$x,
                         parfns = list(ou_zaplost(oupar)),
                         pardims = list(nparams_ou(D$k)),
                         parjacs = list(dou_zaplost(oujac)),
                         parhess = list(hou_zaplost(ouhess)))
        lik.glinv = lik(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        grad.glinv = grad(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        varest.glinv = suppressWarnings(
            varest(glinvmod,unlist(list(D$H,D$theta,D$sig_x)),method="analytical"))
        expect_equal(lik.glinv, -11.1123449633, tol=1e-4)
        expect_equal(sum(grad.glinv), 9.2663673345, tol=1e-4)
        expect_equal(sum(varest.glinv$hessian), -23.3817815558, tol=1e-4)
        i = 4
        D = gen_data(i*5201, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
        glinvmod = glinv(D$tr, D$x0, D$x,
                         parfns = list(ou_zaplost(oupar)),
                         pardims = list(nparams_ou(D$k)),
                         parjacs = list(dou_zaplost(oujac)),
                         parhess = list(hou_zaplost(ouhess)))
        lik.glinv = lik(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        grad.glinv = grad(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        varest.glinv = suppressWarnings(
            varest(glinvmod,unlist(list(D$H,D$theta,D$sig_x)),method="analytical"))
        expect_equal(lik.glinv, -80.4853129893, tol=1e-4)
        expect_equal(sum(grad.glinv), 14.4067625358, tol=1e-4)
        expect_equal(sum(varest.glinv$hessian), -40.6134235336, tol=1e-4)
        i = 5
        D = gen_data(i*5201, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
        glinvmod = glinv(D$tr, D$x0, D$x,
                         parfns = list(ou_zaplost(oupar)),
                         pardims = list(nparams_ou(D$k)),
                         parjacs = list(dou_zaplost(oujac)),
                         parhess = list(hou_zaplost(ouhess)))
        lik.glinv = lik(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        grad.glinv = grad(glinvmod)(unlist(list(D$H,D$theta,D$sig_x)))
        varest.glinv = suppressWarnings(
            varest(glinvmod,unlist(list(D$H,D$theta,D$sig_x)),method="analytical"))
        expect_equal(lik.glinv, -577.8071138033, tol=1e-4)
        expect_equal(sum(grad.glinv), 124.5431408527, tol=1e-4)
        expect_equal(sum(varest.glinv$hessian), -1242.1247178654, tol=1e-4)
        ## ------ END TEST 01!
    })

