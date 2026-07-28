
    ## THESE TESTS ARE GENERATED AUTOMATICALLY. DO NOT MODIFY.
    test_this("With restrictions", {


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
    k = 1, miss = NULL, lost = NULL), list(treesize = 29, k = 2, 
    miss = list(9, 10), lost = list(1:7, 19:20)))
        repar = list(list(fn = "brn(ou_zaplost(oupar))", jac = "dbrn(dou_zaplost(oujac))", 
    hess = "hbrn(hou_zaplost(ouhess))", npar = "nparams_brn(D$k)", 
    parform = "c(D$sig_x)"), list(fn = "brn(ou_haltlost(oupar))", 
    jac = "dbrn(dou_haltlost(oujac))", hess = "hbrn(hou_haltlost(ouhess))", 
    npar = "nparams_brn(D$k)", parform = "c(D$sig_x)"), list(
    fn = "brn_diagSig(ou_zaplost(oupar))", jac = "dbrn_diagSig(dou_zaplost(oujac))", 
    hess = "hbrn_diagSig(hou_zaplost(ouhess))", npar = "nparams_brn_diagSig(D$k)", 
    parform = "{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)}"), 
    list(fn = "brn_diagSig(ou_haltlost(oupar))", jac = "dbrn_diagSig(dou_haltlost(oujac))", 
        hess = "hbrn_diagSig(hou_haltlost(ouhess))", npar = "nparams_brn_diagSig(D$k)", 
        parform = "{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)}"), 
    list(fn = "ou_diagH(ou_zaplost(oupar))", jac = "dou_diagH(dou_zaplost(oujac))", 
        hess = "hou_diagH(hou_zaplost(ouhess))", npar = "nparams_ou_diagH(D$k)", 
        parform = "c(diag(D$H),D$theta,D$sig_x)"), list(fn = "ou_diagH(ou_haltlost(oupar))", 
        jac = "dou_diagH(dou_haltlost(oujac))", hess = "hou_diagH(hou_haltlost(ouhess))", 
        npar = "nparams_ou_diagH(D$k)", parform = "c(diag(D$H),D$theta,D$sig_x)"), 
    list(fn = "ou_logdiagH(ou_zaplost(oupar))", jac = "dou_logdiagH(dou_zaplost(oujac))", 
        hess = "hou_logdiagH(hou_zaplost(ouhess))", npar = "nparams_ou_logdiagH(D$k)", 
        parform = "c(diag(D$H),D$theta,D$sig_x)"), list(fn = "ou_logdiagH(ou_haltlost(oupar))", 
        jac = "dou_logdiagH(dou_haltlost(oujac))", hess = "hou_logdiagH(hou_haltlost(ouhess))", 
        npar = "nparams_ou_logdiagH(D$k)", parform = "c(diag(D$H),D$theta,D$sig_x)"), 
    list(fn = "ou_logdiagH_diagSig(ou_haltlost(oupar))", jac = "dou_logdiagH_diagSig(dou_haltlost(oujac))", 
        hess = "hou_logdiagH_diagSig(hou_haltlost(ouhess))", 
        npar = "nparams_ou_logdiagH_diagSig(D$k)", parform = "c(diag(D$H),D$theta,{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_symH(ou_zaplost(oupar))", jac = "dou_symH(dou_zaplost(oujac))", 
        hess = "hou_symH(hou_zaplost(ouhess))", npar = "nparams_ou_symH(D$k)", 
        parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,D$sig_x)"), 
    list(fn = "ou_symH(ou_haltlost(oupar))", jac = "dou_symH(dou_haltlost(oujac))", 
        hess = "hou_symH(hou_haltlost(ouhess))", npar = "nparams_ou_symH(D$k)", 
        parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,D$sig_x)"), 
    list(fn = "ou_symH_diagSig(ou_zaplost(oupar))", jac = "dou_symH_diagSig(dou_zaplost(oujac))", 
        hess = "hou_symH_diagSig(hou_zaplost(ouhess))", npar = "nparams_ou_symH_diagSig(D$k)", 
        parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_symH_diagSig(ou_haltlost(oupar))", jac = "dou_symH_diagSig(dou_haltlost(oujac))", 
        hess = "hou_symH_diagSig(hou_haltlost(ouhess))", npar = "nparams_ou_symH_diagSig(D$k)", 
        parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_logspdH(ou_zaplost(oupar))", jac = "dou_logspdH(dou_zaplost(oujac))", 
        hess = "hou_logspdH(hou_zaplost(ouhess), dou_zaplost(oujac))", 
        npar = "nparams_ou_logspdH(D$k)", parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,D$sig_x)"), 
    list(fn = "ou_logspdH(ou_haltlost(oupar))", jac = "dou_logspdH(dou_haltlost(oujac))", 
        hess = "hou_logspdH(hou_haltlost(ouhess), dou_haltlost(oujac))", 
        npar = "nparams_ou_logspdH(D$k)", parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,D$sig_x)"), 
    list(fn = "ou_spdH(ou_zaplost(oupar))", jac = "dou_spdH(dou_zaplost(oujac))", 
        hess = "hou_spdH(hou_zaplost(ouhess), dou_zaplost(oujac))", 
        npar = "nparams_ou_spdH(D$k)", parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,D$sig_x)"), 
    list(fn = "ou_diagH_diagSig(ou_haltlost(oupar))", jac = "dou_diagH_diagSig(dou_haltlost(oujac))", 
        hess = "hou_diagH_diagSig(hou_haltlost(ouhess))", npar = "nparams_ou_diagH_diagSig(D$k)", 
        parform = "c(diag(D$H),D$theta,{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_logspdH_diagSig(ou_haltlost(oupar))", jac = "dou_logspdH_diagSig(dou_haltlost(oujac))", 
        hess = "hou_logspdH_diagSig(hou_haltlost(ouhess), dou_haltlost(oujac))", 
        npar = "nparams_ou_logspdH_diagSig(D$k)", parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_spdH(ou_haltlost(oupar))", jac = "dou_spdH(dou_haltlost(oujac))", 
        hess = "hou_spdH(hou_haltlost(ouhess), dou_haltlost(oujac))", 
        npar = "nparams_ou_spdH(D$k)", parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,D$sig_x)"), 
    list(fn = "ou_logspdH_diagSig(ou_haltlost(oupar))", jac = "dou_logspdH_diagSig(dou_haltlost(oujac))", 
        hess = "hou_logspdH_diagSig(hou_haltlost(ouhess), dou_haltlost(oujac))", 
        npar = "nparams_ou_logspdH_diagSig(D$k)", parform = "c(D$H[lower.tri(D$H,diag=T)],D$theta,{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_logspdH_fixedtheta_diagSig(ou_haltlost(oupar), theta=D$theta)", 
        jac = "dou_logspdH_fixedtheta_diagSig(dou_haltlost(oujac), theta=D$theta)", 
        hess = "hou_logspdH_fixedtheta_diagSig(hou_haltlost(ouhess), dou_haltlost(oujac), theta=D$theta)", 
        npar = "nparams_ou_logspdH_fixedtheta_diagSig(D$k)", 
        parform = "c(D$H[lower.tri(D$H,diag=T)],{tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_fixedH_fixedtheta_diagSig(ou_haltlost(oupar), H=D$H, theta=D$theta)", 
        jac = "dou_fixedH_fixedtheta_diagSig(dou_haltlost(oujac), H=D$H, theta=D$theta)", 
        hess = "hou_fixedH_fixedtheta_diagSig(hou_haltlost(ouhess), H=D$H, theta=D$theta)", 
        npar = "nparams_ou_fixedH_fixedtheta_diagSig(D$k)", parform = "c({tmp=matrix(0.,D$k,D$k); tmp[lower.tri(tmp,diag=T)]=D$sig_x; if(D$k==1L) c(tmp) else diag(tmp)})"), 
    list(fn = "ou_fixedtheta_fixedSig(ou_zaplost(oupar), theta=D$theta, Sig=D$sig_x)", 
        jac = "dou_fixedtheta_fixedSig(dou_zaplost(oujac), theta=D$theta, Sig=D$sig_x)", 
        hess = "hou_fixedtheta_fixedSig(hou_zaplost(ouhess), theta=D$theta, Sig=D$sig_x)", 
        npar = "nparams_ou_fixedtheta_fixedSig(D$k)", parform = "c(D$H)"))
        i = 1
        D = gen_data(i*314, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
            j = 1
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.9093028825, tol=1e-4)
        
            j = 2
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.9093028825, tol=1e-4)
        
            j = 3
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.9093028825, tol=1e-4)
        
            j = 4
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.9093028825, tol=1e-4)
        
            j = 5
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -15.7657127690, tol=1e-4)
        
            j = 6
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -15.7657127690, tol=1e-4)
        
            j = 7
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -4.6109789709, tol=1e-4)
        
            j = 8
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -4.6109789709, tol=1e-4)
        
            j = 9
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -4.6109789709, tol=1e-4)
        
            j = 10
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -15.7657127690, tol=1e-4)
        
            j = 11
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -15.7657127690, tol=1e-4)
        
            j = 12
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -15.7657127690, tol=1e-4)
        
            j = 13
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -15.7657127690, tol=1e-4)
        
            j = 14
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.1075126139, tol=1e-4)
        
            j = 15
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.1075126139, tol=1e-4)
        
            j = 16
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -0.8864052260, tol=1e-4)
        
            j = 17
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -15.7657127690, tol=1e-4)
        
            j = 18
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.1075126139, tol=1e-4)
        
            j = 19
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -0.8864052260, tol=1e-4)
        
            j = 20
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.1075126139, tol=1e-4)
        
            j = 21
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -6.1936353847, tol=1e-4)
        
            j = 22
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -7.6742890373, tol=1e-4)
        
            j = 23
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -0.2649126359, tol=1e-4)
        
        i = 2
        D = gen_data(i*314, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
            j = 1
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -24.1310923351, tol=1e-4)
        
            j = 2
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -24.1310923351, tol=1e-4)
        
            j = 3
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -24.1310923351, tol=1e-4)
        
            j = 4
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -24.1310923351, tol=1e-4)
        
            j = 5
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -41.1380058979, tol=1e-4)
        
            j = 6
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -41.1380058979, tol=1e-4)
        
            j = 7
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -122.9088186939, tol=1e-4)
        
            j = 8
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -122.9088186939, tol=1e-4)
        
            j = 9
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -122.9088186939, tol=1e-4)
        
            j = 10
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -41.1380058979, tol=1e-4)
        
            j = 11
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -41.1380058979, tol=1e-4)
        
            j = 12
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -41.1380058979, tol=1e-4)
        
            j = 13
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -41.1380058979, tol=1e-4)
        
            j = 14
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -291.8124158605, tol=1e-4)
        
            j = 15
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -291.8124158605, tol=1e-4)
        
            j = 16
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -49.4063166226, tol=1e-4)
        
            j = 17
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -41.1380058979, tol=1e-4)
        
            j = 18
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -291.8124158605, tol=1e-4)
        
            j = 19
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -49.4063166226, tol=1e-4)
        
            j = 20
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -291.8124158605, tol=1e-4)
        
            j = 21
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -0.0000019392, tol=1e-4)
        
            j = 22
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -46.7209415710, tol=1e-4)
        
            j = 23
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -1.0155016915, tol=1e-4)
        
        i = 3
        D = gen_data(i*314, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
            j = 1
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.5600577846, tol=1e-4)
        
            j = 2
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.5600577846, tol=1e-4)
        
            j = 3
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.5600577846, tol=1e-4)
        
            j = 4
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.5600577846, tol=1e-4)
        
            j = 5
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.8012267595, tol=1e-4)
        
            j = 6
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.8012267595, tol=1e-4)
        
            j = 7
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -9.1136740797, tol=1e-4)
        
            j = 8
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -9.1136740797, tol=1e-4)
        
            j = 9
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -9.1136740797, tol=1e-4)
        
            j = 10
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.8012267595, tol=1e-4)
        
            j = 11
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.8012267595, tol=1e-4)
        
            j = 12
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.8012267595, tol=1e-4)
        
            j = 13
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.8012267595, tol=1e-4)
        
            j = 14
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -72.7750264724, tol=1e-4)
        
            j = 15
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -72.7750264724, tol=1e-4)
        
            j = 16
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -5.4091624372, tol=1e-4)
        
            j = 17
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -2.8012267595, tol=1e-4)
        
            j = 18
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -72.7750264724, tol=1e-4)
        
            j = 19
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -5.4091624372, tol=1e-4)
        
            j = 20
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -72.7750264724, tol=1e-4)
        
            j = 21
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), 0.0833087772, tol=1e-4)
        
            j = 22
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -3.0320863977, tol=1e-4)
        
            j = 23
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -0.4523709245, tol=1e-4)
        
        i = 4
        D = gen_data(i*314, par[[i]]$treesize, par[[i]]$k,
                     par[[i]]$miss, par[[i]]$lost)
            j = 1
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -57.1670072307, tol=1e-4)
        
            j = 2
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -57.1670072307, tol=1e-4)
        
            j = 3
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -35.4156831611, tol=1e-4)
        
            j = 4
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -35.4156831611, tol=1e-4)
        
            j = 5
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -36.3412777622, tol=1e-4)
        
            j = 6
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -36.3412777622, tol=1e-4)
        
            j = 7
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -75.8308281307, tol=1e-4)
        
            j = 8
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -75.8308281307, tol=1e-4)
        
            j = 9
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -109.3607766455, tol=1e-4)
        
            j = 10
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -19.2547160941, tol=1e-4)
        
            j = 11
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -20.7922108725, tol=1e-4)
        
            j = 12
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -46.0357165860, tol=1e-4)
        
            j = 13
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -45.8695649863, tol=1e-4)
        
            j = 14
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), 79.6285321542, tol=1e-4)
        
            j = 15
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), 79.7424933880, tol=1e-4)
        
            j = 16
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -214.4338393448, tol=1e-4)
        
            j = 17
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -45.5022648431, tol=1e-4)
        
            j = 18
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -81.5863147043, tol=1e-4)
        
            j = 19
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -211.5526390619, tol=1e-4)
        
            j = 20
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -81.5863147043, tol=1e-4)
        
            j = 21
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), 8.0972191326, tol=1e-4)
        
            j = 22
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -56.4152762303, tol=1e-4)
        
            j = 23
            mod = glinv(D$tr, D$x0, D$x,
                    parfns = list(eval(parse(text=repar[[j]]$fn))),
                    pardims = list(eval(parse(text=repar[[j]]$npar))),
                    parjacs = list(eval(parse(text=repar[[j]]$jac))),
                    parhess = list(eval(parse(text=repar[[j]]$hess))))
            varest = suppressWarnings(varest(mod, eval(parse(text=repar[[j]]$parform)),
                                      method="analytical"))
            expect_equal(sum(varest$hessian), -29.8335569825, tol=1e-4)
        
    })
