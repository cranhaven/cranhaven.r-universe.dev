toint = function (x)         {mode(x) = 'integer'; x }
nullify = function (mod)     base::unserialize(base::serialize(mod, NULL))
nullify_interleave = function (mod, fnlist, tointerleave = nullify) {
  fnlist_p = vector('list', length(fnlist)+1L -> n)
  fnlist_p[[1L]] = tointerleave
  fnlist_p[2L:(length(fnlist)+1L)] = fnlist
  i = 1L
  while(n > 1L) {
    Reduce(function (acc,f) f(acc), fnlist_p, mod)
    fnlist_p = replace(fnlist_p, c(i,i+1L), fnlist_p[c(i+1L,i)])
    n = n - 1L
    i = i + 1L
  }
}

test_this("Utility C functions: get root", {
  expect_equal(.Call(Rgetroot, toint(t((ape::rtree(10L)->tr1)$edge))), 11L)
  expect_equal(.Call(Rgetroot, toint(t((ape::rtree(27L)->tr2)$edge))), 28L)
  perm = sample(nrow(tr2$edge))
  tr2$edge        = tr2$edge[perm,]
  tr2$edge.length = tr2$edge.length[perm]
  expect_equal(.Call(Rgetroot, toint(t(tr2$edge))), 28L)
  tr2$edge = rbind(tr2$edge, c(1000L, 3000L))
  expect_equal(.Call(Rgetroot, toint(t(tr2$edge))), 28L)
  tr3=tr2
  tr3$edge[which(tr3$edge[,2]==29L),2] = 28L
  expect_equal(.Call(Rgetroot, toint(t(tr3$edge))), 29L)
  tr1$edge[,2] = 1000L
  expect_equal(.Call(Rgetroot, toint(t(tr1$edge))), 1L)

  tr        = ape::rtree(30L)
  root1     = .Call(Rgetroot, t(tr$edge))
  for (i in 1L:4L) {
    tr$edge   = tr$edge[sample(nrow(tr$edge)),]
    root2     = .Call(Rgetroot, t(tr$edge))
    expect_equal(root2, 31L)
    expect_equal(root1, root2)
  }
})

test_this("Utility C functions: fix tree", {
  set.seed(100)
  tro = ape::rtree(10L)
  trf = fix_tree(tro)
  expect_equal(.Call(Rgetroot, toint(t(tro$edge))), .Call(Rgetroot, toint(t(trf$edge))))
  expect_equal(.Call(Rgetroot, toint(t(tro$edge))), trf$edge[1L,1L])
  tro2 = tro
  tro2$edge = tro$edge[sample(nrow(tro$edge)),]
  trf2 = fix_tree(tro2)
  expect_equal(c(trf2$edge),c(trf2$edge))
  tro2$edge[1L,1L] = -1L
  expect_error(fix_tree(tro2))
  tro2$edge[1L,1L] = 25L
  expect_error(fix_tree(tro2))
})

test_this("Weird trees, null data and reset missing tip masks", {
  set.seed(1)
  ntips = 15
  k     = 3         # No. of trait dimensions
  tr    = ape::rtree(ntips)
  tr    = replace(tr, 'edge', list(tr$edge[sample(nrow(tr$edge)),]))
  x0    = rnorm(k)  # Root value
  repar   = get_restricted_ou(H='logdiag', theta=NULL, Sig='diag', lossmiss='halt')
  mod     = glinv(tr, x0, NULL,
                  pardims = repar$nparams(k),
                  parfns  = repar$par,
                  parjacs = repar$jac,
                  parhess = repar$hess)
  H     = diag(0.2, 3L)
  theta = c(0.,0.,0.)
  sig   = diag(0.4, 3L)
  truth = c(H=log(diag(H)), theta=theta, sig_x=log(diag(sig)))
  NAify = function (X) {X[1,2] = NA; X}
  tmp   = NULL
  nullify_interleave(clone_model(mod),
                     list(function (mod) { clone_model(mod) },
                          function (mod) { expect_is(rglinv(mod, truth, Nsamp=2), 'list'); mod },
                          function (mod) { expect_false(has_tipvals(mod));                 mod },
                          function (mod) { expect_error(lik(mod)(truth));                  mod },
                          function (mod) { expect_is(set_tips(mod, rglinv(mod, truth, Nsamp=1)[[1]]),'glinv'); mod},
                          function (mod) { expect_true(has_tipvals(mod));                  mod },
                          function (mod) { expect_is(tmp<<-lik(mod)(truth), 'numeric');    mod },
                          function (mod) { expect_equal(tmp, lik(mod)(truth));             mod },
                          function (mod) { expect_is(grad(mod)(truth), 'numeric');                              mod},
                          function (mod) { expect_is(hess(mod)(truth), 'array');                                mod},
                          function (mod) { expect_is(set_tips(mod, NAify(rglinv(mod, truth, Nsamp=1)[[1]])),'glinv'); mod},
                          function (mod) { expect_equal(as.character(mod$misstags[1,2]),    'MISSING');         mod},
                          function (mod) { expect_equal(as.character(mod$misstags[1,1]),    'OK');              mod},
                          function (mod) { expect_is(l<-lik(mod)(truth), 'numeric'); expect_false(l==tmp);      mod},
                          function (mod) { expect_is(grad(mod)(truth), 'numeric');                              mod},
                          function (mod) { expect_is(hess(mod)(truth), 'array');                                mod}))
})
