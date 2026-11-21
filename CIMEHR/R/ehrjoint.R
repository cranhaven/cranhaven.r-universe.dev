.ehrjoint_check<-function(data,vars){d<-as.data.frame(data);bad<-setdiff(unique(vars),names(d))
if(length(bad))stop("Missing columns: ",paste(bad,collapse=", "),call.=FALSE)
if(anyNA(d[[vars[1L]]]))stop("Subject id cannot be missing.",call.=FALSE);d}
.ehrjoint_equilibrate<-function(A){
  cs<-apply(abs(A),2L,max);cs[cs==0]<-1
  As<-sweep(A,2L,cs,"/");rs<-apply(abs(As),1L,max);rs[rs==0]<-1
  list(matrix=As/rs,column_scale=cs,row_scale=rs)
}
.ehrjoint_pinv<-function(A,tol=sqrt(.Machine$double.eps)){s<-svd(A);k<-s$d>tol*max(s$d,1)
if(!all(k)){
  eq<-.ehrjoint_equilibrate(A);ss<-svd(eq$matrix)
  if(length(ss$d)&&max(ss$d)>0&&all(ss$d>tol*max(ss$d)))
    return(sweep((ss$v%*%(t(ss$u)/ss$d))/eq$column_scale,2L,eq$row_scale,"/"))
}
if(!any(k))stop("Rank-zero estimating equation.",call.=FALSE)
s$v[,k,drop=FALSE]%*%(t(s$u[,k,drop=FALSE])/s$d[k])}
.ehrjoint_const<-function(d,id,vars){g<-NULL
for(v in vars){x<-d[[v]]
if(is.object(x)||!is.null(dim(x)))varies<-any(tapply(x,d[[id]],function(x)length(unique(x[!is.na(x)])))>1L,na.rm=TRUE)
else{if(is.null(g)){u<-unique(d[[id]]);s<-as.character(u);g<-match(s,unique(s))[match(d[[id]],u)]}
  ok<-!is.na(x);xo<-x[ok];go<-g[ok];varies<-any(xo!=xo[match(go,go)])}
if(varies)stop("EHRJoint requires subject-level time-invariant covariate `",v,"`.",call.=FALSE)}}
.ehrjoint_numeric<-function(d,vars){bad<-vars[!vapply(d[vars],is.numeric,logical(1))]
if(length(bad))stop("EHRJoint covariates must be numeric: ",paste(bad,collapse=", "),call.=FALSE)}

.ehrjoint_link_stats <- function(eta, link = c("logit", "probit")) {
  link <- match.arg(link)
  if (link == "logit") {
    p <- stats::plogis(eta)
    dp <- p * (1 - p)

    a <- rep(1, length(eta))
    ap <- rep(0, length(eta))
  } else {
    p <- stats::pnorm(eta)
    dp <- stats::dnorm(eta)
    den <- p * (1 - p)
    if (any(!is.finite(den) | den <= 1e-14))
      stop("Observation probit violates numerical positivity; stacked inference is unavailable.", call.=FALSE)
    a <- dp / den
    d2p <- -eta * dp
    ap <- d2p / den - dp^2 * (1 - 2 * p) / den^2
  }
  if (any(!is.finite(p)) || any(p <= 1e-10 | p >= 1 - 1e-10))
    stop("Observation-model positivity is too weak for regular stacked inference.", call.=FALSE)
  list(p=p, dp=dp, score_multiplier=a, score_multiplier_derivative=ap)
}

.ehrjoint_followup<-function(d,id,time,censor_col,entry_col){
  ids<-unique(d[[id]]);si<-match(d[[id]],ids);first<-match(ids,d[[id]]);tt<-d[[time]];vis<-is.finite(tt)
  if(!any(vis))stop("No visit events.")
  C<-if(!is.null(censor_col))d[[censor_col]][first]else rep(max(tt[vis]),length(ids))
  E<-if(!is.null(entry_col))d[[entry_col]][first]else rep(-Inf,length(ids))
  ev<-vis;ev[vis]<-tt[vis]>E[si[vis]]
  cC<-diff(range(C))<=1e-10
  list(E=E,C=C,ev=ev,common_C=if(cC)C[1L]else NA_real_)}

.ehrjoint_riskset<-function(E,C,times){ut<-sort(unique(times));K<-length(ut)
lo<-findInterval(E,ut)+1L;hi<-findInterval(C,ut);ok<-which(lo<=hi);entry<-any(lo[ok]>1L)
rsum<-function(w){M<-as.matrix(w);if(!ncol(M))return(matrix(0,K,0L))
out<-plm_ehr_rsum(M,lo,hi,K,entry)
if(is.null(dim(w)))drop(out)else out}
wsum<-function(x){M<-as.matrix(x);if(!ncol(M))return(matrix(0,length(lo),0L))
out<-plm_ehr_wsum(M,lo,hi)
if(!is.null(dimnames(M)))dimnames(out)<-list(if(!is.null(rownames(M)))c("",rownames(M))[hi+1L],colnames(M))
if(is.null(dim(x)))drop(out)else out}
list(ut=ut,K=K,rsum=rsum,wsum=wsum)}

.ehrjoint_ag<-function(g,V,rs,dN,SV){e<-exp(drop(V%*%g));S0<-rs$rsum(e)
if(any(!(S0>0)))stop("Empty EHRJoint visiting risk set at a visit time.",call.=FALSE)
Vb<-rs$rsum(V*e)/S0;ng<-ncol(V);H<-matrix(0,ng,ng,dimnames=list(colnames(V),colnames(V)))
for(a in seq_len(ng))H[a,]<--colSums(dN*(rs$rsum(V*(e*V[,a]))/S0-Vb*Vb[,a]))
list(e=e,S0=S0,Vb=Vb,U=SV-colSums(dN*Vb),H=H)}

.ehrjoint_gamma<-function(d,id,time,visit_covs,E,C,ev,tol=1e-12,accept_tol=1e-10,maxit=100L){
  .ehrjoint_const(d,id,visit_covs);.ehrjoint_numeric(d,visit_covs)
  ids<-unique(d[[id]]);si<-match(d[[id]],ids);first<-match(ids,d[[id]])
  V<-as.matrix(d[first,visit_covs,drop=FALSE]);storage.mode(V)<-"double"
  if(any(!is.finite(V)))stop("Baseline visit covariates must be finite.")
  m<-tabulate(si[ev],nbins=length(ids));M<-sum(m)
  if(!M)stop("No visit events inside the risk windows (entry, censoring].")
  tv<-d[[time]][ev];rs<-.ehrjoint_riskset(E,C,tv);ek<-match(tv,rs$ut);es<-si[ev]
  dN<-tabulate(ek,nbins=rs$K);SV<-colSums(V*m);g<-setNames(numeric(ncol(V)),visit_covs)
  lim<-tol*max(1,M);it<-0L;tiny<-FALSE
  repeat{cur<-.ehrjoint_ag(g,V,rs,dN,SV);if(tiny||max(abs(cur$U))<=lim||it>=maxit)break;it<-it+1L
  st<-tryCatch(solve(-cur$H,cur$U),error=function(e)NULL)
  if(is.null(st)||any(!is.finite(st)))stop("Singular EHRJoint visiting information.",call.=FALSE)
  for(h in 0:30){dl<-drop(V%*%st);rc<-rs$rsum(cur$e*expm1(dl))/cur$S0
  gain<-if(all(is.finite(rc)&rc>-1))sum(m*dl)-sum(dN*log1p(rc))else -Inf
  if(gain>=-1e-10)break;st<-st/2}
  if(gain< -1e-10)break;g<-g+st;tiny<-max(abs(st))<=1e-14*(1+max(abs(g)))}
  sm<-max(abs(cur$U));dL<-dN/cur$S0;Vg<-.ehrjoint_pinv(-cur$H);dimnames(Vg)<-list(visit_covs,visit_covs)
  ok<-sm<=accept_tol*max(1,M)
  list(gamma=g,vcov=Vg,ids=ids,si=si,first=first,V=V,m=m,M=M,exp_gamma=cur$e,
       converged=ok,code=as.integer(!ok),score_max=sm,
       rs=rs,dN=dN,vis_s=es,vis_k=ek,dLambda=dL,Lambda_subject=rs$wsum(dL))}

.ehrjoint_stacked_sandwich<-function(sp,gamma,alpha,sigma2,zeta,retain_influence=FALSE,rank_tol=1e-9){
  V<-sp$V;O<-sp$O;X<-sp$X;Q<-sp$Q;m<-sp$m;rs<-sp$rs;use_B<-sp$use_B;obs_link<-sp$obs_link
  n<-length(m);ng<-ncol(V);na<-ncol(O);nz<-length(zeta);K<-rs$K
  ig<-seq_len(ng);ia<-ng+seq_len(na);cur<-ng+na
  is<-if(use_B){cur<-cur+1L;cur}else integer(0)
  iz<-cur+seq_len(nz);P<-max(iz)
  pnames<-c(paste0("gamma:",colnames(V)),paste0("alpha:",colnames(O)),
            if(use_B)"sigma2",paste0("outcome:",names(zeta)))
  J<-matrix(0,P,P,dimnames=list(pnames,pnames))
  Cmat<-matrix(0,P,n,dimnames=list(pnames,NULL))
  F<-setNames(numeric(P),pnames)
  bys<-function(M,s)cimehr_group_sum(as.matrix(M),s,n)

  ag<-.ehrjoint_ag(gamma,V,rs,sp$dN,colSums(V*m));e<-ag$e;S0<-ag$S0;Vb<-ag$Vb
  dL<-sp$dN/S0;Lam<-rs$wsum(dL);nu<-e*Lam;Ai<-rs$wsum(Vb*dL)
  chain<-function(G){Gk<-rs$rsum(as.matrix(G))/S0
  bys(Gk[sp$vis_k,,drop=FALSE],sp$vis_s)-e*rs$wsum(Gk*dL)}
  evt<-bys(V[sp$vis_s,,drop=FALSE]-Vb[sp$vis_k,,drop=FALSE],sp$vis_s)
  F[ig]<-colSums(evt);J[ig,ig]<-ag$H
  Cmat[ig,]<-t(evt-e*(V*Lam-Ai))

  ls<-.ehrjoint_link_stats(drop(O%*%alpha),obs_link)
  ar<-sp$r_success-sp$r_trials*ls$p
  salpha<-ar*ls$score_multiplier
  halpha<-ar*ls$score_multiplier_derivative-sp$r_trials*ls$dp*ls$score_multiplier
  F[ia]<-colSums(O*salpha);J[ia,ia]<-crossprod(O,sweep(O,1L,halpha,"*"));Cmat[ia,]<-t(O*salpha)

  if(use_B){
    nu2<-nu^2;h<-m^2-m-(1+sigma2)*nu2;Gs<--2*(1+sigma2)*nu*e
    F[is]<-sum(h);J[is,ig]<--2*(1+sigma2)*colSums(V*nu2)-colSums(Ai*Gs);J[is,is]<--sum(nu2)
    Cmat[is,]<-h+drop(chain(Gs))
    den<-1+nu*sigma2;B<-(1+m*sigma2)/den-1
    Bnu<--sigma2*(1+m*sigma2)/den^2;Bs<-(m-nu)/den^2
    D<-cbind(X,Q*B)
  }else D<-X
  if(any(!is.finite(D)))stop("Non-finite EHRJoint outcome design.",call.=FALSE)
  pos<-m>0;iL<-numeric(n);iL[pos]<-1/Lam[pos];cc<-ls$p*m*iL
  Dn<-rs$rsum(cc);Db<-rs$rsum(D*cc)/Dn
  os<-sp$obs_s;ok<-sp$obs_k;Do<-D[os,,drop=FALSE];Ho<-Do-Db[ok,,drop=FALSE]
  r<-sp$obs_y-drop(Do%*%zeta)
  F[iz]<-colSums(Ho*r);J[iz,iz]<--crossprod(Ho,Do)
  q<-cimehr_group_sum(as.matrix(r),ok,K)[,1L]/Dn
  rho<-rs$wsum(q);Phi<-D*rho-rs$wsum(Db*q)
  rsi<-drop(bys(r,os));Hs<-bys(Ho,os)
  gD<-function(dD)dD*(rsi-cc*rho)-Hs*drop(dD%*%zeta)
  zx<-matrix(0,n,ncol(X))
  gnu<-if(use_B)gD(cbind(zx,Q*Bnu))else matrix(0,n,nz)
  G<-e*gnu+(cc*iL)*Phi
  Cmat[iz,]<-t(bys(Ho*r,os)-cc*Phi+chain(G))
  J[iz,ig]<-crossprod(gnu,V*nu)-crossprod(G,Ai)
  J[iz,ia]<--crossprod(Phi,O*(m*ls$dp*iL))
  if(use_B)J[iz,is]<-colSums(gD(cbind(zx,Q*Bs)))

  A<--J;s<-svd(A);rank<-sum(s$d>rank_tol*max(s$d,1))
  if(rank<P){
    ss<-svd(.ehrjoint_equilibrate(A)$matrix,nu=0,nv=0)$d
    rank<-if(length(ss)&&max(ss)>0)sum(ss>rank_tol*max(ss))else 0L
  }
  if(rank<P)stop("The full EHRJoint stacked bread is rank deficient (rank ",rank,
                 " of ",P,").",call.=FALSE)
  Ainv<-.ehrjoint_pinv(A,tol=rank_tol)
  IF<-t(Ainv%*%Cmat);colnames(IF)<-pnames
  VV<-crossprod(IF);dimnames(VV)<-list(pnames,pnames)
  IFL<-drop(bys((1/S0)[sp$vis_k],sp$vis_s))-e*rs$wsum(dL/S0)+drop(IF[,ig,drop=FALSE]%*%(-colSums(dL*Vb)))
  beta_pn<-paste0("outcome:",names(zeta)[seq_len(ncol(X))])
  list(vcov=VV,vcov_beta=VV[beta_pn,beta_pn,drop=FALSE],se_Lambda=sqrt(sum(IFL^2)),
       influence=if(retain_influence)IF else NULL,bread=A,case_derivative=Cmat,
       equation=F,parameter_names=pnames,rank=rank,relative_min_singular=min(s$d)/max(s$d),
       regular=use_B,
       scope=paste0(if(use_B)"full regular-interior all-stage patient stacked sandwich" else
         "branch-conditional patient stacked sandwich at the sigma2=0 selection boundary",
         "; time-specific risk sets, Breslow profiled"))
}

.fit_ehrjoint<-function(
    data,id="id",time="time",outcome="Y",obs_indicator="R",
    visit_covs=c("Z","X"),long_covs=c("Z","X"),random_covs="Z",
    obs_link=c("logit","probit"),sigma_floor=1e-6,
    censor_col=if("C"%in%names(data))"C"else NULL,entry_col=NULL,
    retain_influence=FALSE,rank_tol=1e-9,obs_fixed_covs=visit_covs){
  if(is.null(obs_fixed_covs))obs_fixed_covs<-visit_covs
  obs_link<-match.arg(obs_link)
  d<-.ehrjoint_check(data,c(id,time,outcome,obs_indicator,visit_covs,obs_fixed_covs,long_covs,random_covs,
                            if(!is.null(censor_col))censor_col,entry_col))
  .ehrjoint_numeric(d,unique(c(visit_covs,obs_fixed_covs,long_covs,random_covs)))
  fu<-.ehrjoint_followup(d,id,time,censor_col,entry_col);common_C<-fu$common_C;ev<-fu$ev
  .ehrjoint_const(d,id,unique(c(obs_fixed_covs,long_covs,random_covs)))
  ga<-.ehrjoint_gamma(d,id,time,visit_covs,fu$E,fu$C,ev)
  visit<-is.finite(d[[time]])

  bad_R<-visit&(!is.finite(d[[obs_indicator]])|!d[[obs_indicator]]%in%c(0,1))
  if(any(bad_R))
    stop("Every actual EHRJoint visit must have R equal to 0 or 1; only zero-visit placeholders may have R=NA.",call.=FALSE)
  bad_Y<-visit&d[[obs_indicator]]==1&!is.finite(d[[outcome]])
  if(any(bad_Y))
    stop("EHRJoint requires a finite marker whenever an actual visit has R=1.",call.=FALSE)

  ogood<-ev&!is.na(d[[obs_indicator]])&d[[obs_indicator]]%in%c(0,1)&
    stats::complete.cases(d[,obs_fixed_covs,drop=FALSE])
  if(sum(ogood)<=length(obs_fixed_covs)+1L)stop("Too few visit rows for observation model.")
  oform<-stats::reformulate(if(length(obs_fixed_covs))obs_fixed_covs else "1",response=obs_indicator)
  oglm<-stats::glm(oform,data=d[ogood,,drop=FALSE],family=stats::binomial(link=obs_link))
  alpha<-stats::coef(oglm);Va<-as.matrix(stats::vcov(oglm))
  if(any(!is.finite(alpha)))stop("Observation logit is unidentified (non-finite coefficients).")
  base<-d[ga$first,,drop=FALSE]
  pbase<-as.numeric(stats::predict(oglm,newdata=base,type="response"))
  if(any(!is.finite(pbase)))stop("Non-finite observation probabilities.")

  Lam<-sum(ga$dLambda);Li<-ga$exp_gamma*ga$Lambda_subject
  raw<-sum(ga$m^2-ga$m-Li^2)/sum(Li^2);sig<-max(raw,0);use<-sig>sigma_floor
  Bi<-if(use)(1+ga$m*sig)/(1+Li*sig)-1 else rep(0,length(ga$ids))
  X<-as.matrix(d[,long_covs,drop=FALSE]);storage.mode(X)<-"double";Xb<-X[ga$first,,drop=FALSE]
  Q<-as.matrix(d[,random_covs,drop=FALSE]);storage.mode(Q)<-"double"
  Q0<-cbind(`(Intercept)`=1,Q);Qb<-Q0[ga$first,,drop=FALSE]
  if(any(!is.finite(Xb))||any(!is.finite(Qb)))
    stop("EHRJoint subject-level outcome/random-effect covariates must be finite.",call.=FALSE)
  if(use){Bb<-Qb*Bi;D<-cbind(X,Q0*Bi[ga$si]);nm<-c(long_covs,paste0("theta:",colnames(Q0)))}
  else{D<-X;Bb<-NULL;nm<-long_covs};colnames(D)<-nm
  ws<-ga$m*pbase;if(sum(ws)<=0)stop("Zero EHRJoint Stage-3 weight.")
  cs<-numeric(length(ws));cs[ga$m>0]<-ws[ga$m>0]/ga$Lambda_subject[ga$m>0]
  Dbar<-ga$rs$rsum(cbind(Xb,Bb)*cs)/ga$rs$rsum(cs)
  H<-D-Dbar[match(d[[time]],ga$rs$ut),,drop=FALSE]
  obs<-ev&!is.na(d[[obs_indicator]])&d[[obs_indicator]]==1&
    is.finite(d[[outcome]])&stats::complete.cases(D)
  if(sum(obs)<=ncol(D))stop("Too few observed marker rows for EHRJoint.")
  A<-crossprod(H[obs,,drop=FALSE],D[obs,,drop=FALSE]);Ai<-.ehrjoint_pinv(A)
  cf<-as.vector(Ai%*%crossprod(H[obs,,drop=FALSE],d[[outcome]][obs]));names(cf)<-nm
  rr_cond<-d[[outcome]][obs]-drop(D[obs,,drop=FALSE]%*%cf)
  sc_cond<-cimehr_group_sum(H[obs,,drop=FALSE]*rr_cond,ga$si[obs],length(ga$ids))
  Vcond<-Ai%*%crossprod(sc_cond)%*%t(Ai)
  dimnames(Vcond)<-list(nm,nm)

  nsub<-length(ga$ids)
  r_trials<-tabulate(ga$si[ogood],nbins=nsub)
  r_success<-tabulate(ga$si[ogood&d[[obs_indicator]]==1],nbins=nsub)
  Ob<-stats::model.matrix(stats::delete.response(stats::terms(oglm)),base)
  stack<-.ehrjoint_stacked_sandwich(
    list(V=ga$V,m=ga$m,O=Ob,r_trials=r_trials,r_success=r_success,obs_link=obs_link,
         use_B=use,nz=length(cf),X=Xb,Q=Qb,rs=ga$rs,dN=ga$dN,vis_s=ga$vis_s,vis_k=ga$vis_k,
         obs_s=ga$si[obs],obs_k=match(d[[time]][obs],ga$rs$ut),obs_y=d[[outcome]][obs]),
    gamma=ga$gamma,alpha=alpha,sigma2=sig,zeta=cf,
    retain_influence=retain_influence,rank_tol=rank_tol)
  p<-length(long_covs);beta<-cf[seq_len(p)]
  out_names<-paste0("outcome:",nm)
  Vout<-stack$vcov[out_names,out_names,drop=FALSE]
  dimnames(Vout)<-list(nm,nm)
  VB<-Vout[seq_len(p),seq_len(p),drop=FALSE]
  dimnames(VB)<-list(long_covs,long_covs)
  theta_names<-if(use)nm[-seq_len(p)]else character(0)
  Vtheta<-if(use)Vout[theta_names,theta_names,drop=FALSE]else
    matrix(NA_real_,ncol(Q0),ncol(Q0),dimnames=list(colnames(Q0),colnames(Q0)))
  if(use)dimnames(Vtheta)<-list(colnames(Q0),colnames(Q0))
  pg<-paste0("gamma:",visit_covs);pa<-paste0("alpha:",colnames(Ob))
  Vg_stack<-stack$vcov[pg,pg,drop=FALSE];dimnames(Vg_stack)<-list(visit_covs,visit_covs)
  Va_stack<-stack$vcov[pa,pa,drop=FALSE];dimnames(Va_stack)<-list(colnames(Ob),colnames(Ob))
  conv<-isTRUE(ga$converged)&&isTRUE(oglm$converged)&&all(is.finite(cf))
  ans<-list(method_key="ehrjoint",method="EHRJoint",
            estimand=paste0("EHRJoint marginal longitudinal coefficient under shared visiting frailty and a measured-covariate observation ",obs_link),
            coefficients=beta,vcov=VB,
            se=sqrt(pmax(diag(VB),0)),
            vcov_type=stack$scope,
            nuisance=list(gamma=ga$gamma,vcov_gamma=Vg_stack,
                          vcov_gamma_conditional=ga$vcov,
                          alpha=alpha,vcov_alpha=Va_stack,
                          vcov_alpha_model_based=Va,observation_link=obs_link,
                          Lambda=Lam,sigma2=sig,sigma2_raw=raw,
                          se_Lambda=stack$se_Lambda,
                          se_sigma2=if(use)sqrt(stack$vcov["sigma2","sigma2"])else NA_real_,
                          theta=if(use)cf[-seq_len(p)]else setNames(rep(NA_real_,ncol(Q0)),colnames(Q0)),
                          vcov_theta=Vtheta,
                          se_theta=if(use)sqrt(pmax(diag(Vtheta),0))else setNames(rep(NA_real_,ncol(Q0)),colnames(Q0)),
                          Lambda0_hat=setNames(ga$dLambda,as.character(ga$rs$ut))),
            stacked_sandwich=list(vcov_joint=stack$vcov,vcov_outcome=Vout,
                                  vcov_outcome_conditional=Vcond,
                                  bread=stack$bread,
                                  influence=stack$influence,
                                  regular=stack$regular,scope=stack$scope,
                                  parameter_names=stack$parameter_names),
            converged=conv,
            diagnostics=list(ok=isTRUE(conv&&all(is.finite(VB))),
                             n_subjects=length(ga$ids),n_visits=ga$M,n_observed=sum(obs),
                             zero_visit_subjects=sum(ga$m==0L),common_censoring_time=common_C,
                             gamma_converged=ga$converged,gamma_code=ga$code,
                             gamma_score_max=ga$score_max,observation_glm_converged=oglm$converged,
                             condition_number=kappa(A),frailty_boundary=!use,
                             stacked_bread_rank=stack$rank,
                             stacked_relative_min_singular=stack$relative_min_singular,
                             stacked_equation_max=max(abs(stack$equation)),
                             covariance_propagates_stage12=TRUE,
                             covariance_regular=stack$regular,
                             conditional_inference_available=isTRUE(conv)&&all(is.finite(VB))&&all(diag(VB)>=0),
                             inference_scope=stack$scope),
            assumptions=c("Subject-specific risk windows (entry, censoring] with time-specific risk sets, noninformative given covariates; time-invariant covariates",
                          "Gamma shared frailty with method-of-moments variance",
                          paste0(tools::toTitleCase(obs_link)," observation process depends only on measured covariates"),
                          if(use)"Regular interior frailty variance for Wald inference" else
                            "At sigma2=0, covariance is conditional on remaining on the no-frailty branch"),
            call=match.call())
  # dA(t) = beta_0(t) dLambda_0(t).
  num <- cimehr_group_sum(as.matrix(rr_cond),match(d[[time]][obs],ga$rs$ut),length(ga$rs$ut))[,1L]
  Sobs <- as.numeric(ga$rs$rsum(cs))
  dA <- rep(NA_real_,length(Sobs));ok <- is.finite(Sobs)&Sobs>0
  dA[ok] <- num[ok]/Sobs[ok]
  low <- if(is.null(entry_col))min(c(0,d[[time]][ev])) else min(fu$E[is.finite(fu$E)])
  if(!is.finite(low))low <- min(c(0,d[[time]][ev]))
  ans$prediction <- list(
    baseline=list(type="increments",increments=data.frame(time=ga$rs$ut,dA=dA,dLambda=ga$dLambda),
                  normalization="Profiled observation-risk numerator and fitted visiting-baseline increments"),
    reference=base[,long_covs,drop=FALSE],long_covs=long_covs,coefficients=beta,
    time_col=time,time_range=c(low,max(fu$C)))
  class(ans)<-c("ehrjoint");ans
}

EHRJoint <- function(data,...) fit_method(method='ehrjoint',data=data,...)
