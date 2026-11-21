.pairlik_check <- function(data, vars) {
  d <- as.data.frame(data); miss <- setdiff(unique(vars), names(d))
  if (length(miss)) stop("Missing columns: ",paste(miss,collapse=", "),call.=FALSE)
  if (anyNA(d[[vars[1L]]])) stop("Subject id cannot be missing.",call.=FALSE); d
}
.pairlik_inv <- function(A,tol=sqrt(.Machine$double.eps)) {
  z<-svd(as.matrix(A)); if(!length(z$d)||max(z$d)==0)stop("Pairwise information is singular.",call.=FALSE)
  keep<-z$d>tol*max(z$d); if(!all(keep))stop("Pairwise beta scale/design is not identified.",call.=FALSE)
  z$v%*%(t(z$u)/z$d)
}
.pairlik_seed <- function(seed,expr) {
  had<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE); if(had)old<-get(".Random.seed",envir=.GlobalEnv)
  on.exit(if(had)assign(".Random.seed",old,envir=.GlobalEnv) else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  set.seed(as.integer(seed)); force(expr)
}
.pairlik_pairs <- function(n,max_pairs,seed) {
  np<-n*(n-1)/2
  if(np<=max_pairs) return(cbind(rep.int(seq_len(n-1L),(n-1L):1L),sequence((n-1L):1L,from=2:n),deparse.level=0))
  .pairlik_seed(seed,{

    k<-sort(sample.int(np,max_pairs,replace=FALSE))
    i<-ceiling(((2*n-1)-sqrt((2*n-1)^2-8*k))/2)
    before<-(i-1)*(2*n-i)/2
    cbind(as.integer(i),as.integer(i+k-before))
  })
}
.pairlik_select_joint_meat <- function(Gsub,hsub,pair_outer,tol=1e-12) {

  hajek<-crossprod(cbind(Gsub,hsub))
  q1<-ncol(Gsub);q2<-ncol(hsub)
  raw<-hajek
  raw[q1+seq_len(q2),q1+seq_len(q2)]<-crossprod(hsub)-pair_outer
  raw<-(raw+t(raw))/2
  ee<-eigen(raw,symmetric=TRUE)
  scale<-max(1,max(abs(ee$values)))
  rel_min<-min(ee$values)/scale
  fallback<-rel_min < -tol
  roundoff_repair<-!fallback && min(ee$values)<0
  selected<-if(fallback) {
    hajek
  } else if(roundoff_repair) {
    ee$vectors%*%(pmax(ee$values,0)*t(ee$vectors))
  } else raw
  dimnames(selected)<-dimnames(raw)
  list(selected=selected,raw=raw,hajek=hajek,
       raw_min_eigen=min(ee$values),raw_relative_min_eigen=rel_min,
       fallback=fallback,roundoff_repair=roundoff_repair,tolerance=tol)
}

.pairlik_fit_working_scale <- function(W0,y,subject_index,subject_ids) {
  W0<-as.matrix(W0);y<-as.numeric(y)
  if(nrow(W0)!=length(y)||length(subject_index)!=length(y))
    stop("Internal PairLik working-scale row alignment failure.",call.=FALSE)
  qr0<-qr(W0,tol=1e-10);p<-qr0$rank
  if(p<1L)stop("Working-scale mean design has zero rank.",call.=FALSE)
  keep<-sort(qr0$pivot[seq_len(p)])
  dropped<-setdiff(colnames(W0),colnames(W0)[keep])
  W<-W0[,keep,drop=FALSE]
  bf<-stats::lm.fit(W,y)
  n<-length(y)
  if(n<=p)stop("Eligible observed sample is too small for RSS/(Nobs-rank).",call.=FALSE)
  rss<-sum(bf$residuals^2);s2<-rss/(n-p)
  if(!is.finite(s2)||s2<=0)stop("Observed-row RSS produced a non-positive working dispersion.",call.=FALSE)
  gamma<-drop(bf$coefficients);names(gamma)<-colnames(W)
  list(W=W,y=y,residuals=drop(bf$residuals),
       coefficients=gamma,n=n,p=p,df=n-p,rss=rss,dispersion=s2,
       dropped_columns=dropped,subject_index=as.integer(subject_index),
       subject_ids=subject_ids)
}

.fit_pairlik <- function(
    data,id="id",time="time",outcome="Y",obs_indicator="R",
    visit_covs=c("Z","X"),long_covs=c("Z","X"),random_covs=NULL,
    process=c("marker","visit"),time_formula=~time,
    max_subject_pairs=20000L,seed=20260815L,
    optimizer_control=list(maxit=2000L)) {
  process<-match.arg(process)
  d<-.pairlik_check(data,c(id,time,outcome,obs_indicator,long_covs))
  ft<-is.finite(d[[time]])
  marker<-ft & !is.na(d[[obs_indicator]]) & d[[obs_indicator]]==1L
  eligible<-marker & is.finite(d[[outcome]])
  eligible[is.na(eligible)]<-FALSE
  mf<-stats::model.frame(stats::reformulate(long_covs,response=outcome),d[eligible,,drop=FALSE],na.action=stats::na.fail)
  X0<-stats::model.matrix(stats::terms(mf),mf); y<-stats::model.response(mf)
  if(!is.null(time_formula)) {
    if(!inherits(time_formula,"formula")||length(time_formula)!=2L||!all(all.vars(time_formula)%in%time))
      stop("time_formula must be a one-sided formula in the time column, or NULL.",call.=FALSE)
    Tm<-stats::model.matrix(time_formula,stats::model.frame(time_formula,d[eligible,,drop=FALSE],na.action=stats::na.fail))
    X0<-cbind(X0,Tm[,colnames(Tm)!="(Intercept)",drop=FALSE])
  }
  X<-X0[,colnames(X0)!="(Intercept)",drop=FALSE]
  if(!ncol(X))stop("The intercept cancels from conditional PairLik; no slope remains.",call.=FALSE)
  ids<-d[[id]][eligible]; uid<-unique(ids); ii<-match(ids,uid)
  if(length(uid)<3L)stop("At least three subjects with observed outcomes are required.",call.=FALSE)
  working_scale<-.pairlik_fit_working_scale(X0,y,ii,uid)
  dispersion<-working_scale$dispersion
  dispersion_source<-"observed eligible outcome RSS/(Nobs-rank)"

  P<-.pairlik_pairs(length(uid),as.integer(max_subject_pairs),seed)
  subject_rows<-split(seq_along(ii),factor(ii,levels=seq_len(length(uid))))
  rows<-unlist(subject_rows,use.names=FALSE)-1L;off<-c(0L,cumsum(lengths(subject_rows)))
  yd<-as.numeric(y);Xd<-unname(X);storage.mode(Xd)<-"double";P1<-as.integer(P[,1]);P2<-as.integer(P[,2])
  used_cross<-pairlik_n_cross(off,P1,P2)
  last<-list(NULL,NULL)
  nllgr<-function(theta,gradient=FALSE) {
    if(gradient)return(pairlik_gradient(theta,yd,Xd,rows,off,P1,P2))
    if(!identical(theta,last[[1L]],num.eq=FALSE))last<<-list(theta,pairlik_objective(theta,yd,Xd,rows,off,P1,P2))
    last[[2L]]
  }
  op<-stats::optim(rep(0,ncol(X)),function(b)nllgr(b,FALSE),gr=function(b)nllgr(b,TRUE),method="BFGS",control=optimizer_control)

  theta<-op$par
  newton_converged<-FALSE
  for(iter in seq_len(50L)) {
    g<-nllgr(theta,TRUE)
    if(max(abs(g))<=1e-7*(1+used_cross)) {newton_converged<-TRUE;break}
    Hn<-pairlik_hessian(theta,yd,Xd,rows,off,P1,P2,FALSE,0L)$H
    step<-tryCatch(drop(.pairlik_inv(Hn)%*%g),error=function(e)rep(NA_real_,length(theta)))
    if(any(!is.finite(step)))break
    old<-nllgr(theta,FALSE); accepted<-FALSE; fac<-1
    for(ls in seq_len(30L)) {
      cand<-theta-fac*step; val<-nllgr(cand,FALSE)
      if(is.finite(val)&&val<=old-1e-4*fac*sum(g*step)) {
        theta<-cand;accepted<-TRUE;break
      }
      fac<-fac/2
    }
    if(!accepted)break
  }
  names(theta)<-colnames(X)
  hs<-pairlik_hessian(unname(theta),yd,Xd,rows,off,P1,P2,TRUE,length(uid))
  H<-hs$H;Sp<-hs$Sp;score<-colSums(Sp);hsub<-hs$hsub;dimnames(hsub)<-list(as.character(uid),names(theta))
  pair_outer<-crossprod(Sp);dimnames(H)<-list(names(theta),names(theta))
  if(!newton_converged&&max(abs(score))>1e-5*(1+used_cross))stop("Conditional pairwise likelihood did not converge.",call.=FALSE)
  Hi<-.pairlik_inv(H);dimnames(Hi)<-dimnames(H)
  meat_hajek<-crossprod(hsub)
  meat_dcr<-meat_hajek-pair_outer
  Vtheta_dcr<-Hi%*%meat_dcr%*%t(Hi);dimnames(Vtheta_dcr)<-list(names(theta),names(theta))

  beta<-dispersion*theta;names(beta)<-names(theta)
  W<-working_scale$W;e<-working_scale$residuals
  nscale<-working_scale$n;pscale<-working_scale$p
  df_fraction<-(nscale-pscale)/nscale
  gamma<-working_scale$coefficients
  gamma_key<-paste0("pairlik_working_mean:",names(gamma))
  Gobs<-cbind(W*e,e^2-df_fraction*dispersion)
  colnames(Gobs)<-c(gamma_key,"pairlik_sigma2")

  Gsub<-rowsum(Gobs,working_scale$subject_index,reorder=FALSE)
  if(nrow(Gsub)!=length(uid))
    stop("Internal working-scale subject aggregation failure.",call.=FALSE)
  rownames(Gsub)<-as.character(uid)
  B1<-matrix(0,ncol(Gobs),ncol(Gobs),dimnames=list(colnames(Gobs),colnames(Gobs)))
  B1[seq_along(gamma),seq_along(gamma)]<-crossprod(W)
  B1[length(gamma)+1L,seq_along(gamma)]<-2*colSums(W*e)
  B1[length(gamma)+1L,length(gamma)+1L]<-working_scale$df
  B1i<-.pairlik_inv(B1);dimnames(B1i)<-dimnames(B1)
  meat_choice<-.pairlik_select_joint_meat(Gsub,hsub,pair_outer,tol=1e-12)
  Mfull<-meat_choice$selected
  q1<-ncol(Gobs);q2<-length(theta)
  Binv<-matrix(0,q1+q2,q1+q2)
  Binv[seq_len(q1),seq_len(q1)]<-B1i
  Binv[q1+seq_len(q2),q1+seq_len(q2)]<-Hi
  phi<-c(setNames(gamma,gamma_key),pairlik_sigma2=dispersion,
         setNames(theta,paste0("pairlik_theta:",names(theta))))
  dimnames(Binv)<-list(names(phi),names(phi));dimnames(Mfull)<-list(names(phi),names(phi))
  dimnames(meat_choice$raw)<-list(names(phi),names(phi))
  dimnames(meat_choice$hajek)<-list(names(phi),names(phi))
  Vphi<-Binv%*%Mfull%*%t(Binv);dimnames(Vphi)<-list(names(phi),names(phi))
  stage_bread<-matrix(0,q1+q2,q1+q2,dimnames=list(names(phi),names(phi)))
  stage_bread[seq_len(q1),seq_len(q1)]<-B1
  stage_bread[q1+seq_len(q2),q1+seq_len(q2)]<-H
  stage_meat<-Mfull
  working_mean_vcov<-Vphi[gamma_key,gamma_key,drop=FALSE]
  dispersion_se<-sqrt(max(Vphi["pairlik_sigma2","pairlik_sigma2"],0))

  beta_key<-paste0("beta:",names(beta));J<-rbind(diag(length(phi)),matrix(0,length(beta),length(phi)))
  rownames(J)<-c(names(phi),beta_key);colnames(J)<-names(phi)
  sidx<-match("pairlik_sigma2",names(phi));tidx<-match(paste0("pairlik_theta:",names(theta)),names(phi))
  J[length(phi)+seq_along(beta),sidx]<-theta
  J[length(phi)+seq_along(beta),tidx]<-dispersion*diag(length(theta))
  Vall<-J%*%Vphi%*%t(J);dimnames(Vall)<-list(rownames(J),rownames(J))
  all_parameters<-c(phi,setNames(beta,beta_key))
  Vb<-Vall[beta_key,beta_key,drop=FALSE];dimnames(Vb)<-list(names(beta),names(beta))
  theta_key<-paste0("pairlik_theta:",names(theta))
  Vtheta_stacked<-Vphi[theta_key,theta_key,drop=FALSE]
  meat_primary<-Mfull[q1+seq_len(q2),q1+seq_len(q2),drop=FALSE]
  dimnames(meat_primary)<-list(names(theta),names(theta))
  Vtheta<-Hi%*%meat_primary%*%t(Hi);dimnames(Vtheta)<-list(names(theta),names(theta))
  primary_label<-if(meat_choice$fallback) "subject/Hajek outer-product fallback" else "finite-graph DCR joint meat"
  vcov_label<-paste0("observed-row working-RSS plus ",primary_label," stacked sandwich")
  ans<-list(method_key="pairlik",method="Chen conditional PairLik objective + observed-row working-RSS extension",estimand="working Gaussian slope beta using an observed-row RSS scale extension",
            coefficients=beta,vcov=Vb,se=setNames(sqrt(pmax(diag(Vb),0)),names(beta)),vcov_type=vcov_label,
            nuisance=list(dispersion=dispersion,dispersion_source=dispersion_source,
                          dispersion_se=dispersion_se,pairlik_theta=theta,
                          working_mean_coefficients=gamma,
                          working_mean_se=setNames(sqrt(pmax(diag(working_mean_vcov),0)),names(gamma)),
                          working_scale_rss=working_scale$rss,
                          working_scale_df=working_scale$df,
                          working_scale_rank=working_scale$p,
                          working_scale_n_observations=working_scale$n,
                          working_scale_n_subjects=length(uid),
                          working_scale_dropped_columns=working_scale$dropped_columns),
            inference=list(full_parameters=all_parameters,full_covariance=Vall,vcov_all=Vall,
                           stage_parameters=phi,stage_covariance=Vphi,stage_bread=stage_bread,
                           stage_meat=stage_meat,transformation=J,
                           theta_covariance=Vtheta_stacked,theta_covariance_fixed_scale=Vtheta,
                           stage_meat_finite_graph_dcr_raw=meat_choice$raw,
                           stage_meat_subject_hajek=meat_choice$hajek,
                           theta_meat_finite_graph_dcr_raw=meat_dcr,
                           theta_covariance_fixed_scale_finite_graph_dcr_raw=Vtheta_dcr,
                           working_mean_covariance=working_mean_vcov,beta_covariance=Vb),converged=TRUE,
            diagnostics=list(ok=TRUE,process=process,n_subjects=length(uid),n_visit_events=sum(ft),n_marker_events=sum(marker,na.rm=TRUE),
                             n_outcomes=sum(eligible),n_subject_pairs=nrow(P),n_cross_pairs=used_cross,pair_fraction=nrow(P)/(length(uid)*(length(uid)-1)/2),
                             score_max=max(abs(score)),information_condition=kappa(H),
                             optimizer_code=op$convergence,newton_converged=newton_converged,
                             intercept_identified=FALSE,beta_scale_identified=FALSE,
                             dispersion_estimated=TRUE,dispersion_regular=TRUE,
                             dispersion_source=dispersion_source,dispersion_se=dispersion_se,
                             working_scale_rss=working_scale$rss,
                             working_scale_df=working_scale$df,
                             working_scale_rank=working_scale$p,
                             working_scale_n_observations=working_scale$n,
                             working_scale_n_subjects=length(uid),
                             working_scale_dropped_columns=working_scale$dropped_columns,
                             working_scale_meat_clustered_by_subject=TRUE,
                             working_scale_uses_same_eligible_rows=TRUE,
                             uses_auxiliary_outcome=FALSE,
                             primary_joint_meat=primary_label,
                             finite_graph_dcr_used_in_primary=!meat_choice$fallback,
                             finite_graph_dcr_joint_raw_min_eigen=meat_choice$raw_min_eigen,
                             finite_graph_dcr_joint_raw_relative_min_eigen=meat_choice$raw_relative_min_eigen,
                             finite_graph_dcr_roundoff_repair=meat_choice$roundoff_repair,
                             finite_graph_dcr_fallback=meat_choice$fallback,
                             finite_graph_dcr_fallback_type=if(meat_choice$fallback) "subject/Hajek outer product" else NA_character_,
                             finite_graph_dcr_relative_tolerance=meat_choice$tolerance),
            assumptions=c("The conditional objective is the Chen--Ning--Cai cross-swapped pairwise likelihood","The observed-row working-RSS scale is an extension, not Chen--Ning--Cai's universal-baseline scale design","Conditional pairwise likelihood cancels a common observation-selection factor","RSS/(Nobs-rank) from selected observed rows is a working scale and may inherit informative-selection or heterogeneous-variance bias",
                          "Primary covariance uses the finite-graph DCR joint meat when PSD, with a prespecified subject/Hajek fallback for a materially indefinite finite-sample realization","Subject pairs are sampled using only subject order and the seed; every cross observation pair of a sampled subject pair is used","The regression intercept is not identified by cross-swapping"),call=match.call())
  class(ans)<-c("pairlik");ans
}

Pairwise_likelihood <- function(data,...) fit_method(method='pairlik',data=data,...)
