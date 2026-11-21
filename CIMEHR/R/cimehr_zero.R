
evalq({
.lu_zero_rank <- function(M, tol=1e-7) {
    M<-as.matrix(M)
    if(!ncol(M)||!nrow(M))return(0L)
    s<-sqrt(colSums(M^2));keep<-is.finite(s)&s>0
    if(!any(keep))return(0L)
    qr(sweep(M[,keep,drop=FALSE],2L,s[keep],'/'),tol=tol,LAPACK=FALSE)$rank
}
.lu_zero_stop <- function(post, reason, stage='observation', detail=NULL) {
    if(is.null(detail))detail<-.lu_zero_stop_detail[reason]
    if(is.null(detail)||is.na(detail))detail<-NULL
    e<-structure(list(message=paste0('This zero-frailty design cannot be uniquely estimated by the current three-stage procedure: ',
      gsub('_',' ',reason),'. ',if(length(detail))paste0(detail,' '),'The N(0,1) visiting-only posterior and the observation-outcome compensation model are retained; no arbitrary latent decomposition is substituted.'),
      call=NULL,status='not_identified',stage=stage,sigma2_zeta=0,reason=reason,posterior=post),
      class=c('cimehr_zero_frailty_identification_error','error','condition'))
    stop(e)
}
.lu_zero_stop_detail <- character()
# Z_O labels: covariate=value; intercept-only designs use the column name.
.lu_zero_pattern_labels <- function(profiles) {
    profiles<-as.matrix(profiles);G<-nrow(profiles)
    nm<-colnames(profiles) %||% paste0('z',seq_len(ncol(profiles)))
    covs<-which(!nm%in%c('(Intercept)','Intercept'))
    labels<-if(length(covs)) vapply(seq_len(G),function(g)
        paste0(nm[covs],'=',as.character(profiles[g,covs]),collapse=','),character(1))
      else if(G==1L) nm[1L] else paste0('pattern',seq_len(G))
    make.unique(labels)
}
.lu_zero_observation <- function(XO,ZO,m,s,posterior,control=list()) {
    if(!isTRUE(posterior$sigma2_zeta==0)||!all(posterior$mu==0)||!all(posterior$s2==1))
      .lu_zero_stop(posterior,'incompatible_zero_frailty_nuisance_refit_required')
    tol<-control$identification_tol %||% 1e-7
    n<-nrow(ZO);q<-ncol(ZO)
    profiles<-unique(as.data.frame(ZO));profiles<-as.matrix(profiles)
    saturated<-nrow(profiles)<=q && .lu_zero_rank(profiles,tol)==nrow(profiles)
    spec<-list(schema=3L,posterior=posterior,original_ZO=ZO,tolerance=tol)
    if(saturated) {
      key<-function(M)do.call(paste,c(as.data.frame(M),sep='\r'))
      group<-match(key(ZO),key(profiles));G<-nrow(profiles)
      labels<-.lu_zero_pattern_labels(profiles)
      V<-matrix(0,n,G,dimnames=list(NULL,labels))
      V[cbind(seq_len(n),group)]<-1
      pcl<-.lu_fit_pairwise(XO,V,m,s,posterior,include_delta=FALSE,control=control)
      if(!pcl$converged)stop('Pairwise observation fit did not converge.',call.=FALSE)
      if(!pcl$identified).lu_zero_stop(posterior,'total_observation_variance_not_identified')
      attr(pcl$par,'zero_compensation')<-TRUE;pcl$zero_quotient<-TRUE
      spec$kind<-'quotient';spec$raw_chart<-FALSE;spec$family<-'groups'
      spec$profiles<-profiles;spec$group<-group;spec$labels<-labels
      spec$group_variance<-unname(pcl$sigma2_q);spec$variance_design<-V
      f<-.lu_functionals(pcl$par,XO,V,posterior,FALSE)
      return(list(pcl=pcl,ZO=V,include_delta=FALSE,functionals=f,zero_spec=spec))
    }
    pcl<-.lu_fit_pairwise(XO,ZO,m,s,posterior,include_delta=TRUE,control=control)
    if(!pcl$converged)stop('Pairwise observation fit did not converge.',call.=FALSE)
    delta<-unname(pcl$delta);v<-unname(pcl$sigma2_q)
    Omega<-tcrossprod(delta)+diag(v,q,q);dimnames(Omega)<-list(colnames(ZO),colnames(ZO))
    spec$Omega<-Omega;spec$variance_design<-ZO
    ij<-which(upper.tri(Omega,diag=TRUE),arr.ind=TRUE)
    informative<-is.finite(m)&m>0
    Zi<-ZO[informative,,drop=FALSE]
    Q<-Zi[,ij[,1L],drop=FALSE]*Zi[,ij[,2L],drop=FALSE]
    eps<-256*.Machine$double.eps*max(1,max(abs(Omega)))
    complete<-.lu_zero_rank(Q,tol)==nrow(ij)
    spec$covariance_identified<-complete
    edge<-which(upper.tri(Omega)&abs(Omega)>eps,arr.ind=TRUE)
    support<-sort(unique(as.vector(edge)))
    unique_decomposition<-FALSE
    if(complete && length(support)>=3L) {
      off<-Omega[support,support,drop=FALSE]
      unique_decomposition<-all(abs(off[upper.tri(off)])>eps)
    }
    if(complete && nrow(edge)==1L) {
      a<-edge[1L,1L];b<-edge[1L,2L]
      unique_decomposition<-abs(Omega[a,a]*Omega[b,b]-Omega[a,b]^2)<=eps*max(1,Omega[a,a],Omega[b,b])
    }
    if(unique_decomposition) {
      if(!pcl$identified).lu_zero_stop(posterior,'unique_compensation_but_sensitivity_singular')
      spec$kind<-'raw_unique';spec$family<-'unique_up_to_common_sign'
      spec$identification_scope<-paste(
        'unique fitted covariance decomposition up to common sign;',
        'individual loading inference is conditional on the returned sign orientation;',
        'true-stratum regularity is an assumption, not established by numerical rank')
      return(list(pcl=pcl,ZO=ZO,include_delta=TRUE,
        functionals=.lu_functionals(pcl$par,XO,ZO,posterior,TRUE),zero_spec=spec))
    }
    spec$kind<-'quotient';spec$raw_chart<-TRUE
    if(complete && !nrow(edge)) {
      spec$family<-'axes';spec$axes<-which(diag(Omega)>eps)
    } else if(complete && nrow(edge)==1L) {
      spec$family<-'edge';spec$axes<-as.integer(edge[1L,])
      spec$reference_direction<-delta
    } else {
      spec$family<-'all_loadings';spec$axes<-seq_len(q);spec$reference_direction<-delta
    }
    pa<-ncol(XO);oldfree<-pcl$free
    J<-cbind(2*drop(Zi%*%delta)*Zi,Zi^2)
    allowed<-oldfree[oldfree>pa]-pa
    keep<-integer();rank<-0L
    for(j in allowed) {
      newrank<-.lu_zero_rank(J[,c(keep,j),drop=FALSE],tol)
      if(newrank>rank){keep<-c(keep,j);rank<-newrank}
    }
    free<-c(seq_len(pa),pa+keep)
    tryCatch(.lu_inverse_information(pcl$hessian[free,free,drop=FALSE],
      'Identified zero-frailty observation sensitivity'),error=function(e)
        .lu_zero_stop(posterior,'total_observation_variance_sensitivity_singular'))
    pcl$free<-free;pcl$identified<-TRUE;pcl$zero_quotient<-TRUE
    attr(pcl$par,'zero_compensation')<-TRUE
    spec$chart_free<-free
    list(pcl=pcl,ZO=ZO,include_delta=TRUE,
      functionals=.lu_functionals(pcl$par,XO,ZO,posterior,TRUE),zero_spec=spec)
}
# Z_O row keys use 17 significant digits and normalize -0 to 0.
.lu_zero_key <- function(M) {
    M<-as.matrix(M)
    if(!ncol(M))return(rep('',nrow(M)))
    do.call(paste,c(lapply(seq_len(ncol(M)),function(j)sprintf('%.17g',as.numeric(M[,j])+0)),sep='\r'))
}
# Row-space basis of the training compensation union, scaled as in .lu_zero_rank.
.lu_zero_row_space <- function(U,tol) {
    U<-as.matrix(U);s<-sqrt(colSums(U^2));nz<-is.finite(s)&s>0
    r<-.lu_zero_rank(U,tol)
    Q<-if(r>0L)svd(sweep(U[,nz,drop=FALSE],2L,s[nz],'/'),nu=0L,nv=sum(nz))$v[,seq_len(r),drop=FALSE] else
      matrix(0,sum(nz),0L)
    list(scale=s,nonzero=nz,basis=Q,tolerance=tol)
}
.lu_zero_in_row_space <- function(space,u) {
    u<-as.matrix(u);nz<-space$nonzero
    if(!nrow(u))return(logical())
    if(!ncol(u))return(rep(TRUE,nrow(u)))
    size<-apply(abs(u),1L,max)
    off<-if(any(!nz))apply(abs(u[,!nz,drop=FALSE]),1L,max) else rep(0,nrow(u))
    v<-sweep(u[,nz,drop=FALSE],2L,space$scale[nz],'/')
    res<-v-(v%*%space$basis)%*%t(space$basis)
    off<=space$tolerance*size & sqrt(rowSums(res^2))<=space$tolerance*sqrt(rowSums(v^2))
}
.lu_zero_rows_text <- function(rows,labels=NULL) {
    k<-seq_len(min(5L,length(rows)))
    paste0(paste0(rows[k],if(length(labels))paste0(' (',labels[k],')') else '',collapse=', '),
      if(length(rows)>5L)paste0(' and ',length(rows)-5L,' more') else '')
}
# Stage-2 covariance: cross-product of centered inverse-Hessian subject scores.
.lu_zero_stage2_vcov <- function(pcl,posterior) {
    par<-pcl$par;free<-pcl$free
    Hinv<-.lu_inverse_information(as.matrix(pcl$hessian)[free,free,drop=FALSE],
      'Stage-2 negative-loglikelihood Hessian')
    score<-as.matrix(pcl$eval(par,mu=posterior$mu,s2=posterior$s2,scores=TRUE)$subject_score)
    if(nrow(score)!=length(posterior$mu)||ncol(score)!=length(par)||any(!is.finite(score)))
      stop('Stage-2 evaluator did not return finite subject scores.',call.=FALSE)
    influence<-score[,free,drop=FALSE]%*%t(Hinv)
    influence<-sweep(influence,2L,colMeans(influence),'-')
    face<-matrix(0,nrow(influence),length(par),dimnames=list(NULL,names(par)))
    face[,free]<-influence
    crossprod(face)
}
# Compensation requires extra_rank >= 1 and relevance > sqrt(log(n_subjects)).
.lu_zero_compensation_rule <- function(spec,XO,outcome_modifiers,pcl,posterior,observed,n_subjects) {
    zero_total<-if(identical(spec$family,'groups'))all(spec$group_variance==0) else
      identical(spec$family,'axes')&&!length(spec$axes)
    V<-tryCatch(.lu_zero_stage2_vcov(pcl,posterior),error=function(e)NULL)
    screen<-.cimehr_identification_diagnostics(XO,outcome_modifiers,pcl$alpha,V,observed,
      n_subjects,NULL,integer(),list())
    extra<-screen$exclusion$extra_rank;relevance<-screen$relevance
    relevant<-extra>=1L&&isTRUE(relevance$statistic>relevance$threshold)
    status<-if(zero_total)'omitted_zero_total_observation_variance' else
      if(relevant)'estimated' else 'omitted_no_relevant_exclusion'
    reason<-if(!identical(status,'omitted_no_relevant_exclusion'))NA_character_ else
      if(!extra)'no_excluded_direction' else
      if(is.finite(relevance$statistic))'weak_relevance' else 'relevance_not_assessed'
    message<-NULL
    if(!is.na(reason))
      message<-paste0('Zero visiting frailty: ',switch(reason,
        no_excluded_direction='the recording design has no covariate direction outside the outcome design',
        weak_relevance=sprintf(paste('the recording-covariate directions outside the outcome design have weak',
          'relevance (statistic %.3g <= sqrt(log(n_subjects)) = %.3g)'),relevance$statistic,relevance$threshold),
        relevance_not_assessed=paste('the relevance of the recording-covariate directions outside the outcome',
          'design could not be assessed')),
        '. The observation-outcome compensation is not estimated: a zero-compensation restriction (c = 0) is imposed. ',
        'This does not establish the absence of informative observation; estimates and standard errors are ',
        'interpreted under this restriction. A covariate that affects recording but not the outcome can ',
        'strengthen identification.')
    spec$compensation_rule<-list(status=status,reason=reason,extra_rank=extra,
      relevance_statistic=relevance$statistic,relevance_threshold=relevance$threshold,
      relevance_status=relevance$status,
      relevance_covariance=paste('Stage-2 sandwich of the observation fixed coefficients (inverse pairwise Hessian',
        'and centered subject scores); at zero frailty it equals their stacked covariance and is computed for every inference option'),
      rule=paste('at zero visiting frailty the compensation is estimated only when extra_rank >= 1 and the',
        'relevance statistic exceeds sqrt(log(n_subjects)); otherwise c = 0'),
      message=message)
    spec$compensation_omitted<-identical(status,'omitted_no_relevant_exclusion')
    spec
}
.lu_zero_compensation <- function(spec,W0,W1,ZO=NULL) {
    if(isTRUE(spec$compensation_omitted)) {
      empty<-matrix(0,nrow(as.matrix(W0)),0L)
      return(list(W0=empty,W1=empty,spec=spec,prefix='compensation.'))
    }
    if(is.null(spec)||identical(spec$kind,'raw_unique'))
      return(list(W0=W0,W1=W1,spec=spec,prefix='theta.'))
    W0<-as.matrix(W0);W1<-as.matrix(W1);n<-nrow(W0)
    if(is.null(ZO))ZO<-spec$original_ZO
    ZO<-as.matrix(ZO)
    if(nrow(ZO)!=n).lu_zero_stop(spec$posterior,'compensation_prediction_design_mismatch','outcome')
    if(!is.null(spec$basis_map))return(.lu_zero_saved_compensation(spec,W0,W1,ZO))
    tol<-spec$tolerance;W<-rbind(W0,W1)
    multiply<-function(d)W*rep(drop(ZO%*%d),2L)
    q<-ncol(ZO);dir_labels<-character()
    zo_names<-colnames(spec$original_ZO) %||% colnames(ZO) %||% paste0('z',seq_len(q))
    # Compensation directions: W times a Z_O pattern indicator or Z_O column.
    if(spec$family=='groups') {
      patterns<-as.matrix(spec$profiles);G<-nrow(patterns)
      pattern<-if(!is.null(spec[['group']])&&identical(ZO,as.matrix(spec$original_ZO)))spec[['group']] else
        match(.lu_zero_key(ZO),.lu_zero_key(patterns))
      if(length(pattern)!=n||anyNA(pattern))
        .lu_zero_stop(spec$posterior,'compensation_prediction_design_mismatch','outcome')
      positive<-which(spec$group_variance>0)
      pattern_multiplier<-outer(seq_len(G),positive,'==')*1
      dir_labels<-if(G==1L)rep('',length(positive)) else
        (spec$labels %||% .lu_zero_pattern_labels(patterns))[positive]
    } else {
      keys<-.lu_zero_key(ZO)
      first<-!duplicated(keys);patterns<-ZO[first,,drop=FALSE]
      pattern<-match(keys,keys[first])
      axes<-spec$axes
      pattern_multiplier<-patterns[,axes,drop=FALSE]
      dir_labels<-zo_names[axes]
    }
    dimnames(patterns)<-list(NULL,colnames(ZO))
    multiplier<-pattern_multiplier[pattern,,drop=FALSE]
    J<-ncol(multiplier);basis_dirs<-seq_len(J)
    blocks<-lapply(seq_len(J),function(j)W*rep(multiplier[,j],2L))
    U<-if(J)do.call(cbind,blocks) else matrix(0,2L*n,0L)
    if(J) {
      if(spec$family=='groups') {
        ref<-W*rep(rowSums(multiplier),2L)
        if(.lu_zero_rank(U,tol)>.lu_zero_rank(ref,tol))
          .lu_zero_stop(spec$posterior,'compensation_direction_depends_on_unidentified_decomposition','outcome')
      } else if(spec$family=='axes') {
        ranks<-vapply(blocks,.lu_zero_rank,integer(1),tol=tol)
        if(max(ranks)<.lu_zero_rank(U,tol))
          .lu_zero_stop(spec$posterior,'compensation_direction_depends_on_unidentified_decomposition','outcome')
        basis_dirs<-which.max(ranks)
      } else {
        ref<-multiply(spec$reference_direction)
        if(.lu_zero_rank(U,tol)>.lu_zero_rank(ref,tol))
          .lu_zero_stop(spec$posterior,'compensation_direction_depends_on_unidentified_decomposition','outcome')
      }
    }
    Ub<-if(length(basis_dirs))do.call(cbind,blocks[basis_dirs]) else matrix(0,2L*n,0L)
    keep<-integer();rank<-0L
    for(j in seq_len(ncol(Ub))) {
      nr<-.lu_zero_rank(Ub[,c(keep,j),drop=FALSE],tol)
      if(nr>rank){keep<-c(keep,j);rank<-nr}
    }
    # Column names: outcome random-link (Z_Y) column | Z_O pattern or axis.
    w_names<-colnames(W) %||% paste0('W',seq_len(ncol(W)))
    all_names<-if(length(basis_dirs))
      as.vector(outer(w_names,dir_labels[basis_dirs],function(a,b)ifelse(nzchar(b),paste0(a,'|',b),a))) else character()
    nm<-make.unique(as.character(all_names[keep]))
    spec$basis_map<-list(schema=3L,family=spec$family,patterns=patterns,
      pattern_labels=if(spec$family=='groups')(spec$labels %||% .lu_zero_pattern_labels(patterns)) else NULL,
      pattern_multiplier=pattern_multiplier,basis_directions=basis_dirs,columns=keep,names=nm,w_columns=w_names,
      row_space=.lu_zero_row_space(U,tol))
    A<-Ub[,keep,drop=FALSE];colnames(A)<-nm
    directions<-.lu_zero_prediction_directions(spec,q)
    L<-.lu_zero_loading_design(ZO,directions)
    if(spec$family=='groups' && any(spec$group_variance==0))
      L[spec$group_variance[pattern]==0,]<-0
    F<-.lu_zero_products(W0,W1,L)
    space<-.lu_zero_row_space(F,tol)
    mapping<-.lu_zero_basis_map(F,A,space,spec$posterior)
    spec$basis_map$prediction_directions<-directions
    spec$basis_map$prediction_space<-space
    spec$basis_map$prediction_map<-mapping
    list(W0=A[seq_len(n),,drop=FALSE],W1=A[n+seq_len(n),,drop=FALSE],spec=spec,prefix='compensation.')
}
.lu_zero_prediction_directions <- function(spec,q) {
    if(spec$family!='groups')return(diag(q)[,spec$axes,drop=FALSE])
    P<-spec$profiles[spec$group_variance==0,,drop=FALSE]
    if(!nrow(P))return(diag(q))
    r<-.lu_zero_rank(P,spec$tolerance)
    if(r==q)return(matrix(0,q,0L))
    sv<-svd(P,nu=0L,nv=q)
    sv$v[,seq.int(r+1L,q),drop=FALSE]
}
.lu_zero_loading_design <- function(Z,D) {
    L<-Z%*%D
    noise<-64*.Machine$double.eps*(abs(Z)%*%abs(D))
    L[abs(L)<=noise]<-0
    L
}
.lu_zero_products <- function(W0,W1,L) {
    W<-rbind(W0,W1)
    if(!ncol(L)||!ncol(W))return(matrix(0,nrow(W),0L))
    do.call(cbind,lapply(seq_len(ncol(L)),function(j)W*rep(L[,j],2L)))
}
.lu_zero_basis_map <- function(F,A,space,post) {
    nz<-space$nonzero
    mapping<-matrix(0,sum(nz),ncol(A))
    if(ncol(A)) {
      Z<-sweep(F[,nz,drop=FALSE],2L,space$scale[nz],'/')
      sv<-svd(Z,nu=ncol(space$basis),nv=ncol(space$basis))
      k<-seq_len(ncol(space$basis))
      mapping<-sv$v[,k,drop=FALSE]%*%
        (crossprod(sv$u[,k,drop=FALSE],A)/sv$d[k])
      if(max(abs(Z%*%mapping-A))>space$tolerance*max(1,max(abs(A))))
        .lu_zero_stop(post,'compensation_coordinate_map_not_identified','outcome')
    }
    mapping
}
.lu_zero_saved_compensation <- function(spec,W0,W1,ZO) {
    bm<-spec$basis_map;n<-nrow(W0)
    if(!identical(bm[['schema']],3L)||is.null(bm[['prediction_space']])||is.null(bm[['prediction_directions']]))
      .lu_zero_stop(spec$posterior,'incompatible_zero_frailty_nuisance_refit_required','outcome')
    if(ncol(ZO)!=ncol(bm$patterns)||(!is.null(colnames(ZO))&&!identical(colnames(ZO),colnames(bm$patterns))))
      .lu_zero_stop(spec$posterior,'compensation_prediction_design_mismatch','outcome')
    w_now<-colnames(W0) %||% paste0('W',seq_len(ncol(W0)))
    if(ncol(W1)!=ncol(W0)||!identical(w_now,bm$w_columns))
      stop('The newdata outcome random-link design (',paste(w_now,collapse=', '),
        ') differs from the fitted compensation basis (',paste(bm$w_columns,collapse=', '),').',call.=FALSE)
    pattern<-match(.lu_zero_key(ZO),.lu_zero_key(bm$patterns))
    L<-.lu_zero_loading_design(ZO,bm$prediction_directions)
    if(spec$family=='groups') {
      zero<-!is.na(pattern)&spec$group_variance[pattern]==0
      L[zero,]<-0
    }
    F<-.lu_zero_products(W0,W1,L)
    ok<-.lu_zero_in_row_space(bm$prediction_space,F)
    if(!all(ok)) {
      rows<-which(!ok[seq_len(n)]|!ok[n+seq_len(n)])
      .lu_zero_stop(spec$posterior,'compensation_not_identified_by_fitted_design','outcome',
        paste0('The original loading-by-outcome product rows are outside the estimable row space of the fitted design. ',
          'The saved three-stage fit cannot uniquely extend its compensation to newdata row(s) ',
          .lu_zero_rows_text(rows),'.'))
    }
    if(all(!is.na(pattern))) {
      M<-bm$pattern_multiplier[pattern,,drop=FALSE]
      build<-function(W) {
        if(!length(bm$basis_directions))return(matrix(0,n,0L,dimnames=list(NULL,bm$names)))
        ans<-do.call(cbind,lapply(bm$basis_directions,function(j)W*M[,j]))
        ans<-ans[,bm$columns,drop=FALSE];colnames(ans)<-bm$names;ans
      }
      A0<-build(W0);A1<-build(W1)
    } else {
      sp<-bm$prediction_space;nz<-sp$nonzero
      A<-sweep(F[,nz,drop=FALSE],2L,sp$scale[nz],'/')%*%bm$prediction_map
      colnames(A)<-bm$names
      A0<-A[seq_len(n),,drop=FALSE];A1<-A[n+seq_len(n),,drop=FALSE]
    }
    list(W0=A0,W1=A1,spec=spec,prefix='compensation.')
}

.lu_zero_frailty_stop <- function(posterior, reason, ...) {
    .lu_zero_stop(posterior,reason,if(grepl("compensation",reason))"outcome" else "observation")
}
.lu_zero_relabel_fit <- function(fit,spec) {
    fit$zero_frailty_compensation<-spec
    fit$diagnostics$compensation_identification<-if(spec$kind=='quotient')
      'original attainable compensation set represented in identified coordinates; combined centered outcome design full rank' else spec$identification_scope
    fit$diagnostics$identification_scope<-fit$diagnostics$compensation_identification
    if(!is.null(fit$inference)) {
      fit$inference$scope<-paste0(fit$inference$scope,
        '; all identifiable nuisance-stage uncertainty and cross-stage covariance retained; numerical identification checks do not establish true-stratum regularity or post-selection coverage')
      fit$diagnostics$inference_scope<-fit$inference$scope
    }
    if(spec$kind!='quotient')return(fit)
    fit$longitudinal_outcome$compensation<-fit$longitudinal_outcome$theta_Y
    fit$longitudinal_outcome$theta_Y<-setNames(numeric(),character())
    fit$longitudinal_outcome$compensation_multiplier<-fit$longitudinal_outcome$kappa
    fit$longitudinal_outcome$kappa<-NULL
    obs<-fit$observation_process
    obs$delta_latent_random_link<-obs$se_delta<-NULL
    if(isTRUE(spec$raw_chart)) {
      obs$fitted_total_variance<-rowSums((spec$original_ZO%*%spec$Omega)*spec$original_ZO)
      if(isTRUE(spec$covariance_identified))obs$total_covariance<-spec$Omega
      remove<-grepl('^(delta[.]|sigma2_q[.])',names(fit$coefficients))
      keep<-names(fit$coefficients)[!remove]
      fit$coefficients<-fit$coefficients[keep];fit$se<-fit$se[keep]
      if(!is.null(fit$vcov_all))fit$vcov_all<-fit$vcov_all[keep,keep,drop=FALSE]
      ai<-grep('^alpha[.]',rownames(obs$vcov_obs))
      obs$vcov_obs<-obs$vcov_obs[ai,ai,drop=FALSE]
    } else {
      obs$total_variance<-obs$Sigma_q;obs$se_total_variance<-obs$se_Sigma_q
    }
    obs$Sigma_q<-obs$Sigma_q_matrix<-obs$se_Sigma_q<-NULL
    obs$parameterization<-'identified total observation variance; individual shared loading and residual covariance are not separately estimated'
    rename<-function(x)if(isTRUE(spec$raw_chart))x else sub('^sigma2_q[.]','tau2.',x)
    rename_matrix<-function(V) {
      if(is.matrix(V)){if(!is.null(rownames(V)))rownames(V)<-rename(rownames(V));if(!is.null(colnames(V)))colnames(V)<-rename(colnames(V))};V
    }
    names(fit$coefficients)<-rename(names(fit$coefficients));names(fit$se)<-rename(names(fit$se))
    fit$vcov_all<-rename_matrix(fit$vcov_all);obs$vcov_obs<-rename_matrix(obs$vcov_obs)
    fit$observation_process<-obs
    fit$diagnostics$observation_covariance<-'identified fitted total observation variances'
    if(!is.null(fit$inference)) {
      inf<-fit$inference
      for(nm in c('vcov_joint','vcov_observation','influence_joint','influence_joint_active_face','influence_observation'))
        if(!is.null(inf[[nm]]))inf[[nm]]<-rename_matrix(inf[[nm]])
      inf$unavailable_wald_parameters<-rename(inf$unavailable_wald_parameters)
      inf$parameterization<-'identified total observation variance and compensation coordinates'
      if(isTRUE(spec$raw_chart)) {
        hidden<-grep('^(delta[.]|sigma2_q[.])',rownames(inf$vcov_joint),value=TRUE)
        inf$vcov_joint[hidden,]<-NA_real_;inf$vcov_joint[,hidden]<-NA_real_
        inf$unavailable_wald_parameters<-union(inf$unavailable_wald_parameters,hidden)
        if(!is.null(inf$influence_joint))inf$influence_joint[,hidden]<-NA_real_
      }
      fit$inference<-inf
    }
    fit
}

},envir=.cimehr_updated)
