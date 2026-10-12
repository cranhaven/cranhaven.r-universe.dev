library(nnet)

# create predictor matrix from terms
create_pmatrix_from_terms = function(xx, terms)
{
  nt = length(terms);
  nr = nrow(xx);
  pmatrix = matrix(0, nr, 0);
  
  if (nt > 0)
  for(it in 1:nt)
  {
    term = terms[it];
    if (grepl("*",term,fixed=T))
    {
      splits = strsplit(term,"*",fixed=T)[[1]];
      id1 = as.numeric(splits[1]);
      id2 = as.numeric(splits[2]);
      pmatrix = cbind(pmatrix, term=xx[,id1]*xx[,id2]);
    }
    else
    {
      id  = as.numeric(term);
      pmatrix = cbind(pmatrix, term=xx[,id]);
    }
  }
  return(pmatrix);
}

calc_lda_BIC = function(xx, yy, cur_set, D, K, debug=F, gam=0)
{
  N = nrow(xx);
  D = ncol(xx);
  K = max(yy);
  d = length(cur_set);  
  
  ll = 0;
  if (d == 0)
  {
    p = numeric(K);
    for(i in 1:N)
    {
      p[yy[i]] = p[yy[i]] + 1.0/N;
    }
    for(k in 1:K)
    {
      ll = ll + sum(yy==k)*log(p[k]);
    }
    BIC = -2*ll + (K-1)*(log(N) + 2*gam*log(D));
    return(BIC);
  } 
  else 
  {
    lgt = multinom(yy ~ as.matrix(xx[,cur_set]), family = "binomial", trace=F);
    BIC = lgt$deviance + (K-1)*(1+d)*(log(N) + 2*gam*log(D));
    return(BIC);
  }
}


#
#      xx: explanatory variables
#      yy: response variable
# cur_set: current set of selected variables
#   debug: if shows debug information
#     gam: gamma in EBIC
#   terms: selected linear and interaction terms
#
calc_BIC = function(xx, yy, terms, D, K, debug=F, gam=0)
{
  N = length(yy);
  D = ncol(xx);
  K = max(yy);
  d = length(terms);  
  
  ll = 0;
  if (d == 0)
  {
    p = numeric(K);
    for(i in 1:N)
    {
      p[yy[i]] = p[yy[i]] + 1.0/N;
    }
    for(k in 1:K)
    {
      ll = ll + sum(yy==k)*log(p[k]);
    }
    BIC = (K-1) * (log(N) + 2*gam*log(D));
    BIC = BIC - 2*ll;
    return(BIC);
  } 
  else
  {
    pmatrix = create_pmatrix_from_terms(xx, terms);
    
    lgt = multinom(yy ~ pmatrix, family = "multinomial", trace=F);
    BIC = lgt$deviance;
    BIC = BIC + (K-1)*(1+ncol(pmatrix))*(log(N) + 2*gam*log(D)); # BIC with quadratic penalty

    return(BIC);
  }
}

get_term_name = function(x_names, cur_set, term)
{
  if (length(cur_set) == 0)
  {
    return("(Empty)");
  }
  str = "(Empty)";
  splits = strsplit(term, ".", fixed=T)[[1]]
  first = T;
  for(i in 1:length(cur_set))
  {
    split = splits[i];
    if (split=="1")
    {
      if (first)
      {
        first = F;
        str = x_names[cur_set[i]];
      }
      else
      {
        str = paste0(str, "*", x_names[cur_set[i]]);
      }
    }
    if (split=="2")
    {
      if (first)
      {
        first = F;
        str = paste0(x_names[cur_set[i]], "^2");
      }
      else
      {
        str = paste0(str, "*", paste0(x_names[cur_set[i]], "^2"));
      }
    }      
  }
  return (str);
}

get_term_name_2 = function(x_names, term)
{
  if (grepl("*",term,fixed=T))
  {
    splits = strsplit(term,"*",fixed=T)[[1]];
    id1 = as.numeric(splits[1]);
    id2 = as.numeric(splits[2]);
    return(paste(x_names[id1], x_names[id2], sep="*"))
  }
  else
  {
    id  = as.numeric(term);
    return(x_names[id])
  }
}

get_lin_terms = function(n_terms)
{
  terms = c();
  for(i in 1:n_terms)
  {
    arr = numeric(n_terms);
    arr[i] = 1;
    terms = c(terms, paste0(arr, collapse = "."));
  }
  return(terms);
}

get_lin_terms_vec = function(c_set)
{
  terms = as.character(c_set);
  return(terms);
}

get_quad_terms_vec = function(c_set)
{
  terms = c();
  if (length(c_set) <= 0)
    return(terms);
  
  terms = get_lin_terms_vec(c_set);  
  for(i in 1:length(c_set))
    for(j in i:length(c_set))
      terms = c(terms, paste(c_set[i],c_set[j],sep="*"));
  
  return(terms);
}

get_inter_terms_vec = function(c_set)
{
  terms = c();
  if (length(c_set) < 2)
    return(terms);
  
  for(i in 1:(length(c_set)-1))
  {
    for(j in (i+1):length(c_set))
      terms = c(terms, paste(c_set[i],c_set[j],sep="*"));
  }
  
  return(terms);
}

get_set_from_terms = function(terms)
{
  nt = length(terms);
  c_set = c();
  if (nt > 0)
  for(it in 1:nt)
  {
    term = terms[it];
    if (grepl("*",term,fixed=T))
    {
      splits = strsplit(term,"*",fixed=T)[[1]];
      id1 = as.numeric(splits[1]);
      id2 = as.numeric(splits[2]);
      c_set = union(c_set, id1);
      c_set = union(c_set, id2);
    }
    else
    {
      id  = as.numeric(term);
      c_set = union(c_set, id);
    }
  }
  return(c_set)
}

trim_terms = function(terms)
{
  if (length(terms) == 0)
    return(c());
  
  splits = strsplit(terms[1], ".", fixed=T)[[1]]
  Nvar = length(splits);
  var_set = rep(F, Nvar);
  
  if (Nvar == 0)
  {
    return(c());
  }
  
  for(term in terms)
  {
    splits = strsplit(term, ".", fixed=T)[[1]]
    Nvar = length(splits);
    for(i in 1:Nvar)
    {
      split = splits[i];
      if (split != "0")
        var_set[i] = T;
    }
  }
  
  new_terms = c();
  for(term in terms)
  {
    splits = strsplit(term, ".", fixed=T)[[1]]
    splits = splits[which(var_set)];
    new_term = paste0(splits, collapse = ".");
    new_terms = c(new_terms, new_term)
  }

  return(new_terms);
}

nqnorm = function(data)
{
  if (class(data)=="numeric")
  {
    N = length(data);
    qs = rank(data)/N - 0.5/N;
    data = qnorm(qs);
    return(data);
  }
  else
  {
    N = dim(data)[1];
    D = dim(data)[2];
    for(d in 1:D)
    {
      qs = rank(data[,d])/N - 0.5/N;
      data[,d] = qnorm(qs);
    }
    return(data);
  }
}

preprocess_y = function(y)
{
  if (min(y) <= 0)
  {
    cat("Warning: categorical response y should start from 1 instead of 0.\n")
    y = y - min(y) + 1
  }
  return(y)
}


#
#    xx: explanatory variables
#    yy: response variable
#  norm: if TRUE, xx are quantile normalized to normal
# debug: if shows debug information
#   gam: gamma in EBIC
#  minF: minimum number of forward steps
#
soda = function(xx, yy, norm=F, debug=F, gam=0, minF = 3)
{
  if (min(yy) == 0)
    yy = yy + 1;
  
  K = max(yy);
  N = dim(xx)[1];
  D = dim(xx)[2];
  
  minF = min(D, minF);
  
  if (norm)
  {
    for(k in 1:K)
      xx[yy=k,] = nqnorm(xx[yy=k,]);
  }
  
  x_names = colnames(xx);
  if (is.null(x_names))
    x_names = paste0("X",1:D);
  
  set_all = 1:D;
  cur_set = c();
  
  BIC  = c();
  Var  = list();
  Term = list();
  Type = c();
  
  BIC[1]    = calc_BIC(xx, yy, c(), D, K, debug, gam=gam);
  Type[1]   = "Init";
  Var[[1]]  = cur_set;
  Term[[1]] = c();
  
  cur_score = BIC[1];
  
  cat(paste0("Initialization: empty set, EBIC = ", sprintf("%.3f", BIC[1]), "\n\n"));
  
  tt = 1;
  ########################
  # Linear Forward Stage #
  ########################
  cat(paste0("Forward Stage - Main effects:\n"));
  while(T) 
  {
    ops = list();
    n_ops = 0;
    
    ######################
    # Forward Operations #
    ######################
    not_set = setdiff(set_all, cur_set);
    Nnset = length(not_set);
    if (Nnset > 0)
    {
      for(j in 1:Nnset)
      {
        jj = not_set[j];
        new_set   = sort(c(jj, cur_set));
        new_score = calc_lda_BIC(xx, yy, new_set, D, K, debug, gam=gam);
        if (debug)
          cat(paste0("  Trying to add variable ", jj , ": ", x_names[jj], " into main effect set...  D_Score: ", cur_score-new_score, "\n\n"));
        if (new_score < cur_score)
        {
          n_ops = n_ops + 1;
          ops[[n_ops]] = list();
          ops[[n_ops]]$new_set   = new_set;
          ops[[n_ops]]$new_score = new_score;
          ops[[n_ops]]$print     = paste0("  Main effects: add variable ", jj , ": ", x_names[jj], " into selection set...  df = ", length(new_set)+1, ",  EBIC = ", sprintf("%.3f",new_score));
        }
      }
    }
    
    #######################
    # The Best Operations #
    #######################
    if (n_ops == 0)
    {
      break;
    }
    
    toprint = "";
    for(i in 1:n_ops)
    {
      if (ops[[i]]$new_score < cur_score)
      {
        cur_score = ops[[i]]$new_score;
        cur_set   = ops[[i]]$new_set;
        toprint   = ops[[i]]$print;
      }
    }
    
    tt = tt + 1;
    BIC[tt]    = cur_score;
    Type[[tt]] = "Forward (Main)";
    Var[[tt]]  = cur_set;
    Term[[tt]] = get_lin_terms_vec(cur_set);
    
    cat(paste0(toprint,"\n"));
  }
  
  linear_set = cur_set;
  cur_terms  = get_lin_terms_vec(linear_set);
  cur_set    = c();
  #   cur_score  = BIC[1];
  
  ###########################
  # Quadratic Forward Stage #
  ###########################
  cat(paste0("\nForward Stage - Interactions: \n"));
  while(T) 
  {
    ops = list();
    n_ops = 0;
    
    ######################
    # Forward Operations #
    ######################
    not_set = setdiff(set_all, cur_set);
    Nnset = length(not_set);
    if (Nnset > 0)
    {
      for(j in 1:Nnset)
      {
        jj = not_set[j];
        new_set   = sort(c(jj, cur_set));
        new_terms = union(cur_terms, get_quad_terms_vec(new_set));
        
        new_score = calc_BIC(xx, yy, new_terms, D, K, debug, gam=gam);        
        if (debug)
          cat(paste0("  Trying to add variable ", jj , ": ", x_names[jj], " into interaction set...  D_Score: ", cur_score-new_score, "\n"));
        if (new_score < cur_score || length(cur_set) < minF)
        {
          n_ops = n_ops + 1;
          ops[[n_ops]] = list();
          ops[[n_ops]]$new_set   = new_set;
          ops[[n_ops]]$new_score = new_score;
          ops[[n_ops]]$new_terms = new_terms;
          ops[[n_ops]]$print     = paste0("  Interactions: add variable ", jj , ": ", x_names[jj], " into selection set...  df = ", length(new_terms)+1, ",  EBIC = ", sprintf("%.3f",new_score));
        }
      }
    }
    
    ######################
    # The Best Operation #
    ######################
    if (n_ops == 0)
    {
      break;
    }
    
    toprint = "";
    
    if (length(cur_set) < minF)
      cur_score = 1e6;
    
    for(i in 1:n_ops)
    {
      if (ops[[i]]$new_score < cur_score)
      {
        cur_score = ops[[i]]$new_score;
        cur_set   = ops[[i]]$new_set;
        toprint   = ops[[i]]$print;
        cur_terms = ops[[i]]$new_terms;
      }
    }
    
    tt = tt + 1;
    BIC[tt]    = cur_score;
    Type[[tt]] = "Forward (Int)";
    Var[[tt]]  = c(setdiff(linear_set, cur_set), cur_set);
    Term[[tt]] = cur_terms;
    
    cat(paste0(toprint,"\n"));
  }
  
  # set of variables at end of forward stage
  cur_set = c(setdiff(linear_set, cur_set), cur_set);
  int_terms = get_inter_terms_vec(cur_set);
  cur_terms = union(cur_terms, get_lin_terms_vec(linear_set))
  cur_score = calc_BIC(xx, yy, cur_terms, D, K, debug, gam=gam);
  #   #############################
  #   # Interaction Forward Stage #
  #   #############################
  #   while(T) 
  #   {
  #     ops = list();
  #     n_ops = 0;
  #     
  #     ######################
  #     # Forward Operations #
  #     ######################
  #     not_terms = setdiff(int_terms, cur_terms);
  #     Nnset = length(not_terms);
  #     if (Nnset > 0)
  #     {
  #       for(j in 1:Nnset)
  #       {
  #         term = not_terms[j];
  #         new_terms = union(cur_terms, term);        
  #         new_score = calc_BIC(xx, yy, new_terms, D, K, debug, gam=gam);        
  #         if (debug)
  #           cat(paste0("  Trying to add interaction ", term , " into interaction set...  D_Score: ", cur_score-new_score, "\n"));
  #         if (new_score < cur_score)
  #         {
  #           n_ops = n_ops + 1;
  #           ops[[n_ops]] = list();
  #           ops[[n_ops]]$new_set   = cur_set;
  #           ops[[n_ops]]$new_score = new_score;
  #           ops[[n_ops]]$new_terms = new_terms;
  #           ops[[n_ops]]$print     = paste0("  Interactions: add interaction ", term, " into selection set...  df = ", length(new_terms)+1, ",  EBIC = ", sprintf("%.3f",new_score));
  #         }
  #       }
  #     }
  #     
  #     ######################
  #     # The Best Operation #
  #     ######################
  #     if (n_ops == 0)
  #     {
  #       break;
  #     }
  #     
  #     toprint = "";
  #     
  #     if (length(cur_set) < minF)
  #       cur_score = 1e6;
  #     
  #     for(i in 1:n_ops)
  #     {
  #       if (ops[[i]]$new_score < cur_score)
  #       {
  #         cur_score = ops[[i]]$new_score;
  #         cur_set   = ops[[i]]$new_set;
  #         toprint   = ops[[i]]$print;
  #         cur_terms = ops[[i]]$new_terms;
  #       }
  #     }
  #     
  #     tt = tt + 1;
  #     BIC[tt]    = cur_score;
  #     Var[[tt]]  = c(setdiff(linear_set, cur_set), cur_set);
  #     Term[[tt]] = cur_terms;
  #     
  #     cat(paste0(toprint,"\n"));
  #   }
  
  cat(paste0("\nBackward stage: \n"));
  ##################
  # Backward Stage #
  ##################
  if (length(cur_set) > 0)
  {
    while(T) 
    {
      ops = list();
      n_ops = 0;
      
      #######################
      # Backward Operations #
      #######################
      Nterms = length(cur_terms);
      if(Nterms > 0)
      {
        for(j in 1:Nterms)
        {
          term = cur_terms[j];  
          new_terms = setdiff(cur_terms, term);
          #          print("new_terms:")
          #          print(new_terms)
          new_score = calc_BIC(xx, yy, new_terms, D, K, debug, gam=gam);
          if (debug)
          {
            term_name = get_term_name_2(x_names, term);
            cat(paste0("  Trying to remove term ", term_name, " from selection set...  Score: ", cur_score - new_score, "\n\n"));
          }
          #           cat(paste0("new_score = ", new_score, "\n"))
          #           cat(paste0("cur_score = ", cur_score, "\n"))
          if (new_score < cur_score)
          {
            n_ops = n_ops + 1;
            term_name = get_term_name_2(x_names, term);
            ops[[n_ops]] = list();
            #             ops[[n_ops]]$new_set   = cur_set;
            ops[[n_ops]]$new_terms = new_terms;
            ops[[n_ops]]$new_score = new_score;
            ops[[n_ops]]$print     = paste0("  Remove term ", term_name, " from selection set...  df = ", length(new_terms)+1, ",  EBIC = ", sprintf("%.3f", new_score));
          }
        }
      }
      
      #######################
      # The Best Operations #
      #######################
      if (n_ops == 0)
      {
        break;
      }
      
      toprint = "";
      for(i in 1:n_ops)
      {
        if (ops[[i]]$new_score < cur_score)
        {
          cur_score = ops[[i]]$new_score;
          cur_terms = ops[[i]]$new_terms;
          cur_set   = get_set_from_terms(ops[[i]]$new_terms);
          toprint   = ops[[i]]$print;
        }
      }
      
      tt = tt + 1;
      BIC[tt]    = cur_score;
      Type[[tt]] = "Backward";
      Var[[tt]]  = cur_set;
      Term[[tt]] = cur_terms;
      
      cat(paste0(toprint,"\n"));
    }
  }
  
  result = list();
  result$BIC  = BIC;
  result$Type = Type;
  result$Var  = Var;
  result$Term = Term;
  MIN_IDX = -1;
  MIN_BIC = 100000000;
  if (tt > 0)
  {
    for(i in 1:tt)
    {
      #       if (BIC[i] < MIN_BIC)
      {
        MIN_IDX = i;
        MIN_BIC = BIC[i];
      }
    }
    result$final_EBIC = BIC[MIN_IDX];
    result$final_Var  = Var[[MIN_IDX]];
    result$final_Term = Term[[MIN_IDX]];
    
    #     for(ii in 1:tt)
    #     {
    #       cat(paste("Iteration #", ii, ", Variables: (", paste(Var[[ii]], collapse=", "),"), Terms: (", paste0(Term[[ii]], collapse=", "), ")\n", sep=""));
    #     }
  } 
  else 
  {
    result$final_EBIC  = BIC[1];
    result$final_Var  = Var[[1]];
    result$final_Term = Term[[1]];
  }
  cat(paste("\nFinal selected variables: ", paste0(x_names[result$final_Var], collapse=", ")));
  term_names = c();
  for(term in result$final_Term)
  {
    term_name = get_term_name_2(x_names, term);
    term_names = c(term_names, term_name);
  }
  cat(paste("\n                   terms: ", paste0(term_names, collapse=", "), "\n"));
  result$best_Var = result$final_Var
  result$best_Term = result$final_Term
  return(result)
}

logistic_terms_CV = function(xx, yy, terms, KK, Debug=F)
{
  N = length(yy);
  K = max(yy);
  D = dim(xx)[2];
  o = sample(1:N);
  
  n = floor(N/KK);
  
  if (is.null(terms))
  {
    xx = matrix(0, N, 0);
  }
  else
  {
    xx = create_pmatrix_from_terms(as.matrix(xx), terms);
  }
  
  xx = as.matrix(xx[o,]);
  yy = yy[o];
  
  m_succ = 0;
  c_succ = 0;
  
  for(kk in 1:KK)
  {
    if (Debug)
      cat(paste0("Cross validation k = ", kk, " / ", KK, "\n"));
    
    set_tr = setdiff(1:N,((kk-1)*n+1):((kk)*n));
    set_te = ((kk-1)*n+1):((kk)*n);
    xx_tr = as.matrix(xx[set_tr,]);
    yy_tr = yy[set_tr];
    xx_te = as.matrix(xx[set_te,]);
    yy_te = yy[set_te];
    
    pmatrix = rep(1,length(yy_tr));
    if (length(xx_tr) > 0)
      pmatrix = xx_tr;
    
    fit = multinom(yy_tr ~ pmatrix, family = "multinomial", trace=F);
    cef = coef(fit);
    if (K==2)
      cef = t(as.matrix(coef(fit)));
    zz  = matrix(0, K, length(yy_te))
    for(k in 2:K)
    {
      pmatrix = rep(1,length(yy_te));
      if (length(xx_te) > 0)
        pmatrix = xx_te;
      zz[k,] = cbind(1,pmatrix) %*% cef[k-1,];
    }
    pp  = apply(zz,2,which.max);
    
    m_succ = m_succ + sum(pp == yy_te);
    
    if (Debug)
      cat(paste0("  Successes in k = ", kk, ": ", m_succ, " / ", n*kk, "\n"));
  }
  
  res = list();
  res$m_sp = m_succ/N;
  
  if (Debug)
    cat(paste0("Classification accuracy = ", res$m_sp, "\n"));
  
  return(res);
}

#
# Calculate a trace of cross-validation error rate for SODA forward-backward procedure
#
soda_trace_CV = function(xx, yy, res_SODA)
{
  if (min(yy) == 0)
    yy = yy + 1;
  N_CV = 20;  
  Np = length(res_SODA$Var);
  errors_ss = matrix(0, Np, N_CV);
  ss_V    = numeric(Np);
  ss_MT   = numeric(Np);
  ss_IT   = numeric(Np);
  ss_BIC = numeric(Np);
  ss_Typ  = character(Np);
  for(i in 1:Np)
  {
    cat(paste0("Calculating CV error for step ", i, " ...\n"));
    SS = res_SODA$Var[[i]];
    TT = res_SODA$Term[[i]];
    for(icv in 1:N_CV)
    {
      errors_ss[i,icv] = 1 - logistic_terms_CV(xx, yy, TT, 10)$m_sp;
    }
    ss_V[i]    = length(SS);
    ss_MT[i]   = length(TT) - sum(grepl("*",TT,fixed=T));
    ss_IT[i]   = sum(grepl("*",TT,fixed=T));
    ss_BIC[i] = res_SODA$BIC[i]
    ss_Typ[i]  = res_SODA$Type[i]
  }
  ss_mean = apply(errors_ss, 1, mean);
  tab = data.frame(ss_Typ, ss_BIC, ss_V, ss_MT, ss_IT, ss_mean);
  colnames(tab) = c("Step Type", "BIC", "# Variables", "# Main terms", "# Interactions", "CV Error");
  return(tab);
}

s_soda = function(x, y, H=5, gam=0, minF=3, norm=F, debug=F)
{
  cat(paste0("Variable selection using S-SODA....."));
  
  if (norm)
  {
    y = nqnorm(y);
    x = nqnorm(x);
  }
  
  N = length(y);
  
  oo = order(y);
  xx = x[oo,];
  yy = y[oo];
  
  ls = round(seq(1, N, length.out=H+1));
  LL = length(ls);
  
  res = list();
  
  res$S = numeric(N);
  res$H = H
  
  res$int_l = numeric(LL-1);
  res$int_m = numeric(LL-1);
  res$int_u = numeric(LL-1);
  
  res$S[oo[1]] = 1;
  for (i in 1:H)
  {
    ff = ls[i]+1;
    to = ls[i+1];
    
    res$S[oo[ff:to]] = i;
    
    res$int_l[i] = yy[ff];
    res$int_m[i] = mean(yy[ff:to]);
    res$int_u[i] = yy[to];
  }
  
  res_SODA = soda(x, res$S, gam=gam, minF=minF);
  
  res$BIC       = res_SODA$BIC;
  res$Var       = res_SODA$Var;
  res$Term      = res_SODA$Term;
  res$best_BIC  = res_SODA$best_BIC;
  res$best_Var  = res_SODA$best_Var;
  res$best_Term = res_SODA$best_Term; 
  
  pmt = create_pmatrix_from_terms(as.matrix(xx), res$best_Term);
  if (length(pmt) <= 0)
    pmt = matrix(1, length(res$S), 1);
  lgt = multinom(res$S ~ pmt, family = "multinomial", trace=F);
  
  res$logit_m   = lgt;
  
  print(paste("Selected variables: ", paste(names(x)[res$best_Var], collapse=" ")));
  
  return(res)
}

s_soda_model = function(x, y, H=10)
{
  cov_MIN = 0.1;
  
  N = length(y);
  
  yy = y;
  o = order(y);
  y = sort(y);
  xx = sort(y);
  
  result = list();
  result$sort_y = y;
  
  ls = round(seq(1, N, length.out=H+1));
  LL = length(ls);
  
  result$cuts_o = ls;
  result$cuts_v = y[ls];
  
  result$H = H;
  
  result$int_d = numeric(N);
  result$int_h = numeric(N);
  
  result$int_y = list();
  result$int_x = list();
  result$int_p = numeric(H);
  result$int_l = numeric(H);
  result$int_m = list();
  result$int_v = list();
  
  result$int_ul = numeric(LL-1);
  result$int_ur = numeric(LL-1);
  result$int_um = numeric(LL-1);
  
  result$H = LL-1;
  
  result$int_d[1] = 1;
  for (i in 1:(LL-1))
  {
    from = ls[i]+1;
    to   = ls[i+1];
    
    result$int_d[from:to] = i;
    result$int_h[yy>=y[from] & yy<=y[to]] = i;
    
    result$int_p[i]   = (to-from+1)/N;
    result$int_l[i]   = y[to]-y[from];
    
    result$int_ul[i]  = y[from];
    result$int_ur[i]  = y[to];
    result$int_um[i]  = mean(y[from:to]);
  }
  
  x = as.matrix(x);
  x = as.matrix(x[o,]);
  D = dim(x)[2];
  
  result$sort_x = x;
  
  result$int_m0 = list();
  result$int_m1 = list();
  result$int_m2 = list();
  
  for (i in 1:(LL-1))
  {
    from = ls[i];
    to   = ls[i+1];
    
    result$int_d[from:to] = i;
    result$int_h[yy>=y[from] & yy<=y[to]] = i;
    
    result$int_p[i]   = (to-from+1)/N;
    result$int_l[i]   = y[to]-y[from];
    
    result$int_m0[[i]] = coef(lm(y[from:to] ~ 1));
    result$int_m1[[i]] = coef(lm(y[from:to] ~ x[from:to,]));
    result$int_m2[[i]] = coef(lm(y[from:to] ~ poly(as.matrix(x[from:to,]), degree=2, raw=TRUE)));
    
    sx = as.matrix(x[from:to,]);
    
    result$int_m[[i]] = colMeans(sx);
    ss = cov(sx);
    diag(ss) = pmax(diag(ss), cov_MIN);
    result$int_v[[i]] = cov(sx);
  }
  
  return(result);
}

surf.colors = function(x, col = terrain.colors(20)) {
  # First we drop the 'borders' and average the facet corners
  # we need (nx - 1)(ny - 1) facet colours!
  x.avg = (x[-1, -1] + x[-1, -(ncol(x) - 1)] +
           x[-(nrow(x) -1), -1] + x[-(nrow(x) -1), -(ncol(x) - 1)]) / 4
  # Now we construct the actual colours matrix
  colors = col[cut(x.avg, breaks = length(col), include.lowest = T)]
  return(colors)
}

s_soda_pred = function(x, model, po = 1)
{
  x = as.matrix(x);
  N = dim(x)[1];
  D = dim(x)[2];
  H = model$H;
  
  y = numeric(N);
  
  x2 = poly(x, degree=2, raw=TRUE)
  
  cat("Making predictions using S-SODA model...\n")
  for(n in 1:N)
  {
    if (n %% round(N/10) == 0)
      cat(paste0(round(n/N*10), "0%  "));
    pp = log(model$int_p);
    for(h in 1:H)
    {
      pp[h] = pp[h] + dmvnorm(x[n,], model$int_m[[h]], model$int_v[[h]], log=T);
    }
    pp = pp-max(pp);
    pp = exp(pp);
    pp = pp / sum(pp);
    
    y[n] = 0;
    for(h in 1:H)
    {
      if (po == 0)
        y[n] = y[n] + pp[h]*(model$int_m0[[h]]);
      if (po == 1)
        y[n] = y[n] + pp[h]*sum(model$int_m1[[h]] * cbind(1, t(as.numeric(x[n,]))));
      if (po == 2)
        y[n] = y[n] + pp[h]*sum(model$int_m2[[h]] * cbind(1, t(as.numeric(x2[n,]))));
    }
  }
  cat("\n")
  return(y);
}

s_soda_pred_grid = function(xx1, xx2, model, po=1)
{
  xx = expand.grid(xx1,xx2);
  pp = s_soda_pred(xx, model, po=po);
  return(matrix(pp, length(xx1), length(xx2)));
}

compare_surface = function(xx, yy, col_idx, theta=-25, zlab="Y", add_points=F, H=25)
{
  ii = col_idx[1]
  jj = col_idx[2]
  x1 = xx[, ii]
  x2 = xx[, jj]
  name_i = colnames(xx)[ii]
  name_j = colnames(xx)[jj]
  x1_r = range(xx[, ii], na.rm=T)
  x2_r = range(xx[, jj], na.rm=T)
  m_soda = s_soda_model(xx[, col_idx], yy, H=H)
  m_line = lm(yy ~ xx[, col_idx])
  m_coef = m_line$coefficients

  MM = 30;
  g_x1 = seq(x1_r[1], x1_r[2], length.out=MM);
  g_x2 = seq(x2_r[1], x2_r[2], length.out=MM);
  gexx = expand.grid(x1=g_x1, x2=g_x2)
  p_soda = s_soda_pred_grid(g_x1, g_x2, m_soda, po=1)
  p_line = m_coef[1] + m_coef[2] * gexx[, 1] + m_coef[3] * gexx[, 2]
  p_line = matrix(p_line, MM, MM)

  p_combined = c(p_soda, p_line)
  min_p = min(p_combined)
  max_p = max(p_combined)

  par(mfrow=c(1,2))
  main_1 = "Linear Regression"
  main_2 = "S-SODA"
  persp(g_x1, g_x2, p_line, theta=theta, col=surf.colors(p_line), xlab=name_i, ylab=name_j, zlab=zlab, zlim=c(min_p, max_p))
  title(main_1, line=0)
  persp(g_x1, g_x2, p_soda, theta=theta, col=surf.colors(p_soda), xlab=name_i, ylab=name_j, zlab=zlab, zlim=c(min_p, max_p))
  title(main_2, line=0)
}