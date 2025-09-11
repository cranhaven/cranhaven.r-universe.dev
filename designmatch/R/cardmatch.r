#! cardmatch
cardmatch = function(t_ind, mom = NULL, fine = NULL, solver = NULL) {

  if (is.null(mom)) {
    mom_covs = NULL
    mom_tols = NULL
    mom_targets = NULL
  } else {
    mom_covs = mom$covs
    mom_tols = mom$tols
    if (is.null(mom$targets)) {
    	mom_targets = NULL
    }
    if (!is.null(mom$targets)) {
    	mom_targets = mom$targets
    }
  }  
  if (is.null(fine)) {
    fine_covs = NULL
  } else {
    fine_covs = fine$covs
  }
    
  if (is.null(solver)) {
    t_max = 60 * 15
    approximate = 1
    solver = "highs"
  } else {
    t_max = solver$t_max
    approximate = solver$approximate
    trace = solver$trace
    round_cplex = solver$round_cplex
    solver = solver$name
  }
  
  #! CALL ERROR HANDLING

  #! Generate the parameters
  cat(format("  Building the matching problem..."), "\n")
  prmtrs = .problemparameters_cardmatch(t_ind, mom_covs, mom_tols, mom_targets, fine_covs)
  n_t = prmtrs$n_t
  n_c = prmtrs$n_c
  n_dec_vars = prmtrs$n_dec_vars
  cvec = prmtrs$cvec
  Amat = prmtrs$Amat
  bvec = prmtrs$bvec
  sense = prmtrs$sense
  vtype = prmtrs$vtype
  
  #! Find matches and calculate the elapsed time
  #! Gurobi
  if (solver == "gurobi") {
    #library(gurobi)
    if (requireNamespace('gurobi', quietly=TRUE)) {
      cat(format("  Gurobi optimizer is open..."), "\n")
      model = list()
      model$modelsense = 'max'
      model$obj = cvec
      model$A = Amat
      model$sense = rep(NA, length(sense))
      model$sense[sense=="E"] = '='
      model$sense[sense=="L"] = '<='
      model$sense[sense=="G"] = '>='
      model$rhs = bvec
      model$vtypes = vtype
      
      t_lim = list(TimeLimit = t_max, OutputFlag = trace)
      
      cat(format("  Finding the optimal matches..."), "\n")
      ptm = proc.time()
      out = gurobi::gurobi(model, t_lim)
      time = (proc.time()-ptm)[3]
      
      if (out$status == "INFEASIBLE") {
        cat(format("  Error: problem infeasible!"), "\n")
        obj_total = NA
        obj_dist_mat = NA
        t_id = NA
        c_id = NA
        group_id = NA
        time = NA
      }
      
      if (out$status ==  "OPTIMAL" || out$status == "TIME_LIMIT") {
        if (out$status == "OPTIMAL") {
          cat(format("  Optimal matches found"), "\n")
        }
        
        else {
          cat(format("  Time limit reached, best suboptimal solution given"), "\n")
        }
                
        #! Matched units indexes
        t_id = (1:n_dec_vars)[t_ind==1 & out$x==1]
        c_id = (1:n_dec_vars)[t_ind==0 & out$x==1]
        
        #! Group (or pair) identifier
        group_id = c(1:(length(t_id)), 1:(length(c_id)))
        
        #! Optimal value of the objective function
        obj_total = out$objval   
      }
    } else {
      stop('Required solver not installed')
    }

  }
  
  #! CPLEX
  else if (solver == "cplex") {
    #library(Rcplex)
    if (requireNamespace('Rcplex', quietly=TRUE)) {
      cat(format("  CPLEX optimizer is open..."), "\n")
      cat(format("  Finding the optimal matches..."), "\n")
      ptm = proc.time()
      out = Rcplex::Rcplex(objsense = 'max', cvec, Amat, bvec, sense = sense, vtype = vtype, n = 1, 
                           control = list(trace = trace, round = round_cplex, tilim = t_max))
      time = (proc.time()-ptm)[3]
      
      if (out$status==108) {
        cat(format("  Error: time limit exceeded, no integer solution!"), "\n")
        obj_total = NA
        obj_dist_mat = NA
        t_id = NA
        c_id = NA
        group_id = NA
        time = NA
      } else if (is.na(out$obj)) {
        cat(format("  Error: problem infeasible!"), "\n")
        obj_total = NA
        obj_dist_mat = NA
        t_id = NA
        c_id = NA
        group_id = NA
        time = NA
      }
      
      if (!is.na(out$obj)) {
        cat(format("  Optimal matches found"), "\n")

        #! Matched units indexes
        t_id = (1:n_dec_vars)[t_ind==1 & out$xopt==1]
        c_id = (1:n_dec_vars)[t_ind==0 & out$xopt==1]
        
        #! Group (or pair) identifier
        group_id = c(1:(length(t_id)), 1:(length(c_id)))
        
        #! Optimal value of the objective function
        obj_total = out$obj
        
      }
    } else {
      stop('Required solver not installed')
    }

  }
  
  #! GLPK
  else if (solver == "glpk") {
    #library(Rglpk)
    if (requireNamespace('Rglpk', quietly = TRUE)) {
      cat(format("  GLPK optimizer is open..."), "\n")
      dir = rep(NA, length(prmtrs$sense))
      dir[prmtrs$sense=="E"] = '=='
      dir[prmtrs$sense=="L"] = '<='
      dir[prmtrs$sense=="G"] = '>='
      
      cat(format("  Finding the optimal matches..."), "\n")
      ptm = proc.time()
      out= Rglpk::Rglpk_solve_LP(cvec, Amat, dir, bvec, types = vtype, max = TRUE)
      time = (proc.time()-ptm)[3]
      
      if (out$status!=0) {
        cat(format("  Error: problem infeasible!"), "\n")
        obj_total = NA
        obj_dist_mat = NA
        t_id = NA
        c_id = NA
        group_id = NA
        time = NA
      }
      
      if (out$status==0) {
        cat(format("  Optimal matches found"), "\n")
        
        #! Matched units indexes
        t_id = (1:n_dec_vars)[t_ind==1 & out$solution==1]
        c_id = (1:n_dec_vars)[t_ind==0 & out$solution==1]
        
        #! Group (or pair) identifier
        group_id = c(1:(length(t_id)), 1:(length(c_id)))
        
        #! Optimal value of the objective function
        obj_total = out$optimum
        
      }
    }
    else {
      stop('Required solver not installed')
    }
  }
  
  
  #! HiGHS
  else if (solver == "highs"){
    #library(highs)
    cat(format("  HiGHS optimizer is open..."), "\n")
    lhs = rep(-Inf, length(sense))
    rhs = rep(Inf, length(sense))
    lhs[sense == "G"] = bvec[sense == "G"]
    rhs[sense == "L"] = bvec[sense == "L"]
    lhs[sense == "E"] = bvec[sense == "E"]
    rhs[sense == "E"] = bvec[sense == "E"]
    
    types = vtype
    types[types=="B"] = "I"
    
    cat(format("  Finding the optimal matches..."), "\n")
    ptm = proc.time()
    out = highs_solve(L = cvec,
                      lower = 0,
                      upper = 1,
                      A = Amat,
                      lhs = lhs,
                      rhs = rhs,
                      types = types,
                      maximum = TRUE,
                      control = (highs_control(time_limit = t_max)))
    time = (proc.time()-ptm)[3]
    
    if (out$status == 7){
      cat(format("  Optimal matches found"), "\n")
      
      #! Matched units indexes
      t_id = (1:n_dec_vars)[t_ind==1 & round(out$primal_solution, 1e-10)==1]
      c_id = (1:n_dec_vars)[t_ind==0 & round(out$primal_solution, 1e-10)==1]
      
      #! Group (or pair) identifier
      group_id = c(1:(length(t_id)), 1:(length(c_id)))
      
      #! Optimal value of the objective function
      obj_total = out$objective_value   
    }
    else if (out$status == 8) {
      cat(format("  Error: problem infeasible!"), "\n")
      obj_total = NA
      id = NA
      time = NA
    }
    else if (out$status == 13) {
      cat(format("  Time limit reached!"), "\n")
      
      #! Matched units indexes
      t_id = (1:n_dec_vars)[t_ind==1 & round(out$primal_solution, 1e-10)==1]
      c_id = (1:n_dec_vars)[t_ind==0 & round(out$primal_solution, 1e-10)==1]
      
      #! Group (or pair) identifier
      group_id = c(1:(length(t_id)), 1:(length(c_id)))
      
      #! Optimal value of the objective function
      obj_total = out$objective_value   
    }
    else{
      outmessage = paste0("  Error: HiGHS solver message: ", out$status_message)
      cat(format(outmessage), "\n")
      obj_total = NA
      id = NA
      time = NA
    }
  }
  
  
  #! Symphony
  else {
    #library(Rsymphony)
    if (requireNamespace('Rsymphony', quietly=TRUE)) {
      cat(format("  Symphony optimizer is open..."), "\n")
      
      dir = rep(NA, length(prmtrs$sense))
      dir[prmtrs$sense=="E"] = '=='
      dir[prmtrs$sense=="L"] = '<='
      dir[prmtrs$sense=="G"] = '>='
      
      cat(format("  Finding the optimal matches..."), "\n")
      ptm = proc.time()
      out= Rsymphony::Rsymphony_solve_LP(cvec, Amat, dir, bvec, types = vtype, max = TRUE, time_limit = t_max)
      time = (proc.time()-ptm)[3]
      
      if (out$status==228) {
        cat(format("  Error: problem exceeded the time limit and no feasible solution is found!"), "\n")
        obj_total = NA
        obj_dist_mat = NA
        t_id = NA
        c_id = NA
        group_id = NA
        time = NA
      }
      else if (out$status!=0) {
        cat(format("  Error: problem infeasible!"), "\n")
        obj_total = NA
        obj_dist_mat = NA
        t_id = NA
        c_id = NA
        group_id = NA
        time = NA
      }
      
      if (out$status==0) {
        cat(format("  Optimal matches found"), "\n")

        #! Matched units indexes
        t_id = (1:n_dec_vars)[t_ind==1 & out$solution==1]
        c_id = (1:n_dec_vars)[t_ind==0 & out$solution==1]
        
        #! Group (or pair) identifier
        group_id = c(1:(length(t_id)), 1:(length(c_id)))
        
        #! Optimal value of the objective function
        obj_total = out$objval
        
      }
    } else {
      stop('Required solver not installed')
    }

  }
  #! Output
  return(list(obj_total = obj_total, t_id = t_id, c_id = c_id, group_id = group_id, time = time))
}