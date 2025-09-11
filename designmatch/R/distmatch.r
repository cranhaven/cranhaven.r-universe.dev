#! distmatch
distmatch = function(t_ind, dist_mat = NULL, solver = NULL) {

  # Subset matching weight
  subset_weight = 0

  # Total number of matched pairs
  total_groups = sum(t_ind)

  # Match
  out = bmatch(t_ind = t_ind, dist_mat = dist_mat, subset_weight = subset_weight, total_groups = total_groups, solver = solver)

  #! Output
  return(list(obj_total = out$obj_total, obj_dist_mat = out$obj_dist_mat, 
              t_id = out$t_id, c_id = out$c_id, group_id = out$group_id, time = out$time, status = out$status))

}