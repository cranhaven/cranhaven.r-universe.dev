#' local_outlier_factor
#' \code{local_outlier_factor}  is function for calculating the lof factor for a data set using knn
#' This function is not intended to be used by end user.
#'
#' @param dat A data.frame contained only predict variables.
#' @param k Number of neighbors for LOF.Default is 10.
#' @export


local_outlier_factor = function(dat, k = 10) {
    dat = as.matrix(dat)
    row_num = dim(dat)[1L]
    matrix_nrow = k * 2 + 2
    dist_dat = matrix(0, nrow = matrix_nrow, ncol = row_num)
    for (i in 1:row_num) {
        #get k distance neighbors
        data_temp = as.matrix(dat)
        dimnames(data_temp) = NULL
        difference = scale(data_temp, dat[i,], FALSE)
        dist_temp = drop(difference ^ 2 %*% rep(1, ncol(dat)))
        dist_temp = sqrt(dist_temp)
        distance_order = order(dist_temp)
        nn_distance = dist_temp[distance_order]
        knn_distance = nn_distance[k + 1]
        neibs = drop(nn_distance[nn_distance <= knn_distance])
        neibs = neibs[-1]
        neibr_num = length(neibs)
        neighbor_index = distance_order[1:neibr_num + 1]
        neihbor_num1 = neibr_num + 3
        neihbor_num2 = neibr_num + neibr_num + 2
        neighdist = c(neihbor_num1, neihbor_num2, neighbor_index, neibs)
        neighdist[is.na(neighdist)] = 0
        x = length(neighdist)
        if (x > matrix_nrow) {
            dist_dat = rbind(dist_dat, matrix(rep(0, (x - matrix_nrow) * row_num), ncol = row_num))
            matrix_nrow = x
        }
        dist_dat[1:x, i] = neighdist
        dist_dat[1:matrix_nrow, i]
    }

    #calculating local reachability density
    p = dim(dist_dat)[2]
    lrd_dat = rep(0, p)
    lrd_dat = sapply(1:p, function(i) {
        j = seq(3, 3 + (dist_dat[2, i] - dist_dat[1, i]))
        neibr_num = dist_dat[2, i] - dist_dat[1, i] + 1
        dt_temp = rbind(diag(dist_dat[dist_dat[2, dist_dat[j, i]], dist_dat[j, i]]),
                        dist_dat[j + neibr_num, i])
        #calculate reachability
        reach = 1 / (sum(apply(dt_temp, 2, function(x) max(x))) / neibr_num)
        reach
    })
    lof = rep(0, p)
    # computer the local outlier factor of each observation in dat
    lof = sapply(1:p, function(i) {
        n_neighbor = dist_dat[2, i] - dist_dat[1, i] + 1
        j = seq(0, (n_neighbor - 1))
        local_factor = sum(lrd_dat[dist_dat[3 + j, i]] / lrd_dat[i]) / n_neighbor
        lof[i] = local_factor
    }
    )
    lof
}

#' Fuzzy Cluster means.
#'
#' This function is used for Fuzzy Clustering.
#'
#' @param dat  A data.frame contained only predict variables.
#' @param kc The number of cluster center (default is 2),
#' @param sf Default is 2.
#' @param nstart The number of random groups (default is 1),
#' @param max_iter Max iteration number(default is 100) .
#' @param epsm Default is 1e-06.
#' @references
#' Bezdek, James C. "FCM: The fuzzy c-means clustering algorithm".
#' Computers & Geosciences (0098-3004),\doi{10.1016/0098-3004(84)90020-7}
#' @export
fuzzy_cluster_means = function(dat, kc = 2, sf = 2, nstart = 1, max_iter = 100, epsm = 1e-06) {
    dat = as.matrix(dat)
    set.seed(46)
    init_centers_id = sample(1:nrow(dat), kc)
    init_centers = as.matrix(dat[init_centers_id,])
    re_best = fuzzy_cluster(dat = dat, kc = kc, init_centers = init_centers, sf = sf, max_iter = max_iter, epsm = epsm)
    if (nstart > 1) {
        i = 2
        while (i <= nstart) {
            centers_id = sample(1:nrow(dat), kc)
            centers = as.matrix(dat[centers_id,])
            rand_best = fuzzy_cluster(dat, kc = kc, init_centers = centers, sf = sf, max_iter = max_iter, epsm = epsm)
            if (rand_best$obj_fun <= re_best$obj_fun) {
                re_best = rand_best
            }
            i = i + 1
        }
    }
    return(re_best)
}


#' @param init_centers Initial centers of obs.
#' @rdname fuzzy_cluster_means
#' @export

fuzzy_cluster = function(dat, kc = 2, init_centers, sf = 3, max_iter = 100, epsm = 1e-06) {

    dist = sapply(1:kc, function(j) rowSums(scale(dat, init_centers[j,], FALSE) ^ 2))
    history_obj = c()
    obj_fun_old = Inf
    stop_con = TRUE
    iter = 1
    while (stop_con) {
        s = (1 / (dist + epsm)) ^ (1 / (sf - 1))
        md = s / (s %*% matrix(1, kc, kc))
        t1 = t(md ^ sf) %*% dat
        t2 = t(md ^ sf) %*% matrix(1, nrow(dat), ncol(dat))
        centers = t1 / t2
        dist = sapply(1:kc, function(j) rowSums(scale(dat, centers[j,], FALSE) ^ 2))
        obj_fun = sum(md ^ sf * dist)
        stop_con = abs(obj_fun - obj_fun_old) > 0.0001 && (iter < max_iter)
        obj_fun_old = obj_fun
        history_obj = c(history_obj, obj_fun)
        iter = iter + 1
    }
    cluster = apply(md, 1, which.max)
    fcm_res = list(md, obj_fun, history_obj, init_centers, cluster)
    names(fcm_res) = c("membership_degree", "obj_fun", "history_obj", "init_centers", "cluster")
    return(fcm_res)
}


#' euclid_dist
#'
#' This function is not intended to be used by end user.
#'
#' @param  x  A list
#' @param  y  A list
#' @param cos_margin  rows or cols
#' @export
euclid_dist = function(x, y, cos_margin = 1) {
    x = as.matrix(x)
    y = as.matrix(y)
    if (cos_margin == 1) {
        nr = nrow(y)
        dist = sapply(1:nr, function(j) rowSums(scale(x, y[j,], FALSE) ^ 2))
    } else {
        nc = ncol(y)
        dist = sapply(1:nc, function(j) colSums(scale(x, y[, j], FALSE) ^ 2))
    }
    return(dist)
}

#' cos_sim
#'
#' This function is not intended to be used by end user.
#'
#' @param  x  A list of numbers
#' @param  y  A list of numbers
#' @param cos_margin Margin of matrix, 1 for rows and 2 for cols, Default is 1.
#' @return A number of cosin similarity
#' @export
cos_sim = function(x, y, cos_margin = 1) {
  opt = options(digits = 6)
  x = as.numeric(x)
  y = as.numeric(y)
  max_x = max(unlist(x), na.rm = TRUE)
  min_x = min(unlist(x), na.rm = TRUE)
  max_y = max(unlist(y), na.rm = TRUE)
  min_y = min(unlist(y), na.rm = TRUE)
  x = ifelse(x - min_x > 0, x - min_x, 0.00001) / ifelse((max_x - min_x) > 0, max_x - min_x, 1)
  y = ifelse(y - min_y > 0, y - min_y, 0.00001) / ifelse((max_y - min_y) > 0, max_y - min_y, 1)
  x = as.matrix(x)
  y = as.matrix(y)
  dist = NULL
  if (cos_margin == 1) {
    nr = nrow(y)
    dist = sapply(1:nr, function(j) colSums(t(x) * y[j,], na.rm = TRUE) /
                    sqrt(rowSums(x ^ 2, na.rm = TRUE) * sum(y[j,] ^ 2, na.rm = TRUE)))
    colnames(dist) = rownames(y)
  } else {
    nc = ncol(y)
    j =1
    dist = sapply(1:nc, function(j) colSums(x * y[, j], na.rm = TRUE) /
                    sqrt(colSums(x ^ 2, na.rm = TRUE) * sum(y[, j] ^ 2, na.rm = TRUE)))
    colnames(dist) = colnames(y)
  }
  return(round(dist,4))
  on.exit(options(opt))
}


#' Cramer's V matrix between categorical variables.
#'
#' \code{char_cor_vars} is function for calculating Cramer's V matrix between categorical variables.
#' \code{char_cor} is function for calculating the correlation coefficient between variables by cremers 'V
#' @param dat A data frame.
#' @param x  The name of variable to process.
#' @param x_list Names of independent variables.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param note  Logical. Outputs info. Default is TRUE.
#' @return  A list contains correlation index of x with other variables in dat.
#' @examples
#' \dontrun{
#' char_x_list = get_names(dat = UCICreditCard,
#' types = c('factor', 'character'),
#' ex_cols = "ID$|date$|default.payment.next.month$", get_ex = FALSE)
#'  char_cor(dat = UCICreditCard[char_x_list])
#' }
#' @importFrom dplyr  %>% mutate_if
#' @export
char_cor_vars = function(dat, x) {
    dat = as.data.frame(dat) %>% merge_category(note = FALSE) %>% mutate_if(is.factor, as.character)

    vapply(seq_along(dat), function(j) {
        if (length(x) > 1 | length(unlist(x)) > 1) {
            cross_table = table(unlist(x), dat[, j])
        } else {

            cross_table = table(dat[, x], dat[, j])

        }
        sqrt(chisq.test(cross_table, correct = TRUE,
        simulate.p.value = TRUE)$statistic / (sum(cross_table, na.rm = TRUE) * min(ncol(cross_table) - 1,
                                                                     nrow(cross_table) - 1, na.rm = TRUE)))

    }, FUN.VALUE = numeric(1))
}

#' @rdname char_cor_vars
#' @export

char_cor = function(dat, x_list = NULL, ex_cols = "date$", parallel = FALSE, note = FALSE) {
    if (note) {
        cat("[NOTE] Computing the correlation matrix of factor or character variables.\n")
    }
    if (is.null(x_list)) {
        #Obtaining factor or character variables
        x_list = get_names(dat = dat, types = c('factor', 'character'),
                           ex_cols = ex_cols, get_ex = FALSE)
    }
    if (length(x_list) > 0) {
        #calculating the correlation coefficient between variables-cremers'V
        character_cor = loop_function(func = char_cor_vars, x_list = x_list,
                                      args = list(dat = dat[x_list]),
                                      bind = "cbind", as_list = FALSE,
                                      parallel = parallel)
        character_cor = as.matrix(character_cor)
        colnames(character_cor) = rownames(character_cor) = x_list
    } else {
        character_cor = NULL
    }
    return(character_cor)
}

#' auc_value
#' \code{auc_value} is for get best lambda required in lasso_filter. This function required in \code{lasso_filter}
#' @param prob A list of redict probability or score.
#' @param target Vector of target.
#' @return Lanmbda value
#' @export


auc_value = function(target, prob) {
    prob.rank = rank(prob)
    if(!is.numeric(target)){
        target =  as.numeric(as.character(target))
    }
    cnt_1 = sum(target)
    cnt_0 = length(target) - cnt_1
    prob_1 = prob.rank[target == 1]
    u = sum(prob_1) - cnt_1 * (cnt_1 + 1) / 2
    exp(log(u) - log(cnt_1) - log(cnt_0))
}
#' ks_value
#'
#' \code{ks_value} is for get K-S value for a prob or score.
#' @param prob A list of redict probability or score.
#' @param target Vector of target.
#' @return KS value
#' @export

ks_value = function(target, prob) {
  if(!is.numeric(target)){
    target =  as.numeric(as.character(target))
  }
 sum_prob = as.data.frame(table(prob,target))
 sum_prob = as.data.table(sum_prob)
 sum_prob = data.table :: dcast(sum_prob, prob  ~ target, value.var = "Freq")
 sum_prob[is.na(sum_prob)] = 0
 sum_prob = data.frame(unclass(sum_prob))
 cum_sum_1  = (cumsum(sum_prob$X1) / sum(sum_prob$X1))
 cum_sum_0  = (cumsum(sum_prob$X0) / sum(sum_prob$X0))
 KS = max(abs(cum_sum_1 - cum_sum_0), na.rm = TRUE)
  return(KS)
}


#' tnr_value
#'
#' \code{tnr_value} is for get true negtive rate for a prob or score.
#' @param prob A list of redict probability or score.
#' @param target Vector of target.
#' @return True Positive Rate
#' @export

tnr_value = function(prob, target){
   if(!is.numeric(target)){
     target =  as.numeric(as.character(target))
   }
   tnr = ifelse(length(prob > quantile(prob, 0.9,na.rm = TRUE)) > 0,
                mean(target[prob > quantile(prob, 0.9,na.rm = TRUE)],na.rm = TRUE),0)
   if(is.na(tnr)){tnr = 0}
   return(tnr)
}

#' lift_value
#'
#' \code{lift_value} is for getting max lift value for a prob or score.
#' @param prob A list of predict probability or score.
#' @param target Vector of target.
#' @return Max lift value
#' @export

lift_value = function(target, prob) {
   if(!is.numeric(target)){
     target =  as.numeric(as.character(target))
   }
   t_prob = data.frame(prob,target)
   t_prob = subset(t_prob, !is.na(prob))
   breaks = cut_equal(prob,g = 10)
   prob_bins = split_bins(dat =t_prob,x="prob",breaks)
   sum_prob = as.data.frame(table(prob_bins,target = t_prob$target))
   sum_prob = as.data.table(sum_prob)
   sum_prob = data.table :: dcast(sum_prob, prob_bins  ~ target, value.var = "Freq")
   sum_prob[is.na(sum_prob)] = 0
   sum_prob = data.frame(unclass(sum_prob))
   sum_lift = sum_prob[order(sum_prob$prob, decreasing = TRUE),]
   Lift = (cumsum(sum_lift$X1) / ifelse(sum_lift$X0 + sum_lift$X1 > 0 ,cumsum(sum_lift$X0 + sum_lift$X1),1)) /
                  (sum(sum_lift$X1,na.rm = TRUE) / sum(sum_lift$X0 + sum_lift$X1, na.rm = TRUE))
   MAX_Lift = mean(Lift, na.rm = TRUE)
   return(MAX_Lift)
}

#' Functions of xgboost feval
#'
#' \code{eval_auc} ,\code{eval_ks} ,\code{eval_lift},\code{eval_tnr} is for getting best params of xgboost.
#' @param preds A list of predict probability or score.
#' @param dtrain Matrix of x predictors.
#' @return List of best value

eval_auc = function(preds, dtrain) {
    labels = getinfo(dtrain, "label")
    auc = auc_value(target = labels, prob = preds)
    return(list(metric = "auc", value = round(auc, 6)))
}
#' @rdname eval_auc
eval_ks = function(preds, dtrain) {
    labels = getinfo(dtrain, "label")
    ks = ks_value(target = labels, prob = preds)
    return(list(metric = "ks", value = round(ks, 6)))
}
#' @rdname eval_auc
eval_tnr = function(preds, dtrain) {
    labels = getinfo(dtrain, "label")
    tnr = tnr_value(target = labels, prob = preds)
    return(list(metric = "tnr", value = round(tnr, 6)))
}
#' @rdname eval_auc
eval_lift = function(preds, dtrain) {
    labels = getinfo(dtrain, "label")
    lift = lift_value(target = labels, prob = preds)
    return(list(metric = "lift", value = round(lift, 6)))
}


#' get central value.
#'
#' This function is not intended to be used by end user.
#'
#' @param  x  A vector or list.
#' @param  weight_avg  avg weight to calculate means.
#' @export
#' @importFrom stats aggregate

get_median = function(x, weight_avg = NULL) {
    if (any(c('numeric', 'integer', 'double') == class(x)[1])) {
        if (is.null(weight_avg)) {
            central_value = median(x, na.rm = T)
        } else {
            central_value = ifelse(sum(weight_avg) > 0, sum(x * (weight_avg / sum(weight_avg))), NA)
        }
    } else {
        x = as.factor(x)
        if (is.null(weight_avg)) {
            central_value = levels(x)[which.max(table(x))]
        } else {
            central_value = levels(x)[which.max(aggregate(weight_avg, list(x), sum)[, 2])]
        }
    }
    return(central_value)
}



#' Entropy Weight Method
#'
#' \code{entropy_weight} is for calculating Entropy Weight.
#'
#' @param dat A data.frame with independent variables.
#' @param pos_vars Names or index of positive direction variables, the bigger the better.
#' @param neg_vars Names or index of negative direction variables, the smaller the better.
#' @return  A data.frame with weights of each variable.
#' @details
#' Step1 Raw data normalization
#' Step2 Find out the total amount of contributions of all samples to the index Xj
#' Step3 Each element of the step generated matrix is transformed into the product of each element and the LN (element),
#' and the information entropy is calculated.
#' Step4 Calculate redundancy.
#' Step5 Calculate the weight of each index.
#' @examples
#' entropy_weight(dat = ewm_data,
#'               pos_vars = c(6,8,9,10),
#'               neg_vars = c(7,11))
#' @export

entropy_weight = function(dat, pos_vars, neg_vars) {
	dat = quick_as_df(dat)
	dat_pos = apply(dat[pos_vars], 2, min_max_norm)
	dat_neg = apply(dat[neg_vars], 2, max_min_norm)
	dat_total = cbind(dat_pos, dat_neg)
	dat_total = apply(dat_total, 2, p_ij)
	dat_total = apply(dat_total, 2, e_ij)
	n_row = nrow(dat_total)
	k = 1 / log(n_row)
	d = -k * colSums(dat_total)
	d = 1 - d
	w = d / sum(d)
	w = data.frame(Feature = names(w), Weight = w, stringsAsFactors = FALSE, row.names = NULL)
	return(w)
}

#' Entropy
#'
#' This function is not intended to be used by end user.
#' @param x  A numeric vector.
#' @return  A numeric vector of entropy.

p_ij = function(x) {
	x = unlist(x)
	x = x / sum(x, na.rm = TRUE)
	return(x)
}

#' @rdname p_ij
e_ij = function(x) {
	x = unlist(x)
	for (i in 1:length(x)) {
		if (is.na(x[i]) || x[i] == 0) {
			x[i] = 0
		} else {
			x[i] = x[i] * log(x[i])
		}
	}
	return(x)
}


