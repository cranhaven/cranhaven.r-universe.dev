#' Plot PSI(Population Stability Index)
#'
#' You can use the \code{psi_plot} to plot PSI of your data.
#' \code{get_psi_plots} can loop through plots for all specified independent variables.
#' @param dat_train A data.frame with independent variables.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param x_list Names of independent variables.
#' @param x  The name of an independent variable.
#' @param occur_time  The name of occur time.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param breaks Splitting points for a continues variable.
#' @param g_width The width of graphs.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param dir_path The path for periodically saved graphic files.
#' @param file_name  The name for periodically saved data file. Default is NULL.
#' @examples
#' train_test = train_test_split(UCICreditCard[1:1000,], split_type = "Random",
#'  prop = 0.8, save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' get_psi_plots(dat_train[, c(8, 9)], dat_test = dat_test[, c(8, 9)])
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @importFrom data.table melt
#' @export

get_psi_plots = function(dat_train, dat_test = NULL, x_list = NULL,
                          ex_cols = NULL, breaks_list = NULL,occur_time = NULL,
                          g = 10,plot_show = TRUE,
                          save_data = FALSE, file_name = NULL,
                          parallel = FALSE, g_width = 8, dir_path = tempdir()) {
  
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE) #
  if (save_data) {
    dir_path = paste0(dir_path, "/psi_plot/")
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if (dir.exists(dir_path)) { file.remove(list.files(dir_path, recursive = TRUE, full.names = TRUE)) }
  }
  dat_train = checking_data(dat = dat_train)
  if (is.null(x_list)) {
    if (is.null(x_list)) {
      if (!is.null(breaks_list)) {
        x_list = unique(as.character(breaks_list[, "Feature"]))
      }
      x_list = get_x_list(x_list = x_list,
                          dat_train = dat_train,
                          dat_test = dat_test,
                          ex_cols = ex_cols)
    }
  } else {
    x_list = get_x_list(x_list = x_list,
                        dat_train = dat_train,
                        dat_test = dat_test,
                        ex_cols = ex_cols)
  }
  if(is.null(dat_test)){
    train_test = train_test_split(dat_train,split_type = 'OOT',prop = 0.5,
                                  occur_time = occur_time)
    dat_train = train_test$train
    dat_test = train_test$test
  }else{
    dat_test = checking_data(dat = dat_test)
  }
  
  df_ae_list = loop_function(func = psi_plot, x_list = x_list,
                             args = list(dat_train = dat_train, dat_test = dat_test,
                                         breaks_list = breaks_list,
                                         g = g,
                                         plot_show = plot_show,
                                         g_width = g_width, dir_path = dir_path, save_data = save_data),
                             bind = "rbind", parallel = parallel, as_list = FALSE)
  
  if (save_data) {
    save_data(df_ae_list, dir_path = dir_path, file_name = ifelse(is.null(file_name), "PSI_table", paste(file_name, "PSI_table", sep = ".")), append = FALSE, note = FALSE)
  }
  
  return(df_ae_list)
  options(opt) # reset
}


#' @rdname get_psi_plots
#' @export
psi_plot = function (dat_train, x, dat_test = NULL, occur_time = NULL, g_width = 8, 
                     breaks_list = NULL, breaks = NULL, g = 10, plot_show = TRUE, 
                     save_data = FALSE, dir_path = tempdir()) 
{
  digits_x = ifelse(is.numeric(dat_train[, x]), digits_num(dat_train[, 
                                                                     x]), 4)
  opt = options(warn = -1, scipen = 200, stringsAsFactors = FALSE, 
                digits = digits_x + 1)
  if (is.null(dat_test)) {
    train_test = train_test_split(dat_train, split_type = "OOT", 
                                  prop = 0.5, occur_time = occur_time)
    dat_train = train_test$train
    dat_test = train_test$test
  }
  dat_train$ae = "Expected"
  dat_test$ae = "Actual"
  xn = ae = NULL
  df_ae = get_psi(dat = dat_train, dat_test = dat_test, x = x, 
                   breaks_list = breaks_list, breaks = breaks, g = g, as_table = TRUE, 
                   note = FALSE, bins_no = TRUE)
  
  ae_total = data.table::melt(as.data.table(df_ae[c("Bins", 
                                                     "Ac_pct", "Ex_pct")]), id.vars = c("Bins"), 
                               variable.name = "actual_expected", value.name = "value")
  plot_2 = ggplot(ae_total, aes(x = ae_total$Bins, y = de_percent(ae_total$value, 
                                                                   4), fill = ae_total$actual_expected)) + 
    geom_bar(stat = "identity", 
                                                                                                                    position = position_dodge(width = 0.7)) + geom_text(aes(y = de_percent(ae_total$value, 
                                                                                                                                                                                           4), label = paste(ae_total$value)), position = position_dodge(width = 0.7), 
                                                                                                                                                                        size = ifelse(nrow(ae_total) > 10, 2.3, ifelse(nrow(ae_total) > 
                                                                                                                                                                                                                         5, 2.5, ifelse(nrow(ae_total) > 3, 2.8, 3))), vjust = 1, 
                                                                                                                                                                        hjust = 0.3, colour = "white") + annotate(geom = "text", 
                                                                                                                                                                                                                  x = dim(ae_total)[1]/3, y = max(c(de_percent(ae_total$value, 
                                                                                                                                                                                                                                                               4), max(de_percent(ae_total$value, 4)))) + 0.05, 
                                                                                                                                                                                                                  label = paste("PSI:", sum(df_ae$PSI_i))) + 
    scale_fill_manual(values = c(Ac_pct = love_color("shallow_cyan"), 
                                 Ex_pct = love_color("light_purple"))) + ylim(c(-0.01, 
                                                                                max(c(de_percent(ae_total$value, 4), max(de_percent(ae_total$value, 
                                                                                                                                    4)))) + 0.05)) + xlab(x) + ylab("Total Percent") + 
    ggtitle(paste(x, " Distribution of Expected and Actual")) + 
    plot_theme(legend.position = "top", title_size = 9, 
               axis_title_size = 8, angle = ifelse(nrow(ae_total) > 
                                                     10, 60, ifelse(nrow(ae_total) > 5, 40, ifelse(nrow(ae_total) > 
                                                                                                     3, 20, 10))), axis_size_x = ifelse(max(n_char(ae_total$Bins)) > 
                                                                                                                                          30, 5, ifelse(max(n_char(ae_total$Bins)) > 20, 
                                                                                                                                                        6, ifelse(max(n_char(ae_total$Bins)) > 10, 7, 
                                                                                                                                                                  8))))
  if (save_data) {
    ggsave(paste0(dir_path, paste(x, "png", sep = ".")), 
           plot = plot_2, width = g_width, height = g_width/2, 
           dpi = "retina")
  }
  if (plot_show) {
    plot(plot_2)
  }
  return(df_ae)
  options(opt)
}



#' partial_dependence_plot
#'
#' \code{partial_dependence_plot} is for generating a partial dependence plot.
#' \code{get_partial_dependence_plots} is for ploting partial dependence of all vairables in x_list.
#' @param model A data frame of training with predicted prob or score.
#' @param x The name of an independent variable.
#' @param x_list Names of independent variables.
#' @param x_train A data.frame with independent variables.
#' @param n.trees Number of trees for best.iter of gbm.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param dir_path The path for periodically saved graphic files.
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values = list("", -1))
#'
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#' #plot partial dependency of one variable
#' partial_dependence_plot(model = lr_model, x ="LIMIT_BAL", x_train = dat_train)
#' #plot partial dependency of all variables
#' pd_list = get_partial_dependence_plots(model = lr_model, x_list = x_list[1:2],
#'  x_train = dat_train, save_data = FALSE,plot_show = TRUE)
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter
#' @importFrom data.table melt dcast
#' @export


partial_dependence_plot = function(model, x, x_train, n.trees = NULL) {
  if (!requireNamespace("pdp", quietly = TRUE)) {
    cat_rule("Package `pdp` needed for partial dependence plot. Use 'require_packages(pdp)' to install and load it, .\n", col = love_color("deep_red"))
  } else {
    opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
    pd = pdp::partial(model,
                      pred.var = c(x),
                      grid.resolution = NULL,
                      train = x_train,
                      n.trees = n.trees,
                      prob = TRUE,
                      plot = FALSE,
                      plot.engine = c("ggplot2"),
                      .progress = "none",
                      chull = TRUE,
                      trim.outliers = TRUE)
    yhat = NULL
    pd_pl =  autoplot(pd, aes(x = pd[x], y = pd$yhat)) +
      geom_line(color = love_color("green_cyan"), size = 1) +
      theme_light() +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0)) +
      scale_y_continuous(limits = c(min(pd$yhat), max(pd$yhat)),
                         breaks = round(seq(min(pd$yhat), max(pd$yhat), length.out = 10), 4),
                         labels = sprintf("%0.3f", round(seq(min(pd$yhat), max(pd$yhat), length.out = 10), digits = 3))) +
      labs(x = x, y = "y_prob", title = paste(x, " - Partial Dependence"))
    return(pd_pl)
    options(opt)
  }
}


#' @rdname partial_dependence_plot
#' @export



get_partial_dependence_plots = function(model, x_train, x_list, n.trees = NULL,
                                         dir_path = getwd(),save_data =TRUE, plot_show = FALSE, parallel = FALSE) {
  if (!requireNamespace("pdp", quietly = TRUE)) {
    cat_rule("Package `pdp` needed for partial dependence plot. Use 'require_packages(pdp)' install and load it, .\n", col = love_color("deep_red"))
  } else {
    pd_list = loop_function(func = partial_dependence_plot,
                            args = list(model = model, x_train = x_train, n.trees = n.trees),
                            x_list = x_list, parallel = parallel, as_list = TRUE)
    if (save_data) {
      dir_path = paste0(dir_path, "/partial_dependence_plots/")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      if (dir.exists(dir_path)) { file.remove(list.files(dir_path, recursive = TRUE, full.names = TRUE)) }
      for (x in names(pd_list)) {
        ggsave(paste0(dir_path, "/pdp.", paste(x, "png", sep = '.')),
               plot = multi_grid(grobs = pd_list[x]),
               width = 4, height = 3, dpi = "retina")
      }
    }
    if (plot_show) {
      plot(multi_grid(grobs = pd_list))
    }
    return(pd_list)
  }
}


#' ks_table & plot
#'
#' \code{ks_table} is for generating a model performance table.
#' \code{ks_table_plot} is for ploting the table generated by \code{ks_table}
#' \code{ks_psi_plot} is for K-S & PSI distrbution ploting.
#' @param train_pred A data frame of training with predicted prob or score.
#' @param test_pred A data frame of validation with predict prob or score.
#' @param target The name of target variable.
#' @param score The name of prob or score variable.
#' @param g Number of breaks for prob or score.
#' @param g_width Width of graphs.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param gtitle The title of the graph & The name for periodically saved graphic file. Default is "_ks_psi_table".
#' @param breaks Splitting points of prob or score.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param dir_path The path for periodically saved graphic files.
#' @param file_name  The name for periodically saved data file. Default is NULL.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values = list("", -1))
#'
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' # model evaluation
#' ks_psi_plot(train_pred = dat_train, test_pred = dat_test,
#'                             score = "pred_LR", target = "target",
#'                             plot_show = TRUE)
#' tb_pred = ks_table_plot(train_pred = dat_train, test_pred = dat_test,
#'                                         score = "pred_LR", target = "target",
#'                                      g = 10, g_width = 13, plot_show = FALSE)
#' key_index = model_key_index(tb_pred)
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter
#' @importFrom data.table melt dcast
#' @export



ks_table = function(train_pred, test_pred = NULL, target = NULL, score = NULL,
                     g = 10, breaks = NULL, pos_flag = list("1", "1", "Bad", 1)) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `train_GB_index` = `test_GB_index` = `G` = `bins` = `B` = `%train` = `%test` = `#train` = `#test` = NULL
  if (is.null(target)) {
    stop("target is missing!\n")
  }
  if (is.null(score)) {
    stop("score is missing!\n")
  }
  if (is.null(breaks)) {
    breaks = get_breaks(dat = train_pred, x = score,
                        target = target, equal_bins = TRUE,
                        best = FALSE, g = g, note = FALSE)
  }
  train_pred$bins = split_bins(dat = train_pred, x = score, breaks = breaks, bins_no = TRUE)
  
  if (!is.null(target)) {
    if (length(unique(train_pred[, target])) > 1) {
      if (is.null(pos_flag)) {
        train_pred$target = ifelse(train_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
      } else {
        train_pred$target = ifelse(train_pred[, target] %in% pos_flag, "B", "G")
        if (length(unique(train_pred$target)) == 1) {
          stop(paste("The value in pos_flag is not one of the value of train_pred's target.\n"))
        }
      }
    } else {
      stop(paste("The value of train_pred's target is unique.\n"))
    }
  } else {
    stop(paste("The target variable is missing.\n"))
  }
  
  train_sum = train_pred %>%
    dplyr::filter(train_pred$target %in% c("B", "G")) %>%
    dplyr::group_by(bins) %>%
    dplyr::count(bins, target) %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    as.data.table()
  train_sum = data.table::dcast(train_sum[,c("bins", "target", "n"),with = FALSE],
                                 bins ~ target, value.var = "n")
  train_sum = quick_as_df(train_sum)
  train_sum[is.na(train_sum)] = 0
  train_ks = transform(train_sum,
                        train_total = G + B,
                        `%train_total` = round((G + B) / sum(G + B), 2),
                        `%train_B` = round(B / (G + B), 3),
                        `%train_cumG` = round(cumsum(G) / sum(G), 2),
                        `%train_cumB` = round(cumsum(B) / sum(B), 2),
                        `train_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), 2)))
  if (!is.null(test_pred) || length(test_pred) > 1) {
    test_pred$bins = split_bins(dat = test_pred, x = score, breaks = breaks, bins_no = TRUE)
    if (!is.null(target)) {
      if (length(unique(test_pred[, target])) > 1) {
        if (is.null(pos_flag)) {
          test_pred$target = ifelse(test_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
        } else {
          test_pred$target = ifelse(test_pred[, target] %in% pos_flag, "B", "G")
          if (length(unique(test_pred$target)) == 1) {
            stop(paste("The value in pos_flag is not one of the value of test_pred's target.\n"))
          }
        }
      } else {
        stop(paste("The value of test_pred's target is unique.\n"))
      }
    } else {
      stop(paste("The target variable is missing.\n"))
    }
    
    test_sum = test_pred %>%
      dplyr::filter(test_pred$target %in% c("B", "G")) %>%
      dplyr::group_by(bins) %>%
      dplyr::count(bins, target) %>%
      dplyr::mutate(percent = n / sum(n)) %>%
      as.data.table()
    
    test_sum = data.table::dcast(test_sum[,c("bins", "target", "n"),with = FALSE],
                                  bins ~ target, value.var = "n")
    test_sum = quick_as_df(test_sum)
    test_sum[is.na(test_sum)] = 0
    
    test_ks = transform(test_sum,
                         test_total = G + B,
                         `%test_total` = round((G + B) / (sum(G + B)), 2),
                         `%test_B` = round(B / (G + B), 3),
                         `%test_cumG` = round(cumsum(G) / sum(G), 2),
                         `%test_cumB` = round(cumsum(B) / sum(B), 2),
                         `test_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), 2))
    )
    
    dt_ks = merge(train_ks[c(1, 4:9)], test_ks[c(1, 4:9)], all.x = TRUE)
    dt_ks[is.na(dt_ks)] = 0
    names(dt_ks) = c("bins", "#train", "%train", "%train_B",
                     "%train_cumG", "%train_cumB", "train_K-S",
                     "#test", "%test", "%test_B", "%test_cumG",
                     "%test_cumB", "test_K-S")
    dt_ks = dt_ks %>% dplyr::mutate(PSI = ifelse(`%train` == 0 | `%test` == 0, 1, round((`%train` - `%test`) * log(`%train` / `%test`), 3)), `#total` = `#train` + `#test`)
    dt_ks = dt_ks[c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                    "%test_B", "%train_cumG", "%train_cumB", "%test_cumG", "%test_cumB",
                    "train_K-S", "test_K-S", "PSI")]
  } else {
    dt_ks = train_ks[c(1, 4:9)]
    dt_ks[is.na(dt_ks)] = 0
    names(dt_ks) = c("bins", "#train", "%train", "%train_B",
                     "%train_cumG", "%train_cumB", "train_K-S")
    
    dt_ks = dt_ks[c("bins",  "#train", "%train", "%train_B",
                    "%train_cumG", "%train_cumB",
                    "train_K-S")]
  }
  
  return(dt_ks)
  options(opt) # reset
}

#' ks_table_plot
#'
#' @rdname ks_table
#' @export



ks_table_plot = function(train_pred, test_pred, target = "target", score = "score",
                          g = 10,plot_show = TRUE, g_width = 12,file_name = NULL, save_data = FALSE,
                          dir_path = tempdir(), gtitle = NULL) {
  
  opt = options(scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  
  ` %train_cumG` = `%train_cumB` = `%train_B` = `%train` = `%test_cumG` = `%test_cumB` = `%test_B` =
    `%test` =  `%train_cumG` = NULL
  
  tb_pred = ks_table(train_pred=train_pred,test_pred = test_pred, target = target, score = score, g = g)
  total = c("Total",
             sum(tb_pred$`#total`,na.rm = TRUE),
             sum(tb_pred$`#train`, na.rm = TRUE),
             sum(tb_pred$`#test`, na.rm = TRUE),
             as_percent(sum(tb_pred$`#train`, na.rm = TRUE) / sum(tb_pred$`#total`, na.rm = TRUE), 2),
             as_percent(sum(tb_pred$`#test`, na.rm = TRUE) / sum(tb_pred$`#total` , na.rm = TRUE), 2),
             as_percent(sum(tb_pred$`%train_B` * tb_pred$`#train`, na.rm = TRUE) / sum(tb_pred$`#train`,  na.rm = TRUE), 2),
             as_percent(sum(tb_pred$`%test_B` * tb_pred$`#test`,na.rm = TRUE) / sum(tb_pred$`#test`, na.rm = TRUE), 2), "100%", "100%",
             "100%", "100%",
             max(tb_pred$`train_K-S`,na.rm = TRUE),
             max(tb_pred$`test_K-S`, na.rm = TRUE),
             sum(tb_pred$PSI,na.rm = TRUE))
  
  dt_pred = tb_pred[c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                       "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                       "%test_cumB", "train_K-S", "test_K-S", "PSI")]
  dt_pred = transform(dt_pred,
                       `%train` = as_percent(`%train`, digits = 2),
                       `%test` = as_percent(`%test`, digits = 2),
                       `%train_B` = as_percent(`%train_B`, digits = 3),
                       `%test_B` = as_percent(`%test_B`, digits = 3),
                       `%train_cumG` = as_percent(`%train_cumG`, digits = 2),
                       `%train_cumB` = as_percent(`%train_cumB`, digits = 2),
                       `%test_cumG` = as_percent(`%test_cumG`, digits = 2),
                       `%test_cumB` = as_percent(`%test_cumB`, digits = 2)
  )
  
  dt_pred = rbind(dt_pred, total)
  names(dt_pred) = c("bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                     "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                     "%test_cumB", "train_K-S", "test_K-S", "PSI")
  if (save_data) {
    dir_path = ifelse( !is.character(dir_path),
                       tempdir(), dir_path)
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if (!is.character(gtitle)) { gtitle = paste0("mymodel") }
    tb_ks = plot_table(dt_pred)
    ggsave(paste0(dir_path, "/", paste(paste(gtitle, "_ks_psi_table"), "png", sep = '.')),
           plot = tb_ks, dpi = "retina", width = g_width)
    save_data(dt_pred, file_name = paste(gtitle, "_ks_psi_table"), dir_path = dir_path)
  }
  return(dt_pred)
  options(opt) # reset
}


#' ks_table_plot
#'
#' @rdname ks_table
#' @export

ks_psi_plot = function(train_pred, test_pred, target = "target", score = "score",
                        gtitle = NULL, plot_show = TRUE, g_width = 12, save_data = FALSE,
                        breaks = NULL, g = 10, dir_path = tempdir()) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `value` = `train_test` = `bins` =   `%train_cumG` =   `%train_cumB` =  `%test_cumG` =  `%test_cumB` = NULL
  if (!dir.exists(dir_path)) dir.create(dir_path)
  if (is.null(gtitle)) { gtitle = paste0("Model") }
  tb_ks = ks_table(train_pred = train_pred, test_pred = test_pred,
                   target = target, score = score, g = g, breaks = breaks)
  
  ks = rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), tb_ks)
  ks_plot = ggplot(ks, aes(x = reorder(as.factor(as_percent(round(cumsum(ks$`%train`), 1))), cumsum(ks$`%train`)))) +
    geom_line(aes(y = `%train_cumG`, group = 1, color = "%train_cumG"), size = 1) +
    geom_point(aes(x = which.max(ks$`train_K-S`),
                   y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
               size = 2, shape = 21, fill = 'white', color = "#085A9C") +
    geom_segment(aes(x = which.max(ks$`train_K-S`),
                     y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)]) + 0.01,
                     xend = which.max(ks$`train_K-S`),
                     yend = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)]) - 0.01),
                 colour = love_color("deep_grey"), linetype = "dashed",
                 arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
    geom_line(aes(y = `%train_cumB`, color = '%train_cumB', group = 1), size = 1) +
    geom_point(aes(x = which.max(ks$`train_K-S`),
                   y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
               size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
    geom_point(aes(x = which.max(ks$`train_K-S`),
                   y = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)])),
               size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
    annotate(geom = 'text', x = 7, y = 0.1,
             label = paste('train K-S : ', max(round(ks$`train_K-S`, 2))),
             vjust = 1.5) +
    geom_line(aes(y = `%test_cumG`, group = 1, color = "%test_cumG"),
              linetype = "dashed", size = 1) +
    geom_point(aes(x = which.max(ks$`test_K-S`),
                   y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
               size = 2, shape = 21, fill = 'white', color = "#085A9C") +
    geom_segment(aes(x = which.max(ks$`test_K-S`),
                     y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)]) + 0.01,
                     xend = which.max(ks$`test_K-S`),
                     yend = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)]) - 0.01),
                 colour = love_color("deep_grey"), linetype = "dashed",
                 arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
    geom_line(aes(y = `%test_cumB`, color = '%test_cumB', group = 1),
              linetype = "dashed", size = 1) +
    geom_point(aes(x = which.max(ks$`test_K-S`),
                   y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
               size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
    geom_point(aes(x = which.max(ks$`test_K-S`),
                   y = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)])),
               size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
    annotate(geom = 'text', x = 7, y = 0.15,vjust = 1.5,
             label = paste('test K-S : ', max(round(ks$`test_K-S`, 2)))) +
    annotate("text", vjust = -1, label = "1", size = 4, colour = "black",
             x = which.max(ks$`train_K-S`) - 1,
             y = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)])) +
    annotate("text", vjust = 1, label = "0", size = 4, colour = "black",
             x = which.max(ks$`test_K-S`) + 1,
             y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])) +
    scale_colour_manual(values = c("%train_cumG" = love_color("dark_blue"),
                                   "%test_cumG" = love_color("dark_green"),
                                   "%train_cumB" = love_color("dark_red"),
                                   "%test_cumB" = love_color("dark_purple"))) +
    labs(x = "% of Total", y = "% of CumSum G/B",
         title = paste(gtitle,"K-S : Train vs. Test" ))+
    plot_theme(legend.position = "top", angle = 0)
  
  ts_total = data.table::melt(as.data.table(tb_ks[c("bins", "%train", "%test")]), id.vars = c("bins"),
                               variable.name = "train_test", value.name = "value")
  ts_1 = data.table::melt(as.data.table(tb_ks[c("bins", "%train_B", "%test_B")]), id.vars = c("bins"),
                           variable.name = "train_test", value.name = "value")
  
  psi_plot = ggplot(ts_total, aes(x = bins, y = value, fill = train_test)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
    geom_text(aes(y = value, label = paste(as_percent(value, digits = 3))),
              position = position_dodge(width = 0.7),
              size = 3, vjust = 1, hjust = 0.3, colour = "white") +
    geom_line(aes(x = factor(ts_1[[1]]),
                  y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                  color = ts_1$train_test,
                  linetype = ts_1$train_test,
                  group = ts_1$train_test),
              position = position_dodge(width = 0.5),size = 1) +
    geom_point(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                   color = ts_1$train_test,
                   group = ts_1$train_test),
               position = position_dodge(width = 0.5),
               fill = 'white', size = 2, shape = 21) +
    geom_text(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                  label = paste(as_percent(ts_1$value, digits = 3))),
              position = position_dodge(width = 0.5),
              colour = 'black', size = 3, vjust = -0.1) +
    annotate(geom = 'text',
             x = dim(ts_total)[1] / 3,
             y = max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.09,
             label = paste('PSI', round(sum(tb_ks$PSI,na.rm = TRUE),4), sep = " : ")) +
    scale_fill_manual(values = c('%train' = love_color("deep_grey"),
                                 '%test' = love_color("light_yellow"))) +
    scale_color_manual(values = c('%train_B' = love_color("shallow_red"),
                                  '%test_B' = love_color("sky_blue"))) +
    ylim(c(-0.01, max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.1)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    xlab(score) +
    ylab("Total Percent") +
    ggtitle(paste(gtitle,"Train and Test Distribution"))+
    plot_theme(legend.position = "top",
               angle = ifelse(nrow(ts_total) > 10, 50,
                              ifelse(nrow(ts_total) > 5, 30, 0)))
  if (save_data) {
    dir_path = ifelse(is.null(dir_path),
                      tempdir(), dir_path)
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if ( !is.character(gtitle)) { gtitle = paste0("mymodel") }
    ggsave(filename = paste0(dir_path, "/", paste(paste0(gtitle, "_ks_psi_plot"), "png", sep = '.')),
           device = "png",
           plot = multi_grid(grobs = list(ks_plot, psi_plot), ncol = 2, nrow = 1),
           dpi = "retina", width = g_width, height = g_width / 2)
  }
  if (plot_show) {
    ks_psi_plot = multi_grid(grobs = list(ks_plot, psi_plot), ncol = 2, nrow = 1)
    return(plot(ks_psi_plot))
  }
  options(opt) # reset
}

#' Correlation Plot
#'
#' \code{cor_plot} is for ploting correlation matrix
#' @param dat A data.frame with independent variables and target variable.
#' @param x_list Names of independent variables.
#' @param plot_show  Logical, show graph in current graphic device.
#' @param save_data Logical, save results in locally specified folder. Default is TRUE
#' @param gtitle The title of the graph & The name for periodically saved graphic file. Default is "_correlation_of_variables".
#' @param dir_path The path for periodically saved graphic files. Default is "./model/LR"
#' @examples
#' train_test = train_test_split(UCICreditCard,
#' split_type = "Random", prop = 0.8,save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' cor_plot(dat_train[,8:12],plot_show = TRUE)
#' @import ggplot2
#' @export

cor_plot = function(dat, dir_path = tempdir(), x_list = NULL,
                     gtitle = NULL, save_data = FALSE, plot_show = FALSE) {
  if (!is.null(x_list)) {
    dat = dat[, c(x_list)]
  }
  num_x_list = get_names(dat = dat,
                         types = c('numeric', 'integer', 'double'),
                         ex_cols = "",
                         get_ex = FALSE)
  cor_mat = cor(dat[, num_x_list],use = "complete.obs")
  if (save_data) {
    save_data(cor_mat, file_name = paste(gtitle, "correlation_matrix"),
              dir_path = dir_path, note = FALSE, row_names = TRUE)
  }
  if ( !is.character(gtitle)) { gtitle = paste0("Correlation Matrix") }else{gtitle = paste(gtitle, 'Correlation Matrix')}
  cor_p = cor_heat_plot(cor_mat,title = gtitle)
  if (save_data) {
    dir_path = ifelse(!is.character(dir_path) ,
                      tempdir(), dir_path)
    if (!dir.exists(dir_path)) dir.create(dir_path)
    
    ggsave(paste0(dir_path, "/", paste(paste0(gtitle, "_correlation_of_variables"), "png", sep = '.')), cor_p, dpi = "retina", width = 8)
  }
  if (plot_show) {
    plot(cor_p)
  }
}

#' Correlation Heat Plot
#'
#' \code{cor_heat_plot} is for ploting correlation matrix
#' @param cor_mat A correlation matrix.
#' @param low_color color of the lowest correlation between variables.
#' @param high_color  color of the highest correlation between variables.
#' @param title title of plot.
#' @examples
#' train_test = train_test_split(UCICreditCard,
#' split_type = "Random", prop = 0.8,save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' cor_mat = cor(dat_train[,8:12],use = "complete.obs")
#' cor_heat_plot(cor_mat)
#' @import ggplot2
#' @importFrom dplyr %>%
#' @export

cor_heat_plot = function(cor_mat, low_color = love_color('deep_red'),
                          high_color = love_color('light_cyan'), title = 'Correlation Matrix') {
  
  if (!isTRUE(all.equal(cor_mat, t(cor_mat)))) stop("correlation matrix is not symmetric")
  
  cor1 = data.frame(which(abs(cor_mat) > 0, arr.ind = TRUE))
  cor2 = sapply(1:nrow(cor1),
                function(x) { cbind(
                  colnames(cor_mat)[cor1[x,][[1]]],
                  colnames(cor_mat)[cor1[x,][[2]]],
                  cor_mat[cor1[x,][[1]], cor1[x,][[2]]])
                }) %>% t() %>% data.frame(stringsAsFactors = FALSE)
  
  cor_list = cbind(cor1, cor2)
  names(cor_list) = c("Vars1", "Vars2", "V1", "V2", "cor")
  cor_list$cor = as.numeric(cor_list$cor)
  
  ggplot(cor_list, aes(reorder(cor_list$Vars2, cor_list$Vars2),
                       reorder(cor_list$Vars1, cor_list$Vars1),
                       fill = cor_list$cor)) +
    geom_tile(colour = 'white') +
    geom_text(aes(label = round(cor_list$cor, 2)),
              size = ifelse(length(unique(cor_list$Vars2)) < 10, 3,
                            ifelse(length(unique(cor_list$Vars2)) > 20, 2, 2.5))) +
    scale_fill_gradient2(limits = c(-1, 1),
                         low = low_color, mid = 'white',
                         high = high_color,
                         midpoint = 0,
                         na.value = love_color('pale_grey')) +
    scale_y_discrete(limits = rev(unique(cor_list$Vars1)), labels = unique(cor_list$V1)) +
    scale_x_discrete(position = "top", labels = unique(cor_list$V2)) +
    labs(x = "", y = "", title = title) +
    theme(text = element_text(size = 15), rect = element_blank()) +
    plot_theme(legend.position = 'right', angle = 90)
}



#' model_key_index
#' \code{model_key_index} is for get plots for a  variable.
#' @param tb_pred  A table generated by code{\link{ks_table}}
#' @rdname ks_table
#' @export
model_key_index = function(tb_pred) {
  key_index = NULL
  if (any(is.element(c("train_K-S", "test_K-S", "PSI"), names(tb_pred)))) {
    b_psi = as.numeric(tb_pred[nrow(tb_pred), "PSI"])
    train_KS = as.numeric(tb_pred[nrow(tb_pred), "train_K-S"])
    test_KS = as.numeric(tb_pred[nrow(tb_pred), "test_K-S"])
    key_index = data.frame(train_KS = train_KS, test_KS = test_KS, PSI = b_psi)
  } else {
    key_index = data.frame(train_KS = NA, test_KS = NA, PSI = NA)
  }
  return(key_index)
}


#' love_color
#'
#' \code{love_color} is for get plots for a  variable.
#' @param color The name of colors.
#' @param type The type of colors, "deep", or the name of palette:.
#' The sequential palettes names are Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
#' The diverging palettes are BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#' The qualitative palettes are Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
#' @param n Number of different colors, minimum is 1.
#' @param ... Other parameters.
#' @examples
#' love_color(color="dark_cyan")
#' @import ggplot2
#' @export

love_color = function(color =NULL, type = 'Blues', n = 10,...) {
  NISEMURASAKI = rgb(86,46,55 , maxColorValue = 255 )
  TETSUKON = rgb(38,30,71 , maxColorValue = 255 )
  UMENEZUMI = rgb(158,122,122 , maxColorValue = 255 )
  YAMABUKI = rgb(255,177,27 , maxColorValue = 255 )
  KON =  rgb(15,37,64 , maxColorValue = 255 )
  ENTAN = rgb(215,84, 85 , maxColorValue = 255 )
  OITAKE = rgb(106,131, 114 , maxColorValue = 255 )
  WASURENAGUSA = rgb(125,185, 222 , maxColorValue = 255 )
  MIRUCHA = rgb(98,89,44, maxColorValue = 255)
  NAMARI = rgb(120,120,120, maxColorValue = 255)
  BOTAN =  rgb(193,50,142, maxColorValue = 255)
  BENITOBI = rgb(153,70,57, maxColorValue = 255)
  OHDO = rgb(182,142,85, maxColorValue = 255)
  YANAGICHA = rgb(147,150,80, maxColorValue = 255)
  USUGAKI = rgb(236,184,138, maxColorValue = 255)
  SUMIRE = rgb(102,50,124, maxColorValue = 255)
  SHIRONEZUMI =  rgb(189,192,186, maxColorValue = 255)
  KABACHA = rgb(179,92,55, maxColorValue = 255)
  ASAGI  = rgb(51,166,184, maxColorValue = 255)
  BENIMIDORI = rgb(123,144,210, maxColorValue = 255)
  HIGOSUSUTAKE =  rgb(141,116,42, maxColorValue = 255)
  KIMIRUCHA =  rgb(134,120,53, maxColorValue = 255)
  AOTAKE = rgb(0,137,108, maxColorValue = 255)
  NAMAKABE = rgb(125,108,70, maxColorValue = 255)
  WAKATAKE = rgb(93,172,129, maxColorValue = 255)
  NAE= rgb(134,193,102, maxColorValue = 255)
  BENIHIWADA =  rgb(136,76,58, maxColorValue = 255)
  KOHAKU =  rgb(202,122,44, maxColorValue = 255)
  TERIGAKI =  rgb(196,98, 67, maxColorValue = 255)
  ONANDOCHA=  rgb(70, 93, 76, maxColorValue = 255)
  RURI = rgb(0, 92, 175, maxColorValue = 255)
  TAIKOH = rgb(248, 195, 205, maxColorValue = 255)
  SUMI  = rgb(28, 28, 28, maxColorValue = 255)
  ICHIGO =   rgb(181, 73, 91, maxColorValue = 255)
  MIZU =  rgb(129, 199, 212, maxColorValue = 255)
  CHITOSEMIDORI =  rgb(54, 86, 60, maxColorValue = 255)
  TETSUONANDO =  rgb(37, 83, 89, maxColorValue = 255)
  IWAICHA =  rgb(100, 106, 88, maxColorValue = 255)
  GINNEZUMI =  rgb(145, 152, 159, maxColorValue = 255)
  AIKOBICHA =  rgb(75, 78, 42, maxColorValue = 255)
  KOGECHA=  rgb(86, 63, 46, maxColorValue = 255)
  SABISEIJI =  rgb(134, 166, 151, maxColorValue = 255)
  SAKURA=  rgb(254, 223, 225, maxColorValue = 255)
  KAMENOZOKI = rgb(165, 222, 228, maxColorValue = 255)
  AKAKOH = rgb(227, 145, 110, maxColorValue = 255)
  BENIUKON = rgb(233, 139, 42, maxColorValue = 255)
  KONJYO = rgb(17, 50, 133, maxColorValue = 255)
  FUTAAI = rgb(112, 100, 154, maxColorValue = 255)
  TAMAMOROKOSHI =  rgb(232, 182, 71, maxColorValue = 255)
  EBICHA =  rgb(115, 67, 56, maxColorValue = 255)
  ENJI=  rgb(159, 53, 58, maxColorValue = 255)
  AKEBONO =  rgb(241, 148, 131, maxColorValue = 255)
  RIKYUSHIRACHA = rgb(180, 165, 130, maxColorValue = 255)
  NOSHIMEHANA =  rgb(43, 95, 117, maxColorValue = 255)
  UGUISU  =  rgb(108, 106, 45, maxColorValue = 255)
  SAKURANEZUMI =  rgb(177, 150, 147, maxColorValue = 255)
  KUWAZOME =  rgb(100, 54, 60, maxColorValue = 255)
  EDOCHA = rgb(175, 95, 60, maxColorValue = 255)
  BAIKOCHA = rgb(137, 145, 107, maxColorValue = 255)
  KARACHA  = rgb(180, 113, 87, maxColorValue = 255)
  HAIZAKURA = rgb(215, 196, 187, maxColorValue = 255)
  SHINBASHI = rgb(0, 137, 167, maxColorValue = 255)
  KURIKAWACHA = rgb(106, 64, 40, maxColorValue = 255)
  SORA =  rgb(88, 178, 220, maxColorValue = 255)
  URAYANAGI = rgb(181, 202, 160, maxColorValue = 255)
  TOKIWA = rgb(27, 129, 62, maxColorValue = 255)
  SUOH = rgb(142, 53, 74, maxColorValue = 255)
  KURENAI = rgb(203, 27, 69, maxColorValue = 255)
  AONI = rgb(81, 110, 65, maxColorValue = 255)
  KONKIKYO = rgb(33, 30, 85, maxColorValue = 255)
  MURASAKITOBI = rgb(96, 55, 62, maxColorValue = 255)
  SHIRONERI= rgb(252, 250, 242, maxColorValue = 255)
  YANAGISUSUTAKE = rgb(74, 89, 61, maxColorValue = 255)
  JINZAMOMI =  rgb(235, 122, 119, maxColorValue = 255)
  MESSHI  =  rgb(83, 61, 91, maxColorValue = 255)
  NAKABENI =  rgb(219, 77, 109, maxColorValue = 255)
  RURIKON = rgb(11, 52, 110, maxColorValue = 255)
  AISUMICHA = rgb(55, 60, 56, maxColorValue = 255)
  GINSUSUTAKE = rgb(130, 102, 58, maxColorValue = 255)
  BUDOHNEZUMI = rgb(94, 61, 80, maxColorValue = 255)
  AKE = rgb(204, 84, 58, maxColorValue = 255)
  KORAINANDO = rgb(48, 90, 86, maxColorValue = 255)
  KIKYO = rgb(106, 76, 156, maxColorValue = 255)
  TAMAGO = rgb(249, 191, 69, maxColorValue = 255)
  AIMIRUCHA= rgb(15, 76, 58, maxColorValue = 255)
  NATANEYU = rgb(162, 140, 55, maxColorValue = 255)
  MUSHIAO = rgb(32, 96, 79, maxColorValue = 255)
  KOKIAKE = rgb(134, 71, 63, maxColorValue = 255)
  HANABA= rgb(247, 194, 66, maxColorValue = 255)
  NANOHANA =  rgb(247, 217, 76, maxColorValue = 255)
  HIWACHA=  rgb(165, 160, 81, maxColorValue = 255)
  AKABENI=  rgb(203, 64, 66, maxColorValue = 255)
  KAKISHIBU =  rgb(163, 94, 71, maxColorValue = 255)
  OHNI =  rgb(240, 94, 28, maxColorValue = 255)
  SHAREGAKI = rgb(255, 186, 132, maxColorValue = 255)
  SHINSYU= rgb(171, 59, 58, maxColorValue = 255)
  KITSURUBAMI = rgb(186, 145, 50, maxColorValue = 255)
  YANAGIZOME = rgb(145, 173, 112, maxColorValue = 255)
  SEIJI= rgb(105, 176, 172, maxColorValue = 255)
  HAI= rgb(130, 130, 130, maxColorValue = 255)
  KANZO= rgb(252, 159, 77, maxColorValue = 255)
  SODENKARACHA = rgb(160, 103, 75, maxColorValue = 255)
  IMAYOH= rgb(208, 90, 110, maxColorValue = 255)
  FUKAGAWANEZUMI =  rgb(119, 150, 154, maxColorValue = 255)
  MATSUBA =  rgb(66, 96, 45, maxColorValue = 255)
  TOKIGARACHA  =  rgb(219, 142, 113, maxColorValue = 255)
  HASHITA =  rgb(152, 109, 178, maxColorValue = 255)
  OMESHICHA = rgb(55, 107, 109, maxColorValue = 255)
  KUCHINASHI = rgb(246, 197, 85, maxColorValue = 255)
  SUNEZUMI = rgb(120, 125, 123, maxColorValue = 255)
  AONIBI = rgb(83, 89, 83, maxColorValue = 255)
  SABITETSUONANDO= rgb(64, 91, 85, maxColorValue = 255)
  KESHIZUMI = rgb(67, 67, 67, maxColorValue = 255)
  KUROBENI= rgb(63, 43, 54, maxColorValue = 255)
  KIHADA= rgb(251, 226, 81, maxColorValue = 255)
  TORINOKO= rgb(218, 201, 166, maxColorValue = 255)
  MUSHIKURI = rgb(217, 205, 144, maxColorValue = 255)
  CYOHSYUN= rgb(191, 103, 102, maxColorValue = 255)
  USUBENI = rgb(232, 122, 144, maxColorValue = 255)
  UMEMURASAKI = rgb(168, 73, 122, maxColorValue = 255)
  FUJINEZUMI= rgb(110, 117, 164, maxColorValue = 255)
  SHIROTSURUBAMI = rgb(220, 184, 121, maxColorValue = 255)
  BENIKESHINEZUMI= rgb(82, 67, 61, maxColorValue = 255)
  HANADA = rgb(0, 98, 132, maxColorValue = 255)
  OUCHI = rgb(155, 144, 194, maxColorValue = 255)
  MIZUASAGI= rgb(102, 186, 183, maxColorValue = 255)
  HANAASAGI= rgb(30, 136, 168, maxColorValue = 255)
  SABIONANDO= rgb(51, 103, 116, maxColorValue = 255)
  SYOJYOHI = rgb(232, 48, 21, maxColorValue = 255)
  TOHOH= rgb(255, 196, 8, maxColorValue = 255)
  BINROJIZOME= rgb(58, 50, 38, maxColorValue = 255)
  OMINAESHI = rgb(221, 210, 59, maxColorValue = 255)
  BYAKUROKU= rgb(168, 216, 185, maxColorValue = 255)
  darkbrown_blues = c(OITAKE,NAMARI,HAI,
                      FUJINEZUMI,FUTAAI,KIKYO,SUMIRE)
  darkred_yellows = c(BOTAN, UMEMURASAKI)
  darkred_oranges = c(OHNI, SYOJYOHI)
  
  darknihon_brown_blues = c(TETSUKON,KUROBENI, KON, AIMIRUCHA
                            ,AISUMICHA, BINROJIZOME,SUMI)
  darknihon_brown_yellows = c(CHITOSEMIDORI,SABITETSUONANDO,
                              TOKIWA, OMESHICHA
                              ,NOSHIMEHANA,KORAINANDO,TETSUONANDO,MUSHIAO)
  darknihon_reds = c( ICHIGO,  NAKABENI,TERIGAKI,AKE, AKABENI,KURENAI)
  darknihon_green_blues = c(RURI, KONJYO, RURIKON, HANADA)
  darknihon_blue_greens = c(NISEMURASAKI,  EBICHA,
                            KURIKAWACHA, MESSHI
                            ,KESHIZUMI,
                            UGUISU,AONI,
                            YANAGISUSUTAKE, AONIBI,IWAICHA,AIKOBICHA)
  darknihon_yellow_reds = c(KOHAKU ,OHDO,KITSURUBAMI
                            , YANAGICHA,HIWACHA, NATANEYU,KIMIRUCHA)
  darknihon_brown_cha = c(NAMAKABE,SUOH,
                          SHINSYU,BENITOBI,SODENKARACHA,
                          KOKIAKE, EDOCHA)
  darknihon_brown_reds = c(UMENEZUMI, KARACHA, IMAYOH, CYOHSYUN)
  
  lightnihon_fen = c(TAIKOH, SAKURA, SHIRONERI)
  lightnihon_green_purples = c(GINNEZUMI, SABISEIJI, RIKYUSHIRACHA, SAKURANEZUMI,
                               FUKAGAWANEZUMI, HASHITA, OUCHI)
  lightnihon_brown_greens = c(WAKATAKE, NAE,YANAGIZOME, BAIKOCHA)
  lightnihon_green_blues = c(ASAGI, AOTAKE,  HANAASAGI)
  lightnihon_green_yellows =c(SHIRONEZUMI, HAIZAKURA,
                              MUSHIKURI,KAMENOZOKI, BYAKUROKU,URAYANAGI)
  lightnihon_red_yellows = c(JINZAMOMI,AKAKOH, AKEBONO, SHAREGAKI,
                             USUGAKI, SHIROTSURUBAMI)
  lightnihon_blue_greens = c( BENIMIDORI,SORA,WASURENAGUSA,MIZU, MIZUASAGI)
  lightnihon_yellow_oranges = c(KANZO, KUCHINASHI, NANOHANA,
                                OMINAESHI,TAMAMOROKOSHI,TOHOH)
  lightnihon_9x1 = c(lightnihon_blue_greens[1],
                     lightnihon_yellow_oranges[1],
                     lightnihon_green_yellows[1],
                     lightnihon_green_yellows[2],
                     lightnihon_red_yellows[1],
                     lightnihon_brown_greens[1],
                     lightnihon_green_purples[1],
                     lightnihon_green_blues[1],
                     lightnihon_fen[1])
  
  lightnihon_6x1 = c(lightnihon_blue_greens[2],
                     lightnihon_yellow_oranges[2],
                     lightnihon_red_yellows[2],
                     lightnihon_green_purples[2],
                     lightnihon_fen[2],
                     lightnihon_green_yellows[4])
  
  lightnihon_7x1 = c(lightnihon_blue_greens[3],
                     lightnihon_brown_greens[2],
                     lightnihon_yellow_oranges[3],
                     lightnihon_green_yellows[3],
                     lightnihon_red_yellows[3],
                     lightnihon_green_purples[3],
                     lightnihon_green_blues[3])
  lightnihon_8x1 = c(lightnihon_blue_greens[4],
                     lightnihon_yellow_oranges[4],
                     lightnihon_red_yellows[4],
                     lightnihon_green_purples[4],
                     lightnihon_yellow_oranges[6],
                     lightnihon_green_yellows[6],
                     lightnihon_red_yellows[6],
                     lightnihon_green_purples[7])
  
  lightnihon_5x1 = c(lightnihon_blue_greens[5],
                     lightnihon_yellow_oranges[5],
                     lightnihon_green_yellows[5],
                     lightnihon_red_yellows[5],
                     lightnihon_green_purples[5])
  
  
  darknihon_2x1 = c(
    darknihon_brown_blues[3],
    darkred_oranges[2])
  
  darknihon_4x1 = c(
    darkbrown_blues[7],
    darknihon_yellow_reds[7],
    darknihon_brown_cha[7],
    darknihon_brown_yellows[7]
  )
  
  darknihon_5x1 = c(
    darkbrown_blues[1],
    darknihon_reds[1],
    darknihon_yellow_reds[1],
    darknihon_brown_cha[1],
    darknihon_brown_reds[1]
  )
  
  darknihon_5x2 = c(
    darkbrown_blues[5],
    darknihon_reds[5],
    darknihon_yellow_reds[5],
    darknihon_brown_cha[5],
    darknihon_brown_yellows[5]
  )  
  
  darknihon_5x3 = c(
    darkbrown_blues[6],
    darknihon_reds[6],
    darknihon_yellow_reds[6],
    darknihon_brown_cha[6],
    darknihon_brown_yellows[6]
  )  
  
  darknihon_6x1 = c(
    darknihon_brown_yellows[1],
    darknihon_reds[6],
    darknihon_green_blues[1],
    darknihon_brown_blues[2],
    darkred_oranges[1],
    darknihon_blue_greens[2]
  )
  
  darknihon_6x2 = c(
    darkbrown_blues[2],
    darknihon_reds[2],
    darknihon_yellow_reds[2],
    darknihon_brown_cha[2],
    darknihon_brown_reds[2],
    darknihon_brown_yellows[2]
  )
  
  darknihon_6x3 = c(
    darknihon_brown_yellows[3],
    darknihon_reds[3],
    darknihon_green_blues[1],
    darknihon_blue_greens[3],
    darknihon_brown_yellows[8],
    darknihon_green_blues[3]
  )
  darknihon_8x1 = c(
    lightnihon_green_blues[2],
    darknihon_reds[4],
    darkbrown_blues[4],
    darknihon_brown_reds[4],
    darknihon_yellow_reds[4],
    darknihon_brown_cha[4],
    darknihon_brown_yellows[4],
    lightnihon_green_purples[6]
  )
  
  brewer_pal = function (n = 9, name = type) {
    switch(name, Accent = switch(n - 2, rgb(c(127, 190, 253), 
                                            c(201, 174, 192), c(127, 212, 134), maxColorValue = 255), 
                                 rgb(c(127, 190, 253, 255), c(201, 174, 192, 255), c(127, 
                                                                                     212, 134, 153), maxColorValue = 255), rgb(c(127, 
                                                                                                                                 190, 253, 255, 56), c(201, 174, 192, 255, 108), c(127, 
                                                                                                                                                                                   212, 134, 153, 176), maxColorValue = 255), rgb(c(127, 
                                                                                                                                                                                                                                    190, 253, 255, 56, 240), c(201, 174, 192, 255, 108, 
                                                                                                                                                                                                                                                               2), c(127, 212, 134, 153, 176, 127), maxColorValue = 255), 
                                 rgb(c(127, 190, 253, 255, 56, 240, 191), c(201, 174, 
                                                                            192, 255, 108, 2, 91), c(127, 212, 134, 153, 176, 
                                                                                                     127, 23), maxColorValue = 255), rgb(c(127, 190, 253, 
                                                                                                                                           255, 56, 240, 191, 102), c(201, 174, 192, 255, 108, 
                                                                                                                                                                      2, 91, 102), c(127, 212, 134, 153, 176, 127, 23, 
                                                                                                                                                                                     102), maxColorValue = 255)), 
           Blues = switch(n - 2, 
                          rgb(c(222, 158, 49), c(235, 202, 130), c(247, 225, 189), 
                              maxColorValue = 255), rgb(c(239, 189, 107, 33), c(243, 
                                                                                215, 174, 113), c(255, 231, 214, 181), maxColorValue = 255), 
                          rgb(c(239, 189, 107, 49, 8), c(243, 215, 174, 130, 81), 
                              c(255, 231, 214, 189, 156), maxColorValue = 255), 
                          rgb(c(239, 198, 158, 107, 49, 8), c(243, 219, 202, 174, 
                                                              130, 81), c(255, 239, 225, 214, 189, 156), maxColorValue = 255), 
                          rgb(c(239, 198, 158, 107, 66, 33, 8), c(243, 219, 202, 
                                                                  174, 146, 113, 69), c(255, 239, 225, 214, 198, 181, 
                                                                                        148), maxColorValue = 255), rgb(c(247, 222, 198, 
                                                                                                                          158, 107, 66, 33, 8), c(251, 235, 219, 202, 174, 
                                                                                                                                                  146, 113, 69), c(255, 247, 239, 225, 214, 198, 181, 
                                                                                                                                                                   148), maxColorValue = 255), rgb(c(247, 222, 198, 
                                                                                                                                                                                                     158, 107, 66, 33, 8, 8), c(251, 235, 219, 202, 174, 
                                                                                                                                                                                                                                146, 113, 81, 48), c(255, 247, 239, 225, 214, 198, 
                                                                                                                                                                                                                                                     181, 156, 107), maxColorValue = 255)), BrBG = switch(n - 
                                                                                                                                                                                                                                                                                                            2, rgb(c(216, 245, 90), c(179, 245, 180), c(101, 245, 
                                                                                                                                                                                                                                                                                                                                                        172), maxColorValue = 255), rgb(c(166, 223, 128, 1), 
                                                                                                                                                                                                                                                                                                                                                                                        c(97, 194, 205, 133), c(26, 125, 193, 113), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                          rgb(c(166, 223, 245, 128, 1), c(97, 194, 245, 205, 133), 
                                                                                                                                                                                                                                                                                                              c(26, 125, 245, 193, 113), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                          rgb(c(140, 216, 246, 199, 90, 1), c(81, 179, 232, 234, 
                                                                                                                                                                                                                                                                                                                                              180, 102), c(10, 101, 195, 229, 172, 94), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                          rgb(c(140, 216, 246, 245, 199, 90, 1), c(81, 179, 232, 
                                                                                                                                                                                                                                                                                                                                                   245, 234, 180, 102), c(10, 101, 195, 245, 229, 172, 
                                                                                                                                                                                                                                                                                                                                                                          94), maxColorValue = 255), rgb(c(140, 191, 223, 246, 
                                                                                                                                                                                                                                                                                                                                                                                                           199, 128, 53, 1), c(81, 129, 194, 232, 234, 205, 
                                                                                                                                                                                                                                                                                                                                                                                                                               151, 102), c(10, 45, 125, 195, 229, 193, 143, 94), 
                                                                                                                                                                                                                                                                                                                                                                                                         maxColorValue = 255), rgb(c(140, 191, 223, 246, 245, 
                                                                                                                                                                                                                                                                                                                                                                                                                                     199, 128, 53, 1), c(81, 129, 194, 232, 245, 234, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                         205, 151, 102), c(10, 45, 125, 195, 245, 229, 193, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           143, 94), maxColorValue = 255), rgb(c(84, 140, 191, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 223, 246, 199, 128, 53, 1, 0), c(48, 81, 129, 194, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  232, 234, 205, 151, 102, 60), c(5, 10, 45, 125, 195, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  229, 193, 143, 94, 48), maxColorValue = 255), rgb(c(84, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      140, 191, 223, 246, 245, 199, 128, 53, 1, 0), c(48, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      81, 129, 194, 232, 245, 234, 205, 151, 102, 60), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    c(5, 10, 45, 125, 195, 245, 229, 193, 143, 94, 48), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    maxColorValue = 255)), BuGn = switch(n - 2, rgb(c(229, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      153, 44), c(245, 216, 162), c(249, 201, 95), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         rgb(c(237, 178, 102, 35), c(248, 226, 194, 139), c(251, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            226, 164, 69), maxColorValue = 255), rgb(c(237, 178, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       102, 44, 0), c(248, 226, 194, 162, 109), c(251, 226, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  164, 95, 44), maxColorValue = 255), rgb(c(237, 204, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            153, 102, 44, 0), c(248, 236, 216, 194, 162, 109), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          c(251, 230, 201, 164, 95, 44), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         rgb(c(237, 204, 153, 102, 65, 35, 0), c(248, 236, 216, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 194, 174, 139, 88), c(251, 230, 201, 164, 118, 69, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       36), maxColorValue = 255), rgb(c(247, 229, 204, 153, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        102, 65, 35, 0), c(252, 245, 236, 216, 194, 174, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           139, 88), c(253, 249, 230, 201, 164, 118, 69, 36), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      maxColorValue = 255), rgb(c(247, 229, 204, 153, 102, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  65, 35, 0, 0), c(252, 245, 236, 216, 194, 174, 139, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   109, 68), c(253, 249, 230, 201, 164, 118, 69, 44, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               27), maxColorValue = 255)), BuPu = switch(n - 2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         rgb(c(224, 158, 136), c(236, 188, 86), c(244, 218, 167), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             maxColorValue = 255), rgb(c(237, 179, 140, 136), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       c(248, 205, 150, 65), c(251, 227, 198, 157), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         rgb(c(237, 179, 140, 136, 129), c(248, 205, 150, 86, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           15), c(251, 227, 198, 167, 124), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         rgb(c(237, 191, 158, 140, 136, 129), c(248, 211, 188, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                150, 86, 15), c(251, 230, 218, 198, 167, 124), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         rgb(c(237, 191, 158, 140, 140, 136, 110), c(248, 211, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     188, 150, 107, 65, 1), c(251, 230, 218, 198, 177, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              157, 107), maxColorValue = 255), rgb(c(247, 224, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     191, 158, 140, 140, 136, 110), c(252, 236, 211, 188, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      150, 107, 65, 1), c(253, 244, 230, 218, 198, 177, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          157, 107), maxColorValue = 255), rgb(c(247, 224, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 191, 158, 140, 140, 136, 129, 77), c(252, 236, 211, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      188, 150, 107, 65, 15, 0), c(253, 244, 230, 218, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   198, 177, 157, 124, 75), maxColorValue = 255)), Dark2 = switch(n - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    2, rgb(c(27, 217, 117), c(158, 95, 112), c(119, 2, 179), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           maxColorValue = 255), rgb(c(27, 217, 117, 231), c(158, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             95, 112, 41), c(119, 2, 179, 138), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  rgb(c(27, 217, 117, 231, 102), c(158, 95, 112, 41, 166), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      c(119, 2, 179, 138, 30), maxColorValue = 255), rgb(c(27, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           217, 117, 231, 102, 230), c(158, 95, 112, 41, 166, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       171), c(119, 2, 179, 138, 30, 2), maxColorValue = 255), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  rgb(c(27, 217, 117, 231, 102, 230, 166), c(158, 95, 112, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             41, 166, 171, 118), c(119, 2, 179, 138, 30, 2, 29), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      maxColorValue = 255), rgb(c(27, 217, 117, 231, 102, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  230, 166, 102), c(158, 95, 112, 41, 166, 171, 118, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    102), c(119, 2, 179, 138, 30, 2, 29, 102), maxColorValue = 255)), 
           GnBu = switch(n - 2, rgb(c(224, 168, 67), c(243, 221, 
                                                       162), c(219, 181, 202), maxColorValue = 255), rgb(c(240, 
                                                                                                           186, 123, 43), c(249, 228, 204, 140), c(232, 188, 
                                                                                                                                                   196, 190), maxColorValue = 255), rgb(c(240, 186, 
                                                                                                                                                                                          123, 67, 8), c(249, 228, 204, 162, 104), c(232, 188, 
                                                                                                                                                                                                                                     196, 202, 172), maxColorValue = 255), rgb(c(240, 
                                                                                                                                                                                                                                                                                 204, 168, 123, 67, 8), c(249, 235, 221, 204, 162, 
                                                                                                                                                                                                                                                                                                          104), c(232, 197, 181, 196, 202, 172), maxColorValue = 255), 
                         rgb(c(240, 204, 168, 123, 78, 43, 8), c(249, 235, 
                                                                 221, 204, 179, 140, 88), c(232, 197, 181, 196, 
                                                                                            211, 190, 158), maxColorValue = 255), rgb(c(247, 
                                                                                                                                        224, 204, 168, 123, 78, 43, 8), c(252, 243, 235, 
                                                                                                                                                                          221, 204, 179, 140, 88), c(240, 219, 197, 181, 
                                                                                                                                                                                                     196, 211, 190, 158), maxColorValue = 255), rgb(c(247, 
                                                                                                                                                                                                                                                      224, 204, 168, 123, 78, 43, 8, 8), c(252, 243, 
                                                                                                                                                                                                                                                                                           235, 221, 204, 179, 140, 104, 64), c(240, 219, 
                                                                                                                                                                                                                                                                                                                                197, 181, 196, 211, 190, 172, 129), maxColorValue = 255)), 
           Greens = switch(n - 2, rgb(c(229, 161, 49), c(245, 217, 
                                                         163), c(224, 155, 84), maxColorValue = 255), rgb(c(237, 
                                                                                                            186, 116, 35), c(248, 228, 196, 139), c(233, 179, 
                                                                                                                                                    118, 69), maxColorValue = 255), rgb(c(237, 186, 116, 
                                                                                                                                                                                          49, 0), c(248, 228, 196, 163, 109), c(233, 179, 118, 
                                                                                                                                                                                                                                84, 44), maxColorValue = 255), rgb(c(237, 199, 161, 
                                                                                                                                                                                                                                                                     116, 49, 0), c(248, 233, 217, 196, 163, 109), c(233, 
                                                                                                                                                                                                                                                                                                                     192, 155, 118, 84, 44), maxColorValue = 255), rgb(c(237, 
                                                                                                                                                                                                                                                                                                                                                                         199, 161, 116, 65, 35, 0), c(248, 233, 217, 196, 
                                                                                                                                                                                                                                                                                                                                                                                                      171, 139, 90), c(233, 192, 155, 118, 93, 69, 50), 
                                                                                                                                                                                                                                                                                                                                                                       maxColorValue = 255), rgb(c(247, 229, 199, 161, 116, 
                                                                                                                                                                                                                                                                                                                                                                                                   65, 35, 0), c(252, 245, 233, 217, 196, 171, 139, 
                                                                                                                                                                                                                                                                                                                                                                                                                 90), c(245, 224, 192, 155, 118, 93, 69, 50), maxColorValue = 255), 
                           rgb(c(247, 229, 199, 161, 116, 65, 35, 0, 0), c(252, 
                                                                           245, 233, 217, 196, 171, 139, 109, 68), c(245, 
                                                                                                                     224, 192, 155, 118, 93, 69, 44, 27), maxColorValue = 255)), 
           Greys = switch(n - 2, rgb(c(240, 189, 99), c(240, 189, 
                                                        99), c(240, 189, 99), maxColorValue = 255), rgb(c(247, 
                                                                                                          204, 150, 82), c(247, 204, 150, 82), c(247, 204, 
                                                                                                                                                 150, 82), maxColorValue = 255), rgb(c(247, 204, 150, 
                                                                                                                                                                                       99, 37), c(247, 204, 150, 99, 37), c(247, 204, 150, 
                                                                                                                                                                                                                            99, 37), maxColorValue = 255), rgb(c(247, 217, 189, 
                                                                                                                                                                                                                                                                 150, 99, 37), c(247, 217, 189, 150, 99, 37), c(247, 
                                                                                                                                                                                                                                                                                                                217, 189, 150, 99, 37), maxColorValue = 255), rgb(c(247, 
                                                                                                                                                                                                                                                                                                                                                                    217, 189, 150, 115, 82, 37), c(247, 217, 189, 150, 
                                                                                                                                                                                                                                                                                                                                                                                                   115, 82, 37), c(247, 217, 189, 150, 115, 82, 37), 
                                                                                                                                                                                                                                                                                                                                                                  maxColorValue = 255), rgb(c(255, 240, 217, 189, 150, 
                                                                                                                                                                                                                                                                                                                                                                                              115, 82, 37), c(255, 240, 217, 189, 150, 115, 82, 
                                                                                                                                                                                                                                                                                                                                                                                                              37), c(255, 240, 217, 189, 150, 115, 82, 37), maxColorValue = 255), 
                          rgb(c(255, 240, 217, 189, 150, 115, 82, 37, 0), c(255, 
                                                                            240, 217, 189, 150, 115, 82, 37, 0), c(255, 240, 
                                                                                                                   217, 189, 150, 115, 82, 37, 0), maxColorValue = 255)), 
           Oranges = switch(n - 2, rgb(c(254, 253, 230), c(230, 
                                                           174, 85), c(206, 107, 13), maxColorValue = 255), 
                            rgb(c(254, 253, 253, 217), c(237, 190, 141, 71), 
                                c(222, 133, 60, 1), maxColorValue = 255), rgb(c(254, 
                                                                                253, 253, 230, 166), c(237, 190, 141, 85, 54), 
                                                                              c(222, 133, 60, 13, 3), maxColorValue = 255), 
                            rgb(c(254, 253, 253, 253, 230, 166), c(237, 208, 
                                                                   174, 141, 85, 54), c(222, 162, 107, 60, 13, 3), 
                                maxColorValue = 255), rgb(c(254, 253, 253, 253, 
                                                            241, 217, 140), c(237, 208, 174, 141, 105, 72, 
                                                                              45), c(222, 162, 107, 60, 19, 1, 4), maxColorValue = 255), 
                            rgb(c(255, 254, 253, 253, 253, 241, 217, 140), c(245, 
                                                                             230, 208, 174, 141, 105, 72, 45), c(235, 206, 
                                                                                                                 162, 107, 60, 19, 1, 4), maxColorValue = 255), 
                            rgb(c(255, 254, 253, 253, 253, 241, 217, 166, 127), 
                                c(245, 230, 208, 174, 141, 105, 72, 54, 39), 
                                c(235, 206, 162, 107, 60, 19, 1, 3, 4), maxColorValue = 255)), 
           OrRd = switch(n - 2, rgb(c(254, 253, 227), c(232, 187, 
                                                        74), c(200, 132, 51), maxColorValue = 255), rgb(c(254, 
                                                                                                          253, 252, 215), c(240, 204, 141, 48), c(217, 138, 
                                                                                                                                                  89, 31), maxColorValue = 255), rgb(c(254, 253, 252, 
                                                                                                                                                                                       227, 179), c(240, 204, 141, 74, 0), c(217, 138, 89, 
                                                                                                                                                                                                                             51, 0), maxColorValue = 255), rgb(c(254, 253, 253, 
                                                                                                                                                                                                                                                                 252, 227, 179), c(240, 212, 187, 141, 74, 0), c(217, 
                                                                                                                                                                                                                                                                                                                 158, 132, 89, 51, 0), maxColorValue = 255), rgb(c(254, 
                                                                                                                                                                                                                                                                                                                                                                   253, 253, 252, 239, 215, 153), c(240, 212, 187, 141, 
                                                                                                                                                                                                                                                                                                                                                                                                    101, 48, 0), c(217, 158, 132, 89, 72, 31, 0), maxColorValue = 255), 
                         rgb(c(255, 254, 253, 253, 252, 239, 215, 153), c(247, 
                                                                          232, 212, 187, 141, 101, 48, 0), c(236, 200, 
                                                                                                             158, 132, 89, 72, 31, 0), maxColorValue = 255), 
                         rgb(c(255, 254, 253, 253, 252, 239, 215, 179, 127), 
                             c(247, 232, 212, 187, 141, 101, 48, 0, 0), c(236, 
                                                                          200, 158, 132, 89, 72, 31, 0, 0), maxColorValue = 255)), 
           Paired = switch(n - 2, rgb(c(166, 31, 178), c(206, 120, 
                                                         223), c(227, 180, 138), maxColorValue = 255), rgb(c(166, 
                                                                                                             31, 178, 51), c(206, 120, 223, 160), c(227, 180, 
                                                                                                                                                    138, 44), maxColorValue = 255), rgb(c(166, 31, 178, 
                                                                                                                                                                                          51, 251), c(206, 120, 223, 160, 154), c(227, 180, 
                                                                                                                                                                                                                                  138, 44, 153), maxColorValue = 255), rgb(c(166, 31, 
                                                                                                                                                                                                                                                                             178, 51, 251, 227), c(206, 120, 223, 160, 154, 26), 
                                                                                                                                                                                                                                                                           c(227, 180, 138, 44, 153, 28), maxColorValue = 255), 
                           rgb(c(166, 31, 178, 51, 251, 227, 253), c(206, 120, 
                                                                     223, 160, 154, 26, 191), c(227, 180, 138, 44, 
                                                                                                153, 28, 111), maxColorValue = 255), rgb(c(166, 
                                                                                                                                           31, 178, 51, 251, 227, 253, 255), c(206, 120, 
                                                                                                                                                                               223, 160, 154, 26, 191, 127), c(227, 180, 138, 
                                                                                                                                                                                                               44, 153, 28, 111, 0), maxColorValue = 255), rgb(c(166, 
                                                                                                                                                                                                                                                                 31, 178, 51, 251, 227, 253, 255, 202), c(206, 
                                                                                                                                                                                                                                                                                                          120, 223, 160, 154, 26, 191, 127, 178), c(227, 
                                                                                                                                                                                                                                                                                                                                                    180, 138, 44, 153, 28, 111, 0, 214), maxColorValue = 255), 
                           rgb(c(166, 31, 178, 51, 251, 227, 253, 255, 202, 
                                 106), c(206, 120, 223, 160, 154, 26, 191, 127, 
                                         178, 61), c(227, 180, 138, 44, 153, 28, 111, 
                                                     0, 214, 154), maxColorValue = 255), rgb(c(166, 
                                                                                               31, 178, 51, 251, 227, 253, 255, 202, 106, 255), 
                                                                                             c(206, 120, 223, 160, 154, 26, 191, 127, 178, 
                                                                                               61, 255), c(227, 180, 138, 44, 153, 28, 111, 
                                                                                                           0, 214, 154, 153), maxColorValue = 255), rgb(c(166, 
                                                                                                                                                          31, 178, 51, 251, 227, 253, 255, 202, 106, 255, 
                                                                                                                                                          177), c(206, 120, 223, 160, 154, 26, 191, 127, 
                                                                                                                                                                  178, 61, 255, 89), c(227, 180, 138, 44, 153, 
                                                                                                                                                                                       28, 111, 0, 214, 154, 153, 40), maxColorValue = 255)), 
           Pastel1 = switch(n - 2, rgb(c(251, 179, 204), c(180, 
                                                           205, 235), c(174, 227, 197), maxColorValue = 255), 
                            rgb(c(251, 179, 204, 222), c(180, 205, 235, 203), 
                                c(174, 227, 197, 228), maxColorValue = 255), 
                            rgb(c(251, 179, 204, 222, 254), c(180, 205, 235, 
                                                              203, 217), c(174, 227, 197, 228, 166), maxColorValue = 255), 
                            rgb(c(251, 179, 204, 222, 254, 255), c(180, 205, 
                                                                   235, 203, 217, 255), c(174, 227, 197, 228, 166, 
                                                                                          204), maxColorValue = 255), rgb(c(251, 179, 204, 
                                                                                                                            222, 254, 255, 229), c(180, 205, 235, 203, 217, 
                                                                                                                                                   255, 216), c(174, 227, 197, 228, 166, 204, 189), 
                                                                                                                          maxColorValue = 255), rgb(c(251, 179, 204, 222, 
                                                                                                                                                      254, 255, 229, 253), c(180, 205, 235, 203, 217, 
                                                                                                                                                                             255, 216, 218), c(174, 227, 197, 228, 166, 204, 
                                                                                                                                                                                               189, 236), maxColorValue = 255), rgb(c(251, 179, 
                                                                                                                                                                                                                                      204, 222, 254, 255, 229, 253, 242), c(180, 205, 
                                                                                                                                                                                                                                                                            235, 203, 217, 255, 216, 218, 242), c(174, 227, 
                                                                                                                                                                                                                                                                                                                  197, 228, 166, 204, 189, 236, 242), maxColorValue = 255)), 
           Pastel2 = switch(n - 2, rgb(c(179, 253, 203), c(226, 
                                                           205, 213), c(205, 172, 232), maxColorValue = 255), 
                            rgb(c(179, 253, 203, 244), c(226, 205, 213, 202), 
                                c(205, 172, 232, 228), maxColorValue = 255), 
                            rgb(c(179, 253, 203, 244, 230), c(226, 205, 213, 
                                                              202, 245), c(205, 172, 232, 228, 201), maxColorValue = 255), 
                            rgb(c(179, 253, 203, 244, 230, 255), c(226, 205, 
                                                                   213, 202, 245, 242), c(205, 172, 232, 228, 201, 
                                                                                          174), maxColorValue = 255), rgb(c(179, 253, 203, 
                                                                                                                            244, 230, 255, 241), c(226, 205, 213, 202, 245, 
                                                                                                                                                   242, 226), c(205, 172, 232, 228, 201, 174, 204), 
                                                                                                                          maxColorValue = 255), rgb(c(179, 253, 203, 244, 
                                                                                                                                                      230, 255, 241, 204), c(226, 205, 213, 202, 245, 
                                                                                                                                                                             242, 226, 204), c(205, 172, 232, 228, 201, 174, 
                                                                                                                                                                                               204, 204), maxColorValue = 255)), PiYG = switch(n - 
                                                                                                                                                                                                                                                 2, rgb(c(233, 247, 161), c(163, 247, 215), c(201, 
                                                                                                                                                                                                                                                                                              247, 106), maxColorValue = 255), rgb(c(208, 241, 
                                                                                                                                                                                                                                                                                                                                     184, 77), c(28, 182, 225, 172), c(139, 218, 134, 
                                                                                                                                                                                                                                                                                                                                                                       38), maxColorValue = 255), rgb(c(208, 241, 247, 184, 
                                                                                                                                                                                                                                                                                                                                                                                                        77), c(28, 182, 247, 225, 172), c(139, 218, 247, 
                                                                                                                                                                                                                                                                                                                                                                                                                                          134, 38), maxColorValue = 255), rgb(c(197, 233, 253, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                230, 161, 77), c(27, 163, 224, 245, 215, 146), c(125, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 201, 239, 208, 106, 33), maxColorValue = 255), rgb(c(197, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      233, 253, 247, 230, 161, 77), c(27, 163, 224, 247, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      245, 215, 146), c(125, 201, 239, 247, 208, 106, 33), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    maxColorValue = 255), rgb(c(197, 222, 241, 253, 230, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                184, 127, 77), c(27, 119, 182, 224, 245, 225, 188, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 146), c(125, 174, 218, 239, 208, 134, 65, 33), maxColorValue = 255), 
                                                                                                                                                                                                                                               rgb(c(197, 222, 241, 253, 247, 230, 184, 127, 77), 
                                                                                                                                                                                                                                                   c(27, 119, 182, 224, 247, 245, 225, 188, 146), 
                                                                                                                                                                                                                                                   c(125, 174, 218, 239, 247, 208, 134, 65, 33), 
                                                                                                                                                                                                                                                   maxColorValue = 255), rgb(c(142, 197, 222, 241, 
                                                                                                                                                                                                                                                                               253, 230, 184, 127, 77, 39), c(1, 27, 119, 182, 
                                                                                                                                                                                                                                                                                                              224, 245, 225, 188, 146, 100), c(82, 125, 174, 
                                                                                                                                                                                                                                                                                                                                               218, 239, 208, 134, 65, 33, 25), maxColorValue = 255), 
                                                                                                                                                                                                                                               rgb(c(142, 197, 222, 241, 253, 247, 230, 184, 127, 
                                                                                                                                                                                                                                                     77, 39), c(1, 27, 119, 182, 224, 247, 245, 225, 
                                                                                                                                                                                                                                                                188, 146, 100), c(82, 125, 174, 218, 239, 247, 
                                                                                                                                                                                                                                                                                  208, 134, 65, 33, 25), maxColorValue = 255)), 
           PRGn = switch(n - 2, rgb(c(175, 247, 127), c(141, 247, 
                                                        191), c(195, 247, 123), maxColorValue = 255), rgb(c(123, 
                                                                                                            194, 166, 0), c(50, 165, 219, 136), c(148, 207, 160, 
                                                                                                                                                  55), maxColorValue = 255), rgb(c(123, 194, 247, 166, 
                                                                                                                                                                                   0), c(50, 165, 247, 219, 136), c(148, 207, 247, 160, 
                                                                                                                                                                                                                    55), maxColorValue = 255), rgb(c(118, 175, 231, 217, 
                                                                                                                                                                                                                                                     127, 27), c(42, 141, 212, 240, 191, 120), c(131, 
                                                                                                                                                                                                                                                                                                 195, 232, 211, 123, 55), maxColorValue = 255), rgb(c(118, 
                                                                                                                                                                                                                                                                                                                                                      175, 231, 247, 217, 127, 27), c(42, 141, 212, 247, 
                                                                                                                                                                                                                                                                                                                                                                                      240, 191, 120), c(131, 195, 232, 247, 211, 123, 55), 
                                                                                                                                                                                                                                                                                                                                                    maxColorValue = 255), rgb(c(118, 153, 194, 231, 217, 
                                                                                                                                                                                                                                                                                                                                                                                166, 90, 27), c(42, 112, 165, 212, 240, 219, 174, 
                                                                                                                                                                                                                                                                                                                                                                                                120), c(131, 171, 207, 232, 211, 160, 97, 55), maxColorValue = 255), 
                         rgb(c(118, 153, 194, 231, 247, 217, 166, 90, 27), 
                             c(42, 112, 165, 212, 247, 240, 219, 174, 120), 
                             c(131, 171, 207, 232, 247, 211, 160, 97, 55), 
                             maxColorValue = 255), rgb(c(64, 118, 153, 194, 
                                                         231, 217, 166, 90, 27, 0), c(0, 42, 112, 165, 
                                                                                      212, 240, 219, 174, 120, 68), c(75, 131, 171, 
                                                                                                                      207, 232, 211, 160, 97, 55, 27), maxColorValue = 255), 
                         rgb(c(64, 118, 153, 194, 231, 247, 217, 166, 90, 
                               27, 0), c(0, 42, 112, 165, 212, 247, 240, 219, 
                                         174, 120, 68), c(75, 131, 171, 207, 232, 247, 
                                                          211, 160, 97, 55, 27), maxColorValue = 255)), 
           PuBu = switch(n - 2, rgb(c(236, 166, 43), c(231, 189, 
                                                       140), c(242, 219, 190), maxColorValue = 255), rgb(c(241, 
                                                                                                           189, 116, 5), c(238, 201, 169, 112), c(246, 225, 
                                                                                                                                                  207, 176), maxColorValue = 255), rgb(c(241, 189, 
                                                                                                                                                                                         116, 43, 4), c(238, 201, 169, 140, 90), c(246, 225, 
                                                                                                                                                                                                                                   207, 190, 141), maxColorValue = 255), rgb(c(241, 
                                                                                                                                                                                                                                                                               208, 166, 116, 43, 4), c(238, 209, 189, 169, 140, 
                                                                                                                                                                                                                                                                                                        90), c(246, 230, 219, 207, 190, 141), maxColorValue = 255), 
                         rgb(c(241, 208, 166, 116, 54, 5, 3), c(238, 209, 
                                                                189, 169, 144, 112, 78), c(246, 230, 219, 207, 
                                                                                           192, 176, 123), maxColorValue = 255), rgb(c(255, 
                                                                                                                                       236, 208, 166, 116, 54, 5, 3), c(247, 231, 209, 
                                                                                                                                                                        189, 169, 144, 112, 78), c(251, 242, 230, 219, 
                                                                                                                                                                                                   207, 192, 176, 123), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                    236, 208, 166, 116, 54, 5, 4, 2), c(247, 231, 
                                                                                                                                                                                                                                                                                        209, 189, 169, 144, 112, 90, 56), c(251, 242, 
                                                                                                                                                                                                                                                                                                                            230, 219, 207, 192, 176, 141, 88), maxColorValue = 255)), 
           PuBuGn = switch(n - 2, rgb(c(236, 166, 28), c(226, 189, 
                                                         144), c(240, 219, 153), maxColorValue = 255), rgb(c(246, 
                                                                                                             189, 103, 2), c(239, 201, 169, 129), c(247, 225, 
                                                                                                                                                    207, 138), maxColorValue = 255), rgb(c(246, 189, 
                                                                                                                                                                                           103, 28, 1), c(239, 201, 169, 144, 108), c(247, 225, 
                                                                                                                                                                                                                                      207, 153, 89), maxColorValue = 255), rgb(c(246, 208, 
                                                                                                                                                                                                                                                                                 166, 103, 28, 1), c(239, 209, 189, 169, 144, 108), 
                                                                                                                                                                                                                                                                               c(247, 230, 219, 207, 153, 89), maxColorValue = 255), 
                           rgb(c(246, 208, 166, 103, 54, 2, 1), c(239, 209, 
                                                                  189, 169, 144, 129, 100), c(247, 230, 219, 207, 
                                                                                              192, 138, 80), maxColorValue = 255), rgb(c(255, 
                                                                                                                                         236, 208, 166, 103, 54, 2, 1), c(247, 226, 209, 
                                                                                                                                                                          189, 169, 144, 129, 100), c(251, 240, 230, 219, 
                                                                                                                                                                                                      207, 192, 138, 80), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                      236, 208, 166, 103, 54, 2, 1, 1), c(247, 226, 
                                                                                                                                                                                                                                                                                          209, 189, 169, 144, 129, 108, 70), c(251, 240, 
                                                                                                                                                                                                                                                                                                                               230, 219, 207, 192, 138, 89, 54), maxColorValue = 255)), 
           PuOr = switch(n - 2, rgb(c(241, 247, 153), c(163, 247, 
                                                        142), c(64, 247, 195), maxColorValue = 255), rgb(c(230, 
                                                                                                           253, 178, 94), c(97, 184, 171, 60), c(1, 99, 210, 
                                                                                                                                                 153), maxColorValue = 255), rgb(c(230, 253, 247, 
                                                                                                                                                                                   178, 94), c(97, 184, 247, 171, 60), c(1, 99, 247, 
                                                                                                                                                                                                                         210, 153), maxColorValue = 255), rgb(c(179, 241, 
                                                                                                                                                                                                                                                                254, 216, 153, 84), c(88, 163, 224, 218, 142, 39), 
                                                                                                                                                                                                                                                              c(6, 64, 182, 235, 195, 136), maxColorValue = 255), 
                         rgb(c(179, 241, 254, 247, 216, 153, 84), c(88, 163, 
                                                                    224, 247, 218, 142, 39), c(6, 64, 182, 247, 235, 
                                                                                               195, 136), maxColorValue = 255), rgb(c(179, 224, 
                                                                                                                                      253, 254, 216, 178, 128, 84), c(88, 130, 184, 
                                                                                                                                                                      224, 218, 171, 115, 39), c(6, 20, 99, 182, 235, 
                                                                                                                                                                                                 210, 172, 136), maxColorValue = 255), rgb(c(179, 
                                                                                                                                                                                                                                             224, 253, 254, 247, 216, 178, 128, 84), c(88, 
                                                                                                                                                                                                                                                                                       130, 184, 224, 247, 218, 171, 115, 39), c(6, 
                                                                                                                                                                                                                                                                                                                                 20, 99, 182, 247, 235, 210, 172, 136), maxColorValue = 255), 
                         rgb(c(127, 179, 224, 253, 254, 216, 178, 128, 84, 
                               45), c(59, 88, 130, 184, 224, 218, 171, 115, 
                                      39, 0), c(8, 6, 20, 99, 182, 235, 210, 172, 136, 
                                                75), maxColorValue = 255), rgb(c(127, 179, 224, 
                                                                                 253, 254, 247, 216, 178, 128, 84, 45), c(59, 
                                                                                                                          88, 130, 184, 224, 247, 218, 171, 115, 39, 0), 
                                                                               c(8, 6, 20, 99, 182, 247, 235, 210, 172, 136, 
                                                                                 75), maxColorValue = 255)), PuRd = switch(n - 
                                                                                                                             2, rgb(c(231, 201, 221), c(225, 148, 28), c(239, 
                                                                                                                                                                         199, 119), maxColorValue = 255), rgb(c(241, 215, 
                                                                                                                                                                                                                223, 206), c(238, 181, 101, 18), c(246, 216, 176, 
                                                                                                                                                                                                                                                   86), maxColorValue = 255), rgb(c(241, 215, 223, 221, 
                                                                                                                                                                                                                                                                                    152), c(238, 181, 101, 28, 0), c(246, 216, 176, 119, 
                                                                                                                                                                                                                                                                                                                     67), maxColorValue = 255), rgb(c(241, 212, 201, 223, 
                                                                                                                                                                                                                                                                                                                                                      221, 152), c(238, 185, 148, 101, 28, 0), c(246, 218, 
                                                                                                                                                                                                                                                                                                                                                                                                 199, 176, 119, 67), maxColorValue = 255), rgb(c(241, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                 212, 201, 223, 231, 206, 145), c(238, 185, 148, 101, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  41, 18, 0), c(246, 218, 199, 176, 138, 86, 63), maxColorValue = 255), 
                                                                                                                           rgb(c(247, 231, 212, 201, 223, 231, 206, 145), c(244, 
                                                                                                                                                                            225, 185, 148, 101, 41, 18, 0), c(249, 239, 218, 
                                                                                                                                                                                                              199, 176, 138, 86, 63), maxColorValue = 255), 
                                                                                                                           rgb(c(247, 231, 212, 201, 223, 231, 206, 152, 103), 
                                                                                                                               c(244, 225, 185, 148, 101, 41, 18, 0, 0), c(249, 
                                                                                                                                                                           239, 218, 199, 176, 138, 86, 67, 31), maxColorValue = 255)), 
           Purples = switch(n - 2, rgb(c(239, 188, 117), c(237, 
                                                           189, 107), c(245, 220, 177), maxColorValue = 255), 
                            rgb(c(242, 203, 158, 106), c(240, 201, 154, 81), 
                                c(247, 226, 200, 163), maxColorValue = 255), 
                            rgb(c(242, 203, 158, 117, 84), c(240, 201, 154, 107, 
                                                             39), c(247, 226, 200, 177, 143), maxColorValue = 255), 
                            rgb(c(242, 218, 188, 158, 117, 84), c(240, 218, 189, 
                                                                  154, 107, 39), c(247, 235, 220, 200, 177, 143), 
                                maxColorValue = 255), rgb(c(242, 218, 188, 158, 
                                                            128, 106, 74), c(240, 218, 189, 154, 125, 81, 
                                                                             20), c(247, 235, 220, 200, 186, 163, 134), maxColorValue = 255), 
                            rgb(c(252, 239, 218, 188, 158, 128, 106, 74), c(251, 
                                                                            237, 218, 189, 154, 125, 81, 20), c(253, 245, 
                                                                                                                235, 220, 200, 186, 163, 134), maxColorValue = 255), 
                            rgb(c(252, 239, 218, 188, 158, 128, 106, 84, 63), 
                                c(251, 237, 218, 189, 154, 125, 81, 39, 0), c(253, 
                                                                              245, 235, 220, 200, 186, 163, 143, 125), maxColorValue = 255)), 
           RdBu = switch(n - 2, rgb(c(239, 247, 103), c(138, 247, 
                                                        169), c(98, 247, 207), maxColorValue = 255), rgb(c(202, 
                                                                                                           244, 146, 5), c(0, 165, 197, 113), c(32, 130, 222, 
                                                                                                                                                176), maxColorValue = 255), rgb(c(202, 244, 247, 
                                                                                                                                                                                  146, 5), c(0, 165, 247, 197, 113), c(32, 130, 247, 
                                                                                                                                                                                                                       222, 176), maxColorValue = 255), rgb(c(178, 239, 
                                                                                                                                                                                                                                                              253, 209, 103, 33), c(24, 138, 219, 229, 169, 102), 
                                                                                                                                                                                                                                                            c(43, 98, 199, 240, 207, 172), maxColorValue = 255), 
                         rgb(c(178, 239, 253, 247, 209, 103, 33), c(24, 138, 
                                                                    219, 247, 229, 169, 102), c(43, 98, 199, 247, 
                                                                                                240, 207, 172), maxColorValue = 255), rgb(c(178, 
                                                                                                                                            214, 244, 253, 209, 146, 67, 33), c(24, 96, 165, 
                                                                                                                                                                                219, 229, 197, 147, 102), c(43, 77, 130, 199, 
                                                                                                                                                                                                            240, 222, 195, 172), maxColorValue = 255), rgb(c(178, 
                                                                                                                                                                                                                                                             214, 244, 253, 247, 209, 146, 67, 33), c(24, 
                                                                                                                                                                                                                                                                                                      96, 165, 219, 247, 229, 197, 147, 102), c(43, 
                                                                                                                                                                                                                                                                                                                                                77, 130, 199, 247, 240, 222, 195, 172), maxColorValue = 255), 
                         rgb(c(103, 178, 214, 244, 253, 209, 146, 67, 33, 
                               5), c(0, 24, 96, 165, 219, 229, 197, 147, 102, 
                                     48), c(31, 43, 77, 130, 199, 240, 222, 195, 172, 
                                            97), maxColorValue = 255), rgb(c(103, 178, 214, 
                                                                             244, 253, 247, 209, 146, 67, 33, 5), c(0, 24, 
                                                                                                                    96, 165, 219, 247, 229, 197, 147, 102, 48), c(31, 
                                                                                                                                                                  43, 77, 130, 199, 247, 240, 222, 195, 172, 97), 
                                                                           maxColorValue = 255)), RdGy = switch(n - 2, rgb(c(239, 
                                                                                                                             255, 153), c(138, 255, 153), c(98, 255, 153), maxColorValue = 255), 
                                                                                                                rgb(c(202, 244, 186, 64), c(0, 165, 186, 64), c(32, 
                                                                                                                                                                130, 186, 64), maxColorValue = 255), rgb(c(202, 
                                                                                                                                                                                                           244, 255, 186, 64), c(0, 165, 255, 186, 64), 
                                                                                                                                                                                                         c(32, 130, 255, 186, 64), maxColorValue = 255), 
                                                                                                                rgb(c(178, 239, 253, 224, 153, 77), c(24, 138, 219, 
                                                                                                                                                      224, 153, 77), c(43, 98, 199, 224, 153, 77), 
                                                                                                                    maxColorValue = 255), rgb(c(178, 239, 253, 255, 
                                                                                                                                                224, 153, 77), c(24, 138, 219, 255, 224, 153, 
                                                                                                                                                                 77), c(43, 98, 199, 255, 224, 153, 77), maxColorValue = 255), 
                                                                                                                rgb(c(178, 214, 244, 253, 224, 186, 135, 77), c(24, 
                                                                                                                                                                96, 165, 219, 224, 186, 135, 77), c(43, 77, 130, 
                                                                                                                                                                                                    199, 224, 186, 135, 77), maxColorValue = 255), 
                                                                                                                rgb(c(178, 214, 244, 253, 255, 224, 186, 135, 77), 
                                                                                                                    c(24, 96, 165, 219, 255, 224, 186, 135, 77), 
                                                                                                                    c(43, 77, 130, 199, 255, 224, 186, 135, 77), 
                                                                                                                    maxColorValue = 255), rgb(c(103, 178, 214, 244, 
                                                                                                                                                253, 224, 186, 135, 77, 26), c(0, 24, 96, 165, 
                                                                                                                                                                               219, 224, 186, 135, 77, 26), c(31, 43, 77, 130, 
                                                                                                                                                                                                              199, 224, 186, 135, 77, 26), maxColorValue = 255), 
                                                                                                                rgb(c(103, 178, 214, 244, 253, 255, 224, 186, 135, 
                                                                                                                      77, 26), c(0, 24, 96, 165, 219, 255, 224, 186, 
                                                                                                                                 135, 77, 26), c(31, 43, 77, 130, 199, 255, 224, 
                                                                                                                                                 186, 135, 77, 26), maxColorValue = 255)), RdPu = switch(n - 
                                                                                                                                                                                                           2, rgb(c(253, 250, 197), c(224, 159, 27), c(221, 
                                                                                                                                                                                                                                                       181, 138), maxColorValue = 255), rgb(c(254, 251, 
                                                                                                                                                                                                                                                                                              247, 174), c(235, 180, 104, 1), c(226, 185, 161, 
                                                                                                                                                                                                                                                                                                                                126), maxColorValue = 255), rgb(c(254, 251, 247, 
                                                                                                                                                                                                                                                                                                                                                                  197, 122), c(235, 180, 104, 27, 1), c(226, 185, 161, 
                                                                                                                                                                                                                                                                                                                                                                                                        138, 119), maxColorValue = 255), rgb(c(254, 252, 
                                                                                                                                                                                                                                                                                                                                                                                                                                               250, 247, 197, 122), c(235, 197, 159, 104, 27, 1), 
                                                                                                                                                                                                                                                                                                                                                                                                                                             c(226, 192, 181, 161, 138, 119), maxColorValue = 255), 
                                                                                                                                                                                                         rgb(c(254, 252, 250, 247, 221, 174, 122), c(235, 
                                                                                                                                                                                                                                                     197, 159, 104, 52, 1, 1), c(226, 192, 181, 161, 
                                                                                                                                                                                                                                                                                 151, 126, 119), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                                                                                             253, 252, 250, 247, 221, 174, 122), c(247, 224, 
                                                                                                                                                                                                                                                                                                                                                                   197, 159, 104, 52, 1, 1), c(243, 221, 192, 181, 
                                                                                                                                                                                                                                                                                                                                                                                               161, 151, 126, 119), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                253, 252, 250, 247, 221, 174, 122, 73), c(247, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          224, 197, 159, 104, 52, 1, 1, 0), c(243, 221, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              192, 181, 161, 151, 126, 119, 106), maxColorValue = 255)), 
           Reds = switch(n - 2, rgb(c(254, 252, 222), c(224, 146, 
                                                        45), c(210, 114, 38), maxColorValue = 255), rgb(c(254, 
                                                                                                          252, 251, 203), c(229, 174, 106, 24), c(217, 145, 
                                                                                                                                                  74, 29), maxColorValue = 255), rgb(c(254, 252, 251, 
                                                                                                                                                                                       222, 165), c(229, 174, 106, 45, 15), c(217, 145, 
                                                                                                                                                                                                                              74, 38, 21), maxColorValue = 255), rgb(c(254, 252, 
                                                                                                                                                                                                                                                                       252, 251, 222, 165), c(229, 187, 146, 106, 45, 15), 
                                                                                                                                                                                                                                                                     c(217, 161, 114, 74, 38, 21), maxColorValue = 255), 
                         rgb(c(254, 252, 252, 251, 239, 203, 153), c(229, 
                                                                     187, 146, 106, 59, 24, 0), c(217, 161, 114, 74, 
                                                                                                  44, 29, 13), maxColorValue = 255), rgb(c(255, 
                                                                                                                                           254, 252, 252, 251, 239, 203, 153), c(245, 224, 
                                                                                                                                                                                 187, 146, 106, 59, 24, 0), c(240, 210, 161, 114, 
                                                                                                                                                                                                              74, 44, 29, 13), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                           254, 252, 252, 251, 239, 203, 165, 103), c(245, 
                                                                                                                                                                                                                                                                                                      224, 187, 146, 106, 59, 24, 15, 0), c(240, 210, 
                                                                                                                                                                                                                                                                                                                                            161, 114, 74, 44, 29, 21, 13), maxColorValue = 255)), 
           RdYlBu = switch(n - 2, rgb(c(252, 255, 145), c(141, 255, 
                                                          191), c(89, 191, 219), maxColorValue = 255), rgb(c(215, 
                                                                                                             253, 171, 44), c(25, 174, 217, 123), c(28, 97, 233, 
                                                                                                                                                    182), maxColorValue = 255), rgb(c(215, 253, 255, 
                                                                                                                                                                                      171, 44), c(25, 174, 255, 217, 123), c(28, 97, 191, 
                                                                                                                                                                                                                             233, 182), maxColorValue = 255), rgb(c(215, 252, 
                                                                                                                                                                                                                                                                    254, 224, 145, 69), c(48, 141, 224, 243, 191, 117), 
                                                                                                                                                                                                                                                                  c(39, 89, 144, 248, 219, 180), maxColorValue = 255), 
                           rgb(c(215, 252, 254, 255, 224, 145, 69), c(48, 141, 
                                                                      224, 255, 243, 191, 117), c(39, 89, 144, 191, 
                                                                                                  248, 219, 180), maxColorValue = 255), rgb(c(215, 
                                                                                                                                              244, 253, 254, 224, 171, 116, 69), c(48, 109, 
                                                                                                                                                                                   174, 224, 243, 217, 173, 117), c(39, 67, 97, 
                                                                                                                                                                                                                    144, 248, 233, 209, 180), maxColorValue = 255), 
                           rgb(c(215, 244, 253, 254, 255, 224, 171, 116, 69), 
                               c(48, 109, 174, 224, 255, 243, 217, 173, 117), 
                               c(39, 67, 97, 144, 191, 248, 233, 209, 180), 
                               maxColorValue = 255), rgb(c(165, 215, 244, 253, 
                                                           254, 224, 171, 116, 69, 49), c(0, 48, 109, 174, 
                                                                                          224, 243, 217, 173, 117, 54), c(38, 39, 67, 97, 
                                                                                                                          144, 248, 233, 209, 180, 149), maxColorValue = 255), 
                           rgb(c(165, 215, 244, 253, 254, 255, 224, 171, 116, 
                                 69, 49), c(0, 48, 109, 174, 224, 255, 243, 217, 
                                            173, 117, 54), c(38, 39, 67, 97, 144, 191, 248, 
                                                             233, 209, 180, 149), maxColorValue = 255)), RdYlGn = switch(n - 
                                                                                                                           2, rgb(c(252, 255, 145), c(141, 255, 207), c(89, 
                                                                                                                                                                        191, 96), maxColorValue = 255), rgb(c(215, 253, 166, 
                                                                                                                                                                                                              26), c(25, 174, 217, 150), c(28, 97, 106, 65), maxColorValue = 255), 
                                                                                                                         rgb(c(215, 253, 255, 166, 26), c(25, 174, 255, 217, 
                                                                                                                                                          150), c(28, 97, 191, 106, 65), maxColorValue = 255), 
                                                                                                                         rgb(c(215, 252, 254, 217, 145, 26), c(48, 141, 224, 
                                                                                                                                                               239, 207, 152), c(39, 89, 139, 139, 96, 80), 
                                                                                                                             maxColorValue = 255), rgb(c(215, 252, 254, 255, 
                                                                                                                                                         217, 145, 26), c(48, 141, 224, 255, 239, 207, 
                                                                                                                                                                          152), c(39, 89, 139, 191, 139, 96, 80), maxColorValue = 255), 
                                                                                                                         rgb(c(215, 244, 253, 254, 217, 166, 102, 26), c(48, 
                                                                                                                                                                         109, 174, 224, 239, 217, 189, 152), c(39, 67, 
                                                                                                                                                                                                               97, 139, 139, 106, 99, 80), maxColorValue = 255), 
                                                                                                                         rgb(c(215, 244, 253, 254, 255, 217, 166, 102, 26), 
                                                                                                                             c(48, 109, 174, 224, 255, 239, 217, 189, 152), 
                                                                                                                             c(39, 67, 97, 139, 191, 139, 106, 99, 80), maxColorValue = 255), 
                                                                                                                         rgb(c(165, 215, 244, 253, 254, 217, 166, 102, 26, 
                                                                                                                               0), c(0, 48, 109, 174, 224, 239, 217, 189, 152, 
                                                                                                                                     104), c(38, 39, 67, 97, 139, 139, 106, 99, 80, 
                                                                                                                                             55), maxColorValue = 255), rgb(c(165, 215, 244, 
                                                                                                                                                                              253, 254, 255, 217, 166, 102, 26, 0), c(0, 48, 
                                                                                                                                                                                                                      109, 174, 224, 255, 239, 217, 189, 152, 104), 
                                                                                                                                                                            c(38, 39, 67, 97, 139, 191, 139, 106, 99, 80, 
                                                                                                                                                                              55), maxColorValue = 255)), Set1 = switch(n - 
                                                                                                                                                                                                                          2, rgb(c(228, 55, 77), c(26, 126, 175), c(28, 184, 
                                                                                                                                                                                                                                                                    74), maxColorValue = 255), rgb(c(228, 55, 77, 152), 
                                                                                                                                                                                                                                                                                                   c(26, 126, 175, 78), c(28, 184, 74, 163), maxColorValue = 255), 
                                                                                                                                                                                                                        rgb(c(228, 55, 77, 152, 255), c(26, 126, 175, 78, 
                                                                                                                                                                                                                                                        127), c(28, 184, 74, 163, 0), maxColorValue = 255), 
                                                                                                                                                                                                                        rgb(c(228, 55, 77, 152, 255, 255), c(26, 126, 175, 
                                                                                                                                                                                                                                                             78, 127, 255), c(28, 184, 74, 163, 0, 51), maxColorValue = 255), 
                                                                                                                                                                                                                        rgb(c(228, 55, 77, 152, 255, 255, 166), c(26, 126, 
                                                                                                                                                                                                                                                                  175, 78, 127, 255, 86), c(28, 184, 74, 163, 0, 
                                                                                                                                                                                                                                                                                            51, 40), maxColorValue = 255), rgb(c(228, 55, 
                                                                                                                                                                                                                                                                                                                                 77, 152, 255, 255, 166, 247), c(26, 126, 175, 
                                                                                                                                                                                                                                                                                                                                                                 78, 127, 255, 86, 129), c(28, 184, 74, 163, 0, 
                                                                                                                                                                                                                                                                                                                                                                                           51, 40, 191), maxColorValue = 255), rgb(c(228, 
                                                                                                                                                                                                                                                                                                                                                                                                                                     55, 77, 152, 255, 255, 166, 247, 153), c(26, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              126, 175, 78, 127, 255, 86, 129, 153), c(28, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       184, 74, 163, 0, 51, 40, 191, 153), maxColorValue = 255)), 
           Set2 = switch(n - 2, rgb(c(102, 252, 141), c(194, 141, 
                                                        160), c(165, 98, 203), maxColorValue = 255), rgb(c(102, 
                                                                                                           252, 141, 231), c(194, 141, 160, 138), c(165, 98, 
                                                                                                                                                    203, 195), maxColorValue = 255), rgb(c(102, 252, 
                                                                                                                                                                                           141, 231, 166), c(194, 141, 160, 138, 216), c(165, 
                                                                                                                                                                                                                                         98, 203, 195, 84), maxColorValue = 255), rgb(c(102, 
                                                                                                                                                                                                                                                                                        252, 141, 231, 166, 255), c(194, 141, 160, 138, 216, 
                                                                                                                                                                                                                                                                                                                    217), c(165, 98, 203, 195, 84, 47), maxColorValue = 255), 
                         rgb(c(102, 252, 141, 231, 166, 255, 229), c(194, 
                                                                     141, 160, 138, 216, 217, 196), c(165, 98, 203, 
                                                                                                      195, 84, 47, 148), maxColorValue = 255), rgb(c(102, 
                                                                                                                                                     252, 141, 231, 166, 255, 229, 179), c(194, 141, 
                                                                                                                                                                                           160, 138, 216, 217, 196, 179), c(165, 98, 203, 
                                                                                                                                                                                                                            195, 84, 47, 148, 179), maxColorValue = 255)), 
           Set3 = switch(n - 2, rgb(c(141, 255, 190), c(211, 255, 
                                                        186), c(199, 179, 218), maxColorValue = 255), rgb(c(141, 
                                                                                                            255, 190, 251), c(211, 255, 186, 128), c(199, 179, 
                                                                                                                                                     218, 114), maxColorValue = 255), rgb(c(141, 255, 
                                                                                                                                                                                            190, 251, 128), c(211, 255, 186, 128, 177), c(199, 
                                                                                                                                                                                                                                          179, 218, 114, 211), maxColorValue = 255), rgb(c(141, 
                                                                                                                                                                                                                                                                                           255, 190, 251, 128, 253), c(211, 255, 186, 128, 177, 
                                                                                                                                                                                                                                                                                                                       180), c(199, 179, 218, 114, 211, 98), maxColorValue = 255), 
                         rgb(c(141, 255, 190, 251, 128, 253, 179), c(211, 
                                                                     255, 186, 128, 177, 180, 222), c(199, 179, 218, 
                                                                                                      114, 211, 98, 105), maxColorValue = 255), rgb(c(141, 
                                                                                                                                                      255, 190, 251, 128, 253, 179, 252), c(211, 255, 
                                                                                                                                                                                            186, 128, 177, 180, 222, 205), c(199, 179, 218, 
                                                                                                                                                                                                                             114, 211, 98, 105, 229), maxColorValue = 255), 
                         rgb(c(141, 255, 190, 251, 128, 253, 179, 252, 217), 
                             c(211, 255, 186, 128, 177, 180, 222, 205, 217), 
                             c(199, 179, 218, 114, 211, 98, 105, 229, 217), 
                             maxColorValue = 255), rgb(c(141, 255, 190, 251, 
                                                         128, 253, 179, 252, 217, 188), c(211, 255, 186, 
                                                                                          128, 177, 180, 222, 205, 217, 128), c(199, 179, 
                                                                                                                                218, 114, 211, 98, 105, 229, 217, 189), maxColorValue = 255), 
                         rgb(c(141, 255, 190, 251, 128, 253, 179, 252, 217, 
                               188, 204), c(211, 255, 186, 128, 177, 180, 222, 
                                            205, 217, 128, 235), c(199, 179, 218, 114, 211, 
                                                                   98, 105, 229, 217, 189, 197), maxColorValue = 255), 
                         rgb(c(141, 255, 190, 251, 128, 253, 179, 252, 217, 
                               188, 204, 255), c(211, 255, 186, 128, 177, 180, 
                                                 222, 205, 217, 128, 235, 237), c(199, 179, 218, 
                                                                                  114, 211, 98, 105, 229, 217, 189, 197, 111), 
                             maxColorValue = 255)), Spectral = switch(n - 
                                                                        2, rgb(c(252, 255, 153), c(141, 255, 213), c(89, 
                                                                                                                     191, 148), maxColorValue = 255), rgb(c(215, 253, 
                                                                                                                                                            171, 43), c(25, 174, 221, 131), c(28, 97, 164, 186), 
                                                                                                                                                          maxColorValue = 255), rgb(c(215, 253, 255, 171, 43), 
                                                                                                                                                                                    c(25, 174, 255, 221, 131), c(28, 97, 191, 164, 186), 
                                                                                                                                                                                    maxColorValue = 255), rgb(c(213, 252, 254, 230, 153, 
                                                                                                                                                                                                                50), c(62, 141, 224, 245, 213, 136), c(79, 89, 139, 
                                                                                                                                                                                                                                                       152, 148, 189), maxColorValue = 255), rgb(c(213, 
                                                                                                                                                                                                                                                                                                   252, 254, 255, 230, 153, 50), c(62, 141, 224, 255, 
                                                                                                                                                                                                                                                                                                                                   245, 213, 136), c(79, 89, 139, 191, 152, 148, 189), 
                                                                                                                                                                                                                                                                                                 maxColorValue = 255), rgb(c(213, 244, 253, 254, 230, 
                                                                                                                                                                                                                                                                                                                             171, 102, 50), c(62, 109, 174, 224, 245, 221, 194, 
                                                                                                                                                                                                                                                                                                                                              136), c(79, 67, 97, 139, 152, 164, 165, 189), maxColorValue = 255), 
                                                                      rgb(c(213, 244, 253, 254, 255, 230, 171, 102, 50), 
                                                                          c(62, 109, 174, 224, 255, 245, 221, 194, 136), 
                                                                          c(79, 67, 97, 139, 191, 152, 164, 165, 189), 
                                                                          maxColorValue = 255), rgb(c(158, 213, 244, 253, 
                                                                                                      254, 230, 171, 102, 50, 94), c(1, 62, 109, 174, 
                                                                                                                                     224, 245, 221, 194, 136, 79), c(66, 79, 67, 97, 
                                                                                                                                                                     139, 152, 164, 165, 189, 162), maxColorValue = 255), 
                                                                      rgb(c(158, 213, 244, 253, 254, 255, 230, 171, 102, 
                                                                            50, 94), c(1, 62, 109, 174, 224, 255, 245, 221, 
                                                                                       194, 136, 79), c(66, 79, 67, 97, 139, 191, 152, 
                                                                                                        164, 165, 189, 162), maxColorValue = 255)), YlGn = switch(n - 
                                                                                                                                                                    2, rgb(c(247, 173, 49), c(252, 221, 163), c(185, 
                                                                                                                                                                                                                142, 84), maxColorValue = 255), rgb(c(255, 194, 120, 
                                                                                                                                                                                                                                                      35), c(255, 230, 198, 132), c(204, 153, 121, 67), 
                                                                                                                                                                                                                                                    maxColorValue = 255), rgb(c(255, 194, 120, 49, 0), 
                                                                                                                                                                                                                                                                              c(255, 230, 198, 163, 104), c(204, 153, 121, 84, 
                                                                                                                                                                                                                                                                                                            55), maxColorValue = 255), rgb(c(255, 217, 173, 
                                                                                                                                                                                                                                                                                                                                             120, 49, 0), c(255, 240, 221, 198, 163, 104), c(204, 
                                                                                                                                                                                                                                                                                                                                                                                             163, 142, 121, 84, 55), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                 217, 173, 120, 65, 35, 0), c(255, 240, 221, 198, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              171, 132, 90), c(204, 163, 142, 121, 93, 67, 50), 
                                                                                                                                                                                                                                                                                                                                                                                                                                               maxColorValue = 255), rgb(c(255, 247, 217, 173, 120, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           65, 35, 0), c(255, 252, 240, 221, 198, 171, 132, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         90), c(229, 185, 163, 142, 121, 93, 67, 50), maxColorValue = 255), 
                                                                                                                                                                  rgb(c(255, 247, 217, 173, 120, 65, 35, 0, 0), c(255, 
                                                                                                                                                                                                                  252, 240, 221, 198, 171, 132, 104, 69), c(229, 
                                                                                                                                                                                                                                                            185, 163, 142, 121, 93, 67, 55, 41), maxColorValue = 255)), 
           YlGnBu = switch(n - 2, rgb(c(237, 127, 44), c(248, 205, 
                                                         127), c(177, 187, 184), maxColorValue = 255), rgb(c(255, 
                                                                                                             161, 65, 34), c(255, 218, 182, 94), c(204, 180, 196, 
                                                                                                                                                   168), maxColorValue = 255), rgb(c(255, 161, 65, 44, 
                                                                                                                                                                                     37), c(255, 218, 182, 127, 52), c(204, 180, 196, 
                                                                                                                                                                                                                       184, 148), maxColorValue = 255), rgb(c(255, 199, 
                                                                                                                                                                                                                                                              127, 65, 44, 37), c(255, 233, 205, 182, 127, 52), 
                                                                                                                                                                                                                                                            c(204, 180, 187, 196, 184, 148), maxColorValue = 255), 
                           rgb(c(255, 199, 127, 65, 29, 34, 12), c(255, 233, 
                                                                   205, 182, 145, 94, 44), c(204, 180, 187, 196, 
                                                                                             192, 168, 132), maxColorValue = 255), rgb(c(255, 
                                                                                                                                         237, 199, 127, 65, 29, 34, 12), c(255, 248, 233, 
                                                                                                                                                                           205, 182, 145, 94, 44), c(217, 177, 180, 187, 
                                                                                                                                                                                                     196, 192, 168, 132), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                      237, 199, 127, 65, 29, 34, 37, 8), c(255, 248, 
                                                                                                                                                                                                                                                                                           233, 205, 182, 145, 94, 52, 29), c(217, 177, 
                                                                                                                                                                                                                                                                                                                              180, 187, 196, 192, 168, 148, 88), maxColorValue = 255)), 
           YlOrBr = switch(n - 2, rgb(c(255, 254, 217), c(247, 196, 
                                                          95), c(188, 79, 14), maxColorValue = 255), rgb(c(255, 
                                                                                                           254, 254, 204), c(255, 217, 153, 76), c(212, 142, 
                                                                                                                                                   41, 2), maxColorValue = 255), rgb(c(255, 254, 254, 
                                                                                                                                                                                       217, 153), c(255, 217, 153, 95, 52), c(212, 142, 
                                                                                                                                                                                                                              41, 14, 4), maxColorValue = 255), rgb(c(255, 254, 
                                                                                                                                                                                                                                                                      254, 254, 217, 153), c(255, 227, 196, 153, 95, 52), 
                                                                                                                                                                                                                                                                    c(212, 145, 79, 41, 14, 4), maxColorValue = 255), 
                           rgb(c(255, 254, 254, 254, 236, 204, 140), c(255, 
                                                                       227, 196, 153, 112, 76, 45), c(212, 145, 79, 
                                                                                                      41, 20, 2, 4), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                 255, 254, 254, 254, 236, 204, 140), c(255, 247, 
                                                                                                                                                                                       227, 196, 153, 112, 76, 45), c(229, 188, 145, 
                                                                                                                                                                                                                      79, 41, 20, 2, 4), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                                     255, 254, 254, 254, 236, 204, 153, 102), c(255, 
                                                                                                                                                                                                                                                                                                                247, 227, 196, 153, 112, 76, 52, 37), c(229, 
                                                                                                                                                                                                                                                                                                                                                        188, 145, 79, 41, 20, 2, 4, 6), maxColorValue = 255)), 
           YlOrRd = switch(n - 2, rgb(c(255, 254, 240), c(237, 178, 
                                                          59), c(160, 76, 32), maxColorValue = 255), rgb(c(255, 
                                                                                                           254, 253, 227), c(255, 204, 141, 26), c(178, 92, 
                                                                                                                                                   60, 28), maxColorValue = 255), rgb(c(255, 254, 253, 
                                                                                                                                                                                        240, 189), c(255, 204, 141, 59, 0), c(178, 92, 60, 
                                                                                                                                                                                                                              32, 38), maxColorValue = 255), rgb(c(255, 254, 254, 
                                                                                                                                                                                                                                                                   253, 240, 189), c(255, 217, 178, 141, 59, 0), c(178, 
                                                                                                                                                                                                                                                                                                                   118, 76, 60, 32, 38), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                                                                                                                                                                                                                                     254, 254, 253, 252, 227, 177), c(255, 217, 178, 141, 
                                                                                                                                                                                                                                                                                                                                                                                                      78, 26, 0), c(178, 118, 76, 60, 42, 28, 38), maxColorValue = 255), 
                           rgb(c(255, 255, 254, 254, 253, 252, 227, 177), c(255, 
                                                                            237, 217, 178, 141, 78, 26, 0), c(204, 160, 118, 
                                                                                                              76, 60, 42, 28, 38), maxColorValue = 255), rgb(c(255, 
                                                                                                                                                               255, 254, 254, 253, 252, 227, 189, 128), c(255, 
                                                                                                                                                                                                          237, 217, 178, 141, 78, 26, 0, 0), c(204, 160, 
                                                                                                                                                                                                                                               118, 76, 60, 42, 28, 38, 38), maxColorValue = 255)))
  }
  
  color_board = c(
    dark_cyan = rgb(20, 78, 100, maxColorValue = 255),
    deep_cyan = rgb(0, 93, 125, maxColorValue = 255),
    light_cyan = rgb(34, 116, 135, maxColorValue = 255),
    green_cyan = rgb(34, 116, 135, maxColorValue = 255),
    shallow_cyan = rgb(60, 140, 190, maxColorValue = 255),
    pale_cyan = rgb(0, 147, 198, maxColorValue = 255),
    dark_red= rgb(181, 0, 0, maxColorValue = 255),
    deep_red  = rgb(154, 42, 42, maxColorValue = 255),
    light_red = rgb(174, 61, 63, maxColorValue = 255),
    shallow_red = rgb(252, 74, 42, maxColorValue = 255),
    pale_red = rgb(220, 63, 56, maxColorValue = 255),
    shallow_red2 = rgb(220, 63, 56, maxColorValue = 255),
    dark_green=  rgb(0, 100, 50, maxColorValue = 255),
    deep_green = rgb(10, 149, 136, maxColorValue = 255),
    light_green =  rgb(84, 169, 84, maxColorValue = 255),
    shallow_green =rgb(66, 192, 46, maxColorValue = 255),
    pale_green = rgb(180, 202, 198, maxColorValue = 255),
    deep_orange = rgb(255, 140, 0, maxColorValue = 255),
    light_orange = rgb(241, 156, 0, maxColorValue = 255),
    shallow_orange = rgb(241, 121, 0, maxColorValue = 255),
    dark_grey = rgb(102, 102, 102, maxColorValue = 255),
    deep_grey = rgb(128, 129, 128, maxColorValue = 255),
    light_grey = rgb(169, 169, 169, maxColorValue = 255),
    shallow_grey = rgb(191, 192, 191, maxColorValue = 255),
    pale_grey = rgb(240, 240, 240, maxColorValue = 255),
    dark_blue = rgb(33, 71, 117, maxColorValue = 255),
    deep_blue = rgb(32, 81, 139, maxColorValue = 255),
    deep_blue = rgb(80, 99, 139, maxColorValue = 255),
    light_blue = rgb(0, 91, 181, maxColorValue = 255),
    shallow_blue = rgb(0, 142, 213, maxColorValue = 255),
    sky_blue = rgb(0, 142, 213, maxColorValue = 255),
    pale_blue = rgb(144, 190, 216, maxColorValue = 255),
    water_blue = rgb(144, 190, 216, maxColorValue = 255),
    dark_purple = rgb(120, 78, 100, maxColorValue = 255),
    deep_purple = rgb(170, 44, 105, maxColorValue = 255),
    light_purple = rgb(212, 137, 168, maxColorValue = 255),
    dark_purple2 = rgb(71, 0, 123, maxColorValue = 255),
    gold_yellow = rgb(255, 215, 0, maxColorValue = 255),
    light_yellow = rgb(207, 177, 81, maxColorValue = 255),
    red_line = rgb(232, 49 ,50 , maxColorValue = 255 ),
    orange_line = rgb(236, 146, 23  , maxColorValue = 255 ),
    green_line = rgb(16, 174, 181 , maxColorValue = 255 ),
    grey_line = rgb(77 ,80, 84  , maxColorValue = 255 )
    ,NISEMURASAKI = rgb(86,46,55 , maxColorValue = 255 )
    ,TETSUKON = rgb(38,30,71 , maxColorValue = 255 )
    ,UMENEZUMI = rgb(158,122,122 , maxColorValue = 255 )
    ,YAMABUKI = rgb(255,177,27 , maxColorValue = 255 )
    ,KON =  rgb(15,37,64 , maxColorValue = 255 )
    ,ENTAN = rgb(215,84, 85 , maxColorValue = 255 )
    ,OITAKE = rgb(106,131, 114 , maxColorValue = 255 )
    ,WASURENAGUSA = rgb(125,185, 222 , maxColorValue = 255 )
    ,MIRUCHA = rgb(98,89,44, maxColorValue = 255)
    ,NAMARI = rgb(120,120,120, maxColorValue = 255)
    ,BOTAN =  rgb(193,50,142, maxColorValue = 255)
    ,BENITOBI = rgb(153,70,57, maxColorValue = 255)
    ,OHDO = rgb(182,142,85, maxColorValue = 255)
    ,YANAGICHA = rgb(147,150,80, maxColorValue = 255)
    ,USUGAKI = rgb(236,184,138, maxColorValue = 255)
    ,SUMIRE = rgb(102,50,124, maxColorValue = 255)
    ,SHIRONEZUMI =  rgb(189,192,186, maxColorValue = 255)
    ,KABACHA = rgb(179,92,55, maxColorValue = 255)
    ,ASAGI  = rgb(51,166,184, maxColorValue = 255)
    ,BENIMIDORI = rgb(123,144,210, maxColorValue = 255)
    ,HIGOSUSUTAKE =  rgb(141,116,42, maxColorValue = 255)
    ,KIMIRUCHA =  rgb(134,120,53, maxColorValue = 255)
    ,AOTAKE = rgb(0,137,108, maxColorValue = 255)
    ,NAMAKABE = rgb(125,108,70, maxColorValue = 255)
    ,WAKATAKE = rgb(93,172,129, maxColorValue = 255)
    ,NAE= rgb(134,193,102, maxColorValue = 255)
    ,BENIHIWADA =  rgb(136,76,58, maxColorValue = 255)
    ,KOHAKU =  rgb(202,122,44, maxColorValue = 255)
    ,TERIGAKI =  rgb(196,98, 67, maxColorValue = 255)
    ,ONANDOCHA=  rgb(70, 93, 76, maxColorValue = 255)
    ,RURI = rgb(0, 92, 175, maxColorValue = 255)
    ,TAIKOH = rgb(248, 195, 205, maxColorValue = 255)
    ,SUMI  = rgb(28, 28, 28, maxColorValue = 255)
    ,ICHIGO =   rgb(181, 73, 91, maxColorValue = 255)
    ,MIZU =  rgb(129, 199, 212, maxColorValue = 255)
    ,CHITOSEMIDORI =  rgb(54, 86, 60, maxColorValue = 255)
    ,TETSUONANDO =  rgb(37, 83, 89, maxColorValue = 255)
    ,IWAICHA =  rgb(100, 106, 88, maxColorValue = 255)
    ,GINNEZUMI =  rgb(145, 152, 159, maxColorValue = 255)
    ,AIKOBICHA =  rgb(75, 78, 42, maxColorValue = 255)
    ,KOGECHA=  rgb(86, 63, 46, maxColorValue = 255)
    ,SABISEIJI =  rgb(134, 166, 151, maxColorValue = 255)
    ,SAKURA=  rgb(254, 223, 225, maxColorValue = 255)
    ,KAMENOZOKI = rgb(165, 222, 228, maxColorValue = 255)
    ,AKAKOH = rgb(227, 145, 110, maxColorValue = 255)
    ,BENIUKON = rgb(233, 139, 42, maxColorValue = 255)
    ,KONJYO = rgb(17, 50, 133, maxColorValue = 255)
    ,FUTAAI = rgb(112, 100, 154, maxColorValue = 255)
    ,TAMAMOROKOSHI =  rgb(232, 182, 71, maxColorValue = 255)
    ,EBICHA =  rgb(115, 67, 56, maxColorValue = 255)
    ,ENJI=  rgb(159, 53, 58, maxColorValue = 255)
    ,AKEBONO =  rgb(241, 148, 131, maxColorValue = 255)
    ,RIKYUSHIRACHA = rgb(180, 165, 130, maxColorValue = 255)
    ,NOSHIMEHANA =  rgb(43, 95, 117, maxColorValue = 255)
    ,UGUISU  =  rgb(108, 106, 45, maxColorValue = 255)
    ,SAKURANEZUMI =  rgb(177, 150, 147, maxColorValue = 255)
    ,KUWAZOME =  rgb(100, 54, 60, maxColorValue = 255)
    ,EDOCHA = rgb(175, 95, 60, maxColorValue = 255)
    ,BAIKOCHA = rgb(137, 145, 107, maxColorValue = 255)
    ,KARACHA  = rgb(180, 113, 87, maxColorValue = 255)
    ,HAIZAKURA = rgb(215, 196, 187, maxColorValue = 255)
    ,SHINBASHI = rgb(0, 137, 167, maxColorValue = 255)
    ,KURIKAWACHA = rgb(106, 64, 40, maxColorValue = 255)
    ,SORA =  rgb(88, 178, 220, maxColorValue = 255)
    ,URAYANAGI = rgb(181, 202, 160, maxColorValue = 255)
    ,TOKIWA = rgb(27, 129, 62, maxColorValue = 255)
    ,SUOH = rgb(142, 53, 74, maxColorValue = 255)
    ,KURENAI = rgb(203, 27, 69, maxColorValue = 255)
    ,AONI = rgb(81, 110, 65, maxColorValue = 255)
    ,KONKIKYO = rgb(33, 30, 85, maxColorValue = 255)
    ,MURASAKITOBI = rgb(96, 55, 62, maxColorValue = 255)
    ,SHIRONERI= rgb(252, 250, 242, maxColorValue = 255)
    ,YANAGISUSUTAKE = rgb(74, 89, 61, maxColorValue = 255)
    ,JINZAMOMI =  rgb(235, 122, 119, maxColorValue = 255)
    ,MESSHI  =  rgb(83, 61, 91, maxColorValue = 255)
    ,NAKABENI =  rgb(219, 77, 109, maxColorValue = 255)
    ,RURIKON = rgb(11, 52, 110, maxColorValue = 255)
    ,AISUMICHA = rgb(55, 60, 56, maxColorValue = 255)
    ,GINSUSUTAKE = rgb(130, 102, 58, maxColorValue = 255)
    ,BUDOHNEZUMI = rgb(94, 61, 80, maxColorValue = 255)
    ,AKE = rgb(204, 84, 58, maxColorValue = 255)
    ,KORAINANDO = rgb(48, 90, 86, maxColorValue = 255)
    ,KIKYO = rgb(106, 76, 156, maxColorValue = 255)
    ,TAMAGO = rgb(249, 191, 69, maxColorValue = 255)
    ,AIMIRUCHA= rgb(15, 76, 58, maxColorValue = 255)
    ,NATANEYU = rgb(162, 140, 55, maxColorValue = 255)
    ,MUSHIAO = rgb(32, 96, 79, maxColorValue = 255)
    ,KOKIAKE = rgb(134, 71, 63, maxColorValue = 255)
    ,HANABA= rgb(247, 194, 66, maxColorValue = 255)
    ,NANOHANA =  rgb(247, 217, 76, maxColorValue = 255)
    ,HIWACHA=  rgb(165, 160, 81, maxColorValue = 255)
    ,AKABENI=  rgb(203, 64, 66, maxColorValue = 255)
    ,KAKISHIBU =  rgb(163, 94, 71, maxColorValue = 255)
    ,OHNI =  rgb(240, 94, 28, maxColorValue = 255)
    ,SHAREGAKI = rgb(255, 186, 132, maxColorValue = 255)
    ,SHINSYU= rgb(171, 59, 58, maxColorValue = 255)
    ,KITSURUBAMI = rgb(186, 145, 50, maxColorValue = 255)
    ,YANAGIZOME = rgb(145, 173, 112, maxColorValue = 255)
    ,SEIJI= rgb(105, 176, 172, maxColorValue = 255)
    ,HAI= rgb(130, 130, 130, maxColorValue = 255)
    ,KANZO= rgb(252, 159, 77, maxColorValue = 255)
    ,SODENKARACHA = rgb(160, 103, 75, maxColorValue = 255)
    ,IMAYOH= rgb(208, 90, 110, maxColorValue = 255)
    ,FUKAGAWANEZUMI =  rgb(119, 150, 154, maxColorValue = 255)
    ,MATSUBA =  rgb(66, 96, 45, maxColorValue = 255)
    ,TOKIGARACHA  =  rgb(219, 142, 113, maxColorValue = 255)
    ,HASHITA =  rgb(152, 109, 178, maxColorValue = 255)
    ,OMESHICHA = rgb(55, 107, 109, maxColorValue = 255)
    ,KUCHINASHI = rgb(246, 197, 85, maxColorValue = 255)
    ,SUNEZUMI = rgb(120, 125, 123, maxColorValue = 255)
    ,AONIBI = rgb(83, 89, 83, maxColorValue = 255)
    ,SABITETSUONANDO= rgb(64, 91, 85, maxColorValue = 255)
    ,KESHIZUMI = rgb(67, 67, 67, maxColorValue = 255)
    ,KUROBENI= rgb(63, 43, 54, maxColorValue = 255)
    ,KIHADA= rgb(251, 226, 81, maxColorValue = 255)
    ,TORINOKO= rgb(218, 201, 166, maxColorValue = 255)
    ,MUSHIKURI = rgb(217, 205, 144, maxColorValue = 255)
    ,CYOHSYUN= rgb(191, 103, 102, maxColorValue = 255)
    ,USUBENI = rgb(232, 122, 144, maxColorValue = 255)
    ,UMEMURASAKI = rgb(168, 73, 122, maxColorValue = 255)
    ,FUJINEZUMI= rgb(110, 117, 164, maxColorValue = 255)
    ,SHIROTSURUBAMI = rgb(220, 184, 121, maxColorValue = 255)
    ,BENIKESHINEZUMI= rgb(82, 67, 61, maxColorValue = 255)
    ,HANADA = rgb(0, 98, 132, maxColorValue = 255)
    ,OUCHI = rgb(155, 144, 194, maxColorValue = 255)
    ,MIZUASAGI= rgb(102, 186, 183, maxColorValue = 255)
    ,HANAASAGI= rgb(30, 136, 168, maxColorValue = 255)
    ,SABIONANDO= rgb(51, 103, 116, maxColorValue = 255)
    ,SYOJYOHI = rgb(232, 48, 21, maxColorValue = 255)
    ,TOHOH= rgb(255, 196, 8, maxColorValue = 255)
    ,BINROJIZOME= rgb(58, 50, 38, maxColorValue = 255)
    ,OMINAESHI = rgb(221, 210, 59, maxColorValue = 255)
    ,BYAKUROKU= rgb(168, 216, 185, maxColorValue = 255)
    ,darkbrown_blues = darkbrown_blues
    ,darkred_yellows = darkred_yellows
    ,darkred_oranges = darkred_oranges
    ,darknihon_brown_blues = darknihon_brown_blues
    ,darknihon_brown_yellows = darknihon_brown_yellows
    ,darknihon_reds = darknihon_reds
    ,darknihon_green_blues =darknihon_green_blues
    ,darknihon_blue_greens = darknihon_blue_greens
    ,darknihon_yellow_reds = darknihon_yellow_reds
    ,darknihon_brown_cha = darknihon_brown_cha
    ,darknihon_brown_reds = darknihon_brown_reds
    ,lightnihon_fen = lightnihon_fen
    ,lightnihon_green_purples =lightnihon_green_purples
    ,lightnihon_brown_greens = lightnihon_brown_greens
    ,lightnihon_green_blues = lightnihon_green_blues
    ,lightnihon_green_yellows =lightnihon_green_yellows
    ,lightnihon_red_yellows = lightnihon_red_yellows
    ,lightnihon_blue_greens =lightnihon_blue_greens
    ,lightnihon_yellow_oranges = lightnihon_yellow_oranges,
    
    lightnihon_9x1 = lightnihon_9x1,
    lightnihon_6x1 = lightnihon_6x1,
    lightnihon_7x1 = lightnihon_7x1,
    lightnihon_8x1 =lightnihon_8x1,
    lightnihon_5x1 = lightnihon_5x1,
    darknihon_2x1 = darknihon_2x1,
    darknihon_4x1 = darknihon_4x1,
    darknihon_5x1 = darknihon_5x1,
    darknihon_5x2 =darknihon_5x2,
    darknihon_5x3 = darknihon_5x3,
    darknihon_6x1 =darknihon_6x1,
    darknihon_6x2 = darknihon_6x2,
    darknihon_6x3 = darknihon_6x3,
    darknihon_8x1 =darknihon_8x1
  )
  if(!is.null(color)){
    if(all(is.element(color, names(color_board)))){
      colors = color_board[[color]]
    }else{
      palette_colors = brewer_pal(9,name = color)
      if(length(palette_colors) >0 ){
        colors = palette_colors
        get_colors = color_ramp_palette(colors)
        colors = get_colors(2)[2]
      }else{
        warning(paste(color, "is not a valid color name for love_color, use 'dark_cyan'.\n"))
        colors = color_board[["dark_cyan"]]
      }
    }
    
  }else{
    if(!is.null(type)){
      color_index = grep(type,names(color_board))
      if(length(color_index)>0){
        get_colors = color_ramp_palette(as.vector(color_board[color_index]))
        colors = get_colors(n)
      }else{
        palette_colors = brewer_pal(9,name = type)
        if(length(palette_colors)>0){
          get_colors = color_ramp_palette(palette_colors)
          colors = get_colors(n)
        }else{
          warning(paste(type, "is not a valid color palette name for love_color, use 'YlGnBu'.\n"))
          get_colors = color_ramp_palette(brewer_pal(9,name = "YlGnBu"))
          colors = get_colors(n)
        }
      }
    }else{
      warning(paste("color is NULL for love_color, use 'dark_cyan'.\n"))
      colors = color_board[["dark_cyan"]]
    }
  }
  return(unique(colors))
}




#' plot_theme
#'
#' \code{plot_theme} is a simper wrapper of theme for ggplot2.
#' @param legend.position see details at: code{legend.position}
#' @param angle see details at:  code{axis.text.x}
#' @param legend_size  see details at:  code{legend.text}
#' @param axis_size_x see details at:  code{axis.text.x}
#' @param axis_size_y see details at:  code{axis.text.y}
#' @param axis_title_size see details at:  code{axis.title.x}
#' @param title_size see details at:  code{plot.title}
#' @param title_vjust see details at:  code{plot.title}
#' @param title_hjust see details at:  code{plot.title}
#' @param linetype see details at:  code{panel.grid.major}
#' @param face see details at:  code{axis.title.x}
#' @details see details at: code{theme}
#' @import ggplot2
#' @export

plot_theme = function(legend.position = "top", angle = 30,
                       legend_size = 7, axis_size_y = 8,
                       axis_size_x = 8, axis_title_size = 10,
                       title_size = 11, title_vjust = 0, title_hjust = 0,
                       linetype = "dotted", face = "bold") {
  plot_theme = theme_bw() + theme(legend.position = legend.position,
                                   panel.border = element_blank(),
                                   panel.grid.major = element_line(linetype = linetype),
                                   panel.grid.minor = element_blank(),
                                   plot.title = element_text(face = "bold", size = title_size,
                                                             vjust = title_vjust, hjust = title_hjust),
                                   legend.title = element_blank(),
                                   legend.text = element_text(size = legend_size),
                                   legend.key = element_blank(),
                                   axis.text.x = element_text(size = axis_size_x,
                                                              vjust = 0.5, angle = angle),
                                   axis.text.y = element_text(size = axis_size_y),
                                   axis.title.x = element_text(size = axis_title_size, face = face),
                                   axis.title.y = element_text(size = axis_title_size),
                                   strip.text = element_text(size = 10, face = face),
                                   strip.background = element_blank())
  return(plot_theme)
}


#' model result plots
#' \code{model_result_plot} is a wrapper of following:
#' \code{perf_table} is for generating a model performance table.
#' \code{ks_plot} is for K-S.
#' \code{roc_plot} is for ROC.
#' \code{lift_plot} is for Lift Chart.
#' \code{score_distribution_plot} is for ploting the score distribution.
#' @param train_pred A data frame of training with predicted prob or score.
#' @param test_pred A data frame of validation with predict prob or score.
#' @param target The name of target variable.
#' @param score The name of prob or score variable.
#' @param g Number of breaks for prob or score.
#' @param cut_bin A string, if equal_bins is TRUE, 'equal_depth' or 'equal_width', default is 'equal_depth'.
#' @param perf_tb Performance table.
#' @param digits Digits of numeric,default is 4.
#' @param binsNO Bins Number.Default is FALSE.
#' @param gtitle The title of the graph & The name for periodically saved graphic file.
#' @param breaks Splitting points of prob or score.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @param total Whether to summarize the table. default: TRUE.
#' @param plot_show Logical, show model performance in current graphic device. Default is TRUE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param perf_dir_path The path for periodically saved graphic files.
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",x_list = x_list,
#' occur_time = "apply_date", miss_values = list("", -1))
#' dat = process_nas(dat,default_miss = TRUE)
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test

#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' # model evaluation
#' perf_table(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' #lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' #score_distribution_plot(train_pred = dat_train, test_pred = dat_test,
#' #target = "target", score = "pred_LR")
#' #model_result_plot(train_pred = dat_train, test_pred = dat_test,
#' #target = "target", score = "pred_LR")
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter arrange
#' @importFrom data.table melt dcast
#' @export


model_result_plot = function(train_pred , score, target ,test_pred= NULL, gtitle = NULL,perf_dir_path = NULL,
                              save_data = FALSE,plot_show = TRUE,total = TRUE,g = 10,cut_bin = 'equal_depth',digits = 4 ){
  
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = digits) #
  train_pred = checking_data(dat = train_pred, target = target)
  train_pred = train_pred[which(complete.cases(train_pred[, score])),]
  if(!is.null(test_pred)){
    test_pred = checking_data(dat = test_pred, target = target)
    test_pred = test_pred[which(complete.cases(test_pred[, score])),]
  }
  
  perf_tb = perf_table(train_pred = train_pred, test_pred = test_pred, target = target,
                       score = score, total = total,
                       g = g,cut_bin = cut_bin,digits = digits)
  
  tb_ks =  plot_table(grid_table = perf_tb, theme = c("cyan"),
                      title= paste0(gtitle, ".performance_table"),title.size = 12,title.color = 'black',
                      title.face = "bold", title.position='middle',
                      subtitle= NULL,subtitle.size = 8,subtitle.color = 'black',subtitle.face = "plain",
                      subtitle.position='middle',
                      tile.color = 'grey80',tile.size = 1,
                      colname.size = 3,colname.color = 'white',colname.face = 'bold',
                      colname.fill.color =love_color("dark_cyan"),
                      text.size = 3,text.color =love_color("dark_grey"),
                      text.face = 'plain', text.fill.color = c('white',love_color("pale_grey")))
  perf_tb = perf_table(train_pred = train_pred, test_pred = test_pred, target = target,
                       score = score, total = FALSE,
                       g = g,cut_bin = cut_bin)
  dis_pl = score_distribution_plot(train_pred = train_pred, test_pred = test_pred,
                                   target = target, score =score, gtitle = gtitle,g = g,perf_tb = perf_tb)
  
  ks_pl = ks_plot(train_pred = train_pred, test_pred = test_pred, target = target, score = score, gtitle = gtitle, g = g, perf_tb = NULL)
  roc_pl = roc_plot(train_pred = train_pred, test_pred = test_pred, target = target, score = score, gtitle = gtitle)
  lift_pl = lift_plot(train_pred = train_pred, test_pred = test_pred, target = target, score = score, gtitle = gtitle,g = g,perf_tb = perf_tb)
  
  if(save_data){
    perf_dir_path = ifelse(is.null(perf_dir_path), tempdir(), perf_dir_path)
    if (!dir.exists(perf_dir_path)) dir.create(perf_dir_path)
    
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_ks"), "png", sep = '.')), device = "png",
           plot = ks_pl, dpi = "retina", width = 5, height = 4.5)
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_roc"), "png", sep = '.')), device = "png",
           plot = roc_pl, dpi = "retina", width = 5, height = 4.5)
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_psi"), "png", sep = '.')), device = "png",
           plot = dis_pl, dpi = "retina", width = 5, height = 4.5)
    ggsave(paste0(perf_dir_path, "/", paste(paste0(gtitle, ".performance_table"), "png", sep = '.')),
           plot = tb_ks, dpi = "retina", width = 12)
    save_data(perf_tb, file_name = paste0(gtitle, ".performance_table"), dir_path = perf_dir_path,note = TRUE)
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, ".lift"), "png", sep = '.')), device = "png",
           plot = lift_pl, dpi = "retina", width = 5, height = 4.5)
    
  }
  if (plot_show) {
    plot(multi_grid(grobs = list(ks_pl,lift_pl,roc_pl, dis_pl), ncol = 2, nrow = 2))
  }
  return(perf_tb)
  options(opt)
}

#' performance table
#'
#' @rdname model_result_plot
#' @export

perf_table = function(train_pred, test_pred = NULL, target = NULL, score = NULL,
         g = 10, cut_bin = 'equal_depth', breaks = NULL,digits = 2,
         pos_flag = list("1", "1", "Bad", 1),total = FALSE, binsNO = FALSE) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = digits) #
  `train_GB_index` = `test_GB_index` = `G` = `bins` = `B` = `%train` = `%test` = `#train` = `#test` = NULL
  `%test_B` = `%test_cumB` = `%test_cumG` = `%train_B` = `%train_cumB` = `%train_cumG` = bins_n = NULL
  
  train_pred = train_pred[which(complete.cases(train_pred[, score])),]
  if (is.null(target)) { stop("target is missing!\n") }
  if (is.null(score)) { stop("score is missing!\n") }
  if (is.null(breaks)) {
    breaks = get_breaks(dat = train_pred, x = score,
                        target = target, equal_bins = TRUE,
                        cut_bin = cut_bin,
                        best = FALSE, g = g, note = FALSE)
  }
  train_pred$bins = split_bins(dat = train_pred, x = score, breaks = breaks, bins_no = TRUE)
  
  if (!is.null(target)) {
    if (length(unique(train_pred[, target])) > 1) {
      if (is.null(pos_flag)) {
        train_pred$target = ifelse(train_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
      } else {
        train_pred$target = ifelse(train_pred[, target] %in% pos_flag, "B", "G")
        if (length(unique(train_pred$target)) == 1) {
          stop(paste("The value in pos_flag is not one of the value of train_pred's target.\n"))
        }
      }
    } else {
      stop(paste("The value of train_pred's target is unique.\n"))
    }
  } else {
    stop(paste("The target variable is missing.\n"))
  }
  
  train_pred$bins_n = sapply(as.character(train_pred$bins),function(x)
    as.numeric(strsplit(as.character(x),split = "\\.")[[1]][1]))
  
  train_sum = train_pred %>%
    dplyr::filter(train_pred$target %in% c("B", "G")) %>%
    dplyr::group_by(bins,bins_n) %>%
    dplyr::count(bins, target) %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    as.data.table() %>%
    dplyr::arrange(bins_n) 
  train_sum = data.table::dcast(train_sum[,c("bins_n","bins","target", "n"),with = FALSE],
                                bins_n + bins ~ target, value.var = "n") %>% dplyr::arrange(bins_n) 
  train_sum = quick_as_df(train_sum)
  train_sum[is.na(train_sum)] = 0
  f_bad = sum(train_sum[1:floor(nrow(train_sum)/2), "B"],na.rm = TRUE)
  f_good = sum(train_sum[1:floor(nrow(train_sum)/2), "G"],na.rm = TRUE)
  l_bad = sum(train_sum[(floor(nrow(train_sum)/2)+1):nrow(train_sum), "B"],na.rm = TRUE)
  l_good = sum(train_sum[(floor(nrow(train_sum)/2)+1):nrow(train_sum), "G"],na.rm = TRUE)
  f_bad_rate = f_bad /(f_bad + f_good)
  l_bad_rate = l_bad /(l_bad + l_good)
  if (f_bad_rate < l_bad_rate) {
    train_sum_lift = train_sum[order(train_sum$bins, decreasing = TRUE),]
    train_Lift = round((cumsum(train_sum_lift$B) / cumsum(train_sum_lift$G + train_sum_lift$B)) / (sum(train_sum_lift$B) / sum(train_sum_lift$G + train_sum_lift$B)), digits)
    train_Lift = sort(train_Lift,na.last=FALSE)
  } else {
    train_Lift = round((cumsum(train_sum$B) / cumsum(train_sum$G + train_sum$B)) / (sum(train_sum$B) / sum(train_sum$G + train_sum$B)), digits)
    train_Lift = sort(train_Lift, decreasing = TRUE,na.last=FALSE)
  }
  
  train_ks = transform(train_sum,
                        train_total = G + B,
                        `%train_total` = round((G + B) / sum(G + B), digits),
                        `%train_B` = round(B / (G + B), digits),
                        `%train_cumG` = round(cumsum(G) / sum(G), digits),
                        `%train_cumB` = round(cumsum(B) / sum(B), digits),
                        `train_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), digits)),
                        train_Lift = train_Lift
  )
  if (!is.null(test_pred) || length(test_pred) > 1) {
    test_pred = test_pred[which(complete.cases(test_pred[, score])),]
    test_pred$bins = split_bins(dat = test_pred, x = score, breaks = breaks, bins_no = TRUE)
    if (!is.null(target)) {
      if (length(unique(test_pred[, target])) > 1) {
        if (is.null(pos_flag)) {
          test_pred$target = ifelse(test_pred[, target] %in% list("1", "1", "Bad", 1), "B", "G")
        } else {
          test_pred$target = ifelse(test_pred[, target] %in% pos_flag, "B", "G")
          if (length(unique(test_pred$target)) == 1) {
            stop(paste("The value in pos_flag is not one of the value of test_pred's target.\n"))
          }
        }
      } else {
        stop(paste("The value of test_pred's target is unique.\n"))
      }
    } else {
      stop(paste("The target variable is missing.\n"))
    }

    test_pred$bins_n = sapply(as.character(test_pred$bins),function(x)
      as.numeric(strsplit(as.character(x),split = "\\.")[[1]][1]))
    
    test_sum = test_pred %>%
      dplyr::filter(test_pred$target %in% c("B", "G")) %>%
      dplyr::group_by(bins,bins_n) %>%
      dplyr::count(bins, target) %>%
      dplyr::mutate(percent = n / sum(n)) %>%
      as.data.table() %>%
      dplyr::arrange(bins_n) 
    test_sum = data.table::dcast(test_sum[,c("bins_n","bins","target", "n"),with = FALSE],
                                  bins_n + bins ~ target, value.var = "n") %>% dplyr::arrange(bins_n) 
    
    test_sum = quick_as_df(test_sum)
    test_sum[is.na(test_sum)] = 0
    f_t_bad = sum(test_sum[1:floor(nrow(test_sum)/2), "B"],na.rm = TRUE)
    f_t_good = sum(test_sum[1:floor(nrow(test_sum)/2), "G"],na.rm = TRUE)
    l_t_bad = sum(test_sum[(floor(nrow(test_sum)/2)+1):nrow(test_sum), "B"],na.rm = TRUE)
    l_t_good = sum(test_sum[(floor(nrow(test_sum)/2)+1):nrow(test_sum), "G"],na.rm = TRUE)
    f_t_bad_rate = f_t_bad /(f_t_bad + f_t_good)
    l_t_bad_rate = l_t_bad /(l_t_bad + l_t_good)

    
    test_sum = merge(train_ks[c(1:2)], test_sum[c(1, 3:4)], all = TRUE) %>% dplyr::arrange(bins_n)
    test_sum[is.na(test_sum)] = 0
    if (f_t_bad_rate < l_t_bad_rate) {
      test_sum_lift = test_sum[order(test_sum$bins_n, decreasing = TRUE),]
      test_Lift = round(ifelse(test_sum_lift$G + 
                                        test_sum_lift$B == 0,1,cumsum(test_sum_lift$B) / cumsum(test_sum_lift$G + test_sum_lift$B)) / 
                          (sum(test_sum_lift$B) / sum(test_sum_lift$G + test_sum_lift$B)), digits)
      test_Lift = sort(test_Lift,na.last=FALSE)
    } else {
      test_Lift = round((cumsum(test_sum$B) / cumsum(test_sum$G + test_sum$B)) / (sum(test_sum$B) / sum(test_sum$G + test_sum$B)), digits)
      test_Lift = sort(test_Lift, decreasing = TRUE,na.last=FALSE)
    }
    test_ks = transform(test_sum,
                         test_total = G + B,
                         `%test_total` = round((G + B) / (sum(G + B)), digits),
                         `%test_B` = round(B / (G + B), digits),
                         `%test_cumG` = round(cumsum(G) / sum(G), digits),
                         `%test_cumB` = round(cumsum(B) / sum(B), digits),
                         `test_K-S` = abs(round((cumsum(B) / sum(B)) - (cumsum(G) / sum(G)), digits)),
                         test_Lift = test_Lift
    )
    
    dt_ks = merge(train_ks[c(1:2, 5:11)], test_ks[c(1:2, 5:11)], all.x = TRUE) %>% dplyr::arrange(bins_n)
    dt_ks[is.na(dt_ks)] = 0
 
    names(dt_ks) = c("bins_n","bins", "#train", "%train", "%train_B",
                     "%train_cumG", "%train_cumB", "train_K-S", "train_Lift",
                     "#test", "%test", "%test_B", "%test_cumG",
                     "%test_cumB", "test_K-S", "test_Lift")
    dt_ks = dt_ks %>% dplyr::mutate(PSI = ifelse(`%train` == 0 | `%test` == 0, 1, round((`%train` - `%test`) * log(`%train` / `%test`), digits)), `#total` = `#train` + `#test`)
    dt_ks = dt_ks[c("bins_n","bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                    "%test_B", "%train_cumG", "%train_cumB", "%test_cumG", "%test_cumB",
                    "train_K-S", "test_K-S", "train_Lift", "test_Lift", "PSI")]
    
    
    
    if(total){
      total = c("--","Total",
                 sum(dt_ks$`#total`,na.rm = TRUE),
                 sum(dt_ks$`#train`, na.rm = TRUE),
                 sum(dt_ks$`#test`, na.rm = TRUE),
                 as_percent(sum(dt_ks$`#train`, na.rm = TRUE) / sum(dt_ks$`#total`, na.rm = TRUE),digits),
                 as_percent(sum(dt_ks$`#test`, na.rm = TRUE) / sum(dt_ks$`#total` , na.rm = TRUE), digits),
                 as_percent(sum(dt_ks$`%train_B` * dt_ks$`#train`, na.rm = TRUE) / sum(dt_ks$`#train`,  na.rm = TRUE), digits),
                 as_percent(sum(dt_ks$`%test_B` * dt_ks$`#test`,na.rm = TRUE) / sum(dt_ks$`#test`, na.rm = TRUE), digits), "100%", "100%",
                 "100%", "100%",
                 max(dt_ks$`train_K-S`,na.rm = TRUE),
                 max(dt_ks$`test_K-S`, na.rm = TRUE),
                 max(dt_ks$`train_Lift`,na.rm = TRUE),
                 max(dt_ks$`test_Lift`, na.rm = TRUE),
                 sum(dt_ks$PSI,na.rm = TRUE))
      
      dt_ks = dt_ks[c("bins_n","bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                       "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                       "%test_cumB", "train_K-S", "test_K-S","train_Lift", "test_Lift", "PSI")]
      dt_ks = transform(dt_ks,
                         `%train` = as_percent(`%train`, digits = digits),
                         `%test` = as_percent(`%test`, digits = digits),
                         `%train_B` = as_percent(`%train_B`, digits = digits),
                         `%test_B` = as_percent(`%test_B`, digits = digits),
                         `%train_cumG` = as_percent(`%train_cumG`, digits = digits),
                         `%train_cumB` = as_percent(`%train_cumB`, digits = digits),
                         `%test_cumG` = as_percent(`%test_cumG`, digits = digits),
                         `%test_cumB` = as_percent(`%test_cumB`, digits = digits)
      )
      
      dt_ks = rbind(dt_ks, total)
      names(dt_ks) = c("bins_n","bins", "#total", "#train", "#test", "%train", "%test", "%train_B",
                       "%test_B", "%train_cumG", "%train_cumB", "%test_cumG",
                       "%test_cumB", "train_K-S", "test_K-S","train_Lift", "test_Lift", "PSI")
    }
    
  } else {
    dt_ks = train_ks[c(1:2, 5:11)]
    dt_ks[is.na(dt_ks)] = 0
    names(dt_ks) = c("bins_n","bins", "#Pop", "%Pct", "%Pct_1",
                     "%Cumsum_0", "%Cumsum_1", "K-S", "Lift")
    
    dt_ks = dt_ks[c("bins_n","bins", "#Pop", "%Pct", "%Pct_1",
                    "%Cumsum_0", "%Cumsum_1", "K-S", "Lift")]
  }
  if(!binsNO){
     dt_ks = dt_ks[-1]
  }
  return(dt_ks)
  options(opt) # reset
}



#' ks_plot
#'
#' @rdname model_result_plot
#' @export

ks_plot = function(train_pred, test_pred = NULL, target = NULL, score =  NULL,
         gtitle = NULL, breaks = NULL, g = 10,cut_bin = 'equal_width',perf_tb = NULL) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `value` = `train_test` = `bins` = `%train_cumG` = `%train_cumB` = `%test_cumG` = `%test_cumB`= `%Cumsum_0`= `%Cumsum_1` = bins_n= NULL
  train_pred = train_pred[which(complete.cases(train_pred[, score])),]
  if (is.null(gtitle)) { gtitle = paste0("Model") }
  if (is.null(target)) { stop("target is missing!\n")}
  if (is.null(score)) { stop("score is missing!\n")}
  if(is.null(perf_tb)){
    tb_ks = perf_table(train_pred = train_pred, test_pred = test_pred, target = target, 
                       score = score, g = g, breaks = breaks,cut_bin = cut_bin,
                       digits = 3, binsNO = TRUE)%>% dplyr::arrange(bins_n)
  }else{
    tb_ks = perf_tb
  }
  
  if (!is.null(test_pred) || length(test_pred) > 1) {
    test_pred = test_pred[which(complete.cases(test_pred[, score])),]
    ks = rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), tb_ks)
    
    ks_pl = ggplot(ks, aes(x = as.numeric(ks$bins_n))) +
      geom_line(aes(y = ks$`%train_cumG`, group = 1, color = "%train_cumG"), size = 1) +
      geom_point(aes(x = ks$bins_n[which.max(ks$`train_K-S`)],
                     y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
      geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
      geom_segment(aes(x = ks$bins_n[which.max(ks$`train_K-S`)],
                       y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)]) + 0.01,
                       xend = ks$bins_n[which.max(ks$`train_K-S`)],
                       yend = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
      geom_line(aes(y = `%train_cumB`, color = '%train_cumB', group = 1), size = 1) +
      geom_point(aes(x = ks$bins_n[which.max(ks$`train_K-S`)],
                     y = as.numeric(ks$`%train_cumG`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      geom_point(aes(x = ks$bins_n[which.max(ks$`train_K-S`)],
                     y = as.numeric(ks$`%train_cumB`[which.max(ks$`train_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      annotate(geom = 'text', x = g/1.5, y = 0.2,
               label = paste('train K-S : ', max(round(ks$`train_K-S`, 2))),
               vjust = 1.5) +
      geom_line(aes(y = `%test_cumG`, group = 1, color = "%test_cumG"),
                linetype = "dashed", size = 1) +
      geom_point(aes(x = ks$bins_n[which.max(ks$`test_K-S`)],
                     y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
      geom_segment(aes(x =  ks$bins_n[which.max(ks$`test_K-S`)],
                       y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)]) + 0.01,
                       xend =  ks$bins_n[which.max(ks$`test_K-S`)],
                       yend = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
      geom_line(aes(y = `%test_cumB`, color = '%test_cumB', group = 1),
                linetype = "dashed", size = 1) +
      geom_point(aes(x =  ks$bins_n[which.max(ks$`test_K-S`)],
                     y = as.numeric(ks$`%test_cumG`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      geom_point(aes(x =  ks$bins_n[which.max(ks$`test_K-S`)],
                     y = as.numeric(ks$`%test_cumB`[which.max(ks$`test_K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      annotate(geom = 'text', x = g/1.5, y = 0.3, vjust = 1.5,
               label = paste('test K-S : ', max(round(ks$`test_K-S`, 2)))) +
      scale_colour_manual(values = c("%train_cumG" = love_color("light_blue"),
                                     "%test_cumG" = love_color("light_green"),
                                     "%train_cumB" = love_color("light_red"),
                                     "%test_cumB" = love_color("deep_orange")),
                          labels = c("%test_1", "%test_0","%train_1", "%train_0")) +
      labs(x = "% of Total", y = "% of CumSum",
           title = paste(gtitle, "K-S Curve")) + theme_light()+
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11,vjust = 0, hjust = 0)) + 
      scale_x_discrete(limits = seq(0,g,g/10),labels = 
                        as_percent(round(seq(0,1,0.1), 1)))

  } else {
    ks = rbind(c(0, 0, 0, 0, 0, 0, 0,0), tb_ks)
    ks_pl =  ggplot(ks, aes(x = bins_n)) +
      geom_line(aes(y = `%Cumsum_0`, group = 1, color = "%Cumsum_0"), size = 1) +
      geom_point(aes(x = ks$bins_n[which.max(ks$`K-S`)],
                     y = as.numeric(ks$`%Cumsum_0`[which.max(ks$`K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = "#085A9C") +
      geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
      geom_segment(aes(x =ks$bins_n[which.max(ks$`K-S`)],
                       y = as.numeric(ks$`%Cumsum_0`[which.max(ks$`K-S`)]) + 0.01,
                       xend = ks$bins_n[which.max(ks$`K-S`)],
                       yend = as.numeric(ks$`%Cumsum_1`[which.max(ks$`K-S`)]) - 0.01),
                   colour = love_color("deep_grey"), linetype = "dashed",
                   arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
      geom_line(aes(y = `%Cumsum_1`, color = '%Cumsum_1', group = 1), size = 1) +
      geom_point(aes(x = ks$bins_n[which.max(ks$`K-S`)],
                     y = as.numeric(ks$`%Cumsum_0`[which.max(ks$`K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      geom_point(aes(x = ks$bins_n[which.max(ks$`K-S`)],
                     y = as.numeric(ks$`%Cumsum_1`[which.max(ks$`K-S`)])),
                 size = 2, shape = 21, fill = 'white', color = '#ca3e1c') +
      annotate(geom = 'text', x = g/1.5, y = 0.2,
               label = paste('K-S : ', max(round(ks$`K-S`, 2))),
               vjust = 1.5) +
      scale_colour_manual(values = c("%Cumsum_0" = love_color("water_blue"),
                                     "%Cumsum_1" = love_color("gold_yellow")),
                          labels = c("%Cumsum_0", "%Cumsum_1")) +
      labs(x = "% of Total", y = "% of CumSum",
           title = paste(gtitle, "K-S Curve")) + theme_light() +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0)) +
    scale_x_discrete(limits = seq(0,g,g/10),labels = as_percent(round(seq(0,1,0.1), 1)))
  }
  return(ks_pl)
  options(opt) # reset
}



#' lift_plot
#'
#' @rdname model_result_plot
#' @export

lift_plot = function(train_pred, test_pred = NULL, target = NULL, score = NULL,
                      gtitle = NULL,  breaks = NULL, g = 10,cut_bin = 'equal_depth',perf_tb = NULL) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `value` = `train_test` = `bins` = `%train_cumG` = `%train_cumB` = `%test_cumG` = `%test_cumB` = Lift = test_Lift = train_Lift= NULL
  if (is.null(gtitle)) { gtitle = paste0("Model") }
  if (is.null(target)) { stop("target is missing!\n") }
  if (is.null(score)) { stop("score is missing!\n") }
  if(is.null(perf_tb)){
    tb_ks = perf_table(train_pred = train_pred, test_pred = test_pred, target = target, score = score, g = g, breaks = breaks,cut_bin = cut_bin)
  }else{
    tb_ks = perf_tb
  }
  
  if (!is.null(test_pred) || length(test_pred) > 1) {
    if (mean(tb_ks[1:floor(nrow(tb_ks)/2), "train_Lift"]) < mean(tb_ks[(floor(nrow(tb_ks)/2)+1):nrow(tb_ks), "train_Lift"])) {
      tb_ks = tb_ks[order(tb_ks$bins, decreasing = TRUE),]
    }
    lift_pl = ggplot(tb_ks, aes(x = reorder(as.factor(as_percent(round(cumsum(tb_ks$`%train`), 1))), cumsum(tb_ks$`%train`)))) +
      geom_line(aes(y = train_Lift, color = 'train_Lift (Model)', group = 1), size = 1) +
      geom_point(aes(y = train_Lift), size = 1.5, shape = 22, fill = 'white', color = '#ca3e1c') +
      geom_line(aes(y = test_Lift, color = 'test_Lift (Model)', group = 1), size = 1) +
      geom_point(aes(y = test_Lift), size = 1.5, shape = 22, fill = 'white', color = love_color("shallow_cyan")) +
      geom_line(aes(y = rep(1, nrow(tb_ks)), group = 1, color = "Lift (Random)"), size = 1) +
      scale_colour_manual(values = c("Lift (Random)" = "#085A9C", 'test_Lift (Model)' = love_color("green_cyan"), 'train_Lift (Model)' = '#ca3e1c')) +
      geom_point(aes(y = rep(1, nrow(tb_ks))), size = 1.5, shape = 21, fill = 'white', color = "#085A9C") +
      scale_y_continuous(limits = c(0, max(tb_ks$train_Lift) + 0.5)) +
      geom_text(aes(y = train_Lift, label = round(train_Lift, 1)), vjust = -1, hjust = 0.5, size = 3, colour = "darkgrey") +
      geom_text(aes(y = test_Lift, label = round(test_Lift, 1)), vjust = 1.3, hjust = 0.5, size = 3, colour = "darkgrey") +
      labs(x = '%Total', y = 'Lift', title = paste(gtitle,"Lift Chart")) + theme_light() +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
  } else {
    if (mean(tb_ks[1:floor(nrow(tb_ks)/2), "Lift"]) < mean(tb_ks[(floor(nrow(tb_ks)/2)+1):nrow(tb_ks), "Lift"])) {
      tb_ks = tb_ks[order(tb_ks$bins, decreasing = TRUE),]
    }
    lift_pl =  ggplot(tb_ks, aes(x = reorder(as.factor(as_percent(round(cumsum(tb_ks$`%Pct`), 1))), cumsum(tb_ks$`%Pct`)))) +
      geom_line(aes(y = Lift, linetype = 'Lift (Model)', group = 1), color = '#ca3e1c', size = 1) +
      geom_point(aes(y = Lift), size = 1.5, shape = 22, fill = 'white', color = '#ca3e1c') +
      geom_line(aes(y = rep(1, nrow(tb_ks)), group = 1, color = "Lift (Random)"), size = 1) +
      scale_colour_manual(values = c("Lift (Random)" = "#085A9C")) +
      geom_point(aes(y = rep(1, nrow(tb_ks))), size = 1.5, shape = 21, fill = 'white', color = "#085A9C") +
      scale_y_continuous(limits = c(0, max(tb_ks$Lift) + 0.5)) +
      geom_text(aes(y = Lift, label = round(Lift, 1)), vjust = -1, hjust = 0.5, size = 3, colour = "darkgrey") +
      labs(x = '%Total', y = 'Lift', title = paste(gtitle,"Lift Chart")) + theme_light() +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
  }
  return(lift_pl)
  options(opt) # reset
}

#' roc_plot
#'
#' @rdname model_result_plot
#' @export

roc_plot = function(train_pred, test_pred = NULL, target = NULL, score = NULL, gtitle = NULL) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  if (is.null(target)) {stop("target is missing!\n") }
  if (is.null(score)) { stop("score is missing!\n") }
  if (is.null(gtitle)) { gtitle = paste0("Model") }
  train_pred = train_pred[which(complete.cases(train_pred[, score])),]
  if(length(train_pred[,score]) > 1 && max(train_pred[,score]) > 1)train_pred[,score] = 1/train_pred[,score]
  roc_d = data.frame(prob = train_pred[ ,score], target = train_pred[ ,target])
  auc = auc_value(target = roc_d$target, prob = roc_d$prob)
  sam_n = ifelse(nrow(roc_d)>10000,10000,nrow(roc_d))
  roc_d_n = sample(1:nrow(roc_d),sam_n,replace = FALSE)
  roc_d = roc_d[roc_d_n,]
  roc_d = roc_d[order(roc_d$prob),]
  tpr = fpr = rep(0, nrow(roc_d))
  for (i in 1:nrow(roc_d)) {
    threshold = roc_d$prob[i]
    tp = sum(roc_d$prob > threshold & roc_d$target == 1)
    fp = sum(roc_d$prob > threshold & roc_d$target == 0)
    tn = sum(roc_d$prob < threshold & roc_d$target == 0)
    fn = sum(roc_d$prob < threshold & roc_d$target == 1)
    tpr[i] = tp / (tp + fn)
    fpr[i] = fp / (tn + fp)
  }
  tpr_fpr = data.frame(tpr, fpr)

  if (!is.null(test_pred) || length(test_pred) > 1) {
    test_pred = test_pred[which(complete.cases(test_pred[, score])),]
    if(length(test_pred[,score]) > 1 && max(test_pred[,score]) > 1)test_pred[,score] = 1/test_pred[,score]
    roc_dts = data.frame(prob = test_pred[, score], target = test_pred[, target])
    
    auc_ts = auc_value(target = roc_dts$target, prob = roc_dts$prob)
    
    sam_ns = ifelse(nrow(roc_dts)>10000,10000,nrow(roc_dts))
    roc_d_ns = sample(1:nrow(roc_dts),sam_ns,replace = FALSE)
    roc_dts = roc_dts[roc_d_ns,]
    roc_dts = roc_dts[order(roc_dts$prob),]
    tpr_ts = fpr_ts = rep(0, nrow(roc_dts))
    for (i in 1:nrow(roc_dts)) {
      threshold = roc_dts$prob[i]
      tp_ts = sum(roc_dts$prob > threshold & roc_dts$target == 1)
      fp_ts = sum(roc_dts$prob > threshold & roc_dts$target == 0)
      tn_ts = sum(roc_dts$prob < threshold & roc_dts$target == 0)
      fn_ts = sum(roc_dts$prob < threshold & roc_dts$target == 1)
      tpr_ts[i] = tp_ts / (tp_ts + fn_ts)
      fpr_ts[i] = fp_ts / (tn_ts + fp_ts)
    }
    tpr_fpr_ts = data.frame(tpr_ts, fpr_ts)
    roc_pl = ggplot() +
      geom_line(aes(x = tpr_fpr$fpr, y = tpr_fpr$tpr,group = 1, color = "train ROC"), size = 1) +
      annotate(geom = 'text', x = 0.5, y = 0.4, vjust = 1.5,
               label = paste('train AUC : ', round(auc, 4))) +
      geom_line( aes(x = tpr_fpr_ts$fpr_ts, y = tpr_fpr_ts$tpr_ts, group = 1, color = "test ROC"), size = 1) +
      scale_colour_manual(values = c("train ROC" = love_color("deep_orange"), 'test ROC' = love_color("deep_blue"))) +
      annotate(geom = 'text', x = 0.5, y = 0.3, vjust = 1.5,
               label = paste('test AUC : ', round(auc_ts, 4))) +
      geom_abline() + geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
      ylim(c(0, 1)) + labs(x = 'FPR', y = 'TPR',title = paste(gtitle, "ROC Curve"))+ theme_light()+
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
  } else {
    roc_pl = ggplot() +
      geom_line(aes(x = tpr_fpr$fpr, y = tpr_fpr$tpr), size = 1, color = love_color("deep_orange")) +
      geom_abline() + geom_hline(yintercept = 1) + geom_hline(yintercept = 0) +
      annotate(geom = 'text', x = 0.5, y = 0.35, vjust = 1.5,
               label = paste('AUC : ', round(auc, 4))) +
      ylim(c(0, 1)) + labs(x = 'Fpr', y = 'Tpr',title = paste(gtitle, "ROC Curve")) + theme_light() +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 11, vjust = 0, hjust = 0))
  }
  return(roc_pl)
  options(opt) # reset
}



#' score_distribution_plot
#'
#' @rdname model_result_plot
#' @export


score_distribution_plot = function(train_pred, test_pred, target, score,
                                    gtitle = NULL, breaks = NULL, g = 10,cut_bin = 'equal_depth',perf_tb = NULL ) {
  opt = options('warn' = -1,scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  `value` = `train_test` = `bins` = `%train_cumG` = `%train_cumB` = `%test_cumG` = `%test_cumB` =
    `%Pct` = `%Pct_1` =  NULL
  if (is.null(gtitle)) { gtitle = paste0("Model") }
  
  if(is.null(perf_tb)){
    tb_ks = perf_table(train_pred = train_pred, test_pred = test_pred, target = target, score = score, g = g, breaks = breaks,cut_bin = cut_bin)
  }else{
    tb_ks = perf_tb
  }
  if (!is.null(test_pred) || length(test_pred) > 1){
    
    names(tb_ks)[7:8] =  c("%train_1", "%test_1")
    ts_total = data.table::melt(as.data.table(tb_ks[c("bins", "%train", "%test")]), id.vars = c("bins"),
                                variable.name = "train_test", value.name = "value")
    ts_1 = data.table::melt(as.data.table(tb_ks[c("bins", "%train_1", "%test_1")]), id.vars = c("bins"),
                            variable.name = "train_test", value.name = "value")
    
    dis_pl =  ggplot(ts_total, aes(x = bins, y = value,fill = train_test)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
      geom_text(aes(y = value, label = paste(as_percent(value, digits = 3))),
                position = position_dodge(width = 0.7),
                size = 3, vjust = 1, hjust = 0.3, colour = "white") +
      geom_line(aes(x = factor(ts_1[[1]]),
                    y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                    color = ts_1$train_test,
                    linetype = ts_1$train_test,
                    group = ts_1$train_test),
                position = position_dodge(width = 0.5), size = 1) +
      geom_point(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                     color = ts_1$train_test,
                     group = ts_1$train_test),
                 position = position_dodge(width = 0.5),
                 fill = 'white', size = 2, shape = 21) +
      scale_color_manual(values = c(love_color("deep_green"),
                                    love_color("deep_orange")))+
      geom_text(aes(y = as.numeric(ts_1$value) * max(ts_total$value) * 4,
                    label = paste(as_percent(ts_1$value, digits = 3))),
                position = position_dodge(width = 0.5),
                colour = 'black', size = 3, vjust = -0.1) +
      annotate(geom = 'text',
               x = dim(ts_total)[1] / 3,
               y = max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.09,
               label = paste('PSI', round(sum(tb_ks$PSI, na.rm = TRUE), 4), sep = " : ")) +
      scale_fill_manual(values = c('%train' = love_color("shallow_red2"),
                                   '%test' = love_color("green_cyan"))) +
      ylim(c(-0.01, max(c(ts_total$value, as.numeric(ts_1$value) * max(ts_total$value) * 4)) + 0.1)) +
      xlab(score) +
      ylab("% Population") +
      ggtitle(paste(gtitle, "Population Distribution")) +
      plot_theme(legend.position = "top",
                 angle = ifelse(nrow(ts_1) > 20, 50,
                                ifelse(nrow(ts_1) > 10, 30, 0)))
  }else{
    
    
    dis_pl =  ggplot(tb_ks, aes(x = bins, y = `%Pct`)) +
      geom_bar(aes(fill = "%total") ,stat = "identity", position = position_dodge(width = 0.7)) +
      geom_text(aes(y = `%Pct`, label = paste(as_percent(`%Pct`, digits = 3))),
                position = position_dodge(width = 0.7),
                size = 3, vjust = 1, hjust = 0.3, colour = "white") +
      geom_line(aes(x = factor(tb_ks[[1]]),
                    y = as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4,
                    color = "%flag_1"
      ),
      linetype = 1,
      group = 1,
      position = position_dodge(width = 0.5), size = 1) +
      geom_point(aes(y = as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4),
                 
                 color = love_color("deep_orange"),
                 group = 1,
                 position = position_dodge(width = 0.5),
                 fill = 'white', size = 2, shape = 21) +
      geom_text(aes(y = as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4,
                    label = paste(as_percent(tb_ks$`%Pct_1`, digits = 3))),
                position = position_dodge(width = 0.5),
                colour = 'black', size = 3, vjust = -0.1) +
      scale_fill_manual(values = c("%total" = love_color("deep_green"))) +
      scale_color_manual(values = c("%flag_1" = love_color("deep_orange"))) +
      ylim(c(-0.01, max(c(tb_ks$`%Pct`,  as.numeric(tb_ks$`%Pct_1`) * max(tb_ks$`%Pct`) * 4)) + 0.05)) +
      xlab(score) +
      ylab("% Population") +
      ggtitle(paste(gtitle, "Population Distribution")) +
      plot_theme(legend.position = "top",
                 angle = ifelse(nrow(tb_ks) > 10, 50,
                                ifelse(nrow(tb_ks) > 5, 30, 0)))
  }
  return(dis_pl)
  options(opt) # reset 
}


#' plot_oot_perf
#' \code{plot_oot_perf} is for ploting performance of cross time samples in the future
#' @param dat_test A data frame of testing dataset with predicted prob or score.
#' @param target The name of target variable.
#' @param x The name of prob or score variable.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.
#' @param best  Logical, merge initial breaks to get optimal breaks for binning.
#' @param equal_bins  Logical, generates initial breaks for equal frequency or width binning.
#' @param period OOT period, 'weekly' and 'month' are available.if NULL, use k equal frequency samples.
#' @param k  If period is NULL, number of equal frequency samples.
#' @param g Number of breaks for prob or score.
#' @param gtitle The title of the graph & The name for periodically saved graphic file.
#' @param breaks Splitting points of prob or score.
#' @param cut_bin A string, if equal_bins is TRUE, 'equal_depth' or 'equal_width', default is 'equal_depth'.
#' @param pl 'lift' is for lift chart plot,'rate' is for positive rate plot.
#' @param plot_show Logical, show model performance in current graphic device. Default is TRUE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param perf_dir_path The path for periodically saved graphic files.
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",x_list = x_list,
#' occur_time = "apply_date", miss_values = list("", -1))
#' dat = process_nas(dat)
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test

#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' plot_oot_perf(dat_test = dat_test, occur_time = "apply_date", target = "target", x = "pred_LR")
#' @import ggplot2
#' @importFrom data.table rbindlist
#' @export

plot_oot_perf = function(dat_test, x, occur_time, target, k = 3, g = 10, period = 'month',
                         best = FALSE, equal_bins = TRUE, pl = 'rate', breaks = NULL,
                         cut_bin = 'equal_depth', gtitle = NULL, perf_dir_path = NULL,
                         save_data = FALSE, plot_show = TRUE) {
  if (!(class(dat_test[, x]) %in% c("numeric", "double", "integer")) ||
      length(unique(dat_test[, x])) <= 10) {
    dat_test[, x] = as.character(dat_test[, x])
  }
  
  if (!is.null(period) && period == "weekly") {
    dat_test = time_transfer(dat_test, date_cols = "occur_time")
    dat_test$cohort_group = cut(as.Date(dat_test[, occur_time]), breaks = "week")
    unique_wk = sort(unique(dat_test$cohort_group))
    test_cv = list()
    for (w in 1:length(unique_wk)) {
      test_cv[[w]] = which(dat_test$cohort_group == unique_wk[w])
      if (length(unique(dat_test[test_cv[[w]], target])) == 1 |
          length(unique(dat_test[test_cv[[w]], occur_time])) < 3) {
        test_cv[[w]] = NA
      }
    }
  } else {
    if (!is.null(period) && period == "month") {
      dat_test = time_transfer(dat_test, date_cols = "occur_time")
      dat_test$cohort_group = as.character(cut(as.Date(dat_test[, occur_time]), breaks = "month"))
      unique_wk = sort(unique(dat_test$cohort_group))
      test_cv = list()
      for (w in 1:length(unique_wk)) {
        test_cv[[w]] = which(dat_test$cohort_group == unique_wk[w])
        if (length(unique(dat_test[test_cv[[w]], target])) == 1 |
            length(unique(dat_test[test_cv[[w]], occur_time])) < 3) {
          test_cv[[w]] = NA
        }
      }
    } else {
      test_cv = cv_split(dat_test, k = k, occur_time = occur_time)
    }
    
  }
  
  test_cv = test_cv[!is.na(test_cv)]
  if (is.null(breaks)) {
    ts_breaks = get_breaks(dat = dat_test, target = target, x = x,
                           best = best, equal_bins = equal_bins, g = g, cut_bin = cut_bin)
  }  
  cohort_group = c()
  perf_list = list()
  for (i in 1:length(test_cv)) {
    max_time = gsub("-", "/", as.character(max(dat_test[test_cv[[i]],][, occur_time])))
    min_time = gsub("-", "/", as.character(min(dat_test[test_cv[[i]],][, occur_time])))
    cohort_group[i] = paste(min_time,
                            substring(max_time, n_char(max_time) - 4, n_char(max_time)), sep = "_")
    perf_list[[i]] = cbind(cohort_group = cohort_group[i],
                           perf_table(train_pred = dat_test[test_cv[[i]],],
                                      target = target, score = x,
                                      breaks = ts_breaks))
    
  }
  
  cohort_dat = rbindlist(perf_list)
  bins_n = length(unique(as.character(cohort_group)))
  if (bins_n <= 4) {
    values = love_color(type = "Greens",n = 4)
  } else {
    if (bins_n <= 7) {
      values = love_color(type = "Blues",n = 7)
    } else {
      if (bins_n > 7 & bins_n <= 15) {
        values = love_color(type = "deep|light",n = 15)
      } else {
        if (bins_n > 15 & bins_n <= 21) {
          values = love_color(type = "deep|light|pale",n = 21)
        } else {
          if (bins_n > 21 & bins_n <= 27) {
            values = love_color(type = "deep|light|shallow|pale", n = 27)
          } else {
            values = love_color(type = "deep|light|shallow|pale", n = 100)
          }
        }
      }
    }
  }
  
  Lift = bins = `%Pct_1` = NULL
  
  if (pl == 'rate') {
    p1 = ggplot(cohort_dat, aes(as.character(bins), `%Pct_1`)) +
      geom_line(aes(group = as.character(cohort_group), colour = as.character(cohort_group)),
                size = 1.2) +
      geom_point(aes(x = as.character(bins),
                     y = `%Pct_1`, color = as.character(cohort_group)),
                 size = 1.3, shape = 21, fill = 'white') +
      scale_x_discrete(breaks = as.character(cohort_dat$bins)) +
      scale_colour_manual(values = values) +
      plot_theme() + xlab(x) +
      ylab("% Bad") + ggtitle(paste(gtitle, "% Bad of OOT samples"))
  } else {
    p1 = ggplot(cohort_dat, aes(as.character(bins), Lift)) +
      geom_line(aes(group = as.character(cohort_group), colour = as.character(cohort_group)),
                size = 1.2) +
      geom_point(aes(x = as.character(bins),
                     y = Lift, color = as.character(cohort_group)),
                 size = 1.3, shape = 21, fill = 'white') +
      scale_x_discrete(breaks = as.character(cohort_dat$bins)) +
      scale_colour_manual(values = values) +
      plot_theme() + xlab(x) +
      ylab("Lift") + ggtitle(paste(gtitle, "Lift of OOT samples"))
    
  }
  if (save_data) {
    perf_dir_path = ifelse(is.null(perf_dir_path), tempdir(), perf_dir_path)
    if (!dir.exists(perf_dir_path)) dir.create(perf_dir_path)
    
    ggsave(filename = paste0(perf_dir_path, "/", paste(paste0(gtitle, "_oot", "_", pl), "png", sep = '.')), device = "png",
           plot = p1, dpi = "retina", width = 5, height = 4.5)
  }
  if (plot_show) {
    plot(p1)
  }
  return(p1)
}


#' cohort_table_plot
#' \code{cohort_table_plot} is for ploting cohort(vintage) analysis table.
#'
#' This function is not intended to be used by end user.
#'
#' @param cohort_dat  A data.frame generated by \code{cohort_analysis}.
#' @import ggplot2
#' @export


cohort_table_plot = function(cohort_dat) {
  opt = options('warn' = -1, scipen = 200, stringsAsFactors = FALSE, digits = 6) #
  cohort_dat[is.na(cohort_dat)] = 0
  Cohort_Group = Cohort_Period = Events = Events_rate = Opening_Total = Retention_Total = cohor_dat = final_Events = m_a = max_age = NULL
  cohort_plot = ggplot(cohort_dat, aes(reorder(paste0(Cohort_Period), Cohort_Period),
                                       Cohort_Group, fill = Events_rate)) +
    geom_tile(colour = 'white') +
    geom_text(aes(label = as_percent(Events_rate, 4)), size = 3) +
    scale_fill_gradient2(limits = c(0, max(cohort_dat$Events_rate)),
                         low = love_color('deep_red'), mid = 'white',
                         high = love_color(),
                         midpoint = median(cohort_dat$Events_rate,
                                           na.rm = TRUE), 
                         na.value = love_color('pale_grey')) +
    scale_y_discrete(limits = rev(unique(cohort_dat$Cohort_Group))) +
    scale_x_discrete(position = "top") +
    labs(x = "Cohort_Period", title = "Cohort Analysis") +
    theme(text = element_text(size = 15), rect = element_blank()) +
    plot_theme(legend.position = 'right', angle = 0)
  return(cohort_plot)
  options(opt)
}


#' cohort_plot
#'
#' @rdname cohort_table_plot
#' @export

cohort_plot = function(cohort_dat){
  cohort_dat[is.na(cohort_dat)] = 0
  Cohort_Group= Cohort_Period =Events= Events_rate= Opening_Total= Retention_Total =cohor_dat= final_Events =m_a= max_age=NULL
  ggplot(cohort_dat,aes(Cohort_Period,Events_rate )) +
    geom_line(aes(group = Cohort_Group,colour = Cohort_Group),size =1.3 ) +
    geom_point(aes(x = Cohort_Period,
                   y = Events_rate, color =Cohort_Group),
               size = 1.3, shape = 21, fill = 'white') +
    scale_x_continuous(breaks = 1:max(cohort_dat$Cohort_Period)) +
    scale_colour_manual(values = unique(love_color(type = "darknihon_8x1|lightnihon_8x1|darknihon_6x1|darknihon_6x3|
                        lightnihon_4x1|lightnihon_6x1|lightnihon_9x1|darknihon_5x1|darknihon_5x2|
                        darknihon_6x2|lightnihon_7x1|darknihon_4x1")))+
    plot_theme()
}

#' plot_table
#'
#' \code{plot_table} is for table visualizaiton.
#' @param grid_table A data.frame or table
#' @param theme The theme of color, "cyan","grey","green","red","blue","purple" are available.
#' @param title The title of table
#' @param title.size The title size of plot.
#' @param title.color The title color.
#' @param title.face The title face, such as "plain", "bold".
#' @param title.position The title position,such as "left","middle","right".
#' @param subtitle The subtitle of table
#' @param subtitle.size The subtitle size.
#' @param subtitle.color The subtitle color.
#' @param subtitle.face The subtitle face, such as "plain", "bold",default is "bold".
#' @param subtitle.position The subtitle position,such as "left","middle","right", default is "middle".
#' @param tile.color The color of table lines, default is 'white'.
#' @param tile.size The size of table lines , default is 1.
#' @param colname.size The size of colnames, default is 3.
#' @param colname.color  The color of colnames, default is 'white'.
#' @param colname.face The face of colnames,default is 'bold'.
#' @param colname.fill.color  The fill color of colnames, default is love_color("dark_cyan").
#' @param text.size The size of text, default is 3.
#' @param text.color The color of text, default is love_color("dark_grey").
#' @param text.face The face of text, default is 'plain'.
#' @param text.fill.color The fill color of text, default is c('white',love_color("pale_grey").
#' @import ggplot2
#' @examples
#' iv_list = get_psi_iv_all(dat = UCICreditCard[1:1000, ],
#'                          x_list = names(UCICreditCard)[3:5], equal_bins = TRUE,
#'                          target = "default.payment.next.month", ex_cols = "ID|apply_date")
#' iv_dt =get_psi_iv(UCICreditCard, x = "PAY_3",
#'                   target = "default.payment.next.month", bins_total = TRUE)
#'
#' plot_table(iv_dt)
#' @export

plot_table = function (grid_table, theme = c("cyan", "grey", "green",
                                             "red", "blue", "purple"), title = NULL,
                       title.size = 12, title.color = "black", title.face = "bold",
                       title.position = "middle", subtitle = NULL, subtitle.size = 8,
                       subtitle.color = "black", subtitle.face = "plain",
                       subtitle.position = "middle", tile.color = "white",
                       tile.size = 1, colname.size = 3, colname.color = "white",
                       colname.face = "bold", colname.fill.color = love_color("dark_cyan"),
                       text.size = 3, text.color = love_color("dark_grey"),
                       text.face = "plain", text.fill.color = c("white",
                                                                love_color("pale_grey")))
{
  opt = options(warn = -1, scipen = 200, stringsAsFactors = FALSE,
                digits = 6)
  grid_table = rbind(colnames(grid_table), as.data.frame(grid_table))
  grid_table = cbind(rows = rownames(grid_table), as.data.frame(grid_table))
  grid_table = as.data.table(grid_table)
  table_1 = data.table::melt(grid_table, id = 1, measure = 2:ncol(grid_table),
                             value.factor = TRUE)
  grid_table = quick_as_df(grid_table)
  table_1 = data.frame(table_1, ind = paste0("x", 1:nrow(table_1)))
  table_theme = check_table_theme(theme = theme)
  if (!is.null(table_theme$colname.fill.color) && !is.null(table_theme$text.fill.color)) {
    colname.fill.color = table_theme$colname.fill.color
    text.fill.color = table_theme$text.fill.color
  }
  plot_ele = get_plot_elements(table_1 = table_1, grid_table = grid_table,
                               colname.fill.color = colname.fill.color, text.fill.color = text.fill.color,
                               colname.color = colname.color, text.color = text.color,
                               colname.face = colname.face, text.face = text.face, colname.size = colname.size,
                               text.size = text.size)
  fill_colors = plot_ele$fill_colors
  text_colors = plot_ele$text_colors
  fill_size = plot_ele$fill_size
  fill_bold = plot_ele$fill_bold
  value_width = plot_ele$value_width
  nudge_y = plot_ele$nudge_y
  table_2 = rearrange_table(table_1 = table_1, grid_table = grid_table,
                            colname.face = colname.face, colname.size = colname.size,
                            text.size = text.size)
  pl_tb = ggplot(table_2, aes(x = table_2$n_cumsum, y = table_2$rows)) +
    geom_tile(aes(fill = table_2$ind, width = table_2$n_width),
              height = value_width, show.legend = FALSE, colour = tile.color,
              size = tile.size) + geom_text(nudge_x = c(-(table_2$n_width[-c((nrow(table_2) -
                                                                                length(unique(table_2$rows)) + 1):nrow(table_2))]/4),
                                                        table_2$n_width[c((nrow(table_2) - length(unique(table_2$rows)) +
                                                                             1):nrow(table_2))] * 0), nudge_y = nudge_y, aes(label = table_2$value),
                                            size = fill_size, colour = text_colors, fontface = fill_bold) +
    labs(title = title, subtitle = subtitle, x = "",
         y = "") + scale_fill_manual(limits = table_2$ind,
                                     values = fill_colors) + scale_x_discrete(limits = c(1:max(table_2$n_cumsum) -
                                                                                           1)) + scale_y_discrete(limits = rev(unique(table_2$rows))) +
    theme(legend.position = "none", axis.ticks = element_blank(),
          axis.text = element_blank(), plot.title = element_text(face = title.face,
                                                                 size = title.size, color = title.color, hjust = ifelse(title.position ==
                                                                                                                          "middle", 0.5, ifelse(title.position ==
                                                                                                                                                  "right", 1, 0)), vjust = 0), plot.subtitle = element_text(face = subtitle.face,
                                                                                                                                                                                                            size = subtitle.size, color = subtitle.color,
                                                                                                                                                                                                            hjust = ifelse(subtitle.position == "middle",
                                                                                                                                                                                                                           0.5, ifelse(subtitle.position == "right",
                                                                                                                                                                                                                                       1, 0)), vjust = 0), rect = element_blank())
  return(pl_tb)
  options(opt)
}



rearrange_table = function(table_1, grid_table, colname.face, colname.size, text.size) {
  variable = n_char = n_width = rows = NULL
  
  table_1$value = sapply(table_1$value, function(x) {
    if (!is.na(x) && x %in% colnames(grid_table[1, -1])) {
      if (nrow(grid_table) > 10) {
        if (n_char(x) > 20 & n_char(x) <= 40) {
          x = paste(substr(x, 1, 20), substr(x, 21, n_char(x)), sep = "\n")
        } else {
          if (n_char(x) > 40 & n_char(x) <= 60) {
            x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, n_char(x)), sep = "\n")
          } else {
            if (n_char(x) > 60 & n_char(x) <= 80) {
              x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
                        substr(x, 61, n_char(x)), sep = "\n")
            } else {
              if (n_char(x) > 80 & n_char(x) <= 100) {
                x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
                          substr(x, 61, 80), substr(x, 81, n_char(x)), sep = "\n")
              } else {
                if (n_char(x) > 100 & n_char(x) <= 120) {
                  x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
                            substr(x, 61, 80), substr(x, 81, 100), substr(x, 101, n_char(x)), sep = "\n")
                } else {
                  if (n_char(x) > 120 & n_char(x) <= 140) {
                    x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
                              substr(x, 61, 80), substr(x, 81, 100), substr(x, 101, 120),
                              substr(x, 121, n_char(x)), sep = "\n")
                  } else {
                    if (n_char(x) > 140) {
                      x = paste(substr(x, 1, 20), substr(x, 21, 40), substr(x, 41, 60),
                                substr(x, 61, 80), substr(x, 81, 100), substr(x, 101, 120),
                                substr(x, 121, 140), substr(x, 141, n_char(x)), sep = "\n")
                    } else {
                      x
                    }
                  }
                }
              }
            }
          }
        }
      } else {
        if (n_char(x) > 25 & n_char(x) <= 50) {
          x = paste(substr(x, 1, 25), substr(x, 26, n_char(x)), sep = "\n")
        } else {
          if (n_char(x) > 50 & n_char(x) <= 75) {
            x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, n_char(x)), sep = "\n")
          } else {
            if (n_char(x) > 75 & n_char(x) <= 100) {
              x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, 75),
                        substr(x, 76, n_char(x)), sep = "\n")
            } else {
              if (n_char(x) > 100 & n_char(x) <= 125) {
                x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, 75),
                          substr(x, 76, 100), substr(x, 101, n_char(x)), sep = "\n")
              } else {
                if (n_char(x) > 125) {
                  x = paste(substr(x, 1, 25), substr(x, 26, 50), substr(x, 51, 75),
                            substr(x, 76, 100), substr(x, 101, 125), substr(x, 126, n_char(x)), sep = "\n")
                } else {
                  x
                }
              }
            }
          }
        }
      }
    } else {
      if (!is.na(x) && n_char(x) > 10 & n_char(x) <= 15) {
        x = paste(substr(x, 1, 8), substr(x, 9, n_char(x)), sep = "\n")
      } else {
        if (!is.na(x) && n_char(x) > 15) {
          x = paste(substr(x, 1, 12), substr(x, 13, n_char(x)), sep = "\n")
        } else {
          x
          
        }
      }
    }
    
  })
  table_2 = table_1 %>%
    dplyr::mutate(n_char = unlist(
      sapply(table_1$value,
             function(x) ifelse(colname.face == 'bold' &
                                  x %in% colnames(grid_table[1, -1]) & colname.size - text.size == 0,
                                n_char(strsplit(x, "\n")[[1]][1]) + 0,
                                ifelse(colname.face == 'bold' &
                                         colname.size - text.size != 0 & x %in% colnames(grid_table[1, -1]),
                                       n_char(strsplit(x, "\n")[[1]][1]) + 0,
                                       ifelse(colname.face != 'bold' & colname.size - text.size != 0 &
                                                x %in% colnames(grid_table[1, -1])
                                              , n_char(strsplit(x, "\n")[[1]][1]) + 0,
                                              ifelse(colname.face == 'bold' &
                                                       x %in% colnames(grid_table[1, -1]) & colname.size - text.size == 0,
                                                     n_char(strsplit(x, "\n")[[1]][1]) + 0,
                                                     ifelse(colname.face == 'bold' | colname.size - text.size != 0,
                                                            n_char(strsplit(x, "\n")[[1]][1]) +0,
                                                            n_char(strsplit(x, "\n")[[1]][1]))))))))) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(n_width = max(n_char, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(rows) %>%
    dplyr::mutate(n_cumsum = base::cumsum(n_width)/2) %>%
    dplyr::ungroup()
  return(table_2)
}

check_table_theme = function(theme = NULL) {
  colname.fill.color = text.fill.color = NULL
  if (length(theme) > 0 &&
      any(sapply(theme, function(x) is.element(x, c("cyan", "grey", "green", "red", "blue", "purple"))))) {
    if (theme[1] == 'cyan') {
      dark_cyan = rgb(40, 70, 100, maxColorValue = 255)
      shallow_cyan = rgb(200, 230, 245, maxColorValue = 255)
      pale_cyan = rgb(220, 240, 255, maxColorValue = 255)
      colname.fill.color = dark_cyan
      text.fill.color = c(pale_cyan, shallow_cyan)
    } else {
      if (theme[1] == 'grey') {
        dark_grey = rgb(102, 102, 102, maxColorValue = 255)
        shallow_grey = rgb(225, 226, 225, maxColorValue = 255)
        pale_grey = rgb(240, 240, 240, maxColorValue = 255)
        colname.fill.color = dark_grey
        text.fill.color = c(pale_grey, shallow_grey)
        
      } else {
        if (theme[1] == 'red') {
          dark_red = rgb(180, 40, 40, maxColorValue = 255)
          shallow_red = rgb(255, 230, 230, maxColorValue = 255)
          pale_red = rgb(255, 242, 242, maxColorValue = 255)
          colname.fill.color = dark_red
          text.fill.color = c(pale_red, shallow_red)
        } else {
          if (theme[1] == 'blue') {
            deep_blue = rgb(80, 99, 139, maxColorValue = 255)
            shallow_blue = rgb(210, 232, 255, maxColorValue = 255)
            pale_blue = rgb(227, 240, 255, maxColorValue = 255)
            colname.fill.color = deep_blue
            text.fill.color = c(pale_blue, shallow_blue)
          } else {
            if (theme[1] == 'green') {
              deep_green = rgb(50, 150, 150, maxColorValue = 255)
              shallow_green = rgb(200, 240, 240, maxColorValue = 255)
              pale_green = rgb(225, 255, 255, maxColorValue = 255)
              colname.fill.color = deep_green
              text.fill.color = c(pale_green, shallow_green)
            } else {
              if (theme[1] == 'purple') {
                dark_purple = rgb(180, 50, 105, maxColorValue = 255)
                shallow_purple = rgb(245, 225, 245, maxColorValue = 255)
                pale_purple = rgb(255, 240, 255, maxColorValue = 255)
                colname.fill.color = dark_purple
                text.fill.color = c(pale_purple, shallow_purple)
              }
              
            }
          }
        }
      }
    }
  }
  return(list(colname.fill.color = colname.fill.color, text.fill.color = text.fill.color))
}

get_plot_elements = function(table_1, grid_table, colname.fill.color, text.fill.color,
                              colname.color, text.color, colname.face, text.face, colname.size, text.size) {
  fill_colors = c()
  m = 0
  for (i in 1:length(table_1$value)) {
    if (nrow(grid_table) %% 2 == 0) {
      m = m + 1
    }
    if (table_1$value[i] %in% colnames(grid_table[1, -1])) {
      
      fill_colors = c(fill_colors, colname.fill.color[1])
    } else {
      if (nrow(grid_table) %% 2 != 0) {
        m = m + 1
      }
      if (length(text.fill.color) > 1) {
        if (m %% 2 == 0) {
          fill_colors = c(fill_colors, text.fill.color[1])
        } else {
          
          fill_colors = c(fill_colors, text.fill.color[2])
        }
        
      } else {
        fill_colors = c(fill_colors, text.fill.color[1])
        
      }
    }
  }
  
  text_colors = c()
  for (i in 1:length(table_1$value)) {
    if (table_1$value[i] %in% colnames(grid_table[1, -1])) {
      
      text_colors = c(text_colors, colname.color)
    } else {
      text_colors = c(text_colors, text.color)
    }
  }
  
  fill_size = c()
  for (i in 1:length(table_1$value)) {
    if (table_1$value[i] %in% colnames(grid_table[1, -1])) {
      
      fill_size = c(fill_size, colname.size)
    } else {
      fill_size = c(fill_size, text.size)
    }
  }
  
  fill_bold = c()
  for (i in 1:length(table_1$value)) {
    if (table_1$value[i] %in% colnames(grid_table[1, -1])) {
      fill_bold = c(fill_bold, colname.face)
    } else {
      fill_bold = c(fill_bold, text.face)
    }
  }
  value_width = c()
  for (i in 1:length(table_1$value)) {
    if (table_1$value[i] %in% colnames(grid_table[1, -1]) && any(n_char(colnames(grid_table[1, -1])) > 10)) {
      value_width = c(value_width, 2)
    } else {
      
      value_width = c(value_width, 1)
    }
  }
  nudge_y = c()
  for (i in 1:length(table_1$value)) {
    if (table_1$value[i] %in% colnames(grid_table[1, -1]) && any(n_char(colnames(grid_table[1, -1])) > 10)) {
      nudge_y = c(nudge_y, 0.05)
    } else {
      
      nudge_y = c(nudge_y, 0)
    }
  }
  
  return(list(fill_colors = fill_colors, text_colors = text_colors, fill_size = fill_size,
              fill_bold = fill_bold, value_width = value_width, nudge_y = nudge_y))
}

#' @title Arrange list of plots into a grid
#' @name multi_grid
#'
#' @description Plot multiple ggplot-objects as a grid-arranged single plot.
#'
#' @param grobs A list of ggplot-objects to be arranged into the grid.
#' @param nrow  Number of rows in the plot grid.
#' @param ncol Number of columns in the plot grid.
#' @param ... Other parameters.
#'
#' @return An object of class \code{gtable}.
#'
#' @details This function takes a \code{list} of ggplot-objects as argument.
#'          Plotting functions of this package that produce multiple plot
#'          objects (e.g., when there is an argument \code{facet.grid}) usually
#'          return multiple plots as list.
#'
#' @examples
#' library(ggplot2)
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values = list("", -1))
#' dat = process_nas(dat)
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#'
#' dat_train$pred_LR = round(predict(lr_model, dat_train[, x_list], type = "response"), 5)
#' dat_test$pred_LR = round(predict(lr_model, dat_test[, x_list], type = "response"), 5)
#' # model evaluation
#' p1 =  ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' p2 =  roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' p3 =  lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR")
#' p4 = score_distribution_plot(train_pred = dat_train, test_pred = dat_test,
#' target = "target", score = "pred_LR")
#' p_plots= multi_grid(p1,p2,p3,p4)
#' plot(p_plots)
#' @export

multi_grid = function(..., grobs = list(...),
                       nrow = NULL, ncol = NULL) {
  n = length(grobs)
  if (is.null(nrow) && !is.null(ncol)) {
    nrow = ceiling(n / ncol)
  }
  if (is.null(ncol) && !is.null(nrow)) {
    ncol = ceiling(n / nrow)
  }
  stopifnot(nrow * ncol >= n)
  if (is.null(nrow) && is.null(ncol)) {
    if (n <= 3) {
      nrow = n
      ncol = 1
    } else {
      if (n <= 6) {
        nrow = (n + 1) %/% 2
        ncol = 2
      } else {
        if (n <= 12) {
          nrow = (n + 2) %/% 3
          ncol = 3
        } else {
          nrow = ceiling(sqrt(n))
          ncol = ceiling(n / nrow)
        }
      }
    }
  }
  inherit.ggplot = unlist(lapply(grobs, inherits, what = "ggplot"))
  
  if (any(inherit.ggplot)) {
    stopifnot(requireNamespace("ggplot2", quietly = TRUE))
    toconv = which(inherit.ggplot)
    grobs[toconv] = lapply(grobs[toconv], ggplot2::ggplotGrob)
  }
  positions = expand.grid(t = seq_len(nrow),
                          l = seq_len(ncol))
  positions$b = positions$t
  positions$r = positions$l
  positions = positions[order(positions$t),]
  positions = positions[seq_along(grobs),]
  ## sizes
  widths = unit(rep(1, ncol), "null")
  heights = unit(rep(1, nrow), "null")
  
  grob_table = to_gtable(name = "arrange",
                         
                         heights = heights,
                         widths = widths)
  
  grob_table = add_grobs(x = grob_table, grobs,
                         t = positions$t,
                         b = positions$b,
                         l = positions$l,
                         r = positions$r,
                         z = seq_along(grobs),
                         clip = "off")
  grob_table
}

to_gtable = function(widths = list(), heights = list(),
                     name = "layout") {
  layout = new_data_frame(list(t = numeric(), l = numeric(),
                               b = numeric(), r = numeric(), z = numeric(), clip = character(),
                               name = character()), n = 0)
  grid::gTree(grobs = list(), layout = layout, widths = widths, heights = heights,
              respect = FALSE, name = name, rownames = NULL,
              colnames = NULL, vp = NULL, cl = "gtable")
}


add_grobs = function(x, grobs, t, l, b = t, r = l, z = Inf, clip = "off",
                      name = x$name) {
  
  n_grobs = length(grobs)
  layout = unclass(x$layout)
  z = rep(z, length.out = n_grobs)
  zval = c(layout$z, z[!is.infinite(z)])
  if (length(zval) == 0) {
    zmin = 1
    zmax = 0
  } else {
    zmin = min(zval)
    zmax = max(zval)
  }
  z[z == -Inf] = zmin - rev(seq_len(sum(z == -Inf)))
  z[z == Inf] = zmax + seq_len(sum(z == Inf))
  x_row = length(x$heights)
  x_col = length(x$widths)
  t = rep(neg_to_pos(t, x_row), length.out = n_grobs)
  b = rep(neg_to_pos(b, x_row), length.out = n_grobs)
  l = rep(neg_to_pos(l, x_col), length.out = n_grobs)
  r = rep(neg_to_pos(r, x_col), length.out = n_grobs)
  clip = rep(clip, length.out = n_grobs)
  name = rep(name, length.out = n_grobs)
  x$grobs = c(x$grobs, grobs)
  x$layout = new_data_frame(list(t = c(layout$t, t), l = c(layout$l,
                                                           l), b = c(layout$b, b),
                                 r = c(layout$r, r), z = c(layout$z, z),
                                 clip = c(layout$clip, clip), name = c(layout$name,
                                                                       name)))
  x
}

neg_to_pos = function(x, max) {
  ifelse(x >= 0, x, max + 1 + x)
}

new_data_frame = function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
  lengths = vapply(x, length, integer(1))
  if (is.null(n)) {
    n = if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
    x[[i]] = rep(x[[i]], n)
  }
  
  class(x) = "data.frame"
  
  attr(x, "row.names") = .set_row_names(n)
  x
}





outlier_fuc = function(x, m = 1) {
  if (class(x) == 'numeric' | class(x) == 'integer') {
    QL = quantile(x, 0.01, na.rm = TRUE)
    QU = quantile(x, 0.99, na.rm = TRUE)
    QU_QL = QU - QL
    if (QU_QL == 0) {
      out_imp01 = quantile(x, 0.9999, na.rm = TRUE)
    } else {
      out_imp01 = round(QU + m * QU_QL, 0)
    }
    x[x > out_imp01] = out_imp01
  }
  return(x)
}


#' Plot Colors
#'
#' You can use the \code{plot_colors} to show colors on the graph device.
#' @param colors A vector of colors.
#' @examples
#' plot_colors(rgb(158,122,122, maxColorValue = 255 ))
#' @importFrom graphics image
#' @importFrom grDevices colorRamp
#' @export
plot_colors = function(colors){
  image(seq_along(colors) ,1,as.matrix(seq_along(colors)),col=colors,
        ylab="",xlab="",yaxt="n",xaxt="n",bty="n")
}

#' @rdname plot_colors
#' @export
color_ramp_palette = function (colors) {
   ramp <- colorRamp(colors)
   function(n) {
      x <- ramp(seq.int(0, 1, length.out = n))
      if (ncol(x) == 4L) 
         rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
      else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
   }
}


