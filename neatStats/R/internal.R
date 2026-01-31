# globals
pkg.globals = new.env()
pkg.globals$my_unique_grouping_var = NULL
pkg.globals$my_unique_method = NULL
pkg.globals$my_unique_first_iter = NULL

# the function below is to be added later
# to_clipboard = function( printing_function ) {
#    text_for_cb = capture.output( printing_function )
#    write.table( text_for_cb, "clipboard", quote = FALSE, row.names = FALSE, col.names = FALSE )
# }

# misc local functions below

quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

prnt = function(...) {
    to_print = gsub('-', 'CHAR_MINUS', paste0(...))
    to_print = sub("e\\+0*", " CHAR_X 10^", to_print)
    to_print = gsub("p = 0.", "p = .", to_print)
    to_print = gsub(" = .000", " < .001", to_print)
    change_pairs = list(
        c('CHAR_MINUS', '\u2013'),
        c('CHAR_PLUSMIN', '\u00b1'),
        c('CHAR_X', '\u00d7'),
        c('CHAR_ETA', '\u03b7'),
        c('CHAR_EPS', '\u03b5'),
        c('CHAR_INF', '\u221e')
    )
    Encoding(to_print) = "UTF-8"
    for (pair in change_pairs) {
        to_print = gsub(pair[1], pair[2], to_print)
        Encoding(to_print) = "UTF-8"
    }
    cat(to_print, fill = TRUE)
}

to_exp = function(the_num) {
    if (as.numeric(ro(the_num, 2)) >= 10000) {
        the_num = formatC(the_num, format = "e", digits = 2)
        return(the_num)
    } else {
        return(ro(the_num, 2))
    }
}

bf_neat = function(bf) {
    if (is.na(bf)) {
        message("BF = NA")
        return(".")
    } else {
        if (bf < 1) {
            bf2 = 1 / bf
            bf2 = to_exp(bf2)
            bf_dir = paste0(", BF01 = ", bf2)
        } else {
            bf2 = to_exp(bf)
            bf_dir = paste0(", BF10 = ", bf2)
        }
        return(paste0(bf_dir, "."))
        # return(paste0(bf_dir, ". (BFplain = ", ro(bf, 4), ")"))
    }
}

bf_names = function(the_names) {
    new_names = c()
    for (a_name in the_names) {
        a_name = gsub(" .*", "", a_name)
        a_name = sort(strsplit(a_name, ":")[[1]])
        a_name = paste(a_name, collapse = " CHAR_X ")
        new_names = c(new_names, a_name)
    }
    return(new_names)
}

docv_auc = function(v_large,
                    v_small,
                    cv_rep,
                    cv_fold) {
    allvs = sort(c(v_large, v_small))
    allths =  rep(NA, length(allvs))
    for (th_i in 2:length(allvs)) {
        allths[th_i] = mean(allvs[(th_i - 1):th_i])
    }
    allths[1] = -Inf
    allths = c(allths, Inf)

    cv_vec = 1:cv_fold
    all_tps = c()
    all_tns = c()
    optimal_ths = c()
    for (i in 1:cv_rep) {
        v_L = split(sample(v_large), cv_vec)
        v_S = split(sample(v_small), cv_vec)
        maxtps = c()
        maxtns = c()
        maxths = c()
        for (cvnum in cv_vec) {
            maxsum = 0
            for (thres in allths) {
                trainL = unlist(v_L[-cvnum])
                trainS = utils::head(unlist(v_S[-cvnum]), length(trainL))
                trainL = utils::head(trainL, length(trainS))
                newsum = sum(trainL > thres) / length(trainL) + sum(trainS < thres) / length(trainS)
                if (newsum > maxsum) {
                    maxsum = newsum
                    thethres = thres
                }
            }
            maxtps = c(maxtps, sum(v_L[[cvnum]] > thethres) / length(v_L[[cvnum]]))
            maxtns = c(maxtns, sum(v_S[[cvnum]] < thethres) / length(v_S[[cvnum]]))
            maxths = c(maxths, thethres)
        }
        all_tps = c(all_tps, mean(maxtps))
        all_tns = c(all_tns, mean(maxtns))
        optimal_ths = c(optimal_ths, mean(maxths))
    }
    return(list(
        TPRs = all_tps,
        TNRs = all_tns,
        thresholds = optimal_ths
    ))
}

show_auc = function(theroc,
                    ci = 0.95,
                    round_to = 3,
                    for_table = FALSE,
                    thres = NULL,
                    best_tp = NULL,
                    best_tn = NULL,
                    sd_tp = '',
                    sd_tn = '',
                    sd_th = '') {
    if (for_table == TRUE) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
    }
    auc_num = edges(pROC::auc(theroc), round_to)
    auc_ci = as.numeric(pROC::ci.auc(theroc, conf.level = ci))
    lower = edges(auc_ci[1], round_to)
    upper = edges(auc_ci[3], round_to)
    thres = ro(thres, round_to)
    best_tp = edges(best_tp, round_to)
    best_tn = edges(best_tn, round_to)
    if (thres == Inf | thres == -Inf) {
        rates_optim = " (below chance level)"
    } else {
        rates_optim = paste0(
            " (TPR = ",
            best_tp,
            sd_tp,
            ", TNR = ",
            best_tn,
            sd_tn,
            ", with the optimal cutoff ",
            thres,
            sd_th,
            ")"
        )
    }
    prnt("AUC = ",
         auc_num,
         ci_disp,
         " [",
         lower,
         ", ",
         upper,
         "]",
         rates_optim)
}

edges = function(the_num, round_to, no_null = FALSE) {
    if (round(the_num, round_to) == 1) {
        return("1")
    } else if (round(the_num, round_to) == -1) {
        return("-1")
    } else if (round(the_num, round_to) == 0 & no_null == FALSE) {
        return("0")
    } else {
        return(sub("0\\.", "\\.", ro(the_num, round_to)))
    }
}

get_e_corrs = function(mauchly, e_corrects, e_correction, shush) {
    e_corrs_list = list()
    for (indx in 1:length(e_corrects$Effect)) {
        s_name = e_corrects$Effect[indx]
        s_name = sort(strsplit(s_name, ":")[[1]])
        s_name = paste(s_name, collapse = " CHAR_X ")
        e_corrs_list[[s_name]] = c(
            gge = e_corrects$GGe[indx],
            ggp = e_corrects$'p[GG]'[indx],
            hfe = e_corrects$HFe[indx],
            hfp = e_corrects$'p[HF]'[indx]
        )
    }
    spher_real_corrs = list()
    Wval_list = list()
    pval_list = list()
    correction_type_list = list()
    for (indx in 1:length(mauchly$Effect)) {
        m_name = mauchly$Effect[indx]
        m_name = sort(strsplit(m_name, ":")[[1]])
        m_name = paste(m_name, collapse = " CHAR_X ")
        m_w = mauchly$W[indx]
        m_pval = mauchly$p[indx]

        if (e_correction == 'none') {
            m_corr = '.'
        } else if (e_correction == 'gg') {
            m_corr = '. Correction: Greenhouse-Geisser.'
            spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['gge']),
                                           pval = as.numeric(e_corrs_list[[m_name]]['ggp']))

        } else if (e_correction == 'hf') {
            m_corr = '. Correction: Huynh-Feldt.'
            spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['hfe']),
                                           pval = as.numeric(e_corrs_list[[m_name]]['hfp']))
        } else if (round(m_pval, 3) < 0.05) {
            this_gge = as.numeric(e_corrs_list[[m_name]]['gge'])
            if (round(this_gge, 3) > 0.75) {
                m_corr = '. Correction: Huynh-Feldt (Greenhouse-Geisser CHAR_EPS > 0.75).'
                spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['hfe']),
                                               pval = as.numeric(e_corrs_list[[m_name]]['hfp']))

            } else {
                m_corr = '. Correction: Greenhouse-Geisser (CHAR_EPS <= 0.75).'
                spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['gge']),
                                               pval = as.numeric(e_corrs_list[[m_name]]['ggp']))
            }
        } else {
            m_corr = '.'
        }
        x_name = gsub(" CHAR_X ", "_x_", m_name)
        Wval_list[[x_name]] = m_w
        pval_list[[x_name]] = m_pval
        correction_type_list[[x_name]] = ifelse(
            grepl('Huynh', m_corr, fixed = TRUE),
            "Huynh-Feldt",
            ifelse(
                grepl('Greenhouse', m_corr, fixed = TRUE),
                "Greenhouse-Geisser",
                "none"
            )
        )
        if (shush == FALSE) {
            prnt(m_name,
                 ': W = ',
                 ro(m_w, 3),
                 ', p = ',
                 ro(m_pval, 3),
                 m_corr)
        }
    }
    return(
        list(
            Wval = Wval_list,
            pval = pval_list,
            correction_type = correction_type_list,
            eps_p_corrs = spher_real_corrs
        )
    )
}

to_fact = function(var) {
    return(as.factor(tolower(as.character(var))))
}

to_c = function(var) {
    var = gsub("\\s", "", var)
    return(strsplit(var, ",")[[1]])
}

checkcol = function(df_names, thecols) {
    cols_notfound = c()
    for (colname in thecols) {
        if (!colname %in% df_names) {
            cols_notfound = c(cols_notfound, colname)
        }
    }
    if (length(cols_notfound) > 0) {
        if (length(cols_notfound) ==  1) {
            stop(
                'The column "',
                cols_notfound,
                '" was not found in the data frame. Perhaps check for spelling mistakes.'
            )
        } else {
            stop(
                'The following columns were not found in the data frame: "',
                paste(cols_notfound,
                      collapse = '", "'),
                '". Perhaps check for spelling mistakes.'
            )
        }
    }
}



transp = function(to_transpose, headers) {
    if (headers == TRUE) {
        headers = 'aggr_group'
    }
    if (inherits(to_transpose, "list")) {
        to_transpose = Reduce(function(x, y)
            merge(x, y, all = TRUE), to_transpose)
    }
    hnames = to_transpose[[headers]]
    tdat = as.data.frame(t(to_transpose[, -1]))
    colnames(tdat) = hnames
    return(tdat)
}


get_row = function(...) {
    dotdot = c(as.list(environment()), list(...))
    dotnames = names(dotdot)
    if (is.null(dotnames)) {
        dotnames = ''
    }
    newrow = data.table(....temPR = 1)
    for (itnum in seq_along(dotdot)) {
        itname = dotnames[itnum]
        addee = dotdot[[itnum]]
        if (itname != "" & is.atomic(addee) & length(addee) == 1) {
            newrow[[itname]] = addee
        } else {
            if (is.atomic(addee) | inherits(addee, "list")) {
                if (length(names(addee)[(names(addee) != "")]) != length(addee)) {
                    print(addee)
                    stop(
                        "Missing vector names!\n",
                        'Each addition (... argument) must be one of the following: ',
                        'a data frame (either single row or two column); ',
                        'a list or a vector with single elements; ',
                        'or a single value with parameter name ',
                        '(e.g. date = 1989 or id = "jdoe").'
                    )
                }
                newrow = data.table(newrow, as.data.table(as.list(addee)))
            } else if (inherits(addee, "data.frame")) {
                if (nrow(addee) == 0) {
                    print(addee)
                    stop('Data frame should not be empty.')
                } else if (nrow(addee) > 1) {
                    if (ncol(addee) == 2) {
                        hnames = as.character(addee[, 1])
                        addee = as.data.table(t(as.vector(addee[, 2])))
                        colnames(addee) = hnames
                    } else {
                        print(names(addee))
                        stop('Data frame with multiple rows must have two columns.')
                    }
                }
                newrow = data.table(newrow, addee)
            } else {
                print(addee)
                stop(
                    'Each addition (... argument) must be one of the following: ',
                    'a data frame (either single row or two column); ',
                    'a list or a vector with single elements; ',
                    'or a single value with parameter name ',
                    '(e.g. date = 1989 or id = "jdoe").'
                )
            }
        }
    }
    newrow$....temPR = NULL
    return(newrow)
}

mains_ebs = function(data_long, method, eb_method, g_by) {
    fact_names = to_c(g_by)
    g_by_text = paste0('with(data = data_long, list(',
                       g_by,
                       '))')
    group_by = eval(parse(text = g_by_text))
    if (is.null(eb_method)) {
        eb_method2 = mean
    } else {
        eb_method2 = eb_method
    }
    to_plot = do.call(
        data.frame,
        stats::aggregate(
            data_long$..neat_values,
            by = group_by,
            FUN = function(x) {
                c(main = method(x), eb = eb_method2(x))
            }
        )
    )
    for (c_name in names(to_plot)) {
        if (!c_name %in% c('x.main', 'x.eb')) {
            to_plot[[c_name]] = as.character(to_plot[[c_name]])
        }
    }
    return(to_plot)
}
re_n = function(name, n_dict) {
    return(if (is.null(n_dict) ||
               is.na(n_dict[name]))
        name
        else
            n_dict[name])
}

name_taken = function(name, dat) {
    if (name %in%  names(dat)) {
        stop(
            'Sorry, the name "',
            name,
            '" is reserved for this function. Remove or rename that column.'
        )
    }
}

## parameter argument valudations

validate_args = function(func_used, evaled_args) {
    feedback = ''
    for (part_feed in evaled_args) {
        feedback = paste0(feedback, part_feed)
    }
    if (feedback != '') {
        func_used = gsub("\\s+", " ", paste(deparse(func_used), collapse = " "))
        feedback = paste0(
            "Arguments are not correct in the '",
            func_used,
            "' function:",
            feedback,
            '\n... Hint: enter help(',
            strsplit(func_used, "\\(")[[1]][1],
            ') for detailed function info.'
        )
        stop(feedback, call. = FALSE)
    }
}

val_arg = function(arg_val,
                   req_types,
                   req_length = 99,
                   # 0 means multiple, 1 means single, all else passes
                   opts = NULL) {
    failed = FALSE
    arg_name = paste(deparse(substitute(arg_val)), collapse = "")
    if (length(arg_val) > 1) {
        if (req_length == 1 &&
            !is.list(arg_val) && !is.data.frame(arg_val)) {
            failed = TRUE
        }
    } else if (req_length == 0) {
        failed = TRUE
    }
    valid_types = c('char', 'num', 'bool', 'null', 'df', 'list', 'function')
    if (!all(req_types %in% valid_types)) {
        stop(
            'invalid req_types: ',
            paste(req_types, collapse = ', '),
            '\nshould be: ',
            paste(valid_types, collapse = ', ')
        )
    }
    req_types = replace(req_types, req_types == 'char', 'character')
    req_types = replace(req_types, req_types == 'num', 'double')
    req_types = replace(req_types, req_types == 'bool', 'logical')
    req_types = replace(req_types, req_types == 'null', 'NULL')
    req_types = replace(req_types, req_types == 'df', 'data.frame')
    if ((!typeof(arg_val) %in% req_types)
        && (!('data.frame' %in% req_types &&
              is.data.frame(arg_val)))
        && (!('function' %in% req_types &&
              is.function(arg_val)))
        && (!('double' %in% req_types &&
              is.integer(arg_val))) &&
        (!('list' %in% req_types &&
           is.list(arg_val)))) {
        failed = TRUE
    } else if (typeof(arg_val) == 'character' &&
               (!is.null(opts)) && (!arg_val %in% opts)) {
        failed = TRUE
    }
    if (failed == TRUE) {
        req_types = replace(req_types,
                            req_types == 'character',
                            '"character" (string)')
        req_types = replace(req_types, req_types == 'double', '"double" (numeric)')
        req_types = replace(req_types,
                            req_types == 'logical',
                            '"logical" (boolean)')
        req_types = replace(req_types, req_types == 'data.frame', '"data.frame"')
        if (!is.null(opts)) {
            if (suppressWarnings(all(!is.na(as.numeric(opts))))) {
                opts_add = paste0(
                    ' The only acceptable strings or numbers are "',
                    paste(opts, collapse = '", or "'),
                    '".'
                )
            } else {
                opts_add = paste0(
                    ' The only acceptable strings are "',
                    paste(opts, collapse = '", or "'),
                    '".'
                )
            }
        } else {
            opts_add = ''
        }
        if (req_length == 1) {
            len_add = ' must not be a vector, and'
        } else if (req_length == 0) {
            len_add = ' must be a vector, and'
        } else {
            len_add = ''
        }
        arg_msg = paste0(
            '\nThe argument "',
            arg_name,
            '"',
            len_add,
            ' must be ',
            paste(req_types, collapse = ', or '),
            '.',
            opts_add
        )
        return(arg_msg)
    } else {
        return('')
    }
}

val_wi_id = function(func_used, id_arg, val_cols) {
    feedback = ''
    dups = unique(val_cols[duplicated(val_cols)])
    if (length(dups) > 0) {
        feedback = paste0(
            feedback,
            '\nYou have duplicate column names for "values": ',
            paste0(dups, collapse = ", "),
            '.'
        )
    }
    val_levels = c()
    func_used = gsub("\\s+", " ", paste(deparse(func_used), collapse = " "))
    if (is.list(id_arg)) {
        vals_num = length(val_cols)
        w_facts_num = length(id_arg)
        if (2 ** w_facts_num > vals_num) {
            feedback = paste0(
                feedback,
                '\nYou specified ',
                w_facts_num,
                ' within-subject factors for "within_ids". This means there must be at least ',
                2 ** w_facts_num,
                ' values columns specified, but you only specified ',
                vals_num,
                '.'
            )
        }
        for (val_name in val_cols) {
            val_levels[val_name] = ''
            for (fact_name in names(id_arg)) {
                fact_ids = id_arg[[fact_name]]
                if (length(fact_ids) <= 1) {
                    feedback = paste0(
                        feedback,
                        '\nAll within-subject factors must have at least two levels. (Check "',
                        fact_name,
                        '").'
                    )
                } else {
                    id_count = 0
                    for (f_id in fact_ids) {
                        if (grepl(f_id, val_name, fixed = TRUE)) {
                            id_count = id_count + 1
                            val_levels[val_name] = paste0(val_levels[val_name], f_id)
                        }
                    }
                    if (id_count == 0) {
                        feedback = paste0(
                            feedback,
                            '\nNo matching level found for "',
                            val_name,
                            '" for factor "',
                            fact_name,
                            '".'
                        )
                    } else if (id_count > 1) {
                        feedback = paste0(
                            feedback,
                            '\nMore than one matching level found for "',
                            val_name,
                            '" for factor "',
                            fact_name,
                            '". (This means that the specified factor name text is ambiguous,',
                            ' see "within_ids" in documentation e.g. by entering ?anova_neat.',
                            ' Try different naming for level specification, ',
                            'or change column names.)'
                        )
                    }
                }
            }
        }
    }
    if (feedback != '') {
        feedback = paste0(
            "Arguments are not correct in the '",
            func_used,
            "' function:",
            feedback,
            '\n... Hint: enter help(',
            gsub('"', '', strsplit(func_used, "\\(")[[1]][1]),
            ') for detailed function info.'
        )
        stop(feedback, call. = FALSE)
    }
    return(val_levels)
}


## all below: rank BF functions from J. van Doorn

# signRankSampler

signRankGibbsSampler <-
    function(xVals,
             yVals = NULL,
             nSamples = 1e3,
             cauchyPriorParameter = 1 / sqrt(2),
             testValue = 0,
             progBar = TRUE,
             nBurnin = 1,
             nGibbsIterations = 10,
             nChains = 5) {
        if (progBar) {
            myBar <-
                utils::txtProgressBar(
                    min = 1,
                    max = nSamples * nChains,
                    initial = 1,
                    char = "*",
                    style = 3,
                    width = 50
                )
        }

        n <- length(xVals)

        if (!is.null(yVals)) {
            differenceScores <- xVals - yVals
        } else {
            differenceScores <- xVals - testValue
        }

        differenceSigns <- (sign(differenceScores))
        absDifferenceRanked <- rank(abs(differenceScores))
        prodSignAbsRank <- differenceSigns * absDifferenceRanked

        initDiffSamples <-
            sort(abs(stats::rnorm(n)))[absDifferenceRanked]
        sampledDiffsAbs <- abs(initDiffSamples)
        diffSamples <- numeric(n)


        deltaSamples <- numeric(nSamples)
        deltaSamplesMatrix <-
            matrix(ncol = nChains, nrow = nSamples - nBurnin)
        oldDeltaProp <- 0

        for (thisChain in 1:nChains) {
            for (j in 1:nSamples) {
                for (i in sample(1:n)) {
                    currentRank <- absDifferenceRanked[i]

                    currentBounds <-
                        upperLowerTruncation(
                            ranks = absDifferenceRanked,
                            values = sampledDiffsAbs,
                            currentRank = currentRank
                        )
                    if (is.infinite(currentBounds[["under"]])) {
                        currentBounds[["under"]] <- 0
                    }

                    sampledDiffsAbs[i] <-
                        truncNormSample(
                            currentBounds[["under"]],
                            currentBounds[["upper"]],
                            mu = abs(oldDeltaProp),
                            sd = 1
                        )

                }

                diffSamples <- sampledDiffsAbs * differenceSigns

                if (any(differenceSigns == 0)) {
                    nullSamples <-
                        sampledDiffsAbs[differenceSigns == 0] * sample(c(-1, 1),
                                                                       size = sum(differenceSigns == 0),
                                                                       replace = TRUE)
                    diffSamples[which(differenceSigns == 0)] <-
                        nullSamples
                }

                sampledDiffsAbs <- abs(diffSamples)

                thisZ <-
                    decorrelateStepOneSample(diffSamples, oldDeltaProp, sigmaProp = 0.5)
                diffSamples <- diffSamples + thisZ

                gibbsOutput <-
                    sampleGibbsOneSampleWilcoxon(diffScores = diffSamples,
                                                 nIter = nGibbsIterations,
                                                 rscale = cauchyPriorParameter)

                deltaSamples[j] <- oldDeltaProp <- gibbsOutput
                if (progBar)
                    utils::setTxtProgressBar(myBar, j + ((thisChain - 1) * nSamples))

            }

            if (nBurnin > 0) {
                deltaSamples <- deltaSamples[-(1:nBurnin)]
            } else {
                deltaSamples <- deltaSamples
            }
            deltaSamplesMatrix[, thisChain] <- deltaSamples
        }

        betweenChainVar <-
            ((nSamples / (nChains - 1)) * sum((
                apply(deltaSamplesMatrix, 2, mean)  - mean(deltaSamplesMatrix)
            ) ^ 2))
        withinChainVar <-
            (1 / nChains) * sum(apply(deltaSamplesMatrix, 2, stats::var))

        fullVar <-
            (((nSamples - 1) / nSamples) * withinChainVar + (betweenChainVar / nSamples))
        rHat <- sqrt(fullVar / withinChainVar)

        return(list(
            deltaSamples = as.vector(deltaSamplesMatrix),
            rHat = rHat
        ))
    }

sampleGibbsOneSampleWilcoxon <-
    function(diffScores,
             nIter = 10,
             rscale = 1 / sqrt(2)) {
        ybar <- mean(diffScores)
        n <- length(diffScores)
        sigmaSq <- 1
        mu <- ybar
        g <- ybar ^ 2 / sigmaSq + 1

        for (i in 1:nIter) {
            #sample mu
            varMu  <- sigmaSq / (n + (1 / g))
            meanMu <- (n * ybar) / (n + (1 / g))
            mu <- stats::rnorm(1, meanMu, sqrt(varMu))

            # sample g
            scaleg <-
                ((mu ^ 2 + sigmaSq * rscale ^ 2) / (2 * sigmaSq))
            g = 1 / stats::rgamma(1, 1, scaleg)

            delta <- mu / sqrt(sigmaSq)
        }
        return(delta)
    }


decorrelateStepOneSample <- function(d, muProp, sigmaProp = 0.5) {
    thisZ <- stats::rnorm(1, 0, sigmaProp)
    newD <- d + thisZ

    denom <- sum(stats::dnorm(d, (muProp - thisZ), log = TRUE))
    num <- sum(stats::dnorm(newD, muProp, log = TRUE))

    if (stats::runif(1) < exp(num - denom)) {
        return(thisZ)
    } else {
        return(0)
    }

}

# rankSumSampler

rankSumGibbsSampler <-
    function(xVals,
             yVals,
             nSamples = 1e3,
             cauchyPriorParameter = 1 / sqrt(2),
             progBar = TRUE,
             nBurnin = 1,
             nGibbsIterations = 10,
             nChains = 5) {
        if (progBar) {
            myBar <-
                utils::txtProgressBar(
                    min = 1,
                    max = nSamples * nChains,
                    initial = 1,
                    char = "*",
                    style = 3,
                    width = 50
                )
        }

        n1 <- length(xVals)
        n2 <- length(yVals)

        allRanks <- rank(c(xVals, yVals))
        xRanks <- allRanks[1:n1]
        yRanks <- allRanks[(n1 + 1):(n1 + n2)]

        deltaSamples <- numeric(nSamples)
        deltaSamplesMatrix <-
            matrix(ncol = nChains, nrow = nSamples - nBurnin)
        totalIterCount <- 0

        for (thisChain in 1:nChains) {
            currentVals <-
                sort(stats::rnorm((n1 + n2)))[allRanks] # initial values

            oldDeltaProp <- 0

            for (j in 1:nSamples) {
                for (i in sample(1:(n1 + n2))) {
                    currentRank <- allRanks[i]

                    currentBounds <-
                        upperLowerTruncation(
                            ranks = allRanks,
                            values = currentVals,
                            currentRank = currentRank
                        )
                    if (i <= n1) {
                        oldDeltaProp <- -0.5 * oldDeltaProp
                    } else {
                        oldDeltaProp <- 0.5 * oldDeltaProp
                    }

                    currentVals[i] <-
                        truncNormSample(currentBounds[["under"]],
                                        currentBounds[["upper"]],
                                        mu = oldDeltaProp,
                                        sd = 1)

                }

                decorStepResult <-
                    decorrelateStepTwoSample(currentVals[1:n1],
                                             currentVals[(n1 + 1):(n1 +
                                                                       n2)],
                                             oldDeltaProp,
                                             sigmaProp = 0.5)
                xVals <- decorStepResult[[1]]
                yVals <- decorStepResult[[2]]

                gibbsResult <-
                    sampleGibbsTwoSampleWilcoxon(
                        x = xVals,
                        y = yVals,
                        nIter = nGibbsIterations,
                        rscale = cauchyPriorParameter
                    )

                deltaSamples[j] <- oldDeltaProp <- gibbsResult
                if (progBar) {
                    utils::setTxtProgressBar(myBar, j + ((thisChain - 1) * nSamples))
                }

            }

            if (nBurnin > 0) {
                deltaSamples <- -deltaSamples[-(1:nBurnin)]
            } else {
                deltaSamples <- -deltaSamples
            }

            deltaSamplesMatrix[, thisChain] <- deltaSamples

        }

        betweenChainVar <-
            (nSamples / (nChains - 1)) * sum((
                apply(deltaSamplesMatrix, 2, mean)  - mean(deltaSamplesMatrix)
            ) ^ 2)
        withinChainVar <-
            (1 / nChains) * sum(apply(deltaSamplesMatrix, 2, stats::var))

        fullVar <-
            ((nSamples - 1) / nSamples) * withinChainVar + (betweenChainVar / nSamples)
        rHat <- sqrt(fullVar / withinChainVar)

        return(list(
            deltaSamples = as.vector(deltaSamplesMatrix),
            rHat = rHat
        ))
    }

sampleGibbsTwoSampleWilcoxon <-
    function(x,
             y,
             nIter = 10,
             rscale = 1 / sqrt(2)) {
        meanx <- mean(x)
        meany <- mean(y)
        n1 <- length(x)
        n2 <- length(y)
        sigmaSq <- 1 # Arbitrary number for sigma
        g <- 1
        for (i in 1:nIter) {
            #sample mu
            varMu <- ((4 * g * sigmaSq) / (4 + g * (n1 + n2)))
            meanMu <-
                ((2 * g * (n2 * meany - n1 * meanx)) / ((g * (n1 + n2) + 4)))
            mu <- stats::rnorm(1, meanMu, sqrt(varMu))
            # sample g
            betaG <-
                ((mu ^ 2 + sigmaSq * rscale ^ 2) / (2 * sigmaSq))
            g <- 1 / stats::rgamma(1, 1, betaG)
            # convert to delta
            delta <- mu / sqrt(sigmaSq)
        }
        return(delta)
    }


decorrelateStepTwoSample <- function(x, y, muProp, sigmaProp = 1) {
    thisZ <- stats::rnorm(1, 0, sigmaProp)

    newX <- x + thisZ
    newY <- y + thisZ

    denom <-
        sum(stats::dnorm(x, (muProp - thisZ) * -0.5, log = TRUE) + stats::dnorm(y, (muProp -
                                                                                        thisZ) * 0.5, log = TRUE))
    num <-
        sum(
            stats::dnorm(newX, muProp * -0.5, log = TRUE) + stats::dnorm(newY, muProp * 0.5, log = TRUE)
        )

    if (stats::runif(1) < exp(num - denom)) {
        return(list(
            x = newX,
            y = newY,
            accept = TRUE
        ))
    } else {
        return(list(
            x = x,
            y = y,
            accept = FALSE
        ))
    }

}

# spearmanSampler

spearmanGibbsSampler <-
    function(xVals,
             yVals,
             nSamples = 1e3,
             progBar = TRUE,
             kappaPriorParameter = 1,
             nBurnin = 1,
             nChains = 5) {
        if (progBar) {
            myBar <-
                utils::txtProgressBar(
                    min = 1,
                    max = nSamples * nChains,
                    initial = 1,
                    char = "*",
                    style = 3,
                    width = 50
                )
        }

        n <- length(xVals)
        xRanks <- rank(xVals)
        yRanks <- rank(yVals)
        mySigma <- diag(2)

        # Target: posterior samples of rho
        rhoSamples <- numeric(nSamples)
        rhoSamplesMatrix <-
            matrix(ncol = nChains, nrow = nSamples - nBurnin)

        for (thisChain in 1:nChains) {
            # intitialise latent variables
            # intialise rho that is compatible with xVals, yVals
            currentXVals <-
                sort(stats::rnorm((n)))[xRanks] # initial values
            currentYVals <-
                sort(stats::rnorm((n)))[yRanks] # initial values

            currentRho <- stats::cor(currentXVals, currentYVals)
            chanceMechanism <- stats::runif(nSamples)

            for (j in 1:nSamples) {
                # Metropolis sampler:
                # currentXVals and currentYVals first get realigned with the underlying current rho
                #
                for (i in sample(1:n)) {
                    # Gibbs step go through pairs of z^{x}, z^{y} with current rho fixed
                    #   Thus, current is respect to the Gibbs step. Furthermore,
                    #   here align latent variables to the rank
                    #
                    currentXRank <- xRanks[i]
                    currentYRank <- yRanks[i]

                    regressXOnY <-
                        mean(currentYVals[yRanks == currentYRank])
                    regressYOnX <-
                        mean(currentXVals[xRanks == currentXRank])

                    xBounds <-
                        upperLowerTruncation(
                            ranks = xRanks,
                            values = currentXVals,
                            currentRank = currentXRank
                        )
                    currentXVals[i] <-
                        truncNormSample(
                            xBounds[["under"]],
                            xBounds[["upper"]],
                            mu = (currentRho * regressXOnY),
                            sd = sqrt(1 - currentRho ^ 2)
                        )

                    yBounds <-
                        upperLowerTruncation(
                            ranks = yRanks,
                            values = currentYVals,
                            currentRank = currentYRank
                        )
                    currentYVals[i] <-
                        truncNormSample(
                            yBounds[["under"]],
                            yBounds[["upper"]],
                            mu = (currentRho * regressYOnX),
                            sd = sqrt(1 - currentRho ^ 2)
                        )
                }

                currentXVals <-
                    (currentXVals - mean(currentXVals)) / stats::sd(currentXVals)
                currentYVals <-
                    (currentYVals - mean(currentYVals)) / stats::sd(currentYVals)

                # This is the sufficient statistic to evaluate the likelihood part of the MH
                rObs <- stats::cor(currentXVals, currentYVals)

                # Do Metropolis step here
                rhoNew <-
                    metropolisOneStep(
                        rhoCurrent = currentRho,
                        rObs = rObs,
                        n = n,
                        alpha = 1 / kappaPriorParameter,
                        chanceMechanism[j]
                    )

                # Store MH update
                rhoSamples[j] <-
                    rhoNew # add proposal to samples if accepted
                currentRho <-
                    rhoNew # add proposal to samples if accepted

                if (progBar) {
                    utils::setTxtProgressBar(myBar, j + ((thisChain - 1) * nSamples))
                }
            }

            if (nBurnin > 0) {
                rhoSamples <- rhoSamples[-(1:nBurnin)]
            } else {
                rhoSamples <- rhoSamples
            }

            rhoSamplesMatrix[, thisChain] <- rhoSamples

        }

        betweenChainVar <-
            ((nSamples / (nChains - 1)) * sum((
                apply(rhoSamplesMatrix, 2, mean)  - mean(rhoSamplesMatrix)
            ) ^ 2))
        withinChainVar <-
            ((1 / nChains) * sum(apply(rhoSamplesMatrix, 2, stats::var)))

        fullVar <-
            (((nSamples - 1) / nSamples) * withinChainVar + (betweenChainVar / nSamples))
        rHat <- sqrt(fullVar / withinChainVar)

        rhoSamples <-
            pearsonToSpearman(as.vector(rhoSamplesMatrix)) # Transform Pearson's rho to Spearman's rho

        return(list(rhoSamples = rhoSamples, rHat = rHat))
    }

metropolisOneStep <-
    function (rhoCurrent,
              rObs,
              n,
              alpha = 1,
              chanceMechanism) {
        # chanceMechanism is stats::runif(1) vectorised
        #
        zCurrent <- atanh(rhoCurrent)
        zCandidate <-
            stats::rnorm(1, mean = atanh(rhoCurrent), sd = 1 / sqrt(n - 3))
        rhoCandidate <- tanh(zCandidate)

        logAcceptance <-
            ((alpha - n / 2) * (log(1 - rhoCandidate ^ 2) - log(1 - rhoCurrent ^ 2)) +
                 n * ((1 - rhoCurrent * rObs) / (1 - rhoCurrent ^ 2) - (1 - rhoCandidate *
                                                                            rObs) / (1 - rhoCandidate ^ 2)
                 ))

        if (chanceMechanism <= exp(logAcceptance)) {
            return(rhoCandidate)
        } else {
            return(rhoCurrent)
        }
    }

pearsonToSpearman <- function(rho) {
    mySpear <- (6 / pi) * asin(rho / 2)

    return(mySpear)
}

# rankBasedCommonFunctions

truncNormSample <-
    function(lBound = -Inf,
             uBound = Inf,
             mu = 0,
             sd = 1) {
        lBoundUni <- stats::pnorm(lBound, mean = mu, sd = sd)
        uBoundUni <- stats::pnorm(uBound, mean = mu, sd = sd)
        mySample <-
            stats::qnorm(stats::runif(1, lBoundUni, uBoundUni),
                         mean = mu,
                         sd = sd)
        return(mySample)
    }

upperLowerTruncation <- function(ranks, values, currentRank) {
    if (currentRank == min(ranks)) {
        under <- -Inf
    } else {
        under <- max(values[ranks < currentRank])
    }

    if (currentRank == max(ranks)) {
        upper <- Inf
    } else {
        upper <- min(values[ranks > currentRank])
    }

    return(list(under = under, upper = upper))
}


computeBayesFactorOneZero <-
    function(posteriorSamples,
             priorParameter = 1,
             oneSided = FALSE,
             whichTest = "Wilcoxon") {
        postDens <- logspline::logspline(posteriorSamples)
        densZeroPoint <- logspline::dlogspline(0, postDens)

        corFactorPosterior <- logspline::plogspline(0, postDens)
        if (oneSided == "right")
            corFactorPosterior <- 1 - corFactorPosterior

        if (whichTest == "Wilcoxon") {
            # priorParameter should be the Cauchy scale parameter
            priorDensZeroPoint <-
                stats::dcauchy(0, scale = priorParameter)
            corFactorPrior <-
                stats::pcauchy(0,
                               scale = priorParameter,
                               lower.tail = (oneSided != "right"))
        } else if (whichTest == "Spearman") {
            # priorParameter should be kappa
            priorDensZeroPoint <-
                (stats::dbeta(0.5, 1 / priorParameter, 1 / priorParameter) / 2)
            corFactorPrior <-
                stats::pbeta(0.5,
                             1 / priorParameter,
                             1 / priorParameter,
                             lower.tail = (oneSided != "right"))
        }

        if (isFALSE(oneSided)) {
            bf10 <- priorDensZeroPoint / densZeroPoint
        } else {
            bf10 <-
                ((priorDensZeroPoint / corFactorPrior) / (densZeroPoint / corFactorPosterior))
        }

        return(bf10)
    }
