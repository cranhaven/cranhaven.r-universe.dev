# gets strata factor from formula and its respective model
.get_strata <- function(formula, model) {
    if (inherits(model, "Surv")) {
        # in case the model is JUST a surv object (hey! it happens)
        return()
    }

    if (inherits(formula, "coxph")) {
        # discards any columns not starting with strata() assumedly it's only one but...
        columns <- which(grepl("^strata\\(", colnames(model)))
        if (length(columns) > 1) {
            return(survival::strata(model[, columns]))
        } else if (length(columns) == 1) {
            return(model[, columns])
        }
    } else {
        if (ncol(model) >= 2) {
            # the whole right side of the formula IS the strata discard the left side
            # (which is always the Surv object)
            if (ncol(model) == 2 && !is.factor(model[, 2])) {
                return(as.factor(model[, 2]))
            }
            return(survival::strata(model[, -1, drop = FALSE]))
        }
    }
}

# flattens list of graphs (as ggplots are also lists, can't just use unlist)
.unlist_plots <- function(plots) {
    result <- list()

    for (type in names(plots)) {
        item <- plots[[type]]
        name <- type

        sublist <- NULL
        if (is.list(item) && !inherits(item, c("ggplot", "ggsurvplot"))) {
            if (inherits(item, "vsdstrata")) {
                sublist <- list(all = list(item$all), strata = item$strata)
                sublist <- unlist(sublist, recursive = FALSE)
            } else {
                sublist <- item
            }
        }

        if (is.list(sublist)) {
            for (subname in names(sublist)) {
                result[[paste(name, subname, sep = "$")]] <- list(plot = sublist[[subname]], type = type)
            }
        } else {
            result[[name]] <- list(plot = item, type = type)
        }

    }

    return(result)
}
