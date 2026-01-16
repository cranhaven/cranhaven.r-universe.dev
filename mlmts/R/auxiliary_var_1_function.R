

auxiliary_var_1_function <- function(X, max_p = 1, criterion = 'AIC',...) {

  l <- length(X)
  coefficients <- list()


  # Fitting a VAR model for each MTS according to a given criterion


  for (i in (1 : l))  {

    auxiliary <- numeric()
    auxiliary_coeffs <- list()


    for (j in 1 : max_p) {

      var <- MTS::VARMA(X[[i]], p = j, details = F)

      if (criterion == 'AIC') {

      auxiliary[j] <- var$aic

      } else {

      auxiliary[j] <- var$bic

      }

      auxiliary_coeffs[[j]] <- as.vector(var$coef)

    }


    min_pos <- which(auxiliary == min(auxiliary), arr.ind = T)
    coefficients[[i]] <- auxiliary_coeffs[[min_pos]]


  }


  # computing the maximum length of vector coefficients according to the considered criterion


  lengths = numeric()

  for (i in (1 : l)) {

    lengths[i] <- length(coefficients[[i]])

  }

  max_length <- max(lengths)



  for (i in (1 : l)) {

    if (lengths[i] != max_length) {

      li <- max_length - lengths[i]
      add <- numeric(li)
      coefficients[[i]] <- c(coefficients[[i]], add)

    }

  }



  listTomatrix(coefficients)




}
