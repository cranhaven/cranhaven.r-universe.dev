#' The Inhomogenous Polytomous Dataset
#'
#' The artificial dataset of a polytomous responses (five categories) which
#' contains three subsets with different discrimination values.
#' To reproduce this dataset: \cr
#' \code{poly_inh_dset <- generate_data(responseType = "discriminate", ncat = 5, alpha = c(0.04,0.045,0.05,0.055,0.06,0.065,0.2,0.25,0.3,0.35,0.4,0.45,2.6,2.65,2.7,2.75,2.8,2.85,2.9))}
#' \cr\cr will lead to similar but not the same dataset, due to the randomization.
#'
#' @docType data
#'
#' @usage data(poly_inh_dset)
#'
#' @rdname poly_inh_dset
"poly_inh_dset"


#' A Shorter Inhomogenous Polytomous Dataset
#'
#' The artificial dataset of a polytomous responses (three categories) which
#' contains three subsets with different discrimination values.
#' To reproduce this dataset: \cr
#' \code{short_poly_data <- generate_data(alpha = c(0.02,0.5,2), nitem = 3, ndim = 3,ncat = 5, theta = c(-6,6), beta = c(-4,4), ntheta = 151)}
#' \cr\cr will lead to similar but not the same dataset, due to the randomization.
#'
#' @docType data
#'
#' @usage data(short_poly_data)
#'
#' @rdname short_poly_data
"short_poly_data"

#' A Shorter Polytomous Dataset with DIF
#'
#' The artificial dataset of a polytomous responses (three categories) which
#' contains three non-DIF items and a DIF item.
#'
#' @docType data
#'
#' @usage data(shortDIF)
#'
#' @rdname shortDIF
"shortDIF"


#' The Inhomogenous Polytomous Dataset containing DIF items
#'
#' The artificial data set of a polytomous responses (five categories) which
#' contains three subsets with different discrimination values and two DIF-items.
#'
#' @docType data
#'
#' @usage data(polydif_inh_dset)
#'
#' @rdname polydif_inh_dset
"polydif_inh_dset"


#' Multidimensional polytomous data set with 0.2 correlation
#'
#' @docType data
#'
#' @usage data(correl02_multidim)
#'
#' @rdname correl02_multidim
"correl02_multidim"


#' Multidimensional polytomous data set with 0.3 correlation
#'
#' @docType data
#'
#' @usage data(correl03_multidim)
#'
#' @rdname correl03_multidim
"correl03_multidim"


#' Multidimensional polytomous data set with 0.4 Correlation
#'
#' @docType data
#'
#' @usage data(correl04_multidim)
#'
#' @rdname correl04_multidim
"correl04_multidim"


#' Multidimensional polytomous data set with 0.5 Correlation
#'
#' @docType data
#'
#' @usage data(correl05_multidim)
#'
#' @rdname correl05_multidim
"correl05_multidim"


#' Multidimensional polytomous data set with 0.6 Correlation
#'
#' @docType data
#'
#' @usage data(correl06_multidim)
#'
#' @rdname correl06_multidim
"correl06_multidim"


#' Inhomogenous Dichotomous Data Set
#'
#' Data set with binary type responses containing three subsets
#' with different discrimination values.
#'
#' @docType data
#'
#' @usage data(dicho_inh_dset)
#'
#' @rdname dicho_inh_dset
"dicho_inh_dset"


#' Uncorrelated Multidimensional Dichotomous Data Set
#'
#' Data set with binary type responses containing three subsets which
#' represent different uncorrelated dimensions.
#'
#' @docType data
#'
#' @usage data(dicho_md_dset)
#'
#' @rdname dicho_md_dset
"dicho_md_dset"


#' Uncorrelated Multidimensional Polytomous Data Set
#'
#' Data set with polytomous responses (five categories) containing
#' three subsets which represent different uncorrelated dimensions.
#'
#' @docType data
#'
#' @usage data(poly_md_dset)
#'
#' @rdname poly_md_dset
"poly_md_dset"


#' Multi-testlets Polytomous Data Set
#'
#' Generate data set which consist of two bundle items with different level of
#' local dependency effect.
#'
#' @docType data
#'
#' @usage data(testlets_dataset)
#'
#' @rdname testlets_dataset
"testlets_dataset"


#' Within-item Multidimensional Polytomous Data Set
#'
#' Generate multidimensional dataset with some items relate to more than one
#' dimension.
#'
#' @docType data
#'
#' @usage data(withinItem_multidim)
#'
#' @rdname withinItem_multidim
"withinItem_multidim"

