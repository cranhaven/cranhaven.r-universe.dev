#' Chemotherapy data
#' 
#' A list of two components.
#' 
#' Matrix Y : Gene expression dataset from the patients with diffuse large-B-cell lymphoma (DLBCL) after chemotherapy.
#' The data has been used for unsupervised analysis i.e. Biclustering. The data consists of expression levels of q = 661 genes from n =180 patients. Among the patients, 42, 51 and
#' 87 of them were classified to OxPhos, BCR and HR groups, respectively. The data thus
#' form an n by q matrix Y whose rows represent the subjects and columns correspond to the
#' genes, and used in Rosenwald (2002).
#' 
#' classIndex: Out of OxPhos (oxidative phosphorylation), BCR(Bcell response) and HR (host response),
#' the index corresponds to the groups in which these 180 subjects belongs as classified by Hoshida (2007).
#'
#' @docType data
#' @usage data(DLBCL)
#' @format A list with two components: 
#' \describe{
#'   \item{Y}{Chromatin immunoprecipitation. A matrix of 180 rows and 661 columns.}
#'   \item{classIndex}{Group index of 180 patients where \emph{1,2,3} corresponds to OxPhos, BCR and HR groups respectively.}
#' }
#' @keywords datasets
#' @examples
#' # data(DLBCL)
#' @references Rosenwald, A., Wright, G., Chan, W. C., Connors, J. M., Campo, E., Fisher, R. I., 
#' Gascoyne, R. D., Muller-Hermelink, H. K., Smeland, E. B., Giltnane, J. M. et al. (2002) \emph{The
#' use of molecular profling to predict survival after chemotherapy for diffuse large-b-cell
#' lymphoma. New England Journal of Medicine, 346, 1937-1947}.
#' 
#' Hoshida, Y., Brunet, J.-P., Tamayo, P., Golub, T. R. and Mesirov, J. P. (2007) \emph{Subclass
#' mapping: Identifying common subtypes in independent disease data sets. PLoS ONE, 2,
#' e1195}.
"DLBCL"
