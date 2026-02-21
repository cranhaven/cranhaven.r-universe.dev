# Functions to check for errors in the input arguments

check_L_W_H <- function(L, W, H) {

  check_mat_df(L, "L")
  check_mat_df(W, "W")
  check_mat_df(H, "H")
  if (nrow(H) != ncol(W)) stop("ERROR: nrow(H) != ncol(W)")
  if (ncol(L) != ncol(H)) stop("ERROR: ncol(H) != ncol(L)")
  if (nrow(L) != nrow(W)) stop("ERROR: nrow(L) != nrow(W)")

  NULL
}

check_L_W_V <- function(L, W, V) {

  check_mat_df(L, "L")
  check_mat_df(W, "W")
  check_mat_df(V, "V")
  if (any(dim(L) != dim(V))) stop("ERROR: dim(L) != dim(V)")
  if (nrow(L) != nrow(W)) stop("ERROR: nrow(L) != nrow(W)")

  NULL
}


check_mat_df <- function(x, nm, check.finite=1) {

  if (!is.matrix(x) && !is.data.frame(x)) {
    stop(paste0("ERROR: ", nm, " must be a matrix or data frame"))
  }
  if (!nrow(x)) stop(paste0("ERROR: ", nm, " has no rows"))
  if (!ncol(x)) stop(paste0("ERROR: ", nm, " has no columns"))
  if (check.finite) {
    if (any(!is.finite(as.matrix(x)))) {
      stop(paste0("ERROR: ", nm, " contains non-finite values"))
    }
  }

  NULL
}

check_number <- function(x, nm, min=NULL, pos=FALSE, max=NULL) {

  if (length(x) != 1) stop(paste0("ERROR: ", nm, " must be a single numeric value"))
  if (!is.null(min) && (x < min)) {
    stop(paste0("ERROR: ", nm, " must be greater than or equal to ", min))
  }
  if (pos && (x <= 0)) {
    stop(paste0("ERROR: ", nm, " must be positive"))
  }
  if (!is.null(max) && (x > max)) {
    stop(paste0("ERROR: ", nm, " must be less than or equal to ", max))
  }

  NULL

}

isString <- function(x) {
  is.character(x) && (length(x) == 1)
}

check_Types <- function(x) {

  valid <- c("COSMIC", "signeR")
  err   <- "ERROR: Types must be 'COSMIC' or 'signeR'"

  if(length(x) == 2){
    stop("Please specifiy mutation types: available mutations types are either \"COSMIC\" or \"signeR\"")
  }

  if (!isString(x)) stop(err)
  if (!(x %in% valid)) stop(err)  
    
  NULL
}

check_Class <- function(x) {

  valid <- c("SBS", "DBS")
  err   <- "ERROR: Class must be 'SBS' or 'DBS'"

  if(length(x) != 1) stop(err)
  if (!isString(x)) stop(err)
  if (!(x %in% valid)) stop(err)  
    
  NULL
}


check_COSMICv <- function(x) {

  valid <- c("v3.2", "v3.4")
  err   <- "ERROR: COSMICv must be 'v3.2' or 'v3.4'"

  if (!isString(x)) stop(err)
  if (!(x %in% valid)) stop(err)  
    
  NULL
}

check_ref.genome <- function(x) {

  valid <- c("hg19", "hg38")
  err   <- "ERROR: ref.genome must be 'hg19' or 'hg38'"

  if (!isString(x)) stop(err)
  if (!(x %in% valid)) stop(err)  
    
  NULL
}

check_genomic_info <- function(x) {

  if (!is.data.frame(x)) {
    stop("ERROR: genomic_information must be a data frame")
  }
  cols <- c("Chromosome", "Start_Position", "End_Position", "SEQ_ASSAY_ID")
  tmp  <- !(cols %in% colnames(x))
  if (any(tmp)) {
    miss  <- cols[tmp]
    nmiss <- length(miss)
    mstr  <- paste0("'", miss, "'")
    mstr  <- paste0(mstr, collapse=", ")
    if (nmiss == 1) {
      msg <- paste0("ERROR: column ", mstr, " not found in genomic_information")
    } else {
      msg <- paste0("ERROR: columns ", mstr, " not found in genomic_information")
    }
    stop(msg)
  }

  NULL
}

check_Panel_context <- function(x) {

  if (!is.data.frame(x)) stop("ERROR: Panel_context must be a data frame")
  check_mat_df(x, "Panel_context", check.finite=0)

  NULL
}

check_Patient_Info <- function(x) {

  if (!is.data.frame(x)) stop("ERROR: Patient_Info must be a data frame")
  check_mat_df(x, "Patient_Info", check.finite=0)
  cx <- colnames(x)
  vv <- c("PATIENT_ID", "SEQ_ASSAY_ID")
  for (v in vv) {
    if (!(v %in% cx)) {
      stop(paste0("ERROR: Patient_Info must contain the column '", v, "'"))
    }
  }
  NULL	
}
