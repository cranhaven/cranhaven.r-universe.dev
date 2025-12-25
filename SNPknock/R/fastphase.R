#' Fit an HMM to genetic data using fastPHASE
#'
#' This function provides a wrapper for the fastPHASE executable in order to fit an HMM to either
#' unphased genotype data or phased haplotype data.
#' The software fastPHASE will fit the HMM  to the genotype data and write the
#' corresponding parameter estimates in four separate files.
#' Since fastPHASE is not an R package, this executable must be downloaded separately by the
#' user. Visit \url{http://scheet.org/software.html} for more information on how to obtain fastPHASE.
#'
#' @param fp_path a string with the path to the directory with the fastPHASE executable.
#' @param X_file a string with the path of the genotype input file containing X in fastPHASE
#'               format (as created by \link{writeXtoInp}).
#' @param out_path a string with the path of the directory in which the parameter estimates
#'                 will be saved (default: NULL). If this is equal to NULL, a temporary file
#'                 in the R temporary directory will be used.
#' @param K the number of hidden states for each haplotype sequence (default: 12).
#' @param numit the number of EM iterations (default: 25).
#' @param phased whether the data are already phased (default: FALSE).
#' @param seed the random seed for the EM algorithm (default: 1).
#' @return A string containing the path of the directory in which the parameter estimates
#'         were saved. This is useful to find the data when the default option for `out_path`
#'         is used and the output is written in an R temporary directory.
#'
#' @family fastPHASE
#'
#' @details
#' The software fastPHASE saves the parameter estimates in four separate files whose names
#' begin with the string contained in 'out_path' and end with:
#' \itemize{
#'   \item{"_rhat.txt"}
#'   \item{"_alphahat.txt"}
#'   \item{"_thetahat.txt"}
#'   \item{"_origchars"}
#' }
#'
#' The HMM for the genotype data can then be loaded from these files by calling
#' \link{loadHMM}.
#'
#' @references
#'   \insertRef{scheet2006}{SNPknock}
#'   
#' @examples
#' fp_path  = "~/bin/fastPHASE" # Path to the fastPHASE executable
#'
#' # Run fastPHASE on unphased genotypes
#' # Specify the path to the genotype input file in ".inp" format.
#' # An example file containing unphased genotypes can be found in the package installation folder.
#' X_file = system.file("extdata", "genotypes.inp", package = "SNPknock")
#' fp_outPath = runFastPhase(fp_path, X_file)
#'
#' # Run fastPHASE on phased haplotypes
#' # An example file containing phased haplotypes can be found in the package installation folder.
#' H_file = system.file("extdata", "haplotypes.inp", package = "SNPknock")
#' fp_outPath = runFastPhase(fp_path, H_file, phased=TRUE)
#'
#' @export
runFastPhase <- function(fp_path, X_file, out_path=NULL, K=12, numit=25, phased=FALSE, seed=1) {
  # Check that input has the right format
  K = as.integer(K)
  numit = as.integer(numit)
  seed = as.integer(seed)
  stopifnot(is.character(fp_path))
  stopifnot(is.character(X_file))
  stopifnot(is.null(out_path) | is.character(out_path))
  stopifnot(is.integer(K))
  stopifnot(is.integer(numit))
  stopifnot(is.logical(phased))
  stopifnot(is.integer(seed))

  # Verify that the fastPHASE executable can be found
  if(!file.exists(fp_path)) {
    message(paste("SNPknock could find the fastPHASE executable: '",fp_path,"' does not exist.
If you have not downloaded it yet, you can obtain fastPHASE from: http://scheet.org/software.html", sep=""))
    return(NULL)
  }

  # Write to temporary directory unless specified otherwise
  if(is.null(out_path)) {
    out_path = tempfile(pattern="file", tmpdir=tempdir(), fileext = "")
  }

  # Make out_path absolute
  out_path_dirname = tools::file_path_as_absolute(dirname(out_path))
  out_path_basename = basename(out_path)
  out_path_abs = paste(out_path_dirname, out_path_basename, sep="/")

  # Prepare arguments for fastPHASE
  command = fp_path
  command = paste(command, " -Pp -T1 -K", K, sep="")
  command = paste(command, " -g -H-4 -C", numit, sep="")
  if(phased){
    command = paste(command, " -B", sep="")
  }
  command = paste(command, " -S", seed, sep="")
  command = paste(command, " -o'", out_path_abs, "' ", X_file, sep="")

  # Run the fastPHASE executable
  cat(command)
  tryCatch(system(command), error=function(e) 1)

  return(out_path)
}

#' Convert genotypes X into the fastPHASE input format
#'
#' This function converts a genetic matrix X into the fastPHASE input format and saves
#' it to a user-specified file. Then, an HMM can be fitted by calling fastPHASE with
#' \link{runFastPhase}.
#'
#' @param X either a matrix of size n-by-p containing unphased genotypes for n individuals,
#' or a matrix of size 2n-by-p containing phased haplotypes for n individuals.
#' @param phased whether the data are phased (default: FALSE).
#' If this is equal to TRUE, each pair of consecutive rows will be assumed to correspond to phased haplotypes from the
#' same individual.
#' @param out_file a string containing the path of the output file onto which X will be written (default: NULL).
#' If this is equal to NULL, a temporary file in the R temporary directory will be used.
#' @return A string containing the path of the output file onto which X was written. This is useful to find the data
#' when the default option for `out_file` is used and X is written onto a temporary file in the R temporary directory.
#'
#' @family fastPHASE
#'
#' @references
#'   \insertRef{scheet2006}{SNPknock}
#'
#' @examples
#' # Convert unphased genotypes
#' # Load an example data matrix X from the package installation directory.
#' X_file = system.file("extdata", "genotypes.RData", package = "SNPknock")
#' load(X_file)
#' # Write X in a temporary file
#' Xinp_file = writeXtoInp(X)
#'
#' # Convert phased haplotypes
#' # Load an example data matrix H from the package installation directory.
#' H_file = system.file("extdata", "haplotypes.RData", package = "SNPknock")
#' load(H_file)
#' # Write H in a temporary file
#' Hinp_file = writeXtoInp(H, phased=TRUE)
#'
#' @export
writeXtoInp <- function(X, phased=FALSE, out_file=NULL) {
  # Extract dimensions of input
  n = dim(X)[1]
  p = dim(X)[2]

  # Check that input has the right format
  stopifnot(is.integer(X))
  stopifnot(is.logical(phased))
  stopifnot(is.null(out_file) | is.character(out_file))
  if(phased){
    stopifnot(n%%2==0)
    stopifnot(min(X)>=0)
    stopifnot(max(X)<=1)
  } else {
    stopifnot(min(X)>=0)
    stopifnot(max(X)<=2)
  }

  # Write to temporary file unless specified otherwise
  if(is.null(out_file)) {
    out_file = tempfile(pattern="file", tmpdir=tempdir(), fileext = ".inp")
  }

  # Write header to file
  con = file(out_file, "w")
  if(phased) {
    writeLines(toString(n/2), con = con, sep = "\n", useBytes = FALSE)
  }
  else {
    writeLines(toString(n), con = con, sep = "\n", useBytes = FALSE)
  }
  writeLines(toString(p), con = con, sep = "\n", useBytes = FALSE)

  # Write data to file
  if(phased){
    # Write phased haplotypes to file
    for(m in seq(1,n,by=2)) {
      text = paste(c("#id",toString((m-1)/2)), collapse = '')
      writeLines(text, con = con, sep = "\n", useBytes = FALSE)
      text = paste(X[m,], collapse = '')
      writeLines(text, con = con, sep = "\n", useBytes = FALSE)
      text = paste(X[m+1,], collapse = '')
      writeLines(text, con = con, sep = "\n", useBytes = FALSE)
    }
  } else{
    # Phase X for fastPHASE (randomly)
    X = t(X)
    v1 = array(c(0,1,1))
    v2 = array(c(0,0,1))
    Xp1 = array(v1[X+1], dim(X))
    Xp2 = array(v2[X+1], dim(X))
    # Write randomly phased genotypes to file
    for(m in 1:n) {
      text = paste(c("#id",toString(m-1)), collapse = '')
      writeLines(text, con = con, sep = "\n", useBytes = FALSE)
      text = paste(Xp1[,m], collapse = '')
      writeLines(text, con = con, sep = "\n", useBytes = FALSE)
      text = paste(Xp2[,m], collapse = '')
      writeLines(text, con = con, sep = "\n", useBytes = FALSE)
    }
  }

  close(con)

  return(out_file)
}

#' Load HMM parameters fitted by fastPHASE
#'
#' This function loads the parameter estimates obtained by fastPHASE (see \link{runFastPhase})
#' and assembles the Li and Stephens HMM, in the format required by the knockoff generation functions
#' \link{knockoffHaplotypes} and \link{knockoffGenotypes}.
#'
#' @param r_file a string with the path of the "_rhat.txt" file produced by fastPHASE.
#' @param alpha_file a string with the path of the "_alphahat.txt" file produced by fastPHASE.
#' @param theta_file a string with the path of the "_thetahat.txt" file produced by fastPHASE.
#' @param char_file a string with the path of the "_origchars" file produced by fastPHASE.
#' @param compact whether to assemble the explicit transition and emission matrices for the HMM (default: FALSE).
#' @param phased whether to assemble a model for phased haplotypes, if compact==FALSE (default: FALSE).
#'
#' @return A structure containing the parameters from the Li and Stephens HMM for phased haplotypes.
#'
#' @family fastPHASE
#'
#' @details
#' This function by default returns a structure with three fields:
#' \itemize{
#'   \item{"r": a numerical array of length p.}
#'   \item{"alpha": a numerical array of size (p,K).}
#'   \item{"theta": a numerical array of size (p,K).}
#'  }
#'
#' If the parameter compact is FALSE, this function assembles the HMM model for the genotype data
#' (either unphased or phased), in the format required by the knockoff generation function \link{knockoffHMM}.
#' 
#' @references
#'   \insertRef{scheet2006}{SNPknock}
#'
#' @examples
#' # Specify the location of the fastPHASE output files containing the parameter estimates.
#' # Example files can be found in the package installation directory.
#' r_file = system.file("extdata", "genotypes_rhat.txt", package = "SNPknock")
#' alpha_file = system.file("extdata", "genotypes_alphahat.txt", package = "SNPknock")
#' theta_file = system.file("extdata", "genotypes_thetahat.txt", package = "SNPknock")
#' char_file = system.file("extdata", "genotypes_origchars", package = "SNPknock")
#'
#' # Read the parameter files and load the HMM
#' hmm = loadHMM(r_file, alpha_file, theta_file, char_file)
#'
#' # Read the parameter files and load the HMM
#' hmm.large = loadHMM(r_file, alpha_file, theta_file, char_file, compact=FALSE)
#'
#' @export
loadHMM <- function(r_file, alpha_file, theta_file, char_file, compact=TRUE, phased=FALSE) {
  # Check that input has the right format
  stopifnot(is.character(r_file))
  stopifnot(is.character(alpha_file))
  stopifnot(is.character(theta_file))
  stopifnot(is.character(char_file))

  # Load (r,theta,alpha) paramters from fastPHASE fit
  r = loadEMParameters(r_file)
  alpha = loadEMParameters(alpha_file)
  theta = loadEMParameters(theta_file)
  char = loadEMParameters(char_file)

  # Flip theta
  X_chr_flip = char[,2] %in% c("1?", "10")
  theta[X_chr_flip,] = 1-theta[X_chr_flip,]

  # Return the HMM
  hmm = NULL
  if(compact) {
      hmm$r = r
      hmm$alpha = alpha
      hmm$theta = theta
  } else {
    # Assemble transition matrices
    if(phased) {
        hmm$Q = compute_Q1(r, alpha)
        hmm$pEmit = assemble_pEmit_phased(theta)
        hmm$pInit = assemble_pInit_phased(alpha)
    } else {
        Q1 = compute_Q1(r, alpha)
        hmm$Q = assemble_Q(Q1)
        hmm$pEmit = assemble_pEmit(theta)
        hmm$pInit = assemble_pInit(alpha)
    }
  }
  return(hmm)
}

#' Read the files produced by fastPHASE
#'
#' @rdname loadEMParameters
#' @keywords internal
loadEMParameters <- function(data_path) {
  # Load the entire file
  lines = readLines(data_path)
  # Remove first comment row
  lines = lines[2:length(lines)]
  # Keep only first EM start
  last_row = suppressWarnings(min(grep("^\\s*>", lines)))
  if (is.finite(last_row)) {
    lines = lines[1:(last_row-1)]
  }
  # Read the relevant rows as a csv dile
  con = textConnection(lines)
  return(as.matrix(utils::read.table(con)))
}

#' Compute the haplotype transition matrices based on the fastPHASE HMM
#'
#' @rdname compute_Q1
#' @keywords internal
compute_Q1 <- function(r, alpha) {
  p = dim(alpha)[1]
  K = dim(alpha)[2]
  Q = array(rep(0, (p-1)*K*K), c(p-1,K,K))
  rExp = exp(-r)
  for(j in 2:p) {
    v = (1-rExp[j])*alpha[j,]
    Q[j-1,,] = t(matrix(rep(v,K), ncol = K)) + diag(rep(rExp[j],K))
  }
  return(Q)
}

#' Compute the genotype transition matrices based on the fastPHASE HMM
#'
#' @rdname assemble_Q
#' @keywords internal
assemble_Q <- function(Q1) {
  p = dim(Q1)[1]+1
  K = dim(Q1)[2]
  Keff = K*(K+1)/2
  Q = array(rep(0, (p-1)*Keff*Keff), c(p-1,Keff,Keff))
  for (k1 in 1:K) {
    for (k2 in 1:k1) {
      i = ((k1-1)*k1)/2+k2
      for (k1p in 1:K) {
        for (k2p in 1:k1p) {
          j = ((k1p-1)*k1p)/2+k2p
          Q[,i,j] = Q1[,k1,k1p] * Q1[,k2,k2p]
          if (k1p != k2p) {# Normalization seems correct, but different from the paper
            Q[,i,j] = Q[,i,j] + Q1[,k1,k2p] * Q1[,k2,k1p]
          }
        }
      }
    }
  }
  for(m in 1:(p-1)) {
    # Enforce normalization
    QSums = rowSums(Q[m,,])
    #stopifnot(abs(QSums-1)<1e-6)
    Q[m,,] = sweep(Q[m,,],1,QSums,`/`)
  }
  return(Q)
}

#' Compute the genotype emission distributions based on the fastPHASE HMM
#'
#' @rdname assemble_pEmit
#' @keywords internal
assemble_pEmit<- function(theta) {
  p = dim(theta)[1]
  K = dim(theta)[2]
  Keff = K*(K+1)/2
  pEmit = array(rep(0,p*3*Keff), c(p,3,Keff))
  for(m in 1:p) {
    pEmit1 = array(rep(0,3*Keff), c(3,Keff))
    for (k1 in 1:K) {
      for (k2 in 1:k1) {
        i = ((k1-1)*k1)/2+k2
        pEmit1[1,i] = (1-theta[m,k1])*(1-theta[m,k2])
        pEmit1[2,i] = theta[m,k1]*(1-theta[m,k2]) + theta[m,k2]*(1-theta[m,k1])
        pEmit1[3,i] = theta[m,k1]*theta[m,k2]
      }
    }
    pEmit[m,,] = pEmit1

    # Enforce normalization
    pEmit1Sums = colSums(pEmit1)
    #stopifnot(abs(pEmit1Sums-1)<1e-6)
    pEmit[m,,] = sweep(pEmit[m,,],2,pEmit1Sums,`/`)
  }
  return(pEmit)
}

#' Compute the haplotype emission distributions based on the fastPHASE HMM
#'
#' @rdname assemble_pEmit_phased
#' @keywords internal
assemble_pEmit_phased<- function(theta) {
  p = dim(theta)[1]
  K = dim(theta)[2]
  pEmit = array(rep(0,p*2*K), c(p,2,K))
  for(m in 1:p) {
    pEmit[m,1,] = 1-theta[m,]
    pEmit[m,2,] = theta[m,]
  }
  return(pEmit)
}

#' Compute the genotype initial distributions based on the fastPHASE HMM
#'
#' @rdname assemble_pInit
#' @keywords internal
assemble_pInit <- function(alpha) {
  p = dim(alpha)[1]
  K = dim(alpha)[2]
  Keff = K*(K+1)/2
  pInit = rep(0,Keff)
  for (k1 in 1:K) {
    for (k2 in 1:k1) {
      i = ((k1-1)*k1)/2+k2
      if (k1==k2)
        pInit[i] = alpha[1,k1]*alpha[1,k1]
      else
        pInit[i] = 2*alpha[1,k1]*alpha[1,k2]
    }
  }
  # Enforce normalization
  pInitSum = sum(pInit)
  #stopifnot(abs(pInitSum-1)<1e-4)
  pInit = pInit/pInitSum

  return(pInit)
}

#' Compute the haplotype initial distributions based on the fastPHASE HMM
#'
#' @rdname assemble_pInit_phased
#' @keywords internal
assemble_pInit_phased <- function(alpha) {
  p = dim(alpha)[1]
  K = dim(alpha)[2]
  pInit = as.numeric(alpha[1,])

  # Enforce normalization
  pInitSum = sum(pInit)
  #stopifnot(abs(pInitSum-1)<1e-4)
  pInit = pInit/pInitSum

  return(pInit)
}
