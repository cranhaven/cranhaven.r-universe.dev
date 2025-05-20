#' Run spip in a user-specified directory
#'
#' This runs it in a directory and the output from stdout
#' goes into a big file spip_out.txt in that directory. Currently
#' this is pretty bare bones.
#' @param pars A named list of parameter values.
#' @param dir The directory to run it in.  Defaults to a temp directory,
#' which will be unique every time it is run.
#' @param spip_seeds a vector of two positive integers. These get written to the
#' file spip_seeds, which is used by spip to seed its random number generator.  By
#' default, R supplies these two integers from its own random number generator.  This
#' way reproducible results from spip can be obtained by calling `set.seed()` from within
#' R before calling `run_spip()`.  For the most part, the user should never really have
#' to directly supply a value for spip_seeds.
#' @param num_pops the number of demes that are being simulated.  This is still being
#' implemented...
#' @param allele_freqs a list of allele frequencies provided if you want
#' to simulate unlinked genotypes for the sampled individuals. The default
#' is simply a single locus with two alleles at frequencies 0.5, 0.5, which
#' is provided because spip has to be given some allele frequencies if sampling
#' is to be carried out.  Note that a user-specified value to this option should
#' only be given if you want to actually simulate some genetic data from the
#' sampled individuals. The length of the list should be the number of loci
#' desired, and the length of each element should be the number of alleles.
#' For examples for three loci with 2, 3, and 4 equifrequent alleles, respectively,
#' you would provide `list(c(0.5, 0.5), c(0.3333, 0.3333, 0.3333), c(0.25, 0.25, 0.25, 0.25))`.
#' Note that allele frequencies will be normalized to sum to one within each locus.
#' @details This creates a temporary directory and runs spip in that directory, redirecting
#' stdout and stderr to files.  It then processes the output using awk to create a collection
#' of files.  If spip throws an error, the contents of stderr are written to the screen to notify
#' the user of how to correct their input.
#'
#' For a full example of its use see the Vignette:
#' `vignette("species_1_simulation", package = "CKMRpop")`.
#' @return Returns the path to the temporary directory were `spip` was run and where the
#' processed output files can be found to be read in using `slurp_spip()`.
#' @export

run_spip <- function(
  pars,
  dir = tempfile(),
  spip_seeds = ceiling(runif(2, 1, 1e9)),
  num_pops = 1,
  allele_freqs = list(c(0.5, 0.5))
) {

  # before doing anything, check to see if the spip binary is there, and dump
  # an error if it is not:
  boing <- spip_binary()


  cwd = getwd()  # get the current directory to change back to it after the system2 call
  on.exit(setwd(cwd))

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)



  # write parameters to demog.comm
  dfile <- file.path(dir, "demog.comm")
  sfile <- file.path(dir, "spip_out.txt")
  efile <- file.path(dir, "spip_err.txt")
  lfile <- file.path(dir, "alle_freq.txt")
  seedfile <- file.path(dir, "spip_seeds")

  # write the spip_seeds file in dir
  cat(spip_seeds, sep = " ", eol = "\n", file = seedfile)

  # now, make the allele frequency file
  cat(length(allele_freqs), file = lfile, sep = "\n")
  dump <- lapply(allele_freqs, function(x) {
    cat(length(x), "  ", file = lfile, append = TRUE)
    cat(x/sum(x), "\n", sep = " ", file = lfile, append = TRUE)
  })

  # now, make the demography and sampling file of commands.
  # If num_pops == 1 then we just dump the pars list into it.
  if(num_pops == 1) {
    cat("&  generated from R &\n", file = dfile)
    dump <- lapply(names(pars), function(n) {
      # R is inconsistent in type of dash it prints, so standardize with
      # this line:
      n_clean <- gsub("\\p{Pd}", "-", n, perl = TRUE)
      cat(
        "--", n_clean, " ", paste(pars[[n]], collapse = " "), "\n",
        sep = "",
        file = dfile,
        append = TRUE
      )
    })
  }

  # if num_pops > 1, then we check to make sure that pars is a list of length
  # num_pops and we dump each element in the list into dfile separated by --new-pop
  # directives, as appropriate.
  if(num_pops > 1) {
    if(length(pars) != num_pops) {
      stop("Hold it! When num_pops > 1, the pars parameter must be a list with a single list element (or parameters) for each population.")
    }
    cat("&  generated from R &\n", file = dfile)
    for(p in 1:num_pops) {
      subpars <- pars[[p]]
      dump <- lapply(names(subpars), function(n) {
        # R is inconsistent in type of dash it prints, so standardize with
        # this line:
        n_clean <- gsub("\\p{Pd}", "-", n, perl = TRUE)
        cat(
          "--", n_clean, " ", paste(subpars[[n]], collapse = " "), "\n",
          sep = "",
          file = dfile,
          append = TRUE
        )
      })
      cat(
        "--locus-file ", lfile, "\n", sep = "",
        file = dfile,
        append = TRUE
      )
      if(p < num_pops) {
        cat("--new-pop", "\n",
            sep = "",
            file = dfile,
            append = TRUE
        )
      }
    }
  }

  if(num_pops == 1) {
    args <- paste(" --num-pops ", num_pops, " --command-file ", dfile, " --locus-file ", lfile )
  } else {
    args <- paste(" --num-pops ", num_pops, " --command-file ", dfile)
  }

  message("Running spip in directory ", dir)

  # Run this in system2
  setwd(dir)
  spip_ret <- system2(
    command = spip_binary(),
    args = args,
    stdout = sfile,
    stderr = efile
  )

  # catch the case where spip threw an error
  if(spip_ret != 0) {
    error_lines <- readr::read_lines(efile)
    message("\n\n*** spip reported the following errors ***\n\n")
    message(paste(error_lines, collapse = "\n"))
    stop("\n\nAborting.  Please fix error to spip input and try again.\n\n
For a brief listing of all available spip options use:

system2(command = spip_binary(), args = \"--help\")

For a long listing, use:

system2(command = spip_binary(), args = \"--help-full\")\n\n
")
  }

  message(
    "Done running spip. Output file size is ",
    file.size(sfile) / 1e6,
    " Mb"
  )

  message("Processing output file with awk")

  # now, use awk to process that large text file into some things that
  # can be read in by R.  For very large output files, I think that
  # awk will almost certainly be faster.
  single_pass_awk <- system.file("shell/single_pass_cps.awk", package = "CKMRpop")
  awk_binary <- "awk"  # on Linux or Mac this should be on the path
  Sys <- Sys.info()["sysname"]
  if(Sys == "Windows") {
    awk_binary <- system.file("bin/gawk.exe", package = "CKMRpop")
    if(awk_binary == "") {
      stop("Not finding gawk.exe binary on Windows.  Please reinstall spip, with: install_spip(Dir = system.file(package = \"CKMRpop\"))")
    }
  }
  system2(
    command = awk_binary,
    args = c(" -f ", single_pass_awk, " spip_out.txt "),
    stdout = "single_pass_stdout.txt",
    stderr = "single_pass_stderr.txt"
  )

  message("Done processing output into spip_pedigree.tsv, spip_prekill_census.tsv, and spip_samples.tsv")

  # here is how I used to call it using system()
  # call <- paste("./bin/spip --command-file ", dfile, " --locus-file ", lfile, " > ", sfile )
  # system(call)

  # return the path of the directory where everything happened
  dir
}
