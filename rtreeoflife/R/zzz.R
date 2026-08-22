.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("rtreeoflife.quiet", FALSE))) {
    return(invisible())
  }

  packageStartupMessage(tol_attach_message(pkgname))
}

tol_attach_message <- function(pkgname = "rtreeoflife") {
  version <- utils::packageVersion(pkgname)
  header <- cli::rule(
    left = cli::col_white(cli::style_bold(paste("Attaching", pkgname))),
    right = cli::col_white(paste(pkgname, version))
  )

  tick <- cli::col_green(cli::symbol$tick)
  workflows <- c(
    tol_attach_item(
      tick,
      cli::col_blue("species index"),
      "tol_species_index(), tol_match_species()"
    ),
    tol_attach_item(
      tick,
      cli::col_blue("selective download"),
      "tol_download_fasta(), tol_export_fasta()"
    ),
    tol_attach_item(
      tick,
      cli::col_blue("tidy FASTA"),
      "tol_attach_fasta(), tol_fasta_long()"
    ),
    tol_attach_item(
      tick,
      cli::col_blue("visualisation"),
      "tol_plot_gene_recovery(), tol_plot_tree()"
    )
  )

  footer <- paste(
    cli::col_cyan(cli::symbol$info),
    cli::format_inline(
      "Data source: {.url https://treeoflife.kew.org/} and {.url https://sftp.kew.org/pub/treeoflife/current_release/}"
    )
  )

  paste(c(header, workflows, footer), collapse = "\n")
}

tol_attach_item <- function(tick, label, calls, width = 20) {
  visible_width <- cli::ansi_nchar(label)
  padding <- paste(rep(" ", max(width - visible_width, 1)), collapse = "")
  paste0(tick, " ", label, padding, calls)
}
