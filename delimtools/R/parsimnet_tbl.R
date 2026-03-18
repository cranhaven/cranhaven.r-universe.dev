parsimnet_tbl <- function(dna, parsimnet, delimname = "parsimnet"){
  
  dname <- rlang::sym(delimname)

  if(!methods::is(dna, "DNAbin")){
    
    cli::cli_abort(c("Input data must have class {.cls DNAbin}.",
                     "x" = "You've supplied an input of class {.cls {class(fasta)}}.",
                     "i" = "Try importing your input file using {.pkg ape} {.fn read.FASTA}."))
  }
  
  if(!methods::is(parsimnet, "Parsimnet")){
    
    cli::cli_abort(c("Input data must have class {.cls Parsimnet}.",
                     "x" = "You've supplied an input of class {.cls {class(parsimnet)}}.",
                     "i" = "You can create a {.cls Parsimnet} object by running 
                     {fn. haplotypes::parsimnet}."))
  }
  
  row_id <- unlist(parsimnet@rowindex) |> unname()
  
  row_group <- rep(names(parsimnet@rowindex), sapply(parsimnet@rowindex, length)) |>
    stringr::str_remove_all("net") |>
    as.numeric()
  
  parsimnet_tbl <- tibble::tibble(labels= names(dna)[row_id], !!dname:= row_group)
  
  return(parsimnet_tbl)
}
