#' conversion to numeric
#'
#' Conducts conversion: as.character --> as.numeric
#'
#' @param x  single value
#' @return numeric
#'
#' @export
#'
as.Num = function(x){
  as.numeric(as.character(x))
}


#' phyloseq data object conversion to data.frame
#'
#' Conducts conversion of 1 of the data objects
#' in a phyloseq object (eg., tax_table) to a dataframe
#'
#' @param physeq  Phyloseq object
#' @param table_func  See \code{Phyloseq::phyloseq-class} for options
#' @return data.frame
#'
#' @export
#'
#' @examples
#' data(physeq_S2D1)
#' df_otu = phyloseq2df(physeq_S2D1, table_func=phyloseq::otu_table)
#' head(df_otu)
#'
#' df_sample = phyloseq2df(physeq_S2D1, table_func=phyloseq::sample_data)
#' head(df_sample)
#'
phyloseq2df = function(physeq, table_func){
  physeq.md = table_func(physeq)
  physeq.md = suppressWarnings(as.data.frame(as.matrix(physeq.md)))
  physeq.md = as.matrix(data.frame(lapply(physeq.md, as.character)))
  physeq.md = as.data.frame(apply(physeq.md, 2, trimws))
  rownames(physeq.md) = rownames(table_func(physeq))
  return(physeq.md)
}


#' Phyloseq conversion to a ggplot-formatted table
#'
#' Convert the OTU table (+ metadata) to a format that can be
#' easily plotted with phyloseq
#'
#' @param physeq  Phyloseq object
#' @param include_sample_data  Include \code{sample_table} information?
#' @param sample_col_keep  Which columns in the \code{sample_data} table to keep?
#'   Use \code{NULL} to keep all columns.
#' @param include_tax_table  Include \code{tax_table} information?
#' @param tax_col_keep  A vector for column names to keep.
#'   Use \code{NULL} to keep all columns.
#' @param control_expr  An expression for identifying which samples are controls.
#' Control/non-control identification will be in the 'IS_CONTROL' column of the
#' returned data.frame object.
#' @return data.frame
#'
#' @export
#'
#' @examples
#' data(physeq_S2D1)
#' # Including some columns from sample metadata
#' df_OTU = phyloseq2table(physeq_S2D1,
#'                         include_sample_data=TRUE,
#'                         sample_col_keep=c('Buoyant_density', 'Substrate', 'Day'))
#' head(df_OTU)
#'
#' \dontrun{
#' # Including some columns from sample metadata & taxonomy
#' df_OTU = phyloseq2table(physeq_S2D1,
#'                         include_sample_data=TRUE,
#'                         sample_col_keep=c('Buoyant_density', 'Substrate', 'Day'),
#'                         include_tax_table=TRUE)
#' head(df_OTU)
#' }
#'
phyloseq2table = function(physeq,
                          include_sample_data=FALSE,
                          sample_col_keep=NULL,
                          include_tax_table=FALSE,
                          tax_col_keep=NULL,
                          control_expr=NULL){
  # OTU table
  df_OTU = phyloseq::otu_table(physeq)
  df_OTU = suppressWarnings(as.data.frame(as.matrix(df_OTU)))
  df_OTU$OTU = rownames(df_OTU)
  sel_cols = colnames(df_OTU)[colnames(df_OTU) != 'OTU']
  df_OTU = tidyr::gather(df_OTU, "SAMPLE_JOIN", "Count", -"OTU")

  # sample metdata
  if(include_sample_data==TRUE){
    df_meta = phyloseq2df(physeq, phyloseq::sample_data)
    df_meta$SAMPLE_JOIN = rownames(df_meta)

    if(! is.null(control_expr)){
      # setting control
      df_meta = df_meta %>%
        dplyr::mutate_(IS_CONTROL = control_expr)
      # check
      if(all(df_meta$IS_CONTROL == FALSE)){
        stop('control_expr is not valid; no samples selected as controls')
      }
    }

    ## trimming
    if(!is.null(sample_col_keep)){
      sample_col_keep = c('SAMPLE_JOIN', sample_col_keep)
      df_meta = dplyr::select_(df_meta, .dots=as.list(sample_col_keep))
    }
    # join
    df_OTU = dplyr::inner_join(df_OTU, df_meta, c('SAMPLE_JOIN'))
    if(nrow(df_OTU) == 0){
      stop('No rows returned after inner_join of otu_table & sample_data')
    }
  }

  # taxonomy table
  if(include_tax_table==TRUE){
    df_tax = phyloseq::tax_table(physeq)
    df_tax = suppressWarnings(as.data.frame(as.matrix(df_tax)))
    df_tax$OTU = rownames(df_tax)
    ## trimming
    if(!is.null(tax_col_keep)){
      tax_col_keep = c('OTU', tax_col_keep)
      df_tax = dplyr::select_(df_tax, .dots=as.list(tax_col_keep))
    }
    # join
    df_OTU = dplyr::inner_join(df_OTU, df_tax, c('OTU'))
    if(nrow(df_OTU) == 0){
      stop('No rows returned after inner_join of otu_table & tax_table')
    }
  }

  return(df_OTU)
}
