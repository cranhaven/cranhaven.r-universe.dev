


#' @title Award & Effort from Cayuse 
#' 
#' @description
#' Print out grant and effort from Cayuse.
#' 
#' @param path \link[base]{character} scalar, directory of downloaded award `.csv` file.
#' Default is the download directory `'~/Downloads'`
#' 
#' @param fiscal.year \link[base]{integer} scalar
#' 
#' @returns ..
#' 
#' @details 
#' \itemize{
#' \item {go to `https://jefferson.cayuse424.com/sp/index.cfm`}
#' \item {My Proposals -> Submitted Proposals. 
#'   Lower-right corner of screen, 'Export to CSV'.
#'   Downloaded file has name pattern `'^proposals_.*\\.csv'`}
#' \item {My Awards -> Awards (*not* 'Active Projects').
#'   Lower-right corner of screen, 'View All', then 'Export to CSV'.
#'   Downloaded file has name pattern `'^Awards_.*\\.csv'`}
#' \item {My Awards -> Awards.  Click into each project, under 'People' tab to find my 
#'   'Sponsored Effort'}
#' }
#' 
#' Function [aggregateAwards] aggregates grant over different period 
#' (e.g. from Axx-xx-001, Axx-xx-002, Axx-xx-003 to Axx-xx).
#' Then we need to manually added in our 'Sponsored Effort' in the returned `.csv` file.
#' 
#' @examples 
#' if (FALSE) {
#' aggregateAwards()
#' viewAward()
#' viewProposal()
#' award2LaTeX()
#' }
#' 
#' @name TJU_Cayuse
#' @importFrom lubridate year
#' @importFrom utils read.csv write.table
#' @export
aggregateAwards <- function(
    path = '~/Downloads', 
    fiscal.year = year(Sys.Date())
) {
  
  awards_csv_ <- list.files(path = path, pattern = '^Awards_.*\\.csv$', full.names = TRUE)
  if (!length(awards_csv_)) stop('Awards file not downloaded?')
  message('\u261e ', style_basename(awards_csv <- sort.int(awards_csv_, decreasing = TRUE)[1L]))
  
  dim(awards <- read.csv(file = awards_csv, header = TRUE))
  # subset(awards, nzchar(Flags)) # manually inspect
  awards <- within.data.frame(awards, expr = {
    Admin.Unit <- Account.Numbers <- NULL
    Status <- Flags <- NULL # hard to consolidate with a series of extensions
    Lead.PI <- gsub(' AOI$', replacement = '', x = Lead.PI)
    Award.Amount <- as.double(gsub('^\\$|,', replacement = '', x = Award.Amount))
    Award.No. <- vapply(strsplit(Award.No., split = '-'), FUN = function(i) paste(i[1:2], collapse = '-'), FUN.VALUE = '')
    Award.Notice.Received <- as.Date.character(Award.Notice.Received, format = '%m/%d/%Y')
    Award.Begin.Date <- as.Date.character(Award.Begin.Date, format = '%m/%d/%Y')
    Award.End.Date <- as.Date.character(Award.End.Date, format = '%m/%d/%Y')
  })
  
  length(ys <- split.data.frame(awards, f = ~ Award.No. + Sponsor))
  length(ys <- ys[vapply(ys, FUN = .row_names_info, type = 2L, FUN.VALUE = 0L) > 0L])
  rid <- split_int_(data = awards, f = ~ Award.No. + Sponsor)
  
  y1 <- do.call(rbind.data.frame, args = c(lapply(rid, FUN = function(id) { # (id = rid[[1L]])
    with(data = awards[id, , drop = FALSE], expr = {
      if (!all(duplicated(Project.Title)[-1L])) stop('`Project.Title` not same')
      first_begin <- min(Award.Begin.Date, na.rm = TRUE)
      last_end <- max(Award.End.Date, na.rm = TRUE)
      Award.Period <- paste0(
        Award.Begin.Date, ' ~ ', Award.End.Date, ' (', 
        format.difftime(asDifftime(Award.End.Date - Award.Begin.Date, units = 'years'), digits = 1L), ', ',
        '$', formatC(Award.Amount, big.mark = ',', format = 'f', digits = 2L),
        ')', collapse = '\n')
      data.frame(
        Award.No. = Award.No.[1L], 
        Project.Title = trimws(Project.Title[1L]), 
        Lead.PI = paste(unique(Lead.PI), collapse = ', '), 
        Sponsor = Sponsor[1L],
        Award.Amount = paste0('$', formatC(sum(Award.Amount, na.rm = TRUE), big.mark = ',', format = 'f', digits = 2L)),
        #Award.Notice.Received = min(Award.Notice.Received, na.rm = TRUE),
        TimePeriod = paste(first_begin, '~', last_end),
        Award.End.Date = last_end,
        Award.Period = Award.Period
      )
    })
  }), list(make.row.names = FALSE)))
  
  y2 <- within(y1, expr = {
    Status <- .bincode(as.double(Award.End.Date), breaks = c(-Inf, as.double(TJU_Fiscal_Year(fiscal.year)), Inf), right = TRUE, include.lowest = TRUE)
    Status <- structure(Status, levels = c(sprintf('Ends before FY%d', fiscal.year), sprintf('Ends in FY%d', fiscal.year), 'Ongoing'), class = 'factor')
    Award.End.Date <- NULL
  })
  
  y3 <- subset(y2, !is.na(Status))
  y3$Effort <- '' # to be manually filled in

  y4 <- y3[order(y3$Award.No.), ]
  aggAwards_csv <- file.path(path, 'aggregatedAwards.csv')
  write.table(y4, file = aggAwards_csv, sep = ',', row.names = FALSE)
  system(paste('open', aggAwards_csv))
  
  message('\u21ac Fill in `Effort` by clicking into each project under \'Active Projects\'')
  return(invisible(y4))
  
}




#' @rdname TJU_Cayuse
#' @export
viewProposal <- function(path = '~/Downloads', fiscal.year = year(Sys.Date())) {
  
  proposal_csv_ <- list.files(path = path, pattern = '^proposals_.*\\.csv$', full.names = TRUE)
  if (!length(proposal_csv_)) stop('Proposal file not downloaded?')
  message('\u261e ', style_basename(proposal_csv <- sort.int(proposal_csv_, decreasing = TRUE)[1L]))
  
  dim(proposal0 <- read.csv(file = proposal_csv, header = TRUE))
  
  proposal1 <- within.data.frame(data = proposal0, expr = {
    Prop.No <- trimws(Prop.No)
    Lead.PI <- gsub(' AOI$', replacement = '', x = Lead.PI)
    Submitted.Date <- as.Date.character(Submitted.Date, format = '%m/%d/%Y')
    Submitted_FY <- .bincode(Submitted.Date, breaks = TJU_Fiscal_Year(fiscal.year)) # NA, 1, NA
    Submitted_Term <- TJU_SchoolTerm(Submitted.Date)
  })
  
  # status_rm <- c('Not Funded', 'Funded', 'Abandoned', 'Withdrawn', 'TJU Signing Official')
  status_rm <- c('Funded', 'Abandoned', 'Withdrawn', 'TJU Signing Official')
  dim(proposal <- eval(quote(subset(x = proposal1, subset = !is.na(Submitted_FY) & !(Status %in% status_rm)))))
  if (FALSE) { #manually inspect
    freqs(proposal$Submitted.Date)
    freqs(proposal$Status)
    length(unique(proposal$Lead.PI))
  }
  
  # copy to Interfolio
  view_by_row(proposal[c('Submitted_Term', 'Project.Name', 'Sponsor', 'Prop.No', 'My.Role', 'Lead.PI', 'Status')])
  return(invisible(proposal))
}




#' @rdname TJU_Cayuse
#' @export
viewAward <- function(path = '~/Downloads') {
  awards <- read.csv(file = file.path(path, 'aggregatedAwards.csv'))
  # copy to Interfolio
  view_by_row(awards[c('Award.No.', 'Project.Title', 'Lead.PI', 'Sponsor', 'Award.Period', 'Status', 'Effort')])
  return(invisible())
}



#' @rdname TJU_Cayuse
#' @export
award2LaTeX <- function(path = '~/Downloads') {
  awards <- read.csv(file.path(path, 'aggregatedAwards.csv'))
  NoOut = lapply(seq_len(.row_names_info(awards, 2L)), FUN = function(i) { 
    # copy to LaTeX resume
    tmp <- awards[i, ]
    cat('\\textmd{', tmp$Role, '.}\n', sep = '')
    cat('\\textit{', gsub('\\&', '\\\\&', tmp$Project.Title), '}.\n', sep = '')
    if (!is.na(tmp$SponsorAward)) cat(gsub('\\&', '\\\\&', tmp$Sponsor), ', ', tmp$SponsorAward, '\n', sep = '') else cat(gsub('\\&', '\\\\&', tmp$Sponsor), '.\n', sep = '')
    cat('awarded to ', tmp$Lead.PI, ', ', gsub('\\~', '-', tmp$TimePeriod), ', \\', tmp$Award.Amount, '\n', sep = '')
    cat('\n\n')
  })
  return(invisible())
}




if (FALSE) {
  # only inspect
  dim(proposalFunded <- subset(pr1, Status == 'Funded')) # to be compared to `awards`
  # `proposalFunded` is not reliable
  proposalFunded[c('Lead.PI', 'Project.Name')]
  subset(awards, OnGoing, select = c('Lead.PI', 'Project.Title'))
}




view_by_row <- function(data) {
  nr <- .row_names_info(data, type = 2L)
  .mapply(FUN = function(x, nm) {
    message('Row ', sQuote(nm))
    lapply(format_named(x), FUN = message)
    cat('\n')
  }, dots = list(
    x = split.data.frame(data, f = seq_len(nr)), 
    nm = seq_len(nr)
  ), MoreArgs = NULL)
  return(invisible())
}




