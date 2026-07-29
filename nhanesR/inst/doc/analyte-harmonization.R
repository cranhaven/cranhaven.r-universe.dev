## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----early-biopro-------------------------------------------------------------
# library(haven)
# 
# lab18 <- read_xpt(url(
#   "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1999/DataFiles/Lab18.xpt"))
# l40b  <- read_xpt(url(
#   "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2001/DataFiles/L40_B.xpt"))
# 
# early_df <- rbind(
#   data.frame(SEQN = as.character(lab18$SEQN),
#              GGT  = lab18$LBXSGTSI,
#              ALT  = lab18$LBXSATSI,
#              AST  = lab18$LBXSASSI,
#              ALP  = lab18$LBXSAPSI),
#   data.frame(SEQN = as.character(l40b$SEQN),
#              GGT  = l40b$LBXSGTSI,
#              ALT  = l40b$LBXSATSI,
#              AST  = l40b$LBXSASSI,
#              ALP  = l40b$LBDSAPSI)   # prefix change: LBDS- not LBXS-
# )

## ----map-ggt------------------------------------------------------------------
# library(nhanesR)
# 
# nhanes_variable_map("LBXSGTSI")

## ----map-alp------------------------------------------------------------------
# nhanes_variable_map("LBXSAPSI")
# nhanes_variable_map("LBDSAPSI")

## ----search-alp---------------------------------------------------------------
# nhanes_search_variables("alkaline phosphatase", component = "Laboratory")

## ----map-ast------------------------------------------------------------------
# nhanes_variable_map("LBXSASSI")

## ----map-pth------------------------------------------------------------------
# nhanes_variable_map("LBXPT21")

## ----search-liver-------------------------------------------------------------
# nhanes_search_variables("liver condition", component = "Questionnaire")

## ----download-alp-------------------------------------------------------------
# alp_cycles <- c("1999-2000", "2001-2002", "2003-2004",
#                 "2013-2014", "2015-2016", "2017-2018")
# 
# alp_list <- nhanes_download_analyte(
#   "alkaline phosphatase",
#   cycles    = alp_cycles,
#   component = "Laboratory"
# )
# 
# # Each element is a data frame for one cycle
# lapply(alp_list, head, 3)

## ----harmonise-alp------------------------------------------------------------
# alp_df <- do.call(rbind, lapply(alp_list, function(df) {
#   # Identify whichever column was returned — LBXSAPSI or LBDSAPSI
#   v <- intersect(c("LBXSAPSI", "LBDSAPSI"), names(df))
#   data.frame(
#     SEQN = as.character(df$SEQN),
#     ALP  = df[[v[1]]],
#     stringsAsFactors = FALSE
#   )
# }))
# 
# cat("ALP rows:", nrow(alp_df), "  non-NA:", sum(!is.na(alp_df$ALP)), "\n")

## ----download-alt-------------------------------------------------------------
# alt_cycles <- c("2003-2004", "2005-2006", "2007-2008", "2009-2010",
#                 "2011-2012", "2013-2014", "2015-2016", "2017-2018")
# 
# alt_list <- nhanes_download_analyte(
#   "alanine",
#   cycles    = alt_cycles,
#   component = "Laboratory"
# )
# 
# alt_df <- do.call(rbind, lapply(alt_list, function(df) {
#   data.frame(SEQN = as.character(df$SEQN), ALT = df$LBXSATSI,
#              stringsAsFactors = FALSE)
# }))
# 
# cat("ALT rows:", nrow(alt_df), "  non-NA:", sum(!is.na(alt_df$ALT)), "\n")

## ----download-ast-------------------------------------------------------------
# # Explicitly exclude 2007-2008: nhanes_variable_map showed no coverage there
# ast_cycles <- c("2003-2004", "2005-2006", "2009-2010", "2011-2012",
#                 "2013-2014", "2015-2016", "2017-2018")
# 
# ast_list <- nhanes_download_analyte(
#   "aspartate",
#   cycles    = ast_cycles,
#   component = "Laboratory"
# )
# 
# ast_df <- do.call(rbind, lapply(ast_list, function(df) {
#   v <- intersect(c("LBXSASSI", "LBDSASSI"), names(df))
#   data.frame(SEQN = as.character(df$SEQN), AST = df[[v[1]]],
#              stringsAsFactors = FALSE)
# }))

## ----download-liver-----------------------------------------------------------
# # Early cycles: uppercase L
# mcq_early <- nhanes_download_analyte(
#   "MCQ160L",
#   cycles    = c("2003-2004", "2005-2006", "2007-2008", "2009-2010"),
#   component = "Questionnaire"
# )
# df_early <- do.call(rbind, lapply(mcq_early, function(df) {
#   data.frame(SEQN          = as.character(df$SEQN),
#              liver_ever    = df$MCQ160L,
#              liver_current = df$MCQ170L,
#              stringsAsFactors = FALSE)
# }))
# 
# # Late cycles: lowercase l
# mcq_late <- nhanes_download_analyte(
#   "MCQ160l",
#   cycles    = c("2011-2012", "2013-2014", "2015-2016", "2017-2018"),
#   component = "Questionnaire"
# )
# df_late <- do.call(rbind, lapply(mcq_late, function(df) {
#   data.frame(SEQN          = as.character(df$SEQN),
#              liver_ever    = df$MCQ160l,
#              liver_current = df$MCQ170l,
#              stringsAsFactors = FALSE)
# }))
# 
# mcq_df <- rbind(df_early, df_late)
# cat("Liver disease history rows:", nrow(mcq_df), "\n")
# cat("Ever reported (code 1):",
#     sum(mcq_df$liver_ever == 1, na.rm = TRUE), "\n")

## ----search-albumin-----------------------------------------------------------
# nhanes_search_variables("albumin")

## ----search-albumin-lab-------------------------------------------------------
# nhanes_search_variables("albumin", component = "Laboratory")

## ----assemble-----------------------------------------------------------------
# base_full <- readRDS("~/Documents/R.code/nhanesR/analytic_survival.rds")
# 
# # Standard eligibility filters
# base <- base_full[
#   !is.na(base_full$statin)   & !base_full$statin        &
#   !is.na(base_full$ELIGSTAT) & base_full$ELIGSTAT == 1  &
#   !is.na(base_full$time)     & base_full$time > 2, ]
# base$time_lm   <- base$time - 2
# base$WTMEC_adj <- base$WTMEC2YR / 8   # 8 post-Census-2000 cycles
# 
# # Merge each analyte; all.x = TRUE preserves all base rows
# base <- merge(base, alt_df,  by = "SEQN", all.x = TRUE)
# base <- merge(base, ast_df,  by = "SEQN", all.x = TRUE)
# base <- merge(base, alp_df,  by = "SEQN", all.x = TRUE)
# base <- merge(base, mcq_df,  by = "SEQN", all.x = TRUE)
# 
# # Verify availability by cycle
# cat("\nAST availability by cycle:\n")
# print(table(base$cycle, !is.na(base$AST)))
# 
# cat("\nALP availability by cycle:\n")
# print(table(base$cycle, !is.na(base$ALP)))

## ----derived------------------------------------------------------------------
# # De Ritis ratio (requires both AST and ALT)
# base$de_ritis <- base$AST / base$ALT
# 
# # MELD-XI (no INR required; bilirubin and creatinine already in base)
# # Convention: floor both inputs at 1.0 before log-transform
# base$creat_meld <- pmin(pmax(base$creatinine, 1.0), 4.0)
# base$bili_meld  <- pmax(base$bilirubin, 1.0)
# base$MELD_XI <- 5.11 * log(base$bili_meld) +
#                11.76 * log(base$creat_meld) + 9.44
# 
# cat("De Ritis ratio: median",
#     round(median(base$de_ritis, na.rm = TRUE), 2),
#     " IQR", paste(round(quantile(base$de_ritis, c(0.25, 0.75), na.rm = TRUE), 2),
#                   collapse = "–"), "\n")
# 
# cat("MELD-XI: 90th percentile",
#     round(quantile(base$MELD_XI, .90, na.rm = TRUE), 1), "\n")

## ----session------------------------------------------------------------------
# sessionInfo()

