# Worker functions for importing files ------------------------------------

#' Combine ECIS data frames end to end
#' 
#' This function will combine ECIS data sets end to end. Preferential to use over a simple rbind command as it runs additional checks to ensure that data points are correctly generated
#'
#' @param ... List of data frames to be combined
#' @param resample Automatically try and re sample the data set. Default is FALSE
#' 
#' @importFrom dplyr distinct select left_join
#'
#' @return A single data frame containing all the data imported, automatically incremented by experiment
#' 
#' @export
#'
#' @examples
#' #Make three fake experiments worth of data
#' experiment1.df = vascr_subset(growth.df, experiment = "1")
#' experiment2.df = vascr_subset(growth.df, experiment = "2")
#' experiment3.df = vascr_subset(growth.df, experiment = "3")
#' 
#' data = vascr_combine(experiment1.df, experiment2.df, experiment3.df)
#' head(data)
#' 
#' 
vascr_combine = function(..., resample = FALSE) {
  
  dataframes = list(...)
  
  # Generate an empty data frame with the correct columns to fill later
  alldata = dataframes[[1]][0, ]
  loops = 1
  
  # Check that both data frames have the same time base
  for (i in dataframes)
  {
    if (!(exists("timepointstomerge")))
    {
      timepointstomerge = unique(i$Time)
    }
    
    if ((!identical(timepointstomerge,unique(i$Time))) & isFALSE(resample))
    {
      vascr_notify("warning","Datasets have different non-identical timebases. Please resample one or more of these datasets before running this function again or graphs may not be properly generated.")
    }
  }
  
  # Mash all the data frames together
  
  for (i in dataframes) {
    indata = i
    indata = vascr_remove_metadata(indata)
    indata$Experiment = paste(loops, ":", indata$Experiment)
    loops = loops + 1
    alldata = rbind(alldata, indata)
  }
  
  alldata$Experiment = as.factor(alldata$Experiment)
  
  
  sampleids = alldata %>% 
    select("Sample", "SampleID") %>%
    distinct() %>%
    regularise_sampleid()
    
  alldata = alldata %>% select(-"SampleID") %>% left_join(sampleids, by = "Sample")
  
  
  if(isTRUE(resample))
  {
    alldata = vascr_resample_time(alldata)
  }
  
  
  return(alldata)
  
}


# dataframes = list(...)
# 
# e1 = tribble(~SampleID, ~Sample,
#         1, "A",
#         2, "B",
#         3, "C",
#         4, "C",
#         3, "D",
#         6, "D",
#         NA, "not set")
# 
# e2 = tribble(~SampleID, ~Sample,
#              1, "A",
#              2, "B",
#              3, "D")
# 
# e3 = tribble(~SampleID, ~Sample,
#              1, "A",
#              2, "B",
#              4, "D")
# 
# e4 = tribble(~SampleID, ~Sample,
#              1, "A",
#              2, "B",
#              4, "F")
# 
# e5 = tribble(~SampleID, ~Sample,
#              1, "A",
#              2, "B",
#              4, "F1")
# 
# 

# dataframes = list(e1, e2, e3, e4, e5)

# merge = foreach(i = c(1:length(dataframes)), .combine = rbind) %do%
#   {
#     cur = dataframes[[i]]
# 
#     if(!"Experiment" %in% colnames(cur)){
#       cur$Experiment = i
#     }
#     
#     cur$LSampleID = cur$SampleID*1000^(i-1)
#     
#     return(cur)
#     
#   }
# 
# merge %>% 
#   group_by()
#   group_by(Sample) %>%
#   reframe(LSampleID = min(LSampleID), SampleID = min(SampleID))
#   
#   
#   
#   
# a = e1
# b = e1
# 
# a$rpt = "NA"
# a$nrow = "NA"
# a$CSampleID = a$SampleID
# 
# b$rpt = paste(b$SampleID, b$Sample) %in% paste(a$SampleID, a$Sample)
# 
# b$OSampleID = b$SampleID
# 
# duped = rbind(a,b) %>% 
#   select("SampleID", "Sample") %>%
#   distinct() %>%
#   
#   group_by(Sample) %>% 
#   mutate(SampleID = min(SampleID)) %>%
#   distinct() %>%
#   
#   mutate(name_reps = c(1:n())) %>%
#   group_by(SampleID) %>%
#   mutate(id_reps = c(1:n()))
# 
# 
# duped
# 
# b %>% filter(!rpt) %>%
#   mutate(nrow = c(1:nrow(.))) %>%
#   mutate(CSampleID = nrow(a) + nrow) %>%
#   rbind(a, .)



#' Regularize the SampleID from merged data sets
#'
#' @returns a vascr dataset with sample ID corrected for consistency
#' 
#' @noRd
#'
#' @examples
#' regularise_sampleid(all_data = e1)
#' 
regularise_sampleid = function(all_data){

# all_data = bind_rows(...)

d1 = all_data %>%
  select("SampleID", "Sample") %>%
  distinct() %>%
  group_by_all() %>%
  group_split()

repaired = d1[[1]]
nextval = max(all_data$SampleID, na.rm = TRUE) + 1

for(row in d1[c(2:length(d1))]) {

  if(any(row$Sample %in% repaired$Sample))
   {
     glue("Sample repeated, ignoring {row$Sample}")
  } else {
    
        if(sum(row$SampleID %in% repaired$SampleID)>=1 || is.na(row$SampleID)){
        # vascr_notify("information", row)
        row$SampleID = nextval
        nextval = nextval +1
        # print("SampleID repeated, reallocating")
      }
      
      repaired = rbind(repaired, row)
  }
  
  # print(repaired)
  
}

repaired

}
  





